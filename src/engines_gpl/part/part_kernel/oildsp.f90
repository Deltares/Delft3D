!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module oildsp_mod
    use m_stop_exit
    use m_part11


contains
    subroutine oildsp (lgrid, num_rows, conc, volume, area, &
            npart, mpart, wpart, radius, nodye, &
            npwndw, nopart, itime, idelt, wvelo, &
            const, lun2, nosubs, num_layers, lgrid2, &
            lgrid3, &
            num_columns, xb, yb, kpart, mapsub, &
            isfile, nfract, mstick, nstick, fstick, &
            xa, ya, pg, use_settling, xpart, &
            ypart, zpart, za, locdep, dps, &
            tcktot, substi, npmax, rhow, &
            amassd, ioptrad, ndisapp, idisset, tydisp, &
            efdisp, xpoldis, ypoldis, nrowsdis, wpartini, &
            iptime)

        !       Deltares Software Centre

        !>\file
        !>         Does all the process kinetics associated with oil
        !>
        !>         <ul><li> Initial gravity spreading through radius
        !>         Oil released through dye releases will have an initial gravity spreading at the
        !>         water surface where it floats on. This routine is able (optrad(id) == 1) to
        !>         compute this radius using the Fay-Hoult formula. The actual release, using these
        !>         radius values, takes place in the dye release routine part09.f90.\n
        !>         Estimate of initial radius from adios user's manual (p4.9), NOAA 1994\n
        !>         ref: fay,j. and d.hoult, 1971. 'physical processes in the spread of oil on
        !>         a water surface',report dot-cg-01 381-a. Washington, D.C.: U.S. Coast Guard.
        !>         <li> Volatilisation and emulsification through changes of weigth
        !>         All oil particles have always 3 weight factors<ol>
        !>         <li> floating on the water surface
        !>         <li> dispersed over the water column
        !>         <li> sticking at the bed</ol>
        !>         Depending on the location of the weight of the particle it is succeptible to
        !>         wind and water driven transport, transport in the water only or it is laying on
        !>         the bed.
        !>         <li> Different oil fractions with different characteristics
        !>         It is possible to release different oil fractions that behave differently
        !>         with one particle. The code has had a maximum of 4 fractions. During subsequent
        !>         changes it is tried to remove that maximum and to let it be up to the user.
        !>         If 2 fractions are used, each particle has 6 weight factors, 3 for each fraction.
        !>         Note that only the particles move, so the fractions in a particle move always the
        !>         same. It is therefore recommended to specify multiple batches of particles, one only
        !>         with fraction 1, .. etc.
        !>         <li> Entrainment (emulsification) of oil through sophisticated techniques
        !>         The entrainment of particles from the water surface to the watercolumn is computed
        !>         here. It is possible to specify a constant entrainment factor per day (ioptd(ifrac) == 0).
        !>         It is also possible to use the advanced formula of Delvigne and Sweeny (ioptd(ifrac) == 1).
        !>         Steady state oil distribution approximation from Adios used with maximum droplet
        !>         size of 70 micron. See Adios User's Manual p 4-12.\n
        !>         If a random number is lower than the fraction entrained, the whole floating mass is
        !>         migrated to the watercolumn weight. For enough particles, the net effect is that indeed
        !>         the correct fraction is entrained.\n
        !>         Note that for entrainment unpredictable results are reached if the particle really has
        !>         multiple fractions and one fraction wants to entrain whereas the other wants be remain floating.\n
        !>         Volatilisation only takes place for floating oil. A constant volatilisation rate per day
        !>         is specified for that. This reduces the weight of the particle. The amount of volatised
        !>         oil is also accumulated (like many other characteristics).\n
        !>         <li> Sticking of oil at the water bed through sticking probability
        !>         Whether submerged oil sticks is determined by the stickyness probability. The actual sticking
        !>         takes place in the advection diffusion routine (part10.f90), together with the migration
        !>         of the weight from the dispersed box towards the sticking box.\n
        !>         <li>The 10 coefficients for each fraction of oil are read from the input file and read:<ol>
        !>         <li>evaporating fraction per day
        !>         <li>dispersion option (0=fraction per day; 1=delvigne/sweeny formula)
        !>         <li>dispersion rate per day (if dispersion option = 0)
        !>         <li>stickyness probability [0,1]
        !>         <li>volatile fraction [0,1]
        !>         <li>emulsification parameter c1
        !>         <li>maximum water content c2  [0,1]
        !>         <li>evaporated share at which emulsification starts
        !>         <li>oil density of the fraction
        !>         <li>kinematic viscosity of the fraction</ol>
        !>         <li> More Background:<ul>
        !>         <li> oil dispersion from Delvigne, Roelvink and Sweeney:\n
        !>             'Reseach on vertical turbulent dispersion of oil droplets and oiled particles',\n
        !>              OCS study MMS 86-0029 Anchorage, US Department of the Interior'
        !>         <li> G.A.L. Delvigne and L.J.M.hulsen, AMOP 1994, Vancouver, Canada\n
        !>             'Simplified laboratory measurements of oil dispersion coefficient-application in
        !>              computations of natural oil dispersion' - whitecapping:\n
        !>              Holthuysen and Herbers: J. Phys. Ocean 16,290-7,[1986]
        !>         </ol></ol>

        !     System administration : Antoon Koster

        !     Created               : 27 November  1997 by Robert Vos

        !     modified              : 14 September 1998 by Robert Vos    : for sticking oil fractions
        !                                                                  dispersion and evaporation is stopped.
        !                             26 May       2000 by Robert Vos    : mass balance errors: 2 major bug solved!
        !                              2 August    2000 by Robert Vos    : improve accuracy of oil concentrations for oil
        !                                                                  dispersion using the plot grid
        !                             30 October   2002 by Frank Kleissen: surface oil slick thickness (hmin), depending on oil characteristics
        !                                                                  value of 0.0005 m based on comparisons with adios simulations
        !                                                                 (see Kleissen, r&d report z3291, january 2003)
        !                              6 November  2002 by Frank Kleissen: viscosity used to calculate the co (dispersion parameter).
        !                                                                  relationship derived from lab data (Delvgne and Hulsen, 1994)
        !                                                                  visc < 125 :  log(co) = -0.0658*log(visc)+3.2618
        !                                                                  visc > 125 :  log(co) =  1.1951*log(visc)+5.6456
        !                              7 November  2002 by Frank Kleissen: emulsification using growing water content based on Mackay(1980, 1982)
        !                              7 November  2002 by Frank Kleissen: emulsification constants c1, c2 from input. c1 = 2*10-6 means
        !                                                                  emulsification can take place otherwise c1 schould be zero.
        !                                                                  c2 is the maximum water content.
        !                                                                  evaporation constant is 1 for light oils and 10 for heavy oils
        !                                                                 (limit at visc=500)
        !                             26 July      2011 by Leo Postma      some cosmetic redesign and paralellism
        !                             27 Jan       2015 by Frank Kleissen   To calculate the concentration of surface floating oil lgrid2 was used. the Concentration
        !                                                                  should be derived from lgri3 that contains the active segment numbering, this was corrected

        !     note                  : ioilt(1) = mapsub(1), oil in top layer, 1th fraction
        !                             ioild(1) = mapsub(2), oil dispersed, 1th fraction
        !                             ioils(1) = mapsub(3), oil sticking , 1th fraction
        !                             ioilt(2) = mapsub(4), oil in top layer, 2nd fraction
        !                             ioild(2) = mapsub(5), oil dispersed, 2nd fraction
        !                             ioils(2) = mapsub(6), oil sticking , 2nd fraction

        !                             decay of dispersed oil via standard decay routine  (??? lp)

        !     Logical unit numbers  : lun2   - output file to print statistics

        !     Subroutines called    : part11 - make concentrations on a detailed plot grid

        use m_waq_precision         ! single/double precision
        use timers            ! to time the performance
        use grid_search_mod   ! explicit interface
        use alloc_mod         ! to allocate arrays
        use pinpok_mod        ! determine if particle is in polygon
        use typos
        use random_generator

        implicit none
        save

        !     Arguments

        !     kind            function         name                    description

        integer   (int_wp), intent(in) :: itime                 !< current time in the model
        integer  (int_wp), intent(in) :: idelt                 !< computational time step size
        integer  (int_wp), intent(in) :: lun2                  !< unit number output report file
        integer  (int_wp), intent(in) :: nfract                !< number of oil fractions
        integer  (int_wp), intent(in) :: npmax                 !< total number of particles
        integer  (int_wp), intent(in) :: npwndw                !< first active particle in the array
        integer  (int_wp), intent(in) :: nopart                !< current maximum of active particles
        integer  (int_wp), intent(in) :: nodye                 !< number of dye releases
        integer  (int_wp), intent(in) :: num_rows                  !< first dimension of the grid
        integer  (int_wp), intent(in) :: num_columns                  !< second dimension of the grid
        integer  (int_wp), intent(in) :: num_layers                 !< number of layers     (may be more is use_settling)
        integer  (int_wp), intent(in) :: nosubs                !< number of substances (may be more than 3*fract)
        integer  (int_wp), intent(in) :: nstick                !< number of sticking substances
        integer(int_wp), dimension(:) :: iptime
        logical, intent(in) :: use_settling                !< if true, settling and an additional layer exists
        real     (real_wp), pointer :: const  (:)            !< constants as read from the input file
        real     (real_wp), intent(out) :: fstick (nfract)       !< sticking probability of a fraction
        real     (real_wp), intent(in) :: rhow                  !< density of water
        real     (real_wp) :: hmin                  !< hmin=0.0005 m based on adios (see kleissen 2003)
        real     (real_wp), intent(out) :: radius (nodye)        !< computed radius of dye releases of oil
        real     (dp), intent(in) :: wvelo  (*)            !< wind velocity
        integer  (int_wp), pointer :: lgrid (:, :)           !< active grid layout of the area
        integer  (int_wp), pointer :: lgrid2(:, :)           !< total grid layout of the area
        integer  (int_wp), pointer :: lgrid3(:, :)           !< total grid layout of the area
        real     (real_wp), pointer :: xb     (:)            !< x-values of the corners of the gridcells
        real     (real_wp), pointer :: yb     (:)            !< y-values of the corners of the gridclls
        integer  (int_wp), pointer :: npart  (:)            !< 1st cell index of each particle
        integer  (int_wp), pointer :: mpart  (:)            !< 2nd cell index of each particle
        integer  (int_wp), pointer :: kpart  (:)            !< 3rd cell index of each particle
        real     (real_wp), pointer :: xpart  (:)            !< normalized x-value within the cell of the particles
        real     (real_wp), pointer :: ypart  (:)            !< normalized y-value within the cell of the particles
        real     (real_wp), pointer :: zpart  (:)            !< normalized z-value within the cell of the particles
        real     (real_wp), pointer :: xa     (:)            !< x-world coordinate of the particles
        real     (real_wp), pointer :: ya     (:)            !< y-world coordinate of the particles
        type(PlotGrid) :: pg                    !< plot grid information
        real     (real_wp), pointer :: za     (:)            !< z-world coordinate of the particles
        real     (real_wp), pointer :: locdep(:, :)           !< local depths of the gridcells
        real     (real_wp), pointer :: dps    (:)            !< depth of the reference plain in the grid cells
        real     (real_wp), pointer :: tcktot (:)            !< relative layer thickness of the layers
        real     (real_wp), pointer :: conc  (:, :)           !< concentrations on hydrodynamic grid
        real     (real_wp), pointer :: volume (:)            !< volume of the computational cells
        real     (real_wp), pointer :: area   (:)            !< horizontal surface area of the grid
        integer  (int_wp), intent(in) :: ioptrad(nodye)        !< if 1 use fay-holt formula for the radius
        character(20), intent(in) :: substi (nosubs)       !< names of the substances
        integer  (int_wp), intent(in) :: isfile (nosubs)       !< when 1 conc. follows from file in user routine
        integer  (int_wp), intent(in) :: mapsub (nosubs)       !< index array substances in map file
        integer  (int_wp), intent(in) :: mstick (nosubs)       !< sticking index substances
        real     (real_wp), pointer :: amassd  (:, :)         !< mass of substances per dye releases
        real     (real_wp), pointer :: wpart   (:, :)         !< weight of the particles per substance
        real     (real_wp), pointer :: wpartini   (:, :)         !< weight of the particles per substance
        integer  (int_wp), intent(in) :: ndisapp               !< number of dispersant applications
        integer  (int_wp), pointer :: idisset(:)            !< timing of dispersant application
        integer  (int_wp), intent(in) :: tydisp                !< type of dispersant parameterisation
        real     (sp), pointer :: efdisp (:, :)          !< effectiveness parameter of dispersant application per oil type
        real     (sp), pointer :: xpoldis (:, :)         !< x-coordinates of dispersant polygon
        real     (sp), pointer :: ypoldis (:, :)         !< y-coordinates of dispersant polygon
        integer  (int_wp), pointer :: nrowsdis (:)          !< length of dispersant polygon

        !     parameters used in formulae. Do not change the values without explicit permission of Frank Kleissen.

        real   (real_wp), parameter :: dmaxr = 70.0 * 1.0e-6   ! chosen according to steady state distribution of Adios
        real   (real_wp), parameter :: rk1 = 1.14          ! Fays constant k1
        real   (real_wp), parameter :: rk2 = 1.45          ! Fays constant k2
        real   (real_wp), parameter :: visw = 1.0e-6        ! viscosity of water
        real   (real_wp), parameter :: grav = 9.81          ! accelleration of gravity
        real   (real_wp), parameter :: cb = 0.032         ! pre-constant Holthuyzen
        logical, parameter :: lplgr = .true.        ! there is a plotgrid

        !     allocatables, these are arrays only used in this routine and saved between subsequent calls
        !                   NB. the arrays involved in OMP PRIVATE and REDUCTION clauses may NOT be pointers
        !                   and should be allocated every time (that is done here on the stack)

        real   (real_wp), pointer :: fwatoil   (:, :)        ! cumulative water fraction, per fraction and particle
        real   (real_wp), pointer :: rhooilv   (:, :)        ! oil density, initialized with constant 9, changes over time
        real   (real_wp), pointer :: viso      (:, :)        ! kinetic viscosity, initialized with constant 10, changes over time.
        real   (real_wp), pointer :: totfe     (:, :)        ! cumulative evaporated part, per fraction and particle
        real   (real_wp), pointer :: c1         (:)         ! emulsification parameter
        real   (real_wp), pointer :: c2         (:)         ! maximum water content c2 [0,1] ( constant nr. 7 )
        integer(int_wp) :: isurf      (nfract)    ! number of surface particles per fraction at the surface
        real   (real_wp), pointer :: d180       (:)         ! percentage evaporated at 180degC
        real   (real_wp), pointer :: rhotmp     (:)
        real   (real_wp), pointer :: rhooil     (:)         !oil density
        real   (real_wp), pointer :: visotmp    (:)
        real   (real_wp), pointer :: visowat    (:)         ! kinetic viscosity (constant 10)
        real   (real_wp), pointer :: tmpevap    (:)         ! temporary storage of evaporation
        real   (real_wp) :: fracte     (nfract)    ! evaporated part, per fraction
        real   (real_wp) :: tmpfracte  (nfract)    ! evaporated part, per fraction
        real   (real_wp) :: fractd     (nfract)    ! workarray only used for the fraction of dispersed oil
        real   (real_wp) :: tmpfractd  (nfract)    ! workarray only used for the fraction of dispersed oil
        integer(int_wp), pointer :: luncsv     (:)         ! unit numbers for the csv files of the fractions
        integer(int_wp), pointer :: ioptd      (:)         ! dispersion option 0 = fraction / day ; 1 = delvigne/sweeny
        real   (real_wp), pointer :: ioptev     (:)         ! evaporatio option 0 = fraction / day ; other is first order process
        real   (real_wp), pointer :: volfrac    (:)         ! volatile fraction
        integer(int_wp), pointer :: ioilt      (:)         ! substance numbers of the floating part of the oil fractions
        integer(int_wp), pointer :: ioild      (:)         ! substance numbers of the dispersed part of the oil fractions
        integer(int_wp), pointer :: ioils      (:)         ! substance numbers of the sticking part of the oil fractions
        real   (dp), pointer :: wsume      (:)         ! cumulative sum of mass evaporated oil per fraction
        real   (dp) :: wsumd      (nfract)    ! total mass of dispersed oil
        real   (dp) :: wsums      (nfract)    ! total mass of sticking oil
        real   (dp) :: wsumt      (nfract)    ! total mass of floating oil
        real   (dp) :: wevapt     (nfract)    !
        real   (real_wp), pointer :: evemul     (:)         ! evaporated fraction at which emulsification starts (default 1.0)
        real   (real_wp) :: viscsurf   (nfract)    !
        real   (real_wp) :: fwatoilsurf(nfract)    !
        real   (real_wp) :: densurf    (nfract)    !
        !      real   (rp)              :: c1         (nfract)    ! emulsification parameter ( constant nr. 6 )

        !     locals

        real     (real_wp), pointer :: amap(:, :, :, :)         ! concentrations on detail plotgrid
        real     (real_wp) :: window (4)               ! window of the plotgrid
        integer  (int_wp) :: nmap                     ! first dimension of the plot window
        integer  (int_wp) :: mmap                     ! second dimension of the plot window
        real     (real_wp) :: surf                     ! surface area of a plotgrid cell
        logical :: first = .true.
        character(256) :: csv_fnam                 ! help string file names csv files
        real     (real_wp) :: timlc                    ! time since start in hours
        integer  (int_wp) :: nfcons                   ! number of constants per oil fraction
        real     (real_wp) :: cdt                      ! temperature dependency of oil density    f.m. kleissen 2-6-2003
        real     (real_wp) :: temp                     ! actual temperature                       f.m. kleissen 2-6-2003
        real     (real_wp) :: temp0                    ! reference temperature                    f.m. kleissen 2-6-2003
        real     (real_wp) :: cde                      ! density depending on evaporated fraction
        real     (real_wp) :: cdelv                    ! oil parameter c0 of Delvigne
        real     (real_wp) :: voil                     ! volume of oil entrained per unit volume of water
        real     (real_wp) :: pi                       ! pi
        real     (real_wp) :: prefac                   ! to be removed
        integer  (int_wp) :: id, ifrac, isub, jsub   ! help and loop variables for dyes, fractions and substances
        integer  (int_wp) :: ifr, ilay, iseg         ! help and loop variables for fractions, layers and cells
        integer  (int_wp) :: i, i1, i2           ! particle loop counters
        integer  (int_wp) :: ix, iy                  ! help variables plot grid indices
        real     (real_wp) :: xpf, ypf                 ! help variables plot grid coordinates
        real     (real_wp) :: windw1, windw3           ! help variables plot window
        real     (real_wp) :: wveloi                   ! wind velocity at which white capping starts
        integer  (int_wp) :: ndisp                    ! number accumulator entrained particles
        integer  (int_wp) :: nevap                    ! number accumulator evaporated particles
        integer  (int_wp) :: ic                       ! help variable for the grid index
        real     (real_wp) :: oilmass                  !
        real     (real_wp) :: totmas                   ! helpvariable for oil mass
        real     (real_wp) :: cfloat                   ! help variable floating concentrations
        real     (real_wp) :: dfwatoil                 ! help variable change in water fraction oil particle
        real     (real_wp) :: dviso                    ! help variable change in viscosity of the oil particle
        real     (real_wp) :: ac                       ! concentration help variable
        real     (real_wp) :: am                       ! mass help variable
        real     (real_wp) :: fvolum                   ! help variable volume of a plotgridcell
        real     (real_wp) :: wsum                     ! help variable accumulation of particle weight
        real     (dp) :: wevap                    ! not so clear why this simple help variable is double precision
        real     (real_wp) :: volfracw                 ! volume fraction of water per particle
        real     (real_wp) :: qentr                    ! entrainment rate (kg/m2/s)
        real     (real_wp) :: vol0
        real     (real_wp) :: difrho
        real     (real_wp) :: vol56
        real     (real_wp) :: vis13
        real     (real_wp) :: grd16
        real     (real_wp) :: fac
        real     (dp) :: fac1, fac2               ! two factors in the Delvigne/Sweeny formula
        real     (real_wp) :: fw                       ! fraction white-capping
        real     (real_wp) :: tp                       ! peak wave period (sec)
        real     (real_wp) :: fbw                      ! help variable
        real     (real_wp) :: h0wav                    ! significant wave height (m)
        real     (real_wp) :: hrms                     !
        real     (real_wp) :: de                       ! dissipation of wave energy per unit of surface area (j /m2)
        real     (real_wp) :: wfact                    ! wind factor in Delvigne/Sweeny
        real     (real_wp) :: rrand                    ! help variable for the random result
        real     (dp) :: rseed = 0.5d10           ! seed of the random number generator
        integer  (int_wp) :: idisapp                  ! counter for dispersant applications
        logical :: disapp                   ! dispersant application set for this time step
        real     (real_wp) :: pdisapp(nfract)          ! chance for effictive dispersant application
        real     (real_wp) :: fractdapp                ! total chance to disperce
        integer  (int_wp) :: inside                   ! is particle inside polygon?
        real     (dp) :: tmpevapold               ! FMK: temporary variable (previous timestep)
        integer  (int_wp) :: npadd                    !additional parameters if evaporation option 0 is used

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /

        if (nfract <= 0) return
        if (timon) call timstrt("oildsp", ithndl)

        !     For the time being

        mmap = pg%mmap
        nmap = pg%nmap
        window(1) = pg%xlow
        window(2) = pg%xhigh
        window(3) = pg%ylow
        window(4) = pg%yhigh
        surf = pg%surf
        amap => pg%amap

        if (first) then

            !        allocate the local oil-processes arrays

            first = .false.
            call alloc ("fwatoil", fwatoil, nfract, npmax)
            call alloc ("rhooilv", rhooilv, nfract, npmax)
            call alloc ("viso", viso, nfract, npmax)
            call alloc ("totfe", totfe, nfract, npmax)
            call alloc ("c1", c1, nfract)
            call alloc ("c2", c2, nfract)
            call alloc ("d180", d180, nfract)
            call alloc ("rhotmp", rhotmp, nfract)
            call alloc ("rhooil", rhooil, nfract)
            call alloc ("visotmp", visotmp, nfract)
            call alloc ("visowat", visowat, nfract)
            call alloc ("tmpevap", tmpevap, nfract)
            call alloc ("luncsv", luncsv, nfract)
            call alloc ("ioptd", ioptd, nfract)
            call alloc ("ioptev", ioptev, nfract)
            call alloc ("volfrac", volfrac, nfract)
            call alloc ("ioilt", ioilt, nfract)
            call alloc ("ioild", ioild, nfract)
            call alloc ("ioils", ioils, nfract)
            call alloc ("wsume", wsume, nfract)
            call alloc ("evemul", evemul, nfract)
            totfe = 0.0
            fwatoil = 0.0
            wsume = 0.0
            tmpevap = 0.0

            !     open output files for mass balances

            do ifrac = 1, nfract
                luncsv(ifrac) = 70 + ifrac
                write(csv_fnam, '(a,a)') trim(substi((ifrac - 1) * 3 + 1)), '.csv'
                open (newunit = luncsv(ifrac), file = trim(csv_fnam))
                write(luncsv(ifrac), 1000)                      &
                        'Time (hours)          ,', &
                        'Total mass-floating   ,', &
                        'Total mass-dispersed  ,', &
                        'Total mass-evaporated ,', &
                        'Total mass-sticky     ,', &
                        'Total mass            ,', &
                        'Viscosity surface part.     ,', &
                        'Water fraction surface part.,', &
                        'Density surface part.        '
            enddo

            !     initialize local constants but also global arrays with values from the const(nosubs) constants array

            timlc = 0.0                            ! time since start in hours
            nfcons = 10                             ! no. of constants per oil fraction
            cdt = 0.0008                         ! temperature dependency of oil density f.m. kleissen 2-6-2003
            temp = 0.0                            ! no temperature implemented yet        f.m. kleissen 2-6-2003
            temp0 = 0.0                            ! no temperature implemented yet        f.m. kleissen 2-6-2003
            cde = 0.0                            ! dependency of density on the evaporated fraction (for future)
            fac1 = (10**3.2618)
            fac2 = (10**5.6456)
            voil = (dmaxr**(1.7)) / 1.7             !.. dmaxr chosen according to steady state distribution of adios
            prefac = rk2 * rk2 / rk1
            pi = 4.0 * atan(1.0)
            wpartini = 0.0
            npadd = 0   !additional parameters in case we use evaporatoin option 0, added to maintain backward compatibility
            wveloi = 5.0   ! default value

            do ifrac = 1, nfract
                ioptev (ifrac) = const((ifrac - 1) * nfcons + npadd + 1)       ! evaporatoin option (-2 (Fingas incl. effect of waterfraction on evaporation, or -1 = fingas, >0 = first order)
                if (ioptev (ifrac)<=-1) then
                    d180(ifrac) = const((ifrac - 1) * nfcons + npadd + 2)                 ! evaporation rate
                    temp = const((ifrac - 1) * nfcons + npadd + 3)                 ! temperature
                    npadd = npadd + 2
                endif
                c1(ifrac) = const((ifrac - 1) * nfcons + npadd + 6)       ! emulsification parameter    default = 2.0e-06
                ioptd  (ifrac) = const((ifrac - 1) * nfcons + npadd + 2) + 0.5 ! disp opt (0 = fr / day; 1 = delvigne/sweeny)
                fstick (ifrac) = const((ifrac - 1) * nfcons + npadd + 4)       ! stickyness probability [0,1]
                volfrac(ifrac) = const((ifrac - 1) * nfcons + npadd + 5)       ! volatile fraction: [0,1] default = 0.94
                c2     (ifrac) = const((ifrac - 1) * nfcons + npadd + 7)       ! maximum water content [0,1] default = 0.70
                evemul (ifrac) = const((ifrac - 1) * nfcons + npadd + 8)       ! default = 1.0
                visowat(ifrac) = const((ifrac - 1) * nfcons + npadd + 10)       ! kin. viscosity
                if (ioptev(ifrac)>=0) then
                    fracte (ifrac) = (1.0 - exp(-ioptev(ifrac) / 86400.0 * idelt)) * volfrac(ifrac)
                else
                    fracte(ifrac) = 0.0
                endif

                ioilt  (ifrac) = mapsub((ifrac - 1) * 3 + 1)
                ioild  (ifrac) = mapsub((ifrac - 1) * 3 + 2)
                ioils  (ifrac) = mapsub((ifrac - 1) * 3 + 3)
                if (ioptd(ifrac)   ==   0) then              !  dispersion rate/day
                    fractd(ifrac) = const ((ifrac - 1) * nfcons + npadd + 3) * idelt / 86400.0
                elseif (ioptd(ifrac)   ==   1) then                                               !  delvigne/sweeny
                    fractd(ifrac) = 0.0
                elseif (ioptd(ifrac)   ==   2) then
                    wveloi = const ((ifrac - 1) * nfcons + npadd + 3)           !when option 2 the minimu windspeed at which
                    !waves start to break can be varied
                endif
                rhooil(ifrac) = const ((ifrac - 1) * nfcons + npadd + 9)
                visotmp(ifrac) = const ((ifrac - 1) * nfcons + npadd + 10)
                do i = 1, npmax
                    rhooilv(ifrac, i) = rhooil(ifrac)  !  oil density
                    viso   (ifrac, i) = visotmp(ifrac)  !  kin. viscosity
                enddo
            end do
            hmin = const ((nfract - 1) * nfcons + npadd + 11)

            !        calculate and report the release radius (Fay-Holt formula) per release

            write(lun2, '(//)')
            do id = 1, nodye
                if (ioptrad(id) /= 1) cycle

                ifrac = 1                                   !     calculation based on substance #1
                isub = 1                                   !     (and oil fraction #1)
                !            rhooil  = 0.0
                do ifrac = 1, nfract
                    oilmass = 0.0

                    oilmass = oilmass + amassd(1 + (ifrac - 1) * 3, id)
                    vol0 = oilmass / rhooil(ifrac)                      !     volume = mass/rho
                    difrho = (rhow - rhooil(ifrac)) / rhow
                    if (difrho < 0) then
                        write(lun2, '(a      )') ' Problem calculating oil radius'
                        write(lun2, '(a      )') ' Density of oil > water density ??'
                        write(lun2, '(a,i4   )') ' Dye release #', id
                        write(lun2, '(a,f10.2)') ' Density of oil   : ,= ', rhooil(ifrac)
                        write(lun2, '(a,f10.2)') ' Density of water : ,= ', rhow
                        call stop_exit(1)
                    endif
                    vol56 = vol0**(5.0 / 6.0)
                    vis13 = visw**(1.0 / 3.0)
                    grd16 = (grav * difrho)**(1.0 / 6.0)
                    fac = vol56 * grd16 / vis13
                    radius(id) = sqrt(fac) * prefac
                    write(lun2, '(2x,a,i4)')' Oil fraction #', ifrac
                    if (oilmass>0)then
                        write(lun2, '(2x,a,i4)') 'Dye release #', id
                        write(lun2, '(2x,a,es15.7,a)') '   Initial radius(Fay-Hoult) :', radius(id), ' m'
                        write(lun2, '(2x,a,es15.7,a)') '   Mass    of released oil   :', oilmass, ' kg'
                        write(lun2, '(2x,a,es15.7,a)') '   Volume  of released oil   :', vol0, ' m3'
                        write(lun2, '(2x,a,es15.7,a)') '   Density of released oil   :', rhooil(ifrac), ' kg/m3'
                    else
                        write(lun2, '(2x,a,i4,a,i4)') '  No release of fraction #', ifrac, ' for Dye release #', id
                    endif
                enddo
            enddo

            !        write selected evaporation method and used coeficients to the report file

            write(lun2, '(//)')
            do ifrac = 1, nfract
                if (ioptev(ifrac)>=0)write(lun2, *) ' Fraction ', ifrac, ' evaporized: ', ioptev(ifrac), ' per day ' !only when it is a evaporation constant
                write(lun2, *) ' Fraction ', ifrac, ' evaporized: ', fracte(ifrac), ' per time step '
                if (ioptd(ifrac) == 1) then
                    write(lun2, *) ' Fraction ', ifrac, ' volatile fraction: ', volfrac(ifrac)
                    write(lun2, *) ' Oil dispersion according to Delvigne/Sweeney'
                elseif (ioptd(ifrac) == 2) then
                    write(lun2, *) ' Fraction ', ifrac, ' volatile fraction: ', volfrac(ifrac)
                    write(lun2, *) ' Oil dispersion according to Delvigne/Sweeney'
                    write(lun2, *) ' Wind speed at which waves start to break: ', wveloi
                else

                    write(lun2, *) ' Oil dispersion according to a fixed % : '
                    write(lun2, *) ' Fraction ', ifrac, ' dispersed: ', fractd(ifrac) / idelt * 86400.0, ' per day '
                    write(lun2, *) ' Fraction ', ifrac, ' dispersed: ', fractd(ifrac), ' per time step '
                endif
                if ((fracte(ifrac) + fractd(ifrac)) > 1.0) then
                    write(*, *) ' Warning: more than 100% of floating oil'
                    write(*, *) ' decays per time-step !!!!       '
                    write(lun2, *) ' Warning: more than 100% of floating oil'
                    write(lun2, *) ' decays per time-step !!!!       '
                endif
                write(lun2, *) ' Part of oil that sticks: ', fstick(ifrac)
                write(*, *) ' Part of oil that sticks: ', fstick(ifrac)
                if (fstick(ifrac) < 0.0 .or. fstick(ifrac) > 1.0) then
                    write(*, *) 'Sticking fraction must be between 0 and 1'
                    write(lun2, *) 'Sticking fraction must be between 0 and 1'
                    call stop_exit(1)
                endif
                if (rhooil(ifrac) < 0.1) then
                    write(*, *) 'Oil density not set,value of 980kg/m3 assumed'
                    write(lun2, *) 'Oil density not set,value of 980kg/m3 assumed'
                    rhooil(ifrac) = 980.0
                endif
            enddo

            !     Set initial dispersant application counter
            idisapp = 0
            !     -------------------------------- end initial part --------------------

        endif

        !     Zero accumulators

        isurf = 0
        wsumt = 0.0
        wsumd = 0.0
        wsums = 0.0
        viscsurf = 0.0
        fwatoilsurf = 0.0
        densurf = 0.0
        write (lun2, '(/)')
        do ifrac = 1, nfract
            wevapt(ifrac) = 0.0
            if (ioptd(ifrac)   ==   0) then              !  dispersion rate/day
                fractd(ifrac) = const ((ifrac - 1) * nfcons + npadd + 3) * idelt / 86400.0
            else                                               !  delvigne/sweeny
                fractd(ifrac) = 0.0
            endif
        enddo
        !
        ! ================================plotgrid concentrations for high accuracy=========================================
        !
        !     this part was added in order to imporve the accuracy of the
        !     calculation of concentrations for oil dispersion
        !     it was shown that on the map grid this is not always ok
        !     4/8/2000
        !
        !     note: loop 30 was copied from routine part13 (loop 160 there)
        !           all not related to oil was removed from this loop
        !
        if (lplgr) then
            call part11 (lgrid, xb, yb, num_rows, npart, &
                    mpart, xpart, ypart, xa, ya, &
                    nopart, npwndw, lgrid2, kpart, zpart, &
                    za, locdep, dps, num_layers, num_columns, &
                    tcktot)
            windw1 = window(1)
            windw3 = window(3)
            xpf = (window(2) - windw1) / mmap
            ypf = (window(4) - windw3) / nmap
            amap = 0.0
            do i1 = npwndw, nopart
                ix = int((xa(i1) - windw1) / xpf) + 1
                if (ix <= 0 .or. ix > mmap) cycle
                iy = int((ya(i1) - windw3) / ypf) + 1
                if (iy <= 0 .or. iy > nmap) cycle
                i2 = lgrid(npart(i1), mpart(i1))
                if (i2 <= 1) cycle                            ! NB this probably should be 0 lp
                if (area(i2) <= 0.0) cycle
                ilay = kpart(i1)
                if (use_settling .and. ilay == num_layers) then
                    iseg = (ilay - 2) * num_rows * num_columns + i2
                else
                    iseg = (ilay - 1) * num_rows * num_columns + i2
                endif
                fvolum = surf * (volume(iseg) / area(i2))           ! the volume of one plot grid cell
                if (fvolum <= 0.0) cycle
                do isub = 1, nosubs
                    if (isfile(isub) == 1) cycle
                    am = wpart(isub, i1)
                    ac = am / fvolum
                    if (isub < 3 * nfract) then
                        jsub = mod(isub - 1, 3) + 1
                        if (jsub == 2) then                     ! this is the submerged fraction
                            if (use_settling .and. ilay == num_layers) then
                                ac = am / surf
                            endif
                        elseif (mstick(isub) < 0) then         ! this is the sticky part of this fraction (nr 3)
                            ac = am / surf                             ! <==
                        else
                            ac = am / surf                             ! is this wrong ?
                        endif
                    elseif (use_settling .and. ilay == num_layers) then   ! substances above the three oil fractions
                        ac = am / surf                                ! settled mass of this fraction
                    elseif (mstick(isub) < 0) then            ! the sticky part of non-oil
                        ac = am / surf
                    endif
                    amap(isub, ilay, iy, ix) = amap(isub, ilay, iy, ix) + ac
                end do
            end do
        endif
        !
        ! ==============end oh high accuracy loop===============================================================
        !
        !.. determine evaporation per particle and dispersion per particle
        !.. a particle either disperses or evaporates....

        nevap = 0
        ndisp = 0

        !     Check for dispersant applications
        disapp = .false.
        if (idisapp < ndisapp) then
            if (idisset(idisapp + 1) == itime) then
                disapp = .true.
                idisapp = idisapp + 1
                write(lun2, '(a,i4)') ' Dispersant application # ', idisapp
                if (tydisp == 1) then
                    pdisapp(1:nfract) = efdisp(idisapp, 1:nfract)
                else
                    !              no other function implemented yet!
                endif
            end if
        end if

        !$OMP PARALLEL DO PRIVATE   ( wsum, isub, ifrac, volfracw, tmpfracte, ic, wevap, cdelv, qentr,     &
        !$OMP                         cfloat, ix, iy, ilay, tmpfractd, rrand, dfwatoil, dviso, fw, tp, fbw,&
        !$OMP                         h0wav, hrms, de, wfact, inside, fractdapp, tmpevap, tmpevapold ),    &
        !$OMP             REDUCTION ( + : wsums, wevapt, ndisp, wsumt, wsumd, viscsurf, fwatoilsurf,       &
        !$OMP                             densurf, isurf ),                                                &
        !$OMP             SCHEDULE  ( DYNAMIC, max((nopart-npwndw)/100,1)           )
        do i = npwndw, nopart

            !     sticky part of a fraction does not evaporate (but is accumulated in wsums(ifrac)
            tmpfractd = fractd
            tmpfracte = fracte

            if (nstick > 0) then
                wsum = 0.0
                do isub = 1, nosubs
                    if (mstick(isub) < 0) then
                        wsum = wsum + wpart(isub, i)
                        ifrac = isub / 3
                        if (3 * ifrac /= isub) then
                            write(*, *) ' programming error in oildsp.f '
                            call stop_exit(1)
                        endif
                        wsums(ifrac) = wsums(ifrac) + wpart(ioils(ifrac), i)
                    endif
                enddo
                if (wsum > 0.0) goto 100
            endif
            do ifrac = 1, nfract
                !
                !   f.m.kleissen 8-11-2002
                !    calculate the fraction that will be evaporated during this timestep, making use of the
                !    volatile fraction volfrac(ifrac)

                !   f.m.kleissen 18-11-2002 : there was no feedback of emulsifcation on evaporation
                !   and this is needed. in order to ensure that evaporation reduces to zero when emulsification
                !   has occurred, make the volatile fraction a function of water content.  thus
                !   volfracw=volfrac(ifrac)*(c2-fwatoil)/c2, this is not relevant if the log or sqrt (Fingas) evaporation is used.
                !   this will need to change since the emulsification process will affect evaporation
                volfracw = volfrac(ifrac) * (c2(ifrac) - fwatoil(ifrac, i)) / c2(ifrac)
                tmpfracte(ifrac) = (volfracw - totfe(ifrac, i)) / (1.0 - totfe(ifrac, i)) * &
                        (1.0 - exp(-ioptev(ifrac) / 86400.0 * idelt))
                if (tmpfracte(ifrac) < 0.0) tmpfracte(ifrac) = 0.0
                if (ioptev(ifrac)<-0.5) totfe(ifrac, i) = tmpevap(ifrac) / 100.
                !if evaporation option (Fingas) is used then the Fingas rate can be scaled using the oil fraction (ie 1-waterfraction)

            enddo
            ic = lgrid3(npart(i), mpart(i))
            if (ic > 0) then

                !           Compute the Delvigne/Sweeny entrainment value and store for all fractions and particles

                fw = cb * (wvelo(ic) - wveloi)
                fw = max(fw, 0.0)
                tp = 8.13 * wvelo(ic) / grav
                if(tp > (0.0)) then
                    fbw = fw / tp
                else
                    fbw = 0.0
                endif
                h0wav = 0.243 * wvelo(ic) * wvelo(ic) / grav
                hrms = h0wav / sqrt(2.0)
                de = 0.0034 * rhow * grav * hrms * hrms
                if (de > 0.0) then
                    wfact = de**(0.57) * fbw
                else
                    wfact = 0.0
                endif
                if (kpart(i) <= 0 .or. kpart(i) > num_layers) then
                    write(*, *) ' ipart = ', i, ' k = ', kpart(i)
                    write(*, *) ' k is out of range in partwr '
                    write(lun2, *) ' ipart = ', i, ' k = ', kpart(i)
                    write(lun2, *) ' k is out of range in partwr '
                    call stop_exit(1)
                endif
                do ifrac = 1, nfract
                    if  (kpart(i) == 1) then
                        !     This is the evaporation step in the model
                        if (ioptev(ifrac)>=0)then
                            wevap = wpart(ioilt(ifrac), i) * tmpfracte(ifrac)
                            wpart(ioilt(ifrac), i) = wpart(ioilt(ifrac), i) - wevap
                            wevapt(ifrac) = wevapt(ifrac) + wevap
                        else
                            ! Een korte test op de evapbeschrijving van Fingas (2013) (nat log):
                            ! Percentage evaporated = [.165(%D) + .045(T-15)]ln(t) iptime (npoart) is age of particle
                            ! initial weight of particle is needed.NOte that the time here is in minutes
                            !the timestep is less or equal to idelt(the time step), this means that the particle has just been released
                            ! and initial mass of a particle is when iptime(i)<=idelt
                            if (iptime(i)<=idelt) then
                                wpartini(ifrac, i) = wpart(ioilt(ifrac), i) * volfrac(ifrac)
                            endif
                            tmpevapold = 0.0D0
                            if (iptime(i) > 0) then
                                if (d180(ifrac) >0) then
                                    tmpevap(ifrac) = (0.165 * d180(ifrac) + 0.045 * temp) * log(1.0 + real(iptime(i) / 60.)) ! ln description, time here is in minutes (therefore the /60.)
                                    if (iptime(i)>=2 * idelt) then
                                        tmpevapold = (0.165 * d180(ifrac) + 0.045 * temp) * log(1.0 + (real(iptime(i) - idelt) / 60.)) !evaporated fraction of previous timestep
                                    endif
                                endif
                                if (d180(ifrac) <0) then
                                    tmpevap(ifrac) = (-0.0254 * d180(ifrac) + 0.01 * temp) * sqrt(real(iptime(i) / 60.)) ! sqrt description Percentage evaporated = [.0254(%D) + .01(T-15)]vt
                                    if (iptime(i)>=2 * idelt) then
                                        tmpevapold = (-0.0254 * d180(ifrac) + 0.01 * temp) * sqrt((real(iptime(i) - idelt) / 60.))
                                    endif
                                endif
                            endif
                            wevap = wpartini(ifrac, i) * tmpevap(ifrac) / 100.
                            if (ioptev(ifrac)<=-0.5)then
                                tmpfracte(ifrac) = (tmpevap(ifrac) - tmpevapold) / 100.    !fraction evaporated this timestep, not scaled with the water content
                            elseif (ioptev(ifrac)<-1.5) then
                                tmpfracte(ifrac) = (1.0 - fwatoil(ifrac, i)) * (tmpevap(ifrac) - tmpevapold) / 100.    !fraction evaporated this timestep, scaled with the water content
                            endif

                            wpart(ioilt(ifrac), i) = max(0.0, wpart(ioilt(ifrac), i) - tmpfracte(ifrac) * wpartini(ifrac, i))
                            wevapt(ifrac) = wevapt(ifrac) + min(wpart(ioilt(ifrac), i), tmpfracte(ifrac) * wpartini(ifrac, i)) !dble(wpart(ioilt(ifrac),i))
                        endif
                        !     This is the Delvigne/Sweeny formula for entrainment in the model

                        if (ioptd(ifrac) == 1 .or. ioptd(ifrac) == 2) then                     !  delvigne/sweeny
                            if (viso(ifrac, i) < 125.0) then
                                cdelv = fac1 * viso(ifrac, i)**(-0.0658)
                            elseif (viso(ifrac, i) < 10000.0)then        !when visc exceeds 10000 then this process stops
                                cdelv = fac2 * viso(ifrac, i)**(-1.1951)
                            else
                                cdelv = 0.0
                            endif
                            cdelv = cdelv * voil
                            qentr = cdelv * wfact
                            cfloat = conc(ioilt(ifrac), ic)                  ! from the map file
                            if (lplgr) then                               ! but if possible, from the
                                ix = int((xa(i) - windw1) / xpf) + 1         ! more detailed plot grid
                                iy = int((ya(i) - windw3) / ypf) + 1
                                if (ix > 0 .and. ix <= mmap .and.    &
                                        iy > 0 .and. iy <= nmap) then
                                    ilay = kpart(i)
                                    isub = ioilt(ifrac)
                                    cfloat = amap(isub, ilay, iy, ix)
                                endif
                            endif
                            if (cfloat > 0.0) then
                                tmpfractd(ifrac) = qentr * idelt / rhooilv(ifrac, i) / hmin
                                if (tmpfractd(ifrac) > 1.0) tmpfractd(ifrac) = 1.0
                            else
                                tmpfractd(ifrac) = 0.0
                            endif
                        endif

                        !     Extra effect of dispersant?
                        fractdapp = tmpfractd(ifrac)
                        if (disapp) then
                            call pinpok(xa(i), ya(i), nrowsdis(idisapp), xpoldis(1:nrowsdis(idisapp), idisapp), &
                                    ypoldis(1:nrowsdis(idisapp), idisapp), inside)
                            if (inside == 1) then
                                fractdapp = 1.0 - ((1.0 - tmpfractd(ifrac)) * (1.0 - pdisapp(ifrac)))
                            end if
                        end if

                        !     This is the dispersion step in the model
                        rrand = rnd(rseed)
                        if (rrand < fractdapp) then
                            if (wpart(ioilt(ifrac), i) > 0.0) then
                                wpart(ioild(ifrac), i) = wpart(ioilt(ifrac), i)
                                wpart(ioilt(ifrac), i) = 0.0
                                ndisp = ndisp + 1
                            endif
                        endif
                    endif

                    !     The different parts of the fractions are accumulated here
                    wsumt (ifrac) = wsumt(ifrac) + wpart(ioilt(ifrac), i)
                    wsumd (ifrac) = wsumd(ifrac) + wpart(ioild(ifrac), i)
                    if (wpart(ioilt(ifrac), i) > 0.0)then
                        viscsurf   (ifrac) = viscsurf   (ifrac) + viso   (ifrac, i)
                        fwatoilsurf(ifrac) = fwatoilsurf(ifrac) + fwatoil(ifrac, i)
                        densurf    (ifrac) = densurf    (ifrac) + rhooilv(ifrac, i)
                        isurf(ifrac) = isurf(ifrac) + 1
                    endif
                end do
            endif

            !     This apparently also happens if particle i is not in an active grid cell ( ic <= 0 )

            do ifrac = 1, nfract
                if ((totfe(ifrac, i) > evemul(ifrac)).and.ic>0) then
                    dfwatoil = c1(ifrac) * (wvelo(ic) + 1.0) * (wvelo(ic) + 1.0) * &
                            (1 - fwatoil(ifrac, i) / c2(ifrac)) * idelt
                else
                    dfwatoil = 0.0
                endif
                fwatoil(ifrac, i) = fwatoil(ifrac, i) + dfwatoil
                dviso = visowat(ifrac) * &
                        exp(2.5 * fwatoil(ifrac, i) / (1.0 - 0.65 * fwatoil(ifrac, i))) - &
                        visowat(ifrac)
                if (ioptev(ifrac)>=0)then
                    totfe(ifrac, i) = totfe(ifrac, i) + tmpfracte(ifrac) * (1.0 - totfe(ifrac, i))  ! when fixed firt order constant for evap is used
                elseif (ioptev(ifrac)<-0.5) then
                    totfe(ifrac, i) = tmpevap(ifrac) / 100. ! when ln or sqrt function for evaporation is used
                endif
                if (viso(ifrac, i) > 500.0) then
                    viso(ifrac, i) = visowat(ifrac) * exp(10.0 * totfe(ifrac, i)) + dviso
                else
                    viso(ifrac, i) = visowat(ifrac) * exp(totfe(ifrac, i)) + dviso
                endif
                rhooilv(ifrac, i) = rhow * fwatoil(ifrac, i) + rhooil(ifrac) * (1.0 - fwatoil(ifrac, i))         &
                        * (1.0 + cde * totfe(ifrac, i)) * (1.0 - cdt * (temp0 - temp0)) !if we work with temp dependent density then change temp0-temp0 into temp-reftemp (reference temperature)
                !the var temp is used for the calculation of the evaporation
                !            endif
            end do

            !     End of the loop ovver all particles i

            100 continue
        end do
        !$OMP END PARALLEL DO

        write(lun2, 1010) nopart - npwndw + 1, nevap, ndisp
        write(lun2, '(/)')
        timlc = timlc + idelt / 3600.

        do ifr = 1, nfract

            !     f.m. kleissen 10-12-2002 : update the evaporated mass. mass calculation is taken ouside particle loop
            !     because rounding errors cause mass balance problems when using larger number of particles

            wsume(ifr) = wsume(ifr) + wevapt(ifr)
            totmas = wsumt(ifr) + wsumd (ifr) + wsume(ifr) + wsums(ifr)
            if (isurf(ifr) > 0) then
                fwatoilsurf(ifr) = fwatoilsurf(ifr) / isurf(ifr)
                viscsurf   (ifr) = viscsurf   (ifr) / isurf(ifr)
                densurf    (ifr) = densurf    (ifr) / isurf(ifr)
                write(*, 1020) substi((ifr - 1) * 3 + 1), &
                        wsumt(ifr), wsumd(ifr), wsume(ifr), wsums(ifr), &
                        totmas, viscsurf(ifr), fwatoilsurf(ifr), &
                        densurf(ifr)
                write(lun2, 1020) substi((ifr - 1) * 3 + 1), &
                        wsumt(ifr), wsumd(ifr), wsume(ifr), wsums(ifr), &
                        totmas, viscsurf(ifr), fwatoilsurf(ifr), &
                        densurf(ifr)
            else
                fwatoilsurf(ifr) = -999.999
                viscsurf   (ifr) = -999.999
                densurf    (ifr) = -999.999
                write(*, 1030) substi((ifr - 1) * 3 + 1), &
                        wsumt(ifr), wsumd(ifr), wsume(ifr), wsums(ifr), &
                        totmas
                write(lun2, 1030) substi((ifr - 1) * 3 + 1), &
                        wsumt(ifr), wsumd(ifr), wsume(ifr), wsums(ifr), &
                        totmas
            endif
            write(luncsv(ifr), 1040) timlc, wsumt      (ifr), wsumd   (ifr), &
                    wsume   (ifr), wsums      (ifr), totmas, &
                    viscsurf(ifr), fwatoilsurf(ifr), densurf (ifr)

        end do

        !     end of subroutine

        if (timon) call timstop (ithndl)
        return

        !     formats

        1000 format(1x, a, 5(a), 3(a))
        1010 format (6x, 'Total number particles                      :', i6, / &
                6x, 'Number of particles, evaporated in this step:', i6, / &
                6x, 'Number of particles, dispersed in this step :', i6, /)
        1020 format(9x, 'Oil fraction ', a, /                                             &
                12x, 'Total mass floating   oil                 : ', es15.7, ' kg ' /  &
                12x, 'Total mass dispersed  oil                 : ', es15.7, ' kg ' /  &
                12x, 'Total mass evaporated oil                 : ', es15.7, ' kg ' /  &
                12x, 'Total mass sticking   oil                 : ', es15.7, ' kg ' /  &
                12x, 'Total mass (float + disp + evap + sticky) : ', es15.7, ' kg ':/  &
                12x, 'Average surface oil viscosity             : ', es15.7, ' cSt '/  &
                12x, 'Average surface oil water content         : ', es15.7, ' - '  /  &
                12x, 'Average surface oil density               : ', es15.7, ' kg/m3 ')
        1030 format(6x, 'Oil fraction ', a, /                                    &
                10x, 'Total mass floating   oil                 : ', es15.7, ' kg ' /  &
                10x, 'Total mass dispersed  oil                 : ', es15.7, ' kg ' /  &
                10x, 'Total mass evaporated oil                 : ', es15.7, ' kg ' /  &
                10x, 'Total mass sticking   oil                 : ', es15.7, ' kg ' /  &
                10x, 'Total mass (float + disp + evap + sticky) : ', es15.7, ' kg ')
        1040 format(1x, f12.2, 5(',', e22.6), 3(',', e29.6))

    end subroutine
end module
