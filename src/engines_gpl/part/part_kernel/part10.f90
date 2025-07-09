!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module part10_mod
    use m_p10ddb
    use m_stop_exit
    use m_part11sp
    use m_part11
    use m_boombounce

    !
    !  module declarations
    !
contains
    subroutine part10 (lgrid, volume, flow, dx, dy, &
            area, angle, num_rows, mnmaxk, idelt, &
            nopart, npart, mpart, xpart, ypart, &
            zpart, iptime, rough, drand, lgrid2, &
            zmodel, laytop, laybot, &
            wvelo, wdir, decays, wpart, pblay, &
            npwndw, vdiff, nosubs, dfact, modtyp, &
            t0buoy, abuoy, kpart, num_columns, layt, &
            wsettl, depth, ldiffz, ldiffh, &
            acomp, accur, xcor, ycor, &
            tcktot, lun2, alpha, mapsub, nfract, &
            taucs, tauce, chezy, rhow, use_settling, &
            mstick, nstick, ioptdv, cdisp, dminim, &
            fstick, defang, floil, xpart0, ypart0, &
            xa0, ya0, xa, ya, npart0, &
            mpart0, za, locdep, dps, num_layers, &
            vrtdsp, stickdf, subst, nbmax, nconn, &
            conn, tau, caltau, nboomint, iboomset, &
            tyboom, efboom, xpolboom, ypolboom, nrowsboom, &
            itime, v_swim, d_swim)
        !
        !    CALCULATES PARTICLE MOTION FROM ADVECTION, DISPERSION AND SETTLING
        !                 (per time step)


        !     logical unit numbers  : standard output: error messages
        !                             lun2           : error and debug messages


        !     subroutines called    : stop_exit  - stops with return code (typical 1 for stop on error)
        !                             part07  - not documented here
        !                             part11  - not documented here


        !     functions   called    : rnd     - random number generator
        !                             vdisp   - not documented here
        !
        !
        !  data definition module(s)
        !
        use m_waq_precision        ! single/double precision
        use timers           ! performance timer
        use typos
        use grid_search_mod
        use spec_feat_par    ! special feature parameters
        use random_generator
        use m_part_modeltypes
        !
        implicit none

        !**   parameters used for dimensioning

        integer(int_wp), intent(in) :: layt                ! number of layers of hydr. database
        integer(int_wp), intent(in) :: num_columns                ! second grid dimension
        integer(int_wp), intent(in) :: mnmaxk              ! total number of active grid cells
        integer(int_wp), intent(in) :: num_rows                ! first grid dimension
        integer(int_wp), intent(in) :: nopart              ! total number of particles
        integer(int_wp), intent(in) :: nosubs              ! number of substances per particle

        !**   other parameters

        integer(int_wp), intent(in) :: idelt               ! time step size in seconds
        integer(int_wp), intent(in) :: ioptdv              ! if 0 constant vertical diffusion &
        ! if 1 depth averaged algebraic model
        integer(int_wp), pointer :: lgrid (:, :)          ! grid with active grid numbers, negatives for open boundaries
        integer(int_wp), pointer :: lgrid2(:, :)          ! total grid with grid numbers
        logical, intent(in) :: zmodel                 ! layer type
        integer(int_wp), intent(in) :: laytop(:, :)           ! highest active layer in z-layer model
        integer(int_wp), intent(in) :: laybot(:, :)           ! deepest active layer in z-layer model
        integer(int_wp), intent(in) :: lun2                   ! unit number debug in formation file
        integer(int_wp), pointer :: mapsub(:)             ! index for substances, used for oil
        integer(int_wp), intent(in) :: modtyp                 ! 1 = tracer model              &
        ! 2 = 2-layer temperature model &
        ! 3 = obsolete                  &
        ! 4 = oil model                 &
        ! 5 = 1-layer temperature model &
        ! 6 = plastics model
        ! 7 = abm model
        integer(int_wp), intent(in) :: nfract              ! nr of oil fractions, each fraction 3 substances &
        ! floating, dispersed and sticked
        integer(int_wp), intent(in) :: npwndw              ! first active particle
        integer(int_wp), intent(in) :: nstick              ! number of sticked particles !!!!! in !!!!!
        integer(int_wp), pointer :: iptime(:)         ! particle age in seconds
        integer(int_wp), pointer :: kpart (:)         ! third grid index of the particles
        integer(int_wp), pointer :: mpart (:)         ! second grid index of the particles
        integer(int_wp), pointer :: mpart0(:)         ! second grid index particles for previous time step
        integer(int_wp), pointer :: mstick(:)         ! which active substances can sticking (>0) and what is inactive partner?
        ! j = mstick(i), j = inactive, i = active ; if j = 0 no sticking
        ! if j is negative then i itself is sticking
        integer(int_wp), intent(inout) :: num_layers               ! number of layers == layt
        integer(int_wp), pointer :: npart (:)         ! first  grid index of the particles
        integer(int_wp), pointer :: npart0(:)         ! first  grid index particles for previous time step
        integer(int_wp), pointer :: floil (:)         ! contains values 1 or 0
        !
        logical, intent(in) :: acomp               ! use an analytical function for umagi
        logical, intent(in) :: ldiffh              ! horizontal diffusion is on/off
        logical, intent(in) :: ldiffz              ! vertical diffusion is on/off
        logical, intent(in) :: use_settling              ! if on deposition/erosion may occur at the bed
        !
        real   (sp), pointer :: abuoy (:)         ! dispersion coefficient buoyancy
        real   (sp), intent(in) :: accur               ! accuracy limit taylor expansion
        real   (sp), intent(in) :: alpha               ! scale factor vertical diffusion
        real   (sp), pointer :: angle (:)         ! angle with horizontal
        real   (sp), pointer :: area  (:)        ! horizontal surface areas
        real   (sp), intent(in) :: cdisp               ! constant vertical diffusivity in m2/s
        real   (sp), intent(in) :: dminim              ! minimum value vertical diffusivity in m2/s
        real   (sp), pointer :: decays(:)        ! decay coefficients of substances in 1/day
        real   (sp), pointer :: depth (:)        ! depth of segment  (in m)
        real   (sp), intent(in) :: drand (3)         ! drand(1) is 2.0*sqrt(dt*a) in D = a*t^b &
        ! drand(2) is 0.5*b          in D = a*t^b &
        ! drand(3) is winddrag coefficient in %
        real   (sp), pointer :: flow  (:)      ! all flows
        real   (sp), pointer :: fstick(:)         ! part of mass that sticks to land
        real   (sp), intent(in) :: pblay               ! relative thickness of bottom layer ( 0.0 - 1.0 )
        real   (sp), intent(in) :: rhow                ! density of water in g/l (= kg/m3)
        real   (sp), intent(in) :: rough               ! bottom roughness length (m)
        real   (sp), pointer :: t0buoy(:)         ! t0-coeff buoyancy
        real   (sp), intent(in) :: tauce               ! critical shear stress for erosion (pa)
        real   (sp), intent(in) :: taucs               ! critical shear stress for sedimentation (pa)
        real   (sp), pointer :: volume(:)         ! volumes of all computational elements
        real   (dp), intent(in) :: wdir (:)            ! wind direction from north
        real   (dp), intent(in) :: wvelo(:)            ! wind velocity
        real   (sp), pointer :: xcor  (:)         ! bottom coordinate x
        real   (sp), pointer :: ycor  (:)         ! bottom coordinate y
        real   (sp), intent(inout) :: chezy               ! chezy coefficient (is set to 50 if <= 1.0)
        real   (sp), intent(inout) :: defang              ! deflection angle for 3d oil simulations
        ! enters in degrees, becomes radians
        real   (sp), pointer :: dfact (:)         ! decay factor ( is exp(-decays*t) )
        real   (sp), pointer :: dps   (:)         ! depths
        real   (sp), pointer :: dx    (:)         ! dx of the computational elements
        real   (sp), pointer :: dy    (:)         ! dy of the computational elements
        real   (sp), pointer :: locdep(:, :)      ! depth per layer
        real   (sp), pointer :: tcktot(:)         ! relative thickness of the layers
        real   (sp), pointer :: wpart (:, :)       ! weight factors of the subs per particle
        real   (sp), pointer :: wsettl(:)         ! settling per particle
        real   (sp), pointer :: xa    (:)         !
        real   (sp), pointer :: xa0   (:)         !
        real   (sp), pointer :: xpart (:)         ! x-value (0.0-1.0) first  direction within grid cell
        real   (sp), pointer :: xpart0(:)         ! x of particles for previous time step
        real   (sp), pointer :: ya    (:)         !
        real   (sp), pointer :: ya0   (:)         !
        real   (sp), pointer :: ypart (:)         ! y-value (0.0-1.0) second direction within grid cell
        real   (sp), pointer :: ypart0(:)         ! y of particles for previous time step
        real   (sp), pointer :: za    (:)         !
        real   (sp), pointer :: zpart (:)         ! z-value (0.0-1.0) third  direction within grid cell
        real   (sp), pointer :: vdiff (:)         ! vertical diffusion - work array
        real   (sp), pointer :: vrtdsp(:, :)       ! storage of vert disp info for debugging
        character(len = *), pointer :: subst (:)         ! substance names per substance &
        ! and per layer for the plo file
        integer(int_wp), intent(in) :: stickdf             ! if 1 oil sticks at drying flats
        integer(int_wp), intent(in) :: nbmax         ! highest regular open boundary number
        integer(int_wp), intent(in) :: nconn         ! number of interdomain connections
        type(pnt), intent(in) :: conn (nconn)  ! array with interdomain connections
        real   (sp), pointer :: tau   (:)   ! tau
        logical, intent(in) :: caltau        ! if true, tau must be calculated

        integer(int_wp), intent(in) :: nboomint      ! number of boom introductions
        integer(int_wp), pointer :: iboomset(:)         ! timing of boom introduction
        integer(int_wp), intent(in) :: tyboom        ! type of boom effectiveness parameter
        real     (sp), pointer :: efboom (:, :)  ! effectiveness parameter of boom per oil type
        real     (sp), pointer :: xpolboom (:, :)! x-coordinates of boom polygon
        real     (sp), pointer :: ypolboom (:, :)! y-coordinates of boom polygon
        integer  (int_wp), pointer :: nrowsboom (:)  ! length of boom polygon

        integer(int_wp), intent(in) :: itime

        real   (sp), pointer :: v_swim(:)         ! horizontal swimming velocity m/s
        real   (sp), pointer :: d_swim(:)         ! horizontal swimming direction (degree)

        !**   local parameters

        integer(int_wp) :: icounz                  ! count the number of vertical bounces
        integer(int_wp) :: icvis                   ! strange construct to avoid endless operation, cyclic counter
        integer(int_wp) :: icvist                  ! strange construct to avoid endless operation, total counter
        integer(int_wp) :: idep                    ! layer offset in the hydrodynamic arrays
        integer(int_wp) :: idepm1                  ! layer offset - one layer in the hydrodynamic arrays
        integer(int_wp) :: idx                     ! boolean x direction of transport -1, 0, +1
        integer(int_wp) :: idy                     ! boolean y direction of transport -1, 0, +1
        integer(int_wp) :: idz                     ! boolean z direction of transport -1, 0, +1
        integer(int_wp) :: ierror                  ! needed to call part07
        integer(int_wp) :: ifract                  ! loop counter for nfract
        integer(int_wp) :: ipart                   ! loop counter particle loop
        integer(int_wp) :: isub                    ! loop counter nosubs
        integer(int_wp) :: itdelt                  ! delta-t of the particle for smooth loading
        integer(int_wp) :: ivisit(4)             ! part of the strange construct of icvis
        integer(int_wp) :: jsub                    ! pointer containing mstick(isub)
        integer(int_wp) :: kd                      ! loop counter of vertical layers
        integer(int_wp) :: kp                      ! k of the particle
        integer(int_wp) :: kpp                     ! local k of the particle
        integer(int_wp) :: ktopp                   ! local k top of the particle
        integer(int_wp) :: kbotp                   ! local k bot of the particle
        integer(int_wp) :: mp                      ! m of the particle
        integer(int_wp) :: n0                      ! segment number 2d
        integer(int_wp) :: n03d                    ! segment number 3d
        integer(int_wp) :: n0old                   ! old value of segment number 2d
        integer(int_wp) :: n0new                   ! new value of segment number 2d
        integer(int_wp) :: n1                      ! one back from n0 in first index
        integer(int_wp) :: n2                      ! one back from n0 in second index
        integer(int_wp) :: ninact                  ! summation counter inactive particles
        integer(int_wp) :: np                      ! nr of this particle
        integer(int_wp) :: nstpar                  ! summation counter sticking particles
        integer(int_wp) :: nopart_sed              ! no. of particles settled into bed layer (per time step)
        integer(int_wp) :: nopart_ero              ! no. of particles eroded from bed layer (per time step)
        logical :: coriol                  ! = abs( defang ) >= 1.0e-6
        logical :: dstick                  ! logical that determines sticking
        logical :: ldispo                  ! vertical diffusion is on (true) or off (false) for oil particle
        logical :: lstick                  ! keeps the possibility to stick
        logical :: oilmod                  ! = modtyp == model_oil
        logical :: threed                  ! = layt > 1
        logical :: twolay                  ! = modtyp == model_two_layer_temp
        real(dp) :: a                       ! tsja
        real(sp) :: codef                   ! cosine of deflection angle
        real(sp) :: dxx                     ! delta x with deflected oil
        real(sp) :: dyy                     ! delta y with deflected oil
        real(sp) :: nrms                    ! number of vertical diffusion values in dsprms
        real(sp) :: sidef                   ! sine   of deflection angle
        real(sp) :: xx                      ! new x with deflected oil
        real(sp) :: yy                      ! new y with deflected oil
        real(sp) :: abuac                   ! actual value of abuoy(ipart) ( * sqrt(ddfac)
        real(sp) :: c1                      ! depth curve of the wind component
        real(sp) :: c2                      ! depth curve of the velocities
        real(sp) :: c2g                     ! = grav / chezy / chezy
        real(sp) :: c3                      ! something undeterminded yet, fixed at 1.0
        real(sp) :: cdrag                   ! wind drag in fraction = drand(3)
        real(sp) :: chi0                    ! variable in correction process
        real(sp) :: chi1                    ! variable in correction process
        real(sp) :: dax                     ! delta of diffusive spreading x
        real(sp) :: day                     ! delta of diffusive spreading y
        real(sp) :: ddfac                   ! control variable for smooth loading
        real(sp) :: deltt                   ! real value of itdelt, remainder of time step
        real(sp) :: deppar                  ! depth of particle from the surface
        real(sp) :: depth1                  ! total depth of the old segment
        real(sp) :: depth2                  ! total depth of the new segment
        real(sp) :: depthl                  ! depth of the volume(segment)
        real(sp) :: disp                    ! vertical diffusion
        real(sp) :: dran1                   ! actual value of drand(1) or drand(1)*sqrt(ddfac)
        real(sp) :: dred                    ! depth reduction factor for 2 layer modelling
        real(sp) :: dspmax                  ! maximum vertical diffusion value
        real(sp) :: dspmin                  ! minimum vertical diffusion value
        real(dp) :: dsprms                  ! sum of squares of vertical diffusion value
        real(sp) :: dvz                     ! vertical displacement due to vertical diffusion
        real(sp) :: dvzs                    ! vertical displacement due to settling
        real(sp) :: dvzt                    ! total vertical displacement
        real(sp) :: dxp                     ! dx at location of the particle
        real(sp) :: dyp                     ! dy at location of the particle
        real(sp) :: f1                      ! helpvariable rough / depth
        real(sp) :: pstick                  ! used in fraction computation with oil
        real(sp) :: ptlay                   ! relative fraction of top layer in twolayer system is 1.0 - pblay
        real(sp) :: rtim                    ! timestep size that can be set minimum of x, y and z
        real(sp) :: rtim1                   ! unclear help variable associated with rtim
        real(sp) :: rtimx                   ! timestep size that can be set in x direction
        real(sp) :: rtimy                   ! timestep size that can be set in y direction
        real(sp) :: rtimz                   ! timestep size that can be set in z direction
        real(sp) :: sangl                   ! angle of the computational element
        real(sp) :: sq6                     ! = sqrt(6.0)
        real(sp) :: t0                      ! helpvariable with initial time step buoyant spreading
        real(sp) :: tp                      ! real value of iptime(ipart)
        real(sp) :: trp                     ! horizontal random walk
        real(sp) :: twopi                   ! 2 * pi
        real(sp) :: ubstar                  ! bottom  friction velocity
        real(sp) :: ubstar_b                ! bottom  friction velocity at sediment bed
        real(sp) :: uecrit                  ! critical shearstress velocity for erosion
        real(sp) :: umagi                   ! variable for strange correction process
        real(sp) :: umagp                   ! variable for strange correction process
        real(sp) :: uscrit                  ! critical shearstress velocity for sedimentation
        real(sp) :: uwstar                  ! surface friction velocity
        real(sp) :: vol                     ! value of volume (speed)
        real(sp) :: vvx                     ! particle velocity in x dir.
        real(sp) :: vvy                     ! particle velocity in y dir.
        real(sp) :: vvz                     ! particle velocity in z dir.
        real(sp) :: vx                      ! particle velocity in x dir. normalized
        real(sp) :: vx0                     ! particle velocity in x dir. 0
        real(sp) :: vx1                     ! particle velocity in x dir. 1
        real(sp) :: vxnew                   ! particle velocity in x dir. corrected
        real(sp) :: vxr                     ! particle velocity in x dir. real
        real(sp) :: vxw                     ! particle velocity in x dir. winddriven
        real(sp) :: vy                      ! particle velocity in y dir. normalized
        real(sp) :: vy0                     ! particle velocity in y dir. 0
        real(sp) :: vy1                     ! particle velocity in y dir. 1
        real(sp) :: vynew                   ! particle velocity in y dir. corrected
        real(sp) :: vyr                     ! particle velocity in y dir. real
        real(sp) :: vyw                     ! particle velocity in y dir. winddriven
        real(sp) :: vz                      ! particle velocity in z dir. normalized
        real(sp) :: vz0                     ! particle velocity in z dir. 0
        real(sp) :: vz1                     ! particle velocity in z dir. 1
        real(sp) :: vznew                   ! particle velocity in z dir. corrected
        real(sp) :: vzs                     ! vertical velocity due to settling
        real(sp) :: wdirr                   ! is wind direction in radians
        real(sp) :: wstick                  ! used in fraction computation with oil
        real(sp) :: wsum                    ! used to sum the weights of a particle
        real(sp) :: xnew                    ! new x value
        real(sp) :: xp                      ! x of the particle
        real(sp) :: xpold                   ! old x of the particle needed for the correction routine
        real(sp) :: ynew                    ! new y value
        real(sp) :: yp                      ! y of the particle
        real(sp) :: ypold                   ! old y of the particle needed for the correction routine
        real(sp) :: znew                    ! new z value
        real(sp) :: zp                      ! z of the particle
        real(sp) :: zpabs                   ! absolute z of the particle
        real(sp) :: zp2                     ! help variable to correct z of the particle for twolay
        real(sp) :: zpold                   ! old z of the particle needed for the correction routine
        integer(int_wp) :: n03d2                   ! help variable sequence number of cell in next layer
        real   (real_wp) :: disp2                   ! help variable diffusion in next layer
        real   (real_wp) :: pbounce                 ! probability that a particle will NOT bounce
        integer(int_wp) :: ddshift                 ! is it n (1) or m (2) direction
        real(sp) :: xpnew                   ! potential new x of the particle
        real(sp) :: ypnew                   ! potential new y of the particle
        real(sp) :: xrest                   ! residual x offset
        real(sp) :: yrest                   ! residual y offset
        real(sp) :: dxpnew                  ! dxp new potential cell
        real(sp) :: dypnew                  ! dyp new potential cell
        integer(int_wp) :: mpnew                   ! potential new m of the particle
        integer(int_wp) :: npnew                   ! potential new n of the particle
        logical :: booms                   ! indicator for the precence of booms
        logical :: boomseffective          ! indicator for effective precence of booms per particle
        integer(int_wp) :: iboomint                ! counter for boom introductions
        real(real_wp) :: pboomcatch(100)         ! chance for boom to catch particle

        real(real_wp) :: xaold                   ! old real world x-coordinate
        real(real_wp) :: yaold                   ! old real world y-coordinate
        real(real_wp) :: xanew                   ! new real world x-coordinate
        real(real_wp) :: yanew                   ! new real world y-coordinate
        logical :: catch
        real(sp) :: xacatch
        real(sp) :: yacatch
        logical :: bounce
        logical :: bouncefirsttry
        real(sp) :: xabounce
        real(sp) :: yabounce
        real(sp) :: sdir                ! swimming direction
        real(sp) :: displacement        ! swimming displacement
        real(sp) :: dx_swim             ! dx swimming displacement
        real(sp) :: dy_swim             ! dy swimming displacement

        integer(int_wp) :: nboomtry
        integer(int_wp) :: nscreenstry
        logical :: screensfirsttry
        logical :: leftside

        !**   maintain all values

        save

        !**   data assigments

        real(sp) :: cd = 1.3e-03
        real(sp) :: grav = 9.81
        real(sp) :: rhoair = 1.25
        real(dp) :: rseed = 0.5d+00
        real(sp) :: zsurf = 0.001
        logical :: first = .true.
        logical :: lbott = .true.
        logical :: lland = .true.

        logical :: timon_org
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part10", ithndl)

        !**   initialisations

        if (first) then
            first = .false.
            !     Set initial booms flag and booms introduction counter
            booms = .false.
            iboomint = 0
            sq6 = sqrt(6.0)
            twopi = 8.0 * atan(1.0)
            defang = defang * twopi / 360.0    !  deflection angle oil modelling
            coriol = abs(defang) >= 1.0e-6   !  deflection from the equator aparently
            twolay = modtyp == model_two_layer_temp
            oilmod = modtyp == model_oil
            threed = layt > 1
            cdrag = drand(3) / 100.0          !  wind drag as a fraction
            ptlay = 1.0 - pblay
            dspmin = 1.0e+10
            dspmax = 0.0
            dsprms = 0.0
            nrms = 0.0
            if (lbott) then
                write (lun2, '(6x,a)') &
                        'Bottom sticking due to diffusion is included '
            else
                write (lun2, '(6x,a)')  &
                        'Bottom sticking due to diffusion is not included '
            endif
            if (lland) then
                write (lun2, '(6x,a)') &
                        'Sticking at land due to diffusion is included '
            else
                write (lun2, '(6x,a)') &
                        'Sticking at land due to diffusion is not included '
            endif
            if (.not. ldiffz) &
                    write (lun2, '(6x,a)') 'No vertical diffusivity in part10 '
            if (.not. ldiffh) &
                    write (lun2, '(6x,a)') 'No horizontal diffusivity in part10'
            if (chezy <= 1.0) then
                chezy = 50.0
                write(*, *) 'Chezy is missing: assumed is a chezy of: ', chezy
            endif
            if (rhow <= 1.0) then
                write(*, *) 'Rho-w is missing ??'
                call stop_exit(1)
            endif
        endif

        !     Check for (update of) boom introductions
        if (iboomint < nboomint) then
            if (iboomset(iboomint + 1) == itime) then
                booms = .true.
                iboomint = iboomint + 1
                write(lun2, '(a,i4)') ' Booms introduction # ', iboomint
                if (tyboom == 1) then
                    pboomcatch(1:nfract) = 1 - ((1 - efboom(iboomint, 1:nfract)) * idelt / 86400.0)
                else
                    !              no other function implemented yet!
                endif
            end if
        end if

        !**   to avoid repeated calculations

        c2g = grav / chezy / chezy
        uscrit = sqrt(taucs / rhow)
        uecrit = sqrt(tauce / rhow)
        do isub = 1, nosubs
            dfact(isub) = exp(-decays(isub) * idelt / 86400.0)
            write(lun2, '(a,a,14x,a,es15.7,a,es15.7,a)')  &
                    '      Decay ', subst(isub), ' : ', dfact(isub), &
                    ' [coeff = ', decays(isub), ']'
        enddo

        !**   store 'old' particles positions

        if (threed .and. oilmod .and. coriol) then
            do ipart = 1, nopart
                xpart0(ipart) = xpart(ipart)
                ypart0(ipart) = ypart(ipart)
                npart0(ipart) = npart(ipart)
                mpart0(ipart) = mpart(ipart)
            enddo
        endif

        !**   loop over the particles

        ninact = 0
        nstpar = 0

        nopart_sed = 0
        nopart_ero = 0

        timon_org = timon
        timon = .false.
        !$OMP PARALLEL DO PRIVATE ( np, mp, kp, kpp, ktopp, kbotp, n0, n0old, a, n03d, xp, &
        !$OMP                       yp, zp, tp, itdelt, ddfac, dran1, abuac, deltt, dred,  &
        !$OMP                       kd, icvis, icvist, ivisit, lstick, wsum, isub, jsub,   &
        !$OMP                       ifract, pstick, wstick, ldispo, trp, t0, dax, day,     &
        !$OMP                       n1, n2, dxp, dyp, depth1, idep, vol, vy0, vy1,         &
        !$OMP                       vx0, vx1, vvx, vvy, vx, vy, vxr, vyr, ubstar, ubstar_b,&
        !$OMP                       vz0, vz1, disp, dvz, depthl, dvzs, dvzt, vzs, icounz,  &
        !$OMP                       znew, sangl, zp2, c1, f1, c2, c3, xnew,                &
        !$OMP                       idepm1, vvz, vz, umagp, rtimx, idx, rtimy, idy,        &
        !$OMP                       rtimz, idz, rtim, rtim1, xpold, ypold, zpold,          &
        !$OMP                       chi0, vxnew, vynew, vznew, chi1, ynew, vxw, vyw,       &
        !$OMP                       deppar, yy, n0new, depth2, dstick, xx, n03d2, disp2,   &
        !$OMP                       pbounce, xpnew, ypnew, xrest, yrest, dxpnew,           &
        !$OMP                       dypnew, mpnew, npnew, ddshift, wdirr, uwstar, nboomtry,&
        !$OMP                       boomseffective, bouncefirsttry, xaold, yaold, xanew,   &
        !$OMP                       yanew, xacatch, yacatch, catch, xabounce, yabounce,    &
        !$OMP                       bounce, screensfirsttry, nscreenstry, leftside ,       &
        !$OMP                       sdir, displacement, dx_swim, dy_swim) ,                &
        !$OMP           REDUCTION ( +   : ninact, nstpar, nopart_ero, nopart_sed,          &
        !$OMP                             nrms  , dsprms ) ,                               &
        !$OMP           REDUCTION ( MIN : dspmin ) , REDUCTION ( MAX : dspmax ),           &
        !$OMP           SCHEDULE  ( DYNAMIC, max((nopart-npwndw)/100,1)           )

        do ipart = npwndw, nopart

            !**     retrieve particle characteristics

            np = npart(ipart)
            mp = mpart(ipart)
            kp = kpart(ipart)
            kpp = kp

            if (twolay) kpp = 1             !   two layers are dealt with as one
            !   settling particles at the bed come in layer layt + 1
            if(zmodel) then
                kpp = min0 (kp, laybot(np, mp))           !   kpp is a pointer that won't reach the bed..
                ktopp = laytop(np, mp)
                kbotp = laybot(np, mp)
            else
                kpp = min0 (kp, layt)           !   kpp is a pointer that won't reach the bed..
                ktopp = 1
                kbotp = layt
            endif
            n0 = lgrid(np, mp)
            if (n0 < 1) then             !   not an active grid-cell
                ninact = ninact + 1
                npart(ipart) = 1
                mpart(ipart) = 1
                kpart(ipart) = 1
                do isub = 1, nosubs
                    wpart(isub, ipart) = 0.0
                enddo
                a = rnd(rseed)                 !   compatibility
                goto 90                        !   next particle
            endif
            n03d = n0 + (kpp - 1) * num_rows * num_columns     !   3d
            xp = xpart (ipart)
            yp = ypart (ipart)
            zp = zpart (ipart)
            tp = real(iptime(ipart))
            itdelt = idelt
            ddfac = 2.0
            dran1 = drand(1)
            abuac = abuoy(ipart)
            if (tp < 0.0) then           !   adaptations because of smooth loading
                tp = 0.0
                itdelt = idelt + iptime(ipart)
                ddfac = real(itdelt) / real(idelt)
                dran1 = dran1 * sqrt(ddfac)
                abuac = abuac * sqrt(ddfac)
            endif

            deltt = real(itdelt)
            dred = 1.0                       !   depth reduction factor for 2-layer models
            if (twolay) then
                if (kp == 1) then
                    dred = ptlay
                elseif (kp == 2) then
                    dred = pblay
                else
                    write (*, *) ' The layer-number is too large for modtyp=2 (model_two_layer_temp)'
                    write(lun2, *) ' The layer-number is too large for modtyp=2 (model_two_layer_temp)'
                    call stop_exit(1)
                endif
            endif

            !**      strange construction to avoid infinite calculation

            icvis = 1
            icvist = 1
            ivisit(1) = n03d
            ivisit(2) = 0
            ivisit(3) = 0
            ivisit(4) = 0

            !**     look whether the particle sticks

            lstick = .false.
            if (nstick > 0) then         !   nstick is an undocumented parameter
                wsum = 0.0
                do isub = 1, nosubs
                    jsub = mstick(isub)         !   a substance property
                    if (jsub > 0) then      !
                        lstick = .true.          !   this particle might (but need not) stick
                        wsum = wsum + wpart(jsub, ipart)   ! the weight of the sticked fraction
                    endif                  !   the particle sticks if one of its substances sticks
                enddo

                if (wsum > 0.0) then      !   if this is true then
                    nstpar = nstpar + 1         !   the particle sticks,
                    goto 90                     !   go on with next particle
                endif

                if (lstick) then             !   aparently sticked fraction was zero for this particle
                    pstick = 0.0                !   determine the net sticking probability
                    wstick = 0.0                                       ! ==> to be checked
                    do ifract = 1, nfract      !   these are all oil fractions
                        jsub = 3 * (ifract - 1)
                        wstick = wstick + wpart(jsub + 1, ipart)  &
                                + wpart(jsub + 2, ipart)
                        pstick = pstick + wpart(jsub + 1, ipart) * fstick(ifract)  &
                                + wpart(jsub + 2, ipart) * fstick(ifract)
                    enddo
                    if (wstick > 1.0e-15) then
                        pstick = pstick / wstick
                    else
                        pstick = 0.0
                    endif
                    if (pstick > rnd(rseed)) then
                        lstick = .true.
                    else
                        lstick = .false.
                    endif
                endif
            endif

            !**     look whether the (oil)particle floats (version 3.40)

            ldispo = .true.
            if (oilmod .and. kp==ktopp .and. zpart(ipart)<=zsurf) then
                wsum = 0.0                             !   location must be floating
                do isub = 1, nfract                    !   and at least one of the
                    jsub = mapsub((isub - 1) * 3 + 1)     !   fractions has floating
                    wsum = wsum + wpart(jsub, ipart)  !   mass.
                enddo                                  !   then there is no vertical
                if (wsum > 0.0) ldispo = .false.  !   displacement: ldispo = .false.
            endif

            !**      horizontal diffusion ( and if needed buoyancy spreading )

            if (ldiffh) then
                trp = dran1 * tp ** drand(2)
                t0 = t0buoy(ipart)
                if (twolay .and. t0 > 0.0 .and. kp == 1) then
                    trp = max(trp, abuac * (tp + t0)**(-0.125))     ! bouyancy spreading parameter
                endif
                dax = sq6 * trp * (rnd(rseed) - 0.5) ! jvb: This should be changd into a distance and angle rather than x and y direction
                day = sq6 * trp * (rnd(rseed) - 0.5) ! if we want to make dispersion dependent on direction of wind/current
            else
                dax = 0.0
                day = 0.0
            endif

            !**      bed erosion

            n1 = lgrid2(np - 1, mp)
            n2 = lgrid2(np, mp - 1)
            dxp = dx    (n0)
            dyp = dy    (n0)
            depth1 = depth (n0)
            if (depth1 <= 1.0e-25) then
                write(*, *) ' Depth1 too small '
                write(*, *) ' Depth1 = ', depth1, ' gridcell n0= ', n0
                write(*, *) ' Error: abort from program  '
                write(lun2, *) ' Depth1 too small '
                write(lun2, *) ' Depth1 = ', depth1, ' gridcell n0= ', n0
                write(lun2, *) ' Error: abort from program  '
                call stop_exit(1)
            endif

            ! first calculate or get ubstar_b at the bottom used for sedimentation and erosion

            if (kpp /= kbotp) then                  ! tau from file is only defined for bottom layer
                if (zmodel) then
                    idep = (kbotp - 1) * num_rows * num_columns
                else
                    idep = (layt - 1) * num_rows * num_columns
                endif
                if (caltau) then
                    vol = volume(n0 + idep)
                    vy0 = flow  (n1 + idep) / vol
                    vy1 = flow  (n0 + idep) / vol
                    vx0 = flow  (n2 + idep + mnmaxk) / vol
                    vx1 = flow  (n0 + idep + mnmaxk) / vol
                    vvx = vx1 - vx0
                    vvy = vy1 - vy0
                    vx = vx0 + xp * vvx
                    vy = vy0 + yp * vvy
                    vxr = vx * dxp
                    vyr = vy * dyp
                    ubstar_b = sqrt(c2g * (vxr * vxr + vyr * vyr))
                else
                    ubstar_b = sqrt(tau(n0 + idep) / rhow)
                endif
            endif

            idep = (kpp - 1) * num_rows * num_columns
            vol = volume(n03d)

            ! Particles should not find themselves in a segment with zero volume, but it
            ! might happen with restarted calculations. If so, let them be - there is no
            ! sensible transport step we can take

            if (vol == 0.0) then
                cycle
            endif

            vy0 = flow  (n1 + idep) / vol
            vy1 = flow  (n0 + idep) / vol
            vx0 = flow  (n2 + idep + mnmaxk) / vol
            vx1 = flow  (n0 + idep + mnmaxk) / vol
            vvx = vx1 - vx0
            vvy = vy1 - vy0
            vx = vx0 + xp * vvx
            vy = vy0 + yp * vvy
            vxr = vx * dxp                         ! vxr and vyr must be known
            vyr = vy * dyp                         ! from the beginning

            ! get or calculate ubstar for the actual layer

            if (kpp == kbotp) then
                if (caltau) then
                    ubstar = sqrt(c2g * (vxr * vxr + vyr * vyr))  ! ubstar this is requiered for disersion
                else
                    ubstar = sqrt(tau(n03d) / rhow)
                endif
                ubstar_b = ubstar                     ! ubstar_bot is required for sedimentation and erosion
            else
                ubstar = sqrt(c2g * (vxr * vxr + vyr * vyr))  ! ubstar this is requiered for dispersion
            endif

            if (use_settling .and. kp == layt + 1) then
                if (ubstar_b >= uecrit) then
                    kp = kbotp
                    zp = 0.05
                    nopart_ero = nopart_ero + 1
                else
                    goto 50
                endif
            endif
            !**
            !**      vertical diffusion                         ! and for vertical diffusion
            !**
            n03d = n0 + idep
            disp = 0.0
            !         vdiff(n03d) = 0.0
            dvz = 0.0
            depthl = volume(n03d) / area(n0)
            if (ldiffz) then
                if (area(n0) <=  1.0e-25) then
                    write(*, *) ' Area too small '
                    write(*, *) ' n0 = ', n0, ' area(n0) = ', area(n0)
                    write(*, *) ' Error: abort from program  '
                    write(lun2, *) ' Area too small '
                    write(lun2, *) ' n0 = ', n0, ' area(n0) = ', area(n0)
                    write(lun2, *) ' Error: abort from program  '
                    call stop_exit(1)
                endif
                depthl = volume(n03d) / area(n0)
                if (depthl <=  1.0e-25) then
                    write(*, *) ' Depthl too small '
                    write(*, *) ' Depthl = ', depthl, ' n03d = ', n03d
                    write(*, *) ' Error: abort from program  '
                    write(lun2, *) ' Depthl too small '
                    write(lun2, *) ' Depthl = ', depthl, ' n03d = ', n03d
                    write(lun2, *) ' Error: abort from program  '
                    call stop_exit(1)
                endif
                select case (ioptdv)
                case (0)            !  from version 3.60 also constant vertical diseprsion
                    disp = cdisp * alpha
                case (1)            !  from version 3.43 also required for settling:
                    uwstar = sqrt(cd * rhoair / rhow * wvelo(n0) * wvelo(n0))
                    disp = 0.83 * 0.41 * depth1 * sqrt(3.0) * &       !  0.41 is rkappa
                            (ubstar / 6.0 + twopi * uwstar / 32.0)
                    disp = disp * alpha
                case (2)
                    disp = max(cdisp + alpha * vdiff(n03d), dminim)
                case default
                    write(*, *) ' Ioptdv = ', ioptdv
                    write(*, *) ' this option for vert. dispersion '
                    write(*, *) ' is not implemented               '
                    call stop_exit(1)
                end select
                !            vdiff(n03d) = disp
                dvz = 2.0 * sq6 * sqrt(disp * itdelt) * &
                        (rnd(rseed) - 0.5) / depthl / dred
            endif
            if (modtyp/=model_abm) then
                dvzs = wsettl(ipart) * itdelt / depthl          !  settling      ?? what is the effect of this?? jvb: no bouncing for settling particles
                dvzt = dvzs + dvz                           !  vertical diffusion
            else
                dvzt = dvz                                  !  vertical diffusion
            endif

            !**      oil: if floating, no vertical dispersion and no settling...

            if (oilmod .and. .not. ldispo) then
                disp = 0.0
                !            vdiff(n0) = 0.0
                dvz = 0.0
                dvzs = 0.0
                dvzt = 0.0
            endif

            !**      vertically bouncing particles ?

            icounz = 0
            znew = zp + dvzt
            do while (znew > 1.0 .or. znew < 0.0)

                !**      boundary conditions, check here also settling and erosion
                !**      of particles with critical velocities at the bed

                if (znew > 1.0) then
                    if (.not. twolay .and. kp /= kbotp) then
                        n03d2 = n03d + num_rows * num_columns
                        if (ioptdv == 2) then
                            disp2 = max(cdisp + alpha * vdiff(n03d2), dminim)
                            pbounce = sqrt(disp2 / disp)
                            if (disp2 < disp) then
                                if (rnd(rseed) < 1.0 - pbounce) then
                                    znew = 2.0 - znew
                                    cycle
                                endif
                            endif
                            znew = (znew - 1.0) * depthl * pbounce
                            disp = disp2
                        else
                            znew = (znew - 1.0) * depthl
                        endif
                        kp = kp + 1
                        n03d = n03d2
                        depthl = volume(n03d) / area(n0)
                        znew = znew / depthl
                    else                                   ! either twolay or kp == layt
                        if (use_settling .and. ubstar_b < uscrit   &
                                .and. wsettl(ipart) > 0.0) then
                            kpart(ipart) = layt + 1          !  at high vert disp everything settles !!!
                            zpart(ipart) = 0.0
                            nopart_sed = nopart_sed + 1
                            if (lstick .and. lbott) then   !  dispersed oil (or other subs) may stick to the bottom
                                lstick = .false.              !  the particle sticks only once in this algorithm
                                do isub = 1, nosubs
                                    jsub = mstick(isub)
                                    if (jsub > 0) then    !  phase change from floating or dispersed
                                        wpart(jsub, ipart) = wpart(isub, ipart)  ! shouldn't this be summed ?
                                        wpart(isub, ipart) = 0.0
                                    endif
                                enddo                         ! ==> also here
                            endif
                            goto 50
                        else
                            if (vertical_bounce) then
                                znew = 2.0 - znew             !  now it bounces
                            else
                                znew = 0.9990                 !  now it stays near the bottom (no bounce)
                            endif
                        endif
                    endif
                elseif (znew < 0.0) then
                    if (.not. twolay .and. kp /= ktopp) then
                        n03d2 = n03d - num_rows * num_columns
                        if (ioptdv == 2) then
                            disp2 = max(cdisp + alpha * vdiff(n03d2), dminim)
                            pbounce = sqrt(disp2 / disp)
                            if (disp2 < disp) then
                                if (rnd(rseed) < 1.0 - pbounce) then
                                    znew = - znew
                                    cycle
                                endif
                            endif
                            znew = znew * depthl * pbounce
                            disp = disp2
                        else
                            znew = znew * depthl
                        endif
                        kp = kp - 1
                        n03d = n03d2
                        depthl = volume(n03d) / area(n0)
                        znew = znew / depthl + 1.0
                    else
                        if (vertical_bounce) then
                            znew = -znew                      !  now it bounces
                        else
                            znew = 0.0001                     !  now it stays near the surface (no bounce)
                        endif
                    endif
                endif
            enddo
            if (znew == 0.0) znew = 0.0001
            if (znew == 1.0) znew = 0.9999
            zp = znew                                    !  the final vertical position

            !*       for checking results

            dspmin = min(disp, dspmin)
            dspmax = max(disp, dspmax)
            dsprms = dsprms + disp * disp
            nrms = nrms + 1.0

            !         debugging code
            !         vrtdsp(1,ipart) = disp
            !         vrtdsp(2,ipart) = dvz  * depthl
            !         vrtdsp(3,ipart) = dvzs * depthl
            !         vrtdsp(4,ipart) = dvzt * depthl
            !         vrtdsp(5,ipart) = depthl
            !         vrtdsp(6,ipart) = depth1
            !         vrtdsp(7,ipart) = n0
            !**
            !**       this is innerloop for particles crossing gridcell borders
            !**
            10    n1 = lgrid2(np - 1, mp)              ! pas op n0 is niet opnieuw gezet hier
            n2 = lgrid2(np, mp - 1)
            dxp = dx    (n0)
            dyp = dy    (n0)
            sangl = angle (n0)
            depth1 = depth (n0)
            if (depth1 <= 1.0e-25) then
                write(*, *) ' Depth1 too small '
                write(*, *) ' Depth1 = ', depth1, ' gridcell n0= ', n0
                write(*, *) ' Error: abort from program  '
                write(lun2, *) ' Depth1 too small '
                write(lun2, *) ' Depth1 = ', depth1, ' gridcell n0= ', n0
                write(lun2, *) ' Error: abort from program  '
                call stop_exit(1)
            endif

            ! ================a d v e c t i o n=========================================

            !** windprofiles and logarithmic profiles

            if (layt == 1) then
                !              for 2 layers: the pos. in the top layer + thickness bottom layer
                zp2 = 1.0 - zp
                if (twolay .and. kp == 1) zp2 = zp2 - pblay
                !              depth curve of the wind component
                c1 = (3.0 * zp2 - 2.0) * zp2 * wvelo(n0) * cdrag * itdelt
                !              depth curve of the velocities
                f1 = rough / depth1
                c2 = alog(zp2 / f1 + 1.0) / ((1.0 + f1) * alog(1.0 / f1 + 1.0) - 1.0)
                c3 = 1.0
            else
                c1 = 0.0  ! no wind profile in 3d
                c2 = 1.0
                c3 = 1.0
            endif

            !** determine the velocities in the 3 directions

            kpp = min0 (kp, kbotp)           !   kpp is a pointer that won't reach the bed..
            if (twolay) kpp = 1
            if (kpp < 1 .or. kpp > kbotp) then
                write(*, *) ' program error part10: kpp out of range '
                call stop_exit(1)
            endif
            idep = (kpp - 1) * num_rows * num_columns
            vol = volume(n0 + idep)
            vy0 = flow  (n1 + idep) / vol
            vy1 = flow  (n0 + idep) / vol
            vx0 = flow  (n2 + idep + mnmaxk) / vol
            vx1 = flow  (n0 + idep + mnmaxk) / vol
            vz0 = 0.0
            vz1 = 0.0

            if (threed) then
                if     (kpp == ktopp) then  ! first layer
                    vz1 = flow (n0 + idep + 2 * mnmaxk) / vol
                elseif (kpp == kbotp) then  ! last  layer
                    idepm1 = idep - num_rows * num_columns
                    vz0 = flow (n0 + idepm1 + 2 * mnmaxk) / vol
                else
                    idepm1 = idep - num_rows * num_columns
                    vz0 = flow (n0 + idepm1 + 2 * mnmaxk) / vol
                    vz1 = flow (n0 + idep + 2 * mnmaxk) / vol
                endif
            endif
            if (modtyp==model_abm) then
                depthl = volume(n0 + idep) / area(n0)
                vzs = wsettl(ipart) / depthl             !  settling
                if (kpp /= ktopp) vz0 = vz0 + vzs
                if (kpp /= kbotp) vz1 = vz1 + vzs
            endif

            if (oilmod .and. .not. ldispo) then   ! floating oil
                vz0 = 0.0
                vz1 = 0.0
                if (stickdf == 1) then
                    if (vy0 == 0.0 .and. vy1 == 0.0 .and.           &
                            &              vx0 == 0.0 .and. vx1 == 0.0) then ! the grid cell ran dry
                        do isub = 1, nosubs
                            jsub = mstick(isub)
                            if (jsub > 0) then
                                wpart(jsub, ipart) = wpart(jsub, ipart) + wpart(isub, ipart)
                                wpart(isub, ipart) = 0.0
                            endif
                        enddo
                        goto 50
                    endif
                endif
            endif

            vvx = vx1 - vx0
            vvy = vy1 - vy0
            vvz = vz1 - vz0
            vx = vx0 + xp * vvx
            vy = vy0 + yp * vvy
            vz = vz0 + zp * vvz
            vxr = vx * dxp         !     only used for dispersion
            vyr = vy * dyp         !     only used for dispersion


            !**      time of flight particle in x direction

            rtimx = real(itdelt)
            idx = 0
            if (abs(c2 * vvx) > accur) then
                if (vx > 0.0) then
                    if (vx1 >  1.0e-25) then
                        rtimx = alog(vx1 / vx) / (c2 * vvx)
                        idx = 1
                    endif
                endif
                if (vx < 0.0) then
                    if (vx0 < -1.0e-25) then
                        rtimx = alog(vx0 / vx) / (c2 * vvx)
                        idx = -1
                    endif
                endif
            else
                if     (vx >  1.0e-25) then
                    rtimx = (1.0 - xp) / vx / c2
                    idx = 1
                elseif (vx < -1.0e-25) then
                    rtimx = -xp / vx / c2
                    idx = -1
                else
                    rtimx = 9999 * rtimx
                endif
            endif

            !**      time of flight particle in y direction

            rtimy = real(itdelt)
            idy = 0
            if (abs(c2 * vvy) > accur) then
                if (vy > 0.0) then
                    if (vy1 >  1.0e-25) then
                        rtimy = alog(vy1 / vy) / (c2 * vvy)
                        idy = 1
                    endif
                endif
                if (vy < 0.0) then
                    if (vy0 < -1.0e-25) then
                        rtimy = alog(vy0 / vy) / (c2 * vvy)
                        idy = -1
                    endif
                endif
            else
                if     (vy >  1.0e-25) then
                    rtimy = (1.0 - yp) / vy / c2
                    idy = 1
                elseif (vy < -1.0e-25) then
                    rtimy = -yp / vy / c2
                    idy = -1
                else
                    rtimy = 9999 * rtimy
                endif
            endif

            !**       time of flight particle in z direction
            ! 3d..  positive velocity is from z = 0 to z = 1
            ! 3d..                    is assumed to go downwards
            ! 3d..                    from small kp to large kp

            rtimz = real(itdelt)
            idz = 0
            if (abs(c3 * vvz) > accur) then
                if (vz == 0.0) then
                    vz = sign(1.0e-10, vz1)
                endif
                if (vz > 0.0) then
                    if (vz1 >  1.0e-25) then
                        rtimz = alog(vz1 / vz) / (c3 * vvz)
                        idz = 1
                    endif
                endif
                if (vz < 0.0) then
                    if (vz0 < -1.0e-25) then
                        rtimz = alog(vz0 / vz) / (c3 * vvz)
                        idz = -1
                    endif
                endif
            else
                if     (vz >  1.0e-25) then
                    rtimz = (1.0 - zp) / vz / c3
                    idz = 1
                elseif (vz < -1.0e-25) then
                    rtimz = -zp / vz / c3
                    idz = -1
                else
                    rtimz = 9999 * rtimz
                endif
            endif

            !**      (partial) advection step

            rtim = min(rtimx, rtimy, rtimz, deltt)
            rtim1 = rtim

            if (abs(vvx) > accur) then
                xp = (vx / vvx) * exp(vvx * c2 * rtim) - vx0 / vvx
            else
                xp = xp + vx * c2 * rtim
            endif

            if (abs(vvy) > accur) then
                yp = (vy / vvy) * exp(vvy * c2 * rtim) - vy0 / vvy
            else
                yp = yp + vy * c2 * rtim
            endif

            if (abs(vvz) > accur) then
                zp = (vz / vvz) * exp(vvz * c3 * rtim) - vz0 / vvz
            else
                zp = zp + vz * c3 * rtim
            endif


            !**      did the time step end ?

            if (rtim1 < deltt) then
                if (rtim  < deltt) then

                    !             logically one of the 3 conditions below should hold,
                    !             so either rtim=rtimx or rtim=rtimy or rtim=rtimz
                    !             this follows directly from
                    !                   rtim = min(rtimx, rtimy, rtimz, deltt)
                    !             it is however not checked whether the particle is at the border !lp!
                    !             in order to eliminate round off a relative test is
                    !             added for this check

                    if     (eql(rtim, rtimx)) then
                        mp = mp + idx
                        xp = xp - real(idx)
                        ddshift = 1
                    elseif (eql(rtim, rtimy)) then
                        np = np + idy
                        yp = yp - real(idy)
                        ddshift = 2
                    elseif (eql(rtim, rtimz)) then
                        kp = kp + idz
                        zp = zp - real(idz)
                        ddshift = 0
                        if (kp < ktopp .or.   &
                                (kp > kbotp .and. .not. twolay)) then
                            if(kp < ktopp .and. zmodel) then        ! this seems to cause problems in Z-model where model layers change
                                kp = ktopp
                            elseif(kp > kbotp .and. zmodel) then   ! this seems to cause problems in Z-model where model layers change
                                kp = kbotp
                            else
                                write(*, *) ' Particle = ', ipart, kp, kbotp, ktopp, &
                                        ' not on an active layer'
                                write(*, *) ' Programming error in rtim in part10'
                                call stop_exit(1)
                            endif
                        endif
                    else
                        write(*, *) ' ipart, rtim, rtimx, rtimy, rtimz '
                        write(*, *) ipart, rtim, rtimx, rtimy, rtimz
                        write(*, *) ' rtim1, rtim, deltt '
                        write(*, *) rtim1, rtim, deltt
                        write(*, *) ' Programming error in rtim in part10'
                        call stop_exit(1)
                    endif
                endif

                n0old = n0
                n0 = lgrid(np, mp)
                if (n0 < -nbmax) then
                    n0 = - n0 - nbmax
                    call p10ddb (nconn, conn, n0, ddshift, np, &
                            &                       mp, xp, yp)
                    n0 = lgrid(np, mp)
                endif
                if (n0 < 1) then        ! inactive grid cell
                    npart(ipart) = 1
                    mpart(ipart) = 1
                    kpart(ipart) = 1
                    do isub = 1, nosubs
                        wpart(isub, ipart) = 0.0
                    enddo
                    goto 90                   ! next particle
                endif

                if (zmodel .and. ddshift > 0) then
                    call update_k_near_top(lun2, kp, ktopp, laytop(np, mp), zp, locdep, n0old, n0, zsurf)
                    kpp = min0 (kp, laybot(np, mp))           !   kpp is a pointer that won't reach the bed..
                    idep = (kpp - 1) * num_rows * num_columns
                    ktopp = laytop(np, mp)
                    kbotp = laybot(np, mp)
                    if (kp > kbotp) then
                        kp = kbotp
                        zp = 1.0
                    end if
                endif

                deltt = deltt - rtim1        ! reduce remaining part of time step
                xp = amax1(amin1(xp, 1.0), 0.0) ! this is not strong
                yp = amax1(amin1(yp, 1.0), 0.0)
                zp = amax1(amin1(zp, 1.0), 0.0)

                !**         strange construction to avoid infinite computation

                icvis = icvis + 1
                icvist = icvist + 1
                if (icvis == 5) icvis = 1
                !  this is all not so clear

                kpp = min0 (kp, kbotp)           !   kpp is a pointer that won't reach the bed..
                if (twolay) kpp = 1
                n03d = n0 + (kpp - 1) * num_rows * num_columns
                if (ivisit(icvis) /= n03d .and. icvist < 10000) then
                    ivisit(icvis) = n03d
                    goto 10
                endif
            endif

            !**      xnew,ynew is inclusive of diffusion (dax,day); c1 = 0.0 for 3d

            if (modtyp==model_abm) then
                sdir = d_swim(ipart) * twopi / 360.0
                displacement = v_swim(ipart) * idelt
                dx_swim = displacement * sin(sdir + sangl)
                dy_swim = displacement * cos(sdir + sangl)
            else
                dx_swim = 0.0
                dy_swim = 0.0
            endif

            wdirr = wdir(n0) * twopi / 360.0
            xnew = xp + (dax - c1 * sin(wdirr + sangl) + dx_swim) / dxp
            ynew = yp + (day - c1 * cos(wdirr + sangl) + dy_swim) / dyp

            !**      floating oil

            if (threed .and. oilmod) then
                if (ldispo) then                ! vertical diffusion of oil
                    floil(ipart) = 0
                else
                    floil(ipart) = 1             ! oil is floating
                    vxw = - wvelo(n0) * sin(wdirr + sangl)
                    vyw = - wvelo(n0) * cos(wdirr + sangl)
                    !             drag on the difference vector: cd * (wind - flow)
                    xnew = xnew + (cdrag * (vxw - vxr) / dxp) * itdelt    !
                    ynew = ynew + (cdrag * (vyw - vyr) / dyp) * itdelt    !
                endif
            else
                if (apply_wind_drag) then
                    if (kp==ktopp) then
                        zpabs = zp * locdep(n0, kp)
                        if (zpabs < max_wind_drag_depth) then
                            vxw = - wvelo(n0) * sin(wdirr + sangl)
                            vyw = - wvelo(n0) * cos(wdirr + sangl)
                            !                    drag on the difference vector: cd * (wind - flow)
                            xnew = xnew + (cdrag * (vxw - vxr) / dxp) * itdelt    !
                            ynew = ynew + (cdrag * (vyw - vyr) / dyp) * itdelt    !
                        end if
                    end if
                end if
            endif
            znew = zp

            !**      make the depth of the particle from the water surface
            !**      this is part of code that aims at bouncing if a particle meets the wall

            deppar = 0.0
            n03d = n0
            do kd = 1, kpp - 1                     ! volume of layers above
                deppar = deppar + volume(n03d)    ! particle
                n03d = n03d + num_rows * num_columns
            enddo
            deppar = deppar + volume(n03d) * znew    ! + volume above particle
            deppar = deppar / area(n0)             ! gives depth of particle

            nboomtry = 1
            bouncefirsttry = .true.
            if(ldispo .or. (.not.booms)) then
                boomseffective = .false.
            else
                a = rnd(rseed)
                boomseffective = a < pboomcatch(1) ! ifrac is not know multiple oil to one particle??
            end if

            30    if (max(xnew - 1.0, -xnew) < max(ynew - 1.0, -ynew)) then

                idy = 0                             !     y direction first
                if (ynew > 1.0) then
                    idy = 1
                    yy = vy1                        ! velocity at downstream edge
                endif
                if (ynew < 0.0) then
                    idy = -1
                    yy = vy0
                endif
                if (idy  /= 0) then
                    if (np + idy > num_rows) then
                        write(lun2, *) 'particle is now outside grid'
                        write(lun2, *) 'ipart=', ipart
                        write(lun2, *) 'mp   =', mp
                        write(lun2, *) 'np   =', np
                        write(lun2, *) 'kp   =', kp
                        write(lun2, *) 'xp   =', xp
                        write(lun2, *) 'yp   =', yp
                        write(lun2, *) 'zp   =', zp
                        npnew = np
                        mpnew = mp
                        xpnew = xnew
                        goto 49
                    endif
                    n0new = lgrid(np + idy, mp)
                    dstick = .false.                 !  logical to determine sticking
                    if (n0new < -nbmax .and. yy /= 0.0) then
                        xpnew = min(1.0, max(0.0, xnew))
                        xrest = (xnew - xpnew) * dxp
                        n0new = - n0new - nbmax
                        ddshift = 2
                        call p10ddb (nconn, conn, n0new, ddshift, npnew, &
                                &                         mpnew, xpnew, ypnew)
                        n0new = lgrid(npnew, mpnew)
                        if (n0new > 0) then
                            dxpnew = dx(n0new)
                            xpnew = xpnew + xrest / dxpnew
                        endif
                    else
                        npnew = np + idy
                        mpnew = mp
                        xpnew = xnew
                    endif
                    if (n0new > 0 .and.     &    !  new cell is active
                            yy    /= 0.0) then   !  and there is no thin dam
                        depth2 = depth(n0new)
                        if (deppar < depth2) then
                            n0old = n0
                            n0 = n0new
                            np = npnew
                            mp = mpnew
                            if (zmodel) then
                                call update_k_near_top(lun2, kp, ktopp, laytop(np, mp), zp, locdep, n0old, n0, zsurf)
                                kpp = min0 (kp, laybot(np, mp))           !   kpp is a pointer that won't reach the bed..
                                idep = (kpp - 1) * num_rows * num_columns
                                ktopp = laytop(np, mp)
                                kbotp = laybot(np, mp)
                                if (kp > kbotp) then
                                    kp = kbotp
                                    zp = 1.0
                                end if
                            endif
                            n1 = lgrid2(np - 1, mp)
                            n2 = lgrid2(np, mp - 1)
                            ynew = (ynew - 0.5 * (1 + idy)) * dyp
                            dxp = dx(n0)
                            dyp = dy(n0)
                            ynew = ynew / dyp + 0.5 * (1 - idy)
                            vol = volume(n0 + idep)
                            vy0 = flow  (n1 + idep) / vol
                            vy1 = flow  (n0 + idep) / vol
                            vx0 = flow  (n2 + idep + mnmaxk) / vol
                            vx1 = flow  (n0 + idep + mnmaxk) / vol
                        else
                            ynew = (1.0 + idy) - ynew    !  it bounces !
                            if (lstick .and. lbott) dstick = .true.
                        endif
                    else          ! new cell is open boundary or inactive
                        ynew = (1.0 + idy) - ynew       ! it bounces !
                        if (lstick .and. lland .and. n0new == 0)  &
                                dstick = .true.
                    endif
                    if (dstick) then
                        ynew = 0.5 + idy * 0.4999
                        lstick = .false.
                        do isub = 1, nosubs
                            jsub = mstick(isub)
                            if (jsub > 0) then
                                if (wpart(jsub, ipart) == 0.0) then
                                    wpart(jsub, ipart) = wpart(isub, ipart)
                                    wpart(isub, ipart) = 0.0
                                endif
                            endif
                        enddo
                    endif
                    goto 30
                endif
            else
                idx = 0                            !     now x direction
                if (xnew > 1.0) then
                    idx = 1
                    xx = vx1
                endif
                if (xnew < 0.0) then
                    idx = -1
                    xx = vx0
                endif
                if (idx /= 0) then
                    n0new = lgrid(np, mp + idx)
                    dstick = .false.                !  logical to determine sticking
                    if (n0new < -nbmax .and. xx /= 0.0) then
                        ypnew = min(1.0, max(0.0, ynew))
                        yrest = (ynew - ypnew) * dyp
                        n0new = - n0new - nbmax
                        ddshift = 1
                        call p10ddb (nconn, conn, n0new, ddshift, npnew, &
                                &                         mpnew, xpnew, ypnew)
                        n0new = lgrid(npnew, mpnew)
                        if (n0new > 0) then
                            dypnew = dy(n0new)
                            ypnew = ypnew + yrest / dypnew
                        endif
                    else
                        npnew = np
                        mpnew = mp + idx
                        ypnew = ynew
                    endif
                    if (n0new > 0 .and.    &    !  new cell is active
                            xx    /= 0.0) then  !  and there is no thin dam
                        depth2 = depth(n0new)
                        if (deppar < depth2) then
                            n0old = n0
                            n0 = n0new
                            np = npnew
                            mp = mpnew
                            if (zmodel) then
                                call update_k_near_top(lun2, kp, ktopp, laytop(np, mp), zp, locdep, n0old, n0, zsurf)
                                kpp = min0 (kp, laybot(np, mp))           !   kpp is a pointer that won't reach the bed..
                                idep = (kpp - 1) * num_rows * num_columns
                                ktopp = laytop(np, mp)
                                kbotp = laybot(np, mp)
                                if (kp > kbotp) then
                                    kp = kbotp
                                    zp = 1.0
                                end if
                            endif
                            n1 = lgrid2(np - 1, mp)
                            n2 = lgrid2(np, mp - 1)
                            xnew = (xnew - 0.5 * (1 + idx)) * dxp
                            dxp = dx(n0)
                            dyp = dy(n0)
                            xnew = xnew / dxp + 0.5 * (1 - idx)
                            vol = volume(n0 + idep)
                            vy0 = flow  (n1 + idep) / vol
                            vy1 = flow  (n0 + idep) / vol
                            vx0 = flow  (n2 + idep + mnmaxk) / vol
                            vx1 = flow  (n0 + idep + mnmaxk) / vol
                        else
                            xnew = (1.0 + idx) - xnew    !  it bounces !
                            if (lstick .and. lbott) dstick = .true.
                        endif
                    else
                        xnew = (1.0 + idx) - xnew       !  it bounces !
                        if (lstick .and. lland .and. n0new == 0)  &
                                dstick = .true.
                    endif
                    if (dstick) then
                        xnew = 0.5 + idx * 0.4999
                        lstick = .false.
                        do isub = 1, nosubs
                            jsub = mstick(isub)
                            if (jsub > 0) then   ! ==> sticking
                                if (wpart(jsub, ipart) == 0.0) then
                                    wpart(jsub, ipart) = wpart(isub, ipart)
                                    wpart(isub, ipart) = 0.0
                                endif
                            endif
                        enddo
                    endif
                    goto 30
                endif
            endif

            if (kp < ktopp .or. &
                    (kp > kbotp .and. .not. twolay)) then
                if(kp < ktopp .and. zmodel) then        ! this seems to cause problems in Z-model where model layers change
                    kp = ktopp
                elseif(kp > kbotp .and. zmodel) then   ! this seems to cause problems in Z-model where model layers change
                    kp = kbotp
                else
                    write(*, *) ' Particle = ', ipart, &
                            ' not on an active layer:', kp, ktopp, kbotp
                    write(*, *) ' Programming error in rtim in part10'
                    call stop_exit(1)
                endif
            endif

            nscreenstry = 1
            screensfirsttry = .true.
            if (screens) then
                !**      compute absolute x's and y's for a single particle end point
                call part11sp (lgrid, xcor, ycor, num_rows, np, mp, &
                        xnew, ynew, xanew, yanew, lgrid2, num_columns)
                if (iptime(ipart) <= 0 .and. nscreenstry==1) then
                    !      determine absolute location of starting point as well for new particles
                    call part11sp (lgrid, xcor, ycor, num_rows, npart(ipart), mpart(ipart), &  !  new coordinates
                            xpart(ipart), ypart(ipart), xaold, yaold, lgrid2, num_columns)
                else
                    xaold = xa(ipart)
                    yaold = ya(ipart)
                end if
                call boombounce(xaold, yaold, xanew, yanew, nrowsscreens, &
                        xpolscreens(1:nrowsscreens), ypolscreens(1:nrowsscreens), &
                        xacatch, yacatch, catch, xabounce, yabounce, bounce, leftside)
                if (catch) then
                    a = rnd(rseed)
                    if (leftside) then
                        catch = a > permealeft
                    else
                        catch = a > permearight
                    endif
                end if
                if (catch) then
                    if (bounce .and. bouncefirsttry) then
                        !                 go back to relative coordinates from the original cell(!), set boomseffective to false (!?) and go back to check for checking of inactive cells etc... (30)
                        !                 a new bounce may mean that the booms must be effective again... hm, more complicated than I thought... Skip for the first implementation
                        np = npart(ipart)
                        mp = mpart(ipart)
                        kp = kpart(ipart)
                        znew = zpart(ipart)
                        call part07nm (lgrid, lgrid2, num_rows, num_columns, xcor, & ! make relative
                                ycor, xabounce, yabounce, np, &        ! coordinates
                                mp, xnew, ynew, ierror)                    ! again
                        if (ierror/=0) then
                            mpart(ipart) = mpart0(ipart)
                            npart(ipart) = npart0(ipart)
                        end if
                        screensfirsttry = .false.
                        goto 30
                    else
                        if (nscreenstry==1) then
                            screensfirsttry = .true.
                            !                 go back to the original location and try with dispersion only...
                            np = npart(ipart)
                            mp = mpart(ipart)
                            kp = kpart(ipart)
                            xnew = xpart(ipart)
                            ynew = ypart(ipart)
                            znew = zpart(ipart)
                            !**      xnew,ynew is inclusive of diffusion (dax,day); c1 = 0.0 for 3d
                            xnew = xnew + (dax - c1 * sin(wdirr + sangl)) / dxp
                            ynew = ynew + (day - c1 * cos(wdirr + sangl)) / dyp
                            nscreenstry = 2
                            goto 30
                        else
                            !                 finally just put it back to the old location!
                            np = npart(ipart)
                            mp = mpart(ipart)
                            kp = kpart(ipart)
                            xnew = xpart(ipart)
                            ynew = ypart(ipart)
                            znew = zpart(ipart)
                        end if
                    end if
                end if
            end if

            if (boomseffective) then
                !**      compute absolute x's and y's for a single particle end point
                call part11sp (lgrid, xcor, ycor, num_rows, np, mp, &
                        xnew, ynew, xanew, yanew, lgrid2, num_columns)
                if (iptime(ipart) <= 0 .and. nboomtry==1) then
                    !      determine absolute location of starting point as well for new particles
                    call part11sp (lgrid, xcor, ycor, num_rows, npart(ipart), mpart(ipart), &  !  new coordinates
                            xpart(ipart), ypart(ipart), xaold, yaold, lgrid2, num_columns)
                else
                    xaold = xa(ipart)
                    yaold = ya(ipart)
                end if
                call boombounce(xaold, yaold, xanew, yanew, nrowsboom(iboomint), &
                        xpolboom(1:nrowsboom(iboomint), iboomint), &
                        ypolboom(1:nrowsboom(iboomint), iboomint), &
                        xacatch, yacatch, catch, xabounce, yabounce, bounce, leftside)
                if (catch) then
                    if (bounce .and. bouncefirsttry) then
                        !                 go back to relative coordinates from the original cell(!), set boomseffective to false (!?) and go back to check for checking of inactive cells etc... (30)
                        !                 a new bounce may mean that the booms must be effective again... hm, more complicated than I thought... Skip for the first implementation
                        np = npart(ipart)
                        mp = mpart(ipart)
                        kp = kpart(ipart)
                        znew = zpart(ipart)
                        call part07nm (lgrid, lgrid2, num_rows, num_columns, xcor, & ! make relative
                                ycor, xabounce, yabounce, np, &        ! coordinates
                                mp, xnew, ynew, ierror)                    ! again
                        if (ierror/=0) then
                            mpart(ipart) = mpart0(ipart)
                            npart(ipart) = npart0(ipart)
                        end if
                        bouncefirsttry = .false.
                        goto 30
                    else
                        if (nboomtry==1) then
                            bouncefirsttry = .true.
                            !                 go back to the original location and try with dispersion and wind only...
                            np = npart(ipart)
                            mp = mpart(ipart)
                            kp = kpart(ipart)
                            xnew = xpart(ipart)
                            ynew = ypart(ipart)
                            znew = zpart(ipart)
                            !**      xnew,ynew is inclusive of diffusion (dax,day); c1 = 0.0 for 3d
                            !             drag on the difference vector: cd * (wind - flow)
                            xnew = xnew + (dax - c1 * sin(wdirr + sangl)) / dxp + (cdrag * (vxw - vxr) / dxp) * itdelt
                            ynew = ynew + (day - c1 * cos(wdirr + sangl)) / dyp + (cdrag * (vyw - vyr) / dyp) * itdelt
                            nboomtry = 2
                            goto 30
                        else if (nboomtry==2) then
                            bouncefirsttry = .true.
                            !                 go back to the original location and try with dispersion only...
                            np = npart(ipart)
                            mp = mpart(ipart)
                            kp = kpart(ipart)
                            xnew = xpart(ipart)
                            ynew = ypart(ipart)
                            znew = zpart(ipart)
                            !**      xnew,ynew is inclusive of diffusion (dax,day); c1 = 0.0 for 3d
                            !             drag on the difference vector: cd * (wind - flow)
                            xnew = xnew + (dax - c1 * sin(wdirr + sangl)) / dxp
                            ynew = ynew + (day - c1 * cos(wdirr + sangl)) / dyp
                            nboomtry = 3
                            goto 30
                        else
                            !                 finally just put it back to the old location!
                            np = npart(ipart)
                            mp = mpart(ipart)
                            kp = kpart(ipart)
                            xnew = xpart(ipart)
                            ynew = ypart(ipart)
                            znew = zpart(ipart)
                        end if
                    end if
                end if
            end if

            !**      assignment of the coordinates and other properties

            49    continue
            npart (ipart) = np
            mpart (ipart) = mp
            kpart (ipart) = kp
            xpart (ipart) = xnew
            ypart (ipart) = ynew
            zpart (ipart) = znew

            !**      calculate decay wpart for all substances, adapt for smooth loading with ddfac

            50    do isub = 1, nosubs
                if (ddfac >  1.5) then
                    wpart(isub, ipart) = wpart(isub, ipart) * dfact(isub)
                else
                    wpart(isub, ipart) = wpart(isub, ipart) * &
                            dfact(isub)**ddfac
                endif
            enddo
            !         vdiff( n0 ) = disp

            !**     end of particle loop

            90    iptime(ipart) = iptime(ipart) + idelt
        end do
        !$OMP END PARALLEL DO
        timon = timon_org

        if (use_settling) then
            write(lun2, '(4x,a,i12,a,i4,a)') '  No. of particles settled into bed layer  : ', &
                    nopart_sed, ' (layer ', layt + 1, ')'
            write(lun2, '(4x,a,i12,a,i4,a)') '  No. of particles eroded from  bed layer  : ', &
                    nopart_ero, ' (layer ', layt + 1, ')'
        endif

        !**   some statistics on vertical diffusion

        if (nopart == 0) then
            dspmin = 0.0
            dspmax = 0.0
            dsprms = 0.0
        else
            dsprms = sqrt(dsprms / max(nrms, 1.0))
        endif
        write(lun2, '(6x,a,i12)') &
                'No. of particles in inactive grid cells  : ', ninact
        write(lun2, '(6x,a,i12)') &
                'No. of sticking particles                : ', nstpar
        write(lun2, '(6x,a,es15.7)') &
                'Minimum  (*) for vertical dispersion     : ', dspmin
        write(lun2, '(6x,a,es15.7)') &
                'Maximum  (*) for vertical dispersion     : ', dspmax
        write(lun2, '(6x,a,es15.7)') &
                'Rms value(*) for vertical dispersion     : ', dsprms
        write(lun2, '(15x,a)') '(*) over time and space(=particles)'
        !**
        !**   apply rotation on (3d) floating oil particles due to effect of waves (coriolis)
        !**
        if (threed .and. oilmod .and. coriol) then

            !**   disable vertically displacing particles

            do ipart = npwndw, nopart
                if (floil(ipart) == 0) then
                    npart0(ipart) = -npart0(ipart)
                    mpart0(ipart) = -mpart0(ipart)
                    npart (ipart) = -npart (ipart)
                    mpart (ipart) = -mpart (ipart)
                endif
            enddo

            !**      compute absolute x's and y's  ( z's are dummy )

            call part11 (lgrid, xcor, ycor, num_rows, npart0, &  !  old coordinates
                    mpart0, xpart0, ypart0, xa0, ya0, &
                    nopart, npwndw, lgrid2, kpart, zpart, &
                    za, locdep, dps, num_layers, num_columns, &
                    tcktot)
            call part11 (lgrid, xcor, ycor, num_rows, npart, &  !  new coordinates
                    mpart, xpart, ypart, xa, ya, &
                    nopart, npwndw, lgrid2, kpart, zpart, &
                    za, locdep, dps, num_layers, num_columns, &
                    tcktot)

            !**      rotate vector by multiplication with rotation matrix

            codef = cos(-defang)
            sidef = sin(-defang)
            do ipart = npwndw, nopart
                if (floil(ipart) == 0) then
                    npart(ipart) = abs(npart(ipart))
                    mpart(ipart) = abs(mpart(ipart))
                else
                    dxx = xa(ipart) - xa0(ipart)
                    dyy = ya(ipart) - ya0(ipart)
                    xx = xa0(ipart) + dxx * codef - dyy * sidef
                    yy = ya0(ipart) + dxx * sidef + dyy * codef
                    call part07 (lgrid, lgrid2, num_rows, num_columns, xcor, &! make relative
                            ycor, xx, yy, npart(ipart), &! coordinates
                            mpart(ipart), xpart(ipart), ypart(ipart), &! again
                            ierror)
                    if (ierror/=0) then
                        !
                        ! Location after adding the deflection angle correction is outside the grid,
                        ! so the deflection angle correction will not be performed
                        !
                        mpart(ipart) = mpart0(ipart)
                        npart(ipart) = npart0(ipart)
                    endif
                endif
            enddo
        endif

        !**   end of subroutine

        if (timon) call timstop (ithndl)
        return
    end subroutine part10

    logical function eql(a, b)

        !     function to check if two floating point numbers are equal
        !     by checking on the number of equal digits
        implicit none           !   force explicit typing
        real :: a, b
        real, parameter :: epsrel = 0.0001

        if      (b  /= 0.0) then
            eql = abs(a / b - 1.0) < epsrel
        else if (a  == 0.0) then
            eql = .true.
        else
            eql = .false.
        endif

        return
    end function

    !     subroutine to keep a particle near the surface in z-layer models
    subroutine update_k_near_top(lun2, kp, ktopold, ktopnew, zp, locdep, n0old, n0, zsurf)

        use m_waq_precision

        implicit none

        !     Arguments

        !     kind                       name                   description
        integer :: lun2                !< error and debug messages
        integer :: kp                  !< particle layernr
        integer :: ktopold             !< previous time or location k top
        integer :: ktopnew             !< current k top
        real(sp) :: zp                  !< relative location in layer (shouldn't this be absolute?
        real(sp) :: locdep(:, :)         !< depth per layer
        integer(int_wp) :: n0                  !< segment number 2d
        integer(int_wp) :: n0old               !< old value of segment number 2d
        real(sp) :: zsurf               !< threshold for a particle to be close enought to the surface to stat there

        !     local variables

        !     kind                       name                  description
        real(sp) :: partdep             ! absolute location of particle from the top of the water column
        integer(int_wp) :: ilay                ! layer loop counter

        if (ktopold == ktopnew) return ! nothing to do when the toplayer doesn't change
        if (kp > ktopold .and. kp > ktopnew) return ! nothing to do when the particle was and is not in the top layer

        if (kp < ktopold) then
            ! the particle was and is (floating) above the top layer, this should not happen...
            write (*, 1000) kp, ktopold
            write (lun2, 1000) kp, ktopold
            1000 format (/'  WARNING: Particle found in layer ', i4, ' which was less than ktop (=', i4, ')'/)
            kp = ktopold
        end if

        if (kp == ktopold .and. zp <= zsurf) then
            ! do not update zp when close to the surface (floating), just move to the new top layer and keep zp
            kp = ktopnew
        elseif (ktopold < ktopnew) then
            ! moving to less active layers
            ! calculate absolute position from the top of the water column in the old layers
            if (kp == 1) then
                partdep = zp * locdep(n0old, kp)
            else
                partdep = locdep(n0old, kp - 1) + zp * (locdep(n0old, kp) - locdep(n0old, kp - 1))
            endif
            ! calculate the relative location of the particle over all old layers that merge into the new toplayer
            zp = partdep / locdep(n0old, ktopnew)
            kp = ktopnew
        else
            ! moving to more active layers
            ! calculate absolute position from the top of the water column in the new layers
            partdep = zp * locdep(n0, ktopold)
            ! search in which layer we are
            do ilay = ktopnew, ktopold
                if (partdep <= locdep(n0, ilay)) then
                    ! the particle is now in this layer
                    kp = ilay
                    exit
                endif
            enddo
            if (ilay == 1) then
                ! in the top layer the relative lacation is the particle depth divided by the
                zp = partdep / locdep(n0, ilay)
            else
                zp = (partdep - locdep(n0, ilay - 1)) / (locdep(n0, ilay) - locdep(n0, ilay - 1))
            endif
        endif

        return
    end subroutine

end module
