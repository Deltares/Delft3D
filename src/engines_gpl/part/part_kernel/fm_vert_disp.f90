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

module fm_vert_disp_mod

! use m_stop_exit


contains

subroutine fm_vert_disp (lunpr, itime)


!       Deltares Software Centre

!>\file
!>         Does all the process kinetics associated with oil
!>
!>         <ul><li> Initial gravity spreading through radius
!>         Oil released through dye releases will have an initial gravity spreading at the
!>         water surface where it floats on. This routine is able (optrad(id) .eq. 1) to
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
!>         here. It is possible to specify a constant entrainment factor per day (ioptd(ifrac) .eq. 0).
!>         It is also possible to use the advanced formula of Delvigne and Sweeny (ioptd(ifrac) .eq. 1).
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

!   System administration : Antoon Koster
    use m_part_flow, only: h0, h1, kmx
    use m_part_times
    use m_part_geom
    use m_part_parameters
    use m_particles, laypart => kpart
    use partmem
    use m_waq_precision
    use m_part_mesh
    use random_generator
    use m_partvs
    implicit none


    integer(int_wp)   , intent(in   ) :: itime                 !< current time in the model
    integer(int_wp)   , intent(in   ) :: lunpr
    ! local variables
    real(sp)                     :: ddfac
    real(sp)                     :: dran1
    real(sp)                     :: abuac
    real(sp)                     :: tp
    double precision             :: thicknessl, depthp, dred
    double precision             :: kpartold, hlayold  ! working variable for depth in a layer
    double precision             :: totdep, reldep
    double precision             :: dvz, vz
    double precision             :: rseed = 0.5d0
    double precision             :: sq6, random_step              ! = sqrt(6.0)

    integer(int_wp)              :: maxiter
    integer(int_wp)              :: ipart, ilay        ! counters
    integer(int_wp)              :: mpartold
    integer(int_wp)              :: nlay
    integer(int_wp)              :: itdelt                  ! delta-t of the particle for smooth loading
    integer(int_wp)              :: isub
    integer(int_wp)              :: partcel, partlay
    double precision, dimension(:), allocatable     :: totdepthlay       ! total depth (below water surface) of bottom of layers
    logical                      :: rise, sink, neutral   ! has the particle a rising or setting speed?

    logical, save                :: first = .TRUE.

    save ! AM - why a global SAVE?

! temporary, still need to pick up these values
    if (first) then
      allocate ( totdepthlay(kmx) )
      first = .FALSE.
    end if

    dred = 1.0  ! assuming here that it is not a two-layer model
    sq6 = sqrt(6.0)
    maxiter = 100
    sq6    = sqrt( 6.0 )
    ddfac  = 2.0
    dran1  = drand(1)
    ipart = 1
    mpartold = mpart(ipart)
    tp = real(iptime(ipart), kind=kind(int_wp))
    abuac  = abuoy(ipart)
    dran1  = drand(1)
!    wsettl = hyd%surf(1)
    wsettl = 1.0
    itdelt = idelt

    ! calculate settling velocity, check what happens if we change wsettl externally, then we do not need to calculate.
    call partvs( lunpr, itime  , nosubs , nopart , ivtset ,            &
                        ivtime , vsfour , vsfact , wpart  , wsettl ,   &
                        modtyp , 0      , ndxi   , lgrid3 , kmx  ,     &
                        mpart  , mpart  , laypart, nosegp , noseglp ,  &
                        rhopart, rhowatc, spart  , iptime)
    do ipart=1, nopart
        ! set depth at bottom of layer for all layers)
                ! cellnumaer of particle
        if ( mpart(ipart) <= 0 ) then
            cycle
        endif
        partcel = abs(cell2nod(mpart(ipart)))  ! the segment number of the layer 1
        !layer number
        partlay = laypart(ipart)


        totdepthlay(1) = h0(partcel)

        do ilay = 2, kmx
            totdepthlay(ilay) = totdepthlay(ilay - 1) + h0(partcel + (ilay-1) * hyd%nosegl)
        enddo

        hpart_prevt(ipart) = hpart(ipart)
        vz = wsettl(ipart) !settling is positive
        thicknessl = h0(laypart(ipart))
        ! depth of the particle from water surface
        if ( laypart(ipart) == 1 ) then
            depthp = thicknessl * (hpart(ipart))
        else
            depthp = totdepthlay(laypart(ipart)-1) + thicknessl * (hpart(ipart))
        endif

        tp = real(iptime(ipart), kind=kind(tp))
        random_step = rnd(rseed)-0.5d0
        if ( tp .lt. 0.0 ) then           !   adaptations because of smooth loading
            tp     = 0.0
            itdelt = dts + iptime(ipart)
            ddfac  = real(itdelt)/dts
            dran1  = dran1 * sqrt(ddfac)
            abuac  = abuac * sqrt(ddfac)
        endif
        dvz = (2.0 * sq6 * sqrt( cdisp*itdelt ) *   &
              random_step  +  vz * itdelt)  ! note that negative value is now sinking (against the direction of the local h coordinate)
        depthp = depthp + dvz  ! depth is positive downwards,and dvz is the increase in depth

        ! new depth is now calculated and now set the layer and hpart or reached top/bottom
        rise = dvz < 0
        sink = dvz > 0
        neutral = dvz == 0
        if ( depthp <= 0.0 ) then
            depthp = 0.0d0
            laypart(ipart) = 1 ! this does not take into accoutn z-layers where surface may not be layer 1
            hpart(ipart) = 0.0d0
        elseif ( depthp >= totdepthlay(kmx) ) then
            depthp = totdepthlay(kmx)
            laypart(ipart) = kmx
            hpart(ipart) = 1.0d0
        else
            ! find layer starting from partlay and look down if sink or up if rise
            ilay = partlay
            if ( sink) then
                do while ( depthp > totdepthlay(ilay) )
                    ilay =  ilay + 1
                end do
            elseif ( rise ) then
                do while ( depthp < totdepthlay(ilay)-h0(laypart(ipart)) )
                   ilay =  ilay - 1
                end do
            endif
            laypart(ipart) = ilay ! new layer number
            hpart(ipart) = 1.0d0 - ( totdepthlay(ilay) - depthp) / h0(laypart(ipart)) ! new relative height in layer
        end if
    end do
end subroutine
end module
