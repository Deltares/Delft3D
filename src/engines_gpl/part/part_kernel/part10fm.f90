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

module part10fm_mod
    use m_fm_particles_in_grid

    implicit none

contains
subroutine part10fm()
!
!    CALCULATES PARTICLE MOTION FROM DISPERSION, WIND AND SETTLING
!                 (per time step)
!
!     system administration : frank kleissen
    use m_waq_precision
    use partmem, only: nopart, modtyp, drand, oil, nfract, wpart, iptime, abuoy, t0buoy, ldiffh, nosubs, mpart
    use m_part_mesh
    use m_particles, laypart => kpart
    use m_part_times
    use m_part_recons
    use m_part_transport
    use m_sferic
    use m_sferic_part, only: ptref
    use geometry_module, only: Cart3Dtospher, sphertocart3D
    use physicalconsts, only: earth_radius
    use mathconsts, only: raddeg_hp
    use fileinfo  , lun=> lunit    ! logical unit numbers for files
    use random_generator
    use timers
    use m_part_modeltypes

    !locals
    logical          :: partdomain, skip_pt, openbound, mirror
    integer          :: ipart, ifract, niter
    double precision :: rseed  =  0.5d+00

    integer(int_wp )   :: ierror                  ! needed to call part07
    integer(int_wp )   :: isub                    ! loop counter nosubs
    integer(int_wp )   :: itdelt                  ! delta-t of the particle for smooth loading
    integer(int_wp )   :: kp                      ! k of the particle
    logical            :: twolay                  ! = modtyp .eq. model_two_layer_temp
    double precision      :: abuac           ! actual value of abuoy(ipart) ( * sqrt(ddfac)
    double precision      :: cdrag           ! local drag coefficient (converted from persentage)
    double precision      :: dax             ! delta of diffusive spreading x
    double precision      :: day             ! delta of diffusive spreading y
    real(sp)              :: ddfac           ! control variable for smooth loading
    double precision      :: dran1           ! actual value of drand(1) or drand(1)*sqrt(ddfac)
    double precision      :: dradius         ! horizontal diffusion step distance
    double precision      :: dpangle         ! horizontal diffusion step angle
    double precision      :: sq6             ! = sqrt(6.0)
    double precision      :: t0              ! helpvariable with initial time step buoyant spreading
    double precision      :: tp              ! real value of iptime(ipart)
    double precision      :: trp             ! horizontal random walk
    real(sp)              :: wdirr           ! is wind direction in radians
    integer               :: mpartold,iedge, i, maxiter, npbounce
    double precision      :: xpartold, ypartold, zpartold, xnew, ynew, fangle, fanglew, difangle
    double precision      :: dpxwind, dpywind, windcurratio, dwx, dwy, daz, xcr, ycr
    double precision, dimension(1)      :: xx, yy
    integer(4) ithndl              ! handle to time this subroutine
    data       ithndl / 0 /

    if ( timon ) call timstrt( "part10fm", ithndl )

    maxiter = 100
    twolay = modtyp .eq. model_two_layer_temp
    sq6    = sqrt( 6.0 )
    ddfac  = 2.0
    dran1  = drand(1)
    ipart = 1
    xpartold = xpart(ipart)
    ypartold = ypart(ipart)
    mpartold = mpart(ipart)
    if (jsferic == 1) zpartold = zpart(ipart)

    partdomain = mpart(ipart) == 0 !after advection

    openbound = .TRUE.

    niter = 0
    npbounce = 0
    nqzero = 0
    cdrag  = drand(3) / 100.0          !  wind drag as a fraction
    i = 1

    ! first after the first timestep check which L's have discharge = 0 (they ware closed boundaries )
    if (time0 > tstart_user) then
        do iedge=1, numedges
            if (qe(iedge)== 0.0D0) then
                qbnd(i) = iedge
                i = i +1
            endif
        enddo
        nqzero = i-1
    endif

    ! Here we need to check which segments have a closed boundary
    !
    ! Note AM (2 september 2021): this loop needs revising, but for the moment let us concentrate on
    ! the diffusion coefficient
    !

    ! pick up the current particle coordinates loop over the particles, no movement if the sticky mass >0

    do while (ipart <= nopart)
        if (niter >= maxiter) then
            partdomain = .TRUE. ! to escape theloop
            npbounce = npbounce + 1
        endif
        skip_pt = .FALSE.
        if (niter==0) mirror = .TRUE.
        partdomain =  mpart(ipart) == 0 ! this is for the particles that come out of advection outside model domain
        ! if after the advectio the particle is outside the gridthen also set the previous mpart to 0 to avoid resetting back into the domain
        if (partdomain) then
            skip_pt = .TRUE.
            mpart_prevt(ipart) = 0
        endif

        if (oil) then
            do ifract = 1 , nfract
                if (wpart(3 + 3 * (ifract - 1), ipart) > 0.0) skip_pt = .TRUE.
            enddo
        endif
        if (.not.skip_pt) then
            tp = real(iptime(ipart), kind=kind(tp))
            abuac  = abuoy(ipart)
            dran1  = drand(1)
            if ( tp .lt. 0.0 ) then           !   adaptations because of smooth loading
                tp     = 0.0
                itdelt = dts + iptime(ipart)
                ddfac  = float(itdelt)/dts
                dran1  = dran1 * sqrt(ddfac)
                abuac  = abuac * sqrt(ddfac)
            endif

            ! calculate the dispersion step if the switch for horizontal diffusion is true
            !
            ! Note AM: handle spherical grids differently
            !
            if ( ldiffh ) then
                trp = dran1 * tp ** drand(2) ! Effectively 2.0 sqrt( D delta-t )
                t0  = t0buoy(ipart)
                if ( twolay .and. t0 .gt. 0.0 .and. kp .eq. 1 ) then
                    trp = max( trp , abuac * (tp+t0)**(-0.125) )     ! bouyancy spreading parameter
                endif
                if (mirror) then
                    dax = sq6 * trp * (rnd(rseed) - 0.5)
                    day = sq6 * trp * (rnd(rseed) - 0.5)
                    mirror = .FALSE.
                else
                    day = -day
                    dax = -dax
                    mirror = .TRUE.
                endif
            else
                dax = 0.0
                day = 0.0
            endif
            if (partdomain) then ! only update if the particle was not outside the model domain (open boundary), also if outside (partdomain true) then remove mass
                do isub = 1 , nosubs
                    wpart(isub,ipart) = 0.0D0
                enddo
                ! move to next particle
                niter = 0
                ipart = ipart + 1
                xpartold = xpart(ipart)
                ypartold = ypart(ipart)
                mpartold = mpart(ipart)
                if  (jsferic==1) zpartold = zpart(ipart)
                partdomain = mpart(ipart) == 0
                openbound = .TRUE.
            else  ! particle is within the model domain
                xpart(ipart) = xpartold + dax
                ypart(ipart) = ypartold + day
                !temporary check
                ! assign the new k (segment) for this new coordinate for the original coordinate system, if sferical provide the actual sferical coordinates.
                xx(1) = xpart(ipart)
                yy(1) = ypart(ipart)
                if (jsferic == 1) call Cart3Dtospher(xpart(ipart),ypart(ipart),zpart(ipart),xx(1),yy(1),ptref)
                call part_findcell(1,xx,yy,mpart(ipart:ipart),ierror)

                !  We sill need to check for internal boundaries (eg thin dam or dry cell in the FM model)
                call checkpart_openbound(ipart, xpartold, ypartold, mpartold, openbound, xcr, ycr) ! if openbound = false then there is an internal (no flow) boundary.

                if (mpart(ipart)>0 .and. openbound) then
                    ! stays in model due to dispersion, now ceck on the wind and recalculate x, y, but only for the oil model (surface floating), to be consistent with the delft3d approach
                    ! for all oil fractions. If it hits mpart=0 due to dispersion the particle will not stick but resamples.
                    if (oil) then
                        do ifract = 1 , nfract
                            if (wpart(1 + 3 * (ifract - 1), ipart) > 0.0) call part10fm_pdrag(ipart, ifract, rseed) ! only for floating oil
                        enddo
                    endif

                    niter =0
                    xpart_prevt(ipart) = xpart(ipart)  ! to remember the location before the next advectionstep
                    ypart_prevt(ipart) = ypart(ipart)
                    mpart_prevt(ipart) = mpart(ipart)
                    if  (jsferic ==1) zpart_prevt(ipart) = zpart(ipart)
                    ipart = ipart + 1 ! move to the next particle
                    xpartold = xpart(ipart)
                    ypartold = ypart(ipart)
                    mpartold = mpart(ipart)
                    openbound = .TRUE.
                    if (jsferic ==1) zpartold = zpart(ipart)
                    partdomain = mpart(ipart) == 0
                elseif (niter < maxiter) then ! to prevent ending up in a endless loop just for the dispersion step, due to this it can cross an intenal boundary (eg thin dam)
                    niter = niter + 1
                    xpart(ipart) = xpartold
                    ypart(ipart) = ypartold
                    mpart(ipart) = mpartold
                    partdomain = mpart(ipart) == 0
                    openbound = .TRUE.
                    if (jsferic ==1) zpart(ipart) = zpartold
                endif
            endif
        else
            xpart(ipart) = xpart_prevt(ipart)  ! when the particle is skipped then reset the coordinates to those of the previous timestep
            ypart(ipart) = ypart_prevt(ipart)
            mpart(ipart) = mpart_prevt(ipart)
            ! if particle was outside domain then stay there
            !mpart(ipart)=0

            if (jsferic == 1) zpart(ipart) = zpart_prevt(ipart)
            ipart = ipart + 1 !now we move to the next particle
            xpartold = xpart(ipart)
            ypartold = ypart(ipart)
            mpartold = mpart(ipart)
            partdomain = mpart(ipart) == 0
            openbound = .TRUE.
            if (jsferic ==1) zpartold = zpart(ipart)
        endif
    enddo

    ! need to store the x, y and k in case of sticky oil that should not move.
    write (   *  , * )'Number of particles reaching maximum bouncing iterations: ', npbounce
    write(lun(2)  , * )'Number of particles reaching maximum bouncing iterations: ', npbounce

    if ( timon ) call timstop ( ithndl )

    return
    end subroutine

    subroutine part10fm_pdrag(ipart, ifract, rseed)
!
!   CALCULATES PARTICLE MOTION FROM DISPERSION, WIND AND SETTLING
!               (per time step)
!
!   system administration : frank kleissen
    use partmem
    use m_particles
    use m_part_times
    use m_part_recons
    use m_sferic
    use m_sferic_part,only: ptref

    use m_part_mesh, only: xzwcell, yzwcell, zzwcell
    use geometry_module, only: Cart3Dtospher, sphertocart3D
    use physicalconsts, only: earth_radius
    use mathconsts, only: raddeg_hp
    use random_generator
    use timers
    use m_part_modeltypes

    !locals
    logical               :: partdomain, openbound
    integer               :: ipart, niter
    double precision      :: rseed

    integer(int_wp )      :: ierror                  ! needed to call part07
    integer(int_wp )      :: ifract                  ! loop counter for nfract
    integer(int_wp )      :: itdelt                  ! delta-t of the particle for smooth loading
    integer(int_wp )      :: kp                      ! k of the particle
    logical               :: dstick                  ! logical that determines sticking
    logical               :: twolay                  ! = modtyp .eq. model_two_layer_temp
    double precision      :: abuac                   ! actual value of abuoy(ipart) ( * sqrt(ddfac)
    double precision      :: cdrag                   ! local drag coefficient (converted from persentage)
    double precision      :: dax                     ! delta of diffusive spreading x
    double precision      :: day                     ! delta of diffusive spreading y
    real(sp)              :: ddfac                   ! control variable for smooth loading
    double precision      :: dran1                   ! actual value of drand(1) or drand(1)*sqrt(ddfac)
    double precision      :: dradius                 ! horizontal diffusion step distance
    double precision      :: dpangle                 ! horizontal diffusion step angle
    double precision      :: sq6                     ! = sqrt(6.0)
    double precision      :: t0                      ! helpvariable with initial time step buoyant spreading
    double precision      :: tp                      ! real value of iptime(ipart)
    double precision      :: trp                     ! horizontal random walk
    double precision      :: wdirr                        ! is wind direction in radians
    integer               :: mpartold, npadd, mparttemp
    integer               :: nfcons = 10
    double precision      :: xpartold, ypartold, zpartold, xnew, ynew, fangle, fanglew, difangle
    double precision, dimension(1) :: xx, yy
    double precision      :: dpxwind, dpywind, windcurratio, wvel_sf, dwx, dwy, ux0, uy0, ux0old, uy0old, xcr, ycr
    double precision      :: ioptev(nfract)
    integer(4), save      :: ithndl = 0              ! handle to time this subroutine

    if ( timon ) call timstrt( "part10fm_pdrag", ithndl )

    twolay = modtyp .eq. model_two_layer_temp
    sq6    = sqrt( 6.0 )
    ddfac  = 2.0
    twopi  = 8.0 * atan(1.0)
    dran1  = drand(1)
    openbound = .TRUE.
    xpartold = xpart(ipart)
    ypartold = ypart(ipart)
    mpartold = mpart(ipart)
    partdomain = mpart(ipart) == 0
    !partdomain = .FALSE.    ! if false the particle is in the domain
    niter = 0
    cdrag  = drand(3) / 100.0          !  wind drag as a fraction
    ! the next section is taken from the oildsp routine to allow access to the stickyness (if oilmod).
    if (oil) then
        npadd = 0
        ioptev (ifract) = const((ifract-1)*nfcons+npadd+ 1)       ! evaporation option (-2 (Fingas incl. effect of waterfraction on evaporation, or -1 = fingas, >0 = first order)
        if (ioptev (1).le.-1) then
            npadd = npadd + 2
        endif
        fstick(ifract) = const((ifract - 1)*nfcons+npadd+ 4)      ! stickiness probability [0,1], this part is taken from the delf3d oildsp code
    endif

    wdirr = wdir(mpart(ipart)) * twopi / 360.0d0
    dpxwind  = - wvelo(mpart(ipart)) * sin( wdirr) !TODO check the angles . for both sferical as wel as cartesian grids.
    dpywind  = - wvelo(mpart(ipart)) * cos( wdirr)  ! here it is in m/s
    wvel_sf  = atan2(wvelo(mpart(ipart)),earth_radius) * raddeg_hp !wind magnitude in degrees/sec

    ! drag on the difference vector: cd * (wind - flow)
    ux0 = u0x(mpart(ipart)) + alphafm(mpart(ipart))*(xpart(ipart)-xzwcell(mpart(ipart)))  ! in m/s ?
    uy0 = u0y(mpart(ipart)) + alphafm(mpart(ipart))*(ypart(ipart)-yzwcell(mpart(ipart)))
    ux0old = u0x(mpartold) + alphafm(mpartold)*(xpartold-xzwcell(mpartold))  ! in m/s ?
    uy0old = u0y(mpartold) + alphafm(mpartold)*(ypartold-yzwcell(mpartold))
    dwx = cdrag*(dpxwind - ux0) * dts
    dwy = cdrag*(dpywind - uy0) * dts  !this is for carthesian grids.
    xpart(ipart) = xpartold  + dwx    !cartesian
    ypart(ipart) = ypartold  + dwy    !

    ! the z-coordinate needs to be updated
    if (jsferic .eq. 1) then ! if sferical then for a good conversion we need to calculate distances first in sferical to be able to update zpart, is this correct?
        zpartold = zpart(ipart)
        ux0old = atan2(ux0old,earth_radius) * raddeg_hp
        uy0old = atan2(uy0old,earth_radius) * raddeg_hp
        dpxwind  = - wvel_sf * sin( wdirr)  !in degrees (radians). note that the direction is from the north and defined clockwise, 0 means no x displacement
        dpywind  = - wvel_sf * cos( wdirr)
        dwx = cdrag*(dpxwind - ux0old) * dts
        dwy = cdrag*(dpywind - uy0old) * dts
        call Cart3Dtospher(xpartold,ypartold,zpartold,xx(1),yy(1),ptref)
        xx(1) = xx(1) + dwx
        yy(1) = yy(1) + dwy
        call sphertocart3D(xx(1),yy(1),xpart(ipart),ypart(ipart),zpart(ipart)) !to convert back to meters
    endif

    xx(1) = xpart(ipart)
    yy(1) = ypart(ipart)
    if (jsferic == 1) call Cart3Dtospher(xpart(ipart),ypart(ipart),zpart(ipart),xx(1),yy(1),ptref)
    call part_findcell(1,xx(1),yy(1),mpart(ipart:ipart),ierror)
    call checkpart_openbound(ipart, xpartold, ypartold, mpartold, openbound, xcr, ycr)  ! check around the starting point and end point

    ! if the particle goes outside the domain due to wind, use the current angle to transport, otherwise it sticks
    dstick = rnd(rseed) > fstick(ifract) ! if dstick is true then the particle will not stick
    if ((mpart(ipart) == 0 .or. .not.openbound) .and. dstick) then
        ! here we can use the fstick parameter to select whether to take the particle out (kpart-0), ie sticks. Question is that this will not work in case of an internal boundary
        fangle  = datan2( ux0old , uy0old)
        fanglew = datan2(dpywind,dpxwind)
        difangle = min( twopi - abs(fangle - fanglew), abs(fangle - fanglew))

        ! to map it on the current vector use cos
        windcurratio = dcos(difangle) * wvelo(mpartold)
!        if ( jsfer_old ==1 )  windcurratio = dcos(difangle) *  atan2(wvelo(mpartold),earth_radius) * raddeg_hp
        dpywind = windcurratio * uy0old
        dpxwind = windcurratio * ux0old

        !now we can calculate the new displacement in the direction of hte flow
        dwx = (cdrag*(dpxwind-ux0old)) * dts
        dwy = (cdrag*(dpywind-uy0old)) * dts
        xpart(ipart) = xpartold  + dwx
        ypart(ipart) = ypartold  + dwy

        xx(1) = xpart(ipart)
        yy(1) = ypart(ipart)
        if (jsferic == 1) call Cart3Dtospher(xpart(ipart),ypart(ipart),zpart(ipart),xx(1),yy(1),ptref)
        call part_findcell(1,xx(1),yy(1),mpart(ipart:ipart),ierror)
        call checkpart_openbound(ipart, xpartold, ypartold, mpartold, openbound, xcr, ycr)  ! check around the starting point and end point

    elseif ((mpart(ipart) == 0 .or. .not.openbound) .and. .not.dstick .and. wpart(1 + 3 * (ifract - 1), ipart)>0.0 ) then !now it sticks and the floating mass is transferred to sticky mass and put the particles in their old position, so that they show up in concentrations
        ! if mpart= 0 here then the transport is across an open boundary ?
        if (.not.openbound) then
            wpart(3 + 3 * (ifract - 1), ipart) = wpart(1 + 3 * (ifract - 1), ipart)
            wpart(1 + 3 * (ifract - 1), ipart) = 0.0d0
            xpart(ipart) = xcr
            ypart(ipart) = ycr
            mpart(ipart) = mpartold
            if (jsferic == 1) zpart(ipart) = zpartold
        endif
    endif

    ! if a closed boundary is found after resetting and moving with wind along flow lines then it is stuck in a corner and then reset particle
    if (.not. openbound) then
        xpart(ipart) = xcr
        ypart(ipart) = ycr
        mpart(ipart) = mpartold
        if (jsferic == 1) zpart(ipart) = zpartold
    endif
    if ( timon ) call timstop ( ithndl )

    return
    end subroutine

    subroutine checkpart_openbound(ipart, xpartold, ypartold, mpartold, openbound, xcr, ycr)
    use m_part_times, only: dts, time0
    use m_particles
    use m_part_recons
    use m_part_mesh
    use partmem, only: mpart, hyd
    use m_part_geom
    use MessageHandling
    use m_sferic_part, only: jsferic
    use timers
    use geometry_module
    use network_data

    integer          :: ipart
    integer          :: i, k, k1, k2, L!, kpartold
    integer          :: ja, jacros
    integer          :: Lexit
    double precision :: sl
    double precision :: sm, xcr, ycr, crp
    double precision :: dmiss

    ! locals

    double precision            :: d, un
    double precision            :: t, tex, dt
    double precision            :: ux0, uy0, cs, sn, xpartold, ypartold, txk, tyk
    integer                     :: mpartold, mpart_tmp
    double precision            :: xn, yn, zn, rl
    double precision            :: dvar, dis, dn

    double precision, dimension(3) :: ddn

    logical                     :: isboundary
    logical, intent(out)        :: openbound !! true then the particle crosses an openboundary (ie no sticking)

    double precision, parameter :: DTOL = 1d-4
    double precision, parameter :: DTOLd  = 1d-4
    double precision, parameter :: DTOLun_rel = 1d-4
    double precision, parameter :: DTOLun = 1e-14
    double precision            :: tolx, toly
    integer,          parameter :: MAXNUMZERO = 10

    integer(4), save            :: ithndl = 0             ! handle to time this subroutine
    integer                     :: j, iq

    if (mpartold == 0) then
        return  ! if the particle was not in the grid then skip
    endif

    dmiss = -999.D0
    openbound = .TRUE.
    !go along the path between new and old to find a closed boundary
    mpart_tmp = mpartold
    do while (mpart_tmp .ne. mpart(ipart))
        ! cross the boundaries and find next cell
        ! loop over the edges
        isboundary = .FALSE.
        do i=jcell2edge(mpart_tmp),jcell2edge(mpart_tmp+1)-1
            L = icell2edge(i)   ! edge
            k1 = edge2node(1, L)
            k2 = edge2node(2, L)
            call CROSS (xpart(ipart), ypart(ipart), xpartold, ypartold, xnode(k1), ynode(k1), xnode(k2), ynode(k2), &
                       JACROS,SL,SM,XCR,YCR,CRP, 0, dmiss)
            if (jacros == 1) then
                isboundary = .TRUE.
                ! check whether the bondary is closed
                if (abs(qe(L)) == 0.0D0) then
                    openbound = .FALSE.
                    return  ! if a closed boundary is found then return with openbound = false
                else
                    ! ensure a tolerance so that the coordinate is actually in the next cell.
                    tolx = sign(1.0D0,(xpart(ipart) - xcr)) * DTOL
                    toly = sign(1.0D0,(ypart(ipart) - ycr)) * DTOL
                    xpartold = xcr + tolx
                    ypartold = ycr + toly
                    ! find the neighbouring cell
                    if ( edge2cell(1,L).gt.0 .and. edge2cell(2,L).gt.0 ) then   ! internal edge (netlink)
                        mpart_tmp = edge2cell(1,L) + edge2cell(2,L) - mpart_tmp
                    else
                        mpart_tmp = 0  ! this is outside the grid exit the routine
                        mpart(ipart) = 0
                        openbound = .TRUE. ! has to be an open boundary because the q is greater than 0
                        return
                    endif
                endif
                exit !one boundary was found, go to next cell
            endif

        end do
        ! if no boundary found, then we are in the same segment,  exit the routine
        if (.not.isboundary) then
            return
        endif
    end do

    end subroutine
end module
