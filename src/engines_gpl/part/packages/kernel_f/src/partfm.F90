!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2023.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

   subroutine partfm(lunpr)

   use precision_part
   use MessageHandling
   use partmem
   use m_partmesh
   use m_particles, only: NopartTot, Nrpart, trpart, xrpart, yrpart, zrpart, mrpart, irpart
   use part10fm_mod
   use oildspfm_mod
   use m_partfm_decay
   use alloc_mod
   use m_alloc
   use fileinfo  , lun=> lunit    ! logical unit numbers for files
   use m_flowtimes
   use timers
   use m_sferic_part, only: ptref
   use m_sferic, only: jsferic
   use geometry_module, only: Cart3Dtospher, sphertocart3D
   use physicalconsts, only: earth_radius
   use mathconsts, only: raddeg_hp, pi
   use random_generator

   implicit none

   integer(ip), intent(in) :: lunpr
   integer(ip)             :: ierror, ntimes, ictimes, npload, ipartload
   integer(ip)             :: lunmem, istat, itime, ilay, Ldot, ipart, iload, idye, iinsti, ndpart
   double precision        :: dtcontp, totcload
   double precision        :: rseed = 0.5d0
   double precision        :: dpangle, dxp, dyp, dradius, xx, yy
   integer(4) ithndl              ! handle to time this subroutine
   logical                 :: mapfil  ! true if map file extension
   logical                 :: trkfil   ! true if track file extension
   data ithndl / 0 /
   if ( timon ) call timstrt( "partfm", ithndl )

   call SetMessageHandling(lunMessages=lunpr)
   ! initialize the allocation system
   call init_alloc( lunmem , lunpr )

   if (hyd%nolay /= 1) then
      write ( lunpr, * ) ' WARNING: 3D hydrodynamics is not yet supported for unstructured grids!'
      write (   *  , * ) ' WARNING: 3D hydrodynamics is not yet supported for unstructured grids!'
   endif
   !dts   = real(hyd%cnv_step_sec, 8)  !idelt in seconds taken from the hyd file (conversion timestep)
   tzone = 0.0_hp
   refdat = hyd%HYD_REF(1:8)
   call setTUDUnitString()

   !
   ! For a model based on z-layers we need extra administration
   ! but that is not part of the current implementation yet
   !
   allocate( laytop(0,0), laybot(0,0), laytopp(0,0) )

   !
   ! Read the grid information and the actual input file
   !
   call ini_part_grid(hyd)

   call rdpart ( lun(1)   , lun(2)   , fname(1) )

   dts       = real(idelt, kind=kind(dts))
   noparttot = npmax + npmax/100
   nopart    = 0
   allocate( nplay(hyd%nolay) )

!  part08 - calculates total released mass and mass/particle

   call part08 ( lun(2)   , nodye    , nocont   , ictmax   , amassd   ,    &
                 ictime   , amassc   , aconc    , tmass    , tmassc   ,    &
                 nosubs   , ndprt    , tmassu   , ftime    , linear   ,    &
                 substi   , nmdyer   , nmconr   )


!  calculate dump-sites in the grids

   call part06fm ( lun(2)   , nodye    , nocont   , xwaste   ,    &
                   ywaste   , zwaste   , nwaste   , mwaste   )

   tstart_user = itstrtp
   tstop_user  = itstopp
   Ldot = index(fname(1), '.', .true.)-1
   filebase = ' '
   filebase = fname(1)(1:Ldot)


   !! AM NpartTot = npmax
   !! AM Nrpart = npmax !npmax is the number of particles released due to the instantaneous and continuous discharges

   call ini_part(partinifile, partrelfile, tstart_user, dts, 0)

if (.false.) then
   call realloc(trpart, Nrpart)
   call realloc(xrpart, Nrpart)
   call realloc(yrpart, Nrpart)
   call realloc(zrpart, Nrpart)
   call realloc(mrpart, Nrpart)
   !set up the trpart xrpart etc arrays
   !loop over the instantaneous discharges, still assuming one substance
   ipart = 1
   npmax = 0
   do iload = 1, nodye
        npmax = ndprt(iload) + npmax
        wpart(1, ipart:npmax+1) = amassd(1, iload) / ndprt(iload)
        do while (ipart <= npmax)
            dpangle =2.0D0 * pi * rnd(rseed)
            dradius = rnd(rseed) * radius(iload) !noteradius is in m. need to convert to degrees.
            dxp = cos(dpangle) * dradius
            dyp = sin(dpangle) * dradius
            trpart(ipart) = iwtime(iload)
            if (jsferic == 1) then
                dradius = atan2(dradius,earth_radius)*raddeg_hp !in degrees
                dxp = cos(dpangle) * dradius
                dyp = sin(dpangle) * dradius
! the distance is expressed in degrees (to make a circle for spherical models,
                !call Cart3Dtospher(dble(xwaste(iload)),dble(ywaste(iload)),dble(zwaste(iload)),xx,yy,ptref)
              !  xx = xx + dxp
               ! yy = yy + dyp
                !call sphertocart3D(xx,yy,xrpart(ipart),yrpart(ipart),zrpart(ipart))
            endif
            xrpart(ipart) = xwaste(iload) + dxp !radius(iload)/2. * rnd(rseed)
            yrpart(ipart) = ywaste(iload) + dyp !radius(iload)/2. * rnd(rseed)
            zrpart(ipart) = zwaste(iload)
        !  endif
            ipart = ipart +1
        end do
        ipart = ipart -1
   enddo

   ndpart = npmax-1
   do iload = 1, nocont  !here follows the infor from the continuous discharge, no time interpolation.
        npmax = ndprt(nodye + iload) + npmax
        ! total mass per load to calculate mass per particle
        totcload = 0.0
        do ictimes = 1, ictmax(iload)-1
            if (linear(iload) == 0) then
                totcload = totcload + amassc(iload,1,ictimes) * (ictime(iload, ictimes+1) - ictime(iload, ictimes))
            else
                totcload = totcload + amassc(iload,1,ictimes) + abs((amassc(iload, 1, ictimes+1) - amassc(iload, 1, ictimes))) * &
                          (ictime(iload, ictimes+1) - ictime(iload, ictimes))/2.0 !total mass for interpolated load
            endif

        end do
        ! calculate the mass per particle for this load, store this in wpart
        wpart(1, ipart:npmax+1) = totcload / ndprt(nodye + iload)
        npload = 0
        ! her add the loop for the number of timings
        ipartload = 0
        do ictimes = 1, ictmax(iload)-1
            trpart(ipart)=ictime(iload,ictimes)
            xrpart(ipart) = xwaste(iload) !radius(iload)/2. * rnd(rseed)
            yrpart(ipart) = ywaste(iload) !radius(iload)/2. * rnd(rseed)
            zrpart(ipart) = zwaste(iload)
            ntimes = (ictime(iload, ictimes + 1) - ictime(iload, ictimes)) / dts
            ipartload = amassc(iload,1,ictimes) * (ictime(iload, ictimes+1) - ictime(iload, ictimes)) / wpart(1, ipart) ! this is the number of particles to be added for this period
            npload = npload + ipartload
            if (ipartload > 0) then
               dtcontp =   (float(ntimes) * dts) / ipartload
            else
               dtcontp = 0.0
            endif
            do while (ipart <= ndpart + npload)
                dpangle =2.0D0 * pi * rnd(rseed)
                dradius = rnd(rseed) * radius(iload)
                dxp = cos(dpangle) * dradius
                dyp = sin(dpangle) * dradius
                if (jsferic == 1) then ! the distance is expressed in degrees(to make a circle for spherical models,
                    dradius = atan2(dradius,earth_radius)*raddeg_hp !in degrees
                    dxp = cos(dpangle) * dradius
                    dyp = sin(dpangle) * dradius
                endif
                xrpart(ipart) = xwaste(iload) + dxp !radius(iload)/2. * rnd(rseed)
                yrpart(ipart) = ywaste(iload) + dyp !radius(iload)/2. * rnd(rseed)
                zrpart(ipart) = zwaste(iload)
                trpart(ipart+1) = trpart(ipart) + dtcontp  !here the time is set for each particle that is released.
                ipart = ipart +1
            end do
            ipartload = ipart-1
        end do
   enddo
endif
   call realloc(xrpart, npmax)
   call realloc(yrpart, npmax)
   call realloc(zrpart, npmax)

   call realloc_particles(npmax, .true., ierror)
   irpart = 1
   ptref = 0.0D0
   !call part_findcell(Nrpart,xrpart,yrpart,mrpart,ierror)

   if ( notrak > 0 ) call unc_init_trk()
   call unc_init_map(hyd%crs, hyd%waqgeom, hyd%nosegl, hyd%nolay)

   time0 = tstart_user
   time1 = time0
   istat = -1 ! skip copying of data durin the first stime stap
   call part_readhydstep(hyd,itime,istat)
   if (istat == 99) then
      write ( lunpr, * ) ' Timing mismatch between input and actual data:', time0, itime
      write (   *  , * ) ' Timing mismatch between input and actual data:', time0, itime
      goto 1234
   else if (istat /= 0) then
      write ( lunpr, * ) ' Error during reading of the hydrodynamic time step data', time0, itime
      write (   *  , * ) ' Error during reading of the hydrodynamic time step data', time0, itime
      goto 1234
   end if

   do while (istat == 0)
   !     determine if map and track files must be produced

      mapfil = .true.
      trkfil = .true.
      if ( notrak .eq. 0 ) trkfil = .false.
      if (icwste                     < 1     ) mapfil = .false.
      if (itime                      < icwsta) mapfil = .false.
      if (itime - idelt              >=  icwsto) mapfil = .false.
      if (mod(itime-icwsta, icwste)  >=  idelt ) mapfil = .false.

      if ( trkfil .and. mod(itime, notrak * idelt) .ge. idelt) trkfil = .false.
      if ( trkfil ) call unc_write_trk()
      if (mapfil) call unc_write_map()

      call report_progress( lunpr, int(time0), itstrtp, itstopp, nopart, npmax )



      if (time1 .ge. tstop_user) then
         exit
      endif
      time0 = time1
      time1 = min(tstop_user, time0 + dts)
      call part_readhydstep(hyd,itime,istat)
      if ( idtset .gt. 0 )                                                    &
         call part17 ( itime    , nosubs   , idtset   , idtime   , decay    ,    &
                       decays   )
      call partfm_decay()
!     interpolation for wind speed/direction in the wind table
      call part15 ( lun(2)   , itime    , spawnd   , numcells , nowind   ,    &
                    iwndtm   , wveloa   , wdira    , wvelo    , wdir     )
!     transport (advection, dispersion, winddrag)
!      jsfer_old = jsferic
!      jsferic = 0 ! everything in part10fm is in meters
      call update_part(itime)
      call part10fm()
!      jsferic = jsfer_old ! back to what it should be
      call oildspfm(itime)
!     interpolation for wind speed/direction in the wind table
   end do

1234 continue

   call unc_close_trk()
   call unc_close_map()

   if ( timon ) call timstop ( ithndl )

   return

   contains

   subroutine report_progress( lunpr, itime, itstrtp, itstopp, nopart, npmax )
   integer, intent(in) :: lunpr, itime, itstrtp, itstopp, nopart, npmax

   real                :: pctprogress

   pctprogress = 100.0 * (real(itime,4) - real(itstrtp,4)) / (real(itstopp,4) - real(itstrtp,4)) ! percentage progress

   write ( *,     1020) itime  /86400, mod(itime  , 86400)/3600, mod(itime  , 3600)/60, mod(itime  , 60),  &
                        itstopp/86400, mod(itstopp, 86400)/3600, mod(itstopp, 3600)/60, mod(itstopp, 60),  &
                        pctprogress, nopart, npmax
   write ( lunpr, 1020) itime  /86400, mod(itime  , 86400)/3600, mod(itime  , 3600)/60, mod(itime  , 60),  &
                        itstopp/86400, mod(itstopp, 86400)/3600, mod(itstopp, 3600)/60, mod(itstopp, 60),  &
                        pctprogress, nopart, npmax

 1020 format(/,'  Time ', i6.4 ,'D-', i2.2 ,'H-', i2.2 ,'M-', i2.2 ,'S.',' Stop time ',     &
                 i6.4 ,'D-', i2.2 ,'H-', i2.2 ,'M-', i2.2 ,'S. (', f5.1, '% completed) ',   &
                 i11,' part. (of',i11,')')

   end subroutine report_progress
   end subroutine partfm

   subroutine update_particles(q,h0,h1,Dt)
   use precision_part
   use partmem, only: nopart, mpart
   use m_particles
   use m_flowgeom, only: Ndx, Lnx
   use m_sferic
   use m_sferic_part, only: ptref
   use geometry_module, only: Cart3Dtospher
   use MessageHandling
   use timers

   implicit none

   double precision, dimension(Lnx), intent(in) :: q  !< fluxes
   double precision, dimension(Ndx), intent(in) :: h0 !< water deoths at start of time interval
   double precision, dimension(Ndx), intent(in) :: h1 !< water deoths at end of time interval
   double precision,                 intent(in) :: Dt !< time interval

   integer, dimension(1) :: numremaining ! number of remaining particles to be updated

   double precision :: xx, yy

   integer :: i
   integer :: iter
   integer :: ierror

   integer, parameter :: MAXITER = 1000 ! maximum number of substeps

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "update_particles", ithndl )

   ierror = 1

   ! reconstruct velocity field
   call reconst_vel(q, h0, h1, ierror)
   if ( ierror.ne.0 ) goto 1234

   if ( Nopart.gt.0 ) then
      ! set remaining time to time step
      dtremaining = Dt
      numzero = 0
   end if

   do iter=1,MAXITER
      ! update particles in cells
      call update_particles_in_cells(numremaining(1), ierror)
      if ( ierror.ne.0 ) goto 1234

      write(6,*) 'iter=', iter, 'numremaining=', numremaining(1)
      if ( numremaining(1).eq.0 ) then
         write(6,*) 'iter=', iter
         exit
      end if
   end do


   ! check for remaining particles
   if ( numremaining(1).gt.0 ) then
      ! plot remaining particles
      do i=1,Nopart
         if ( dtremaining(i).gt.0d0 .and. mpart(i).gt.0 ) then
            if ( jsferic.eq.0 ) then
               xx = xpart(i)
               yy = ypart(i)
            else
               call Cart3Dtospher(xpart(i),ypart(i),zpart(i),xx,yy,ptref)
            end if
            write(*,"(I0, ':', 2E25.15, ', ', I0)") i, xx, yy, mpart(i)
         end if
      end do
      call mess(LEVEL_WARN, 'update_particles: iter>MAXITER')
      goto 1234
   end if

   ierror = 0
1234 continue

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine update_particles

   !> update positions of particles within triangles
   subroutine update_particles_in_cells(numremaining, ierror)
   use partmem, only: nopart, mpart
   use m_particles
   use m_partrecons
   use m_partmesh
   use MessageHandling
   use m_sferic, only: jsferic
   use timers

   implicit none

   integer,        intent(out) :: numremaining !< number of remaining particles to be updated
   integer,        intent(out) :: ierror       !< error (1) or not (0)

   integer                     :: ipart
   integer                     :: i, k, k1, k2, L
   integer                     :: ja
   integer                     :: Lexit

   double precision            :: d, un
   double precision            :: t, tex, dt
   double precision            :: ux0, uy0, uz0, cs, sn
   double precision            :: xn, yn, zn, rl
   double precision            :: dvar, dis, dn

   double precision, dimension(3) :: ddn

   logical                     :: isboundary

   double precision, parameter :: DTOL = 1d-4
   double precision, parameter :: DTOLd  = 1d-4
   double precision, parameter :: DTOLun_rel = 1d-4
   double precision, parameter :: DTOLun = 1e-14

   integer,          parameter :: MAXNUMZERO = 10

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "update_particles_in_cells", ithndl )

   ierror = 0
   numremaining = 0

   dt = 0.0

!$OMP PARALLEL DO PRIVATE (i, k, k1, k2, L, ja, Lexit, d, un, t, tex, dt,          &
!$OMP                      ux0, uy0, uz0, cs, sn, xn, yn, zn, rl, dvar, dis, dn,   &
!$OMP                      ddn, isboundary),                                       &
!$OMP           REDUCTION ( +   : numremaining) ,                                  &
!$OMP           REDUCTION ( MAX : ierror ),                                        &
!$OMP           SCHEDULE  ( DYNAMIC, max(Nopart/100,1)           )
   do ipart=1,Nopart
      ! check if this particle needs to be updated
      if ( dtremaining(ipart).eq.0d0 .or. mpart(ipart).lt.1 ) cycle
      ! get cell (flownode) particle in in
      k = mpart(ipart)

      ! compute exit time <= dtremaining
      tex = dtremaining(ipart)

      Lexit = 0   ! exit edge (flowlink)

      ! compute velocity at current position
      ux0 = u0x(k) + alphafm(k)*(xpart(ipart)-xzwcell(k))
      uy0 = u0y(k) + alphafm(k)*(ypart(ipart)-yzwcell(k))
      if ( jsferic.ne.0 ) then
         uz0 = u0z(k) + alphafm(k)*(zpart(ipart)-zzwcell(k))
      end if

      ! loop over edges (netlinks) of cells
      do i=jcell2edge(k),jcell2edge(k+1)-1
         L = icell2edge(i)   ! edge

         k1 = edge2node(1,L)
         k2 = edge2node(2,L)

         if ( jsferic.eq.0 ) then
            cs = dnx(1,L)
            sn = dny(1,L)
            if ( edge2cell(2,L).eq.k ) then
               cs = -cs
               sn = -sn
            end if
         else
            if ( edge2cell(1,L).eq.k ) then
               ddn = (/ dnx(1,L), dny(1,L), dnz(1,L) /)
            else
               ddn = (/ dnx(2,L), dny(2,L), dnz(2,L) /)
            end if
         end if

         ! check for boundary edge
         isboundary = ( edge2cell(1,L).eq.0 .or. edge2cell(2,L).eq.0 )

         ! compute normal distance to edge
         if ( jsferic.eq.0 ) then
            if ( isboundary ) then ! boundary: add tolerance
               call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1)+cs*DTOLd,ynode(k1)+sn*DTOLd,xnode(k2)+cs*DTOLd,ynode(k2)+sn*DTOLd,ja,d,xn,yn,rl)
            else
               call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1),ynode(k1),xnode(k2),ynode(k2),ja,d,xn,yn,rl)
            end if
            dis = (xn-xpart(ipart))*cs + (yn-ypart(ipart))*sn

         else
            if ( isboundary ) then ! boundary: add tolerance
               call dlinedis3D(xpart(ipart),ypart(ipart),zpart(ipart),xnode(k1)+DTOLd*ddn(1),  &
                  ynode(k1)+DTOLd*ddn(2),  &
                  znode(k1)+DTOLd*ddn(3),  &
                  xnode(k2)+DTOLd*ddn(1),   &
                  ynode(k2)+DTOLd*ddn(2),   &
                  znode(k2)+DTOLd*ddn(3),   &
                  ja,d,xn,yn,zn,rl)
            else
               call dlinedis3D(xpart(ipart),ypart(ipart),zpart(ipart),xnode(k1),ynode(k1),znode(k1),xnode(k2),ynode(k2),znode(k2),ja,d,xn,yn,zn,rl)
            end if
            dis = (xn-xpart(ipart))*ddn(1) + (yn-ypart(ipart))*ddn(2) + (zn-zpart(ipart))*ddn(3)
         end if

         ! check inside or outside triangle
         if ( dis.lt.-DTOLd .and. .not.isboundary ) then
            ! outside triangle
            !!AM tex = 0d0
            Lexit = L
            exit
         else
            ! inside triangle
            ! compute normal velocity to edge (outward positive)
            if ( jsferic.eq.0 ) then
               un =  ux0*cs + uy0*sn
            else
               un =  ux0*ddn(1) + uy0*ddn(2) + uz0*ddn(3)
            end if

            if ( un.gt.max(DTOLun_rel*d,DTOLun) ) then   ! normal velocity does not change sign: sufficient to look at u0.n
               ! compute exit time for this edge: ln(1+ d/un alpha) / alpha
               dvar = alphafm(k)*dis/un
               if ( dvar.gt.-1d0) then
                  t = dis/un
                  if ( abs(dvar).ge.DTOL ) then
                     t = t * log(1d0+dvar)/dvar
                  end if
               else
                  t = huge(1d0)
               end if

               ! update exit time/edge (flowlink)
               if ( t.le.tex ) then

                  tex = t
                  Lexit = L
               end if
            end if

         end if
      end do

      if ( dtremaining(ipart).eq.0d0 ) then
         !continue
         cycle
      end if

      ! compute timestep in cell (flownode)
      dt = min(dtremaining(ipart), tex)

      ! update particle
      if ( abs(alphafm(k)).lt.DTOL ) then
         dvar = dt
      else
         dvar = (exp(alphafm(k)*dt)-1d0)/alphafm(k)
      end if

      xpart(ipart) = xpart(ipart) + dvar * ux0
      ypart(ipart) = ypart(ipart) + dvar * uy0

      if ( jsferic.ne.0 ) then
         zpart(ipart) = zpart(ipart) + dvar * uz0
      end if

      dtremaining(ipart) = dtremaining(ipart) - dt
      ! Lpart(ipart) = Lexit

      if ( dt.eq.0d0 ) then
         numzero(ipart) = numzero(ipart) + 1
      end if

      if ( numzero(ipart).gt.MAXNUMZERO ) then
         ! disable particle that is not moving
         mpart(ipart) = 0
         dtremaining(ipart) = 0d0

         ! proceed to neighboring cell (if applicable)
      else if ( Lexit.gt.0 ) then
         numremaining = numremaining + 1  ! number of remaining particles for next substep
         if ( edge2cell(1,Lexit).gt.0 .and. edge2cell(2,Lexit).gt.0 ) then   ! internal edge (netlink)
            mpart(ipart) = edge2cell(1,Lexit) + edge2cell(2,Lexit) - k


            if ( mpart(ipart).eq.0 ) then
               continue
            else
               if ( jsferic.eq.1 ) then
                  ! project node on triangle
                  k = mpart(ipart)
                  k1 = edge2node(1,Lexit)
                  k2 = edge2node(2,Lexit)
                  xn = 0.5d0*(xnode(k1)+xnode(k2))
                  yn = 0.5d0*(ynode(k1)+ynode(k2))
                  zn = 0.5d0*(znode(k1)+znode(k2))
                  dn = (xpart(ipart) - xn) * dnn(1,k) +  &
                     (ypart(ipart) - yn) * dnn(2,k) +  &
                     (zpart(ipart) - zn) * dnn(3,k)
                  xpart(ipart) = xpart(ipart) - dn * dnn(1,k)
                  ypart(ipart) = ypart(ipart) - dn * dnn(2,k)
                  zpart(ipart) = zpart(ipart) - dn * dnn(3,k)
               end if
            end if
         else  ! on boundary
            mpart(ipart) = 0
         end if
      else
         ! safety check
         if ( dtremaining(ipart).ne.0d0 ) then
            ierror = 1
         end if
      end if

   end do
!$OMP END PARALLEL DO

   if (ierror == 1) then
      call mess(LEVEL_ERROR, 'update_particles_in_cells: dtremaining <> 0', ' ', ' ')
   endif

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine update_particles_in_cells

   !> reconstruct velocity in cells
   subroutine reconst_vel_coeffs_fmx()

   use m_partrecons
   use m_partmesh
   use m_alloc
   use m_sferic
   use geometry_module, only: dbdistance, gaussj
   use timers

   implicit none

   integer,                          parameter   :: N = 4

   double precision, dimension(N,N)              :: Amat ! matrix
   double precision, dimension(N)                :: rhs

   double precision                              :: cs, sn

   integer                                       :: i, icell, j, jj, k, L, NN
   integer                                       :: k1, k2
   integer                                       :: i12, isign
   integer                                       :: ierror

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "reconst_vel_coeffs_fm", ithndl )

   ierror = 1

   ! allocate startpointers
   call realloc(jreconst, numcells+1, keepExisting=.false., fill=0)

   ! set startpointers
   jreconst(1) = 1
   do icell=1,numcells
      jreconst(icell+1) = jcell2edge(icell) + jcell2edge(icell+1)-jcell2edge(icell)
   end do

   ! allocate column indices and matrix entries
   NN = jreconst(numcells+1)-1
   call realloc(ireconst, NN, keepExisting=.false., fill=0)
   if ( jsferic.eq.0 ) then
      call realloc(Areconst, (/3, NN /), keepExisting=.false., fill=0d0)
   else
      call realloc(Areconst, (/4, NN /), keepExisting=.false., fill=0d0)
   end if

   ! dummy rhs
   rhs = 0d0

   ! loop over internal cells
   jj = 0
   do icell=1,numcells
      ! get flownode number (for s, bl)
      k = iabs(cell2nod(icell))

      ! fill system for (ux,uy) = (ux0, uy0) + alpha (x-x0, y-y0)

      ! loop over edges (netlinks)
      i = 0
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         i = i+1
         L = icell2edge(j) ! netlink

         k1 = edge2node(1,L)
         k2 = edge2node(2,L)

         if ( jsferic.eq.0 ) then
            cs = dnx(1,L)
            sn = dny(1,L)

            ! add to system
            Amat(i,1) = cs
            Amat(i,2) = sn
            Amat(i,3) = (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*cs + (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*sn
         else
            i12 = 1
            isign = 1
            if ( edge2cell(2,L).eq.icell ) then
               i12 = 2
               isign = -1d0
            end if

            Amat(i,1) = dnx(i12,L)*isign
            Amat(i,2) = dny(i12,L)*isign
            Amat(i,3) = dnz(i12,L)*isign
            Amat(i,4) = ( (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*dnx(i12,L) + &
               (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*dny(i12,L) + &
               (0.5d0*(znode(k1)+znode(k2))-zzwcell(icell))*dnz(i12,L) ) * isign
         end if

         jj = jj+1
         ireconst(jj) = L
      end do

      if ( jsferic.eq.0 ) then
         ! solve system
         call gaussj(Amat,3,N,rhs,1,1)

         do i=1,3
            L = icell2edge(jcell2edge(icell)+i-1)
            Areconst(1:3,jreconst(icell)+i-1) = Amat(1:3,i)
         end do
      else
         ! impose zero cell normal velocity
         Amat(4,1:3) = dnn(:,icell)
         Amat(4,4) = 0d0

         ! solve system
         call gaussj(Amat,4,N,rhs,1,1)

         do i=1,3
            L = icell2edge(jcell2edge(icell)+i-1)
            Areconst(1:4,jreconst(icell)+i-1) = Amat(1:4,i)
         end do

      end if
   end do

   ierror = 0
!  error handling
1234 continue

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine reconst_vel_coeffs_fmx


   !> reconstruct velocity in cells
   subroutine reconst_vel(q, h0, h1, ierror)
   use m_flowgeom, only: Ndx, Lnx, bl
   use m_flowparameters, only: epshs
   use m_partrecons
   use m_partmesh
   use m_sferic
   use geometry_module, only: dbdistance
   use timers

   implicit none

   double precision, dimension(Lnx), intent(in)  :: q    ! flowlink-based discharge (m3/s)
   double precision, dimension(Ndx), intent(in)  :: h0   ! flownode-based water level (m) at begin of interval
   double precision, dimension(Ndx), intent(in)  :: h1   ! flownode-based water level (m) at end of interval

   integer,                          intent(out) :: ierror

   integer,                          parameter   :: N = 4

   integer                                       :: icell, j, k, L

   double precision                              :: hk0, hk1, h, un

   double precision, parameter                   :: DTOL=1d-10

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "reconst_vel", ithndl )

   ierror = 1

   ! get fluxes at all edges, including internal
   call set_fluxes(Lnx, q, qe)

   ! initialize
   u0x = 0d0
   u0y = 0d0
   if ( jsferic.eq.1 ) then
      u0z = 0d0
   end if
   alphafm = 0d0

   do icell=1,numcells
      ! get flownode number (for s, bl)
      k = iabs(cell2nod(icell))

      ! get water depth
      hk0 = h0(k) !s0(k)-bl(k)
      hk1 = h1(k) !s1(k)-bl(k)
      if ( abs(hk1-hk0).gt.DTOL ) then
         if ( hk0.gt.epshs .and. hk1.gt.epshs ) then
            h = (hk1-hk0)/(log(hk1)-log(hk0))
         else if ( hk0.gt.epshs .or. hk1.gt.epshs ) then
            h = 0.5d0*(hk0+hk1)
         else
            h = 0d0
         end if
      else
         h = 0.5d0*(hk0+hk1)
      end if

      if ( h.le.epshs ) cycle

      if ( jsferic.eq.0 ) then
         do j=jreconst(icell),jreconst(icell+1)-1
            L = ireconst(j)
            un = qe(L)/(h*w(L))
            u0x(icell)     = u0x(icell)   + Areconst(1,j)*un
            u0y(icell)     = u0y(icell)   + Areconst(2,j)*un
            alphafm(icell) = alphafm(icell) + Areconst(3,j)*un
         end do
      else
         do j=jreconst(icell),jreconst(icell+1)-1
            L = ireconst(j)
            un = qe(L)/(h*w(L))
            u0x(icell)     = u0x(icell)   + Areconst(1,j)*un
            u0y(icell)     = u0y(icell)   + Areconst(2,j)*un
            u0z(icell)     = u0z(icell)   + Areconst(3,j)*un
            alphafm(icell) = alphafm(icell) + Areconst(4,j)*un
         end do
      end if
   end do

   ierror = 0
!  error handling
1234 continue

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine reconst_vel

   !> set pointer
   subroutine part_fill_networkdata(hyd, waqgeom,openbndsect_coll)
   use hydmod
   use io_ugrid
   use network_data, only: kn, xk, yk, zk, xzw, yzw, numk, numL, nump, netcell, lnn, lne
   use m_flowgeom, only: Ndxi, Ndx, Lnx, ba, bl, lne2ln
   use m_flow
   use m_transport
   use m_alloc
   use m_missing
   use m_partmesh
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance, sphertocart3D, normaloutchk, comp_masscenter
   use timers

   implicit none

   type(t_hyd)        ,intent(in) :: hyd           !< description of the hydrodynamics
   type(t_ug_meshgeom),intent(in) :: waqgeom
   type(t_openbndsect_coll),intent(in) :: openbndsect_coll       ! collection of openbndsects

   integer :: ierr, i, j, k, L, ln, N, Nmax
   real(8) :: bottomlevel

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "part_fill_networkdata", ithndl )

   if (hyd%crs%epsg_code == 4326) then ! epsg 4326 is assumed spherical, WGS84 system.
      jsferic = 1
   else
      jsferic = 0
   endif

   numk = waqgeom%numnode
   call realloc(xk, numk, keepExisting=.false., fill = dmiss)
   call realloc(yk, numk, keepExisting=.false., fill = dmiss)
   call realloc(zk, numk, keepExisting=.false., fill = 0.0d0)
   do k = 1, numk
      xk(k) = waqgeom%nodex(k)
      yk(k) = waqgeom%nodey(k)
      if (waqgeom%nodez(k) .ne. dmiss) then
         zk(k) = waqgeom%nodez(k)
      endif
   enddo

   numl = waqgeom%numedge
   call realloc(kn, [3, numl], keepExisting=.false., fill = 0)
   call realloc(lne2ln, numl, keepExisting=.false., fill = 0)
   call realloc(lnn, numl, keepExisting=.false., fill = 0)
   call realloc(lne, [2, numl], keepExisting=.false., fill = 0)
   Lnx = 0
   do L = 1, numl
      kn(1,L) = waqgeom%edge_nodes(1,L)
      kn(2,L) = waqgeom%edge_nodes(2,L)
      if (hyd%edge_type(L) == 1 .or. hyd%edge_type(L) == 2 .or. hyd%edge_type(L) == 21 .or. hyd%edge_type(L) == 22) then
         Lnx = Lnx + 1
         lne2ln(L) = Lnx
         lnn = 2
      else
         lne2ln(L) = 0
         lnn = 1
      endif
      lne(1, L) = max(waqgeom%edge_faces(1,L),0)
      lne(2, L) = max(waqgeom%edge_faces(2,L),0)
   enddo
   call realloc(q0, hyd%noq, keepExisting=.false., fill=0d0)
   call realloc(q1, hyd%noq, keepExisting=.false., fill=0d0)

   nump = waqgeom%numface
   Ndx = hyd%noseg! waqgeom%numface
   call realloc(xzw, nump, keepExisting=.false., fill = dmiss)
   call realloc(yzw, nump, keepExisting=.false., fill = dmiss)
   call realloc(ba, Ndx, keepExisting=.false., fill = 0.0d0)
   call realloc(bl, Ndx, keepExisting=.false., fill = 0.0d0)
   call realloc(h0, Ndx, keepExisting=.false., fill=0d0)
   call realloc(h1, Ndx, keepExisting=.false., fill=0d0)
   call realloc(vol0, Ndx, keepExisting=.false., fill=0d0)
   call realloc(vol1, Ndx, keepExisting=.false., fill=0d0)

   allocate(netcell(nump), stat = ierr)
   Nmax = waqgeom%maxnumfacenodes
   do k=1,nump
      N = 0
      bottomlevel = 0.0
      do i = 1, Nmax
         if (waqgeom%face_nodes(i, k) > 0) then
            N = i
            bottomlevel = bottomlevel + zk(waqgeom%face_nodes(i, k))
         else
            exit
         endif
      enddo
      netcell(k)%N = N
      allocate(netcell(k)%nod(N))
      netcell(k)%nod = waqgeom%face_nodes(1:N, k)
      allocate(netcell(k)%lin(N))
      netcell(k)%lin = waqgeom%face_edges(1:N, k)
!      allocate(netcell(k)%zlin(N))

      xzw(k) = waqgeom%facex(k)
      yzw(k) = waqgeom%facey(k)

      do i = 1,hyd%nolay
         j     = k + (i-1) * hyd%nosegl
         ba(j) = hyd%surf(k)
         bl(j) = bottomlevel / N
      end do
   end do

   if ( timon ) call timstop ( ithndl )

   end subroutine

   !> set pointer
   subroutine part_setmesh()
   use network_data, only: kn, xk, yk, xzw, yzw, numk, numL, nump, netcell, lnn, lne
   use m_flowgeom, only: lne2ln, ba
   use m_alloc
   use m_missing
   use m_partmesh
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance, sphertocart3D, normaloutchk, comp_masscenter
   use timers

   implicit none

   integer                        :: i, node1, node2, icell, j, k, L, N
   integer                        :: im1, ip1, L1, L2
   integer                        :: newnode, newedge
   integer                        :: isign, ja, k1, k2, k3, kL, kR
   integer                        :: jaswap

   integer                        :: numnontris
   integer                        :: numaddedges

   double precision, dimension(3) :: xv, yv
   double precision, dimension(3) :: t, t1, t2    ! edge tangential vector

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "part_setmesh", ithndl )

   numnodes = numk
   numedges = numL
   numcells = nump

   ! count number of non-triangles and additional (internal) edges
   numnontris=0
   numaddedges=0
   do k=1,nump
      N = netcell(k)%N
      if ( N.gt.3 ) then
         numnontris = numnontris+1
         numaddedges = numaddedges+N
      end if
   end do

   ! compute sizes
   numnodes = numk + numnontris                 ! add centers of non-triangles
   numedges = numL + numaddedges                ! add internal edges of non-triangles
   numcells = nump - numnontris + numaddedges   ! add internal triangles of non-triangles

   ! (re)allocate
   call realloc_partmesh()

   ! nodes
   if ( jsferic.eq.0 ) then
      do k=1,numk
         xnode(k) = xk(k)
         ynode(k) = yk(k)
      end do
   else
      do k=1,numk
         call sphertocart3D(xk(k),yk(k),xnode(k),ynode(k),znode(k))
      end do
   end if

   ! edges
   do L=1,numL
      edge2node(1,L) = kn(1,L)
      edge2node(2,L) = kn(2,L)
      if ( lne2ln(L).ge.0 ) then
         edge2link(L) = lne2ln(L)
      else
         ! not a flowlink number, see flow_geominit
         continue
      end if
   end do

   ! set jcell2edge startpointers and add new triangles
   icell=0
   jcell2edge(1) = 1
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 ) then
         icell = icell+1
         jcell2edge(icell+1) = jcell2edge(icell) + 3
!         nod2cell(k) = icell
         cell2nod(icell) = k
      else if ( N.gt.3 ) then
         do j=1,N
            icell = icell+1
            jcell2edge(icell+1) = jcell2edge(icell) + 3
!            if ( j.eq.1 ) nod2cell(k) = -icell
            cell2nod(icell) = -k
         end do
      else  ! should not happen
         continue
      end if
   end do

   ! allocate jcell2edge
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)

   ! copy netcell data into cell2edge and add new triangles (nodes, edges)
   icell = 0
   newnode = numk
   numorigedges = numL
   newedge = numorigedges
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 )then
         icell = icell+1   ! add existing triangle
         i = 0
         do j=jcell2edge(icell),jcell2edge(icell+1)-1
            i = i+1
            L = netcell(k)%lin(i)
            icell2edge(j) = L
         end do
         if ( jsferic.eq.0 ) then
            xzwcell(icell) = xzw(k)
            yzwcell(icell) = yzw(k)
         else
            call sphertocart3D(xzw(k),yzw(k),xzwcell(icell),yzwcell(icell),zzwcell(icell))
         end if
         areacell(icell) = ba(k)
      else
         ! add node
         newnode = newnode+1
         if ( jsferic.eq.0 ) then
            xnode(newnode) = xzw(k)
            ynode(newnode) = yzw(k)
         else
            call sphertocart3D(xzw(k),yzw(k),xnode(newnode),ynode(newnode),znode(newnode))
         end if

         xv(1) = xzw(k)
         yv(1) = yzw(k)

         do i=1,N ! add new edges and triangles
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
            L = netcell(k)%lin(i)
            call find_common_node(netcell(k)%lin(im1),L,node1)
            call find_common_node(netcell(k)%lin(ip1),L,node2)

            icell = icell+1
            ! add edges
            newedge = newedge+1
            edge2node(1,newedge) = newnode
            edge2node(2,newedge) = node1
            edge2link(newedge) = -1

            ! add edges to cell2edge
            j = jcell2edge(icell)
            icell2edge(j) = newedge
            icell2edge(j+1) = L
            icell2edge(j+2) = newedge+1
            if ( i.eq.N ) then
               icell2edge(j+2) = icell2edge(j+2) - N
            end if

            ! compute mass center
            xv(2) = xk(node1)
            yv(2) = yk(node1)
            xv(3) = xk(node2)
            yv(3) = yk(node2)
            call comp_masscenter(3, xv, yv, xzwcell(icell), yzwcell(icell), areacell(icell), ja, jsferic, jasfer3D, dmiss)

            if ( jsferic.eq.1 ) then
               xv(1) = xzwcell(icell)  ! reuse xv
               yv(1) = yzwcell(icell)   ! reuse yv
               call sphertocart3D(xv(1), yv(1),xzwcell(icell),yzwcell(icell),zzwcell(icell))
            end if
         end do
      end if
   end do

   ! set edge2cell
   edge2cell = 0
   do icell=1,numcells
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         L = icell2edge(j)
         if ( edge2cell(1,L).eq.0 ) then
            edge2cell(1,L) = icell
         else
            edge2cell(2,L) = icell
         end if
      end do
   end do

   !  fix orientation of edge2cell
   do L=1,numL ! exclude new edges
      k1 = edge2cell(1,L)
      if ( k1.eq.0 ) cycle

      k1 = iabs(cell2nod(k1))

      jaswap = 0

      if ( lnn(L).eq.1 ) then  !     boundaries: inward normal
         jaswap = 1
      else if ( k1.eq.lne(2,L) .and. lnn(L).gt.1 ) then
         jaswap = 1
      end if

      if ( jaswap.eq.1 ) then
         icell = edge2cell(1,L)
         edge2cell(1,L) = edge2cell(2,L)
         edge2cell(2,L) = icell
      end if
   end do

   if ( jsferic.ne.0 ) then
      ! compute cell normal vectors from first two edges
      do k=1,numcells
         L1 = icell2edge(jcell2edge(k))
         L2 = icell2edge(jcell2edge(k)+1)
         k1 = edge2node(1,L1)
         k2 = edge2node(2,L1)
         k3 = edge2node(1,L2)
         if ( k3.eq.k1 .or. k3.eq.k2 ) then
            k3 = edge2node(2,L2)
         end if
         t1 = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1)/)
         t2 = (/ xnode(k3)-xnode(k2), ynode(k3)-ynode(k2), znode(k3)-znode(k2)/)

         dnn(:,k) = (/ t1(2)*t2(3) - t1(3)*t2(2), t1(3)*t2(1) - t1(1)*t2(3), t1(1)*t2(2) - t1(2)*t2(1) /)
         dnn(:,k) = dnn(:,k) / sqrt( dnn(1,k)**2 + dnn(2,k)**2 + dnn(3,k)**2 )

         ! fix orientation
         if ( dnn(1,k)*xnode(k1) + dnn(2,k)*ynode(k1) + dnn(3,k)*znode(k1) .lt. 0d0 ) then
            dnn(:,k) = -dnn(:,k)
         end if
      end do
   end if

   ! nx, ny, w
   do L=1,numedges
      k1 = edge2node(1,L)
      k2 = edge2node(2,L)

      kL = edge2cell(1,L)
      kR = edge2cell(2,L)

      k = kL   ! outward positive

      isign = 1
      if ( k.le.0 ) then
         k = edge2cell(2,L)   ! inward positive
         isign = -1
      end if

      if ( k.eq.0 ) cycle  ! isolated edge

      ! compute normal vector (outward positive)
      if ( jsferic.eq.0 ) then
         call normaloutchk(xnode(k1),ynode(k1),xnode(k2),ynode(k2),xzwcell(k),yzwcell(k),dnx(1,L),dny(1,L),ja, jsferic, jasfer3D, dmiss, dxymis)

         if ( isign.eq.-1 ) then
            dnx(1,L) = -dnx(1,L)
            dny(1,L) = -dny(1,L)
         end if

         w(L) = dbdistance(xnode(k1),ynode(k1),xnode(k2),ynode(k2),jsferic, jasfer3D, dmiss)
      else
         ! compute outward normal with respect to the left cell
         t = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1) /)
         w(L) = sqrt( t(1)**2 + t(2)**2 + t(3)**2)

         t = t/w(L)

         if ( kL.gt.0 ) then
            ! left cell normal : nn X t
            dnx(1,L) = dnn(2,kL) * t(3) - dnn(3,kL) * t(2)
            dny(1,L) = dnn(3,kL) * t(1) - dnn(1,kL) * t(3)
            dnz(1,L) = dnn(1,kL) * t(2) - dnn(2,kL) * t(1)

            ! fix orientation
            if ( (xnode(k1)-xzwcell(kL))*dnx(1,L) + (ynode(k1)-yzwcell(kL))*dny(1,L) + (znode(k1)-zzwcell(kL))*dnz(1,L) .lt. 0d0 ) then
               dnx(1,L) = -dnx(1,L)
               dny(1,L) = -dny(1,L)
               dnz(1,L) = -dnz(1,L)
            end if
         end if

         if ( kR.gt.0 ) then
            ! left cell normal : nn X t
            dnx(2,L) = dnn(2,kR) * t(3) - dnn(3,kR) * t(2)
            dny(2,L) = dnn(3,kR) * t(1) - dnn(1,kR) * t(3)
            dnz(2,L) = dnn(1,kR) * t(2) - dnn(2,kR) * t(1)

            ! fix orientation
            if ( (xnode(k1)-xzwcell(kR))*dnx(2,L) + (ynode(k1)-yzwcell(kR))*dny(2,L) + (znode(k1)-zzwcell(kR))*dnz(2,L) .lt. 0d0 ) then
               dnx(2,L) = -dnx(2,L)
               dny(2,L) = -dny(2,L)
               dnz(2,L) = -dnz(2,L)
            end if
         end if
      end if
   end do

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine part_setmesh

   !> set all fluxes, including internal
   subroutine set_fluxes(Lnx,q,qe)
   use m_partmesh
   use m_partfluxes
   use timers

   implicit none

   integer,                               intent(in)  :: Lnx
   double precision, dimension(Lnx),      intent(in)  :: q

   double precision, dimension(numedges), intent(out) :: qe

   integer                                            :: j, L, Lf

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "set_fluxes", ithndl )

   do L=1,numedges
      qe(L) = 0d0
      do j=jflux2link(L),jflux2link(L+1)-1
         Lf = iflux2link(j)
         if ( Lf.gt.0 ) then
            qe(L) = qe(L) + Aflux2link(j)*q(Lf)
         end if
      end do
   end do

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine set_fluxes


   !> compute mapping from prescribed (at flowlinks) to all fluxes (at all edges, including "internal")
   subroutine comp_fluxcoeffs()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use MessageHandling
   use geometry_module, only: gaussj
   use timers

   implicit none

   integer                                  :: N         ! number of subtriangles

   integer,          dimension(MAXSUBCELLS) :: L1        ! "internal" edges
   integer,          dimension(MAXSUBCELLS) :: L2        ! original "non-internal" edges
   integer,          dimension(MAXSUBCELLS) :: icell     ! subcells
   integer,          dimension(MAXSUBCELLS) :: isign1    ! orientation of "internal: edges L1, all cw or
   integer,          dimension(MAXSUBCELLS) :: isign2    ! orientation of "non-internal: edges L2, outward normal
   double precision, dimension(MAXSUBCELLS) :: circ      ! weight of edge L1 in computation of circulation

   double precision, dimension(MAXSUBCELLS,MAXSUBCELLS) :: A, B

   double precision                         :: areasum, dlen

   integer                                  :: i, im1, ip1, j, j2, k, L, L3, Lf

   integer, external                        :: icommonval

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "comp_fluxcoeffs", ithndl )

   ! allocate
   call realloc_partfluxes()

   ! set startpointers
   jflux2link(1) = 1
   L = 1
   do while ( L.le.numedges )
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
         ! original flowlink
         jflux2link(L+1) = jflux2link(L) + 1

         ! proceed to next edge
         L = L+1
      else if ( L.gt.numorigedges ) then
         ! internal edge: find number of subtriangels
         N = 0
         do while ( icommonval(edge2cell(:,L+N), edge2cell(:,L+N+1)).ne.0 )
            N = N+1
            if ( L+N.ge.numedges ) exit
         end do

         if ( N.gt.0 ) then
            ! check connection first and last subtriangle
            if ( icommonval(edge2cell(:,L), edge2cell(:,L+N)).ne.0 ) then
               N = N+1

               do i=1,N
                  jflux2link(L+1) = jflux2link(L) + N
                  ! proceed to next edge
                  L = L+1
               end do
            else
               call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
            end if

         else  ! should not happen
            call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
         end if

         if ( L.ge.numedges ) exit
      else  ! other
         jflux2link(L+1) = jflux2link(L)
         ! proceed to next edge
         L = L+1
      end if
   end do

   ! reallocate
   call realloc(iflux2link, jflux2link(numedges+1)-1, fill=0, keepExisting=.false.)
   call realloc(Aflux2link, jflux2link(numedges+1)-1, fill=0d0, keepExisting=.false.)

   ! fill iflux2link (first in edge-numbers) and compute coefficients
   L = 1
   do while ( L.le.numedges )
      j=jflux2link(L)
      N = jflux2link(L+1)-j

      if ( N.eq.1 ) then
         ! original "non-internal" link
         iflux2link(j) = L
         Aflux2link(j) = 1d0
         ! proceed to next edge
         L = L+1
      else if ( N.gt.1 ) then
         ! "internal" link
         do i=1,N
            ip1 = i+1; if ( ip1.gt.N ) ip1 = ip1-N
            L1(i) = L+i-1
            L3 = L1(i)+ip1

            ! find subcell
            icell(i) = icommonval(edge2cell(:,L+i-1), edge2cell(:,L+ip1-1))

            ! find original edge
            j2 = jcell2edge(icell(i))+1   ! order of cell edges: "left" internal (L1), orginal (L2), "right internal (L3)
            L2(i) = icell2edge(j2)

            ! get orientation of "internal" edges (i-dir positive)
            ! note: will always be (/ -1, 1, 1, ... /), due to generation of edge2cells in part_setmesh
            isign1(i) = 1
            if ( edge2cell(1,L1(i)).eq.icell(i) ) isign1(i) = -1

            ! get orientation of original "non-internal" edges (outward normal)
            isign2(i) = 1
            if ( edge2cell(2,L2(i)).eq.icell(i) ) isign2(i) = -1
         end do

         ! compute summed area and circulation weights
         areasum = 0d0
         do i=1,N
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            areasum = areasum + areacell(icell(i))
            dlen = 0.25*(areacell(icell(im1))+areacell(icell(i)))/w(L1(i))
            circ(i) = dlen/w(L1(i))
         end do

         ! build system: A qe = B qlink
         A = 0d0
         B = 0d0
         ! continuity equations
         do i=1,N-1
            A(i,i)   = -isign1(i)   ! outward
            A(i,i+1) =  isign1(i+1) ! outward
            B(i,i)   = -isign2(i)   ! outward, rhs
            do k=1,N
               B(i,k) = B(i,k) + areacell(icell(i))*isign2(k)/areasum
            end do
         end do
         ! circulation
         do k=1,N
            A(N,k) = circ(k) * isign1(k)
         end do

         ! invert matrix: qe = inv(A) B qlink
         call gaussj(A,N,MAXSUBCELLS,B,N,MAXSUBCELLS)

         ! fill data
         do i=1,N
            iflux2link(j:j+N-1) = L2(1:N)
            Aflux2link(j:j+N-1) = B(i,1:N)

            ! proceed to next edge
            j = j+N
            L = L+1
         end do
      else
         ! closed boundary: proceed to next edge
         L = L+1
      end if
   end do

   ! convert to link number
   do j=1,jflux2link(numedges+1)-1
      L  = iflux2link(j)
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
         iflux2link(j) = Lf
      else  ! closed boundary
         iflux2link(j) = 0
      end if
   end do

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine comp_fluxcoeffs

   !> find common value of two-dimensional arrays i1 and i2
   !> it is assumed there is at most one common value
   integer function icommonval(i1, i2)
   implicit none

   integer, dimension(2), intent(in)  :: i1
   integer, dimension(2), intent(in)  :: i2

   icommonval = 0
   if ( i1(1).eq.i2(1) .or. i1(1).eq.i2(2) ) then
      icommonval = i1(1)
   else if ( i1(2).eq.i2(1) .or. i1(2).eq.i2(2) ) then
      icommonval = i1(2)
   end if

   return
   end function icommonval

   !> add particles
   subroutine add_particles(Nadd, xadd, yadd)
   use partmem, only: nopart, mpart
   use m_particles
   use m_alloc
   use m_sferic, only: jsferic
   use geometry_module, only: sphertocart3D
   use timers

   implicit none

   integer,                           intent(in)  :: Nadd       !< number of particles to be added
   double precision, dimension(Nadd), intent(in)  :: xadd       !< x-coordinates of particles to be added
   double precision, dimension(Nadd), intent(in)  :: yadd       !< y-coordinates of particles to be added

   call calculate_position_in_grid( nadd, xadd, yadd, nopart, xpart, ypart, zpart, mpart )

   return
   end subroutine add_particles

   !> calculate where the particles are within the grid
   subroutine calculate_position_in_grid(Nadd, xadd, yadd, np, xcrd, ycrd, zcrd, kcrd)
   use m_partmesh
   use m_alloc
   use m_sferic, only: jsferic
   use geometry_module, only: sphertocart3D
   use timers

   implicit none

   integer,                           intent(in)    :: Nadd       !< number of particles to be added
   double precision, dimension(Nadd), intent(in)    :: xadd       !< x-coordinates of particles to be added
   double precision, dimension(Nadd), intent(in)    :: yadd       !< y-coordinates of particles to be added
   integer,                           intent(inout) :: np         !< offset in coordinate arrays
   double precision, dimension(*),    intent(inout) :: xcrd       !< x-coordinates wrt grid
   double precision, dimension(*),    intent(inout) :: ycrd       !< y-coordinates wrt grid
   double precision, dimension(*),    intent(inout) :: zcrd       !< z-coordinates wrt grid
   integer,          dimension(*),    intent(inout) :: kcrd       !< cell number for the particle

   integer,          dimension(:),    allocatable   :: kadd       !< cell numbers

   integer                                        :: i, ipoint, Nsize
   integer                                        :: ierror
   integer                                        :: Nopartnew
   integer                                        :: Nreplace
   integer                                        :: Nloc

   double precision                               :: xn, yn, zn, dn
   integer                                        :: k, k1

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "calculate_position_in_grid", ithndl )

   ! get new particle cell number
   allocate(kadd(Nadd))
   kadd = 0
   call part_findcell(Nadd,xadd,yadd,kadd,ierror)

   !
   ! Convert to the definitive coordinates. If the location is not in the active grid,
   ! only store the grid cell number as zero, as the error should be handled elsewhere
   !
   do i=1,Nadd
      Np = Np + 1
      if ( kadd(i) > 0 ) then
         if ( jsferic.eq.0 ) then
            xcrd(Np) = xadd(i)
            ycrd(Np) = yadd(i)
            zcrd(Np) = 0.0d0
         else
            call sphertocart3D(xadd(i),yadd(i),xcrd(Np),ycrd(Np),zcrd(Np))
            if ( jsferic.eq.1 ) then
               ! project particle on triangle
               k = kadd(i)
               if ( k.gt.0 ) then
                  k1 = edge2node(1,icell2edge(jcell2edge(k)))
                  xn = xnode(k1)
                  yn = ynode(k1)
                  zn = znode(k1)
                  dn = (xcrd(Np) - xn) * dnn(1,k) +  &
                     (ycrd(Np) - yn) * dnn(2,k) +  &
                     (zcrd(Np) - zn) * dnn(3,k)
                  xcrd(Np) = xcrd(Np) - dn * dnn(1,k)
                  ycrd(Np) = ycrd(Np) - dn * dnn(2,k)
                  zcrd(Np) = zcrd(Np) - dn * dnn(3,k)
               end if
            end if

         end if
         kcrd(Np) = kadd(i)
      else
         xcrd(Np) = 0.0
         ycrd(Np) = 0.0
         zcrd(Np) = 0.0
         kcrd(Np) = 0
      end if
   end do

   !  deallocate
   if ( allocated(kadd) ) deallocate(kadd)

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine calculate_position_in_grid



   !> find in which cells particles are located
   subroutine part_findcellsingle(xxpart, yypart, kpart, ierror)
      use m_partmesh
      use MessageHandling
      use kdtree2Factory
      use m_alloc
      use m_sferic, only: jsferic, jasfer3D
      use m_missing, only: jins, dmiss
      use geometry_module, only: pinpok, dbdistance, pinpok3D, cart3Dtospher
      implicit none

      double precision,                   intent(in)    :: xxpart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
      double precision,                   intent(in)    :: yypart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
      integer,                            intent(out)   :: kpart    !< cell numbers

      integer                           , intent(out)   :: ierror   !< error (1) or not (0)

      double precision, dimension(:), allocatable, save :: xxzwcell, yyzwcell
      type(kdtree_instance), save                       :: kdtreecell
      logical, save                                     :: initkdtreecell = .false.

      double precision, dimension(3)                    :: xv, yv

      double precision                                  :: dmaxsize
      double precision                                  :: R2search
      double precision                                  :: xx, yy

      integer                                           :: i, ip1, j, k, knode, L, Lp1, N, NN, nres
      integer                                           :: inside

      ierror = 1

   !  build kdtree
      if (.not.initkdtreecell) then
         if ( jsferic.eq.0 ) then
            call build_kdtree(kdtreecell, numcells, xzwcell, yzwcell, ierror, 0, dmiss)
         else
            call realloc(xxzwcell, numcells, keepExisting=.false., fill=DMISS)
            call realloc(yyzwcell, numcells, keepExisting=.false., fill=DMISS)
            do i = 1, numcells
               call Cart3Dtospher(xzwcell(i), yzwcell(i), zzwcell(i),xxzwcell(i), yyzwcell(i) ,0d0)
            end do
            call build_kdtree(kdtreecell, numcells, xxzwcell, yyzwcell, ierror, 0, dmiss)
         end if
         if ( ierror.ne.0 ) then
            goto 1234
         end if
         initkdtreecell = .true.
      end if

      kpart = 0

   !     fill query vector
      call make_queryvector_kdtree(kdtreecell,xxpart,yypart, 0)

   !  reallocate if necessary
      NN = min(numcells,100) ! Was: 100)
      call realloc_results_kdtree(kdtreecell,NN)

   !  find nearest NN samples
      call kdtree2_n_nearest(kdtreecell%tree,kdtreecell%qv,NN,kdtreecell%results)

   !  check if samples are in cell
   !  loop over cells
      do nres=1,NN
         k = kdtreecell%results(nres)%idx
   !     check cell size
         N = jcell2edge(k+1)-jcell2edge(k)
         if ( N.ne.3 ) then
            call mess(LEVEL_ERROR, 'part_findcellsingle: non-triangle')
            goto 1234
         end if

   !     get cell polygon
         i=0
         do j = jcell2edge(k),jcell2edge(k+1)-1
            i = i+1
            L = icell2edge(j)
            ip1 = i+1; if ( ip1.gt.3 ) ip1=ip1-3
            Lp1 = icell2edge(j-i+ip1)
   !        find common node of L and Lp1
            if ( edge2node(1,L).eq.edge2node(1,Lp1) .or. edge2node(1,L).eq.edge2node(2,Lp1) ) then
               knode = edge2node(1,L)
            else if ( edge2node(2,L).eq.edge2node(1,Lp1) .or. edge2node(2,L).eq.edge2node(2,Lp1) ) then
               knode = edge2node(2,L)
            else  ! should not happen
               continue
            end if
            if ( jsferic.eq.0 ) then
               xv(i) = xnode(knode)
               yv(i) = ynode(knode)
            else
               call Cart3Dtospher(xnode(knode),ynode(knode),znode(knode),xv(i),yv(i),0d0)
            end if
         end do

         if ( jsferic.eq.0 ) then
            call pinpok(xxpart, yypart, 3, xv, yv, inside, jins, dmiss)
         else
            call pinpok3D(xxpart, yypart, 3, xv, yv, inside, dmiss, jins, jsferic, jasfer3D)
         end if

         if ( inside.eq.1 ) then
            kpart = k
            exit
         end if
      end do

      ierror = 0

   1234 continue

      return
   end subroutine part_findcellsingle


   !> find in which cells particles are located
   subroutine part_findcell(Nopart, xxpart, yypart, mpart, ierror)
   use timers

   implicit none

   integer,                             intent(in)  :: Nopart   !< number of particles
   double precision, dimension(Nopart), intent(in)  :: xxpart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
   double precision, dimension(Nopart), intent(in)  :: yypart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
   integer,          dimension(Nopart), intent(out) :: mpart    !< cell numbers

   integer                            , intent(out) :: ierror   !< error (1) or not (0)

   integer                                          :: i

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "part_findcell", ithndl )

   ierror = 0
   do i = 1,nopart
       call part_findcellsingle( xxpart(i), yypart(i), mpart(i), ierror )
       if ( ierror /= 0 ) then
           exit
       endif
   enddo

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine part_findcell

   !> (re)allocate
   subroutine realloc_particles(Nsize, LkeepExisting, ierror)
   use partmem, only: mpart
   use m_particles
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   integer, intent(in)  :: Nsize          !< array sizes
   logical, intent(in)  :: LkeepExisting  !< keep existing data (1) or not (0)
   integer, intent(out) :: ierror         !< error (1) or not

   ! local
   integer                                        :: npmargin
   ierror = 1
   npmargin = Nsize / 100 + 1 + Nsize
   !  reallocate
   call realloc(xpart, npmargin, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(ypart, npmargin, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(xpart_prevt, npmargin, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(ypart_prevt, npmargin, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(zpart, npmargin, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(zpart_prevt, npmargin, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(dtremaining, npmargin, keepExisting=LkeepExisting, fill=0d0)
   call reallocp(mpart, npmargin, keepExisting=LkeepExisting, fill=0)
   call realloc(mpart_prevt, npmargin, keepExisting=LkeepExisting, fill=0)

   call realloc(numzero, npmargin, keepExisting=LkeepExisting, fill=0)
   numzero = 0
   ierror = 0
1234 continue

   return
   end subroutine realloc_particles

   !> deallocate particle data
   subroutine dealloc_particles()
   use partmem, only: nopart, mpart
   use m_particles
   implicit none

   if ( allocated(xpart)       ) deallocate(xpart)
   if ( allocated(ypart)       ) deallocate(ypart)
   if ( allocated(zpart)       ) deallocate(zpart)
   if ( allocated(dtremaining) ) deallocate(dtremaining)
   if ( associated(mpart)      ) deallocate(mpart)

   if ( allocated(numzero)     ) deallocate(numzero)

   Nopart = 0

   if ( allocated(trpart)       ) deallocate(trpart)
   if ( allocated(xrpart)       ) deallocate(xrpart)
   if ( allocated(yrpart)       ) deallocate(yrpart)
   if ( allocated(zrpart)       ) deallocate(zrpart)
   Nrpart = 0

   return
   end subroutine dealloc_particles


   !> (re)allocate partmesh data
   subroutine realloc_partmesh()
   use m_partmesh
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   integer :: N

   call realloc(edge2node, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(edge2cell, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(xnode, numnodes, fill=0d0, keepExisting=.false.)
   call realloc(ynode, numnodes, fill=0d0, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(znode, numnodes, fill=0d0, keepExisting=.false.)
   end if

   call realloc(xzwcell, numcells, fill=DMISS, keepExisting=.false.)
   call realloc(yzwcell, numcells, fill=DMISS, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(zzwcell, numcells, fill=DMISS, keepExisting=.false.)
   end if
   call realloc(areacell, numcells, fill=DMISS, keepExisting=.false.)

   if ( jsferic.eq.0 ) then
      call realloc(dnx, (/1, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/1, numedges/), fill=DMISS, keepExisting=.false.)
   else
      call realloc(dnx, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnz, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnn, (/3, numcells/), fill=DMISS, keepExisting=.false.)
   end if
   call realloc(w, numedges, fill=DMISS, keepExisting=.false.)

   call realloc(edge2link, numedges, fill=0, keepExisting=.false.)
   call realloc(cell2nod, numcells, fill=0, keepExisting=.false.)

   call realloc(jcell2edge, numcells+1, fill=1, keepExisting=.false.)
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)

   return
   end subroutine realloc_partmesh

   !> deallocate particle mesh data
   subroutine dealloc_partmesh()
   use m_partmesh
   implicit none

   if ( allocated(edge2node ) ) deallocate(edge2node )
   if ( allocated(edge2cell ) ) deallocate(edge2cell )
   if ( allocated(xnode     ) ) deallocate(xnode     )
   if ( allocated(ynode     ) ) deallocate(ynode     )
   if ( allocated(znode     ) ) deallocate(znode     )

   if ( allocated(xzwcell   ) ) deallocate(xzwcell   )
   if ( allocated(yzwcell   ) ) deallocate(yzwcell   )
   if ( allocated(zzwcell   ) ) deallocate(zzwcell   )
   if ( allocated(areacell  ) ) deallocate(areacell  )

   if ( allocated(dnn       ) ) deallocate(dnn       )
   if ( allocated(dnx       ) ) deallocate(dnx       )
   if ( allocated(dny       ) ) deallocate(dny       )
   if ( allocated(dnz       ) ) deallocate(dnz       )
   if ( allocated(w         ) ) deallocate(w         )

   if ( allocated(edge2link ) ) deallocate(edge2link )
   !  if ( allocated(nod2cell  ) ) deallocate(nod2cell  )
   if ( allocated(cell2nod  ) ) deallocate(cell2nod  )

   if ( allocated(icell2edge) ) deallocate(icell2edge)
   if ( allocated(jcell2edge) ) deallocate(jcell2edge)


   return
   end subroutine dealloc_partmesh


   !> (re)allocate flux coefficients
   subroutine realloc_partfluxes()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use m_missing
   implicit none

   integer :: N

   call realloc(jflux2link, numedges+1, keepExisting=.false., fill=1)
   N = jflux2link(numedges+1)-1
   call realloc(iflux2link, N,  keepExisting=.false., fill=0)
   call realloc(Aflux2link, N,  keepExisting=.false., fill=0d0)

   return
   end subroutine realloc_partfluxes

   !> deallocate flux_coeffs
   subroutine dealloc_partfluxes()
   use m_partfluxes
   implicit none

   if ( allocated(iflux2link) ) deallocate(iflux2link)
   if ( allocated(jflux2link) ) deallocate(jflux2link)
   if ( allocated(Aflux2link) ) deallocate(Aflux2link)
   end subroutine dealloc_partfluxes

   !> (re)allocate flux coefficients et cetera
   subroutine realloc_partrecons()
   use m_partmesh
   use m_partrecons
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   call realloc(qe, numedges, keepExisting=.false., fill=DMISS)
   call realloc(qbnd, numedges, keepExisting=.false., fill=0)
   call realloc(cell_closed_edge, numedges, keepExisting=.false., fill=0)
   call realloc(u0x, numcells, keepExisting=.false., fill=DMISS)
   call realloc(u0y, numcells, keepExisting=.false., fill=DMISS)
   if ( jsferic.eq.1 ) then
      call realloc(u0z, numcells, keepExisting=.false., fill=DMISS)
   end if
   call realloc(alphafm, numcells, keepExisting=.false., fill=DMISS)

   call realloc(ireconst, numcells+1, keepExisting=.false., fill=0)
   return
   end subroutine realloc_partrecons

   !> deallocate flux_coeffs
   subroutine dealloc_partrecons()
   use m_partrecons
   implicit none

   if ( allocated(qe) ) deallocate(qe)

   if ( allocated(u0x) ) deallocate(u0x)
   if ( allocated(u0y) ) deallocate(u0y)
   if ( allocated(u0z) ) deallocate(u0z)
   if ( allocated(alphafm) ) deallocate(alphafm)

   if ( allocated(ireconst) ) deallocate(ireconst)
   if ( allocated(jreconst) ) deallocate(jreconst)
   if ( allocated(Areconst) ) deallocate(Areconst)

   return
   end subroutine dealloc_partrecons

!  initialise grid and hydro data
   subroutine ini_part_grid(hyd)
   use hydmod
   use partmem, only: ihdel, layt, nolayp, tcktot, nmaxp, mmaxp, mnmax2, mnmaxk
   use m_flow
   use m_transport, only: constituents, numconst
   use m_missing
   use m_alloc
   use MessageHandling
   use timers

   implicit none

   type(t_hyd)                    :: hyd           !< description of the hydrodynamics

   integer             :: minp, ilay, lun
   logical             :: lexist

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "ini_part_grid", ithndl )

   !  deallocate
   call dealloc_partmesh()
   call dealloc_partfluxes()
   call dealloc_partrecons()
   call dealloc_particles()
   call dealloc_auxfluxes()

   kmx = 0 ! for now 2D only

   !     fill network_data
   call part_fill_networkdata(hyd, hyd%waqgeom, hyd%openbndsect_coll)

   !     set pointers with mesh connectivity etc.
   call part_setmesh()

   !     set flux coeffecients
   call comp_fluxcoeffs()

   call realloc_partrecons()

   call reconst_vel_coeffs_fmx()

   call alloc_auxfluxes()

   ihdel = hyd%cnv_step_sec
   layt = hyd%nolay
   nolayp = hyd%nolay
   allocate ( tcktot(hyd%nolay))
   do ilay = 1, hyd%nolay
      tcktot(ilay) = hyd%waq_layers(ilay)
   enddo
   mnmax2 = hyd%nosegl
   mnmaxk = hyd%nosegl*hyd%nolay

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine ini_part_grid

   !> initialize particles
   subroutine ini_part(partfile, partrelfile, starttime_loc, timestep_loc, threeDtype_loc)
   use hydmod
   use partmem, only: xwaste, ywaste, zwaste, &
        radius, wparm, iwtime, ictime, amassd, amassc, nodye, nocont, ndprt, nopart, npmax, nosubs

   use m_particles
   use m_samples
   use m_flow
   use m_flowgeom, only: Ndx
   use m_transport, only: constituents !, numconst
   use m_flowtimes, only: tstart_user
   use m_missing
   use m_alloc
   use MessageHandling
   use timers

   implicit none

   character(len=255), intent(in) :: partfile      !< initial particle file
   character(len=255), intent(in) :: partrelfile   !< particle release file
   double precision,   intent(in) :: starttime_loc !< start time (>0) or not (0)
   double precision,   intent(in) :: timestep_loc  !< time step (>0) or every computational time step (0)
   integer,            intent(in) :: threeDtype_loc    !< depth averaged (0) or free surface (1)

   integer             :: minp, ierror
   logical             :: lexist

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "ini_part", ithndl )

!! AM   Nopart = 0
!! AM   Nrpart = 0
!! AM   irpart = 0
   !!NopartTot = 0 -- AM: accept the number of particles the user has specified

!  add particle tracer (when tracers are initialized)
   part_iconst = 1
   call realloc(constituents, (/ nosubs, Ndx /), keepExisting=.false., fill=0d0)

   timenext = 0d0
   timelast = DMISS

   !  start time
   if ( starttime_loc.gt.0d0 ) then
      starttime = starttime_loc
      timenext = starttime
   end if

   !  time step
   if ( timestep_loc.gt.0d0 ) then
      timestep = timestep_loc
   end if

   if ( len_trim(partfile).gt.0 ) then
      !     read initial samples from inputfile
      inquire(FILE = trim(partfile), exist = lexist)
      if ( lexist ) then
         call oldfil(minp, partfile)
         call savesam()
         call reasam(minp, 0)
         NopartTot = NopartTot + Ns
      else
         call mess(LEVEL_ERROR, 'the specified initial particle locations file could not be found: ', trim(partfile))
      end if
   end if
   if ( len_trim(partrelfile).gt.0 ) then
      !     read initial samples from inputfile
      inquire(FILE = trim(partrelfile), exist = lexist)
      if ( lexist ) then
         call read_particles_release_file(partrelfile)
      else
         call mess(LEVEL_ERROR, 'the specified particle release file could not be found: ', trim(partfile))
      end if
   end if

   call realloc_particles(npmax, .true., ierror)
   if ( Ns.gt.0 ) then
      call add_particles(Ns, xs, ys)
      call delsam(0)
   end if
   timepart = tstart_user

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine ini_part


   !> read particles release file
   subroutine read_particles_release_file(partrelfile)
   use m_particles
   use m_missing
   use m_alloc
   use MessageHandling
   implicit none

   character(len=255), intent(in) :: partrelfile   !< release particle file
   character(len=1000) :: line
   character(len=1)    :: char
   integer             :: lun, ios, ipart, linenr, ierror
   double precision    :: tr, xr, yr, zr

   call oldfil(lun, partrelfile)

   Nrpart = 0
   linenr = 0
   ios = 0

   do while ( ios==0 )
      read(lun, '(a1000)', iostat=ios) line
      linenr = linenr + 1
      if (ios==0) then
         read(line, '(a)', iostat=ios) char
         if (char.ne.'*'.and.char.ne.'#'.and.char.ne.'!') then
            read(line, *, iostat=ios) tr, xr, yr, zr
            if (ios==0) then
               Nrpart = Nrpart + 1
            endif
         endif
      endif
   end do

   if (Nrpart.gt.0) then
      ipart = 0
      linenr = 0
      ios = 0

      rewind (lun)
      call realloc(trpart, Nrpart)
      call realloc(xrpart, Nrpart)
      call realloc(yrpart, Nrpart)
      call realloc(zrpart, Nrpart)
      call realloc(mrpart, Nrpart)

      do while ( ios==0 )
         read(lun, '(a1000)', iostat=ios) line
         linenr = linenr + 1
         if (ios==0) then
            read(line, '(a)', iostat=ios) char
            if (char.ne.'*'.and.char.ne.'#'.and.char.ne.'!') then
               ipart = ipart + 1
               read(line, *, iostat=ios) trpart(ipart), xrpart(ipart), yrpart(ipart), zrpart(ipart)
               if (ios.ne.0 .or. trpart(ipart).eq.dmiss .or. xrpart(ipart).eq.dmiss .or.  yrpart(ipart).eq.dmiss .or. zrpart(ipart).eq.dmiss) then
                  call mess(LEVEL_ERROR, 'error reading particle release file '''//trim(partrelfile)//''' at line', linenr)
               endif
               if (ipart.gt.1) then
                  if (trpart(ipart).lt.trpart(ipart-1)) then
                     call mess(LEVEL_ERROR, 'timing in particle release file '''//trim(partrelfile)//''' is not incremental at line', linenr)
                  endif
               endif
            endif
         endif
      end do
      irpart = 1
      call part_findcell(Nrpart,xrpart,yrpart,mrpart,ierror)
      do ipart = 1, Nrpart
         if (mrpart(ipart).gt.0) then
            NopartTot = NopartTot + 1
         endif
      enddo
   endif
   close(lun)
   end subroutine read_particles_release_file


   !> add released particles
   subroutine add_particles_from_release_file(time0)
   use precision_part
   use partmem, only: nopart, mpart
   use partmem, only: iptime
   use m_particles
   use m_partmesh
   use m_sferic, only: jsferic
   use geometry_module, only: sphertocart3D
   use timers

   implicit none

   double precision, intent(in) :: time0       !< current   julian (s) of h0
   integer                      :: k, k1
   real(rp)                     :: xn, yn, zn, dn

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /

   if (irpart.eq.0 .or. irpart.gt.Nrpart) return

   if ( timon ) call timstrt( "add_particles_from_release_file", ithndl )


   do while (irpart.le.Nrpart)
!      if (trpart(irpart)*60.0d0.gt.time0) exit
      if (trpart(irpart).gt.time0) exit  ! assume that the timestamp in the file is in seconds
      if(mrpart(irpart).gt.0) then
         Nopart = Nopart + 1
         if ( jsferic.eq.0 ) then
            xpart(Nopart) = xrpart(irpart)
            ypart(Nopart) = yrpart(irpart)
         else
            call sphertocart3D(xrpart(irpart),yrpart(irpart),xpart(Nopart),ypart(Nopart),zpart(Nopart))
            ! project particle on triangle
            k = mrpart(irpart)
            if ( k.gt.0 ) then
               k1 = edge2node(1,icell2edge(jcell2edge(k)))
               xn = xnode(k1)
               yn = ynode(k1)
               zn = znode(k1)
               dn = (xpart(Nopart) - xn) * dnn(1,k) +  &
                  (ypart(Nopart) - yn) * dnn(2,k) +  &
                  (zpart(Nopart) - zn) * dnn(3,k)
               xpart(Nopart) = xpart(Nopart) - dn * dnn(1,k)
               ypart(Nopart) = ypart(Nopart) - dn * dnn(2,k)
               zpart(Nopart) = zpart(Nopart) - dn * dnn(3,k)
            end if
         end if
         mpart(Nopart) = mrpart(irpart)
      end if
      iptime(irpart) = 0.0
      irpart = irpart + 1
   end do

   if ( timon ) call timstop ( ithndl )

   end subroutine add_particles_from_release_file


   !> compute concentrations of particles (parts per unit volume) in flownodes
   subroutine comp_concentration(h, nconst, iconst, c)
   use partmem, only: mpart, wpart, oil, nfract, nopart
   use m_particles
   use m_partmesh
   use m_flowgeom, only : Ndx, ba, bl
   use m_flowparameters, only: epshs
   use m_flow, only: Ndkx
   use timers

   implicit none

   double precision, dimension(Ndx),        intent(in)  :: h      !< water depth
   integer,                                 intent(in)  :: nconst !< number of constituents
   integer,                                 intent(in)  :: iconst !< particle tracer constituent number
   double precision, dimension(Nconst,Ndx), intent(out) :: c      !< constituents

   integer :: i, k, ifract

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "comp_concentration", ithndl )

   do i=1,Ndx
      c(iconst,i) = 0d0
   end do

   !  count number of particles per cell
   do i=1,Nopart
      k = mpart(i)
      if ( k.eq.0 ) cycle

      k = iabs(cell2nod(k))

      c(iconst,k) = c(iconst,k) + wpart(iconst, i)
   end do

   !  compute concentration (parts per unit volume) , but for the oil module shoudl it be per m2 (ie divided by the depth of the segment), for sticky and surface oil
   do k=1,Ndx
      if ( h(k) .gt. epshs ) then
         c(iconst,k) = c(iconst,k) / (ba(k)*(h(k)-bl(k)))
         if (oil) then
            do ifract = 1 , nfract
                c(1 + 3 * (ifract - 1), k) =  c(1 + 3 * (ifract - 1), k) * (h(k)-bl(k))  ! surface floating oil per m2
                c(3 + 3 * (ifract - 1), k) =  c(3 + 3 * (ifract - 1), k) * (h(k)-bl(k))  ! surface floating oil per m2
            enddo
         endif

      else
         c(iconst,k) = 0d0
      end if
   end do

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine comp_concentration

   !> allocate auxiliary fluxes
   subroutine alloc_auxfluxes()
   use m_particles
   use m_flowgeom, only: Ndx, Lnx
   use m_alloc
   implicit none

   !!if ( timestep.gt.0d0 ) then - AM: why was this condition required?
   call realloc(hbegin, Ndx, fill=0d0, keepExisting=.false.)
   call realloc(qpart, Lnx, fill=0d0, keepExisting=.false.)
   !!end if

!   if ( threeDtype.eq.1 ) then
!      call realloc(qfreesurf, Lnx, fill=0d0, keepExisting=.false.)
!   end if

   return
   end subroutine alloc_auxfluxes

   !> deallocate auxiliary fluxes
   subroutine dealloc_auxfluxes()
   use m_particles
   implicit none

   if ( allocated(hbegin) ) deallocate(hbegin)
   if ( allocated(qpart)  ) deallocate(qpart)

   if ( allocated(qfreesurf) ) deallocate(qfreesurf)

   return
   end subroutine dealloc_auxfluxes

   subroutine part_readhydstep(hyd,itime,istat)
   use m_flow
   use m_flowgeom, only: ba, ndx, lnx, lne2ln
   use m_flowtimes, only: time1
   use hydmod
   use timers

   implicit none

   type(t_hyd)        ,intent(inout) :: hyd           !< description of the hydrodynamics
   integer            ,intent(inout) :: itime
   integer            ,intent(inout) :: istat

   !  local declarations
   integer iseg, iq, L

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "part_readhydstep", ithndl )

   if (istat==0) then
      do iseg = 1, hyd%noseg
         vol0(iseg) = vol1(iseg)
         h0(iseg) = h1(iseg)
      end do
      do iq = 1, hyd%noq
         q0(iq) = q1(iq)
      end do
   end if

   itime = int(time1)
   call read_hyd_step_fm(hyd,itime,istat)
   if (nint(time1) /= itime) then
      istat = 99
   end if
   if (istat == 0) then
      do iseg = 1, ndx !hyd%noseg
         vol1(iseg) = real(hyd%volume(iseg),8)
         h1(iseg) = vol1(iseg)/ba(iseg)
      end do
      do L = 1, lnx !hyd%noq
         iq = lne2ln(L)
         if (iq > 0) then
            if (hyd%edge_type(iq) == 2 .or. hyd%edge_type(iq) == 22) then
               q1(iq) = real(hyd%flow(iq),8)
            else
               q1(iq) = real(-hyd%flow(iq),8)
            endif
         endif
      end do
   end if

   if ( timon ) call timstop ( ithndl )

   end subroutine part_readhydstep

   !
   ! Wrapper for rdhydr from the "classic" reading routine
   !
   subroutine read_hyd_step_fm(hyd,itime,istat)
   use partmem, only: t_hyd, itstrtp, idelt, caltau, vol1, vol2, flow1, flow2m, vdiff1, tau1, salin1, temper1, flow2, rhowatc
   use m_flowgeom, only: lnx
   use fileinfo
   use rdhydr_mod, only: rdhydr

   type(t_hyd), intent(inout) :: hyd
   integer, intent(in)        :: itime
   integer, intent(out)       :: istat

   integer, dimension(:), allocatable, save   :: cellpnt
   integer, dimension(:,:), allocatable, save :: flowpnt
  ! real, dimension(:), allocatable, save      :: vol1, vol2, flow1, vdiff1, tau1, salin1, temper1, rhowatc
   logical, save                              :: first = .true.
   logical                                    :: update
   integer                                    :: i

   if ( first ) then
      first = .false.
      allocate( vol1(hyd%noseg),     &
                vol2(hyd%noseg),     &
                tau1(hyd%noseg),     &
                salin1(hyd%noseg),   &
                temper1(hyd%noseg),  &
                rhowatc(hyd%noseg),  &
                vdiff1(hyd%noseg),   &
                flow1(hyd%noq)   ,   &
                flow2(hyd%noq)   ,   &
                flow2m(hyd%noq)          )
      allocate( cellpnt(hyd%noseg),  &
                flowpnt(hyd%noq,2)       )

      do i = 1,hyd%noseg
         cellpnt(i) = i
      enddo
      do i = 1,hyd%noq
         flowpnt(i,1) = i
         flowpnt(i,2) = 0 ! Ignore the "to" part
      enddo

      hyd%cnv_step_sec = -999 ! To properly initialise the reading procedure
   endif
                                               ! Was: lnx - AM
   call rdhydr( hyd%nmax, hyd%mmax,  hyd%noseg, hyd%noq,          hyd%noseg,  &
                hyd%noq,  itime,     itstrtp,   hyd%cnv_step_sec, hyd%volume, &
                hyd%vdf,  hyd%surf,  hyd%flow,  vol1,             vol2,       &
                flow1,    flow2m,    vdiff1,    update,           cellpnt,          flowpnt,    &
                hyd%tau,  tau1,      caltau,    hyd%sal,          salin1,     &
                hyd%tem,  temper1,   nfiles,    lunit,            fname,      &
                flow2,    rhowatc                                  )

   istat = 0 ! Assume it goes well
   end subroutine read_hyd_step_fm


   subroutine part_sumfluxes(q1,Dts)
   use m_particles
   use m_partmesh
   use m_flowgeom, only: Lnx
   implicit none

   double precision, dimension(Lnx), intent(in) :: q1  !< fluxes
   double precision,                 intent(in) :: Dts !< time interval

   integer :: L

   do L=1,Lnx
      qpart(L) = qpart(L) + q1(L)*dts
   end do

   return
   end subroutine part_sumfluxes

   !> update particles or add to summed fluxes
   subroutine update_part(itime)
   use partmem
   use m_part_regular, only: npart
   use m_particles
   use m_flowtimes
   use m_flowgeom, only: Lnx, bl
   use m_flow
   use m_transport, only: numconst, constituents
   use m_missing
   use timers
   use fileinfo  , lun=> lunit    ! logical unit numbers for files

   use part09fm_mod
   use part14fm_mod

   implicit none

   integer, intent(in)         :: itime

   integer                     :: LL, Lb, Lt

   logical                     :: Lsurface

   real, dimension(:), allocatable :: xpartr
   real, dimension(:), allocatable :: ypartr
   real, dimension(:), allocatable :: zpartr
   integer                         :: nopartorg

   double precision, parameter :: huni=1d0

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "update_part", ithndl )

   Lsurface = .false. !( threeDtype.eq.1 )

   if ( Lsurface ) then
      do LL=1,Lnx
         !         call getLbotLtop(LL,Lb,Lt)
         qfreesurf(LL) = huni ! u1(Lt)*huni*wu(LL) -> q from top layer!
      end do
   end if

   if ( time0.ge.starttime ) then

!     add releases defined via an xyz file

      call add_particles_from_release_file(time0)


!     add dye release (from input file directly)

      if ( nodye .gt. 0 ) then
         !
         ! AM: this needs to be improved
         !
         nopartorg = nopart
         call part09fm ( lun(2)   , itime    , nodye    , nwaste   , mwaste   ,    &
                         xwaste   , ywaste   , iwtime   , amassd   , aconc    ,    &
                         npart    , mpart    , xpart    , ypart    , zpart    ,    &
                         wpart    , iptime   , nopart   , radius   , nrowswaste,   &
                         xpolwaste           , ypolwaste           ,               &
                         ndprt    , nosubs   ,                           &
                         layt     , tcktot   , zmodel   , laytop   , laybot   ,    &
                         nplay    , kwaste   , nolayp   ,                          &
                         modtyp   , zwaste   , track    , nmdyer   , substi   ,    &
                         rhopart)
      endif

!     add continuous release (from input file directly)

      if ( nocont .gt. 0 ) then
         call part14fm ( itime    , idelt    , nodye    , nocont   , ictime   ,    &
                         ictmax   , nwaste   , mwaste   , xwaste   , ywaste   ,    &
                         zwaste   , aconc    , rem      , npart    , ndprt    ,    &
                         mpart    , xpart    , ypart    , zpart    , wpart    ,    &
                         iptime   , nopart   , pblay    , radius   , nrowswaste,   &
                         xpolwaste           , ypolwaste           ,               &
                         ftime    , tmassu   , nosubs   ,                          &
                         ncheck   , t0buoy   , modtyp   , abuoy    , t0cf     ,    &
                         acf      , lun(2)   , layt     , tcktot   ,    &
                         zmodel   , laytop   , laybot   , nplay    , kwaste   ,    &  !< kwaste -> laywaste
                         nolayp   , linear   , track    ,                          &
                         nmconr   , spart    , rhopart  , noconsp  , const)
      endif

      if ( timestep.le.0d0 ) then   ! update particles every computational time step
         if ( .not.Lsurface ) then
            call update_particles(q1,h0,h1,dts)
         else
            call update_particles(qfreesurf,bl+huni,bl+huni,dts)
         end if
         timepart = time1
      else

         !        check if timestep has been started
         if ( timelast.eq.DMISS ) then
            !           start particle timestep
            timelast = time0
            timenext = time0+timestep
            hbegin = h0
            qpart = 0d0
         end if

         !        sum fluxes of this computational time step
         !         if ( .not.Lsurface ) then
         !            call part_sumfluxes(q1,Dts)
         !         else
         !            call part_sumfluxes(qfreesurf,Dts)
         !         end if

         if ( time1.ge.timenext ) then
            !           finish particle timestep
            qpart = qpart/(time1-timelast)
            if ( .not.Lsurface ) then
               call update_particles(q0, hbegin, h1, time1-timelast)
               !               call update_particles(qpart, hbegin, h1, time1-timelast)
            else
!               call update_particles(qfreesurf, bl+huni, bl+huni, time1-timelast)
               call update_particles(q0, bl+huni, bl+huni, time1-timelast)

            end if

            timepart = time1

            !           start new particle timestep
            timelast = time1
            timenext = time1 + timestep
            hbegin = h1
            qpart = 0d0
         end if
      end if
   end if

   if ( timon ) call timstop ( ithndl )

   return
   end subroutine update_part

   subroutine finalize_part()
   use m_particles
   implicit none

   call dealloc_partmesh()
   call dealloc_partfluxes()
   call dealloc_partrecons()
   call dealloc_particles()
   call dealloc_auxfluxes()

   return
   end subroutine finalize_part

   !> return common node of links L1 and L2
   subroutine find_common_node(L1, L2, node)

   use network_data
   use m_missing

   implicit none

   integer, intent(in)   :: L1, L2           !< links
   integer, intent(out)  :: node             !< common node

   integer, dimension(4) :: a                ! dummy array with nodes of L1 and L2
   ! integer, parameter    :: IMISS = -999999

   a(1:2)    = kn(1:2, L1)
   a(3:4)    = kn(1:2, L2)

   do
      node = IMISS

      if ( a(1).eq.a(3) .or. a(1).eq.a(4) ) node = a(1)
      if ( a(2).eq.a(3) .or. a(2).eq.a(4) ) node = a(2)

      if ( node.ne.IMISS ) exit

      write(6,*) 'find_common_node: no common node found'
      exit
   end do

   end subroutine find_common_node

   subroutine dlinedis2(x3,y3,x1,y1,x2,y2,ja,dis,xn,yn,rl)
   use m_sferic
   use geometry_module, only: getdx, getdy, dbdistance, sphertocart3d, cart3dtospher
   use m_missing, only: dmiss

   implicit none
   integer          :: ja
   double precision :: x1,y1,x2,y2,x3,y3,dis,xn,yn,zn, d2
   double precision :: r2,rl,x21,y21,z21,x31,y31,z31
   double precision :: xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xxn,yyn,zzn

   ja  = 0

   if (jsferic == 0 .or. jasfer3d == 0) then

      x21 = getdx(x1,y1,x2,y2,jsferic)
      y21 = getdy(x1,y1,x2,y2,jsferic)
      x31 = getdx(x1,y1,x3,y3,jsferic)
      y31 = getdy(x1,y1,x3,y3,jsferic)
      r2  = dbdistance(x2,y2,x1,y1,jsferic, jasfer3d, dmiss)
      r2  = r2*r2
      if (r2 .ne. 0) then
         rl  = (x31*x21 + y31*y21) / r2
         if (0d0 .le. rl .and. rl .le. 1d0) then
            ja = 1
         endif
         xn  = x1 + rl*(x2-x1)
         yn  = y1 + rl*(y2-y1)
         dis = dbdistance(x3,y3,xn,yn,jsferic, jasfer3d, dmiss)
      else
         dis = dbdistance(x3,y3,x1,y1,jsferic, jasfer3d, dmiss)
      endif

   else

      call sphertocart3d(x1,y1,xx1,yy1,zz1)
      call sphertocart3d(x2,y2,xx2,yy2,zz2)
      call sphertocart3d(x3,y3,xx3,yy3,zz3)

      x21 = xx2-xx1
      y21 = yy2-yy1
      z21 = zz2-zz1
      x31 = xx3-xx1
      y31 = yy3-yy1
      z31 = zz3-zz1

      r2  = x21*x21 + y21*y21 + z21*z21
      if (r2 .ne. 0d0) then
         rl = (x31*x21 + y31*y21 + z31*z21) / r2
         if (0d0 .le. rl .and. rl .le. 1d0) then
            ja = 1
         endif
         xxn  = xx1 + rl*x21
         yyn  = yy1 + rl*y21
         zzn  = zz1 + rl*z21
         x31 = xxn-xx3
         y31 = yyn-yy3
         z31 = zzn-zz3
         dis = sqrt(x31*x31 + y31*y31 + z31*z31)

         call cart3dtospher(xxn,yyn,zzn,xn,yn,maxval((/x1,x2,x3/)))
      else
         dis = dbdistance(x3,y3,x1,y1, jsferic, jasfer3d, dmiss)
      endif

   endif

   return
   end subroutine dlinedis2

   subroutine dlinedis3D(xx3,yy3,zz3,xx1,yy1,zz1,xx2,yy2,zz2,JA,DIS,xxn,yyn,zzn,rl)

   implicit none

   integer          :: ja
   double precision :: dis,xn,yn,zn, d2
   double precision :: r2,rl,x21,y21,z21,x31,y31,z31
   double precision :: xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xxn,yyn,zzn

   !  korste afstand tot lijnelement
   ja  = 0
   x21 = xx2-xx1
   y21 = yy2-yy1
   z21 = zz2-zz1
   x31 = xx3-xx1
   y31 = yy3-yy1
   z31 = zz3-zz1
   r2  = x21*x21 + y21*y21 + z21*z21
   if (r2 .ne. 0d0) then
      rl = (x31*x21 + y31*y21 + z31*z21) / r2
      if (0d0 .le. rl .and. rl .le. 1d0) then
         ja = 1
      endif
      xxn  = xx1 + rl*x21
      yyn  = yy1 + rl*y21
      zzn  = zz1 + rl*z21
      x31 = xxn-xx3
      y31 = yyn-yy3
      z31 = zzn-zz3
      dis = sqrt(x31*x31 + y31*y31 + z31*z31)
   else
      dis = 0d0
   endif

   return

   end subroutine dlinedis3d

   SUBROUTINE REASAM(MSAM, JADOORLADEN)
   USE M_MISSING
   USE M_SAMPLES
   use m_alloc
   use MessageHandling
   implicit none
   integer, intent(inout) :: msam        !< already opened file pointer to sample file
   integer, intent(in)    :: jadoorladen !< whether to append to global set (1) or start empty (0)
   integer :: ierr
   integer :: jflow
   integer :: jqn
   integer :: mcs
   integer :: ncs
   integer :: ndraw
   integer :: nkol
   integer :: nrow
   integer :: ns1
   integer :: nsm
   integer :: num
   integer :: K, K0
   double precision :: x, y, z
   double precision :: XX, YY, ZZ, ZZ2


   COMMON /PHAROSFLOW/  JFLOW
   COMMON /PHAROSLINE/  REC1
   COMMON /SAMPLESADM/  MCS,NCS,NS1
   COMMON /QNRGF/ JQN
   COMMON /DRAWTHIS/ ndraw(50)

   CHARACTER REC*132, TEX*10, REC1*132
   LOGICAL THISISANUMBER

   CALL SAVESAM()
   NSM = 0
   MXSAM = 0
   MYSAM = 0
   IPSTAT = IPSTAT_NOTOK
   nkol = 0
11 READ (MSAM,'()',END = 31)
   NSM = NSM + 1
   GOTO 11
31 NSMAX = 1.2d0*(NSM + JADOORLADEN*NS)
   IF (ALLOCATED (XS) ) DEALLOCATE (XS,YS,ZS)
   ALLOCATE (XS(NSMAX),YS(NSMAX),ZS(NSMAX),STAT=IERR)
   IF (IERR.NE.0) CALL MESS(LEVEL_ERROR, 'XS(NSMAX),YS(NSMAX),ZS(NSMAX)',IERR,NSMAX)
   if ( allocated(ipsam) ) deallocate(ipsam)
   allocate(ipsam(NSMAX),stat=ierr)
   IF (IERR.NE.0) CALL MESS(LEVEL_ERROR, 'ipsam(NSMAX)',ierr,NSMAX)

   REWIND(MSAM)

   WRITE(TEX,'(I10)') NSM
   IF (JADOORLADEN .EQ. 0) THEN
      XS=dmiss
      YS=dmiss
      ZS=dmiss
      K = 0
   ELSE
      CALL RESTORESAM()
      K   = NS
      NS1 = NS
   ENDIF
   K0 = K

   !    check of dit een PHAROS file is
   JFLOW = 1
14 READ (MSAM,'(A)',END = 30) REC1
   if (rec1(1:1) == '*') goto 14

   IF ( .NOT. (THISISANUMBER(REC1)) ) THEN
      READ (MSAM,'(A)',END = 30) REC
      IF ( THISISANUMBER(REC) ) THEN
         READ (REC,*,ERR = 16) NROW,NKOL
         GOTO 15
16       CONTINUE
         READ (MSAM,'(A)',END = 30) REC
         READ (REC,*,ERR = 15) NUM,X,Y,Z
         JFLOW = 3
      ENDIF
   ENDIF
15 CONTINUE

   REWIND (MSAM)


   KMOD = max(1, NSM/100)
10 CONTINUE
   READ (MSAM,'(A)',END = 30) REC
   IF (REC(1:1) .EQ. '*') GOTO 10
   IF ( .NOT. (THISISANUMBER(REC)) ) THEN
      !        we nemen aan dat er net een blokcode is gelezen
      !        en we lezen meteen de nrow ncol regel, maar checken die regel niet
      READ  (MSAM,'(A)',END = 30) REC
   ELSE

      IF (JFLOW .EQ. 3) THEN
         READ (REC,*,ERR = 40) NUM,XX,YY,ZZ
      ELSE IF (NKOL == 4) THEN
         READ (REC,*,ERR = 40) XX,YY, ZZ, ZZ2
         if (zz .ne. -999d0) then
            zz = sqrt(zz*zz + zz2*zz2)
         endif
      ELSE
         READ (REC,*,end = 40) XX,YY,ZZ
         READ (REC,*,ERR = 40) XX,YY,ZZ
      ENDIF

      IF (K  .LE. NSMAX-1 .AND. XX .NE. XYMIS .AND.   &
         ZZ .NE. dmiss .AND. ZZ .NE. 999.999d0 .and. &
         .not.(isnan(XX) .or. isnan(YY) .or. isnan(ZZ)) ) THEN
         K     = K + 1
         NS    = K
         XS(K) = XX
         YS(K) = YY
         ZS(K) = ZZ
      ENDIF
   ENDIF
   GOTO 10

40 CONTINUE
   WRITE(TEX,'(I10)') K
   CALL MESS(LEVEL_ERROR, 'ERROR READING SAMPLES FILE LINE NR ',TEX,REC)

30 CONTINUE
   IF (K .GT. NSMAX) THEN
      WRITE(TEX,'(I8)') NSMAX
      CALL MESS(LEVEL_WARN, 'ONLY',TEX,'SAMPLE POINTS CAN BE LOADED')
      WRITE(TEX,'(I8)') K
      CALL MESS(LEVEL_ERROR, 'YOU TRIED TO LOAD',TEX,'SAMPLE POINTS')
   ENDIF
   WRITE(TEX,'(I10)') NS
   IF (NS .GT. 1) THEN
      CALL TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      call get_samples_boundingbox()
      IPSTAT = IPSTAT_OK
   END IF
   call doclose(MSAM)
   RETURN
   END


   !>    delete samples
   !>      jaconfirm=0: do not prompt for confirmation,       keep arrays,        make copy
   !>                1:        prompt for confirmation,       keep arrays,        make copy
   !>               -1: do not prompt for confirmation, deallocate arrays, do not make copy
   SUBROUTINE DELSAM(JACONFIRM)      ! SPvdP: need promptless delsam in orthogonalisenet
   USE M_SAMPLES
   use m_polygon
   USE m_missing
   use geometry_module, only: dbpinpol


   implicit none

   integer, intent(in) :: JACONFIRM  !< prompt for confirmation (1) or not (0)

   integer :: i
   integer :: inhul
   integer :: ja
   integer :: k
   integer :: key
   integer :: nsol
   double precision :: rd
   double precision :: xi
   double precision :: yi

   if (jaconfirm == -1) then
      if (nsmax > 0) then
         nsmax = 0 ; ns = 0
         if ( allocated(xs)    ) deallocate (xs, ys, zs)
         if ( allocated(ipsam) ) deallocate(ipsam)
      endif
      return
   endif

   IF (Npl .LE. 2) THEN
      JA = 1
      IF (JA .EQ. 0) THEN
         KEY = 0
         RETURN
      ENDIF
      CALL SAVESAM()
      DO 5 I = 1,NS
         XS(I) = DMISS
         YS(I) = DMISS
         ZS(I) = DMISS
         ipsam(i) = 0
5     CONTINUE
      NS = 0
      RETURN
   ENDIF
   ! Else: check in polygon
   CALL SAVESAM()
   INHUL = -1
   DO 10 I = 1,NS
      RD = ZS(I)
      XI = XS(I)
      YI = YS(I)
      CALL DBPINPOL(xI, yI, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
      IF (INHUL .EQ. 1) ZS(I) = dmiss
10 CONTINUE

   K = 0
   NSOL = NS
   DO 20 I = 1,NS
      IF (ZS(I) .NE. dmiss) THEN
         K     = K + 1
         XS(K) = XS(I)
         YS(K) = YS(I)
         ZS(K) = ZS(I)
         ipsam(k) = ipsam(i)
      ENDIF
20 CONTINUE
   NS = K

   DO 30 I = NS+1,NSOL
      XS(I) = DMISS
      YS(I) = DMISS
      ZS(I) = DMISS
      ipsam(i) = 0
30 CONTINUE

   RETURN
   END SUBROUTINE DELSAM

   function thisisanumber(rec)
   use string_module, only: find_first_char
   !!--description-----------------------------------------------------------------
   ! NONE
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   implicit none
   !
   ! Global variables
   !
   logical                      :: thisisanumber
   character(len=*), intent(in) :: rec
   !
   !
   ! Local variables
   !
   integer                        :: ich
   integer                        :: l
   !
   !
   !! executable statements -------------------------------------------------------
   !
   !
   !     is waar als eerste character van rec een getal is.
   l = find_first_char(rec)
   if (l==0) then
      thisisanumber = .false.
   else
      ich = ichar(rec(l:l))
      if (ich==43 .or. ich==45 .or. ich==46 .or. ich>=48 .and. ich<=57) then
         thisisanumber = .true.
      else
         thisisanumber = .false.
      endif
   endif
   end function thisisanumber

   SUBROUTINE TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
   use sorting_algorithms, only: indexx
   implicit none
   integer :: ns
   double precision :: XS(NS), YS(NS), ZS(NS)   !< sample coordinates
   integer, dimension(NS), intent(out) :: IPSAM !< permutation array (increasing x-coordinate)
   integer,                intent(in)  :: MXSAM, MYSAM   !< structured sample data dimensions (>0) or unstructured (0)
   !      IF (NS .GT. 1) CALL RSORT3(XS,YS,ZS,NS)

   if ( NS.gt.1 ) then
      call indexx(Ns,xs,IPSAM)
   end if

   !!     remove double/missing samples (non-structured sample data only)
   !      if ( MXSAM*MYSAM.ne.NS ) then
   !         IF (NS .GT. 1) CALL RMDOUBLE(XS,YS,ZS,IPSAM,NS)
   !      end if

   RETURN
   END


   !>    determine sample bounding box
   subroutine get_samples_boundingbox()
   use m_samples
   use m_missing
   implicit none

   integer :: i

   xsammin =  huge(1d0)
   xsammax = -huge(1d0)
   ysammin =  huge(1d0)
   ysammax = -huge(1d0)

   do i=1,NS
      if ( xs(i).ne.DMISS .and. ys(i).ne.DMISS .and. zs(i).ne.DMISS ) then
         xsammin = min(xsammin,xs(i))
         xsammax = max(xsammax,xs(i))
         ysammin = min(ysammin,ys(i))
         ysammax = max(ysammax,ys(i))
      end if
   end do

   return
   end subroutine get_samples_boundingbox

   subroutine unc_init_trk()

   use netcdf
   use m_partfm_trk_netcdf
   use fileinfo, only: filebase
   use m_flowtimes
   use MessageHandling

   implicit none

   integer                      :: ierr

   trkncfilename = trim(filebase)//'_trk.nc'
   ierr = nf90_create(trkncfilename, 0, itrkfile)
   if (ierr /= nf90_noerr) then
      call mess(LEVEL_WARN, 'Could not create tracks file.')
   end if
   call unc_addglobalatts(itrkfile)
   ierr = nf90_def_dim(itrkfile, 'time', nf90_unlimited, id_trk_timedim)
   ierr = nf90_def_var(itrkfile, 'time', nf90_double, id_trk_timedim, id_trk_time)
   ierr = nf90_put_att(itrkfile, id_trk_time,  'units'        , trim(Tudunitstr))
   ierr = nf90_put_att(itrkfile, id_trk_time,  'standard_name', 'time')

   call unc_write_part_header(itrkfile,id_trk_timedim,id_trk_partdim,id_trk_parttime,id_trk_partx,id_trk_party,id_trk_partz)
   ierr = nf90_enddef(itrkfile)
   it_trk = 0
   end subroutine unc_init_trk

   subroutine unc_write_trk()

   use m_flowtimes
   use netcdf
   use m_partfm_trk_netcdf
   use MessageHandling
   use timers

   implicit none

   integer                      :: ierr, time_trk

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "unc_write_trk", ithndl )

   ! Increment output counters in m_flowtimes.
   time_trk = nint(time1)
   it_trk   = it_trk + 1
   ierr = nf90_put_var(itrkfile, id_trk_time, time_trk, (/ it_trk /))

   call unc_write_part(itrkfile,it_trk,id_trk_parttime,id_trk_partx,id_trk_party,id_trk_partz)

   if ( timon ) call timstop ( ithndl )

   return

   end subroutine unc_write_trk

   subroutine unc_close_trk()

   use netcdf
   use m_partfm_trk_netcdf
   use MessageHandling

   implicit none

   integer                      :: ierr

   ierr = nf90_close(itrkfile)
   end subroutine unc_close_trk

   !> Writes the (possibly aggregated) unstructured network and edge type to a netCDF file for DelWAQ.
   !! If file exists, it will be overwritten.
   subroutine unc_init_map(crs, meshgeom, nosegl, nolay)

   use partmem, only: nosubs, nfract, oil, substi
   use m_partfm_map_netcdf
   use fileinfo, only: filebase
   use netcdf
   use io_ugrid
   use m_flowtimes
   use m_flowgeom
   use m_alloc
   use m_missing

   implicit none

   type(t_crs),         intent(in)  :: crs      !< Optional crs containing metadata of unsupported coordinate reference systems
   type(t_ug_meshgeom), intent(in)  :: meshgeom !< The complete mesh geometry in a single struct.
   integer,             intent(in)  :: nosegl   !< Number of segments per layer
   integer,             intent(in)  :: nolay    !< Number of layers (meshgeom%numlayer turns out to be -1)

   character(len=10)                :: cell_method   !< cell_method for this variable (one of 'mean', 'point', see CF for details).
   character(len=50)                :: cell_measures !< cell_measures for this variable (e.g. 'area: mesh2d_ba', see CF for details).
   integer                          :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).
   logical                          :: success  !< Helper variable.
   character(len = 3), dimension(nosubs)  :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
   integer                          :: ifract, isub

   character(len=len(substi))       :: substnc
   integer                          :: i

   !   character(len=*)                 :: var_name      !< Variable name for in NetCDF variable, will be prefixed with mesh name.
   !   character(len=*)                 :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
   !   character(len=*)                 :: long_name     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
   !   character(len=*)                 :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).

   mapncfilename = trim(filebase)//'_map.nc'
   ierr = nf90_create(mapncfilename, 0, imapfile)
   if (ierr /= nf90_noerr) then
      call mess(LEVEL_WARN, 'Could not create map file.')
   end if
   call realloc(id_map_depth_averaged_particle_concentration, nosubs)

   ierr = UG_NOERR

   ! Add global attributes to NetCDF file.
   call unc_addglobalatts(imapfile)
   ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_map_timedim)
   ierr = nf90_def_var(imapfile, 'time', nf90_double, id_map_timedim, id_map_time)
   ierr = nf90_put_att(imapfile, id_map_time,  'units'        , trim(Tudunitstr))
   ierr = nf90_put_att(imapfile, id_map_time,  'standard_name', 'time')

   ! Note: a bit of trickery here ... we should probably define the concentrations as a 2D array
   if ( nolay > 1 ) then
      ierr = nf90_def_dim(imapfile, 'layer', nolay, id_map_layersdim)
   endif

   ! Write mesh geometry.
   ierr = ug_write_mesh_struct(imapfile, meshids, networkids, crs, meshgeom)
   if (ierr /= nf90_noerr) then
      call mess(LEVEL_ERROR, 'Could not write geometry to file map')
      return
   end if

   cell_method = 'mean' !< Default cell average.
   cell_measures = ''
   !   cell_measures = 'area: '//trim(mesh2dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer

   !   if (kmx == 1) then
   !               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_map_depth_averaged_particle_concentration, nf90_double, UNC_LOC_S, 'depth_averaged_particle_concentration', 'depth_averaged_particle_concentration', 'depth-averaged particle concentration', 'm-3', jabndnd=jabndnd_)

   !              function unc_def_var_map(ncid,id_tsp, id_var, itype, iloc,
   !               var_name = 'depth_averaged_particle_concentration'
   !               standard_name = 'depth_averaged_particle_concentration'
   !               long_name = 'depth_averaged_particle_concentration'
   !               unit = 'm-3'
   !               , is_timedep, dimids, cell_method, which_meshdim, jabndnd) result(ierr)


   !ierr = ug_def_var(imapfile, id_map_depth_averaged_particle_concentration, [meshids%dimids(mdim_face), id_map_timedim], nf90_double, UG_LOC_FACE, &
   !   trim(meshgeom%meshName), 'depth_averaged_particle_concentration', 'depth_averaged_particle_concentration', &
   !   'depth_averaged_particle_concentration', 'm-3', cell_method, cell_measures, crs, ifill=-999, dfill=dmiss)
   ! add concentrations of all available substances
   ! adapt units for surfcace and sticky oil
    unit = 'm-3'
    if (oil) then
        do ifract = 1 , nfract
            unit(1 + 3 * (ifract - 1)) = 'm-2'
            unit(3 + 3 * (ifract - 1)) = 'm-2'
        enddo
    endif

   do isub = 1, nosubs
      substnc = substi(isub)

      do i = 1,len_trim(substnc)
         if ( substnc(i:i) == ' ' ) then
            substnc(i:i) = '_'
         endif
      enddo

      if ( nolay == 1 ) then
         ierr = ug_def_var(imapfile, id_map_depth_averaged_particle_concentration(isub), [meshids%dimids(mdim_face), id_map_timedim], nf90_double, UG_LOC_FACE, &
                    trim(meshgeom%meshName), substnc, 'depth_averaged_particle_concentration', &
                    substi(isub), unit(isub), cell_method, cell_measures, crs, ifill=-999, dfill=dmiss)
      else
         ierr = ug_def_var(imapfile, id_map_depth_averaged_particle_concentration(isub), [meshids%dimids(mdim_face), id_map_layersdim, id_map_timedim], nf90_double, UG_LOC_FACE, &
                    trim(meshgeom%meshName), substnc, 'particle_concentration', &
                    substi(isub), unit(isub), cell_method, cell_measures, crs, ifill=-999, dfill=dmiss)
      endif
   end do

   if (ierr /= nf90_noerr) then
      call mess(LEVEL_ERROR, 'Could not create concentration variable in map file')
      return
   end if
   !   else
   !      continue
   !   endif

   ierr = nf90_enddef(imapfile)
   ierr = nf90_sync(imapfile)

   it_map = 0
   call realloc(work, ndx, keepExisting = .false., fill = dmiss)

   end subroutine unc_init_map

   subroutine unc_write_map()

   use partmem, only: nosubs, hyd
   use m_flowtimes
   use m_flowgeom
   use m_transport
   use m_flow, only: h1
   use m_particles, only: part_iconst
   use netcdf
   use m_partfm_map_netcdf
   use MessageHandling
   use timers

   implicit none

   integer                      :: ierr, time_map, k, isub
   double precision, pointer    :: pconc(:,:)

   integer(4) ithndl              ! handle to time this subroutine
   data ithndl / 0 /
   if ( timon ) call timstrt( "unc_write_map", ithndl )

   ! Increment output counters in m_flowtimes.
   time_map = nint(time1)
   it_map   = it_map + 1
   ierr = nf90_put_var(imapfile, id_map_time, time_map, (/ it_map /))
   if (ierr /= nf90_noerr) then
      call mess(LEVEL_ERROR, 'Could not write time to file map')
      if ( timon ) call timstop( ithndl )
      return
   end if

   ! AM: this is superfluous: we calculate the concentrations per substance, so we could
   !     simply fill the work array directly
   do isub = 1, nosubs
      call comp_concentration(h1,nosubs,isub,constituents)
      do k=1,Ndx
         work(k) = constituents(isub,k)
      end do

      ! AM: consider using a 2D pointer p, pointing to the work array as p(nosegl,nolay)
      !     then we can make the structure of the netCDF file nicer
      if ( hyd%nolay == 1 ) then
         ierr = nf90_put_var(imapfile, id_map_depth_averaged_particle_concentration(isub), work(1:Ndx), start = (/ 1, it_map /))
      else
         pconc(1:hyd%nosegl,1:hyd%nolay) => work
         ierr = nf90_put_var(imapfile, id_map_depth_averaged_particle_concentration(isub), pconc, start = (/ 1, 1, it_map /))
      endif

      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not write concentrations to file map')
         if ( timon ) call timstop( ithndl )
         return
      end if
   end do
   ierr = nf90_sync(imapfile)

   if ( timon ) call timstop ( ithndl )

   end subroutine unc_write_map

   subroutine unc_close_map()

   use netcdf
   use m_partfm_map_netcdf
   use MessageHandling

   implicit none

   integer                      :: ierr

   ierr = nf90_close(imapfile)
   end subroutine unc_close_map

   !> write particle tracks header to netcdf trk file
   subroutine unc_write_part_header(ifile,id_timedim,id_trk_partdim,id_trk_parttime,id_trk_partx,id_trk_party,id_trk_partz)
   use m_particles
   use netcdf
   use m_flow, only: kmx
   use m_sferic, only: jsferic
   use MessageHandling
   use m_missing
   implicit none

   integer, intent(in)    :: ifile  !< output file identifier
   integer, intent(in)    :: id_timedim
   integer, intent(inout) :: id_trk_partdim, id_trk_parttime, id_trk_partx, id_trk_party, id_trk_partz

   character(len=128)     :: mesg

   integer                :: ierr
   integer                :: jaInDefine

   ! Put dataset in define mode (possibly again) to add dimensions and variables.
   ierr = nf90_redef(ifile)
   if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
   if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
      call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
      !       call check_error(ierr)
      return
   end if

   ierr = nf90_def_dim(ifile, 'particles', NopartTot, id_trk_partdim)

   ierr = nf90_def_var(ifile, 'particles_time', nf90_double, id_timedim, id_trk_parttime)
   ierr = nf90_put_att(ifile, id_trk_parttime, 'long_name', 'particles time')

   ierr = nf90_def_var(ifile, 'particles_x_coordinate', nf90_double, (/ id_trk_partdim, id_timedim /), id_trk_partx)
   ierr = nf90_def_var(ifile, 'particles_y_coordinate', nf90_double, (/ id_trk_partdim, id_timedim /), id_trk_party)
   if (jsferic == 0) then
      ierr = nf90_put_att(ifile, id_trk_partx, 'units',         'm')
      ierr = nf90_put_att(ifile, id_trk_party, 'units',         'm')
      ierr = nf90_put_att(ifile, id_trk_partx, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ifile, id_trk_party, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ifile, id_trk_partx, 'long_name'    , 'x-coordinate')
      ierr = nf90_put_att(ifile, id_trk_party, 'long_name'    , 'y-coordinate')
   else
      ierr = nf90_put_att(ifile, id_trk_partx, 'units',         'degrees_east')
      ierr = nf90_put_att(ifile, id_trk_party, 'units',         'degrees_north')
      ierr = nf90_put_att(ifile, id_trk_partx, 'standard_name', 'longitude')
      ierr = nf90_put_att(ifile, id_trk_party, 'standard_name', 'latitude')
      ierr = nf90_put_att(ifile, id_trk_partx, 'long_name'    , 'longitude')
      ierr = nf90_put_att(ifile, id_trk_party, 'long_name'    , 'latitude')
   end if
   ierr = nf90_put_att(ifile, id_trk_partx, 'long_name', 'x-coordinate of particles')
   ierr = nf90_put_att(ifile, id_trk_party, 'long_name', 'y-coordinate of particles')
   ierr = nf90_put_att(ifile, id_trk_partx, '_FillValue', dmiss)
   ierr = nf90_put_att(ifile, id_trk_party, '_FillValue', dmiss)


   if ( kmx.gt.0 ) then
      ierr = nf90_def_var(ifile, 'particle_z_coordinate', nf90_double, (/ id_trk_partdim, id_timedim /), id_trk_partz)
      ierr = nf90_put_att(ifile, id_trk_partz, 'long_name', 'z-coordinate of particle')
   end if

   ! Leave the dataset in the same mode as we got it.
   if (jaInDefine == 1) then
      ierr = nf90_redef(ifile)
   end if

   return
   end subroutine unc_write_part_header

   !> write particles to netcdf file
   subroutine unc_write_part(ifile,itime,id_trk_parttime,id_trk_partx,id_trk_party,id_trk_partz)
   use partmem, only: nopart
   use m_particles
   use netcdf
   use m_sferic
   use m_sferic_part, only: ptref
   use m_flow, only: kmx
   use geometry_module, only: cart3Dtospher
   use m_missing
   use MessageHandling
   use m_alloc
   implicit none

   integer,                        intent(in)  :: ifile  !< output file identifier
   integer,                        intent(in)  :: itime
   integer,                        intent(in)  :: id_trk_parttime, id_trk_partx, id_trk_party, id_trk_partz

   double precision, dimension(:), allocatable :: xx, yy

   double precision                            :: dis2

   integer                                     :: i, i0, ii, iglb
   integer                                     :: ierr, ierror

   double precision,                 parameter :: dtol = 1d-8

   integer, save :: icount=0

   ierror = 1

   icount = icount+1

   if ( icount.ge.24 ) then
      continue
   end if

   !  allocate
   call realloc(xx, NopartTot, keepExisting=.false., fill = dmiss)
   call realloc(yy, NopartTot, keepExisting=.false., fill = dmiss)

   if ( jsferic.eq.1 ) then
      do ii=1,Nopart
         call Cart3Dtospher(xpart(ii),ypart(ii),zpart(ii),xx(ii),yy(ii),ptref)
      end do
   else
      do ii=1,NopartTot
         xx(ii) = xpart(ii)
         yy(ii) = ypart(ii)
      end do
   end if

   ierr = nf90_put_var(ifile, id_trk_parttime, timepart, (/ itime /))
   if ( ierr.ne.0 ) goto 1234
   ierr = nf90_put_var(ifile, id_trk_partx, xx, start=(/ 1,itime /), count=(/ NopartTot,1 /) )
   if ( ierr.ne.0 ) goto 1234
   ierr = nf90_put_var(ifile, id_trk_party, yy, start=(/ 1,itime /), count=(/ NopartTot,1 /) )
   if ( ierr.ne.0 ) goto 1234

   if ( kmx.gt.0 ) then
      !     particle vertical coordinate
   end if

   ierror = 0
1234 continue

   !  error handling
   if ( ierror.ne.0 ) then
      call mess(LEVEL_ERROR, 'particles output error')
   end if

   return
   end subroutine unc_write_part

   !> Puts global attributes in NetCDF data set.
   !! This includes: institution, Conventions, etc.
   subroutine unc_addglobalatts(ncid)
   use netcdf
   use MessageHandling
   use delwaq_version_module

   integer, intent(in) :: ncid

   character*8  :: cdate
   character*10 :: ctime
   character*5  :: czone
   integer :: ierr, jaInDefine
   ierr = nf90_noerr
   jaInDefine = 0

   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
   if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
      write (msgbuf, '(a,i0,a,i0,a,a)') 'Could not put global attributes in NetCDF #', ncid, '. Error code ', ierr, ': ', nf90_strerror(ierr)
      call err_flush()
      return
   end if

   ierr = nf90_put_att(ncid, nf90_global,  'institution', trim(company))
   ierr = nf90_put_att(ncid, nf90_global,  'references', trim(company_url))
   ierr = nf90_put_att(ncid, nf90_global,  'source', version_full)

   call date_and_time(cdate, ctime, czone)
   ierr = nf90_put_att(ncid, nf90_global,  'history', &
      'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// ', PART')
   ierr = nf90_put_att(ncid, nf90_global,  'date_created',  cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))
   ierr = nf90_put_att(ncid, nf90_global,  'date_modified', cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))

   ierr = nf90_put_att(ncid, nf90_global,  'Conventions', 'CF-1.5 Deltares-0.1')

   ! Leave the dataset in the same mode as we got it.
   if (jaInDefine == 0) then
      ierr = nf90_enddef(ncid)
   end if
   end subroutine unc_addglobalatts

   !> Sets the UDUnit timestring based on current model time settings.
   !! Module variable Tudunitstr can the be used in various output routines.
   subroutine setTUDUnitString()
   use m_flowtimes

   implicit none

   integer          :: Tzonehrs
   character(len=1) :: Tzonesgn

   Tzonehrs = int(TZone)
   if (Tzone<0) then
      Tzonesgn = '-'
   else
      Tzonesgn = '+'
   end if
   write(Tudunitstr,'(a,i2.2,a)') 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00 '//Tzonesgn, abs(Tzonehrs),':00'

   end subroutine setTUDUnitString

   subroutine part06fm ( lun    , nodye  , nocont , xwaste ,      &
                         ywaste , zwaste , nwaste , mwaste )

!       Deltares Software Centre

!>\file
!>            Determines the grid cells and relative coordinates of waste locations
!>
!>            The wastelocations are given by the user in global x,y coordinates.\n
!>            This routine determines the n,m grid indices and the local x,y coordinates.\n
!>            The local x,y coordinates are 0< .. <1 and are store in the old x,y locations
!
!     System administration : Antoon Koster
!
!     Created               : February 1990, by Leo Postma
!
!     Modified              : August   2011, by Leo Postma, only warning if outside grid
!                           : June     2021, by Arjen Markus, mimick the behaviour suitable for FM
!
!     logical unit numbers  : lun    - output log file
!
!     Note                  : we need to be careful about the names nwaste and mwaste, nwaste
!                             is actually a dummy in the case of unstructured grids

      use precision_part
      use timers

      implicit none

!     Arguments

!     kind            function         name                   description

      integer  ( ip), intent(in   ) :: lun                  !< unit number output log file
      integer  ( ip), intent(in   ) :: nodye                !< number of dye releases
      integer  ( ip), intent(in   ) :: nocont               !< number of continuous release
      real     ( rp), intent(inout) :: xwaste(nodye+nocont) !< x of wasteload location
      real     ( rp), intent(inout) :: ywaste(nodye+nocont) !< y of wasteload location
      real     ( rp), intent(inout) :: zwaste(nodye+nocont) !< z of wasteload location
      integer  ( ip), intent(  out) :: nwaste(nodye+nocont) !< first grid index wasteloads
      integer  ( ip), intent(  out) :: mwaste(nodye+nocont) !< second grid index wasteloads

      real     ( dp)                :: xwasted(nodye+nocont) !< x of wasteload location
      real     ( dp)                :: ywasted(nodye+nocont) !< y of wasteload location
      real     ( dp)                :: xcrd(nodye+nocont)    !< x coordinate in grid
      real     ( dp)                :: ycrd(nodye+nocont)    !< y coordinate in grid
      real     ( dp)                :: zcrd(nodye+nocont)    !< z coordinate in grid
      integer  ( ip)                :: kwaste(nodye+nocont) !< grid index wasteloads

!     Locals

      integer  ( ip) ::  id      ! loop counter wasteloads
      integer  ( ip) ::  ierror  ! error variable of part07
      real     ( rp) ::  xnloc   ! input x coordinate for the grid search
      real     ( rp) ::  ynloc   ! input y coordinate for the grid search
      real     ( rp) ::  xmloc   ! output x coordinate for the grid search
      real     ( rp) ::  ymloc   ! output y coordinate for the grid search
      integer  ( ip) ::  nmloc   ! output n index of the wasteload point
      integer  ( ip) ::  mmloc   ! output m index of the wasteload point
      integer  ( ip) ::  noerr   ! local error accumulator
      integer  ( ip) ::  np      ! index for storing particle information

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part06", ithndl )

      noerr   =  0
      kwaste  = -1
      np      =  0
      xwasted = xwaste
      ywasted = ywaste

      call calculate_position_in_grid( nodye+nocont, xwasted, ywasted, np, xcrd, ycrd, zcrd, kwaste )

      ! Convert to single-precision

      do id = 1,nodye+nocont
         if ( kwaste(id) <= 0 ) then
            if ( id > nodye ) then
               write ( lun, 1010 ) id-nodye, xwaste(id), ywaste(id)
            else
               write ( lun, 1000 ) id      , xwaste(id), ywaste(id)
            endif
            noerr = noerr + 1
         endif

         xwaste(id) = xcrd(id)
         ywaste(id) = ycrd(id)
         zwaste(id) = zcrd(id)
         mwaste(id) = kwaste(id)
         nwaste(id) = 1 ! Dummy
      enddo

      if ( noerr .ne. 0 ) then
         write ( lun, 1020 ) noerr
         stop 1
      endif

!     end of routine

      if ( timon ) call timstop ( ithndl )
      return

 1000 format( '  Error 4901. Dye release', i0,' at (x,y): (',   &
                 f9.2,',',f9.2,') not on active grid cell.' )
 1010 format( '  Error 4902. Continuous release', i0,' at (x,y): (',   &
                 f9.2,',',f9.2,') not on active grid cell.' )
 1020 format( '  Total number of errors regarding waste loads: ', i0 )

      end subroutine part06fm

