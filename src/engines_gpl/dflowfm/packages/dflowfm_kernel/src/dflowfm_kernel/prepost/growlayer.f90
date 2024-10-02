!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

!
!
module m_grow_layer
   use m_halt3
   use m_comp_vel
   use m_comp_tmax_self
   use m_comp_tmax_other

   implicit none
contains
!> grow a gridlayer
subroutine growlayer(mc, nc, mmax, nmax, idir, j, edgevel, dt, xc, yc, ifront, istop)

   use m_missing, only: dmiss, dxymis
   use unstruc_display, only: ncolrg
   use m_sferic, only: jasfer3d, jsferic
   use m_spline2curvi, only: jaCheckFrontCollision, dtolLR
   use geometry_module, only: dbdistance, dcosphi
   use m_drawthis
   use m_qnerror
   use m_get_lr
   use m_set_col
   use m_movabs
   use m_lnabs

   integer, intent(in) :: mc !< number of grid points
   integer, intent(in) :: nc !< number of grid layers
   integer, intent(in) :: mmax !< array size
   integer, intent(in) :: nmax !< array size
   integer, intent(in) :: idir !< grow direction, -1 or 1  (not used)
   integer, intent(in) :: j !< grid layer
   double precision, dimension(mc - 1), intent(in) :: edgevel !< grid layer edge-height
   double precision, intent(inout) :: dt !< time step
   double precision, dimension(mmax, nmax), intent(inout) :: xc, yc !< coordinates of grid points
   integer, dimension(mc), intent(inout) :: ifront !< active nodes (1) or not (0)
   integer, intent(out) :: istop !< stop (1) or not (0)

   integer, dimension(mc) :: ifrontold, ifrontnew

   double precision, dimension(mc) :: xc1, yc1 !  active grid layer coordinates
   double precision, dimension(2, mc) :: vel !  growth velocity vector at grid layer, per node
!   double precision, dimension(mc-1)                     :: edgevel  !  edge normal-velocity

   double precision, dimension(mc) :: dtmax !  maximum allowable grid layer growth time, per node
   double precision, dimension(mc) :: dtmax_self !  maximum allowable grid layer growth time, per node
   double precision, allocatable, dimension(:) :: dtmax2 !  maximum allowable grid layer growth time, per node

   double precision :: dt_other !  maximum alloweble grid layer growth time; collision only

   integer :: nf !< front dimension
   integer :: numf !< array size
   double precision, allocatable, dimension(:) :: xf, yf !< front point coordinates
   double precision, allocatable, dimension(:, :) :: velf !< front growth velocity vectors
   integer, allocatable, dimension(:, :) :: idxf !< (i,j)-indices of front points

   double precision :: dt_loc, dt_tot
   double precision :: dtolLR_bak, dhmax
   integer :: i, ii, iL, iR, ja3
   double precision, parameter :: dtol = 1d-8
   double precision, parameter :: dclearance = 5d2
   logical :: Lalllines = .false. ! all gridlines (.true.) or not (.false.)

   integer, save :: numgrow = 0

!  store settings
   dtolLR_bak = dtolLR

   if (abs(idir) /= 1) then
      call qnerror('growlayer: abs(idir).ne.1', ' ', ' ')
   end if

   if (j == 60) then
      continue
   end if

   if (j - 1 == 1) numgrow = 0

!   dheight = 1d0

   ifrontold = ifront

   dt_tot = 0d0
   xc1 = xc(:, j - idir)
   yc1 = yc(:, j - idir)

!  compute maximum mesh width and get dtolLR in the proper dimension
   dhmax = 0d0
   do i = 1, mc - 1
      if (xc(i, 1) == DMISS .or. xc(i + 1, 1) == DMISS) cycle
      dhmax = max(dhmax, dbdistance(xc(i, 1), yc(i, 1), xc(i + 1, 1), yc(i + 1, 1), jsferic, jasfer3D, dmiss))
   end do
   dtolLR = dtolLR * dhmax

!  allocate
   numf = mc * (1 + nc)
   allocate (dtmax2(numf), xf(numf), yf(numf), velf(2, numf), idxf(2, numf))

!  compute growth velocity vectors
!   edgevel = 1d0
   call comp_vel(mc, xc1, yc1, edgevel, vel)
!  disable points that have no valid velocity vector
   do i = 1, mc
      if (vel(1, i) == DMISS) then
         xc(i, j - idir) = DMISS
         xc1(i) = DMISS
      end if
   end do

!  find front points
   call findfront(mc, nc, mmax, nmax, xc, yc, numf, xf, yf, idxf, nf)

!  copy growth velocity vectors to front
   call copy_vel_to_front(mc, j - 1, vel, ifrontold, nf, numf, xf, yf, velf, idxf)

   do while (dt_tot < dt)
      numgrow = numgrow + 1
      ifrontnew = ifrontold

!     plot
!      call teksam(xs,ys,zs,ns,ndraw(32))
!      call tekgrid(i)
!      call plotsplines()
!      call teklan(ncolln)
      call setcol(ncolrg)
      call movabs(xf(1), yf(1))
      do i = 2, nf
         if (xf(i) /= DMISS) then
            call lnabs(xf(i), yf(i))
         else
            if (i < nf) call movabs(xf(i + 1), yf(i + 1))
         end if
      end do
!      call qnerror(' ', ' ', ' ')

!      if ( idir.lt.0 ) dnormal = -dnormal

!     remove stationary points
      where (ifrontold == 0) xc1 = DMISS

!     compute maximum allowable growth time; node merger in grid layer
      istop = 0
      dt_other = 1d99
      dtmax_self = 1d99
      call comp_tmax_self(mc, xc1, yc1, vel, dtmax_self)
      dt_loc = min(dt - dt_tot, minval(dtmax_self))

      if (jaCheckFrontCollision == 1) then
         !     collision with front
         dtmax = dt_loc + 1d0 ! a bit larger, for safety
         dtmax2 = 1d99 ! not used
         call comp_tmax_other(mc, j, xc1, yc1, vel, nf, xf, yf, velf, idxf, dtmax)

         dt_other = minval(dtmax)
      else
         dt_other = 1d99
      end if

!     update new frontmask
      if (dt_other < dt_loc) then
         do i = 1, mc
            if (dtmax(i) - dt_other <= dtol .and. (dt_loc - dtmax(i)) > dtol) ifrontnew(i) = 0
         end do
      end if

!     remove isolated points from frontmask
      if (ifrontnew(1) == 1 .and. ifrontnew(2) == 0) ifrontnew(1) = 0
      if (ifrontnew(mc) == 1 .and. ifrontnew(mc - 1) == 0) ifrontnew(mc) = 0
      where (ifrontnew(2:mc - 1) == 1 .and. ifrontnew(1:mc - 2) == 0 .and. ifrontnew(3:mc) == 0) ifrontnew(2:mc - 1) = 0

      write (6, *) numgrow, j, dt_loc, dt_other

      if (dt_other < dt_loc) then
!         istop = 1
         write (6, '(A)', advance="no") "--- stop ---"
         do i = 1, mc
            if (ifrontnew(i) == 0 .and. ifrontold(i) == 1) then
               write (6, '(I5, ":")', advance="no") i
            end if
         end do
         write (6, *)
!         call qnerror(' ', ' ', ' ')
      end if

!     determine grid layer growth time
      if (Lalllines) then
         dt_loc = min(dt_loc, dt_other)
      else
!        only consider node merger, colliding gridlines/nodes will be disabled
         ifrontold = ifrontnew
         dt_loc = min(dt_loc, dt_other)
      end if

!     update new grid layer coordinates
      do i = 1, mc
         if (ifrontold(i) == 1 .and. vel(1, i) /= DMISS) then
            if (vel(1, i) == 0d0 .and. vel(2, i) == 0d0) then
               continue
            end if
            xc1(i) = xc1(i) + dt_loc * vel(1, i)
            yc1(i) = yc1(i) + dt_loc * vel(2, i)
         else
            xc1(i) = DMISS
            yc1(i) = DMISS
         end if

!         if ( i.lt.mc ) then
!            if ( dtmax_self(i).le.dt_loc .and. dtmax_self(i+1).lt.dt_loc ) then
!               xc1(i+1) = xc1(i)
!               yc1(i+1) = yc1(i)
!            end if
!         end if

      end do

      xc(:, j) = xc1
      yc(:, j) = yc1

      if (Lalllines) then
         dt_tot = dt
      else
         dt_tot = dt_tot + dt_loc
      end if

      ifrontold = ifrontnew

!      call qnerror(' ', ' ', ' ')

!     erase front
!      if ( dt_tot.lt.dheight ) then
      call setcol(0)
      call movabs(xf(1), yf(1))
      do i = 2, nf
         if (xf(i) /= dmiss) then
            call lnabs(xf(i), yf(i))
         else
            if (i < nf) call movabs(xf(i + 1), yf(i + 1))
         end if
      end do
!      end if

!     press any mouse button to terminate
      call halt3(ja3)
      if (ja3 > 0) then
         istop = 1
         if (dt_tot < dt) then ! remove this incomplete front
            xc1 = DMISS
            yc1 = DMISS
         end if
         exit
      end if

      if (dt_tot < dt) then
!        update normal vectors and front

!        compute the growth velocity vectors
!         edgevel = 1d0
         call comp_vel(mc, xc1, yc1, edgevel, vel)

!        disable points that have no valid normal vector
         do i = 1, mc
            if (vel(1, i) == DMISS) then
!               xc(i,j) = DMISS
               xc1(i) = DMISS
            end if
         end do
!        remove stationary points
         where (ifrontold == 0) xc1 = DMISS

!        find front points and fill front normal vectors
         call findfront(mc, nc, mmax, nmax, xc, yc, numf, xf, yf, idxf, nf)

!        copy growth velocity vectors to front
         call copy_vel_to_front(mc, j, vel, ifrontold, nf, numf, xf, yf, velf, idxf)

      end if ! if ( dt_tot.lt.dt )
   end do

!  disable reverted gridlines
!    the gridline connecting two layers will cross
   if (.not. Lalllines .and. j > 2) then
      do i = 2, mc - 1
         if (xc1(i) == DMISS) cycle
         if (dcosphi(xc(i, j - 2), yc(i, j - 2), xc(i, j - 1), yc(i, j - 1), xc(i, j - 1), yc(i, j - 1), xc1(i), yc1(i), jsferic, jasfer3D, dxymis) < -0.5) then
            call get_LR(mc, xc1, yc1, i, iL, iR)
            do ii = iL + 1, iR - 1
               ifrontnew(ii) = 0
               xc(ii, j) = DMISS
            end do
         end if
      end do
   end if

!  check aspectratio
!   if ( .not.Lalllines ) then
!      do i=1,mc
!         call get_LR(mc, xc(:,j), yc(:,j), i, iL, iR)
!         if ( iL.eq.iR ) cycle
!         dh = dbdistance(xc(i,j-1),  yc(i,j-1),  xc(i,j),  yc(i,j))
!         daspect = 2d0*dh/dbdistance(xc(iL,j),yc(iL,j),xc(iR,j),yc(iR,j))
!         if ( daspect.ge.maxaspect ) then
!            ifrontnew(i) = 0
!         end if
!      end do
!   end if

   ifront = ifrontnew

   if (Lalllines) then
      dt = min(dt, 5d2)
   end if

!  deallocate
   deallocate (dtmax2, xf, yf, velf, idxf)

!  restore settings
   dtolLR = dtolLR_bak

   return
end subroutine growlayer
end module m_grow_layer
