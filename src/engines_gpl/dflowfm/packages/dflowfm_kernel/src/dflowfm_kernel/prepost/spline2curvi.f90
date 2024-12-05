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

!> generate curvilinear grid from spline
subroutine spline2curvi()
   use precision, only: dp

   use m_confrm
   use m_comp_edgevel
   use m_change_spline2curvi_param
   use m_grid
   use m_splines
   use m_gridsettings
   use m_missing, only: dmiss, jins
   use m_spline2curvi
   use m_sferic
   use m_polygon
   use geometry_module, only: pinpok, normalout
   use m_readyy
   use m_increase_grid
   use m_restore_grd
   use m_deallocate_spline_props
   use m_postgrid
   use m_grow_layer

   implicit none

   integer :: ierror ! 0: no error, 1: error

   integer, allocatable, dimension(:) :: ifront ! active node in front (1) or not (0), dim(mc)
   real(kind=dp), allocatable, dimension(:) :: edgevel ! grid layer segment normal-velocity, dim(mc-1)
   integer, allocatable, dimension(:, :) :: nfac1 ! number of cells perpendicular to center spline, per edge on spline for each subinterval of grid layers, dimension(Nsubmax,mc-1)
   real(kind=dp), allocatable, dimension(:, :) :: dgrow1 ! grow factor, per edge on the spline for each subinterval of grid layers, dimension(Nsubmax,mc-1)

   integer, allocatable, dimension(:) :: nlist ! dummy array, dimension(Nsubmax)
   real(kind=dp) :: dt ! time step
   integer :: jacancelled
   integer :: igL
   integer :: i, is, js, isnew, isubL, isubR, j, jc
   integer :: mcs_old, j_loc
   integer :: istop, jatopol, mfacmax
   integer :: inhul
   real(kind=dp) :: hmax
!  grid edge-based cross splines
   real(kind=dp), dimension(2) :: xs1, ys1
   real(kind=dp) :: xe, ye, nx, ny
   integer, dimension(3, mcs) :: iLRmfac
   integer, dimension(mcs) :: id
   logical :: Lnewsplines
   logical :: jaAllPoints
   integer, external :: comp_nfac, get_isub
   real(kind=dp), external :: splinelength, comp_dgrow
   real(kind=dp), parameter :: dnu = -0.50d0
   integer :: nul, nul1(1), nul2(1, 1)

!  Note: edge_vel is the grow velocity per front edge and in Cartesian coordinates
!        vel is the grow velocity per front node and in spherical coordinates (when applicable)

   ierror = 1

   Lnewsplines = .false.

!   if ( jsferic.eq.1 ) then
!      call qnerror('spherical coordinates not supported', ' ', ' ')
!      return
!   end if

   call READYY('Growing curvilinear grid', 0d0)

!  get the settings from a parameter menu, if user presses 'Esc', do nothing.
   jacancelled = 0
   call change_spline2curvi_param(jacancelled)
   if (jacancelled == 1) then
      return
   end if

   mfacmax = mfac
   nc = nfac + 1

   if (mcs < 1) goto 1234 ! no splines

!  save splines
   call savesplines()

!  delete splines outside selecting polygon (mostly copied from deleteSelectedSplines)
   if (NPL > 2) then
      i = 1
      do while (i < mcs)
         jaAllPoints = .true.
         do j = 1, lensp(i)
            call pinpok(xsp(i, j), ysp(i, j), NPL, xpl, ypl, inhul, jins, dmiss)
            jaAllpoints = jaAllpoints .and. (inhul == 1)
         end do
         if (.not. jaAllpoints) then
            call delspline(i)
            ! splines are shifted to the left, so don't increment i.
         else
            i = i + 1
         end if
      end do
   end if

!  get the properties of the center splines (no angle check)
   nul = 0; nul1 = 0; nul2 = 0
   call get_splineprops(nul, nul1, nul2)

!  make the whole first gridline
   call make_wholegridline(ierror)
   call READYY('Growing curvilinear grid', 0.4d0)

   if (ierror == 1) goto 1234

   if (mc < 2) goto 1234 ! no curvigrid

!  make all gridedge based cross splines
!  remember original spline information
   Lnewsplines = .false.
   do is = 1, mcs
      id(is) = splineprops(is)%id
   end do
   iLRmfac = 0
   mcs_old = mcs

!  construct new, or artificial, cross splines through the grid edge center points
   do is = 1, mcs_old
      if (splineprops(is)%id /= 0) cycle ! center splines only

!      id(is)        = 0
      iLRmfac(1, is) = splineprops(is)%iL
      iLRmfac(2, is) = splineprops(is)%iR
      iLRmfac(3, is) = splineprops(is)%mfac

      hmax = splineprops(is)%hmax
      igL = splineprops(is)%iL

      do i = igL, igL + splineprops(is)%mfac - 1
         !     construct the cross spline through this edge
         xe = 0.5d0 * (xg1(i) + xg1(i + 1))
         ye = 0.5d0 * (yg1(i) + yg1(i + 1))
         call normalout(xg1(i), yg1(i), xg1(i + 1), yg1(i + 1), nx, ny, jsferic, jasfer3D, dmiss, dxymis)

         if (jsferic /= 1) then
            xs1 = xe + 2d0 * hmax * (/-nx, nx/)
            ys1 = ye + 2d0 * hmax * (/-ny, ny/)
         else
            xs1 = xe + 2d0 * hmax * (/-nx, nx/) / (Ra * dg2rd)
            ys1 = ye + 2d0 * hmax * (/-ny, ny/) / (Ra * dg2rd)
         end if

         isnew = mcs + 1

         call addSplinePoint(isnew, xs1(1), ys1(1))
         call addSplinePoint(isnew, xs1(2), ys1(2))
         if (.not. Lnewsplines) Lnewsplines = .true.
      end do
   end do

   call deallocate_splineprops()
   call get_splineprops(mcs_old, id, iLRmfac)

!  artificial cross spline: remove the last part of the subintervals (since it makes no sence, as the artificial cross spline has an arbitrary, but sufficiently large, length)
   do is = 1, mcs_old
      if (splineprops(is)%id /= 0) cycle ! center splines only
      do j = 1, splineprops(is)%ncs
         js = splineprops(is)%ics(j)
         if (splineprops(js)%id == 3) then ! artificial cross spline only
            splineprops(is)%NsubL(j) = splineprops(is)%NsubL(j) - 1
            splineprops(is)%NsubR(j) = splineprops(is)%NsubR(j) - 1
         end if
      end do
   end do
   call READYY('Growing curvilinear grid', .7d0)

!  allocate
   allocate (edgevel(mc - 1), nfac1(Nsubmax, mc - 1), dgrow1(Nsubmax, mc - 1), nlist(Nsubmax))

!  compute edge velocities

   call comp_edgevel(mc, edgevel, dgrow1, nfac1, ierror)
   if (ierror /= 0) goto 1234

   call increasegrid(mc, nc)
   xc = DMISS
   yc = DMISS

!  copy first gridline into grid
   jc = 1
   xc(1:mc, jc) = xg1(1:mc)
   yc(1:mc, jc) = yg1(1:mc)

!  allocate
   allocate (ifront(mc))
   ifront = 1
   where (xc(:, jc) == DMISS) ifront = 0

   do i = 1, mc
      if (sum(nfac1(:, max(i - 1, 1))) == 0 .and. sum(nfac1(:, min(i, mc - 1))) == 0) then
         ifront(i) = 0
      end if
   end do

!  grow the grid
   dt = 1d0
   do j = jc + 1, nc
!      idum = 1
!      call plot(idum)
      call growlayer(mc, nc, mmax, nmax, 1, j, edgevel, dt, xc, yc, ifront, istop)

!     update edge velocity
      nlist(:) = nfac1(:, 1)
      isubR = get_isub(j, nlist, j_loc)
      do i = 1, mc ! loop over grid points
!        determine the subinterval of grid layers
         isubL = isubR ! edge left of grid point
         nlist(:) = nfac1(:, min(i, mc - 1))
         isubR = get_isub(j, nlist, j_loc) ! edge right of grid point

         if (isubR > 0 .and. i < mc .and. j_loc > 0) then ! j_loc.eq.0: first layer in subinterval
            edgevel(i) = dgrow1(isubR, i) * edgevel(i)
         end if

         if (isubL == 0 .and. isubR == 0) then ! deactivate grid point
            ifront(i) = 0
         end if
      end do

      if (dt < 1d-8 .or. istop == 1) exit
   end do

   call postgrid()
   call READYY('Growing curvilinear grid', 1d0)
   call READYY('Growing curvilinear grid', -1d0)

   jatopol = 1
   call confrm('Copy center splines to polygon?', jatopol)
   if (jatopol == 1) then
      call spline2poly() ! (re)sample the spline
   end if

!  merge grids on both sides of centerspline(s)
   call merge_spline2curvigrids()

   ierror = 0

1234 continue

!  deallocate
   if (allocated(edgevel)) deallocate (edgevel, nfac1, dgrow1, nlist)
   if (allocated(ifront)) deallocate (ifront)
   if (allocated(xg1)) deallocate (xg1, yg1, sg1)
   call deallocate_splineprops()

!  restore
   mfac = mfacmax
   if (Lnewsplines) call restoresplines()

!  in case of error: restore previous the grid
   if (ierror == 1) call restoregrd()

   return
end subroutine spline2curvi
