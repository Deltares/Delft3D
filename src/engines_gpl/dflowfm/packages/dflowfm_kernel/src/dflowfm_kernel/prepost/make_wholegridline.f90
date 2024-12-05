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

!> generate the first gridline of the whole grid, i.e. on all center splines
subroutine make_wholegridline(ierror)
   use precision, only: dp
   use m_splines
   use m_grid
   use m_gridsettings
   use m_spline2curvi
   use m_alloc
   use m_missing
   use m_qnerror

   implicit none

   integer, intent(out) :: ierror ! error (1) or not (0)

   real(kind=dp), allocatable, dimension(:) :: xlist, ylist

   integer :: ig ! index in gridline array
   integer :: is, mfacmax, num, Nmaxsize, numnew
   integer :: igL, igR, numcentersplines

   ierror = 1

   jacirc = 0 ! circularly connected gridlines not supported

   mfacmax = mfac ! from grid settings

!  allocate
   if (allocated(xg1)) deallocate (xg1)
   if (allocated(yg1)) deallocate (yg1)
   if (allocated(sg1)) deallocate (sg1)
   Nmaxsize = 2 * mfacmax + 1
   allocate (xg1(Nmaxsize), yg1(Nmaxsize), sg1(Nmaxsize))
   allocate (xlist(1), ylist(1))

!  make the first gridline
   ig = 0 ! index in gridline array
   numcentersplines = 0
   do is = 1, mcs
      if (splineprops(is)%id /= 0) cycle ! center splines only
      numcentersplines = numcentersplines + 1

!     determine the number of control points in the spline
      call nump(is, num)

!     reallocate if necessary
      Nmaxsize = ig + 2 * (mfacmax + 1) + 2 ! upper bound of new grid size, i.e. with two sides of spline and two DMISSes added
      if (Nmaxsize > ubound(xg1, 1)) then
         call realloc(xg1, Nmaxsize)
         call realloc(yg1, Nmaxsize)
         call realloc(sg1, Nmaxsize)
      end if

!     add DMISS if necessary
      if (ig > 1) then
         ig = ig + 1
         xg1(ig) = DMISS
         yg1(ig) = DMISS
         sg1(ig) = DMISS
      end if

!     make a gridline on the spline
      ig = ig + 1
      igL = ig

!     reallocate if necessary
      if (num > ubound(xlist, 1)) then
         numnew = int(1.2d0 * dble(num)) + 1
         call realloc(xlist, numnew)
         call realloc(ylist, numnew)
      end if
      xlist(1:num) = xsp(is, 1:num)
      ylist(1:num) = ysp(is, 1:num)
      call make_gridline(num, xlist, ylist, dwidth, mfacmax, mfac, splineprops(is)%hmax, xg1(ig), yg1(ig), sg1(ig), jacurv)

!     compute new (actual) grid size
!     new size   old size   both sides of spline   DMISS between both sides
      mc = ig - 1 + 2 * (mfac + 1) + 1

      ig = ig + mfac

!     add DMISS
      ig = ig + 1
      xg1(ig) = DMISS
      yg1(ig) = DMISS
      sg1(ig) = DMISS

!     add other side of gridline
      ig = ig + 1
      igR = ig
      xg1(ig:ig + mfac) = xg1(ig - 2:ig - 2 - mfac:-1)
      yg1(ig:ig + mfac) = yg1(ig - 2:ig - 2 - mfac:-1)
      sg1(ig:ig + mfac) = sg1(ig - 2:ig - 2 - mfac:-1)
      ig = ig + mfac

!     store indices in gridline array
      splineprops(is)%mfac = mfac
      splineprops(is)%iL = igL
      splineprops(is)%iR = igR
   end do ! do is=1,mcs

   if (numcentersplines == 0) then
      call qnerror('no center splines found', ' ', ' ')
      goto 1234
   end if

   ierror = 0

!  error handling
1234 continue

!  deallocate
   deallocate (xlist, ylist)

   mfac = mfacmax ! restore

   return
end subroutine make_wholegridline
