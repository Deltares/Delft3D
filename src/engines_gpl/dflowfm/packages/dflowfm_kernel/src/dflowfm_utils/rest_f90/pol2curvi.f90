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

!> generate curvilinear mesh in polygon, based on three polygon nodes that define two sides 1-2 and 2-3
!>    the third side 3-4 is defined by the polygon nodes by matching the number of nodes with side 1-2
!>    ja4=1: the fourth side is also taken from the polygon, starting from 1 and matching the number of nodes with side 2-3, the end-node will not necessarily be 4
!>    ja4=0: the fourth side is linearly interpolated between nodes 1 and 4, polygon nodes beyond 4 not used
!>
!>   1 x - - - - - x 4
!>     |           |
!>     |           |
!>     |           |
!>   2 x-----------x 3
subroutine pol2curvi(i1, i2, i3, ja4)
   use m_grid
   use m_gridsettings
   use m_alloc
   use m_missing
   use m_polygon
   implicit none

   integer, intent(in) :: i1, i2, i3 !< first, second and third corner point in polygon, respectively
   integer, intent(in) :: ja4 !< use polygon for fourth side (1) or not (0)

   double precision, dimension(:, :), allocatable :: xh, yh ! mesh boundary coordinates

   double precision :: xi

   integer :: i4, Nh
   integer :: istart, iend
   integer :: i, j, mcL, mcR, ncL, ncR

   integer :: idir, ipoint, num, numsubpol

   integer :: ierror ! error (1) or not (0)

   ierror = 1

   if (NPL <= 4) goto 1234

!  get start and end pointers in polygon
   call get_polstartend(NPL, XPL, YPL, i1, istart, iend)
   numsubpol = iend - istart + 1

!  get grid size and orientation
   mcR = i2 - i1; if (mcR < 0) mcR = mcR + numsubpol
   mcL = i1 - i2; if (mcL < 0) mcL = mcL + numsubpol

   if (mcR <= mcL) then
      idir = 1
      mc = mcR + 1
   else
      idir = -1
      mc = mcL + 1
   end if

   ncR = i3 - i2; if (ncR < 0) ncR = ncR + numsubpol
   ncL = i2 - i3; if (ncL < 0) ncL = ncL + numsubpol

   if (idir == 1) then
      nc = ncR + 1
   else
      nc = ncL + 1
   end if

!  get fourth corner index
   i4 = i3 + idir * (mc - 1)
   if (i4 < istart) i4 = i4 + numsubpol
   if (i4 > iend) i4 = i4 - numsubpol

!  check if polygon suffices
   if (ja4 == 1) then
      num = 2 * (mc - 1) + 2 * (nc - 1)
   else
      num = 1 + 2 * (mc - 1) + (nc - 1)
   end if

   if (num > numsubpol) then
!     polygon not large enough
      call qnerror('polygon is not large enough', ' ', ' ')
      goto 1234
   end if

!  allocate array with boundary coordinates
   Nh = max(mc, nc)
   allocate (xh(Nh, 4))
   allocate (yh(Nh, 4))

!  fill boundary coordinates
   if (ja4 == 1) then
!     fourth side from polygon
      ipoint = i1
      do j = 1, nc
         xh(j, 1) = xpl(ipoint)
         yh(j, 1) = ypl(ipoint)
         ipoint = ipoint - idir
         if (ipoint < istart) ipoint = ipoint + numsubpol
         if (ipoint > iend) ipoint = ipoint - numsubpol
      end do
   else
!     interpolate fourth side
      do i = 1, nc
         xi = dble(i - 1) / dble(nc - 1)
         xh(i, 1) = (1d0 - xi) * xpl(i1) + xi * xpl(i4)
         yh(i, 1) = (1d0 - xi) * ypl(i1) + xi * ypl(i4)
      end do
   end if

   ipoint = i2
   do j = 1, nc
      xh(j, 2) = xpl(ipoint)
      yh(j, 2) = ypl(ipoint)
      ipoint = ipoint + idir
      if (ipoint < istart) ipoint = ipoint + numsubpol
      if (ipoint > iend) ipoint = ipoint - numsubpol
   end do

   ipoint = i1
   do i = 1, mc
      xh(i, 3) = xpl(ipoint)
      yh(i, 3) = ypl(ipoint)
      ipoint = ipoint + idir
      if (ipoint < istart) ipoint = ipoint + numsubpol
      if (ipoint > iend) ipoint = ipoint - numsubpol
   end do

   ipoint = i4
   do i = 1, mc
      xh(i, 4) = xpl(ipoint)
      yh(i, 4) = ypl(ipoint)
      ipoint = ipoint - idir
      if (ipoint < istart) ipoint = ipoint + numsubpol
      if (ipoint > iend) ipoint = ipoint - numsubpol
   end do

!  increase grid
   call increasegrid(mc, nc)

   MFAC = mc - 1
   NFAC = nc - 1

!  make grid
   call TRANFN2(xh(1, 1), xh(1, 2), xh(1, 3), xh(1, 4), & ! . 3 .       . 4 .
                yh(1, 1), yh(1, 2), yh(1, 3), yh(1, 4), & ! 4   2       1   2
                Nh, MMAX, NMAX, XC, YC) ! . 1 .       . 3 .

   ierror = 0
1234 continue

   if (ierror /= 0) then
      mc = 0
      nc = 0
   end if

!  deallocate
   if (allocated(xh)) deallocate (xh)
   if (allocated(yh)) deallocate (yh)

   return
end subroutine pol2curvi
