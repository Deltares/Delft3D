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

!> find the frontline of the old (static) grid
subroutine findfront(mc, nc, mmax, nmax, xc, yc, num, xf, yf, idxf, nf)
   use precision, only: dp
   use m_missing, only: dmiss
   use m_get_lr

   implicit none

   integer, intent(in) :: mc, nc !< grid dimensions
   integer, intent(in) :: mmax, nmax !< array size
   real(kind=dp), dimension(mmax, nmax), intent(in) :: xc, yc !< grid point coordinates

   integer, intent(in) :: num !< array size
   real(kind=dp), dimension(num), intent(inout) :: xf, yf !< front point coordinates
   integer, dimension(2, num), intent(inout) :: idxf !< (i,j)-indices of grid points
   integer, intent(out) :: nf !< front dimension

   integer, dimension(mc - 1) :: jhfrontedge ! j-index of i-front edges

   integer :: i, j, iL, iR

   integer :: j1, j2

   idxf = 0

!  find the j-index of the i-front edges
   do i = 1, mc - 1
      jhfrontedge(i) = nc
      do j = 1, nc
         if (xc(i, j) == DMISS .or. xc(i + 1, j) == DMISS) then
            jhfrontedge(i) = j - 1
            exit
         end if
      end do
   end do

!  make the front
   nf = 0
   j1 = 1

!  check for circular connectivity
   i = 1
   call get_LR(mc, xc(:, 1), yc(:, 1), i, iL, iR)
   if (iL == i) then
      nf = nf + 1
      xf(nf) = xc(1, 1)
      yf(nf) = yc(1, 1)
      idxf(:, nf) = (/1, 1/)
   else
      nf = nf + 1
      j1 = jhfrontedge(iL)
      j2 = jhfrontedge(i)
      xf(nf) = xc(i, j2)
      yf(nf) = yc(i, j2)
      idxf(:, nf) = (/i, j2/)
   end if

   do i = 1, mc - 1
      call get_LR(mc, xc(:, 1), yc(:, 1), i, iL, iR)
      j2 = jhfrontedge(i)

      if (j2 > 0) then
         if (j1 == 0) then
            nf = nf + 1
            xf(nf) = xc(i, 1)
            yf(nf) = yc(i, 1)
            idxf(:, nf) = (/i, 1/)
         end if

!        add j-edges from j1 to j2
         do j = j1 + 1, j2
            nf = nf + 1
            xf(nf) = xc(i, j)
            yf(nf) = yc(i, j)
            idxf(:, nf) = (/i, j/)
         end do
         do j = j1 - 1, j2, -1
            nf = nf + 1
            xf(nf) = xc(i, j)
            yf(nf) = yc(i, j)
            idxf(:, nf) = (/i, j/)
         end do

!        add i-edge from i to i+1
         nf = nf + 1
         xf(nf) = xc(i + 1, j2)
         yf(nf) = yc(i + 1, j2)
         idxf(:, nf) = (/i + 1, j2/)
      else
         if (j1 > 0) then
            do j = j1 - 1, 1, -1
               nf = nf + 1
               xf(nf) = xc(i, j)
               yf(nf) = yc(i, j)
               idxf(:, nf) = (/i, j/)
            end do
            nf = nf + 1
            xf(nf) = DMISS
            yf(nf) = DMISS
            idxf(:, nf) = (/i, 0/)
         end if
      end if

      j1 = j2
   end do

!  add last j-edges
!  check for circular connectivity
   i = mc
   call get_LR(mc, xc(:, 1), yc(:, 1), i, iL, iR)
   if (iR == i) then
      do j = j2, 1, -1
         nf = nf + 1
         xf(nf) = xc(i, j)
         yf(nf) = yc(i, j)
         idxf(:, nf) = (/i, j/)
      end do
   end if

   return
end subroutine findfront
