!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_get_ij
   implicit none
contains
   subroutine get_ij(x, xh, mmax, nmax, mnmax, i1, i2, j1, j2)
      use precision, only: dp

      integer, intent(in) :: mmax
      integer, intent(in) :: nmax
      integer, intent(in) :: mnmax
      real(kind=dp), intent(in) :: x(mmax, nmax)
      real(kind=dp), intent(out) :: xh(mnmax)
      integer, intent(in) :: i1
      integer, intent(in) :: i2
      integer, intent(in) :: j1
      integer, intent(in) :: j2

      integer :: i, j, k

      ! Extract a line (xh) from an array (x)
      k = 0
      do j = j1, j2
         do i = i1, i2
            k = k + 1
            xh(k) = x(i, j)
         end do
      end do
   end subroutine get_ij
end module m_get_ij
