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

!
!
module m_tridag
   use precision, only: dp

   implicit none(type, external)

   private

   public :: tridag

   real(kind=dp), parameter :: PIVOT_FLOOR = 1.0e-15_dp

contains

   !> Solve a tridiagonal system with the Thomas algorithm, regularizing small pivots after the first row.
   !! The first pivot is used unchanged, preserving the original solver behaviour.
   !! Provide normalized_upper as function parameter such that its memory is handled by the caller.
   !! A local array may cause stack overflows and a local save array may cause race conditions.
   subroutine tridag(lower, diagonal, upper, rhs, normalized_upper, solution, n)
      integer, intent(in) :: n !< Number of rows; must be at least one.
      real(kind=dp), dimension(n), intent(in) :: lower !< Lower diagonal; lower(1) is unused.
      real(kind=dp), dimension(n), intent(in) :: diagonal !< Main diagonal.
      real(kind=dp), dimension(n), intent(in) :: upper !< Upper diagonal; upper(n) is unused.
      real(kind=dp), dimension(n), intent(in) :: rhs !< Right-hand side.
      real(kind=dp), dimension(n), intent(out) :: normalized_upper !< Normalized upper-diagonal scratch coefficients; entry 1 is unused.
      real(kind=dp), dimension(n), intent(out) :: solution !< Solution.

      integer :: row
      real(kind=dp) :: pivot

      pivot = diagonal(1)
      solution(1) = rhs(1) / pivot
      do row = 2, n
         normalized_upper(row) = upper(row - 1) / pivot
         pivot = diagonal(row) - lower(row) * normalized_upper(row)
         if (abs(pivot) < PIVOT_FLOOR) then
            pivot = sign(PIVOT_FLOOR, pivot)
         end if
         solution(row) = (rhs(row) - lower(row) * solution(row - 1)) / pivot
      end do

      do row = n - 1, 1, -1
         solution(row) = solution(row) - normalized_upper(row + 1) * solution(row + 1)
      end do
   end subroutine tridag

end module m_tridag
