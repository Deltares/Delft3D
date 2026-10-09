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

   public :: tridag, tridag_two_rhs

   real(kind=dp), parameter :: pivot_tolerance = 1.0e-15_dp

contains

   !> Solve a tridiagonal system with the Thomas algorithm and small-pivot regularization.
   subroutine tridag(lower, diagonal, upper, rhs, work, solution, n)
      integer, intent(in) :: n !< Number of rows; must be at least one.
      real(kind=dp), dimension(n), intent(in) :: lower !< Lower diagonal; lower(1) is unused.
      real(kind=dp), dimension(n), intent(in) :: diagonal !< Main diagonal.
      real(kind=dp), dimension(n), intent(in) :: upper !< Upper diagonal; upper(n) is unused.
      real(kind=dp), dimension(n), intent(in) :: rhs !< Right-hand side.
      real(kind=dp), dimension(n), intent(out) :: work !< Workspace for the normalized upper diagonal.
      real(kind=dp), dimension(n), intent(out) :: solution !< Solution.

      integer :: row
      real(kind=dp) :: pivot, inverse_pivot

      work(1) = 0.0_dp
      pivot = diagonal(1)
      if (abs(pivot) < pivot_tolerance) then
         pivot = sign(pivot_tolerance, pivot)
      end if
      inverse_pivot = 1.0_dp / pivot
      solution(1) = rhs(1) * inverse_pivot
      do row = 2, n
         work(row) = upper(row - 1) * inverse_pivot
         pivot = diagonal(row) - lower(row) * work(row)
         if (abs(pivot) < pivot_tolerance) then
            pivot = sign(pivot_tolerance, pivot)
         end if
         inverse_pivot = 1.0_dp / pivot
         solution(row) = (rhs(row) - lower(row) * solution(row - 1)) * inverse_pivot
      end do

      do row = n - 1, 1, -1
         solution(row) = solution(row) - work(row + 1) * solution(row + 1)
      end do
   end subroutine tridag

   !> Solve two systems with the same tridiagonal matrix, sharing the Thomas elimination.
   !! The second right-hand side has the same value in every row. Reciprocal pivots
   !! are shared by both solutions; results need not be bitwise identical to tridag.
   subroutine tridag_two_rhs(lower, diagonal, upper, rhs, constant_rhs, work, solution, constant_solution, n)
      integer, intent(in) :: n !< Number of rows; must be at least one.
      real(kind=dp), dimension(n), intent(in) :: lower !< Lower diagonal; lower(1) is unused.
      real(kind=dp), dimension(n), intent(in) :: diagonal !< Main diagonal.
      real(kind=dp), dimension(n), intent(in) :: upper !< Upper diagonal; upper(n) is unused.
      real(kind=dp), dimension(n), intent(in) :: rhs !< First right-hand side.
      real(kind=dp), intent(in) :: constant_rhs !< Value in every row of the second right-hand side.
      real(kind=dp), dimension(n), intent(out) :: work !< Workspace for the normalized upper diagonal.
      real(kind=dp), dimension(n), intent(out) :: solution !< Solution for rhs.
      real(kind=dp), dimension(n), intent(out) :: constant_solution !< Solution for constant_rhs.

      integer :: row
      real(kind=dp) :: pivot, inverse_pivot

      work(1) = 0.0_dp
      pivot = diagonal(1)
      if (abs(pivot) < pivot_tolerance) then
         pivot = sign(pivot_tolerance, pivot)
      end if
      inverse_pivot = 1.0_dp / pivot
      solution(1) = rhs(1) * inverse_pivot
      constant_solution(1) = constant_rhs * inverse_pivot

      do row = 2, n
         work(row) = upper(row - 1) * inverse_pivot
         pivot = diagonal(row) - lower(row) * work(row)
         if (abs(pivot) < pivot_tolerance) then
            pivot = sign(pivot_tolerance, pivot)
         end if
         inverse_pivot = 1.0_dp / pivot
         solution(row) = (rhs(row) - lower(row) * solution(row - 1)) * inverse_pivot
         constant_solution(row) = (constant_rhs - lower(row) * constant_solution(row - 1)) * inverse_pivot
      end do

      do row = n - 1, 1, -1
         solution(row) = solution(row) - work(row + 1) * solution(row + 1)
         constant_solution(row) = constant_solution(row) - work(row + 1) * constant_solution(row + 1)
      end do
   end subroutine tridag_two_rhs

end module m_tridag
