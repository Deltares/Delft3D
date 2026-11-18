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
   use, intrinsic :: iso_fortran_env, only: int64
   use precision, only: dp

   implicit none(type, external)

   private

   public :: tridag, tridag_two_rhs
   public :: reset_tridag_pivot_count, report_tridag_pivot_count

   real(kind=dp), parameter :: PIVOT_FLOOR = 1.0e-15_dp
   ! Temporary partition-local diagnostic, reported at the end of the run.
   integer(kind=int64) :: pivot_floor_count = 0_int64

contains

   !> Solve a tridiagonal system with the Thomas algorithm and small-pivot regularization.
   !! Provide normalized_upper as function parameter such that its memory is handled by the caller.
   !! A local array may cause stack overflows and a local save array may cause race conditions.
   subroutine tridag(lower, diagonal, upper, rhs, normalized_upper, solution, n)
      integer, intent(in) :: n !< Number of rows; must be at least one.
      real(kind=dp), dimension(n), intent(in) :: lower !< Lower diagonal; lower(1) is unused.
      real(kind=dp), dimension(n), intent(in) :: diagonal !< Main diagonal.
      real(kind=dp), dimension(n), intent(in) :: upper !< Upper diagonal; upper(n) is unused.
      real(kind=dp), dimension(n), intent(in) :: rhs !< Right-hand side.
      real(kind=dp), dimension(n), intent(out) :: normalized_upper !< Normalized upper-diagonal scratch coefficients; entry 1 is zero.
      real(kind=dp), dimension(n), intent(out) :: solution !< Solution.

      integer :: row, regularized_pivots
      real(kind=dp) :: pivot, inverse_pivot

      regularized_pivots = 0
      normalized_upper(1) = 0.0_dp
      pivot = diagonal(1)
      if (abs(pivot) < PIVOT_FLOOR) then
         pivot = sign(PIVOT_FLOOR, pivot)
         regularized_pivots = regularized_pivots + 1
      end if
      inverse_pivot = 1.0_dp / pivot
      solution(1) = rhs(1) * inverse_pivot
      do row = 2, n
         normalized_upper(row) = upper(row - 1) * inverse_pivot
         pivot = diagonal(row) - lower(row) * normalized_upper(row)
         if (abs(pivot) < PIVOT_FLOOR) then
            pivot = sign(PIVOT_FLOOR, pivot)
            regularized_pivots = regularized_pivots + 1
         end if
         inverse_pivot = 1.0_dp / pivot
         solution(row) = (rhs(row) - lower(row) * solution(row - 1)) * inverse_pivot
      end do

      do row = n - 1, 1, -1
         solution(row) = solution(row) - normalized_upper(row + 1) * solution(row + 1)
      end do
      if (regularized_pivots > 0) then
         !$omp atomic update
         pivot_floor_count = pivot_floor_count + int(regularized_pivots, int64)
      end if
   end subroutine tridag

   !> Solve two systems with the same tridiagonal matrix and a shared elimination.
   !! The second right-hand side has the same value in every row.
   !! Pivots are regularized consistently with tridag.
   !! Reciprocal pivots may cause small rounding differences from separate tridag calls.
   subroutine tridag_two_rhs(lower, diagonal, upper, rhs, constant_rhs, normalized_upper, solution, constant_solution, n)
      integer, intent(in) :: n !< Number of rows; must be at least one.
      real(kind=dp), dimension(n), intent(in) :: lower !< Lower diagonal; lower(1) is unused.
      real(kind=dp), dimension(n), intent(in) :: diagonal !< Main diagonal.
      real(kind=dp), dimension(n), intent(in) :: upper !< Upper diagonal; upper(n) is unused.
      real(kind=dp), dimension(n), intent(in) :: rhs !< First right-hand side.
      real(kind=dp), intent(in) :: constant_rhs !< Value in every row of the second right-hand side.
      real(kind=dp), dimension(n), intent(out) :: normalized_upper !< Normalized upper-diagonal workspace.
      real(kind=dp), dimension(n), intent(out) :: solution !< Solution for rhs.
      real(kind=dp), dimension(n), intent(out) :: constant_solution !< Solution for constant_rhs.

      integer :: row, regularized_pivots
      real(kind=dp) :: pivot, inverse_pivot

      regularized_pivots = 0
      normalized_upper(1) = 0.0_dp
      pivot = diagonal(1)
      if (abs(pivot) < PIVOT_FLOOR) then
         pivot = sign(PIVOT_FLOOR, pivot)
         regularized_pivots = regularized_pivots + 1
      end if
      inverse_pivot = 1.0_dp / pivot
      solution(1) = rhs(1) * inverse_pivot
      constant_solution(1) = constant_rhs * inverse_pivot

      do row = 2, n
         normalized_upper(row) = upper(row - 1) * inverse_pivot
         pivot = diagonal(row) - lower(row) * normalized_upper(row)
         if (abs(pivot) < PIVOT_FLOOR) then
            pivot = sign(PIVOT_FLOOR, pivot)
            regularized_pivots = regularized_pivots + 1
         end if
         inverse_pivot = 1.0_dp / pivot
         solution(row) = (rhs(row) - lower(row) * solution(row - 1)) * inverse_pivot
         constant_solution(row) = (constant_rhs - lower(row) * constant_solution(row - 1)) * inverse_pivot
      end do

      do row = n - 1, 1, -1
         solution(row) = solution(row) - normalized_upper(row + 1) * solution(row + 1)
         constant_solution(row) = constant_solution(row) - normalized_upper(row + 1) * constant_solution(row + 1)
      end do
      if (regularized_pivots > 0) then
         !$omp atomic update
         pivot_floor_count = pivot_floor_count + int(regularized_pivots, int64)
      end if
   end subroutine tridag_two_rhs

   !> Reset the partition-local count before a run.
   subroutine reset_tridag_pivot_count()
      pivot_floor_count = 0_int64
   end subroutine reset_tridag_pivot_count

   !> Report the number of pivots regularized on this partition.
   subroutine report_tridag_pivot_count(partition_rank)
      use MessageHandling, only: mess, LEVEL_INFO

      integer, intent(in) :: partition_rank !< MPI rank of the partition, or zero in a serial run.
      character(len=160) :: message

      write (message, '(a,i0,a,i0)') 'Tridiagonal pivot floor applications on partition ', partition_rank, ': ', pivot_floor_count
      call mess(LEVEL_INFO, trim(message))
   end subroutine report_tridag_pivot_count

end module m_tridag
