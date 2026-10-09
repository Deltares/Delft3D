!> Tests for single and paired tridiagonal solves.
module test_tridag
   use assertions_gtest, only: f90_expect_near
   use precision, only: dp
   use m_tridag, only: tridag, tridag_two_rhs

   implicit none(type, external)
   private
   public :: test_single_rhs, test_two_rhs, test_zero_constant_rhs, test_first_pivot, test_eliminated_pivot

   real(kind=dp), parameter :: tolerance = 1.0e-12_dp

contains

   !$f90tw TESTCODE(TEST, tests_tridag, single_rhs, test_single_rhs,
   !> Check a known solution and preservation of the input matrix and right-hand side.
   subroutine test_single_rhs() bind(C)
      real(kind=dp), dimension(3) :: lower, diagonal, upper, rhs, work, solution

      lower = [99.0_dp, -2.0_dp, -1.0_dp]
      diagonal = [4.0_dp, 5.0_dp, 6.0_dp]
      upper = [-1.0_dp, -3.0_dp, 99.0_dp]
      rhs = [6.0_dp, -21.0_dp, 20.0_dp]

      call tridag(lower, diagonal, upper, rhs, work, solution, 3)

      call f90_expect_near(solution, [1.0_dp, -2.0_dp, 3.0_dp], tolerance, "Single RHS solution")
      call f90_expect_near(lower, [99.0_dp, -2.0_dp, -1.0_dp], 0.0_dp, "Lower diagonal unchanged")
      call f90_expect_near(diagonal, [4.0_dp, 5.0_dp, 6.0_dp], 0.0_dp, "Main diagonal unchanged")
      call f90_expect_near(upper, [-1.0_dp, -3.0_dp, 99.0_dp], 0.0_dp, "Upper diagonal unchanged")
      call f90_expect_near(rhs, [6.0_dp, -21.0_dp, 20.0_dp], 0.0_dp, "RHS unchanged")
      call f90_expect_near(work(1), 0.0_dp, 0.0_dp, "First workspace entry initialized")
   end subroutine test_single_rhs
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, two_rhs, test_two_rhs,
   !> Compare paired solves with separate solves and independent residuals for 1--64 rows.
   subroutine test_two_rhs() bind(C)
      integer, parameter :: max_rows = 64
      real(kind=dp), parameter :: constant_rhs = -0.7_dp
      real(kind=dp), dimension(max_rows) :: lower, diagonal, upper, rhs, constant_vector
      real(kind=dp), dimension(max_rows) :: work, reference_work, solution, constant_solution
      real(kind=dp), dimension(max_rows) :: reference_solution, reference_constant_solution, expected
      real(kind=dp), dimension(max_rows) :: saved_lower, saved_diagonal, saved_upper, saved_rhs
      integer :: row, n

      do row = 1, max_rows
         lower(row) = -0.2_dp - 0.01_dp * real(mod(row, 5), dp)
         upper(row) = 0.15_dp + 0.025_dp * real(mod(row, 3), dp)
         diagonal(row) = 1.5_dp + abs(lower(row)) + abs(upper(row)) + 0.01_dp * real(row, dp)
         expected(row) = sin(real(row, dp))
      end do
      constant_vector = constant_rhs
      saved_lower = lower
      saved_diagonal = diagonal
      saved_upper = upper

      do n = 1, max_rows
         rhs(1:n) = matrix_vector(lower(1:n), diagonal(1:n), upper(1:n), expected(1:n))
         saved_rhs(1:n) = rhs(1:n)
         call tridag(lower, diagonal, upper, rhs, reference_work, reference_solution, n)
         call tridag(lower, diagonal, upper, constant_vector, reference_work, reference_constant_solution, n)
         call tridag_two_rhs(lower, diagonal, upper, rhs, constant_rhs, work, solution, constant_solution, n)

         call f90_expect_near(solution(1:n), expected(1:n), tolerance, "Paired vector RHS known solution")
         call f90_expect_near(solution(1:n), reference_solution(1:n), tolerance, "Paired vector RHS vs single solve")
         call f90_expect_near(constant_solution(1:n), reference_constant_solution(1:n), tolerance, &
                              "Paired constant RHS vs single solve")
         call f90_expect_near(matrix_vector(lower(1:n), diagonal(1:n), upper(1:n), solution(1:n)), &
                              rhs(1:n), tolerance, "Vector RHS residual")
         call f90_expect_near(matrix_vector(lower(1:n), diagonal(1:n), upper(1:n), constant_solution(1:n)), &
                              constant_vector(1:n), tolerance, "Constant RHS residual")
         call f90_expect_near(rhs(1:n), saved_rhs(1:n), 0.0_dp, "Paired RHS unchanged")
      end do

      call f90_expect_near(lower, saved_lower, 0.0_dp, "Paired lower diagonal unchanged")
      call f90_expect_near(diagonal, saved_diagonal, 0.0_dp, "Paired main diagonal unchanged")
      call f90_expect_near(upper, saved_upper, 0.0_dp, "Paired upper diagonal unchanged")
   end subroutine test_two_rhs
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, zero_constant_rhs, test_zero_constant_rhs,
   !> A zero constant right-hand side must produce a zero second solution.
   subroutine test_zero_constant_rhs() bind(C)
      real(kind=dp), dimension(3) :: lower, diagonal, upper, rhs, work, solution, constant_solution

      lower = -1.0_dp
      diagonal = 4.0_dp
      upper = -1.0_dp
      rhs = [5.0_dp, 6.0_dp, 7.0_dp]

      call tridag_two_rhs(lower, diagonal, upper, rhs, 0.0_dp, work, solution, constant_solution, 3)

      call f90_expect_near(constant_solution, [0.0_dp, 0.0_dp, 0.0_dp], 0.0_dp, "Zero constant RHS")
      call f90_expect_near(matrix_vector(lower, diagonal, upper, solution), rhs, tolerance, "Nonzero vector RHS residual")
   end subroutine test_zero_constant_rhs
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, first_pivot, test_first_pivot,
   !> Guard zero and signed small first pivots in both solver variants.
   subroutine test_first_pivot() bind(C)
      real(kind=dp), dimension(3), parameter :: pivots = [0.0_dp, 0.5e-15_dp, -0.5e-15_dp]
      real(kind=dp), dimension(1) :: lower, diagonal, upper, rhs, work, solution, constant_solution, reference
      real(kind=dp) :: expected_sign
      integer :: pivot_index

      lower = 99.0_dp
      upper = 99.0_dp
      rhs = 2.0e-15_dp

      do pivot_index = 1, size(pivots)
         diagonal = pivots(pivot_index)
         expected_sign = sign(1.0_dp, pivots(pivot_index))
         call tridag(lower, diagonal, upper, rhs, work, reference, 1)
         call tridag_two_rhs(lower, diagonal, upper, rhs, 3.0e-15_dp, work, solution, constant_solution, 1)

         call f90_expect_near(reference(1), 2.0_dp * expected_sign, tolerance, "Single solve guarded first pivot")
         call f90_expect_near(solution(1), reference(1), tolerance, "Paired solve guarded first pivot")
         call f90_expect_near(constant_solution(1), 3.0_dp * expected_sign, tolerance, "Constant RHS guarded first pivot")
         call f90_expect_near(work(1), 0.0_dp, 0.0_dp, "Single-row workspace initialized")
      end do
   end subroutine test_first_pivot
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, eliminated_pivot, test_eliminated_pivot,
   !> Preserve the signed small-pivot regularization after elimination.
   subroutine test_eliminated_pivot() bind(C)
      real(kind=dp), dimension(3), parameter :: second_diagonals = [0.5_dp, 0.5_dp + 0.5e-15_dp, 0.5_dp - 0.5e-15_dp]
      real(kind=dp), dimension(2) :: lower, diagonal, upper, rhs, work, solution, constant_solution, reference
      real(kind=dp) :: expected_sign
      integer :: pivot_index

      lower = [99.0_dp, 1.0_dp]
      upper = [1.0_dp, 99.0_dp]
      rhs = 2.0e-15_dp

      do pivot_index = 1, size(second_diagonals)
         diagonal = [2.0_dp, second_diagonals(pivot_index)]
         expected_sign = sign(1.0_dp, diagonal(2) - 0.5_dp)
         call tridag(lower, diagonal, upper, rhs, work, reference, 2)
         call tridag_two_rhs(lower, diagonal, upper, rhs, 1.0e-15_dp, work, solution, constant_solution, 2)

         call f90_expect_near(solution, reference, tolerance, "Paired solve guarded eliminated pivot")
         call f90_expect_near(solution(2), expected_sign, tolerance, "Vector RHS signed eliminated pivot")
         call f90_expect_near(constant_solution(2), 0.5_dp * expected_sign, tolerance, "Constant RHS signed eliminated pivot")
      end do
   end subroutine test_eliminated_pivot
   !$f90tw)

   pure function matrix_vector(lower, diagonal, upper, vector) result(product)
      real(kind=dp), dimension(:), intent(in) :: lower, diagonal, upper, vector
      real(kind=dp), dimension(size(vector)) :: product
      integer :: row, n

      n = size(vector)
      product = diagonal * vector
      do row = 2, n
         product(row) = product(row) + lower(row) * vector(row - 1)
      end do
      do row = 1, n - 1
         product(row) = product(row) + upper(row) * vector(row + 1)
      end do
   end function matrix_vector

end module test_tridag
