!> Tests for tridiagonal solves, input preservation and small-pivot handling.
module test_tridag
   use assertions_gtest, only: f90_expect_near
   use precision, only: dp
   use m_tridag, only: tridag

   implicit none(type, external)
   private
   public :: test_single_rhs, test_varying_sizes, test_zero_rhs, test_first_pivot, test_eliminated_pivot

   real(kind=dp), parameter :: tolerance = 1.0e-12_dp

contains

   !$f90tw TESTCODE(TEST, tests_tridag, single_rhs, test_single_rhs,
   !> Check a known solution and preservation of the input matrix and right-hand side.
   subroutine test_single_rhs() bind(C)
      real(kind=dp), dimension(3) :: lower, diagonal, upper, rhs, normalized_upper, solution

      lower = [99.0_dp, -2.0_dp, -1.0_dp]
      diagonal = [4.0_dp, 5.0_dp, 6.0_dp]
      upper = [-1.0_dp, -3.0_dp, 99.0_dp]
      rhs = [6.0_dp, -21.0_dp, 20.0_dp]

      call tridag(lower, diagonal, upper, rhs, normalized_upper, solution, 3)

      call f90_expect_near(solution, [1.0_dp, -2.0_dp, 3.0_dp], tolerance, "Single RHS solution")
      call f90_expect_near(lower, [99.0_dp, -2.0_dp, -1.0_dp], 0.0_dp, "Lower diagonal unchanged")
      call f90_expect_near(diagonal, [4.0_dp, 5.0_dp, 6.0_dp], 0.0_dp, "Main diagonal unchanged")
      call f90_expect_near(upper, [-1.0_dp, -3.0_dp, 99.0_dp], 0.0_dp, "Upper diagonal unchanged")
      call f90_expect_near(rhs, [6.0_dp, -21.0_dp, 20.0_dp], 0.0_dp, "RHS unchanged")
   end subroutine test_single_rhs
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, varying_sizes, test_varying_sizes,
   !> Check known solutions and residuals for vector and constant right-hand sides with 1--64 rows.
   subroutine test_varying_sizes() bind(C)
      integer, parameter :: max_rows = 64
      real(kind=dp), parameter :: constant_rhs = -0.7_dp
      real(kind=dp), dimension(max_rows) :: lower, diagonal, upper, rhs, constant_vector
      real(kind=dp), dimension(max_rows) :: normalized_upper, solution, constant_solution, expected
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
         rhs(1:n) = matrix_vector_product(lower(1:n), diagonal(1:n), upper(1:n), expected(1:n))
         saved_rhs(1:n) = rhs(1:n)
         call tridag(lower, diagonal, upper, rhs, normalized_upper, solution, n)
         call tridag(lower, diagonal, upper, constant_vector, normalized_upper, constant_solution, n)

         call f90_expect_near(solution(1:n), expected(1:n), tolerance, "Vector RHS known solution")
         call f90_expect_near(matrix_vector_product(lower(1:n), diagonal(1:n), upper(1:n), solution(1:n)), &
                              rhs(1:n), tolerance, "Vector RHS residual")
         call f90_expect_near(matrix_vector_product(lower(1:n), diagonal(1:n), upper(1:n), constant_solution(1:n)), &
                              constant_vector(1:n), tolerance, "Constant RHS residual")
         call f90_expect_near(rhs(1:n), saved_rhs(1:n), 0.0_dp, "RHS unchanged")
      end do

      call f90_expect_near(lower, saved_lower, 0.0_dp, "Lower diagonal unchanged")
      call f90_expect_near(diagonal, saved_diagonal, 0.0_dp, "Main diagonal unchanged")
      call f90_expect_near(upper, saved_upper, 0.0_dp, "Upper diagonal unchanged")
      call f90_expect_near(constant_vector, spread(constant_rhs, 1, max_rows), 0.0_dp, "Constant RHS unchanged")
   end subroutine test_varying_sizes
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, zero_rhs, test_zero_rhs,
   !> A zero right-hand side must produce a zero solution.
   subroutine test_zero_rhs() bind(C)
      real(kind=dp), dimension(3) :: lower, diagonal, upper, rhs, normalized_upper, solution

      lower = -1.0_dp
      diagonal = 4.0_dp
      upper = -1.0_dp
      rhs = 0.0_dp

      call tridag(lower, diagonal, upper, rhs, normalized_upper, solution, 3)

      call f90_expect_near(solution, [0.0_dp, 0.0_dp, 0.0_dp], 0.0_dp, "Zero RHS")
   end subroutine test_zero_rhs
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, first_pivot, test_first_pivot,
   !> Preserve the original use of signed small first pivots without regularization.
   subroutine test_first_pivot() bind(C)
      real(kind=dp), dimension(6), parameter :: pivots = [0.5e-15_dp, -0.5e-15_dp, 1.0e-15_dp, -1.0e-15_dp, 2.0e-15_dp, -2.0e-15_dp]
      real(kind=dp), dimension(6), parameter :: expected = [4.0_dp, -4.0_dp, 2.0_dp, -2.0_dp, 1.0_dp, -1.0_dp]
      real(kind=dp), dimension(1) :: lower, diagonal, upper, rhs, normalized_upper, solution
      integer :: pivot_index

      lower = 99.0_dp
      upper = 99.0_dp
      rhs = 2.0e-15_dp

      do pivot_index = 1, size(pivots)
         diagonal = pivots(pivot_index)
         call tridag(lower, diagonal, upper, rhs, normalized_upper, solution, 1)

         call f90_expect_near(solution(1), expected(pivot_index), 0.0_dp, "First pivot used without regularization")
      end do
   end subroutine test_first_pivot
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_tridag, eliminated_pivot, test_eliminated_pivot,
   !> Check that zero and small pivots created by elimination are replaced with a sign-preserving minimum magnitude.
   subroutine test_eliminated_pivot() bind(C)
      real(kind=dp), dimension(3), parameter :: second_diagonals = [0.5_dp, 0.5_dp + 0.5e-15_dp, 0.5_dp - 0.5e-15_dp]
      real(kind=dp), dimension(2) :: lower, diagonal, upper, rhs, normalized_upper, solution
      real(kind=dp) :: expected_sign
      integer :: pivot_index

      lower = [99.0_dp, 1.0_dp]
      upper = [1.0_dp, 99.0_dp]
      rhs = 2.0e-15_dp

      do pivot_index = 1, size(second_diagonals)
         diagonal = [2.0_dp, second_diagonals(pivot_index)]
         expected_sign = sign(1.0_dp, diagonal(2) - 0.5_dp)
         call tridag(lower, diagonal, upper, rhs, normalized_upper, solution, 2)

         call f90_expect_near(solution, [1.0e-15_dp - 0.5_dp * expected_sign, expected_sign], &
                              tolerance, "Signed eliminated pivot solution")
      end do
   end subroutine test_eliminated_pivot
   !$f90tw)

   pure function matrix_vector_product(lower, diagonal, upper, vector) result(product)
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
   end function matrix_vector_product

end module test_tridag
