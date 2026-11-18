!> Relative comparisons for tests migrated from ftnunit.
module test_relative_assertions
   use assertions_gtest, only: f90_assert_eq, f90_expect_near
   use precision, only: dp
   implicit none(type, external)
   private

   !> Preserve the symmetric relative tolerance used by the original tests.
   public :: expect_relative_near
   interface expect_relative_near
      module procedure expect_relative_near_scalar
      module procedure expect_relative_near_array
   end interface expect_relative_near

contains

   subroutine expect_relative_near_scalar(actual, expected, tolerance, message)
      real(kind=dp), intent(in) :: actual, expected, tolerance
      character(len=*), intent(in) :: message

      call f90_expect_near(actual, expected, 0.5_dp * tolerance * (abs(actual) + abs(expected)), message)
   end subroutine expect_relative_near_scalar

   subroutine expect_relative_near_array(actual, expected, tolerance, message)
      real(kind=dp), dimension(:), intent(in) :: actual, expected
      real(kind=dp), intent(in) :: tolerance
      character(len=*), intent(in) :: message
      integer :: i
      character(len=32) :: index

      if (.not. f90_assert_eq(size(actual), size(expected), message)) then
         return
      end if
      do i = 1, size(actual)
         write (index, '(a,i0,a)') ' (index ', i, ')'
         call expect_relative_near_scalar(actual(i), expected(i), tolerance, message // trim(index))
      end do
   end subroutine expect_relative_near_array

end module test_relative_assertions
