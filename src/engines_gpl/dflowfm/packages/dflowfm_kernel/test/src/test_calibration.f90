module test_calibration
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal

   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_calibration, test_valid_area_fractions_do_not_exceed_one, test_valid_area_fractions_do_not_exceed_one,
   !> Valid decimal area fractions must not be rejected due to accumulated rounding.
   subroutine test_valid_area_fractions_do_not_exceed_one() bind(C)
      integer, parameter :: number_of_areas = 21
      real(kind=dp), parameter :: area_fraction = 0.047619047619047616_dp
      real(kind=dp) :: area_sum
      integer :: area_index

      area_sum = 0.0_dp
      do area_index = 1, number_of_areas
          area_sum = area_sum + area_fraction
      end do

      call f90_expect_false(comparereal(area_sum, 1.0_dp) == 1, &
                           "Valid calibration area fractions must not be reported as exceeding one")
   end subroutine test_valid_area_fractions_do_not_exceed_one
   !$f90tw)

end module test_calibration