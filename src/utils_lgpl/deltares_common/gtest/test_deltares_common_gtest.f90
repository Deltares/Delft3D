module test_deltares_common_gtest
   use assertions_gtest
   use precision, only: sp, dp
   use precision_basics, only: comparereal, equal
   use m_date_time_from_ref_date, only: date_time_from_ref_date

   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_within_tolerance, test_dp_equal_within_tolerance,
   !> Two dp numbers 1 ULP apart should be equal (within 2*epsilon)
   subroutine test_dp_equal_within_tolerance() bind(C)
      real(kind=dp) :: a, b
      a = 1.0_dp
      b = a + epsilon(a)
      call f90_expect_true(equal(a, b))
   end subroutine test_dp_equal_within_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_outside_tolerance, test_dp_equal_outside_tolerance,
   !> Two dp numbers >2 ULPs apart should not be equal
   subroutine test_dp_equal_outside_tolerance() bind(C)
      real(kind=dp) :: a, b
      a = 1.0_dp
      b = a + 3.0_dp * epsilon(a)
      call f90_expect_false(equal(a, b))
   end subroutine test_dp_equal_outside_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_large_values, test_dp_equal_large_values,
   !> Two large dp numbers 1 ULP apart at their scale should be equal (tolerance scales up)
   subroutine test_dp_equal_large_values() bind(C)
      real(kind=dp) :: a, b
      a = 1.0e12_dp
      b = a + epsilon(a) * a
      call f90_expect_true(equal(a, b))
   end subroutine test_dp_equal_large_values
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_small_values, test_dp_equal_small_values,
   !> Two very small dp numbers should be equal due to the max(..., 1.0) floor
   subroutine test_dp_equal_small_values() bind(C)
      real(kind=dp) :: a, b
      a = 1.0e-300_dp
      b = 2.0e-300_dp
      call f90_expect_true(equal(a, b))
   end subroutine test_dp_equal_small_values
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_sp_equal_within_tolerance, test_sp_equal_within_tolerance,
   !> Two sp numbers 1 ULP apart should be equal (within 2*epsilon)
   subroutine test_sp_equal_within_tolerance() bind(C)
      real(kind=sp) :: a, b
      a = 1.0_sp
      b = a + epsilon(a)
      call f90_expect_true(equal(a, b))
   end subroutine test_sp_equal_within_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_sp_equal_outside_tolerance, test_sp_equal_outside_tolerance,
   !> Two sp numbers >2 ULPs apart should not be equal
   subroutine test_sp_equal_outside_tolerance() bind(C)
      real(kind=sp) :: a, b
      a = 1.0_sp
      b = a + 3.0_sp * epsilon(a)
      call f90_expect_false(equal(a, b))
   end subroutine test_sp_equal_outside_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_eps_outside_tolerance, test_dp_equal_eps_outside_tolerance,
   !> Two dp numbers outside custom epsilon should not be equal
   subroutine test_dp_equal_eps_outside_tolerance() bind(C)
      real(kind=dp) :: a, b
      a = 1.0_dp
      b = 1.0_dp + 2.0e-6_dp
      call f90_expect_false(equal(a, b, 1.0e-6_dp))
   end subroutine test_dp_equal_eps_outside_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_large_values_requires_scaling, test_dp_equal_large_values_requires_scaling,
   !> Two large dp numbers that are equal within scaled tolerance but NOT within unscaled tolerance.
    !! This test would fail with a naive abs(a-b) < 2*epsilon(1.0) check.
   subroutine test_dp_equal_large_values_requires_scaling() bind(C)
      real(kind=dp) :: a, b
      a = 1.0e16_dp
      b = a + 3.0_dp ! abs(a-b)=3 > 2*epsilon(1.0)~4.4e-16 (naive fails), but 3 < 2*epsilon(a)*a~4.4 (scaled passes)

      ! verify the naive check would indeed fail (3 >> 2*epsilon(1.0))
      call f90_expect_false(abs(a - b) < 2.0_dp * epsilon(1.0_dp), "naive unscaled check should report not equal for large values")

      ! verify the scaled check correctly identifies them as equal (3 < 2*epsilon(a)*1e16 ~ 4.4)
      call f90_expect_true(equal(a, b))
   end subroutine test_dp_equal_large_values_requires_scaling
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_date_time_from_ref_date_int64, test_date_time_from_ref_date_int64,
   !> Times beyond signed 32-bit seconds produce the correct calendar date.
   subroutine test_date_time_from_ref_date_int64() bind(C)
      use iso_fortran_env, only: int64

      ! Local variables
      real(kind=dp) :: time_since_ref
      integer :: year, month, day, hour, minute, second

      time_since_ref = real(2_int64 ** 33, kind=dp)

      call date_time_from_ref_date(time_since_ref, '20000101', year, month, day, hour, minute, second)

      call f90_expect_eq(year, 2272)
      call f90_expect_eq(month, 3)
      call f90_expect_eq(day, 15)
      call f90_expect_eq(hour, 12)
      call f90_expect_eq(minute, 56)
      call f90_expect_eq(second, 32)

   end subroutine test_date_time_from_ref_date_int64
   !$f90tw)

end module test_deltares_common_gtest
