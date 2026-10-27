module test_accumulate_distance
   use assertions_gtest
   use m_accumulatedistance, only: accumulateDistance
   implicit none

contains

   !$f90tw TESTCODE(TEST, tests_accumulate_distance, test_simple_path, test_simple_path,
   subroutine test_simple_path() bind(C)
      use precision, only: dp
      integer, parameter :: PATH_LENGTH = 2
      real(kind=dp), parameter :: tolerance = 1e-8_dp
      real(kind=dp), dimension(PATH_LENGTH) :: x, y, running_distance, expected_running_distance

      x = [0.0_dp, 1.0_dp]
      y = [0.0_dp, 1.0_dp]
      expected_running_distance = [0.0_dp, sqrt(2.0_dp)]

      call accumulateDistance(x, y, running_distance, PATH_LENGTH)

      call f90_expect_near(running_distance, expected_running_distance, tolerance, "path length is not square root of 2")
   end subroutine test_simple_path
   !$f90tw)

   !$f90tw TESTCODE(TEST, tests_accumulate_distance, test_three_point_path, test_three_point_path,
   !> Distances along a three-point path must accumulate across both segments.
   subroutine test_three_point_path() bind(C)
      use precision, only: dp
      use iso_c_utils, only: cstr
      use m_sferic, only: jsferic, jasfer3D

      integer, parameter :: PATH_LENGTH = 3
      real(kind=dp), parameter :: tolerance = 1.0e-8_dp
      real(kind=dp), dimension(PATH_LENGTH) :: x, y, running_distance, expected_running_distance

      jsferic = 0
      jasfer3D = 0
      x = [0.0_dp, 3.0_dp, 3.0_dp]
      y = [0.0_dp, 0.0_dp, 4.0_dp]
      expected_running_distance = [0.0_dp, 3.0_dp, 7.0_dp]

      call accumulateDistance(x, y, running_distance, PATH_LENGTH)

      call f90_expect_near(running_distance, expected_running_distance, tolerance, &
                           cstr("Distances should accumulate along the path: [0, 3, 7]"))
   end subroutine test_three_point_path
   !$f90tw)
end module test_accumulate_distance
