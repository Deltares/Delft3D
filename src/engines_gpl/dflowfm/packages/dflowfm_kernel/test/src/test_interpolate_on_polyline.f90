!> Regression test for the range status of polyline interpolation.
module test_interpolate_on_polyline
   use assertions_gtest, only: f90_expect_eq, f90_expect_near
   use precision, only: dp
   use iso_c_utils, only: cstr
   use m_interpolateOnPolyline, only: interpolateOnPolyline

   implicit none(type, external)
   private
   public :: test_distance_beyond_end_is_rejected

contains

   !$f90tw TESTCODE(TEST, tests_interpolate_on_polyline, test_distance_beyond_end_is_rejected, test_distance_beyond_end_is_rejected,
   !> Interpolate within a length-7 path, but reject a distance of 8.
   subroutine test_distance_beyond_end_is_rejected() bind(C)
      real(kind=dp), dimension(3), parameter :: x = [0.0_dp, 3.0_dp, 3.0_dp]
      real(kind=dp), dimension(3), parameter :: y = [0.0_dp, 0.0_dp, 4.0_dp]
      real(kind=dp), dimension(3), parameter :: z = [0.0_dp, 0.0_dp, 0.0_dp]
      real(kind=dp), dimension(3), parameter :: distance = [0.0_dp, 3.0_dp, 7.0_dp]
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp) :: xp, yp, zp
      integer :: inside

      ! Distance s is measured along A -> B -> C:
      !
      !              o (3,5)  s=8: outside the polyline
      !              :
      !              C (3,4)  s=7: endpoint
      !              |
      !              P (3,2)  s=5: interior point
      !              |
      ! (0,0) A------B (3,0)  s=3
      !       s=0

      call interpolateOnPolyline(x, y, z, distance, size(x), xp, yp, zp, 5.0_dp, inside)

      call f90_expect_eq(inside, 1, cstr("Distance 5 should be inside the polyline"))
      call f90_expect_near(xp, 3.0_dp, tolerance, cstr("Interior point should have x=3"))
      call f90_expect_near(yp, 2.0_dp, tolerance, cstr("Interior point should have y=2"))

      call interpolateOnPolyline(x, y, z, distance, size(x), xp, yp, zp, 7.0_dp, inside)

      call f90_expect_eq(inside, 1, cstr("Distance 7 should be accepted at the endpoint"))
      call f90_expect_near(xp, 3.0_dp, tolerance, cstr("Endpoint should have x=3"))
      call f90_expect_near(yp, 4.0_dp, tolerance, cstr("Endpoint should have y=4"))

      call interpolateOnPolyline(x, y, z, distance, size(x), xp, yp, zp, 8.0_dp, inside)

      call f90_expect_eq(inside, 0, cstr("Distance 8 is beyond the length-7 polyline and must be rejected"))
   end subroutine test_distance_beyond_end_is_rejected
   !$f90tw)

end module test_interpolate_on_polyline
