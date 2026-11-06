!> Regression test for depth averaging in a 2D model.
module test_getverticallyaveraged
   use assertions_gtest, only: f90_expect_near
   use precision, only: dp
   use iso_c_utils, only: cstr
   use m_alloc, only: realloc
   use m_flow, only: kmx, ndkx, vol1
   use m_flowgeom, only: ndx
   use m_getverticallyaveraged, only: getverticallyaveraged

   implicit none(type, external)
   private
   public :: test_depth_average_preserves_2d_salinity

contains

   !$f90tw TESTCODE(TEST, tests_getverticallyaveraged, test_depth_average_preserves_2d_salinity, test_depth_average_preserves_2d_salinity,
   !> A single wet 2D cell must retain its salinity when depth averaged.
   subroutine test_depth_average_preserves_2d_salinity() bind(C)
      real(kind=dp), dimension(1) :: salinity

      ndx = 1
      ndkx = ndx
      kmx = 0
      call realloc(vol1, ndkx, fill=2.0_dp)
      salinity = 35.0_dp

      call getverticallyaveraged(salinity, ndkx)

      call f90_expect_near(salinity(1), 35.0_dp, 1.0e-12_dp, cstr("Depth averaging must preserve 2D salinity"))

      deallocate (vol1)
   end subroutine test_depth_average_preserves_2d_salinity
   !$f90tw)

end module test_getverticallyaveraged
