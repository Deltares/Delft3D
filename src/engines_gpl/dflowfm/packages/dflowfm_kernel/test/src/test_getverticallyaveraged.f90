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
   public :: test_structure_density_2d, test_structure_density_stratified_column

contains

   !$gtest
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

   !$gtest
   !> The general-structure density in 2D is the adjacent cell's absolute density.
   subroutine test_structure_density_2d() bind(C)
      use m_flow, only: potential_density, rho
      use m_furu_structures, only: depth_averaged_density

      kmx = 0
      call realloc(potential_density, 1, fill=1025.0_dp)
      rho => potential_density
      call f90_expect_near(depth_averaged_density(1), 1025.0_dp, 1.0e-12_dp, cstr('Use the 2D cell density'))
      nullify(rho)
      deallocate(potential_density)
   end subroutine test_structure_density_2d

   !$gtest
   !> Use thickness weighting, not a layer-count average, and ignore inactive layers.
   subroutine test_structure_density_stratified_column() bind(C)
      use m_flow, only: potential_density, rho, kbot, ktop, zws
      use m_physcoef, only: rhomean
      use m_furu_structures, only: depth_averaged_density

      kmx = 3
      call realloc(potential_density, 3)
      potential_density = [1030.0_dp, 1010.0_dp, 1000.0_dp]
      rho => potential_density
      call realloc(kbot, 1, fill=1)
      call realloc(ktop, 1, fill=3)
      allocate(zws(0:3))
      zws = [-10.0_dp, -7.0_dp, -2.0_dp, 0.0_dp]
      call f90_expect_near(depth_averaged_density(1), 1014.0_dp, 1.0e-12_dp, cstr('Weight densities by layer thickness'))

      ktop(1) = 2
      call f90_expect_near(depth_averaged_density(1), 1017.5_dp, 1.0e-12_dp, cstr('Exclude inactive layers'))
      zws = 0.0_dp
      call f90_expect_near(depth_averaged_density(1), rhomean, 1.0e-12_dp, cstr('Zero-depth columns use reference density'))
      ktop(1) = 0
      call f90_expect_near(depth_averaged_density(1), rhomean, 1.0e-12_dp, cstr('Dry columns use reference density'))

      nullify(rho)
      deallocate(potential_density, kbot, ktop, zws)
   end subroutine test_structure_density_stratified_column

end module test_getverticallyaveraged
