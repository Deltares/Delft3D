module test_3d_layer_administration
   use assertions_gtest
   implicit none

contains

   !$f90tw TESTCODE(TEST, tests_3d_layer_administration, test_layer_height_water_level_consistency, test_layer_height_water_level_consistency,
   subroutine test_layer_height_water_level_consistency() bind(C)
      use precision, only: dp
      use m_sets01zbnd, only: sets01zbnd
      use m_flow, only: zws, s0, hs, kmx, kmxn, layertype, LAYTP_SIGMA, vol1, kbot, ktop, zslay
      use m_flowgeom, only: bl
      use m_cell_geometry, only: ndx, ba
      use fm_external_forcings_data, only: nbndz, kbndz, zbndz
      use m_boundary_condition_type, only: BOUNDARY_WATER_LEVEL
      use m_fm_icecover, only: ice_apply_pressure
      use m_sediment, only: jased
      use m_alloc, only: realloc
      real(kind=dp), parameter :: tolerance = 1e-8_dp
      integer, parameter :: number_of_boundary_points = 1
      integer, parameter :: number_of_layers = 1
      real(kind=dp), parameter :: new_water_level = 19.92_dp
      logical, target :: ice_apply_pressure_target
      real(kind=dp), parameter :: normalized_layer_thickness = 1.0_dp
      real(kind=dp), parameter :: bottom_area = 1.0_dp

      ! Test set-up
      call realloc(zws, number_of_boundary_points * (number_of_layers + 1), fill=0.0_dp)
      call realloc(s0, number_of_boundary_points, fill=0.0_dp)
      call realloc(kbndz, [6, number_of_boundary_points], fill=0)
      call realloc(bl, number_of_boundary_points, fill=0.0_dp)
      call realloc(hs, number_of_boundary_points, fill=0.0_dp)
      call realloc(zbndz, number_of_boundary_points, fill=0.0_dp)
      call realloc(vol1, number_of_boundary_points * (number_of_layers + 1), fill=0.0_dp)
      call realloc(kbot, number_of_boundary_points, fill=0)
      call realloc(ktop, number_of_boundary_points, fill=0)
      call realloc(kmxn, number_of_boundary_points, fill=0)
      call realloc(zslay, [number_of_boundary_points, 1], fill=0.0_dp)
      call realloc(ba, number_of_boundary_points, fill=0.0_dp)
      kmx = number_of_layers
      kmxn(1) = number_of_layers
      layertype = LAYTP_SIGMA
      ndx = number_of_boundary_points
      nbndz = number_of_boundary_points
      kbndz(1, 1) = 1
      zslay(1, 1) = normalized_layer_thickness
      ba(1) = bottom_area
      kbot(1) = 2
      ktop(1) = 2
      kbndz(4, 1) = BOUNDARY_WATER_LEVEL
      zbndz(1) = new_water_level
      ice_apply_pressure_target = .false.
      ice_apply_pressure => ice_apply_pressure_target
      jased = 0

      ! Run the code under test
      call sets01zbnd(n01=0, jasetBlDepth=0)

      ! Check results
      call f90_expect_near(hs, s0, tolerance, "water depth is not consistent with water level")
      call f90_expect_near(vol1, &
                           [(bottom_area*normalized_layer_thickness*new_water_level, integer :: i=1, number_of_boundary_points * (number_of_layers + 1))], &
                           tolerance, "cell volume is not consistent with water level")
      call f90_expect_near(zws(2), s0(1), tolerance, "top layer interface is not consistent with water level")

   end subroutine test_layer_height_water_level_consistency
   !$f90tw)
end module test_3d_layer_administration
