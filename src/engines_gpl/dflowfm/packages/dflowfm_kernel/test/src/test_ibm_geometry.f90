module test_ibm_geometry
   use assertions_gtest
   use precision, only: dp
   use m_immersed_boundary, only: ibm_build_geometry, ibm_measure_wet_polygon, ibm_validate_cutcelllist, &
                                  ibm_set_perot_weights, ibm_set_wet_link_metrics, ibm_set_wet_bed, &
                                  ibm_wet_area_from_polygon, t_ibm_cut_cell, t_ibm_state
   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_half_plane_cut_metrics, test_half_plane_cut_metrics,
   subroutine test_half_plane_cut_metrics() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(3) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 10.0_dp, 0.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 50.0_dp, 0.0_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'Half-plane cut geometry was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.5_dp, 1.0e-14_dp, 'Unexpected half-plane area fraction.')
      call F90_ASSERT_NEAR(cell%xc, 10.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected wet centroid x coordinate.')
      call F90_ASSERT_NEAR(cell%yc, 10.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected wet centroid y coordinate.')
      call F90_ASSERT_EQ(cell%nseg, 1, 'Expected one immersed segment.')
      call F90_ASSERT_NEAR(cell%seg_len, sqrt(200.0_dp), 1.0e-13_dp, 'Unexpected immersed segment length.')
      call F90_ASSERT_NEAR(cell%nx, -sqrt(0.5_dp), 1.0e-14_dp, 'Unexpected inward normal x component.')
      call F90_ASSERT_NEAR(cell%ny, -sqrt(0.5_dp), 1.0e-14_dp, 'Unexpected inward normal y component.')
   end subroutine test_half_plane_cut_metrics
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_floor_crosscheck, test_area_floor_crosscheck,
   subroutine test_area_floor_crosscheck() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(4) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y = [9.5_dp, 9.5_dp, 10.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 10.0_dp, 0.1_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'Correctly floored cut-cell area was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.05_dp, 1.0e-14_dp, 'Unexpected unfloored area fraction.')
      call F90_ASSERT_NEAR(cell%ba_used, 10.0_dp, 1.0e-14_dp, 'Unexpected floored area.')

      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 11.0_dp, 0.1_dp, 0.0_dp, 1.0e-7_dp, cell)
      call F90_ASSERT_TRUE(.not. cell%valid, 'Mismatched cutcelwu area was accepted.')
      call F90_ASSERT_TRUE(index(cell%reason, 'cutcelwu') > 0, 'Area cross-check failure did not identify cutcelwu.')
   end subroutine test_area_floor_crosscheck
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_independent_polygon_cell_area, test_independent_polygon_cell_area,
   subroutine test_independent_polygon_cell_area() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y, polygon_x, polygon_y
      real(kind=dp) :: area
      logical :: valid

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      polygon_x = [0.0_dp, 5.0_dp, 5.0_dp, 0.0_dp]
      polygon_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      call ibm_wet_area_from_polygon(cell_x, cell_y, polygon_x, polygon_y, 1.0e-8_dp, area, valid)

      call F90_ASSERT_TRUE(valid, 'Independent polygon clipping rejected a valid rectangle.')
      call F90_ASSERT_NEAR(area, 50.0_dp, 1.0e-13_dp, 'Independent polygon clipping returned the wrong area.')
   end subroutine test_independent_polygon_cell_area
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_classification_uses_geometric_tolerance, test_area_classification_uses_geometric_tolerance,
   subroutine test_area_classification_uses_geometric_tolerance() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(2) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 0) :: link_cells
      real(kind=dp), dimension(2, 0) :: face_x, face_y
      real(kind=dp), dimension(0) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_corner_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 0.005_dp, 0.005_dp]
      wet_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      wet_y(:, 2) = [0.0_dp, 0.0_dp, 9.995_dp, 9.995_dp]
      wet_count = [4, 4]
      area_full = [100.0_dp, 100.0_dp]
      area_used = [0.05_dp, 99.95_dp]

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-3_dp, &
                              0.5_dp, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Sliver and near-full wet cells were rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 2, 'Area classification used the Cartesian tolerance.')
      call F90_ASSERT_EQ(state%cell_class(1), 1, 'Sliver wet cell was classified as solid.')
      call F90_ASSERT_EQ(state%cell_class(2), 1, 'Near-full wet cell was classified as uncut.')
   end subroutine test_area_classification_uses_geometric_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_geometry_state_and_wet_edge, test_geometry_state_and_wet_edge,
   subroutine test_geometry_state_and_wet_edge() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_corner_x, cell_corner_y
      real(kind=dp), dimension(5, 2) :: wet_x, wet_y
      integer, dimension(2) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 1) :: link_cells
      real(kind=dp), dimension(2, 1) :: face_x, face_y
      real(kind=dp), dimension(1) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_corner_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp, 0.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 5.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_count = [5, 4]
      area_full = [100.0_dp, 100.0_dp]
      area_used = [87.5_dp, 100.0_dp]
      link_cells(:, 1) = [1, 2]
      face_x(:, 1) = [10.0_dp, 10.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      width_full = [10.0_dp]
      width_used = [5.0_dp]

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Consistent cut-cell geometry was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 1, 'Unexpected number of cut cells.')
      call F90_ASSERT_EQ(state%cell_class(1), 1, 'Cut cell was not classified as cut.')
      call F90_ASSERT_EQ(state%cell_class(2), 0, 'Uncut cell was not classified as fluid.')
      call F90_ASSERT_EQ(size(state%clink), 1, 'Unexpected number of cut links.')
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.5_dp, 1.0e-14_dp, 'Unexpected wet-edge fraction.')
      call F90_ASSERT_NEAR(state%clink(1)%xm, 10.0_dp, 1.0e-14_dp, 'Unexpected wet-edge midpoint x coordinate.')
      call F90_ASSERT_NEAR(state%clink(1)%ym, 2.5_dp, 1.0e-14_dp, 'Unexpected wet-edge midpoint y coordinate.')
   end subroutine test_geometry_state_and_wet_edge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_closed_link_next_to_solid_cell, test_closed_link_next_to_solid_cell,
   subroutine test_closed_link_next_to_solid_cell() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(2) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 1) :: link_cells
      real(kind=dp), dimension(2, 1) :: face_x, face_y
      real(kind=dp), dimension(1) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_corner_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [5.0_dp, 10.0_dp, 10.0_dp, 5.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 2) = 0.0_dp
      wet_y(:, 2) = 0.0_dp
      wet_count = [4, 0]
      area_full = [100.0_dp, 100.0_dp]
      area_used = [50.0_dp, 0.0_dp]
      link_cells(:, 1) = [1, 2]
      face_x(:, 1) = [10.0_dp, 10.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      width_full = [10.0_dp]
      width_used = [0.0_dp]

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Closed link next to a solid cell was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%cell_class(1), 1, 'Expected one cut cell.')
      call F90_ASSERT_EQ(state%cell_class(2), 2, 'Expected one solid cell.')
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.0_dp, 1.0e-14_dp, 'Solid-adjacent link should be closed.')
   end subroutine test_closed_link_next_to_solid_cell
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_open_boundary_link_keeps_legacy_metrics, test_open_boundary_link_keeps_legacy_metrics,
   subroutine test_open_boundary_link_keeps_legacy_metrics() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 1) :: link_cells
      real(kind=dp), dimension(2, 1) :: face_x, face_y
      real(kind=dp), dimension(1) :: width_full, width_used, cell_x, cell_y, normal_x, normal_y, acl_full, dxi_full
      real(kind=dp), dimension(2) :: bed_level
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 5.0_dp, 5.0_dp]
      wet_count = [4]
      area_full = [100.0_dp, 0.0_dp]
      area_used = [50.0_dp, 0.0_dp]
      link_cells(:, 1) = [1, 2]
      face_x(:, 1) = [10.0_dp, 10.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      width_full = [10.0_dp]
      width_used = [5.0_dp]

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Open-boundary cut geometry was rejected: '//trim(reason))

      state%grid%dx = 10.0_dp
      state%grid%dy = 10.0_dp
      cell_x = [5.0_dp]
      cell_y = [5.0_dp]
      normal_x = [1.0_dp]
      normal_y = [0.0_dp]
      acl_full = [0.5_dp]
      dxi_full = [0.1_dp]
      call ibm_set_wet_link_metrics(link_cells, [5.0_dp, 15.0_dp], [5.0_dp, 5.0_dp], normal_x, normal_y, &
                                    width_used, acl_full, dxi_full, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Open-boundary IBM metrics were rejected: '//trim(reason))
      call F90_ASSERT_NEAR(state%clink(1)%acl_wet, acl_full(1), 1.0e-14_dp, 'Open-boundary ACL was overridden.')
      call F90_ASSERT_NEAR(state%clink(1)%dxi_wet, dxi_full(1), 1.0e-14_dp, 'Open-boundary DXI was overridden.')

      bed_level = [7.0_dp, -999.0_dp]
      call ibm_set_wet_bed(link_cells, width_used, [5.0_dp, 15.0_dp], [5.0_dp, 5.0_dp], -999.0_dp, bed_level, &
                           state, valid, error_index, reason, owned_cell=[.false.])
      call F90_ASSERT_TRUE(valid, 'Unowned IBM ghost cut cell was fitted: '//trim(reason))
      call F90_ASSERT_NEAR(bed_level(1), 7.0_dp, 1.0e-14_dp, 'Unowned ghost-cell bed level was modified.')
   end subroutine test_open_boundary_link_keeps_legacy_metrics
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_cutcelllist_validation, test_cutcelllist_validation,
   subroutine test_cutcelllist_validation() bind(C)
      character(len=*), parameter :: list_file = 'ibm_geometry_check.lst'
      character(len=*), parameter :: empty_list_file = 'ibm_geometry_empty.lst'
      character(len=*), parameter :: polygon_file = 'ibm_geometry_check.pol'
      character(len=256) :: reason
      logical :: valid
      integer :: unit

      open (newunit=unit, file=empty_list_file, status='replace', action='write')
      close (unit)
      call ibm_validate_cutcelllist(empty_list_file, valid, reason)
      call F90_ASSERT_TRUE(.not. valid, 'Empty Cutcelllist was accepted.')
      call F90_ASSERT_TRUE(index(reason, 'at least one polygon') > 0, 'Empty-list failure did not identify the missing polygon.')
      open (newunit=unit, file=empty_list_file, status='old')
      close (unit, status='delete')

      open (newunit=unit, file=polygon_file, status='replace', action='write')
      write (unit, '(a)') 'polygon test data'
      close (unit)
      open (newunit=unit, file=list_file, status='replace', action='write')
      write (unit, '(a)') polygon_file
      close (unit)
      call ibm_validate_cutcelllist(list_file, valid, reason)
      call F90_ASSERT_TRUE(valid, 'Valid Cutcelllist was rejected: '//trim(reason))
      open (newunit=unit, file=list_file, status='old')
      close (unit, status='delete')
      open (newunit=unit, file=polygon_file, status='old')
      close (unit, status='delete')
   end subroutine test_cutcelllist_validation
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_angle_43_80_geometry, test_angle_43_80_geometry,
   subroutine test_angle_43_80_geometry() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(3) :: wet_x, wet_y
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x_matrix, wet_y_matrix
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      type(t_ibm_cut_cell) :: cell
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason
      real(kind=dp) :: normal_length

      cell_x = [0.0_dp, 80.0_dp, 80.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 43.0_dp, 43.0_dp]
      wet_x = [0.0_dp, 80.0_dp, 80.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 43.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 3440.0_dp, 1720.0_dp, 0.0_dp, 0.0_dp, &
                                   1.0e-7_dp, cell)
      call F90_ASSERT_TRUE(cell%valid, 'The 43/80 triangle cut was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.5_dp, 1.0e-14_dp, 'Unexpected 43/80 triangle area fraction.')
      normal_length = sqrt(43.0_dp**2 + 80.0_dp**2)
      call F90_ASSERT_NEAR(cell%nx, 43.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 triangle normal x.')
      call F90_ASSERT_NEAR(cell%ny, -80.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 triangle normal y.')

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 1) = cell_x
      cell_corner_y(:, 1) = cell_y
      wet_x_matrix(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y_matrix(:, 1) = [0.0_dp, 0.0_dp, 7.375_dp, 2.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x_matrix(:, 1), wet_y_matrix(:, 1), 100.0_dp, 46.875_dp, &
                                   0.0_dp, 0.0_dp, 1.0e-7_dp, cell)
      call F90_ASSERT_TRUE(cell%valid, 'The 43/80 trapezoid cut was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.46875_dp, 1.0e-14_dp, 'Unexpected 43/80 trapezoid area fraction.')
      call F90_ASSERT_EQ(cell%nseg, 1, 'Expected one immersed trapezoid segment.')
      normal_length = sqrt(8249.0_dp)
      call F90_ASSERT_NEAR(cell%nx, 43.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 trapezoid normal x.')
      call F90_ASSERT_NEAR(cell%ny, -80.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 trapezoid normal y.')

      wet_count = 4
      area_full = 100.0_dp
      area_used = 46.875_dp
      link_cells = 0
      link_cells(1, :) = 1
      face_x(:, 1) = [0.0_dp, 0.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      face_x(:, 2) = [10.0_dp, 10.0_dp]
      face_y(:, 2) = [0.0_dp, 10.0_dp]
      face_x(:, 3) = [0.0_dp, 10.0_dp]
      face_y(:, 3) = [0.0_dp, 0.0_dp]
      face_x(:, 4) = [10.0_dp, 0.0_dp]
      face_y(:, 4) = [10.0_dp, 10.0_dp]
      width_full = 10.0_dp
      width_used = [2.0_dp, 7.375_dp, 10.0_dp, 0.0_dp]
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x_matrix, wet_y_matrix, wet_count, area_full, area_used, &
                              link_cells, face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, &
                              1.0e-6_dp, 0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Consistent 43/80 wet-face geometry was rejected: '//trim(reason))
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.2_dp, 1.0e-14_dp, 'Unexpected left-face fraction.')
      call F90_ASSERT_NEAR(state%edge_frac(2), 0.7375_dp, 1.0e-14_dp, 'Unexpected right-face fraction.')
      call F90_ASSERT_NEAR(state%edge_frac(3), 1.0_dp, 1.0e-14_dp, 'Unexpected bottom-face fraction.')
      call F90_ASSERT_NEAR(state%edge_frac(4), 0.0_dp, 1.0e-14_dp, 'Unexpected top-face fraction.')
   end subroutine test_angle_43_80_geometry
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_cut_through_net_node, test_cut_through_net_node,
   subroutine test_cut_through_net_node() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y
      real(kind=dp), dimension(3, 1) :: wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_count = 3
      area_full = 100.0_dp
      area_used = 50.0_dp
      link_cells = 0
      link_cells(1, :) = 1
      face_x(:, 1) = [0.0_dp, 0.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      face_x(:, 2) = [10.0_dp, 10.0_dp]
      face_y(:, 2) = [0.0_dp, 10.0_dp]
      face_x(:, 3) = [0.0_dp, 10.0_dp]
      face_y(:, 3) = [0.0_dp, 0.0_dp]
      face_x(:, 4) = [10.0_dp, 0.0_dp]
      face_y(:, 4) = [10.0_dp, 10.0_dp]
      width_full = 10.0_dp
      width_used = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'A cut through the net node was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 1, 'Expected one cut cell at the net node.')
      call F90_ASSERT_EQ(state%cut(1)%nseg, 1, 'Expected one nonzero immersed segment.')
      call F90_ASSERT_NEAR(state%cut(1)%seg_len, sqrt(200.0_dp), 1.0e-14_dp, 'Unexpected net-node segment length.')
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.0_dp, 1.0e-14_dp, 'Expected a dry left face.')
      call F90_ASSERT_NEAR(state%edge_frac(2), 1.0_dp, 1.0e-14_dp, 'Expected a fully wet right face.')
      call F90_ASSERT_NEAR(state%edge_frac(3), 1.0_dp, 1.0e-14_dp, 'Expected a fully wet bottom face.')
      call F90_ASSERT_NEAR(state%edge_frac(4), 0.0_dp, 1.0e-14_dp, 'Expected a dry top face.')
   end subroutine test_cut_through_net_node
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_face_aligned_fluid_cell, test_face_aligned_fluid_cell,
   subroutine test_face_aligned_fluid_cell() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = cell_corner_x(:, 1)
      wet_y(:, 1) = cell_corner_y(:, 1)
      wet_count = 4
      area_full = 100.0_dp
      area_used = 100.0_dp
      link_cells = 0
      link_cells(1, :) = 1
      face_x(:, 1) = [0.0_dp, 0.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      face_x(:, 2) = [10.0_dp, 10.0_dp]
      face_y(:, 2) = [0.0_dp, 10.0_dp]
      face_x(:, 3) = [0.0_dp, 10.0_dp]
      face_y(:, 3) = [0.0_dp, 0.0_dp]
      face_x(:, 4) = [10.0_dp, 0.0_dp]
      face_y(:, 4) = [10.0_dp, 10.0_dp]
      width_full = 10.0_dp
      width_used = 10.0_dp
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'A face-aligned full cell was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 0, 'A face-aligned boundary created a cut cell.')
      call F90_ASSERT_EQ(state%cell_class(1), 0, 'The face-aligned full cell was not classified as fluid.')
      call F90_ASSERT_EQ(size(state%clink), 0, 'A full cell created an immersed cut link.')
   end subroutine test_face_aligned_fluid_cell
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_two_segment_corner, test_two_segment_corner,
   subroutine test_two_segment_corner() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(5) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp, 0.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 10.0_dp, 5.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 75.0_dp, 0.0_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'A right-angle corner cut was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.75_dp, 1.0e-14_dp, 'Unexpected right-angle corner area.')
      call F90_ASSERT_EQ(cell%nseg, 2, 'Expected two immersed segments at the corner.')
      call F90_ASSERT_NEAR(cell%nx, 0.0_dp, 1.0e-14_dp, 'Unexpected corner-cut normal x component.')
      call F90_ASSERT_NEAR(cell%ny, -1.0_dp, 1.0e-14_dp, 'Unexpected corner-cut normal y component.')
   end subroutine test_two_segment_corner
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_smooth_multivertex_segment, test_smooth_multivertex_segment,
   subroutine test_smooth_multivertex_segment() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(8) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 2.0_dp, 2.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 2.0_dp, 2.0_dp]
      wet_x = [0.0_dp, 2.0_dp, 2.0_dp, 0.0_dp, 0.1_dp, 1.0_dp, 1.5_dp, 1.7_dp]
      wet_y = [0.0_dp, 0.0_dp, 2.0_dp, 2.0_dp, 1.9_dp, 0.9_dp, 0.3_dp, 0.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 4.0_dp, 2.215_dp, 0.1_dp, 1.0e-6_dp, &
                                   1.0e-8_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'A polygonized smooth wall was rejected.')
      call F90_ASSERT_EQ(cell%nseg, 1, 'Nearly collinear wall edges were not coalesced.')
   end subroutine test_smooth_multivertex_segment
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_split_cell_rejected, test_split_cell_rejected,
   subroutine test_split_cell_rejected() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(8) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 4.0_dp, 4.0_dp, 6.0_dp, 6.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 3.0_dp, 3.0_dp, 0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 94.0_dp, 0.0_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(.not. cell%valid, 'A cell with a three-segment solid spit was accepted.')
      call F90_ASSERT_TRUE(index(cell%reason, 'more than two') > 0, 'Split-cell failure did not identify excess segments.')
   end subroutine test_split_cell_rejected
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_conservation, test_area_conservation,
   subroutine test_area_conservation() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_x, cell_y
      real(kind=dp), dimension(3, 2) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell
      real(kind=dp) :: total_area
      integer :: k

      cell_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_x(:, 2) = [10.0_dp, 20.0_dp, 10.0_dp]
      wet_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp]

      total_area = 0.0_dp
      do k = 1, 2
         call ibm_measure_wet_polygon(cell_x(:, k), cell_y(:, k), wet_x(:, k), wet_y(:, k), 100.0_dp, 50.0_dp, &
                                      0.0_dp, 0.0_dp, 1.0e-7_dp, cell)
         call F90_ASSERT_TRUE(cell%valid, 'A triangle in the closed two-cell polygon was rejected.')
         total_area = total_area + cell%area_frac * 100.0_dp
      end do
      call F90_ASSERT_NEAR(total_area, 100.0_dp, 1.0e-12_dp, 'Wet areas do not conserve the closed triangular polygon area.')
   end subroutine test_area_conservation
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_floor_accounting, test_area_floor_accounting,
   subroutine test_area_floor_accounting() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 0) :: link_cells
      real(kind=dp), dimension(2, 0) :: face_x, face_y
      real(kind=dp), dimension(0) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y(:, 1) = [9.5_dp, 9.5_dp, 10.0_dp, 10.0_dp]
      wet_count = 4
      area_full = 100.0_dp
      area_used = 10.0_dp

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.1_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Area-floor geometry was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%nfloored, 1, 'Expected one floored cell.')
      call F90_ASSERT_NEAR(state%area_deficit, 5.0_dp, 1.0e-14_dp, 'Unexpected area-floor deficit.')
   end subroutine test_area_floor_accounting
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_perot_tangential_constant, test_perot_tangential_constant,
   subroutine test_perot_tangential_constant() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y
      real(kind=dp), dimension(3, 1) :: wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      real(kind=dp), dimension(4) :: wcx1, wcy1, wcx2, wcy2, normal_velocity
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason
      real(kind=dp) :: ucx, ucy

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_count = 3
      area_full = 100.0_dp
      area_used = 50.0_dp
      link_cells = 0
      link_cells(1, :) = 1
      face_x(:, 1) = [0.0_dp, 0.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      face_x(:, 2) = [10.0_dp, 10.0_dp]
      face_y(:, 2) = [0.0_dp, 10.0_dp]
      face_x(:, 3) = [0.0_dp, 10.0_dp]
      face_y(:, 3) = [0.0_dp, 0.0_dp]
      face_x(:, 4) = [10.0_dp, 0.0_dp]
      face_y(:, 4) = [10.0_dp, 10.0_dp]
      width_full = 10.0_dp
      width_used = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Perot test geometry was rejected: '//trim(reason))
      state%ba_full = area_full

      wcx1 = 0.0_dp
      wcy1 = 0.0_dp
      wcx2 = 0.0_dp
      wcy2 = 0.0_dp
      call ibm_set_perot_weights(link_cells, width_used, wcx1, wcy1, wcx2, wcy2, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Wet-polygon Perot weights were rejected: '//trim(reason))

      normal_velocity = [-1.0_dp, 1.0_dp, -1.0_dp, 1.0_dp]
      ucx = sum(wcx1 * normal_velocity)
      ucy = sum(wcy1 * normal_velocity)
      call F90_ASSERT_NEAR(ucx, 1.0_dp, 1.0e-13_dp, 'Tangential Perot reconstruction did not preserve constant x velocity.')
      call F90_ASSERT_NEAR(ucy, 1.0_dp, 1.0e-13_dp, 'Tangential Perot reconstruction did not preserve constant y velocity.')

      normal_velocity = [-1.0_dp, 1.0_dp, 0.0_dp, 0.0_dp]
      ucx = sum(wcx1 * normal_velocity)
      ucy = sum(wcy1 * normal_velocity)
      call F90_ASSERT_NEAR(ucx, 2.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected normal-flow Perot x component.')
      call F90_ASSERT_NEAR(ucy, 1.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected normal-flow Perot y component.')
   end subroutine test_perot_tangential_constant
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_wet_link_metrics, test_wet_link_metrics,
   subroutine test_wet_link_metrics() bind(C)
      integer, dimension(2, 2) :: link_cells
      real(kind=dp), dimension(3) :: cell_x, cell_y
      real(kind=dp), dimension(2) :: normal_x, normal_y, width_used, acl_full, dxi_full
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason
      real(kind=dp) :: zeta1, zeta2, dn

      state%grid%dx = 1.0_dp
      state%grid%dy = 1.0_dp
      allocate (state%cell_class(3), state%cut(1), state%clink(1))
      state%cell_class = [1, 0, 0]
      state%ncut = 1
      state%cut(1)%k = 1
      state%cut(1)%xc = 0.5_dp
      state%cut(1)%yc = 0.5_dp
      state%clink(1)%L = 1
      state%clink(1)%xm = 1.0_dp
      state%clink(1)%ym = 0.5_dp

      link_cells(:, 1) = [1, 2]
      link_cells(:, 2) = [2, 3]
      cell_x = [0.5_dp, 1.5_dp, 2.5_dp]
      cell_y = [0.5_dp, 0.5_dp, 0.5_dp]
      normal_x = 1.0_dp
      normal_y = 0.0_dp
      width_used = 1.0_dp
      acl_full = 0.5_dp
      dxi_full = 1.0_dp
      call ibm_set_wet_link_metrics(link_cells, cell_x, cell_y, normal_x, normal_y, width_used, acl_full, dxi_full, &
                                    state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Wet-link metrics were rejected: '//trim(reason))
      if (valid) then
         dn = 1.0_dp / state%clink(1)%dxi_wet
         zeta1 = 2.0_dp * state%cut(1)%xc + 4.0_dp * state%cut(1)%yc
         zeta2 = 2.0_dp * cell_x(2) + 4.0_dp * cell_y(2)
         call F90_ASSERT_NEAR((zeta2 - zeta1) / dn, 2.0_dp, 1.0e-14_dp, &
                              'Wet-centroid pressure gradient was not exact for a normal linear field.')
      end if

      state%cut(1)%xc = 0.95_dp
      cell_x(2) = 1.05_dp
      cell_y(2) = 0.7_dp
      call ibm_set_wet_link_metrics(link_cells, cell_x, cell_y, normal_x, normal_y, width_used, acl_full, dxi_full, &
                                    state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Sliver wet-link metrics were rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_NEAR(state%clink(1)%dxi_wet, 4.0_dp, 1.0e-14_dp, 'Unexpected clipped wet-centroid inverse distance.')
         call F90_ASSERT_NEAR(state%clink(1)%acl_wet, 0.5_dp, 1.0e-14_dp, 'Unexpected wet-face interpolation weight.')
         call F90_ASSERT_NEAR(state%clink(1)%dt12, 0.2_dp, 1.0e-14_dp, 'Unexpected tangential centroid offset.')
         call F90_ASSERT_EQ(state%nclipped_dn, 1, 'Expected one clipped wet-centroid distance.')
         call F90_ASSERT_EQ(state%cut_link_index(1), 1, 'Cut-link lookup was not populated.')
         call F90_ASSERT_EQ(state%cut_link_index(2), 0, 'Non-cut link was added to the cut-link lookup.')
      end if
   end subroutine test_wet_link_metrics
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_wet_bed_planar_fit, test_wet_bed_planar_fit,
   subroutine test_wet_bed_planar_fit() bind(C)
      integer, dimension(2, 3) :: link_cells
      real(kind=dp), dimension(4) :: cell_x, cell_y, bed_level
      real(kind=dp), dimension(3) :: width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      state%grid%dx = 1.0_dp
      state%grid%dy = 1.0_dp
      allocate (state%cell_class(4), state%cut(1))
      state%cell_class = [1, 0, 0, 0]
      state%ncut = 1
      state%cut(1)%k = 1
      state%cut(1)%xc = 0.25_dp
      state%cut(1)%yc = 0.5_dp

      link_cells(:, 1) = [1, 2]
      link_cells(:, 2) = [1, 3]
      link_cells(:, 3) = [1, 4]
      cell_x = [0.5_dp, 1.5_dp, -0.5_dp, 0.5_dp]
      cell_y = [0.5_dp, 0.5_dp, 0.5_dp, 1.5_dp]
      width_used = 1.0_dp
      bed_level = [10.0_dp, 4.5_dp, -1.5_dp, -2.5_dp]
      call ibm_set_wet_bed(link_cells, width_used, cell_x, cell_y, -9999.0_dp, bed_level, state, &
                           valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Wet-bed planar fit was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_NEAR(bed_level(1), 0.75_dp, 1.0e-13_dp, 'Wet-bed fit did not recover the planar value at the centroid.')
      end if
   end subroutine test_wet_bed_planar_fit
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_wet_bed_boundary_fallback, test_wet_bed_boundary_fallback,
   subroutine test_wet_bed_boundary_fallback() bind(C)
      integer, dimension(2, 5) :: link_cells
      real(kind=dp), dimension(6) :: cell_x, cell_y, bed_level
      real(kind=dp), dimension(5) :: width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      state%grid%dx = 1.0_dp
      state%grid%dy = 1.0_dp
      allocate (state%cell_class(6), state%cut(3))
      state%cell_class = [1, 1, 1, 0, 0, 0]
      state%ncut = 3
      state%cut(1)%k = 1
      state%cut(1)%xc = 0.25_dp
      state%cut(1)%yc = 0.5_dp
      state%cut(2)%k = 2
      state%cut(2)%xc = 1.5_dp
      state%cut(2)%yc = 0.5_dp
      state%cut(3)%k = 3
      state%cut(3)%xc = 2.5_dp
      state%cut(3)%yc = 0.5_dp

      link_cells(:, 1) = [1, 2]
      link_cells(:, 2) = [2, 3]
      link_cells(:, 3) = [3, 4]
      link_cells(:, 4) = [3, 5]
      link_cells(:, 5) = [3, 6]
      width_used = 1.0_dp
      cell_x = [0.5_dp, 1.5_dp, 2.5_dp, 3.5_dp, 2.5_dp, 3.5_dp]
      cell_y = [0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 1.5_dp, 1.5_dp]
      bed_level = [100.0_dp, 101.0_dp, 102.0_dp, 10.5_dp, 3.5_dp, 6.5_dp]
      call ibm_set_wet_bed(link_cells, width_used, cell_x, cell_y, -9999.0_dp, bed_level, state, &
                           valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Wet-bed boundary fallback was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_NEAR(bed_level(1), 0.75_dp, 1.0e-13_dp, 'Fallback fit did not recover the planar value.')
      end if
   end subroutine test_wet_bed_boundary_fallback
   !$f90tw)

end module test_ibm_geometry
