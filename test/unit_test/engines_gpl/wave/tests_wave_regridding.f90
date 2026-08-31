module tests_wave_regridding
   use assertions_gtest, only: f90_expect_near, f90_expect_true
   use m_ec_basic_interpolation, only: tricall
   use m_wave_regrid, only: generate_regrid_weights, generate_triangle_regrid_weights, quadrilateral_bilinear_weights, &
                            triangle_barycentric_weights
   use precision_basics, only: hp
   use swan_flow_grid_maps, only: grid, grid_map, make_grid_map

   implicit none(type, external)

   interface
      subroutine grmap_esmf(i1, f1, n1, f2, mmax, nmax, f2s, f2g)
         use swan_flow_grid_maps, only: grid, grid_map
         integer, intent(in) :: i1
         integer, intent(in) :: n1
         integer, intent(in) :: mmax
         integer, intent(in) :: nmax
         real, dimension(n1), intent(in) :: f1
         real, dimension(mmax, nmax) :: f2
         type(grid_map), intent(in) :: f2s
         type(grid) :: f2g
      end subroutine grmap_esmf
   end interface

contains

   !$f90tw TESTCODE(TEST, wave_regridding, native_make_grid_map, native_make_grid_map,
   subroutine native_make_grid_map() bind(C)
      type(grid) :: provider_grid
      type(grid) :: receiver_grid
      type(grid_map) :: sparse_map

      provider_grid%mmax = 4
      provider_grid%nmax = 1
      provider_grid%npts = 4
      provider_grid%sferic = .false.
      provider_grid%numenclpts = 4
      provider_grid%numenclparts = 1
      allocate(provider_grid%x(4, 1), source=reshape([0.0_hp, 2.0_hp, 2.0_hp, 0.0_hp], [4, 1]))
      allocate(provider_grid%y(4, 1), source=reshape([0.0_hp, 0.0_hp, 2.0_hp, 2.0_hp], [4, 1]))
      allocate(provider_grid%quadrilaterals(4, 1), source=reshape([1, 2, 3, 4], [4, 1]))
      allocate(provider_grid%triangles(3, 0))
      allocate(provider_grid%bndx, source=[0.0_hp, 2.0_hp, 2.0_hp, 0.0_hp])
      allocate(provider_grid%bndy, source=[0.0_hp, 0.0_hp, 2.0_hp, 2.0_hp])
      allocate(provider_grid%numenclptsppart, source=[4])

      receiver_grid%mmax = 2
      receiver_grid%nmax = 2
      receiver_grid%npts = 4
      receiver_grid%sferic = .false.
      allocate(receiver_grid%x(2, 2), source=reshape([0.25_hp, 0.75_hp, 0.25_hp, 0.75_hp], [2, 2]))
      allocate(receiver_grid%y(2, 2), source=reshape([0.25_hp, 0.25_hp, 0.75_hp, 0.75_hp], [2, 2]))
      allocate(receiver_grid%covered(2, 2), source=0)

      sparse_map%msurpnts = 4
      call make_grid_map(1, 1, provider_grid, receiver_grid, sparse_map, .true.)

      call f90_expect_true(sparse_map%grids_linked, 'Native production map must link overlapping grids')
      call f90_expect_true(sparse_map%n_s == 16, 'Native production map must contain four weights per target')
      call f90_expect_true(all(sparse_map%col == [1, 2, 3, 4, 1, 2, 3, 4, &
                                                  1, 2, 3, 4, 1, 2, 3, 4]), &
                           'Native production columns must match ESMF')
      call f90_expect_true(all(sparse_map%row == [1, 1, 1, 1, 2, 2, 2, 2, &
                                                  3, 3, 3, 3, 4, 4, 4, 4]), &
                           'Native production rows must match ESMF')
      call f90_expect_true(all(receiver_grid%covered == 1), &
                           'Native production map must preserve partition coverage ownership')
   end subroutine native_make_grid_map
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, current_cartesian_sparse_map, current_cartesian_sparse_map,
   subroutine current_cartesian_sparse_map() bind(C)
      real, dimension(3) :: source_values
      real, dimension(2, 2) :: target_values
      type(grid_map) :: sparse_map
      type(grid) :: target_grid

      source_values = [10.0, 20.0, 30.0]
      target_values = -99.0
      allocate(target_grid%covered(2, 2), source=0)
      target_grid%covered(1, 1) = 1
      target_grid%covered(2, 2) = 1

      sparse_map%grids_linked = .true.
      sparse_map%sferic = .false.
      sparse_map%n_s = 5
      allocate(sparse_map%col, source=[1, 2, 1, 2, 3])
      allocate(sparse_map%row, source=[1, 1, 4, 4, 4])
      allocate(sparse_map%s, source=[0.25_hp, 0.75_hp, 0.2_hp, 0.3_hp, 0.5_hp])

      call grmap_esmf(1, source_values, 3, target_values, 2, 2, sparse_map, target_grid)

      call f90_expect_near(target_values(1, 1), 17.5, 1.0e-6, 'Cartesian weighted value')
      call f90_expect_near(target_values(2, 2), 23.0, 1.0e-6, 'Cartesian accumulated value')
      call f90_expect_near(target_values(2, 1), -99.0, 1.0e-6, 'Uncovered value is preserved')
      call f90_expect_near(target_values(1, 2), -99.0, 1.0e-6, 'Uncovered value is preserved')

      sparse_map%grids_linked = .false.
      target_values = 42.0
      call grmap_esmf(1, source_values, 3, target_values, 2, 2, sparse_map, target_grid)
      call f90_expect_true(all(target_values == 42.0), 'Unlinked grids must leave the destination unchanged')
   end subroutine current_cartesian_sparse_map
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, current_spherical_sparse_map, current_spherical_sparse_map,
   subroutine current_spherical_sparse_map() bind(C)
      real, dimension(3) :: source_values
      real, dimension(2, 3) :: target_values
      type(grid_map) :: sparse_map
      type(grid) :: target_grid

      source_values = [4.0, 8.0, 12.0]
      target_values = -1.0
      allocate(target_grid%covered(2, 3), source=0)
      target_grid%covered(2, 2) = 2

      sparse_map%grids_linked = .true.
      sparse_map%sferic = .true.
      sparse_map%n_s = 3
      allocate(sparse_map%col, source=[1, 2, 3])
      allocate(sparse_map%row, source=[5, 5, 5])
      allocate(sparse_map%s, source=[0.25_hp, 0.25_hp, 0.5_hp])

      call grmap_esmf(2, source_values, 3, target_values, 2, 3, sparse_map, target_grid)

      call f90_expect_near(target_values(2, 2), 9.0, 1.0e-6, 'Spherical row ordering')
      call f90_expect_near(target_values(1, 2), -1.0, 1.0e-6, 'Other spherical points are preserved')
   end subroutine current_spherical_sparse_map
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, legacy_esmf_triangle_weights, legacy_esmf_triangle_weights,
   subroutine legacy_esmf_triangle_weights() bind(C)
      real(kind=hp), dimension(3) :: source_x
      real(kind=hp), dimension(3) :: source_y
      real(kind=hp), dimension(4) :: target_x
      real(kind=hp), dimension(4) :: target_y
      integer, dimension(3, 1) :: triangles
      integer, dimension(:), allocatable :: columns
      integer, dimension(:), allocatable :: rows
      real(kind=hp), dimension(:), allocatable :: weights
      real(kind=hp), dimension(12) :: expected_weights
      integer :: number_of_weights

      source_x = [0.0_hp, 2.0_hp, 0.0_hp]
      source_y = [0.0_hp, 0.0_hp, 2.0_hp]
      triangles(:, 1) = [1, 2, 3]
      target_x = [0.25_hp, 0.75_hp, 0.75_hp, 0.25_hp]
      target_y = [0.25_hp, 0.25_hp, 0.75_hp, 0.75_hp]
      expected_weights = [0.75_hp, 0.125_hp, 0.125_hp, &
                          0.5_hp, 0.375_hp, 0.125_hp, &
                          0.25_hp, 0.375_hp, 0.375_hp, &
                          0.5_hp, 0.125_hp, 0.375_hp]

      call generate_triangle_regrid_weights(source_x, source_y, triangles, target_x, target_y, &
                                            columns, rows, weights, number_of_weights)

      call f90_expect_true(number_of_weights == 12, 'ESMF fixture must produce three weights per target')
      call f90_expect_true(all(columns == [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]), &
                           'Source indices must match the ESMF fixture')
      call f90_expect_true(all(rows == [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4]), &
                           'Destination indices must match the ESMF fixture')
      call f90_expect_near(weights, expected_weights, 1.0e-14_hp, &
                           'Triangle weights must match the legacy ESMF output')
   end subroutine legacy_esmf_triangle_weights
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, legacy_esmf_cartesian_quad_weights, legacy_esmf_cartesian_quad_weights,
   subroutine legacy_esmf_cartesian_quad_weights() bind(C)
      real(kind=hp), dimension(4) :: source_x
      real(kind=hp), dimension(4) :: source_y
      real(kind=hp), dimension(4) :: target_x
      real(kind=hp), dimension(4) :: target_y
      real(kind=hp), dimension(4, 4) :: expected_weights
      real(kind=hp), dimension(4) :: weights
      integer :: target_index
      logical :: is_inside

      source_x = [0.0_hp, 2.0_hp, 2.0_hp, 0.0_hp]
      source_y = [0.0_hp, 0.0_hp, 2.0_hp, 2.0_hp]
      target_x = [0.25_hp, 0.75_hp, 0.75_hp, 0.25_hp]
      target_y = [0.25_hp, 0.25_hp, 0.75_hp, 0.75_hp]
      expected_weights(:, 1) = [0.765625_hp, 0.109375_hp, 0.015625_hp, 0.109375_hp]
      expected_weights(:, 2) = [0.546875_hp, 0.328125_hp, 0.046875_hp, 0.078125_hp]
      expected_weights(:, 3) = [0.390625_hp, 0.234375_hp, 0.140625_hp, 0.234375_hp]
      expected_weights(:, 4) = [0.546875_hp, 0.078125_hp, 0.046875_hp, 0.328125_hp]

      do target_index = 1, size(target_x)
         call quadrilateral_bilinear_weights(source_x, source_y, target_x(target_index), target_y(target_index), &
                                             weights, is_inside, .false.)
         call f90_expect_true(is_inside, 'ESMF Cartesian quad target must be inside')
         call f90_expect_near(weights, expected_weights(:, target_index), 1.0e-14_hp, &
                              'Cartesian quad weights must match legacy ESMF')
      end do
   end subroutine legacy_esmf_cartesian_quad_weights
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, legacy_esmf_multiple_triangles, legacy_esmf_multiple_triangles,
   subroutine legacy_esmf_multiple_triangles() bind(C)
      real(kind=hp), dimension(4) :: source_x
      real(kind=hp), dimension(4) :: source_y
      real(kind=hp), dimension(5) :: target_x
      real(kind=hp), dimension(5) :: target_y
      integer, dimension(3, 2) :: triangles
      integer, dimension(:), allocatable :: columns
      integer, dimension(:), allocatable :: rows
      real(kind=hp), dimension(:), allocatable :: weights
      real(kind=hp), dimension(12) :: expected_weights
      integer :: number_of_weights

      source_x = [0.0_hp, 2.0_hp, 2.0_hp, 0.0_hp]
      source_y = [0.0_hp, 0.0_hp, 2.0_hp, 2.0_hp]
      triangles(:, 1) = [1, 2, 3]
      triangles(:, 2) = [1, 3, 4]
      target_x = [0.5_hp, 1.5_hp, 1.5_hp, 0.5_hp, 3.0_hp]
      target_y = [0.5_hp, 0.5_hp, 1.5_hp, 1.5_hp, 3.0_hp]
      expected_weights = [0.75_hp, 0.0_hp, 0.25_hp, &
                          0.25_hp, 0.5_hp, 0.25_hp, &
                          0.25_hp, 0.0_hp, 0.75_hp, &
                          0.25_hp, 0.25_hp, 0.5_hp]

      call generate_triangle_regrid_weights(source_x, source_y, triangles, target_x, target_y, &
                                            columns, rows, weights, number_of_weights)

      call f90_expect_true(number_of_weights == 12, 'The outside target must remain unmapped')
      call f90_expect_true(all(columns == [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 3, 4]), &
                           'Multiple-triangle source indices must match ESMF')
      call f90_expect_true(all(rows == [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4]), &
                           'Multiple-triangle destination indices must match ESMF')
      call f90_expect_near(weights, expected_weights, 1.0e-14_hp, &
                           'Multiple-triangle weights must match legacy ESMF')
   end subroutine legacy_esmf_multiple_triangles
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, legacy_esmf_spherical_quad_weights, legacy_esmf_spherical_quad_weights,
   subroutine legacy_esmf_spherical_quad_weights() bind(C)
      real(kind=hp), dimension(4) :: source_longitude
      real(kind=hp), dimension(4) :: source_latitude
      real(kind=hp), dimension(4) :: weights
      real(kind=hp), dimension(4) :: expected_weights
      logical :: is_inside

      source_longitude = [-2.0_hp, 0.0_hp, 0.0_hp, -2.0_hp]
      source_latitude = [-2.0_hp, -2.0_hp, 0.0_hp, 0.0_hp]
      expected_weights = [0.06250238062401524_hp, 0.1874881030414334_hp, &
                          0.5624928574032072_hp, 0.1875166589313442_hp]

      call quadrilateral_bilinear_weights(source_longitude, source_latitude, -0.5_hp, -0.5_hp, &
                                          weights, is_inside, .true.)

      call f90_expect_true(is_inside, 'ESMF spherical quad target must be inside')
      call f90_expect_near(weights, expected_weights, 1.0e-13_hp, &
                           'Spherical quad weights must match legacy ESMF')
   end subroutine legacy_esmf_spherical_quad_weights
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, legacy_esmf_spherical_triangle_weights, legacy_esmf_spherical_triangle_weights,
   subroutine legacy_esmf_spherical_triangle_weights() bind(C)
      real(kind=hp), dimension(3) :: source_longitude
      real(kind=hp), dimension(3) :: source_latitude
      real(kind=hp), dimension(3) :: weights
      real(kind=hp), dimension(3) :: expected_weights
      logical :: is_inside

      source_longitude = [1.0_hp, -1.0_hp, 1.0_hp]
      source_latitude = [0.5773502691896258_hp, 0.5773502691896258_hp, -0.5773502691896258_hp]
      expected_weights = [0.13658161313872397_hp, 0.4500050262956249_hp, 0.41341336056565114_hp]

      call triangle_barycentric_weights(source_longitude, source_latitude, 0.1_hp, 0.1_hp, &
                                        weights, is_inside, .true.)

      call f90_expect_true(is_inside, 'ESMF spherical triangle target must be inside')
      call f90_expect_near(weights, expected_weights, 1.0e-13_hp, &
                           'Spherical triangle weights must match legacy ESMF')
   end subroutine legacy_esmf_spherical_triangle_weights
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, efficient_quad_and_triangle_search, efficient_quad_and_triangle_search,
   subroutine efficient_quad_and_triangle_search() bind(C)
      real(kind=hp), dimension(4) :: source_x
      real(kind=hp), dimension(4) :: source_y
      real(kind=hp), dimension(5) :: target_x
      real(kind=hp), dimension(5) :: target_y
      integer, dimension(4, 1) :: quadrilaterals
      integer, dimension(:), allocatable :: columns
      integer, dimension(:), allocatable :: rows
      real(kind=hp), dimension(:), allocatable :: weights
      integer :: number_of_weights

      source_x = [0.0_hp, 2.0_hp, 2.0_hp, 0.0_hp]
      source_y = [0.0_hp, 0.0_hp, 2.0_hp, 2.0_hp]
      quadrilaterals(:, 1) = [1, 2, 3, 4]
      target_x = [0.25_hp, 0.75_hp, 0.75_hp, 0.25_hp, 3.0_hp]
      target_y = [0.25_hp, 0.25_hp, 0.75_hp, 0.75_hp, 3.0_hp]

      call generate_regrid_weights(source_x, source_y, quadrilaterals, target_x, target_y, .false., &
                                   columns, rows, weights, number_of_weights)

      call f90_expect_true(number_of_weights == 16, 'The quad search must leave the outside point unmapped')
      call f90_expect_true(all(columns == [1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]), &
                           'Efficient quad source indices must match ESMF')
      call f90_expect_true(all(rows == [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4]), &
                           'Efficient quad destination indices must match ESMF')
   end subroutine efficient_quad_and_triangle_search
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, efficient_triangle_search, efficient_triangle_search,
   subroutine efficient_triangle_search() bind(C)
      real(kind=hp), dimension(6) :: source_longitude
      real(kind=hp), dimension(6) :: source_latitude
      real(kind=hp), dimension(1) :: target_longitude
      real(kind=hp), dimension(1) :: target_latitude
      integer, dimension(4, 0) :: no_quadrilaterals
      integer, dimension(3, 1) :: triangles
      integer, dimension(:), allocatable :: columns
      integer, dimension(:), allocatable :: rows
      real(kind=hp), dimension(:), allocatable :: weights
      real(kind=hp), dimension(3) :: expected_weights
      integer :: number_of_weights

      source_longitude = [1.0_hp, 0.0_hp, -1.0_hp, -1.0_hp, 0.0_hp, 1.0_hp]
      source_latitude = [0.5773502691896258_hp, 1.1547005383792515_hp, 0.5773502691896258_hp, &
                         -0.5773502691896258_hp, -1.1547005383792515_hp, -0.5773502691896258_hp]
      target_longitude = 0.1_hp
      target_latitude = 0.1_hp
      expected_weights = [0.13658161313872397_hp, 0.4500050262956249_hp, 0.41341336056565114_hp]
      triangles(:, 1) = [1, 3, 6]

      call generate_regrid_weights(source_longitude, source_latitude, no_quadrilaterals, &
                                   target_longitude, target_latitude, .true., &
                       columns, rows, weights, number_of_weights, triangles)

      call f90_expect_true(number_of_weights == 3, 'Spherical triangle search must find one triangle')
      call f90_expect_true(all(columns == [1, 3, 6]), 'Spherical triangle source indices must match ESMF')
      call f90_expect_true(all(rows == 1), 'Spherical triangle destination index must match ESMF')
      call f90_expect_near(weights, expected_weights, 1.0e-13_hp, &
                  'Spherical triangle search weights must match legacy ESMF')
      end subroutine efficient_triangle_search
   !$f90tw)

      !$f90tw TESTCODE(TEST, wave_regridding, cocircular_delaunay_permutation, cocircular_delaunay_permutation,
      subroutine cocircular_delaunay_permutation() bind(C)
         real(kind=hp), dimension(6) :: source_longitude
         real(kind=hp), dimension(6) :: source_latitude
         real(kind=hp), dimension(6) :: permuted_longitude
         real(kind=hp), dimension(6) :: permuted_latitude
         real(kind=hp), dimension(1) :: target_longitude
         real(kind=hp), dimension(1) :: target_latitude
         integer, dimension(6) :: permutation
         integer, dimension(4, 0) :: no_quadrilaterals
         integer, dimension(:), allocatable :: columns
         integer, dimension(:), allocatable :: permuted_columns
         integer, dimension(:), allocatable :: rows
         integer, dimension(:), allocatable :: permuted_rows
         real(kind=hp), dimension(:), allocatable :: weights
         real(kind=hp), dimension(:), allocatable :: permuted_weights
         integer :: number_of_weights
         integer :: permuted_number_of_weights

         source_longitude = [1.0_hp, 0.0_hp, -1.0_hp, -1.0_hp, 0.0_hp, 1.0_hp]
         source_latitude = [0.5773502691896258_hp, 1.1547005383792515_hp, 0.5773502691896258_hp, &
                            -0.5773502691896258_hp, -1.1547005383792515_hp, -0.5773502691896258_hp]
         permutation = [4, 1, 6, 3, 2, 5]
         permuted_longitude = source_longitude(permutation)
         permuted_latitude = source_latitude(permutation)
         target_longitude = 0.1_hp
         target_latitude = 0.1_hp

         call generate_regrid_weights(source_longitude, source_latitude, no_quadrilaterals, &
                                      target_longitude, target_latitude, .true., &
                                      columns, rows, weights, number_of_weights)
         call generate_regrid_weights(permuted_longitude, permuted_latitude, no_quadrilaterals, &
                                      target_longitude, target_latitude, .true., &
                                      permuted_columns, permuted_rows, permuted_weights, &
                                      permuted_number_of_weights)

         call f90_expect_true(number_of_weights == 3, 'Cocircular source centers must produce one triangle')
         call f90_expect_true(permuted_number_of_weights == number_of_weights, &
                              'Permutation must not change the sparse weight count')
         call f90_expect_near(source_longitude(columns), permuted_longitude(permuted_columns), 0.0_hp, &
                              'Permutation must not change selected triangle longitudes')
         call f90_expect_near(source_latitude(columns), permuted_latitude(permuted_columns), 0.0_hp, &
                              'Permutation must not change selected triangle latitudes')
         call f90_expect_near(weights, permuted_weights, 0.0_hp, &
                              'Permutation must not change cocircular interpolation weights')
      end subroutine cocircular_delaunay_permutation
      !$f90tw)

      !$f90tw TESTCODE(TEST, wave_regridding, constrained_polygon_mesh, constrained_polygon_mesh,
      subroutine constrained_polygon_mesh() bind(C)
         use iso_c_binding, only: c_double, c_int

         integer(c_int), parameter :: maximum_output_size = 1000
         integer(c_int), parameter :: number_of_boundary_points = 4
         real(c_double), parameter :: maximum_area = 0.05_c_double
         real(c_double), dimension(number_of_boundary_points) :: boundary_x
         real(c_double), dimension(number_of_boundary_points) :: boundary_y
         integer(c_int), dimension(3, maximum_output_size) :: triangle_nodes
         integer(c_int), dimension(2, 1) :: unused_edge_nodes
         integer(c_int), dimension(3, 1) :: unused_triangle_edges
         real(c_double), dimension(maximum_output_size) :: mesh_x
         real(c_double), dimension(maximum_output_size) :: mesh_y
         integer(c_int) :: mode
         integer(c_int) :: number_of_edges
         integer(c_int) :: number_of_mesh_points
         integer(c_int) :: number_of_triangles
         integer(c_int) :: triangle
         real(c_double) :: area
         real(c_double) :: twice_area
         real(c_double) :: requested_area

         boundary_x = [0.0_c_double, 1.0_c_double, 1.0_c_double, 0.0_c_double]
         boundary_y = [0.0_c_double, 0.0_c_double, 1.0_c_double, 1.0_c_double]
         mode = 2
         number_of_edges = 0
         number_of_mesh_points = 1
         number_of_triangles = 1
         requested_area = maximum_area

         call tricall(mode, boundary_x, boundary_y, number_of_boundary_points, triangle_nodes, number_of_triangles, &
                      unused_edge_nodes, number_of_edges, unused_triangle_edges, mesh_x, mesh_y, &
                      number_of_mesh_points, requested_area)

         call f90_expect_true(number_of_triangles < 0, 'Insufficient triangle capacity must return required size')
         call f90_expect_true(number_of_mesh_points < 0, 'Insufficient point capacity must return required size')

         number_of_mesh_points = maximum_output_size
         number_of_triangles = maximum_output_size
         call tricall(mode, boundary_x, boundary_y, number_of_boundary_points, triangle_nodes, number_of_triangles, &
                      unused_edge_nodes, number_of_edges, unused_triangle_edges, mesh_x, mesh_y, &
                      number_of_mesh_points, requested_area)

         call f90_expect_true(number_of_triangles > 0, 'Constrained polygon must produce triangles')
         call f90_expect_true(number_of_mesh_points >= number_of_boundary_points, 'Mesh must retain polygon vertices')
         call f90_expect_true(all(triangle_nodes(:, 1:number_of_triangles) >= 1), 'Triangle nodes must be one-based')
         call f90_expect_true(all(triangle_nodes(:, 1:number_of_triangles) <= number_of_mesh_points), &
                              'Triangle nodes must reference generated points')

         do triangle = 1, number_of_triangles
            twice_area = abs( &
               mesh_x(triangle_nodes(1, triangle)) * &
                  (mesh_y(triangle_nodes(2, triangle)) - mesh_y(triangle_nodes(3, triangle))) + &
               mesh_x(triangle_nodes(2, triangle)) * &
                  (mesh_y(triangle_nodes(3, triangle)) - mesh_y(triangle_nodes(1, triangle))) + &
               mesh_x(triangle_nodes(3, triangle)) * &
                  (mesh_y(triangle_nodes(1, triangle)) - mesh_y(triangle_nodes(2, triangle))))
            area = 0.5_c_double * twice_area
            call f90_expect_true(area <= maximum_area * (1.0_c_double + 1.0e-12_c_double), &
                                 'Generated triangle must satisfy maximum area')
         end do
      end subroutine constrained_polygon_mesh
      !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, triangle_regrid_weights, triangle_regrid_weights,
   subroutine triangle_regrid_weights() bind(C)
      real(kind=hp), dimension(3) :: source_x
      real(kind=hp), dimension(3) :: source_y
      real(kind=hp), dimension(4) :: target_x
      real(kind=hp), dimension(4) :: target_y
      integer, dimension(3, 1) :: triangles
      integer, dimension(:), allocatable :: columns
      integer, dimension(:), allocatable :: rows
      real(kind=hp), dimension(:), allocatable :: weights
      integer :: number_of_weights
      real, dimension(3) :: source_values
      real, dimension(4, 1) :: target_values
      type(grid_map) :: sparse_map
      type(grid) :: target_grid

      source_x = [0.0_hp, 2.0_hp, 0.0_hp]
      source_y = [0.0_hp, 0.0_hp, 2.0_hp]
      triangles(:, 1) = [1, 2, 3]
      target_x = [0.5_hp, 1.0_hp, 0.0_hp, 2.0_hp]
      target_y = [0.5_hp, 0.0_hp, 0.0_hp, 2.0_hp]

      call generate_triangle_regrid_weights(source_x, source_y, triangles, target_x, target_y, &
                                            columns, rows, weights, number_of_weights)

      call f90_expect_true(number_of_weights == 9, 'Three interior or boundary targets must have weights')
      call f90_expect_true(all(columns == [1, 2, 3, 1, 2, 3, 1, 2, 3]), 'Sparse source indices')
      call f90_expect_true(all(rows == [1, 1, 1, 2, 2, 2, 3, 3, 3]), 'Sparse target indices')
      call f90_expect_near(real(weights(1)), 0.5, 1.0e-6, 'Interior first weight')
      call f90_expect_near(real(weights(2)), 0.25, 1.0e-6, 'Interior second weight')
      call f90_expect_near(real(weights(3)), 0.25, 1.0e-6, 'Interior third weight')

      source_values = [2.0, 8.0, -6.0]
      target_values = -999.0
      allocate(target_grid%covered(4, 1), source=0)
      target_grid%covered(1:3, 1) = 1
      sparse_map%grids_linked = .true.
      sparse_map%sferic = .false.
      sparse_map%n_s = number_of_weights
      allocate(sparse_map%col, source=columns)
      allocate(sparse_map%row, source=rows)
      allocate(sparse_map%s, source=weights)

      call grmap_esmf(1, source_values, 3, target_values, 4, 1, sparse_map, target_grid)

      call f90_expect_near(target_values(1, 1), 1.5, 1.0e-6, 'Affine field at interior point')
      call f90_expect_near(target_values(2, 1), 5.0, 1.0e-6, 'Affine field at edge point')
      call f90_expect_near(target_values(3, 1), 2.0, 1.0e-6, 'Affine field at vertex')
      call f90_expect_near(target_values(4, 1), -999.0, 1.0e-6, 'Outside point remains unmapped')
   end subroutine triangle_regrid_weights
   !$f90tw)

   !$f90tw TESTCODE(TEST, wave_regridding, degenerate_triangle, degenerate_triangle,
   subroutine degenerate_triangle() bind(C)
      real(kind=hp), dimension(3) :: vertex_x
      real(kind=hp), dimension(3) :: vertex_y
      real(kind=hp), dimension(3) :: weights
      logical :: is_inside

      vertex_x = [0.0_hp, 1.0_hp, 2.0_hp]
      vertex_y = [0.0_hp, 0.0_hp, 0.0_hp]

      call triangle_barycentric_weights(vertex_x, vertex_y, 0.5_hp, 0.0_hp, weights, is_inside)

      call f90_expect_true(.not. is_inside, 'Collinear triangle must not contain a target')
      call f90_expect_true(all(weights == 0.0_hp), 'Degenerate triangle must return zero weights')
   end subroutine degenerate_triangle
   !$f90tw)

end module tests_wave_regridding