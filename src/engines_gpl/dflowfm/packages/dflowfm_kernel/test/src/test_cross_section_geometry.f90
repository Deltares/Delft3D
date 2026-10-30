module test_cross_section_geometry
   use assertions_gtest, only: f90_expect_eq, f90_expect_near
   use precision, only: dp
   use m_partitioninfo, only: fill_geometry_arrays_crs, jampi
   use m_monitoring_crosssections, only: crs, ncrs, n_nodes_crs => nNodesCrs, &
                                        node_count_crs => nodeCountCrs, geom_x_crs => geomXCrs, geom_y_crs => geomYCrs

   implicit none(type, external)
   private

contains

   !$f90tw TESTCODE(TEST, test_cross_section_geometry, single_link, test_single_link,
   !> Verify that one cross-section link contributes both endpoints and their coordinates.
   subroutine test_single_link() bind(C)
      integer :: original_jampi

      original_jampi = jampi
      jampi = 0
      ncrs = 1
      allocate (crs(ncrs))
      call set_path(1, reshape([1.0_dp, 2.0_dp], [2, 1]))

      call fill_geometry_arrays_crs()

      call f90_expect_eq(n_nodes_crs, 2, 'A single link contributes two geometry nodes')
      call f90_expect_eq(node_count_crs(1), 2, 'The cross section contains both endpoints')
      call f90_expect_near(geom_x_crs, [1.0_dp, 2.0_dp], 0.0_dp, 'Both x coordinates are written')
      call f90_expect_near(geom_y_crs, [101.0_dp, 102.0_dp], 0.0_dp, 'Both y coordinates are written')

      call cleanup_geometry()
      jampi = original_jampi
   end subroutine test_single_link
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_cross_section_geometry, mixed_paths, test_mixed_paths,
   !> Verify node counts and coordinates for connected and disconnected cross-section paths.
   subroutine test_mixed_paths() bind(C)
      integer :: original_jampi, i
      real(kind=dp), dimension(11), parameter :: EXPECTED_X = &
                                                 [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, &
                                                  6.0_dp, 7.0_dp, 9.0_dp, 10.0_dp, 11.0_dp, 12.0_dp]
      integer, dimension(5), parameter :: EXPECTED_COUNTS = [2, 0, 3, 4, 2]

      original_jampi = jampi
      jampi = 0
      ncrs = 5
      allocate (crs(ncrs))
      call set_path(1, reshape([1.0_dp, 2.0_dp], [2, 1]))
      crs(2)%path%lnx = 0
      call set_path(3, reshape([3.0_dp, 4.0_dp, 4.0_dp, 5.0_dp], [2, 2]))
      call set_path(4, reshape([6.0_dp, 7.0_dp, 9.0_dp, 10.0_dp], [2, 2]))
      call set_path(5, reshape([11.0_dp, 12.0_dp], [2, 1]))

      call fill_geometry_arrays_crs()

      call f90_expect_eq(n_nodes_crs, size(EXPECTED_X), 'All geometry nodes are counted, including disconnected endpoints')
      call f90_expect_eq(n_nodes_crs, sum(node_count_crs), 'The total matches the per-section node counts')
      do i = 1, ncrs
         call f90_expect_eq(node_count_crs(i), EXPECTED_COUNTS(i), 'Each section has the expected node count')
      end do
      call f90_expect_near(geom_x_crs, EXPECTED_X, 0.0_dp, 'Section offsets and trailing x coordinates are correct')
      call f90_expect_near(geom_y_crs, EXPECTED_X + 100.0_dp, 0.0_dp, 'Section offsets and trailing y coordinates are correct')

      call cleanup_geometry()
      jampi = original_jampi
   end subroutine test_mixed_paths
   !$f90tw)

   subroutine set_path(index, x)
      integer, intent(in) :: index
      real(kind=dp), dimension(:, :), intent(in) :: x
      integer :: i

      crs(index)%path%lnx = size(x, 2)
      crs(index)%path%xk = x
      crs(index)%path%yk = x + 100.0_dp
      crs(index)%path%iperm = [(i, i=1, size(x, 2))]
   end subroutine set_path

   subroutine cleanup_geometry()
      deallocate (crs, node_count_crs, geom_x_crs, geom_y_crs)
      ncrs = 0
      n_nodes_crs = 0
   end subroutine cleanup_geometry

end module test_cross_section_geometry
