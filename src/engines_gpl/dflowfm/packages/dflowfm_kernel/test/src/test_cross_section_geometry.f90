module test_cross_section_geometry
   use assertions_gtest, only: f90_expect_eq, f90_expect_near
   use precision, only: dp
   use m_partitioninfo, only: fill_geometry_arrays_crs, jampi
   use m_monitoring_crosssections, only: crs, ncrs, nNodesCrs, nodeCountCrs, geomXCrs, geomYCrs

   implicit none(type, external)
   private

contains

   !$f90tw TESTCODE(TEST, test_cross_section_geometry, single_link, test_single_link,
   subroutine test_single_link() bind(C)
      integer :: original_jampi

      original_jampi = jampi
      jampi = 0
      ncrs = 1
      allocate (crs(ncrs))
      call set_path(1, reshape([1.0_dp, 2.0_dp], [2, 1]))

      call fill_geometry_arrays_crs()

      call f90_expect_eq(nNodesCrs, 2, 'A single link contributes two geometry nodes')
      call f90_expect_eq(nodeCountCrs(1), 2, 'The cross section contains both endpoints')
      call f90_expect_near(geomXCrs, [1.0_dp, 2.0_dp], 0.0_dp, 'Both x coordinates are written')
      call f90_expect_near(geomYCrs, [101.0_dp, 102.0_dp], 0.0_dp, 'Both y coordinates are written')

      call cleanup_geometry()
      jampi = original_jampi
   end subroutine test_single_link
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_cross_section_geometry, mixed_paths, test_mixed_paths,
   subroutine test_mixed_paths() bind(C)
      integer :: original_jampi, i
      real(kind=dp), dimension(11), parameter :: expected_x = &
                                                 [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, &
                                                  6.0_dp, 7.0_dp, 9.0_dp, 10.0_dp, 11.0_dp, 12.0_dp]
      integer, dimension(5), parameter :: expected_counts = [2, 0, 3, 4, 2]

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

      call f90_expect_eq(nNodesCrs, size(expected_x), 'All geometry nodes are counted, including disconnected endpoints')
      call f90_expect_eq(nNodesCrs, sum(nodeCountCrs), 'The total matches the per-section node counts')
      do i = 1, ncrs
         call f90_expect_eq(nodeCountCrs(i), expected_counts(i), 'Each section has the expected node count')
      end do
      call f90_expect_near(geomXCrs, expected_x, 0.0_dp, 'Section offsets and trailing x coordinates are correct')
      call f90_expect_near(geomYCrs, expected_x + 100.0_dp, 0.0_dp, 'Section offsets and trailing y coordinates are correct')

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
      deallocate (crs, nodeCountCrs, geomXCrs, geomYCrs)
      ncrs = 0
      nNodesCrs = 0
   end subroutine cleanup_geometry

end module test_cross_section_geometry
