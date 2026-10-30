!> Tests of ArcInfo raster loading and in-memory bilinear interpolation.
module test_bilinear_arcinfo
   use assertions_gtest
   use precision, only: dp
   use m_bilinear_interpolation, only: bilinear_data
   use m_missing, only: dmiss
   use timespace_parameters, only: ARCINFO

   implicit none(type, external)
   private
   public :: test_bilinear_arcinfo_read, test_bilinear_weights, test_bilinear_boundaries
   public :: test_bilinear_missing_corners, test_bilinear_signed_spacing
   public :: test_bilinear_thin_grids
   public :: test_bilinear_arcinfo_rectangular

contains

   !$f90tw TESTCODE(TEST, test_bilinear_arcinfo, test_bilinear_arcinfo_read, test_bilinear_arcinfo_read,
   !> Read geometry, row order, and missing values from an ArcInfo file, then interpolate.
   subroutine test_bilinear_arcinfo_read() bind(C)
      character(len=*), parameter :: filename = 'test_bilinear_arcinfo.asc'
      type(bilinear_data) :: data
      integer :: file_unit, status

      open (newunit=file_unit, file=filename, status='replace', action='write', iostat=status)
      call f90_expect_eq(status, 0)
      if (status /= 0) then
         return
      end if
      write (file_unit, '(a)') 'ncols 3', 'nrows 2', 'xllcorner 10', 'yllcorner 20', &
                              'cellsize 2', 'NODATA_value -9999', '4 5 -9999', '1 2 3'
      close (file_unit)

      data = bilinear_data(filename, ARCINFO, status)
      call f90_expect_eq(status, 0)
      open (newunit=file_unit, file=filename, status='old', iostat=status)
      call f90_expect_eq(status, 0)
      if (status == 0) then
         close (file_unit, status='delete')
      end if
      call f90_expect_true(allocated(data%values))
      if (.not. allocated(data%values)) then
         return
      end if

      call f90_expect_eq(size(data%values, 1), 3)
      call f90_expect_eq(size(data%values, 2), 2)
      call f90_expect_near(data%origin(1), 11.0_dp, 0.0_dp)
      call f90_expect_near(data%origin(2), 21.0_dp, 0.0_dp)
      call f90_expect_near(data%spacing(1), 2.0_dp, 0.0_dp)
      call f90_expect_near(data%spacing(2), 2.0_dp, 0.0_dp)
      call f90_expect_near(data%missing_value, dmiss, 0.0_dp)
      call f90_expect_near(data%values(1, 1), 1.0_dp, 0.0_dp)
      call f90_expect_near(data%values(1, 2), 4.0_dp, 0.0_dp)
      call f90_expect_near(data%values(3, 2), dmiss, 0.0_dp)
      call check_bilinear_results(data, [0.5_dp, 1.5_dp], [0.5_dp, 0.5_dp], [3.0_dp, dmiss])
   end subroutine test_bilinear_arcinfo_read
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_arcinfo, test_bilinear_arcinfo_rectangular, test_bilinear_arcinfo_rectangular,
   !> Non-square rasters shift corner coordinates by their own axis spacing, but not centre coordinates.
   subroutine test_bilinear_arcinfo_rectangular() bind(C)
      character(len=*), parameter :: filename = 'test_bilinear_rectangular.asc'
      type(bilinear_data) :: data
      real(kind=dp), dimension(2) :: expected_origin
      integer :: file_unit, status, coordinate_case

      do coordinate_case = 1, 2
         open (newunit=file_unit, file=filename, status='replace', action='write', iostat=status)
         call f90_expect_eq(status, 0)
         if (status /= 0) then
            return
         end if
         write (file_unit, '(a)') 'ncols 2', 'nrows 2'
         if (coordinate_case == 1) then
            write (file_unit, '(a)') 'xllcorner 10', 'yllcorner 20'
            expected_origin = [11.0_dp, 22.0_dp]
         else
            write (file_unit, '(a)') 'xllcenter 10', 'yllcenter 20'
            expected_origin = [10.0_dp, 20.0_dp]
         end if
         write (file_unit, '(a)') 'cellsize 2 4', 'NODATA_value -9999', '10 22', '2 6'
         close (file_unit)
         data = bilinear_data(filename, ARCINFO, status)
         call f90_expect_eq(status, 0)
         open (newunit=file_unit, file=filename, status='old', iostat=status)
         call f90_expect_eq(status, 0)
         if (status == 0) then
            close (file_unit, status='delete')
         end if
         if (.not. allocated(data%values)) then
            return
         end if
         call f90_expect_near(data%origin(1), expected_origin(1), 0.0_dp)
         call f90_expect_near(data%origin(2), expected_origin(2), 0.0_dp)
         call f90_expect_near(data%spacing(1), 2.0_dp, 0.0_dp)
         call f90_expect_near(data%spacing(2), 4.0_dp, 0.0_dp)
         call f90_expect_near(data%interpolate(expected_origin(1) + 1.0_dp, expected_origin(2) + 2.0_dp), &
                              10.0_dp, 1.0e-12_dp)
      end do
   end subroutine test_bilinear_arcinfo_rectangular
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_arcinfo, test_bilinear_weights, test_bilinear_weights,
   !> Verify corner weights, exact sample coordinates, and elemental vector calls.
   subroutine test_bilinear_weights() bind(C)
      type(bilinear_data) :: data

      data = make_bilinear_data()
      call check_bilinear_results(data, &
                                  [0.0_dp, 0.25_dp, 0.5_dp, 0.0_dp, 0.5_dp, 0.75_dp], &
                                  [0.0_dp, 0.5_dp, 0.5_dp, 0.5_dp, 0.0_dp, 0.25_dp], &
                                  [2.0_dp, 8.0_dp, 10.0_dp, 6.0_dp, 4.0_dp, 8.5_dp])
   end subroutine test_bilinear_weights
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_arcinfo, test_bilinear_boundaries, test_bilinear_boundaries,
   !> Preserve truncation below the origin and exclusion of the last row and column.
   subroutine test_bilinear_boundaries() bind(C)
      type(bilinear_data) :: data

      data = make_bilinear_data()
      call check_bilinear_results(data, &
                                  [-0.25_dp, 0.0_dp, -0.25_dp, -1.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 2.0_dp], &
                                  [0.0_dp, -0.25_dp, -0.5_dp, 0.0_dp, -1.0_dp, 0.0_dp, 1.0_dp, 2.0_dp], &
                                  [1.0_dp, 0.0_dp, -2.0_dp, data%missing_value, data%missing_value, &
                                   data%missing_value, data%missing_value, data%missing_value])
   end subroutine test_bilinear_boundaries
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_arcinfo, test_bilinear_missing_corners, test_bilinear_missing_corners,
   !> Every missing corner rejects interpolation, even when its weight would be zero.
   subroutine test_bilinear_missing_corners() bind(C)
      type(bilinear_data) :: data
      integer :: column_index, row_index

      do row_index = 1, 2
         do column_index = 1, 2
            data = make_bilinear_data()
            data%values(column_index, row_index) = data%missing_value
            call check_bilinear_results(data, [0.0_dp, 0.25_dp, 0.5_dp], [0.0_dp, 0.5_dp, 0.5_dp], &
                                        [data%missing_value, data%missing_value, data%missing_value])
         end do
      end do
   end subroutine test_bilinear_missing_corners
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_arcinfo, test_bilinear_signed_spacing, test_bilinear_signed_spacing,
   !> Coordinate mapping supports unequal spacing and either signed axis direction.
   subroutine test_bilinear_signed_spacing() bind(C)
      type(bilinear_data) :: data
      integer :: x_direction, y_direction

      do x_direction = -1, 1, 2
         do y_direction = -1, 1, 2
            data = make_bilinear_data()
            data%spacing = [2.0_dp * x_direction, 4.0_dp * y_direction]
            call check_bilinear_results(data, [0.0_dp, 0.25_dp, 0.5_dp, 1.0_dp], &
                                        [0.0_dp, 0.5_dp, 0.5_dp, 0.0_dp], &
                                        [2.0_dp, 8.0_dp, 10.0_dp, data%missing_value])
         end do
      end do
   end subroutine test_bilinear_signed_spacing
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_arcinfo, test_bilinear_thin_grids, test_bilinear_thin_grids,
   !> Grids without a two-by-two neighborhood return the configured missing marker.
   subroutine test_bilinear_thin_grids() bind(C)
      integer, dimension(3), parameter :: num_columns = [1, 1, 2]
      integer, dimension(3), parameter :: num_rows = [1, 2, 1]
      type(bilinear_data) :: data
      integer :: grid_index

      do grid_index = 1, size(num_columns)
         data = bilinear_data(origin=[10.0_dp, 20.0_dp], spacing=[2.0_dp, 4.0_dp], missing_value=-4321.0_dp)
         allocate (data%values(num_columns(grid_index), num_rows(grid_index)))
         data%values = 2.0_dp
         call check_bilinear_results(data, [0.0_dp, 0.5_dp, -0.25_dp], [0.0_dp, 0.5_dp, -0.25_dp], &
                                     [data%missing_value, data%missing_value, data%missing_value])
      end do
   end subroutine test_bilinear_thin_grids
   !$f90tw)

   function make_bilinear_data() result(data)
      type(bilinear_data) :: data

      data = bilinear_data(values=reshape([2.0_dp, 6.0_dp, 10.0_dp, 22.0_dp], [2, 2]), &
                           origin=[10.0_dp, 20.0_dp], spacing=[2.0_dp, 4.0_dp], missing_value=-4321.0_dp)
   end function make_bilinear_data

   subroutine check_bilinear_results(data, grid_x, grid_y, expected)
      type(bilinear_data), intent(in) :: data
      real(kind=dp), dimension(:), intent(in) :: grid_x, grid_y
      real(kind=dp), dimension(:), intent(in) :: expected
      real(kind=dp), dimension(size(grid_x)) :: x_coordinates, y_coordinates, interpolated_values
      integer :: point_index

      x_coordinates = data%origin(1) + grid_x * data%spacing(1)
      y_coordinates = data%origin(2) + grid_y * data%spacing(2)
      interpolated_values = data%interpolate(x_coordinates, y_coordinates)
      do point_index = 1, size(grid_x)
         call f90_expect_near(data%interpolate(x_coordinates(point_index), y_coordinates(point_index)), &
                              interpolated_values(point_index), 0.0_dp)
         call f90_expect_near(interpolated_values(point_index), expected(point_index), 1.0e-12_dp)
      end do
   end subroutine check_bilinear_results

end module test_bilinear_arcinfo