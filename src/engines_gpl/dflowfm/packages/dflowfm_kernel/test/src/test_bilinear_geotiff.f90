!> GeoTIFF raster loading, interpolation, and initial-field integration tests.
module test_bilinear_geotiff
   use assertions_gtest
   use precision, only: dp
   use iso_c_binding, only: c_float, c_double, c_null_char, c_null_ptr
   use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
   use gdal, only: gdaldriverh, gdaldataseth, gdalallregister, gdalgetdriverbyname, gdalcreate, gdalassociated, &
                   gdalsetgeotransform, gdaldatasetrasterio_f, gf_write, gdalgetrasterband, &
                   gdalsetrasternodatavalue, gdalclose, gdt_float32, gdt_float64, gdt_int16, &
                   gdalsetmetadataitem, gdalmajorobjecth_new
   use m_bilinear_interpolation, only: bilinear_data
   use timespace_parameters, only: GEOTIFF
   use m_missing, only: dmiss
   use m_samples, only: ns, mxsam, mysam

   implicit none(type, external)
   private
   public :: test_geotiff_numeric_bands, test_geotiff_missing_values
   public :: test_geotiff_pixel_coordinates, test_geotiff_rejects_invalid_geometry
   public :: test_geotiff_rotation_tolerance
   public :: test_bilinear_arcinfo_geotiff_initial_field

contains

   !$f90tw TESTCODE(TEST, test_bilinear_geotiff, test_geotiff_numeric_bands, test_geotiff_numeric_bands,
   !> Read floating-point and integer bands without modifying global samples.
   subroutine test_geotiff_numeric_bands() bind(C)
      character(len=*), parameter :: filename = 'test_bilinear_numeric.tif'
      integer, dimension(3), parameter :: data_types = [gdt_float32, gdt_float64, gdt_int16]
      real(kind=c_double), dimension(2, 2, 1) :: values
      real(kind=c_double), dimension(6) :: transform
      type(bilinear_data) :: data
      integer :: status, type_index, saved_ns, saved_mxsam, saved_mysam

      saved_ns = ns
      saved_mxsam = mxsam
      saved_mysam = mysam
      values = reshape([2.0_dp, 6.0_dp, 10.0_dp, 22.0_dp], shape(values))
      transform = [20.0_dp, 2.0_dp, 0.0_dp, 30.0_dp, 0.0_dp, -4.0_dp]
      do type_index = 1, size(data_types)
         if (.not. create_test_geotiff(filename, values, transform, data_types(type_index))) then
            return
         end if
         data = bilinear_data(filename, GEOTIFF, status)
         call delete_test_geotiff(filename)
         call f90_expect_eq(status, 0)
         call f90_expect_true(allocated(data%values))
         if (.not. allocated(data%values)) then
            return
         end if
         call f90_expect_eq(size(data%values, 1), 2)
         call f90_expect_eq(size(data%values, 2), 2)
         call f90_expect_near(data%origin(1), 21.0_dp, 0.0_dp)
         call f90_expect_near(data%origin(2), 28.0_dp, 0.0_dp)
         call f90_expect_near(data%spacing(1), 2.0_dp, 0.0_dp)
         call f90_expect_near(data%spacing(2), -4.0_dp, 0.0_dp)
         call f90_expect_true(all(data%values == values(:, :, 1)))
         call f90_expect_near(data%interpolate(21.5_dp, 26.0_dp), 8.0_dp, 1.0e-12_dp)
         call f90_expect_near(data%interpolate(22.0_dp, 26.0_dp), 10.0_dp, 1.0e-12_dp)
         call f90_expect_eq(ns, saved_ns)
         call f90_expect_eq(mxsam, saved_mxsam)
         call f90_expect_eq(mysam, saved_mysam)
      end do
   end subroutine test_geotiff_numeric_bands
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_geotiff, test_geotiff_missing_values, test_geotiff_missing_values,
   !> Normalize finite and NaN NoData and preserve the missing-corner interpolation rule.
   subroutine test_geotiff_missing_values() bind(C)
      character(len=*), parameter :: filename = 'test_bilinear_missing.tif'
      real(kind=c_double), dimension(2, 2, 1) :: values
      real(kind=c_double), dimension(6) :: transform
      real(kind=c_double) :: file_missing
      type(bilinear_data) :: data
      integer :: status, missing_case, data_type

      transform = [20.0_dp, 2.0_dp, 0.0_dp, 30.0_dp, 0.0_dp, -4.0_dp]
      do missing_case = 1, 5
         data_type = gdt_float64
         select case (missing_case)
         case (1)
            file_missing = -9999.0_dp
         case (2, 5)
            file_missing = ieee_value(0.0_dp, ieee_quiet_nan)
         case (3, 4)
            file_missing = -3.4_dp
         end select
         if (missing_case == 3 .or. missing_case == 5) then
            data_type = gdt_float32
         end if
         values = reshape([2.0_dp, 6.0_dp, 10.0_dp, file_missing], shape(values))
         if (missing_case == 3) then
            values(1, 1, 1) = real(nearest(real(file_missing, c_float), 1.0_c_float), c_double)
         end if
         if (.not. create_test_geotiff(filename, values, transform, data_type, file_missing)) then
            return
         end if
         data = bilinear_data(filename, GEOTIFF, status)
         call delete_test_geotiff(filename)
         call f90_expect_eq(status, 0)
         if (.not. allocated(data%values)) then
            return
         end if
         call f90_expect_near(data%values(1, 1), values(1, 1, 1), 0.0_dp)
         call f90_expect_near(data%values(2, 2), dmiss, 0.0_dp)
         call f90_expect_near(data%interpolate(22.0_dp, 26.0_dp), dmiss, 0.0_dp)
         call f90_expect_near(data%interpolate(21.0_dp, 28.0_dp), dmiss, 0.0_dp)
      end do
   end subroutine test_geotiff_missing_values
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_geotiff, test_geotiff_pixel_coordinates, test_geotiff_pixel_coordinates,
   !> GDAL's corner-based geotransform gives pixel centres for both Area and Point rasters.
   subroutine test_geotiff_pixel_coordinates() bind(C)
      character(len=*), parameter :: filename = 'test_bilinear_coordinates.tif'
      real(kind=c_double), dimension(2, 2, 1) :: values
      real(kind=c_double), dimension(6) :: transform
      type(bilinear_data) :: data
      integer :: status, point_case, x_direction, y_direction

      values = reshape([2.0_dp, 6.0_dp, 10.0_dp, 22.0_dp], shape(values))
      do point_case = 0, 1
         do x_direction = -1, 1, 2
            do y_direction = -1, 1, 2
               transform = [20.0_dp, 2.0_dp * x_direction, 0.0_dp, 30.0_dp, 0.0_dp, 4.0_dp * y_direction]
               if (.not. create_test_geotiff(filename, values, transform, gdt_float64, is_point=point_case == 1)) then
                  return
               end if
               data = bilinear_data(filename, GEOTIFF, status)
               call delete_test_geotiff(filename)
               call f90_expect_eq(status, 0)
               if (.not. allocated(data%values)) then
                  return
               end if
               call f90_expect_near(data%origin(1), 20.0_dp + x_direction, 0.0_dp)
               call f90_expect_near(data%origin(2), 30.0_dp + 2.0_dp * y_direction, 0.0_dp)
               call f90_expect_near(data%interpolate(20.0_dp + 2.0_dp * x_direction, &
                                                   30.0_dp + 4.0_dp * y_direction), 10.0_dp, 1.0e-12_dp)
            end do
         end do
      end do
   end subroutine test_geotiff_pixel_coordinates
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_geotiff, test_geotiff_rotation_tolerance, test_geotiff_rotation_tolerance,
   !> Match the legacy rotation tolerance for both signs and both geotransform terms.
   subroutine test_geotiff_rotation_tolerance() bind(C)
      character(len=*), parameter :: filename = 'test_bilinear_rotation.tif'
      integer, dimension(2), parameter :: rotation_terms = [3, 5]
      real(kind=dp), dimension(3), parameter :: rotations = [0.5e-6_dp, 1.0e-6_dp, 1.0001e-6_dp]
      real(kind=c_double), dimension(2, 2, 1) :: values
      real(kind=c_double), dimension(6) :: transform
      type(bilinear_data) :: data
      integer :: term_index, rotation_index, direction, status

      values = reshape([2.0_dp, 6.0_dp, 10.0_dp, 22.0_dp], shape(values))
      do term_index = 1, size(rotation_terms)
         do direction = -1, 1, 2
            do rotation_index = 1, size(rotations)
               transform = [20.0_dp, 2.0_dp, 0.0_dp, 30.0_dp, 0.0_dp, -4.0_dp]
               transform(rotation_terms(term_index)) = direction * rotations(rotation_index)
               if (.not. create_test_geotiff(filename, values, transform, gdt_float64)) then
                  return
               end if
               data = bilinear_data(filename, GEOTIFF, status)
               call delete_test_geotiff(filename)
               if (rotation_index <= 2) then
                  call f90_expect_eq(status, 0)
                  call f90_expect_true(allocated(data%values))
                  if (allocated(data%values)) then
                     call f90_expect_near(data%origin(1), 21.0_dp, 0.0_dp)
                     call f90_expect_near(data%origin(2), 28.0_dp, 0.0_dp)
                     call f90_expect_near(data%interpolate(22.0_dp, 26.0_dp), 10.0_dp, 1.0e-12_dp)
                  end if
               else
                  call f90_expect_true(status /= 0)
                  call f90_expect_false(allocated(data%values))
               end if
            end do
         end do
      end do
   end subroutine test_geotiff_rotation_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bilinear_geotiff, test_geotiff_rejects_invalid_geometry, test_geotiff_rejects_invalid_geometry,
   !> Reject rotation, degenerate spacing, and multiple bands without retaining partial data.
   subroutine test_geotiff_rejects_invalid_geometry() bind(C)
      character(len=*), parameter :: filename = 'test_bilinear_invalid.tif'
      real(kind=c_double), dimension(2, 2, 2) :: values
      real(kind=c_double), dimension(6) :: transform
      type(bilinear_data) :: data
      integer :: status, invalid_case, num_bands

      values = 2.0_dp
      do invalid_case = 1, 4
         transform = [20.0_dp, 2.0_dp, 0.0_dp, 30.0_dp, 0.0_dp, -4.0_dp]
         num_bands = 1
         select case (invalid_case)
         case (1)
            transform(3) = 0.25_dp
         case (2)
            transform(5) = 0.25_dp
         case (3)
            transform(2) = 0.0_dp
         case (4)
            num_bands = 2
         end select
         if (.not. create_test_geotiff(filename, values(:, :, 1:num_bands), transform, gdt_float64)) then
            return
         end if
         data = bilinear_data(filename, GEOTIFF, status)
         call delete_test_geotiff(filename)
         call f90_expect_true(status /= 0)
         call f90_expect_false(allocated(data%values))
      end do
   end subroutine test_geotiff_rejects_invalid_geometry
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration,
   !$f90tw test_bilinear_arcinfo_geotiff_initial_field, test_bilinear_arcinfo_geotiff_initial_field,
   !> Equivalent ArcInfo and GeoTIFF rasters initialize identical water-level fields.
   subroutine test_bilinear_arcinfo_geotiff_initial_field() bind(C)
      use test_init_spatial_fields_integration, only: setup_minimal_grid_with_points, teardown_minimal_grid, parse_spatial_block
      use fm_external_forcings, only: init_spatial_fields
      use m_file_helpers, only: create_file
      use m_cell_geometry, only: xz, yz, ndx
      use m_flow, only: s1, hs
      use m_flowgeom, only: ndx2D, ndxi, bl
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_meteo, only: initialize_ec_module
      use m_polygon, only: m_polygon_destructor
      use m_alloc, only: realloc
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_destroy
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL

      character(len=24), dimension(2), parameter :: raster_files = &
                                                  [character(len=24) :: 'test_init_bilinear.asc', 'test_init_bilinear.tif']
      character(len=8), dimension(2), parameter :: file_types = [character(len=8) :: 'arcinfo', 'geotiff']
      character(len=*), parameter :: ext_file = 'test_init_bilinear.ext'
      real(kind=c_double), dimension(3, 2, 1) :: values
      real(kind=c_double), dimension(6) :: transform
      real(kind=dp), dimension(4, 2) :: results
      real(kind=dp), dimension(4) :: expected
      type(tree_data), pointer :: bnd_ptr, block_ptr
      integer :: format_index, point_index, status
      logical :: success

      values = reshape([10.0_dp, 22.0_dp, -9999.0_dp, 2.0_dp, 6.0_dp, 18.0_dp], shape(values))
      transform = [0.0_dp, 2.0_dp, 0.0_dp, 4.0_dp, 0.0_dp, -2.0_dp]
      if (.not. create_test_geotiff(trim(raster_files(2)), values, transform, gdt_float64, -9999.0_dp)) then
         return
      end if
      call create_file(trim(raster_files(1)), [character(len=32) :: &
                                              'ncols 3', 'nrows 2', 'xllcorner 0', 'yllcorner 0', &
                                              'cellsize 2', 'NODATA_value -9999', '10 22 -9999', '2 6 18'])

      ndx2D = 0
      call setup_minimal_grid_with_points(4)
      xz = [1.5_dp, 2.0_dp, 3.5_dp, 7.0_dp]
      yz = 2.0_dp
      call realloc(bl, ndx, fill=0.0_dp, keepExisting=.false.)
      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()
      status = m_polygon_destructor()
      expected = [8.0_dp, 10.0_dp, dmiss, dmiss]

      do format_index = 1, size(raster_files)
         call realloc(s1, ndx, fill=dmiss, keepExisting=.false.)
         call realloc(hs, ndx, fill=0.0_dp, keepExisting=.false.)
         call create_file(ext_file, [character(len=80) :: &
                                     '[Spatial]', 'quantity = initialwaterlevel', &
                                     'forcingFile = '//trim(raster_files(format_index)), &
                                     'forcingFileType = '//trim(file_types(format_index)), &
                                     'interpolationMethod = bilinear'])
         call parse_spatial_block(ext_file, bnd_ptr, block_ptr)
         success = init_spatial_fields(block_ptr, '.', ext_file, 'Spatial')
         call tree_destroy(bnd_ptr)
         call f90_expect_true(success, trim(file_types(format_index))//' initial field should succeed')
         results(:, format_index) = s1
      end do

      call f90_expect_true(all((results(:, 1) == dmiss) .eqv. (results(:, 2) == dmiss)), &
                           'ArcInfo and GeoTIFF should have identical missing-value masks')
      do point_index = 1, size(expected)
         call f90_expect_near(results(point_index, 1), expected(point_index), 1.0e-12_dp)
         call f90_expect_near(results(point_index, 2), results(point_index, 1), 1.0e-12_dp)
      end do

      ndxi = 0
      ndx2D = 0
      deallocate (bl, s1, hs)
      call teardown_minimal_grid()
      call delete_test_geotiff(trim(raster_files(1)))
      call delete_test_geotiff(trim(raster_files(2)))
      call delete_test_geotiff(ext_file)
   end subroutine test_bilinear_arcinfo_geotiff_initial_field
   !$f90tw)

   function create_test_geotiff(filename, values, transform, data_type, file_missing, is_point) result(success)
      character(len=*), intent(in) :: filename
      real(kind=c_double), dimension(:, :, :), intent(inout) :: values
      real(kind=c_double), dimension(6), intent(inout) :: transform
      integer, intent(in) :: data_type
      real(kind=c_double), intent(in), optional :: file_missing
      logical, intent(in), optional :: is_point
      logical :: success
      type(gdaldriverh) :: driver
      type(gdaldataseth) :: dataset
      integer :: status

      success = .false.
      call gdalallregister()
      driver = gdalgetdriverbyname('GTiff'//c_null_char)
      call f90_expect_true(gdalassociated(driver))
      if (.not. gdalassociated(driver)) then
         return
      end if
      dataset = gdalcreate(driver, filename//c_null_char, size(values, 1), size(values, 2), size(values, 3), &
                           data_type, c_null_ptr)
      call f90_expect_true(gdalassociated(dataset))
      if (.not. gdalassociated(dataset)) then
         return
      end if
      status = gdalsetgeotransform(dataset, transform)
      call f90_expect_eq(status, 0)
      success = status == 0
      if (present(file_missing)) then
         status = gdalsetrasternodatavalue(gdalgetrasterband(dataset, 1), file_missing)
         call f90_expect_eq(status, 0)
         success = success .and. status == 0
      end if
      if (present(is_point)) then
         if (is_point) then
            status = gdalsetmetadataitem(gdalmajorobjecth_new(dataset), 'AREA_OR_POINT'//c_null_char, &
                                         'Point'//c_null_char, c_null_char)
            call f90_expect_eq(status, 0)
            success = success .and. status == 0
         end if
      end if
      status = gdaldatasetrasterio_f(dataset, gf_write, 0, 0, values)
      call f90_expect_eq(status, 0)
      success = success .and. status == 0
      call gdalclose(dataset)
   end function create_test_geotiff

   subroutine delete_test_geotiff(filename)
      character(len=*), intent(in) :: filename
      integer :: file_unit, status

      open (newunit=file_unit, file=filename, status='old', access='stream', form='unformatted', iostat=status)
      call f90_expect_eq(status, 0)
      if (status == 0) then
         close (file_unit, status='delete')
      end if
   end subroutine delete_test_geotiff

end module test_bilinear_geotiff