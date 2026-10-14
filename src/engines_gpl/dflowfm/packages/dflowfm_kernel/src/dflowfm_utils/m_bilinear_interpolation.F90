!> Bilinear interpolation of uniform rectangular raster data.
module m_bilinear_interpolation
   use precision, only: dp

   implicit none(type, external)
   private

   public :: bilinear_data, arcinfo_data

   !> Raster values and geometry, with coordinates defined at sample locations.
   type :: bilinear_data
      real(kind=dp), dimension(:, :), allocatable :: values !< Raster values; array dimensions define the raster size.
      real(kind=dp), dimension(2) :: origin = 0.0_dp !< Coordinates of values(1,1).
      real(kind=dp), dimension(2) :: spacing = 0.0_dp !< Nonzero signed coordinate increments along the array dimensions when populated.
      real(kind=dp) :: missing_value = -999.0_dp !< Missing-value marker used in values.
   contains
      procedure :: interpolate => interpolate_bilinear
   end type bilinear_data

   type(bilinear_data) :: arcinfo_data !< Shared interpolation data refreshed from the legacy ArcInfo globals for refinement.

   interface bilinear_data
      module procedure read_bilinear_data
   end interface bilinear_data

contains

   !> Construct bilinear interpolation data from a file without changing global sample data.
   function read_bilinear_data(filename, filetype, status) result(data)
      use timespace_parameters, only: ARCINFO, GEOTIFF
      use m_read_arcinfo, only: readarcinfoheader, readarcinfoblock
      use m_missing, only: dmiss

      character(len=*), intent(in) :: filename !< Input raster filename.
      integer, intent(in) :: filetype !< Input format: ARCINFO or GEOTIFF.
      integer, intent(out) :: status !< Zero on success, nonzero on unsupported format or read failure.
      type(bilinear_data) :: data !< Loaded interpolation data; values remain unallocated on failure.

      integer :: file_unit, num_columns, num_rows, close_status
      real(kind=dp) :: file_missing

      data%missing_value = dmiss
      status = 1
      if (filetype == GEOTIFF) then
         call read_bilinear_geotiff(filename, data%values, data%origin, data%spacing, data%missing_value, status)
      else if (filetype == ARCINFO) then
         open (newunit=file_unit, file=filename, status='old', action='read', iostat=status)
         if (status /= 0) then
            return
         end if

         call readarcinfoheader(file_unit, num_columns, num_rows, data%origin(1), data%origin(2), &
                                data%spacing(1), data%spacing(2), file_missing, status)
         if (status == 0) then
            if (num_columns <= 0 .or. num_rows <= 0 .or. any(data%spacing == 0.0_dp)) then
               status = 1
            else
               allocate (data%values(num_columns, num_rows), stat=status)
               if (status == 0) then
                  call readarcinfoblock(file_unit, data%values, num_columns, num_rows, file_missing, status)
               end if
            end if
         end if

         close (file_unit, iostat=close_status)
         if (status == 0) then
            status = close_status
         end if
         if (status /= 0 .and. allocated(data%values)) then
            deallocate (data%values)
         end if
      end if
   end function read_bilinear_data

   !> Read bilinear raster values and pixel-centre geometry without modifying global samples.
   !! Rotation terms up to 1e-6 are ignored, matching the legacy sample reader.
   subroutine read_bilinear_geotiff(filename, values, origin, spacing, missing_value, status)
#ifdef HAVE_GDAL
      use iso_c_binding, only: c_int, c_double, c_null_char
      use ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
      use gdal, only: gdaldataseth, gdalallregister, gdalopen, ga_readonly, gdalassociated, gdalclose, &
                      gdalgetrasterxsize, gdalgetrasterysize, gdalgetrastercount, gdalgetrasterband, &
                      gdalgetgeotransform, gdaldatasetrasterio_f, gf_read, gdalgetrasternodatavalue, &
                      gdalgetrasterdatatype, gdt_byte, gdt_uint16, gdt_int16, gdt_uint32, gdt_int32, gdt_float32, gdt_float64
#endif
      character(len=*), intent(in) :: filename !< GeoTIFF filename.
      real(kind=dp), dimension(:, :), allocatable, intent(out) :: values !< Raster values in GDAL column/row order.
      real(kind=dp), dimension(2), intent(out) :: origin !< Coordinates of the first pixel centre.
      real(kind=dp), dimension(2), intent(out) :: spacing !< Signed coordinate increments.
      real(kind=dp), intent(in) :: missing_value !< Internal missing marker used to normalize NoData and nonfinite values.
      integer, intent(out) :: status !< Zero on success; nonzero on unsupported geometry, read failure, or missing GDAL.
#ifdef HAVE_GDAL
   real(kind=dp), parameter :: rotation_tolerance = 1.0e-6_dp
      type(gdaldataseth) :: dataset
      real(kind=c_double), dimension(:, :, :), allocatable :: buffer
      real(kind=c_double), dimension(6) :: geotransform
      real(kind=c_double) :: file_missing
      integer(kind=c_int) :: num_columns, num_rows, has_missing
#endif

      status = 1
      origin = 0.0_dp
      spacing = 0.0_dp
#ifdef HAVE_GDAL
      call gdalallregister()
      dataset = gdalopen(trim(filename)//c_null_char, ga_readonly)
      if (.not. gdalassociated(dataset)) then
         return
      end if

      do
         num_columns = gdalgetrasterxsize(dataset)
         num_rows = gdalgetrasterysize(dataset)
         if (num_columns <= 0 .or. num_rows <= 0 .or. gdalgetrastercount(dataset) /= 1) then
            exit
         end if
         select case (gdalgetrasterdatatype(gdalgetrasterband(dataset, 1)))
         case (gdt_byte, gdt_uint16, gdt_int16, gdt_uint32, gdt_int32, gdt_float32, gdt_float64)
         case default
            exit
         end select
         if (gdalgetgeotransform(dataset, geotransform) /= 0) then
            exit
         end if
         if (.not. all(ieee_is_finite(geotransform))) then
            exit
         end if
         if (geotransform(3) /= 0.0_dp .or. geotransform(5) /= 0.0_dp .or. &
             geotransform(2) == 0.0_dp .or. geotransform(6) == 0.0_dp) then
            exit
         end if

         allocate (buffer(num_columns, num_rows, 1), stat=status)
         if (status /= 0) then
            exit
         end if
         status = 1
         if (gdaldatasetrasterio_f(dataset, gf_read, 0_c_int, 0_c_int, buffer) /= 0) then
            exit
         end if
         allocate (values(num_columns, num_rows), stat=status)
         if (status /= 0) then
            exit
         end if
         values = buffer(:, :, 1)
         has_missing = 0
         file_missing = gdalgetrasternodatavalue(gdalgetrasterband(dataset, 1), has_missing)
         if (has_missing /= 0 .and. .not. ieee_is_nan(file_missing)) then
            where (values == file_missing)
               values = missing_value
            end where
         end if
         where (.not. ieee_is_finite(values))
            values = missing_value
         end where
         spacing = [geotransform(2), geotransform(6)]
         origin = [geotransform(1), geotransform(4)] + 0.5_dp * spacing
         status = 0
         exit
      end do
      call gdalclose(dataset)
      if (status /= 0 .and. allocated(values)) then
         deallocate (values)
      end if
#endif
   end subroutine read_bilinear_geotiff

   !> Bilinear interpolation of coordinates from a uniform rectangular raster.
   elemental function interpolate_bilinear(raster, x, y) result(res)
      real(kind=dp), intent(in) :: x, y !< Target coordinates.
      class(bilinear_data), intent(in) :: raster !< bilinear_data raster object
      real(kind=dp) :: res !< Interpolated value or the raster's missing-value marker.

      real(kind=dp) :: grid_x, grid_y, x_fraction, y_fraction
      real(kind=dp) :: x_complement, y_complement
      real(kind=dp) :: value00, value10, value01, value11
      integer :: column_index, row_index

      grid_x = (x - raster%origin(1)) / raster%spacing(1)
      grid_y = (y - raster%origin(2)) / raster%spacing(2)
      column_index = int(grid_x)
      row_index = int(grid_y)
      x_fraction = grid_x - column_index
      y_fraction = grid_y - row_index
      column_index = column_index + 1
      row_index = row_index + 1

      res = raster%missing_value
      if (column_index < 1 .or. column_index >= size(raster%values, 1) .or. &
          row_index < 1 .or. row_index >= size(raster%values, 2)) then
         return
      end if

      value00 = raster%values(column_index, row_index)
      value10 = raster%values(column_index + 1, row_index)
      value01 = raster%values(column_index, row_index + 1)
      value11 = raster%values(column_index + 1, row_index + 1)
      if (value00 == raster%missing_value .or. value10 == raster%missing_value .or. &
          value01 == raster%missing_value .or. value11 == raster%missing_value) then
         return
      end if

      x_complement = 1.0_dp - x_fraction
      y_complement = 1.0_dp - y_fraction
      res = x_fraction * y_fraction * value11 + &
          x_complement * y_fraction * value01 + &
          x_complement * y_complement * value00 + &
          x_fraction * y_complement * value10

   end function interpolate_bilinear

end module m_bilinear_interpolation