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
      procedure :: interpolate => bilinear_rectangular_point
   end type bilinear_data

   type(bilinear_data) :: arcinfo_data !< Shared interpolation data refreshed from the legacy ArcInfo globals for refinement.

   interface bilinear_data
      module procedure read_bilinear_data
   end interface bilinear_data

contains

   !> Construct bilinear interpolation data from a file without changing global sample data.
   function read_bilinear_data(filename, filetype, status) result(data)
      use timespace_parameters, only: ARCINFO
      use m_read_arcinfo, only: readarcinfoheader, readarcinfoblock
      use m_missing, only: dmiss

      character(len=*), intent(in) :: filename !< Input raster filename.
      integer, intent(in) :: filetype !< Input format; currently only ARCINFO is supported.
      integer, intent(out) :: status !< Zero on success, nonzero on unsupported format or read failure.
      type(bilinear_data) :: data !< Loaded interpolation data; values remain unallocated on failure.

      integer :: file_unit, num_columns, num_rows, close_status
      real(kind=dp) :: file_missing

      data%missing_value = dmiss
      status = 1
      if (filetype /= ARCINFO) then
         return
      end if

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
   end function read_bilinear_data

   !> Bilinear interpolation of one point from a uniform rectangular raster with explicit inputs.
   elemental function bilinear_rectangular_point(raster, x, y) result(z)
      real(kind=dp), intent(in) :: x, y !< Target coordinates.
      class(bilinear_data), intent(in) :: raster !< Populated data with nonzero signed spacing.
      real(kind=dp) :: z !< Interpolated value or the raster's missing-value marker.

      real(kind=dp) :: dm, dn, am, an
      integer :: column_index, row_index

      dm = (x - raster%origin(1)) / raster%spacing(1)
      column_index = int(dm)
      am = dm - column_index
      column_index = column_index + 1
      dn = (y - raster%origin(2)) / raster%spacing(2)
      row_index = int(dn)
      an = dn - row_index
      row_index = row_index + 1
      z = raster%missing_value
      if (column_index < size(raster%values, 1) .and. row_index < size(raster%values, 2) .and. &
          column_index >= 1 .and. row_index >= 1) then
         if (raster%values(column_index, row_index) /= raster%missing_value .and. &
             raster%values(column_index + 1, row_index) /= raster%missing_value .and. &
             raster%values(column_index, row_index + 1) /= raster%missing_value .and. &
             raster%values(column_index + 1, row_index + 1) /= raster%missing_value) then
            z = am * an * raster%values(column_index + 1, row_index + 1) + &
                (1.0_dp - am) * an * raster%values(column_index, row_index + 1) + &
                (1.0_dp - am) * (1.0_dp - an) * raster%values(column_index, row_index) + &
                am * (1.0_dp - an) * raster%values(column_index + 1, row_index)
         end if
      end if
   end function bilinear_rectangular_point

end module m_bilinear_interpolation