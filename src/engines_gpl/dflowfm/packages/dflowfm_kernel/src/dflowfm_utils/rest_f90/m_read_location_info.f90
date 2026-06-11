module m_read_location_info
   implicit none(type, external)
   private
   public :: read_polyline_coordinates

contains
!> Generic reader for polyline coordinates from either a locationFile (pli/pliz) or inline keys
   !! (numCoordinates + xCoordinates + yCoordinates + optionally zCoordinates).
   !! Can be reused by source/sinks, long culverts, or any other feature that needs polyline input.
   subroutine read_polyline_coordinates(block_ptr, object_id, file_name, base_dir, group_name, x_coordinates, y_coordinates, z_coordinates, num_columns, is_successful)
      use messageHandling, only: err_flush, msgbuf
      use tree_data_types, only: tree_data
      use properties, only: prop_get
      use m_missing, only: dmiss
      use m_filez, only: oldfil
      use m_polygon, only: xpl, ypl, zpl, npl, dzL, colpl, m_polygon_destructor
      use precision, only: dp
      use m_reapol, only: reapol
      use system_utils, only: split_filename
      use unstruc_files, only: resolvePath

      type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to ini-file block; child node of a file tree
      character(len=*), intent(in) :: object_id !< Id of the object being read, used in error messages
      character(len=*), intent(in) :: file_name !< Name of the input file, only used in error messages
      character(len=*), intent(in) :: base_dir !< Base directory of the input file, used to resolve relative paths
      character(len=*), intent(in) :: group_name !< Name of the block type, only used in error messages

      real(kind=dp), dimension(:), allocatable, intent(out) :: x_coordinates !< x-coordinates read from file or inline
      real(kind=dp), dimension(:), allocatable, intent(out) :: y_coordinates !< y-coordinates read from file or inline
      real(kind=dp), dimension(:), allocatable, intent(out) :: z_coordinates !< z-coordinates: from 3rd column of pliz, from zCoordinates key, or dmiss if unavailable
      integer, intent(out) :: num_columns !< Number of columns found in polyline file (0 if read inline)
      logical, intent(out) :: is_successful

      character(len=256) :: location_file
      integer :: num_coordinates
      integer :: ierr
      integer :: polyline_file_lun
      logical :: is_read
      logical :: have_location_file

      is_successful = .false.
      num_columns = 0
      call prop_get(block_ptr, '', 'locationFile', location_file, have_location_file)
      if (have_location_file) then
         ! Read data from polyline file (pli or pliz)
         call resolvePath(location_file, base_dir)
         call oldfil(polyline_file_lun, location_file)
         if (polyline_file_lun == 0) then
            write (msgbuf, '(a)') trim(file_name)//" '"//trim(group_name)//" '"//trim(object_id)//"': failed to read polyline file '"//trim(location_file)//"'"
            call err_flush()
            return
         end if
         ierr = m_polygon_destructor()
         call reapol(polyline_file_lun, 0)
         if (npl == 0) then
            write (msgbuf, '(a)') trim(file_name)//" '"//trim(group_name)//" '"//trim(object_id)//"': no data in polyline file '"//trim(location_file)//"'"
            call err_flush()
            return
         end if

         num_columns = colpl
         allocate (x_coordinates(npl), stat=ierr)
         allocate (y_coordinates(npl), stat=ierr)
         allocate (z_coordinates(npl), stat=ierr)
         x_coordinates = xpl(1:npl)
         y_coordinates = ypl(1:npl)
         z_coordinates = zpl(1:npl) ! Will be dmiss if file has no z column
      else
         ! Read data directly from block
         call prop_get(block_ptr, '', 'numCoordinates', num_coordinates, is_read)
         if (.not. is_read .or. num_coordinates <= 0) then
            if (is_read .and. num_coordinates <= 0) then
               write (msgbuf, '(a)') trim(group_name)//" '"//trim(object_id)//"': numCoordinates must be greater than 0."
               call err_flush()
            else
               write (msgbuf, '(a)') 'Incomplete block in file '''//trim(file_name)//''': ['//trim(group_name)// &
                  ']. Location information is incomplete or missing.'
               call err_flush()
            end if
            return
         end if

         allocate (x_coordinates(num_coordinates), stat=ierr)
         call prop_get(block_ptr, '', 'xCoordinates', x_coordinates, num_coordinates, is_read)
         if (.not. is_read) then
            write (msgbuf, '(a)') trim(group_name)//" '"//trim(object_id)//"': xCoordinates not found."
            call err_flush()
            return
         end if

         allocate (y_coordinates(num_coordinates), stat=ierr)
         call prop_get(block_ptr, '', 'yCoordinates', y_coordinates, num_coordinates, is_read)
         if (.not. is_read) then
            write (msgbuf, '(a)') trim(group_name)//" '"//trim(object_id)//"': yCoordinates not found."
            call err_flush()
            return
         end if

         allocate (z_coordinates(num_coordinates), stat=ierr)
         z_coordinates = dmiss
         call prop_get(block_ptr, '', 'zCoordinates', z_coordinates, num_coordinates, is_read)
         ! zCoordinates is optional: no error if missing, z_coordinates stays dmiss
      end if

      is_successful = .true.
   end subroutine read_polyline_coordinates

end module m_read_location_info
