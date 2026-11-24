module m_file_helpers
   implicit none(type, external)

contains
   !> Create a file with the specified name and content. If the file already exists it will be replaced.
   subroutine create_file(file_name, lines)
      implicit none
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: lines(:)
      integer :: error_code, i, file_lun
      open (newunit=file_lun, file=file_name, status='replace', action='write', &
            form='formatted', iostat=error_code)
      if (error_code /= 0) then
         error stop "Failed to open file "//trim(file_name)
      end if
      do i = 1, size(lines)
         write (file_lun, '(A)', iostat=error_code) trim(lines(i))
         if (error_code /= 0) then
            close (file_lun)
            error stop "Failed to write to file "//trim(file_name)
         end if
      end do
      close (file_lun)
   end subroutine create_file

   !> Write a CDL fixture and convert it to NetCDF using ncgen from PATH.
   subroutine create_netcdf_from_cdl(nc_file, lines, cmdstat, exitstat, cmdmsg)
      ! Arguments
      character(len=*), intent(in) :: nc_file !< Path to the NetCDF file to generate.
      character(len=*), dimension(:), intent(in) :: lines !< CDL file contents.
      integer, intent(out) :: cmdstat !< Command execution status.
      integer, intent(out) :: exitstat !< ncgen exit status, or -1 if the command could not execute.
      character(len=*), intent(out) :: cmdmsg !< Diagnostic message if command execution fails.

      ! Local variables
      character(len=512) :: cdl_file !< Path to the CDL file to write.

      cdl_file = trim(nc_file)//".cdl"

      call create_file(cdl_file, lines)
      cmdmsg = ""
      call execute_command_line('ncgen -o "'//trim(nc_file)//'" "'//trim(cdl_file)//'"', &
                                wait=.true., cmdstat=cmdstat, cmdmsg=cmdmsg, exitstat=exitstat)
      if (cmdstat /= 0) then
         exitstat = -1
      end if
   end subroutine create_netcdf_from_cdl

   !> Create a dummy file with the specified content.
   subroutine create_scratch_file(lines, unit, iostat)
      character(len=*), intent(in) :: lines(:) !< Lines to write to the dummy file
      integer, intent(out) :: unit !< Unit number of the dummy file
      integer, optional, intent(out) :: iostat !< Error code returned by open

      integer :: i
      integer :: iostat_

      if (present(iostat)) then
         iostat = 0
      end if

      open(newunit=unit, status="scratch", iostat=iostat_)
      if (iostat_ /= 0) then
         if (present(iostat)) then
            iostat = iostat_
            return
         end if
      end if

      do i = 1, size(lines)
         write (unit, '(A)', iostat=iostat_) trim(lines(i))
         if (iostat_ /= 0) then
            if (present(iostat)) then
               iostat = iostat_
               return
            end if
         end if
      end do
      rewind(unit=unit, iostat=iostat_)
   end subroutine create_scratch_file

end module
