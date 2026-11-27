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
   !! The input filename's final extension is replaced with .cdl and .nc respectively.
   subroutine create_netcdf_from_cdl(nc_file, lines, success)
      use, intrinsic :: iso_fortran_env, only: error_unit
      use system_utils, only: split_filename

      character(len=*), intent(in) :: nc_file !< Test-controlled relative filename, without shell expansion syntax or double quotes.
      character(len=*), dimension(:), intent(in) :: lines !< CDL file contents.
      logical, optional, intent(out) :: success !< Whether ncgen completed successfully.

      character(len=len(nc_file)) :: directory, stem, extension
      character(len=:), allocatable :: base_path, cdl_file, output_file
      character(len=512) :: cmdmsg
      integer :: cmdstat, exitstat

      if (present(success)) then
         success = .false.
      end if
      call split_filename(nc_file, directory, stem, extension)
      if (len_trim(stem) == 0) then
         write (error_unit, '(2a)') "Expected a NetCDF fixture filename with a basename: ", trim(nc_file)
         return
      end if
      base_path = trim(directory)//trim(stem)
      cdl_file = base_path//".cdl"
      output_file = base_path//".nc"

      cmdmsg = ""
      call create_file(cdl_file, lines)
      call execute_command_line( &
         'ncgen -o "'//output_file//'" "'//cdl_file//'"', &
         cmdstat=cmdstat, cmdmsg=cmdmsg, exitstat=exitstat)
      if (cmdstat /= 0) then
         write (error_unit, '(3a,i0,2a)') "Could not run ncgen for ", output_file, " (cmdstat=", cmdstat, "): ", trim(cmdmsg)
         return
      end if
      if (exitstat /= 0) then
         write (error_unit, '(3a,i0)') "ncgen failed for ", output_file, " with exit status ", exitstat
         return
      end if
      if (present(success)) then
         success = .true.
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
