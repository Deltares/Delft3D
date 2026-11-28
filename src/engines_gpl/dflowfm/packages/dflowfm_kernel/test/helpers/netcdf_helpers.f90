module m_netcdf_helpers
   use m_file_helpers, only: create_file
   implicit none(type, external)
   private
   public create_netcdf_from_cdl
contains

   !> Write a CDL fixture and convert it to NetCDF using ncgen from PATH.
   !! The input filename's final extension is replaced with .cdl and .nc respectively.
   !! Only ASCII letters, digits, underscores, hyphens, dots, forward slashes and colons are allowed.
   subroutine create_netcdf_from_cdl(nc_file, lines, success)
      use, intrinsic :: iso_fortran_env, only: error_unit
      use system_utils, only: split_filename

      character(len=*), intent(in) :: nc_file !< Name of the netcdf file to generate. The suffix will be replaced by '.nc'
      character(len=*), dimension(:), intent(in) :: lines !< CDL file contents.
      logical, optional, intent(out) :: success !< Whether ncgen completed successfully.

      character(len=len(nc_file)) :: directory, stem, extension
      character(len=:), allocatable :: base_path, cdl_file, output_file
      character(len=512) :: cmdmsg
      integer :: cmdstat, exitstat

      if (present(success)) then
         success = .false.
      end if
      if (.not. is_shell_safe(nc_file)) then
         write (error_unit, '(a)') "NetCDF fixture filename may not contain special characters."
         return
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

   !> Only allow certain characters, because `nc_file` ends up in a shell command.
   pure logical function is_shell_safe(nc_file) result(is_safe)
      character(len=*), intent(in) :: nc_file !< Filename to validate.
      character(len=*), parameter :: ALLOWED_CHARACTERS = &
         "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-./:"

      is_safe = verify(trim(nc_file), ALLOWED_CHARACTERS) == 0
   end function is_shell_safe

end module m_netcdf_helpers
