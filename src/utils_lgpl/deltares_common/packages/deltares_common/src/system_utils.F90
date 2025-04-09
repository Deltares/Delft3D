!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
!
!   Support for low level system routines
!
!-------------------------------------------------------------------------------
!
module system_utils
   implicit none
   private

#if (defined(__linux__))
   character(5), parameter, public :: ARCH = 'linux'
   character(3), parameter, public :: SCRIPT_EXTENSION = '.sh'
   character(3), parameter, public :: SHARED_LIB_PREFIX = 'lib'
   character(3), parameter, public :: SHARED_LIB_EXTENSION = '.so'
   character(1), parameter, public :: FILESEP = '/'

   character(1), parameter, public :: FILESEP_OTHER_ARCH = '\'
#else
   character(7), parameter, public :: ARCH = 'windows'
   character(4), parameter, public :: SCRIPT_EXTENSION = '.bat'
   character(0), parameter, public :: SHARED_LIB_PREFIX = ''
   character(4), parameter, public :: SHARED_LIB_EXTENSION = '.dll'
   character(1), parameter, public :: FILESEP = '\'

   character(1), parameter, public :: FILESEP_OTHER_ARCH = '/'
#endif

   public :: cat_filename
   public :: split_filename
   public :: remove_path
   public :: exifil
   public :: directory_exists
   public :: makedir
   public :: is_abs
   public :: find_last_slash
   public :: get_executable_directory

   contains
   
   subroutine get_executable_directory(exe_path, ierr) 
        character(len=*), intent(out) :: exe_path
        integer, intent(out) :: ierr
        character(len=1024) :: exe_fullpath, exe_name
        
      call get_command_argument(0, exe_fullpath, status=ierr)
      if (ierr /= 0) then
         call get_executable_path_symlink(exe_fullpath, ierr)
         if (ierr /= 0) then
            return
         end if
      end if
      call split_filename(exe_fullpath, exe_path, exe_name)
   end subroutine
   
   subroutine get_executable_path_symlink(exe_fullpath, ierr)
      use iso_c_binding
      character(len=*), intent(out) :: exe_fullpath
      integer, intent(out) :: ierr
      character(kind=c_char) :: cbuf(1024)
      integer(c_size_t) :: szret
      integer :: i

      interface
         function readlink(path, buf, bufsize) bind(C, name='readlink')
            import :: c_char, c_size_t
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char) :: buf(*)
            integer(c_size_t), value :: bufsize
            integer(c_size_t) :: readlink
         end function readlink
      end interface
      ierr = 0
      exe_fullpath = ''
#if (defined(__linux__))
      szret = readlink('/proc/self/exe'//c_null_char, cbuf, size(cbuf, kind=c_size_t))
      if (szret == -1) then
         ierr = -1
         exe_fullpath = ''
         return
      end if

      exe_fullpath = ''
      do i = 1, szret
         exe_fullpath(i:i) = cbuf(i)
      end do


#endif      
   end subroutine get_executable_path_symlink

   function cat_filename(path, file, ext) result(name)
!!--description-----------------------------------------------------------------
!
!    Function: A function to concatenate a path and file name into an extended
!              file specification.
!
!!--declarations----------------------------------------------------------------
      !
      ! Arguments
      !
      character(*), intent(in) :: path ! Path name
      character(*), intent(in) :: file ! File name
      character(*), optional, intent(in) :: ext ! File name extension
      character(1024) :: name ! Full name of file (path,file,ext)
      !
      ! Local variables
      !
      integer :: lenpath ! length of path name
      character(1) :: sep ! separator
      logical :: path_ends_with_sep
!
!! executable statements -------------------------------------------------------
!
      ! don't create a name out of an empty name
      if (file == ' ') then
         name = ' '
         return
      end if
      !
      lenpath = len_trim(path)
      sep = ' '
      if (lenpath > 0) then
         ! on Windows also check forward slash
         path_ends_with_sep = path(lenpath:lenpath) == FILESEP .or. (ARCH == 'windows' .and. path(lenpath:lenpath) == FILESEP_OTHER_ARCH)
         if (.not. path_ends_with_sep) then
            sep = FILESEP
         end if
      end if
      name = trim(path)//trim(sep)//file
      if (present(ext)) then
         name = trim(name)//ext
      end if
   end function cat_filename

   subroutine split_filename(name, path, file, ext)
!!--description-----------------------------------------------------------------
!
!    Function: A subroutine to split a full file name into a path, file name
!              and file name extension.
!
!!--declarations----------------------------------------------------------------
      !
      ! Arguments
      !
      character(*), intent(in) :: name ! Full name of file (path,file,ext)
      character(*), intent(out) :: path ! Path name
      character(*), intent(out) :: file ! File name (excluding extension if ext is present)
      character(*), optional, intent(out) :: ext ! File name extension
      !
      ! Local variables
      !
      integer :: ifilesep ! index of last file separator
      integer :: idot ! index of last dot
!
!! executable statements -------------------------------------------------------
!
      ! find last file separator
      ifilesep = index(name, FILESEP, back=.true.)
      if (ARCH == 'windows') then
         ! on Windows also check forward slash
         ifilesep = max(ifilesep, index(name, FILESEP_OTHER_ARCH, back=.true.))
      end if
      !
      ! split name
      if (ifilesep > 0) then
         path = name(1:ifilesep)
      else
         path = ' '
      end if
      if (present(ext)) then
         ! find last dot
         idot = index(name, '.', back=.true.)
         if (idot > ifilesep) then
            file = name(ifilesep + 1:idot - 1)
            ext = name(idot:len_trim(name))
         else
            file = name(ifilesep + 1:len_trim(name))
            ext = ' '
         end if
      else
         file = name(ifilesep + 1:len_trim(name))
      end if
   end subroutine split_filename

   subroutine remove_path(name, file)
!!--description-----------------------------------------------------------------
!
!    Function: A subroutine to remove the path from a full file name and return
!              a file name with extension.
!
!!--declarations----------------------------------------------------------------
      !
      ! Arguments
      !
      character(*), intent(in) :: name ! Full name of file (path,file,ext)
      character(*), intent(out) :: file ! File name (including extension if ext is present)
      !
      ! Local variables
      !
      integer :: ifilesep ! index of last file separator
!
!! executable statements -------------------------------------------------------
!
      ! find last file separator
      ifilesep = index(name, FILESEP, back=.true.)
      if (ARCH == 'windows') then
         ! on Windows also check forward slash
         ifilesep = max(ifilesep, index(name, FILESEP_OTHER_ARCH, back=.true.))
      end if
      !
      ! file name with extention
      file = name(ifilesep + 1:len_trim(name))
   end subroutine remove_path

   function exifil(name, unit)
!!--description-----------------------------------------------------------------
!
!    Function: A logical function which checks the existence of a
!              specified file (path may be included). Set to TRUE
!              when the file is found, FALSE otherwise.
!
!!--declarations----------------------------------------------------------------
      use string_module
      use message_module
      !
      ! Arguments
      !
      integer, optional :: unit ! File unit number for
      logical :: exifil
      character(*) :: name ! Name of file
      !
      ! Local variables
      !
      integer :: ipos ! Help var.
      logical :: ex ! Help flag = TRUE when file is found
!
!! executable statements -------------------------------------------------------
!
      call remove_leading_spaces(name, ipos)
      !
      inquire (file=name(:ipos), exist=ex)
      if (.not. ex) then
         if (present(unit)) then
            call write_error(FILE_NOT_FOUND//trim(name), unit=unit)
         end if
         !
         exifil = .false.
      else
         exifil = .true.
      end if
   end function exifil

!> Test if directory exists
   function directory_exists(dir_name)
      character(len=*), intent(in) :: dir_name !< Name of the directory
      logical :: directory_exists

      character(len=:), allocatable :: sanitized_dir_name

!! executable statements -------------------------------------------------------
      sanitized_dir_name = sanitize_path(dir_name)

#ifdef __INTEL_COMPILER
      inquire (directory=trim(sanitized_dir_name), exist=directory_exists)
#else
      ! GNU
      inquire (file=trim(sanitized_dir_name)//FILESEP//".", exist=directory_exists)
#endif
   end function directory_exists

!> Replace slashes by the OS-specific path separators
   function sanitize_path(path) result(sanitized_path)
      use string_module, only: replace_char
      character(len=*), intent(in) :: path !< The path to be sanitized
      character(len=len(path)) :: sanitized_path

      sanitized_path = path
      call replace_char(sanitized_path, ichar(FILESEP_OTHER_ARCH), ichar(FILESEP))
   end function sanitize_path

   subroutine makedir(dir_name)
!!--description-----------------------------------------------------------------
!
!    Function: An integer function that creates a directory (also for linux)
!              when it does not yet exist.
!
!!--declarations----------------------------------------------------------------

      use MessageHandling, only: err
      character(len=*), intent(in) :: dir_name

      character(len=:), allocatable :: command, sanitized_dir_name
      integer :: istat
!
!! executable statements -------------------------------------------------------

      sanitized_dir_name = sanitize_path(dir_name)

      if (directory_exists(sanitized_dir_name)) then
         return
      end if

      if (ARCH == 'linux') then
         command = 'mkdir -p '//trim(sanitized_dir_name)
      else if (ARCH == 'windows') then
         command = 'mkdir '//trim(sanitized_dir_name)
      else
         call err('makedir could not determine the system architecture "'//trim(ARCH)//'".')
      end if

      call execute_command_line(command, exitstat=istat)
      if (istat /= 0) then
         ! Multiple processes could have attempted to create the directory
         if (.not. directory_exists(sanitized_dir_name)) then
            call err('Cannot create output directory "'//trim(sanitized_dir_name)//'".')
         end if
      end if
   end subroutine makedir

!> Return .true. if path is an absolute pathname.
!! On Unix, that means it begins with a slash, on Windows that it begins
!! with a (back)slash after chopping off a potential drive letter.
   logical function is_abs(path)
      character(len=*), intent(in) :: path !< Input path

      integer :: idrive ! last char position of possible drive letter start, e.g. 'D:'
      if (ARCH == 'linux') then
         is_abs = (path(1:1) == FILESEP)
      else
         idrive = index(path, ':') ! Find piece after drive letter:. When not found, still check from index 1, because it might start with / for Windows UNC paths \\share\etc.
         is_abs = (path(idrive + 1:idrive + 1) == FILESEP .or. path(idrive + 1:idrive + 1) == FILESEP_OTHER_ARCH) ! On Windows, also allow forward lash.
      end if
   end function is_abs

!> find the last slash in a string.
!! can a forward or a backward slash
!! returns 0 if not found
   function find_last_slash(path) result(ipos)
      character(len=*), intent(in) :: path !< string with a path including slash(es)
      integer :: ipos !< position of slash

      ipos = max(index(path, '\', .true.), index(path, '/', .true.))

   end function find_last_slash

end module system_utils
