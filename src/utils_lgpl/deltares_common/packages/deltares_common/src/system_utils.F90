module system_utils
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

#if (defined(__linux__))
    character(5), parameter :: ARCH = 'linux'
    character(3), parameter :: SCRIPT_EXTENSION = '.sh'
    character(3), parameter :: SHARED_LIB_PREFIX = 'lib'
    character(3), parameter :: SHARED_LIB_EXTENSION = '.so'
    character(1), parameter :: FILESEP = '/'

    character(1), parameter :: FILESEP_OTHER_ARCH = '\'
#else    
    character(7), parameter :: ARCH = 'windows'
    character(4), parameter :: SCRIPT_EXTENSION = '.bat'
    character(0), parameter :: SHARED_LIB_PREFIX = ''
    character(4), parameter :: SHARED_LIB_EXTENSION = '.dll'
    character(1), parameter :: FILESEP = '\'

    character(1), parameter :: FILESEP_OTHER_ARCH = '/'
#endif    

contains


function cat_filename(path, file, ext) result(name)
!!--description-----------------------------------------------------------------
!
!    Function: A function to concatenate a path and file name into an extended
!              file specification.
!
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Arguments
    !
    character(*)          , intent(in) :: path   ! Path name
    character(*)          , intent(in) :: file   ! File name
    character(*), optional, intent(in) :: ext    ! File name extension
    character(1024)                    :: name   ! Full name of file (path,file,ext)
    !
    ! Local variables
    !
    integer      :: lenpath ! length of path name
    character(1) :: sep     ! separator
!
!! executable statements -------------------------------------------------------
!
    ! don't create a name out of an empty name
    if (file == ' ') then
       name = ' '
       return
    endif
    !
    lenpath = len_trim(path)
    sep = ' '
    if (lenpath>0) then
       if (path(lenpath:lenpath) /= FILESEP                      &
#ifndef HAVE_CONFIG_H
          ! on Windows also check forward slash
          & .and. path(lenpath:lenpath) /= '/'                   &
#endif
          &) then
          sep = FILESEP
       endif
    endif
    name = trim(path) // trim(sep) // file
    if (present(ext)) then
       name = trim(name) // ext
    endif
end function cat_filename


subroutine split_filename(name, path, file, ext)
!!--description-----------------------------------------------------------------
!
!    Function: A subroutine to split a full file name into a path, file name
!              and file name extension.
!
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Arguments
    !
    character(*)          , intent(in)  :: name   ! Full name of file (path,file,ext)
    character(*)          , intent(out) :: path   ! Path name
    character(*)          , intent(out) :: file   ! File name (excluding extension if ext is present)
    character(*), optional, intent(out) :: ext    ! File name extension
    !
    ! Local variables
    !
    integer    :: ifilesep   ! index of last file separator
    integer    :: idot       ! index of last dot
!
!! executable statements -------------------------------------------------------
!
    ! find last file separator
    ifilesep = index(name, FILESEP, back=.true.)
#ifndef HAVE_CONFIG_H
    ! on Windows also check forward slash
    ifilesep = max(ifilesep,index(name, '/', back=.true.))
#endif
    !
    ! split name
    if (ifilesep>0) then
       path = name(1:ifilesep)
    else
       path = ' '
    endif
    if (present(ext)) then
       ! find last dot
       idot = index(name, '.', back=.true.)
       if (idot>ifilesep) then
          file = name(ifilesep+1:idot-1)
          ext  = name(idot:len_trim(name))
       else
          file = name(ifilesep+1:len_trim(name))
          ext = ' '
       endif
    else
       file = name(ifilesep+1:len_trim(name))
    endif
end subroutine split_filename

subroutine remove_path(name, file)
!!--description-----------------------------------------------------------------
!
!    Function: A subroutine to remove the path from a full file name and return
!              a file name with extension.
!
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Arguments
    !
    character(*)          , intent(in)  :: name   ! Full name of file (path,file,ext)
    character(*)          , intent(out) :: file   ! File name (including extension if ext is present)
    !
    ! Local variables
    !
    integer    :: ifilesep   ! index of last file separator
!
!! executable statements -------------------------------------------------------
!
    ! find last file separator
    ifilesep = index(name, FILESEP, back=.true.)
#ifndef HAVE_CONFIG_H
    ! on Windows also check forward slash
    ifilesep = max(ifilesep,index(name, '/', back=.true.))
#endif
    !
    ! file name with extention
    file = name(ifilesep+1:len_trim(name))
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
    implicit none
    !
    ! Arguments
    !
    integer  , optional  :: unit   ! File unit number for 
    logical              :: exifil
    character(*)         :: name   ! Name of file
    !
    ! Local variables
    !
    integer    :: ipos   ! Help var. 
    logical    :: ex     ! Help flag = TRUE when file is found 
!
!! executable statements -------------------------------------------------------
!
    call remove_leading_spaces(name , ipos)
    !
    inquire (file = name(:ipos), exist = ex)
    if (.not.ex) then
       if (present(unit)) then
          call write_error(FILE_NOT_FOUND // trim(name), unit = unit)
       endif
       !
       exifil = .false.
    else
       exifil = .true.
    endif
end function exifil

function makedir(dirname) result(istat)
!!--description-----------------------------------------------------------------
!
!    Function: An integer function that creates a directory (also for linux)
!              when it does not yet exist.
!              Returns the error status from the 'system' command.
!
!!--declarations----------------------------------------------------------------

#ifdef __INTEL_COMPILER
    use ifport
#endif
    implicit none
    character(len=*), intent(in) :: dirname

    character(len=256)           :: command
    integer                      :: istat
    logical                      :: l_exist
    integer                      :: lslash
    character(len=999)           :: pathstr
    character(len=1)             :: slash
!
!! executable statements -------------------------------------------------------
!
    istat = 0

    call get_environment_variable('PATH',pathstr)
   
    slash = char(47)
    lslash = index (pathstr,slash)
    if (lslash == 0) then
       slash = char(92)
    endif

#ifdef __INTEL_COMPILER
    inquire(directory = trim(dirname), exist = l_exist)
#else
    ! GNU
    inquire(file = trim(dirname)//slash//".", exist = l_exist)
#endif
    if (l_exist) then
       return
    end if

    if ( slash == char(47)) then
!      linux
       command = "mkdir -p "//trim(dirname)
    else
!      windows
       command = "mkdir "//trim(dirname)
    end if

    istat = system(command)
    ! Fortran2008, not available before Intel 15:
    ! call execute_command_line(command)
      
    return
   end function

!> Return .true. if path is an absolute pathname.
!! On Unix, that means it begins with a slash, on Windows that it begins
!! with a (back)slash after chopping off a potential drive letter.
logical function is_abs(path)
   character(len=*), intent(in   ) :: path !< Input path

   integer :: idrive ! last char position of possible drive letter start, e.g. 'D:'
#ifdef HAVE_CONFIG_H
   is_abs = (path(1:1) == FILESEP)
#else
   idrive = index(path, ':') ! Find piece after drive letter:. When not found, still check from index 1, because it might start with / for Windows UNC paths \\share\etc.
   is_abs = (path(idrive+1:idrive+1) == FILESEP .or. path(idrive+1:idrive+1) == '/') ! On Windows, also allow forward lash.
#endif

end function is_abs

!> find the last slash in a string.
!! can a forward or a backward slash
!! returns 0 if not found
function find_last_slash(path) result (ipos)
   character(len=*), intent(in) :: path  !< string with a path including slash(es)
   integer                      :: ipos  !< position of slash

   ipos = max(index(path,'\', .true.), index(path,'/', .true.))

end function find_last_slash

end module system_utils
