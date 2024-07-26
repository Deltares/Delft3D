!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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

#include "rr_version.h"
   
   module rr_dll_version_module
!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2013.
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
!  $Id: rr_dll_version.F90.svn 54645 2018-02-23 14:58:20Z zeekant $
!  $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/engines/rr/packages/rr_dll/src/rr_dll_version.F90.svn $
    use iso_c_binding

    implicit none

    character(*),  public, parameter :: rr_dll_major        = MAJOR_STR
    character(*),  public, parameter :: rr_dll_minor        = MINOR_STR
    character(*),  public, parameter :: rr_dll_revision     = REVISION_STR
    character(*),  public, parameter :: rr_dll_build_number = BUILD_NR

#if defined(W32)
    character(*),  public, parameter :: rr_dll_architecture = '(Win32)'
#elif defined(X64)
    character(*),  public, parameter :: rr_dll_architecture = '(Win64)'
#elif defined(LINUX32)
    character(*),  public, parameter :: rr_dll_architecture = '(Linux32)'
#elif defined(LINUX64)
    character(*),  public, parameter :: rr_dll_architecture = '(Linux64)'
#else
    character(*),  public, parameter :: rr_dll_architecture = '(Unknown)'
#endif

    character(*),  public, parameter :: rr_dll_company      = COMPANY_NAME
    character(*),  public, parameter :: rr_dll_company_url  = COMPANY_URL
    character(*),  public, parameter :: rr_dll_program      = PRODUCT_NAME
    character(*),  public, parameter :: rr_dll_programname  = PRODUCT_NAME  ! use in about box and window title

    character(*),  public, parameter :: rr_dll_version      = trim(rr_dll_major)//'.'//trim(rr_dll_minor)//'.'//trim(rr_dll_revision)//'.'//trim(rr_dll_build_number)
    character(*),  public, parameter :: rr_dll_version_full = rr_dll_company//', '//rr_dll_program//' Version '//rr_dll_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: rr_dll_version_id   = '@(#)'//rr_dll_version_full

contains

    subroutine getfullversionstring_rr_dll(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(rr_dll_version_full),len(stringout))
        stringout = rr_dll_version_id(5:5+length-1)
    end subroutine getfullversionstring_rr_dll

    subroutine getprogramnamestring_rr_dll(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rr_dll_programname)
    end subroutine getprogramnamestring_rr_dll

    subroutine getshortprogramnamestring_rr_dll(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rr_dll_program)
    end subroutine getshortprogramnamestring_rr_dll

    subroutine getfeaturenumberstring_rr_dll(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rr_dll_version)
    end subroutine getfeaturenumberstring_rr_dll

    subroutine getversionnumberstring_rr_dll(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rr_dll_version)
    end subroutine getversionnumberstring_rr_dll

    subroutine getcompanystring_rr_dll(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rr_dll_company)
    end subroutine getcompanystring_rr_dll

    subroutine getsvnrevisionstring_rr_dll(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rr_dll_build_number)
    end subroutine getsvnrevisionstring_rr_dll

    subroutine getarchitecturestring_rr_dll(stringout)
        character(*), intent(out) :: stringout

#if defined(WIN32)
        stringout = trim('Win32')
#elif defined(WIN64)
        stringout = trim('Win64')
#else
        if (c_size_t == 4) then
            stringout = trim('Linux32')
        elseif (c_size_t == 8) then
            stringout = trim('Linux64')
        else
            stringout = trim('Unknown')
        end if
#endif

    end subroutine getarchitecturestring_rr_dll

subroutine get_rr_dll_sbkrel(sbkrel)

   integer, intent(out)      :: sbkrel(3)

   !
   ! body

   sbkrel(1) = 3
   sbkrel(2) = 216
   sbkrel(3) = 65

end subroutine get_rr_dll_sbkrel

end module rr_dll_version_module
