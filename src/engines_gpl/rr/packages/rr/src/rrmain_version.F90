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
module rrmain_version_module

!  $Id: rrmain_version.F90.svn 54640 2018-02-23 13:59:07Z zeekant $
!  $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/engines/rr/packages/rr/src/rrmain_version.F90.svn $
    use iso_c_binding

    implicit none

    character(*),  public, parameter :: rrmain_major        = MAJOR_STR
    character(*),  public, parameter :: rrmain_minor        = MINOR_STR
    character(*),  public, parameter :: rrmain_revision     = REVISION_STR
    character(*),  public, parameter :: rrmain_build_number = BUILD_NR

#if defined(W32)
    character(*),  public, parameter :: rrmain_architecture = '(Win32)'
#elif defined(X64)
    character(*),  public, parameter :: rrmain_architecture = '(Win64)'
#elif defined(LINUX32)
    character(*),  public, parameter :: rrmain_architecture = '(Linux32)'
#elif defined(LINUX64)
    character(*),  public, parameter :: rrmain_architecture = '(Linux64)'
#else
    character(*),  public, parameter :: rrmain_architecture = '(Unknown)'
#endif

    character(*),  public, parameter :: rrmain_company      = COMPANY_NAME
    character(*),  public, parameter :: rrmain_company_url  = COMPANY_URL
    character(*),  public, parameter :: rrmain_program      = PRODUCT_NAME
    character(*),  public, parameter :: rrmain_programname  = PRODUCT_NAME  ! use in about box and window title

    character(*),  public, parameter :: rrmain_version      = rrmain_major//'.'//rrmain_minor//'.'//rrmain_revision//'.'//rrmain_build_number//' '//rrmain_architecture
    character(*),  public, parameter :: rrmain_version_full = 'Deltares, '//rrmain_program//' Version '//rrmain_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: rrmain_version_id   = '@(#)'//rrmain_version_full

contains

    subroutine getfullversionstring_rrmain(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(rrmain_version_full),len(stringout))
        stringout = rrmain_version_id(5:5+length-1)
    end subroutine getfullversionstring_rrmain

    subroutine getprogramnamestring_rrmain(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rrmain_programname)
    end subroutine getprogramnamestring_rrmain

    subroutine getshortprogramnamestring_rrmain(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rrmain_program)
    end subroutine getshortprogramnamestring_rrmain

    subroutine getfeaturenumberstring_rrmain(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rrmain_version)
    end subroutine getfeaturenumberstring_rrmain

    subroutine getversionnumberstring_rrmain(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rrmain_version)
    end subroutine getversionnumberstring_rrmain

    subroutine getcompanystring_rrmain(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rrmain_company)
    end subroutine getcompanystring_rrmain

    subroutine getsvnrevisionstring_rrmain(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rrmain_build_number)
    end subroutine getsvnrevisionstring_rrmain

    subroutine getarchitecturestring_rrmain(stringout)
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

    end subroutine getarchitecturestring_rrmain

subroutine get_rrmain_sbkrel(sbkrel)

   integer, intent(out)      :: sbkrel(3)

   !
   ! body

   sbkrel(1) = 3
   sbkrel(2) = 216
   sbkrel(3) = 65

end subroutine get_rrmain_sbkrel

end module rrmain_version_module
