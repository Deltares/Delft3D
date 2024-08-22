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

#include "rtc_version.h"
module rtc_version_module
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
!  $Id: rtc_version.F90.svn 54644 2018-02-23 14:50:13Z zeekant $
!  $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/engines/rtc/packages/rtc/src/rtc_version.F90.svn $
    use iso_c_binding

    implicit none

    character(*),  public, parameter :: rtc_major        = MAJOR_STR
    character(*),  public, parameter :: rtc_minor        = MINOR_STR
    character(*),  public, parameter :: rtc_revision     = REVISION_STR
    character(*),  public, parameter :: rtc_build_number = BUILD_NR

#if defined(W32)
    character(*),  public, parameter :: rtc_architecture = '(Win32)'
#elif defined(X64)
    character(*),  public, parameter :: rtc_architecture = '(Win64)'
#elif defined(LINUX32)
    character(*),  public, parameter :: rtc_architecture = '(Linux32)'
#elif defined(LINUX64)
    character(*),  public, parameter :: rtc_architecture = '(Linux64)'
#else
    character(*),  public, parameter :: rtc_architecture = '(Unknown)'
#endif

    character(*),  public, parameter :: rtc_company      = COMPANY_NAME
    character(*),  public, parameter :: rtc_company_url  = COMPANY_URL
    character(*),  public, parameter :: rtc_program      = PRODUCT_NAME
    character(*),  public, parameter :: rtc_programname  = PRODUCT_NAME  ! use in about box and window title

    character(*),  public, parameter :: rtc_version      = rtc_major//'.'//rtc_minor//'.'//rtc_revision//'.'//rtc_build_number//' '//rtc_architecture
    character(*),  public, parameter :: rtc_version_full = 'Deltares, '//rtc_program//' Version '//rtc_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: rtc_version_id   = '@(#)'//rtc_version_full
    
contains

    subroutine getfullversionstring_rtc(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(rtc_version_full),len(stringout))
        stringout = rtc_version_id(5:5+length-1)
    end subroutine getfullversionstring_rtc

    subroutine getprogramnamestring_rtc(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rtc_programname)
    end subroutine getprogramnamestring_rtc

    subroutine getshortprogramnamestring_rtc(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rtc_program)
    end subroutine getshortprogramnamestring_rtc

    subroutine getfeaturenumberstring_rtc(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rtc_version)
    end subroutine getfeaturenumberstring_rtc

    subroutine getversionnumberstring_rtc(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rtc_version)
    end subroutine getversionnumberstring_rtc

    subroutine getcompanystring_rtc(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rtc_company)
    end subroutine getcompanystring_rtc

    subroutine getsvnrevisionstring_rtc(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(rtc_build_number)
    end subroutine getsvnrevisionstring_rtc

    subroutine getarchitecturestring_rtc(stringout)
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

    end subroutine getarchitecturestring_rtc

subroutine get_rtc_sbkrel(sbkrel)

   integer, intent(out)      :: sbkrel(3)

   !
   ! body

   sbkrel(1) = 3
   sbkrel(2) = 216
   sbkrel(3) = 005

end subroutine get_rtc_sbkrel

end module rtc_version_module
