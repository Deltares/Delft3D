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

module wlopenmi_version_module
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
!  $Id: wlopenmi_version.F90.svn 54613 2018-02-22 13:50:28Z zeekant $
!  $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/utils/wl_openmi_support/packages/wl_openmi_support/src/wlopenmi_version.F90.svn $
    use iso_c_binding

    implicit none

    character(*),  public, parameter :: wlopenmi_major        = '1'
    character(*),  public, parameter :: wlopenmi_minor        = '01'
    character(*),  public, parameter :: wlopenmi_revision     = '00'
    character(*),  public, parameter :: wlopenmi_build_number = '79208M'

#if defined(W32)
    character(*),  public, parameter :: wlopenmi_architecture = '(Win32)'
#elif defined(X64)
    character(*),  public, parameter :: wlopenmi_architecture = '(Win64)'
#elif defined(LINUX32)
    character(*),  public, parameter :: wlopenmi_architecture = '(Linux32)'
#elif defined(LINUX64)
    character(*),  public, parameter :: wlopenmi_architecture = '(Linux64)'
#else
    character(*),  public, parameter :: wlopenmi_architecture = '(Unknown)'
#endif

    character(*),  public, parameter :: wlopenmi_company      = 'Deltares'
    character(*),  public, parameter :: wlopenmi_company_url  = 'http://www.deltares.nl'
    character(*),  public, parameter :: wlopenmi_program      = 'WLOpenMI'
    character(*),  public, parameter :: wlopenmi_programname  = 'WLOPENMI'  ! use in about box and window title

    character(*),  public, parameter :: wlopenmi_version      = wlopenmi_major//'.'//wlopenmi_minor//'.'//wlopenmi_revision//'.'//wlopenmi_build_number//' '//wlopenmi_architecture
    character(*),  public, parameter :: wlopenmi_version_full = 'Deltares, '//wlopenmi_program//' Version '//wlopenmi_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: wlopenmi_version_id   = '@(#)'//wlopenmi_version_full
    character(*),  public, parameter :: wlopenmi_checkout     = '$HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/utils/wl_openmi_support/packages/wl_openmi_support/src/wlopenmi_version.F90.svn $'

contains

    subroutine getfullversionstring_wlopenmi(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(wlopenmi_version_full),len(stringout))
        stringout = wlopenmi_version_id(5:5+length-1)
    end subroutine getfullversionstring_wlopenmi

    subroutine getprogramnamestring_wlopenmi(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(wlopenmi_programname)
    end subroutine getprogramnamestring_wlopenmi

    subroutine getshortprogramnamestring_wlopenmi(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(wlopenmi_program)
    end subroutine getshortprogramnamestring_wlopenmi

    subroutine getfeaturenumberstring_wlopenmi(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(wlopenmi_version)
    end subroutine getfeaturenumberstring_wlopenmi

    subroutine getversionnumberstring_wlopenmi(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(wlopenmi_version)
    end subroutine getversionnumberstring_wlopenmi

    subroutine getcompanystring_wlopenmi(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(wlopenmi_company)
    end subroutine getcompanystring_wlopenmi

    subroutine getsvnrevisionstring_wlopenmi(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(wlopenmi_build_number)
    end subroutine getsvnrevisionstring_wlopenmi

    subroutine getbuildlocation(stringout)
        character(*), intent(out) :: stringout

        stringout = trim(wlopenmi_checkout)
    end subroutine getbuildlocation

    subroutine getarchitecturestring_wlopenmi(stringout)
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

    end subroutine getarchitecturestring_wlopenmi

subroutine get_wlopenmi_sbkrel(sbkrel)

   integer, intent(out)      :: sbkrel(3)

   !
   ! body

   sbkrel(1) = 1
   sbkrel(2) = 01
   sbkrel(3) = 00

end subroutine get_wlopenmi_sbkrel

end module wlopenmi_version_module
