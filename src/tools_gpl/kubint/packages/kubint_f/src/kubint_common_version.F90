module kubint_version_module
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
#include "version_definition.h"


implicit none

    character(*),  public, parameter :: kubint_major        = '2'
    character(*),  public, parameter :: kubint_minor        = '02'
    character(*),  public, parameter :: kubint_revision     = '01'
    character(*),  public, parameter :: kubint_build_number = BUILD_NR

    character(*),  public, parameter :: kubint_company      = COMPANY_NAME
    character(*),  public, parameter :: kubint_company_url  = COMPANY_URL
    character(*),  public, parameter :: kubint_program      = 'kubint'

    character(*),  public, parameter :: kubint_version      = kubint_major//'.'//kubint_minor//'.'//kubint_revision//'.'//kubint_build_number
    character(*),  public, parameter :: kubint_version_full = 'Deltares, '//kubint_program//' Version '//kubint_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: kubint_version_id   = '@(#)'//kubint_version_full
    character(*),  public, parameter :: kubint_source_code  = '@(#)HeadURL '//BRANCH

contains

    subroutine getfullversionstring_kubint(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(kubint_version_full),len(stringout))
        stringout = kubint_version_id(5:5+length-1)
    end subroutine getfullversionstring_kubint

end module kubint_version_module
