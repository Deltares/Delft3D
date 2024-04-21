module nesthd1_version_module
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

    character(*),  public, parameter :: nesthd1_major        = '1'
    character(*),  public, parameter :: nesthd1_minor        = '60'
    character(*),  public, parameter :: nesthd1_revision     = '02'
    character(*),  public, parameter :: nesthd1_build_number = BUILD_NR

    character(*),  public, parameter :: nesthd1_company      = COMPANY_NAME
    character(*),  public, parameter :: nesthd1_company_url  = COMPANY_URL
    character(*),  public, parameter :: nesthd1_program      = 'NESTHD1'

    character(*),  public, parameter :: nesthd1_version      = nesthd1_major//'.'//nesthd1_minor//'.'//nesthd1_revision//'.'//nesthd1_build_number
    character(*),  public, parameter :: nesthd1_version_full = 'Deltares, '//nesthd1_program//' Version '//nesthd1_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: nesthd1_version_id   = '@(#)'//nesthd1_version_full
    character(*),  public, parameter :: nesthd1_source_code  = '@(#) '//char(0)

contains

    subroutine getfullversionstring_nesthd1(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(nesthd1_version_full),len(stringout))
        stringout = nesthd1_version_id(5:5+length-1)
    end subroutine getfullversionstring_nesthd1

end module nesthd1_version_module
