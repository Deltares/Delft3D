module datsel_version_module
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

    character(*),  public, parameter :: datsel_major        = '2'
    character(*),  public, parameter :: datsel_minor        = '12'
    character(*),  public, parameter :: datsel_revision     = '00'
    character(*),  public, parameter :: datsel_build_number = BUILD_NR

    character(*),  public, parameter :: datsel_company      = COMPANY_NAME
    character(*),  public, parameter :: datsel_company_url  = COMPANY_URL
    character(*),  public, parameter :: datsel_program      = 'datsel'

    character(*),  public, parameter :: datsel_version      = datsel_major//'.'//datsel_minor//'.'//datsel_revision//'.'//datsel_build_number
    character(*),  public, parameter :: datsel_version_full = 'Deltares, '//datsel_program//' Version '//datsel_version//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: datsel_version_id   = '@(#)'//datsel_version_full
    character(*),  public, parameter :: datsel_source_code  = '@(#)HeadURL '//BRANCH

contains

    subroutine getfullversionstring_datsel(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(datsel_version_full),len(stringout))
        stringout = datsel_version_id(5:5+length-1)
    end subroutine getfullversionstring_datsel

end module datsel_version_module
