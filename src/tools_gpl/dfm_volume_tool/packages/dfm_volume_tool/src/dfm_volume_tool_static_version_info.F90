!!  Copyright (C)  Stichting Deltares, 2012-2026.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

#include "dfm_volume_tool_version.h"

module dfm_volume_tool_static_version_info
    implicit none

    character(*),  public, parameter :: TOOL_COMPANY      = COMPANY_NAME
    character(*),  public, parameter :: TOOL_COMPANY_URL  = COMPANY_URL

    character(*),  public, parameter :: TOOL_PRODUCT_NAME = PRODUCT_NAME
    character(*),  public, parameter :: TOOL_BASE_NAME    = BASE_NAME

    character(*),  public, parameter :: TOOL_BUILD_NR     = BUILD_NR
    character(*),  public, parameter :: TOOL_MAJOR        = MAJOR_STR
    character(*),  public, parameter :: TOOL_MINOR        = MINOR_STR
    character(*),  public, parameter :: TOOL_REVISION     = REVISION_STR
    character(*),  public, parameter :: TOOL_BRANCH       = BRANCH

    character(*), public, parameter :: TOOL_VERSION = &
        trim(TOOL_MAJOR)//'.'//trim(TOOL_MINOR)//'.'//trim(TOOL_REVISION)//'.'//trim(TOOL_BUILD_NR)
    character(*), public, parameter :: TOOL_VERSION_FULL = &
        trim(TOOL_COMPANY)//', '//trim(TOOL_PRODUCT_NAME)//' Version '//TOOL_VERSION//', '//__DATE__//', '//__TIME__
    character(*), public, parameter :: TOOL_VERSION_ID = '@(#)'//TOOL_VERSION_FULL

    character(*), public, parameter :: TOOL_VERSION_PREFIX = TOOL_COMPANY
    character(*), public, parameter :: TOOL_VERSION_PREFIX_ID = '@(#)'//TOOL_COMPANY
    character(*), public, parameter :: TOOL_VERSION_SUFFIX = TOOL_VERSION_FULL
    character(*), public, parameter :: TOOL_VERSION_SUFFIX_FULL = &
        'Version '//TOOL_VERSION_SUFFIX//', '//__DATE__//', '//__TIME__

    ! While in principle we can use a literal constant, this allows the complier/linker to get rid of the
    ! literal string. Since we want that string to be present in the executable or DLL, we need to use
    ! this trick instead. It fools the compiler into retaining the full string, so that a tool like
    ! "strings" on Linux can extract it.
    integer, protected, save :: offset = 5

end module dfm_volume_tool_static_version_info
