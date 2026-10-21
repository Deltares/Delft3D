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

!> Build-time product and version metadata for the volume tool.
module dfm_volume_tool_static_version_info
   implicit none(type, external)
   private

   public :: TOOL_BASE_NAME, TOOL_VERSION, TOOL_PRODUCT_NAME, TOOL_BRANCH
   public :: TOOL_VERSION_PREFIX, TOOL_VERSION_PREFIX_ID, TOOL_VERSION_SUFFIX, TOOL_VERSION_SUFFIX_FULL
   public :: offset

   character(*), parameter :: TOOL_COMPANY = COMPANY_NAME
   character(*), parameter :: TOOL_COMPANY_URL = COMPANY_URL

   character(*), parameter :: TOOL_PRODUCT_NAME = PRODUCT_NAME !< Product display name.
   character(*), parameter :: TOOL_BASE_NAME = BASE_NAME !< Command-line program name.

   character(*), parameter :: TOOL_BUILD_NR = BUILD_NR
   character(*), parameter :: TOOL_MAJOR = MAJOR_STR
   character(*), parameter :: TOOL_MINOR = MINOR_STR
   character(*), parameter :: TOOL_REVISION = REVISION_STR
   character(*), parameter :: TOOL_BRANCH = BRANCH !< Source branch recorded at build time.

   !> Numeric product version for the command-line interface.
   character(*), parameter :: TOOL_VERSION = &
                              trim(TOOL_MAJOR)//'.'//trim(TOOL_MINOR)//'.'//trim(TOOL_REVISION)//'.'//trim(TOOL_BUILD_NR)
   character(*), parameter :: TOOL_VERSION_FULL = &
                              trim(TOOL_COMPANY)//', '//trim(TOOL_PRODUCT_NAME)//' Version '//TOOL_VERSION//', '//__DATE__//', '//__TIME__
   character(*), parameter :: TOOL_VERSION_ID = '@(#)'//TOOL_VERSION_FULL

   character(*), parameter :: TOOL_VERSION_PREFIX = TOOL_COMPANY !< Company prefix for version strings.
   character(*), parameter :: TOOL_VERSION_PREFIX_ID = '@(#)'//TOOL_COMPANY !< Searchable company prefix.
   character(*), parameter :: TOOL_VERSION_SUFFIX = TOOL_VERSION_FULL !< Full build metadata.
   !> Version suffix including compilation date and time.
   character(*), parameter :: TOOL_VERSION_SUFFIX_FULL = &
                              'Version '//TOOL_VERSION_SUFFIX//', '//__DATE__//', '//__TIME__

   ! While in principle we can use a literal constant, this allows the complier/linker to get rid of the
   ! literal string. Since we want that string to be present in the executable or DLL, we need to use
   ! this trick instead. It fools the compiler into retaining the full string, so that a tool like
   ! "strings" on Linux can extract it.
   integer, protected, save :: offset = 5 !< Start of version text after the searchable marker.

end module dfm_volume_tool_static_version_info
