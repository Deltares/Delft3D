!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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

!> Version strings and accessors for the volume tool.
module dfm_volume_tool_version_module
   use dfm_volume_tool_static_version_info, only: TOOL_VERSION_PREFIX, TOOL_VERSION_PREFIX_ID, &
                                                  TOOL_PRODUCT_NAME, TOOL_VERSION_SUFFIX, TOOL_VERSION_SUFFIX_FULL, TOOL_BRANCH, offset

   implicit none(type, external)
   private

   public :: DFM_VOLUME_TOOL_VERSION_FULL, DFM_VOLUME_TOOL_VERSION_ID
   public :: getfullversionstring_dfm_volume_tool, getbranch_dfm_volume_tool

   !> Product and build metadata written to NetCDF output.
   character(*), parameter :: DFM_VOLUME_TOOL_VERSION_FULL = &
                              TOOL_VERSION_PREFIX//', '//TOOL_PRODUCT_NAME//TOOL_VERSION_SUFFIX
   !> Searchable product and build identification string.
   character(*), parameter :: DFM_VOLUME_TOOL_VERSION_ID = &
                              TOOL_VERSION_PREFIX_ID//', '//TOOL_PRODUCT_NAME//' '//TOOL_VERSION_SUFFIX_FULL

contains

   !> Return the full version string without its searchable marker.
   subroutine getfullversionstring_dfm_volume_tool(stringout)
      character(*), intent(out) :: stringout !< Product and build identification.

      stringout = DFM_VOLUME_TOOL_VERSION_ID(offset:)
   end subroutine getfullversionstring_dfm_volume_tool

   !> Return the source branch recorded at build time.
   subroutine getbranch_dfm_volume_tool(stringout)
      character(*), intent(out) :: stringout !< Source branch name.

      stringout = TOOL_BRANCH
   end subroutine getbranch_dfm_volume_tool

end module dfm_volume_tool_version_module
