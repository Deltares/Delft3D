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

module dfm_volume_tool_version_module
    use dfm_volume_tool_static_version_info

    implicit none

    character(*),  public, parameter :: dfm_volume_tool_version_full = version_prefix // ', ' // product_name // version_suffix
    character(*),  public, parameter :: dfm_volume_tool_version_id   = version_prefix_id // ', ' // product_name // ' ' // version_suffix_full

contains

    subroutine getfullversionstring_dfm_volume_tool(stringout)
        character(*), intent(out) :: stringout

        stringout = dfm_volume_tool_version_id(offset:)
    end subroutine getfullversionstring_dfm_volume_tool

    subroutine getbranch_dfm_volume_tool(stringout)
        character(*), intent(out) :: stringout

        stringout = branch
    end subroutine getbranch_dfm_volume_tool

end module dfm_volume_tool_version_module
