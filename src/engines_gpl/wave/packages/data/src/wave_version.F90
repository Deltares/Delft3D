!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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

module dwaves_version_module
    use dwaves_static_version_info

    implicit none

    character(*),  public, parameter :: dwaves_version_full = version_prefix // ', ' // product_name // version_suffix
    character(*),  public, parameter :: dwaves_version_id   = version_prefix_id // ', ' // product_name // ' ' // version_suffix_full
    character(*),  public, parameter :: dwaves_branch       = branch

contains

    subroutine getfullversionstring_dwaves(stringout)
        character(*), intent(out) :: stringout

        stringout = dwaves_version_id(offset:)
    end subroutine getfullversionstring_dwaves

    subroutine getbranch_dwaves(stringout)
        character(*), intent(out) :: stringout

        stringout = dwaves_branch(offset:)
    end subroutine getbranch_dwaves

end module dwaves_version_module
