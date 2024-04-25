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

module flow2d3d_version_module
    use flow2d3d_static_version_info

    implicit none

    character(*),  public, parameter :: flow2d3d_version_full = version_prefix // ', ' // product_name // version_suffix
    character(*),  public, parameter :: flow2d3d_version_id   = version_prefix_id // ', ' // product_name // ' ' // version_suffix_full
    character(*),  public, parameter :: flow2d3d_branch       = branch
    character(*),  public, parameter :: com_file_version      = "3.55.00"
    character(*),  public, parameter :: dro_file_version      = "3.20.01"
    character(*),  public, parameter :: his_file_version      = "3.52.11"
    character(*),  public, parameter :: map_file_version      = "3.54.30"

contains

subroutine getfullversionstring_flow2d3d(stringout)
    character(*), intent(out) :: stringout
    stringout = flow2d3d_version_id(offset:)
end subroutine getfullversionstring_flow2d3d

subroutine getbranch_flow2d3d(stringout)
    character(*), intent(out) :: stringout
    stringout = flow2d3d_branch(offset:)
end subroutine getbranch_flow2d3d

subroutine getdrofileversionstring_flow2d3d(stringout)
    character(*), intent(out) :: stringout
    stringout = dro_file_version
end subroutine getdrofileversionstring_flow2d3d

subroutine gethisfileversionstring_flow2d3d(stringout)
    character(*), intent(out) :: stringout
    stringout = his_file_version
end subroutine gethisfileversionstring_flow2d3d

subroutine getmapfileversionstring_flow2d3d(stringout)
    character(*), intent(out) :: stringout
    stringout = map_file_version
end subroutine getmapfileversionstring_flow2d3d

subroutine getcomfileversionstring_flow2d3d(stringout)
    character(*), intent(out) :: stringout
    stringout = com_file_version
end subroutine getcomfileversionstring_flow2d3d

subroutine getshortversionstring_flow2d3d(stringout)
    character(*), intent(out) :: stringout
    stringout = version_shrt
end subroutine getshortversionstring_flow2d3d

end module flow2d3d_version_module
