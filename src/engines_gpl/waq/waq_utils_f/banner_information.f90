!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

!> Module containing the definition of the banner_information class
module m_banner_information
    use m_waq_precision

    implicit none

    private

    !> Class for holding information related to the banner
    type, public :: banner_information
        character(:), allocatable :: name            !< Name of the program
        character(:), allocatable :: description     !< Description of the program
        character(:), allocatable :: suite_name      !< Name of the suite that the program belongs to
        character(:), allocatable :: version_string  !< String containing the version information
        character(:), allocatable :: built_on        !< String containing the datetime the program was built
        character(:), allocatable :: copyright       !< String containing the copyright information
    contains
        procedure get_max_width
    end type banner_information

contains

    !> Gets the maximum width of the text properties
    function get_max_width(this) result(max_width)
        class(banner_information), intent(in) :: this    !< Instance of this banner_information
        integer(kind=int_wp) :: max_width   !< Max width of all string properties

        max_width = max(len(this%name), &
                        len(this%description), &
                        len(this%suite_name), &
                        len(this%version_string), &
                        len(this%built_on), &
                        len(this%copyright))
    end function get_max_width

end module m_banner_information
