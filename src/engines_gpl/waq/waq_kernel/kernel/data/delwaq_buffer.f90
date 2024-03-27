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

module m_waq_data_buffer
    use m_waq_precision

    implicit none

    type, public :: waq_data_buffer
        integer(kind = int_wp), dimension(:), allocatable :: ibuf
        real(kind = real_wp), dimension(:), allocatable :: rbuf
        character, dimension(:), allocatable :: chbuf

    contains
        procedure :: intialize => intialize_buffer
        procedure :: create_strings_20_array
        final :: destruct_buffer

    end type waq_data_buffer

contains
    subroutine intialize_buffer(this)
        !< Initialization of buffer
        class(waq_data_buffer), intent(out) :: this

        allocate (this%rbuf(0))
        allocate (this%ibuf(0))
        allocate (this%chbuf(0))

    end subroutine intialize_buffer

    subroutine destruct_buffer(this)
        !< destructor of buffer, free allocated memory
        type(waq_data_buffer) :: this

        if (allocated(this%ibuf)) deallocate (this%ibuf)
        if (allocated(this%rbuf)) deallocate (this%rbuf)
        if (allocated(this%chbuf)) deallocate (this%chbuf)

    end subroutine destruct_buffer

    function create_strings_20_array(this, start_index, number_of_strings) result(result_val)
        !< convert the chbuf (character array) property to a new string(len=20) array
        class(waq_data_buffer), intent(in) :: this
        integer(kind = int_wp), intent(in) :: start_index       !< start index to start creating strings
        integer(kind = int_wp), intent(in) :: number_of_strings !< number of strings to create
        character(len = 20) :: result_val(number_of_strings)       !< created string array

        result_val = convert(this%chbuf(start_index), number_of_strings)
    end function create_strings_20_array

    function convert(item_to_convert, number_of_strings) result(result_val)
        !< implicitly converts provided item_to_convert to string array
        integer(kind = int_wp), intent(in) :: number_of_strings !< number of strings to create
        character(len = 20) :: item_to_convert(number_of_strings)  !< character array to convert
        character(len = 20) :: result_val(number_of_strings)       !< created string array

        result_val = item_to_convert

    end function convert
end module m_waq_data_buffer
