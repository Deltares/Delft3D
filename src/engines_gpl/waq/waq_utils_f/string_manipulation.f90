!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
module m_string_manipulation
    use m_waq_precision
    implicit none
    private
    public :: upper_case, shift_char_subarray, get_trimmed_length
contains

    subroutine upper_case(input_string, output_string, string_length)

        integer(kind = int_wp), intent(in) :: string_length
        character(len = *), intent(in) :: input_string
        character(len = *), intent(out) :: output_string

        integer :: ic, ascii_code
        logical :: is_lower_case
        character(len = 1) :: current_char

        ! copy the input string
        output_string = input_string

        do ic = 1, string_length
            current_char = output_string(ic:ic)          ! Extract the current character
            ascii_code = iachar(current_char)            ! Convert character to its ASCII code

            ! Determine if the character is lowercase (ASCII code between 'a' and 'z')
            is_lower_case = ascii_code >= iachar('a') .and. ascii_code <= iachar('z')

            ! If the character is lowercase, convert to uppercase by subtracting 32 from ASCII code
            if (is_lower_case) then
                ascii_code = ascii_code - 32             ! Convert ASCII code to uppercase
                output_string(ic:ic) = achar(ascii_code)         ! Convert ASCII code back to character
            end if
        end do
    end subroutine upper_case

    subroutine shift_char_subarray(total_array, start_shift, end_shift)
        character(len = *), dimension(:), intent(inout) :: total_array
        integer(kind = int_wp), intent(in) :: start_shift, end_shift
        integer i

        do i = end_shift, start_shift, -1
            total_array(i + 1) = total_array(i)
        end do
    end subroutine shift_char_subarray

    subroutine get_trimmed_length(input_string, trimmed_length)
        !! Returns the length of the input string after trimming trailing spaces.
        !! The length is always at least 1.
        character(len = *), intent(in) :: input_string         !! Input string to be trimmed
        integer(kind = int_wp), intent(out) :: trimmed_length  !! Length of the trimmed string

        integer(kind = int_wp) :: string_length, char_index

        string_length = LEN(input_string)  ! Total length of the string
        trimmed_length = 1                ! Initialize to minimum length

        ! Iterate through the string to find the length of the trimmed part
        do char_index = 1, string_length
            if (input_string(char_index:char_index) /= ' ') then
                trimmed_length = char_index  ! Update length to the last non-space character
            end if
        end do

    end subroutine get_trimmed_length

end module m_string_manipulation
