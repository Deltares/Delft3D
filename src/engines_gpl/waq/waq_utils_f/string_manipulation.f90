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
    use timers
    implicit none

    private
    public :: upper_case, shift_char_subarray, get_trimmed_length, replace_space_by_underscore, is_same_letter

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

    ! Replace embedded spaces by underscores (and remove leading spaces)
    function replace_space_by_underscore(input_string) result(ouput_string)

        character(len = *), intent(in) :: input_string
        character(len = len(input_string)) :: ouput_string

        integer(kind = int_wp) :: i

        ouput_string = adjustl(input_string)
        do i = 1, len_trim(ouput_string)
            if (ouput_string(i:i) == ' ') then
                ouput_string(i:i) = '_'
            endif
        enddo

    end function replace_space_by_underscore

    !! is_same_letter returns .TRUE. if CA is the same letter as CB regardless of case.
    logical function is_same_letter(ca, cb)

        character ca, cb ! CA& CB (input) character(len=1)
        intrinsic ichar
        integer            inta, intb, zcode
        integer(4) :: ithandl = 0
        if (timon) call timstrt ("is_same_letter", ithandl)
        ! Test if the characters are equal
        is_same_letter = ca==cb
        if(is_same_letter) &
                goto 9999  !   RETURN
        !
        !          Now test for equivalence if both characters are alphabetic.
        !
        ZCODE = ICHAR('Z')
        !
        !          Use 'Z' rather than 'A' so that ASCII can be detected on Prime
        !          machines, on which ICHAR returns a value with bit 8 set.
        !          ICHAR('A') on Prime machines returns 193 which is the same as
        !          ICHAR('A') on an EBCDIC machine.
        !
        INTA = ICHAR(CA)
        INTB = ICHAR(CB)
        !
        IF(ZCODE==90 .OR. ZCODE==122) THEN
            !
            !             ASCII is assumed - ZCODE is the ASCII code of either lower or
            !             upper case 'Z'.
            !
            IF(INTA>=97 .AND. INTA<=122) INTA = INTA - 32
            IF(INTB>=97 .AND. INTB<=122) INTB = INTB - 32

        ELSE IF(ZCODE==233 .OR. ZCODE==169) THEN
            ! EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or upper case 'Z'.
            if(inta>=129 .and. inta<=137 .or. &
                    inta>=145 .and. inta<=153 .or. &
                    inta>=162 .and. inta<=169) inta = inta + 64
            if(intb>=129 .and. intb<=137 .or. &
                    intb>=145 .and. intb<=153 .or. &
                    intb>=162 .and. intb<=169) intb = intb + 64

        else if(zcode==218 .or. zcode==250) then

            ! ASCII is assumed, on Prime machines - ZCODE is the ASCII code
            ! plus 128 of either lower or upper case 'Z'.
            if(inta>=225 .and. inta<=250) inta = inta - 32
            if(intb>=225 .and. intb<=250) intb = intb - 32
        end if
        is_same_letter = inta==intb

        9999 if (timon) call timstop (ithandl)
    end function

end module m_string_manipulation
