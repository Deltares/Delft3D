!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
!
!
module m_string_utils

    use string_module

    implicit none

    private
    public :: join_strings, contains_any, contains_only_valid_chars, starts_with_valid_char
    public :: starts_with, index_in_array, remove_duplicates, string_equals

    contains

    function join_strings(strings, separator) result(concatenated_string)
        !<  Returns a single string by joining all the strings in the array <strings>, divided by the given separator.
        character(*), intent(in)  :: strings(:)           !< Array containing strings to be joined
        character(*), intent(in)  :: separator            !< Separator that will be placed among the strings of the array to join.
        character(:), allocatable :: concatenated_string  !< Result of joining all string in the array.
        integer :: i

        ! Allocate memory for the concatenated string
        allocate(character(len=0) :: concatenated_string)

        ! Concatenate the strings with the separator
        do i = 1, size(strings)
            if (i > 1) then
                concatenated_string = trim(adjustl(concatenated_string)) // separator
            end if
            concatenated_string = concatenated_string // trim(adjustl(strings(i)))
        end do
    end function join_strings

    logical function contains_any(whole_string, substring_array)
        !< Returns true if any of the substrings in <substring_array> is contained in <whole_string>.
        character(*), dimension(:), intent(in) :: substring_array !< Array containing multiple (sub)strings.
        character(*), intent(in)               :: whole_string    !< String to check if any of the substrings in contained inside.

        integer :: i
        do i = 1, size(substring_array)
            if (index(trim(whole_string), trim(substring_array(i))) > 0) then
                contains_any = .true.
                return
            end if
        end do

        contains_any = .false.

    end function contains_any

    logical function contains_only_valid_chars(names_array, valid_characters, logging_unit)
        !< Returns .true. if all characters in each string name of <names_array> is contained in the string <valid_characters>.
        !< Otherwise, it returns .false.
        character(*), dimension(:), intent(in) :: names_array      !< Array with all names to validate
        character(*), intent(in)               :: valid_characters !< Characters permitted in names
        integer, intent(in)                    :: logging_unit !< Number of the logging unit to which messages are sent.

        integer                            :: i, j
        character(len=len(names_array(1))) :: arrows_invalid_chars
        logical                            :: current_name_is_valid

        contains_only_valid_chars = .true.
        do i = 1, size(names_array)
            arrows_invalid_chars = repeat(' ', len(names_array(1)))
            current_name_is_valid = .true.
            do j=1, len_trim(names_array(i))
                if (verify(names_array(i)(j:j), valid_characters)/=0) then
                    arrows_invalid_chars(j:j) = '^'
                    current_name_is_valid = .false.
                end if
            end do
            if (.not.current_name_is_valid) then
                contains_only_valid_chars = .false.
                write(logging_unit,*) "Error: invalid characters found in the name:"
                write(logging_unit,*) names_array(i)
                write(logging_unit,*) arrows_invalid_chars
            end if
        end do
    end function contains_only_valid_chars

    logical function starts_with_valid_char(names_array, valid_start_characters, logging_unit)
        !< Returns .true. if the first character of each string name of <names_array> is contained in the string <valid_characters>.
        !< Otherwise, it returns .false.
        character(*), dimension(:), intent(in) :: names_array            !< Array with all names to validate
        character(*), intent(in)               :: valid_start_characters !< Characters permitted as start of names
        integer, intent(in)                    :: logging_unit !< Number of the logging unit to which messages are sent.

        integer                            :: i

        starts_with_valid_char = .true.
        do i = 1, size(names_array)
            if (verify(names_array(i)(1:1), valid_start_characters)/=0) then
                starts_with_valid_char = .false.
                write(logging_unit,*) "Error: invalid character found at the start of name:"
                write(logging_unit,*) names_array(i)
                write(logging_unit,*) '^'
            end if
        end do
    end function starts_with_valid_char

    function index_in_array(string_to_find, array_of_strings, exact_match, case_sensitive) result(location)
        !< Gives the index of the string_to_find in the array_of_strings (returns -1 if no match can be found)
        character(len=*), intent(in)               :: string_to_find   !< string to find in the array
        character(len=*), dimension(:), intent(in) :: array_of_strings !< array of strings to check

        logical, intent(in), optional :: exact_match    !< needs to be an exact match (not starts with) (default is false)
        logical, intent(in), optional :: case_sensitive !< check case sensitive (default is false)

        integer :: i, location
        logical :: found

        location = -1

        if (len(string_to_find) == 0) then
            return
        end if

        do i = 1, size(array_of_strings)
            if (string_equals(string_to_find,array_of_strings(i), exact_match, case_sensitive)) then
                location = i
                return
            end if
        end do
    end function index_in_array

    recursive function remove_duplicates( array ) result(unique_array)
    !< Takes an array of strings which may contain duplicated strings and returns an array in which all duplicates have been removed.
        character(*), dimension(:)         :: array            !< input array containing (possibly) duplicate strings.
        character(len(array)), allocatable :: unique_array(:)  !< output array containing only unique elements.

        if (size(array) > 0) then
            unique_array = [array(1), remove_duplicates(pack(array(2:), array(2:) /= array(1)))]
        else
            allocate(unique_array(0))
        endif
    end function remove_duplicates

    logical function string_equals(source_string, target_string, exact_match, case_sensitive) result(found)
        !< Checks two strings to see if they are equal with the given conditions.
        character(len=*), intent(in) :: source_string !< string to compare
        character(len=*), intent(in) :: target_string !< string to compare with

        logical, intent(in), optional :: exact_match     !< needs to be an exact match (not starts with) (default is false)
        logical, intent(in), optional :: case_sensitive  !< check case sensitive (default is false)

        logical :: exact_match_
        logical :: case_sensitive_

        exact_match_ = .false.
        if (present(exact_match)) exact_match_ = exact_match

        case_sensitive_ = .false.
        if (present(case_sensitive)) case_sensitive_ = case_sensitive

        if (exact_match_) then
            if (case_sensitive_) then
                found = source_string == target_string
            else
                found = str_tolower(source_string) == str_tolower(target_string)
            end if
        else
            found = starts_with(target_string, source_string, case_sensitive_)
        end if

    end function

    logical function starts_with(string_to_check, string_to_search, case_sensitive)
        !< Checks if the provided string_to_check starts with the string_to_search
        !< Optionally the case_sensitive can be used (default = false)

        character(len=*), intent(in)  :: string_to_check  !< string to check
        character(len=*), intent(in)  :: string_to_search !< string to search for

        logical, intent(in), optional :: case_sensitive   !< check case sensitive

        ! local variables
        character(len=len(string_to_search)) :: string_to_compare
        logical :: exact_match_
        logical :: case_sensitive_

        case_sensitive_ = .false.
        if (present(case_sensitive)) case_sensitive_ = case_sensitive

        if (len(string_to_check)<len(string_to_search)) then
            starts_with = .false.
            return
        end if

        string_to_compare = string_to_check(1:len(string_to_search))

        if (case_sensitive_) then
            starts_with = string_to_search == string_to_compare
            return
        end if

        starts_with = check_case_insensitive(string_to_search, string_to_compare)
    end function

    logical function check_case_insensitive(string_to_check, string_to_compare)
        !< Compares two strings (case insensative)
        character(len=*), intent(in)  :: string_to_check   !< string to check
        character(len=*), intent(in)  :: string_to_compare !< string to search for

        integer :: i, i1, i2

        do i = 1, len(string_to_check)
            i1 = ICHAR(string_to_check(i:i))
            i2 = ICHAR(string_to_compare(i:i))

            if (i1 == i2) then
                cycle
            end if

            if (i1 >= 97 .and. i1 <= 122) then
                ! upper case letter
                i1 = i1 - 32
                if (i1 == i2) then
                    cycle
                end if
            end if

            if (i2 >= 97 .and. i2 <= 122) then
                ! upper case letter
                i2 = i2 - 32
            end if

            if (i1 == i2) then
                cycle
            else
                ! not equal
                check_case_insensitive = .false.
                return
            end if
        end do

        check_case_insensitive = .true.
    end function check_case_insensitive

end module m_string_utils
