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

program tests_string_utils
    !! tests_dlwq13.f90 --
    !!     Run unit tests for routines in Delft3D-WAQ

    use ftnunit
    use m_string_utils

    implicit none
    character(len=200) :: cmd_arg
    integer :: iargc, getarg

    ! Determine the number of command line arguments
    iargc = command_argument_count()

    call prepare_tests()
    call runtests_init()

    ! Run a specific test based on the first argument
    if (iargc > 0) then
        call get_command_argument(1, cmd_arg)

        select case (trim(cmd_arg))
        case ('test_string_equals_default_settings')
            write (*, *) "Running test_string_equals_default_settings"
            call runtests(call_test_string_equals_default_settings)
        case ('test_string_equals_with_custom_settings')
            write (*, *) "Running test_string_equals_with_custom_settings"
            call runtests(call_test_string_equals_with_custom_settings)
        case ('test_index_in_array_default_settings')
            write (*, *) "Running test_index_in_array_default_settings"
            call runtests(call_test_index_in_array_default_settings)
        case ('test_index_in_array_custom_settings')
            write (*, *) "Running test_index_in_array_custom_settings"
            call runtests(call_test_index_in_array_custom_settings)
        case ('test_remove_duplicates')
            write (*, *) "Running test_remove_duplicates"
            call runtests(call_test_remove_duplicates)
        case ('test_centre_text')
            write (*, *) "Running test_centre_text"
            call runtests(call_test_centre_text)
        end select
    else
        write (*, *) "No test specified, running all tests"
        call runtests(call_test_string_equals_default_settings)
        call runtests(call_test_string_equals_with_custom_settings)
        call runtests(call_test_index_in_array_default_settings)
        call runtests(call_test_index_in_array_custom_settings)
        call runtests(call_test_remove_duplicates)
        call runtests(call_test_centre_text)
    end if

    call runtests_final()

contains

    subroutine call_test_string_equals_default_settings
        call test(test_string_equals_default_settings, 'String equals: is case insensitive and uses starts with as default')
    end subroutine call_test_string_equals_default_settings

    subroutine call_test_string_equals_with_custom_settings
        call test(test_string_equals_with_custom_settings, 'String equals: should be able to be configured with custom settings')
    end subroutine call_test_string_equals_with_custom_settings

    subroutine call_test_index_in_array_default_settings
        call test(test_index_in_array_default_settings, 'index_in_array: with default settings')
    end subroutine call_test_index_in_array_default_settings

    subroutine call_test_index_in_array_custom_settings
        call test(test_index_in_array_custom_settings, 'index_in_array: with custom settings')
    end subroutine call_test_index_in_array_custom_settings

    subroutine call_test_remove_duplicates
        call test(test_remove_duplicates, 'remove_duplicates in array')
    end subroutine call_test_remove_duplicates

    subroutine call_test_centre_text
        call test(test_centre_text_odd_length, 'check if centre_text works correctl for odd length strings')
        call test(test_centre_text_even_length, 'check if centre_text works correctly for even length strings')
    end subroutine call_test_centre_text

    subroutine test_string_equals_default_settings
        ! test default settings for string_equals

        call assert_true(string_equals("abc", "abc"), "string_equals should match case insensitive by default")
        call assert_true(string_equals("abc", "ABC"), "string_equals should match case insensitive by default")
        call assert_true(string_equals("abc", "aBc"), "string_equals should match case insensitive by default")

        call assert_true(string_equals("ab", "abc"), "string_equals should match using starts with by default")
        call assert_false(string_equals("bc", "abc"), "string_equals should match using starts with by default")

        call assert_false(string_equals("short", "looooong"), "string_equals should be able to handle string_to_check and string_to_search of different length")

    end subroutine test_string_equals_default_settings

    subroutine test_string_equals_with_custom_settings
        ! test custom settings for string_equals
        call assert_true(string_equals("abc", "abc", case_sensitive=.true.), "case_sensitive check should work")
        call assert_true(string_equals("ab", "abc", case_sensitive=.true.), "case_sensitive check with short string should work")
        call assert_false(string_equals("abc", "ABC", case_sensitive=.true.), "case_sensitive check with differnt case should fail")

        call assert_true(string_equals("abc", "ABC", exact_match=.true.), "exact_match with different case should match")
        call assert_true(string_equals("abc", "abc", exact_match=.true.), "exact_match with same case should match")
        call assert_false(string_equals("ab", "ABC", exact_match=.true.), "exact_match with short string should not match")

        call assert_true(string_equals("abc", "abc", exact_match=.true., case_sensitive=.true.), "exact_match and case_sensitive with same string should match")
        call assert_false(string_equals("abc", "ABC", exact_match=.true., case_sensitive=.true.), "exact_match and case_sensitive with different case should not match")
        call assert_false(string_equals("ab", "abc", exact_match=.true., case_sensitive=.true.), "exact_match and case_sensitive with short string should not match")
        call assert_false(string_equals("ab", "ABC", exact_match=.true., case_sensitive=.true.), "exact_match and case_sensitive with short and different case string should not match")

    end subroutine test_string_equals_with_custom_settings

    subroutine test_index_in_array_default_settings
        ! test default settings for index_in_array
        character(len=3), allocatable :: strings_to_search(:)

        strings_to_search = (/'abc', 'def', 'ghi', 'jkl', 'abc'/)

        call assert_true(1 == index_in_array("abc", strings_to_search), "find match with same case and full name")
        call assert_true(3 == index_in_array("ghi", strings_to_search), "find match with same case and full name")

        call assert_true(3 == index_in_array("GHI", strings_to_search), "find match with different case and full name")
        call assert_true(3 == index_in_array("gh", strings_to_search), "find match with same case and short name")
        call assert_true(3 == index_in_array("GH", strings_to_search), "find match with different case and short name")

    end subroutine test_index_in_array_default_settings

    subroutine test_index_in_array_custom_settings
        ! test default custom settings for string_equals
        character(len=3), allocatable :: strings_to_search(:)

        strings_to_search = (/'abc', 'def', 'ghi', 'jkl', 'abc'/)

        call assert_true(1 == index_in_array("abc", strings_to_search, exact_match=.true.), "exact_match finds match with full name")
        call assert_false(1 == index_in_array("ab", strings_to_search, exact_match=.true.), "exact_match does not find match short name")

        call assert_true(3 == index_in_array("gh", strings_to_search, case_sensitive=.true.), "case_sensitive finds match for short name")
        call assert_false(3 == index_in_array("GHI", strings_to_search, case_sensitive=.true.), "case_sensitive does not find match with different case and full name")
        call assert_false(3 == index_in_array("GH", strings_to_search, case_sensitive=.true.), "case_sensitive does not find match different case and short name")

        call assert_true(3 == index_in_array("ghi", strings_to_search, case_sensitive=.true., exact_match=.true.), "case_sensitive and exact_match finds match for full name right case")
        call assert_false(3 == index_in_array("gh", strings_to_search, case_sensitive=.true., exact_match=.true.), "case_sensitive and exact_match does not find match for short name right case")
        call assert_false(3 == index_in_array("gHi", strings_to_search, case_sensitive=.true., exact_match=.true.), "case_sensitive and exact_match does not find match for full name wrong case")
    end subroutine test_index_in_array_custom_settings

    subroutine test_remove_duplicates
        character(2) :: array(8)
        character(2), allocatable :: unique_values(:)

        !! Arrange
        array = ['AA', 'BB', 'CB', 'AA', 'BB', 'CB', 'DD', 'AA']

        !! Act
        unique_values = remove_duplicates(array)

        !! Assert
        call assert_true('AA' == unique_values(1), '1st element of array with no duplicates')
        call assert_true('BB' == unique_values(2), '2nd element of array with no duplicates')
        call assert_true('CB' == unique_values(3), '3rd element of array with no duplicates')
        call assert_true('DD' == unique_values(4), '4th element of array with no duplicates')

    end subroutine test_remove_duplicates

    subroutine test_centre_text_odd_length
        character(:), allocatable :: text_odd
        character(:), allocatable :: expected_centred_text_odd

        text_odd = 'odd test string'
        expected_centred_text_odd = "                 odd test string                   "

        call test_centre_text(text_odd, expected_centred_text_odd, 50)
    end subroutine test_centre_text_odd_length

    subroutine test_centre_text_even_length
        character(:), allocatable :: text_even
        character(:), allocatable :: expected_centred_text_even

        text_even = 'even test string'
        expected_centred_text_even = "                 even test string                 "

        call test_centre_text(text_even, expected_centred_text_even, 50)
    end subroutine test_centre_text_even_length

    subroutine test_centre_text(text, expected_text, expected_length)
        character(:), allocatable, intent(in) :: text           !< text to centre
        character(:), allocatable, intent(in) :: expected_text  !< expected result
        integer, intent(in) :: expected_length                  !< expected length of the result

        character(:), allocatable :: centred_text

        !! Act
        centred_text = centre_text(text, expected_length)

        !! Assert
        call assert_true(len(centred_text) == expected_length, 'centre_text should be of provided length')
        call assert_true(centred_text == expected_text, 'centre_text should put the provided text in the centre')
    end subroutine test_centre_text

end program tests_string_utils
