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

program tests_file_path_utils
    !!  tests_file_path_utils.f90
    !!  Runs unit tests for tests_file_path_utils

    use m_waq_precision
    use m_file_path_utils
    use m_string_utils, only: string_equals
    use ftnunit, only: runtests_init, &
                       runtests, &
                       runtests_final, &
                       assert_comparable, &
                       test, &
                       assert_true

    implicit none
    character(len=200) :: cmd_arg
    integer :: iargc, getarg
    real(kind=real_wp), parameter :: tolerance = 0.0001

    ! Determine the number of command line arguments
    iargc = command_argument_count()
    call prepare_tests()
    call runtests_init()

    ! Run the test specified in the argument, if no argument run all tests
    if (iargc > 0) then
        call get_command_argument(1, cmd_arg)

        select case (trim(cmd_arg))
        case ('test_get_file_name')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_get_file_name)
        case ('test_get_filename_without_extension')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_get_file_name_without_extension)
        end select
    else
        write (*, *) "No test specified, running all tests"
        call runtests(call_test_get_file_name)
        call runtests(call_test_get_file_name_without_extension)
    end if

    call runtests_final()

contains

    subroutine prepare_tests
        ! prepare_tests
        !     Routine to start the testing
        !
        ! Note:
        !     This routine merely takes care that the unit tests are indeed run
        integer :: lunrun

        open (newunit=lunrun, file='ftnunit.run')
        write (lunrun, '(a)') 'ALL'
        close (lunrun)
    end subroutine prepare_tests

    subroutine show_result
        ! show_result
        !     Start the browser to show the result
        call system('ftnunit.html')
    end subroutine show_result

    subroutine call_test_get_file_name
        call test(test_get_file_name, 'Test getting file name from a path')
    end subroutine

    subroutine call_test_get_file_name_without_extension
        call test(test_get_file_name_without_extension, 'Test getting file name without an extension from a path')
    end subroutine

    subroutine test_get_file_name()
        character(len=17), allocatable :: paths(:)
        character(len=7), allocatable :: expected_file_names(:)

        integer(kind=int_wp) :: i
        character(:), allocatable :: path
        character(:), allocatable :: file_name
        character(:), allocatable :: expected_file_name
        logical :: strings_are_equal

        paths = (/'ghi.jkl          ', &
                  'ghi              ', &
                  'abc/def/ghi.jkl  ', &
                  'abc\\def\\ghi.jkl'/)

        expected_file_names = (/'ghi.jkl', &
                                'ghi    ', &
                                'ghi.jkl', &
                                'ghi.jkl'/)

        do i = 1, size(paths)
            path = trim(paths(i))
            expected_file_name = trim(expected_file_names(i))

            file_name = get_file_name(path)

            strings_are_equal = string_equals(file_name, expected_file_name)
            call assert_true(strings_are_equal, 'file_name is not equal to expected file_name')
        end do

    end subroutine

    subroutine test_get_file_name_without_extension()
        character(len=17), allocatable :: paths(:)
        character(len=3), allocatable :: expected_file_names(:)

        integer(kind=int_wp) :: i
        character(:), allocatable :: path
        character(:), allocatable :: file_name
        character(:), allocatable :: expected_file_name
        logical :: strings_are_equal

        paths = (/'ghi.jkl          ', &
                  'ghi              ', &
                  'abc/def/ghi.jkl  ', &
                  'abc\\def\\ghi.jkl'/)

        expected_file_names = (/'ghi', &
                                'ghi', &
                                'ghi', &
                                'ghi'/)

        do i = 1, size(paths)
            path = trim(paths(i))
            expected_file_name = trim(expected_file_names(i))

            file_name = get_file_name_without_extension(path)

            strings_are_equal = string_equals(file_name, expected_file_name)
            call assert_true(strings_are_equal, 'file name '// file_name // ' is not equal to expected file name ' // expected_file_name)
        end do
    end subroutine

end program
