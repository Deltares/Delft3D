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

module ftnunit
    !!     Module that implements part of the "ftnunit" framework:
    !!     It is inspired by the well-known JUnit framework for
    !!     integrating unit tests in a Java application.
    !!
    !!     The module offers:
    !!     - a set of common utilities, such as assertion checking
    !!       routines
    !!     - a general routine to run the tests if requested
    !!     - resources that keep track of the status
    !!
    !!     Related files:
    !!     ftnunit_test.f90 -- deprecated
    !!     runtests.bat
    !!     runtests.sh
    !!     runtests.tcl
    !!
    !!     TODO: Test the various assertion failures,
    !!     TODO: HTML output of number of differences
    use ftnunit_hooks

    implicit none

    integer, private, parameter :: dp = kind(1.0d0)

    integer, private, save :: last_test           ! Last test that was started
    integer, private, save :: testno              ! Current test number
    integer, private, save :: nofails             ! Number of assertions that failed
    integer, private, save :: noruns              ! Number of runs so far
    logical, private, save :: call_final = .true. ! Call runtests_final implicitly?
    logical, private, save :: previous = .false.! Previous test run?
    integer, private, save :: failed_asserts = 0
    logical, private, save :: has_run = .false.
    character(len = 20), private, save :: html_file = 'ftnunit.html'
    character(len = 20), private :: history_file = "ftnunit.lst"
    character(len = 20), private :: run_file = 'ftnunit.run'
    character(len = :), allocatable, private, save :: testname

    interface assert_equal
        module procedure assert_equal_int
        module procedure assert_equal_int1d
        module procedure assert_equal_logical
        module procedure assert_equal_string
    end interface

    interface assert_comparable
        module procedure assert_comparable_real
        module procedure assert_comparable_real1d
        module procedure assert_comparable_real2d
        module procedure assert_comparable_double
        module procedure assert_comparable_double1d
        module procedure assert_comparable_double2d
    end interface

    private
    public :: prepare_tests, show_result, test, runtests_init, runtests_final, runtests_close_report, runtests
    public :: assert_true, assert_false, assert_equal, assert_comparable, assert_files_comparable
    public :: ftnunit_file_exists, ftnunit_remove_file, ftnunit_make_empty_file

contains

    subroutine prepare_tests
        !! prepare_tests
        !!     Routine to start the testing
        !!
        !! Note:
        !!     This routine merely takes care that the unit tests are indeed run
        integer :: lunrun

        open (newunit = lunrun, file = run_file)
        write (lunrun, '(a)') 'ALL'
        close (lunrun)

    end subroutine prepare_tests

    subroutine show_result
        !! show_result
        !!     Start the browser to show the result
        call system(html_file)
    end subroutine show_result

    subroutine test(test_procedure, test_description)
        !! Runs a unit test and logs its execution.

        external :: test_procedure  !! The subroutine implementing the unit test
        character(len = *) :: test_description  !! Description of the test being run

        integer :: log_unit
        integer :: ierr

        ! Check if the test should run
        testno = testno + 1
        if (testno > last_test) then

            ! Record the fact that we started the test
            has_run = .true.
            open(newunit = log_unit, file = history_file)
            write(log_unit, *) testno, nofails, noruns, ' ', .true.
            close(log_unit)

            testname = test_description
            ! Display test start message
            write(*, '(2a)') 'Test: ', trim(test_description)
            call ftnunit_write_html_test_begin(test_description)
            call ftnunit_hook_test_start(test_description)

            ! Execute the test
            call test_procedure

            ! No runtime error or premature end of
            ! the program ...
            previous = .true.
            open(newunit = log_unit, file = history_file)
            write(log_unit, *) testno, nofails, noruns, ' ', .true.
            close(log_unit)

            call ftnunit_hook_test_stop(test_description)

        end if

    end subroutine test

    subroutine runtests_init
        !! runtests_init --
        !!     Subroutine to initialise the ftnunit system
        !! Arguments:
        !!     None
        !! Note:
        !!     Use in conjunction with runtests_final to enable multiple calls
        !!     to the runtests subroutine. This makes it easier to run tests
        !!     from different modules, as you have more than one subroutine to
        !!     do the actual tests.
        call_final = .false.

        if (.not. ftnunit_file_exists(history_file)) then
            call ftnunit_write_html_header
        endif
    end subroutine


    subroutine runtests_final
        !! runtests_final --
        !!     Subroutine to report the overall statistics
        !! Arguments:
        !!     None
        !! Note:
        !!     Use in conjunction with runtests_init to enable multiple calls
        !!     to the runtests subroutine. This makes it easier to run tests
        !!     from different modules, as you have more than one subroutine to
        !!     do the actual tests.
        if (ftnunit_file_exists(run_file)) then
            write(*, '(a,i5)') 'Number of failed assertions:                ', nofails
            write(*, '(a,i5)') 'Number of runs needed to complete the tests:', noruns
            call ftnunit_remove_file(history_file)
            call ftnunit_write_html_footer
            call ftnunit_hook_test_completed
            stop
        endif
    end subroutine

    subroutine runtests_close_report
        !! runtests_close_report --
        !!     Subroutine to report the overall statistics
        !! Arguments:
        !!     None
        !! Note:
        !!     Use in conjunction with runtests_init to enable multiple calls
        !!     to the runtests subroutine. This makes it easier to run tests
        !!     from different modules, as you have more than one subroutine to
        !!     do the actual tests.
        !!
        !!     This version does not stop
        if (ftnunit_file_exists(run_file)) then
            write(*, '(a,i5)') 'Number of failed assertions:                ', nofails
            write(*, '(a,i5)') 'Number of runs needed to complete the tests:', noruns
            call ftnunit_remove_file(history_file)
            call ftnunit_write_html_footer
            call ftnunit_hook_test_completed
        endif
    end subroutine

    subroutine runtests(testproc)
        ! runtests --
        !     Subroutine to run the tests if requested
        ! Arguments:
        !     testproc      The test subroutine that actually runs the unit test
        interface
            subroutine testproc
            end subroutine testproc
        end interface

        integer :: log_unit
        integer :: ierr

        last_test = 0
        nofails = 0
        noruns = 0
        testno = 0
        previous = .false.
        has_run = .false.

        if (ftnunit_file_exists(run_file)) then
            if (ftnunit_file_exists(history_file)) then
                open(newunit = log_unit, file = history_file, iostat = ierr)
                if (ierr == 0) then
                    read(log_unit, *, iostat = ierr) last_test, nofails, noruns, previous
                    if (ierr /= 0) then
                        last_test = 0
                        nofails = 0
                        noruns = 0
                        previous = .false.
                    endif
                    close(log_unit)
                endif
                if (previous) then
                    call ftnunit_write_html_previous_failed
                endif
            endif

            noruns = noruns + 1
            if (noruns == 1 .and. call_final) then
                call ftnunit_write_html_header
            endif

            call testproc

            if (call_final) then
                call runtests_final
            endif

        endif

    end subroutine runtests


    subroutine assert_true(cond, test_description)
        ! assert_true --
        !     Subroutine to check if a condition is true
        ! Arguments:
        !     cond          Condition to be checked
        !     test_description          Text describing the assertion
        ! Side effects:
        !     If the assertion fails, this is reported to standard
        !     output. Also, nofails is increased by one.
        logical, intent(in) :: cond
        character(len = *), intent(in) :: test_description

        if (.not. cond) then
            nofails = nofails + 1
            write(*, *) '    Condition "', trim(test_description), '" failed'
            write(*, *) '    It should have been true'
            call ftnunit_write_html_failed_logic(test_description, .true.)
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Condition should have been true")
        endif
    end subroutine assert_true

    subroutine assert_false(cond, test_description)
        !! assert_false --
        !!     Subroutine to check if a condition is false
        !! Arguments:
        !!     cond          Condition to be checked
        !!     test_description          Text describing the assertion
        !! Side effects:
        !!     If the assertion fails, this is reported to standard
        !!     output. Also, nofails is increased by one.
        logical, intent(in) :: cond
        character(len = *), intent(in) :: test_description

        if (cond) then
            nofails = nofails + 1
            write(*, *) '    Condition "', trim(test_description), '" failed'
            write(*, *) '    It should have been false'
            call ftnunit_write_html_failed_logic(test_description, .false.)
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Condition should have been false")
        endif
    end subroutine assert_false

    subroutine assert_equal_logical(value1, value2, test_description)
        !! assert_equal_logical --
        !!     Subroutine to check if two logical values are equivalent
        !! Arguments:
        !!     value1        First logical value
        !!     value2        Second logical value
        !!     test_description          Text describing the assertion
        !! Side effects:
        !!     If the assertion fails, this is reported to standard
        !!     output. Also, nofails is increased by one.
        logical, intent(in) :: value1
        logical, intent(in) :: value2
        character(len = *), intent(in) :: test_description

        if (value1 .neqv. value2) then
            nofails = nofails + 1
            write(*, *) '    Assertion "', trim(test_description), '" failed'
            write(*, *) '    The two logical values are not the same'
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Logical values are not the same")
            call ftnunit_write_html_failed_equivalent(test_description)
        endif
    end subroutine assert_equal_logical

    subroutine assert_equal_string(value1, value2, test_description)
        !! assert_equal_string --
        !!     Subroutine to check if two string are equal
        !! Arguments:
        !!     value1        First string value
        !!     value2        Second string value
        !!     test_description          Text describing the assertion
        !! Side effects:
        !!     If the assertion fails, this is reported to standard
        !!     output. Also, nofails is increased by one.
        character(len = *), intent(in) :: value1
        character(len = *), intent(in) :: value2
        character(len = *), intent(in) :: test_description

        if (value1 /= value2) then
            nofails = nofails + 1
            write(*, *) '    Assertion "', trim(test_description), '" failed'
            write(*, *) '    The two strings are not the same:'
            write(*, *) '    String 1: ' // trim(value1)
            write(*, *) '    String 2: ' // trim(value2)
            call ftnunit_write_html_failed_string(test_description, value1, value2)
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Strings are not the same")
        endif
    end subroutine assert_equal_string

    subroutine assert_equal_int(value1, value2, test_description)
        !! assert_equal_int --
        !!     Subroutine to check if two integers are equal
        !! Arguments:
        !!     value1        First value
        !!     value2        Second value
        !!     test_description          Text describing the assertion
        !! Side effects:
        !!     If the assertion fails, this is reported to standard
        !!     output. Also, nofails is increased by one.
        integer, intent(in) :: value1
        integer, intent(in) :: value2
        character(len = *), intent(in) :: test_description

        if (value1 /= value2) then
            nofails = nofails + 1
            write(*, *) '    Values not equal: "', trim(test_description), '" - assertion failed'
            write(*, *) '    Values: ', value1, ' and ', value2
            call ftnunit_write_html_failed_int(test_description, value1, value2)
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Integer values are not the same")
        endif
    end subroutine assert_equal_int

    subroutine assert_equal_int1d(array1, array2, test_description)
        !! assert_equal_int1d --
        !!     Subroutine to check if two integer arrays are equal
        !! Arguments:
        !!     array1        First array
        !!     array2        Second array
        !!     test_description          Text describing the assertion
        !! Side effects:
        !!     If the assertion fails, this is reported to standard
        !!     output. Also, nofails is increased by one.
        integer, dimension(:), intent(in) :: array1
        integer, dimension(:), intent(in) :: array2
        character(len = *), intent(in) :: test_description

        integer :: i
        integer :: count
        logical :: addtext

        addtext = .false.

        if (size(array1) /= size(array2)) then
            nofails = nofails + 1
            write(*, *) '    Arrays have different sizes: "', trim(test_description), '" - assertion failed'
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Arrays have different sizes")
        else
            if (any(array1 /= array2)) then
                nofails = nofails + 1
                write(*, *) '    One or more values different: "', trim(test_description), '" - assertion failed'
                call ftnunit_hook_test_assertion_failed(testname, test_description, "One or more values are different")
                count = 0
                do i = 1, size(array1)
                    if (array1(i) /= array2(i)) then
                        count = count + 1
                        write(*, '(3a10)')    '    Index', '     First', '    Second'
                        if (count < 50) then
                            write(*, '(3i10)')    i, array1(i), array2(i)
                            call ftnunit_write_html_failed_int1d(&
                                    test_description, i, array1(i), array2(i), addtext)
                            addtext = .false.
                        endif
                        write(*, *) 'Number of differences: ', count
                    endif
                enddo
            endif
        endif
    end subroutine assert_equal_int1d


    subroutine assert_comparable_real(value1, value2, margin, test_description)
        !! assert_comparable_real --
        !!     Subroutine to check if two reals are approximately equal
        !! Arguments:
        !!     value1        First value
        !!     value2        Second value
        !!     margin        Allowed margin (relative)
        !!     test_description          Text describing the assertion
        !! Side effects:
        !!     If the assertion fails, this is reported to standard
        !!     output. Also, nofails is increased by one.
        real, intent(in) :: value1
        real, intent(in) :: value2
        real, intent(in) :: margin
        character(len = *), intent(in) :: test_description

        if (abs(value1 - value2) > 0.5 * margin * (abs(value1) + abs(value2))) then
            nofails = nofails + 1
            write(*, *) '    Values not comparable: "', trim(test_description), '" - assertion failed'
            write(*, *) '    Values: actual = ', value1, ' and expected = ', value2
            call ftnunit_hook_test_assertion_failed(testname, test_description, "One or more values differ more than the margin")
            call ftnunit_write_html_failed_real(test_description, value1, value2)
        endif
    end subroutine assert_comparable_real

    ! assert_comparable_real1d --
    !     Subroutine to check if two real arrays are comparable
    ! Arguments:
    !     array1        First array
    !     array2        Second array
    !     test_description          Text describing the assertion
    ! Side effects:
    !     If the assertion fails, this is reported to standard
    !     output. Also, nofails is increased by one.
    !
    subroutine assert_comparable_real1d(array1, array2, margin, test_description)
        real, dimension(:), intent(in) :: array1
        real, dimension(:), intent(in) :: array2
        real, intent(in) :: margin
        character(len = *), intent(in) :: test_description

        integer :: i
        integer :: count
        logical :: addtext

        addtext = .false.

        if (size(array1) /= size(array2)) then
            nofails = nofails + 1
            write(*, *) '    Arrays have different sizes: "', trim(test_description), '" - assertion failed'
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Arrays have different sizes")
        else
            if (any(abs(array1 - array2) > 0.5 * margin * (abs(array1) + abs(array2)))) then
                nofails = nofails + 1
                write(*, *) '    One or more values different: "', trim(test_description), '" - assertion failed'
                call ftnunit_hook_test_assertion_failed(testname, test_description, "One or more values differ more than the margin")
                count = 0
                do i = 1, size(array1)
                    if (abs(array1(i) - array2(i)) > &
                            0.5 * margin * (abs(array1(i)) + abs(array2(i)))) then
                        count = count + 1
                        write(*, '(a10,2a15)')    '    Index', '          First', '         Second'
                        if (count < 50) then
                            write(*, '(i10,2e15.5)')    i, array1(i), array2(i)
                            call ftnunit_write_html_failed_real1d(&
                                    test_description, i, array1(i), array2(i), addtext)
                            addtext = .false.
                        endif
                    endif
                enddo
                if (count > 0) then
                    write(*, *) 'Number of differences: ', count
                endif
            endif
        endif
    end subroutine assert_comparable_real1d

    ! assert_comparable_real2d --
    !     Subroutine to check if two two-dimensional real arrays are comparable
    ! Arguments:
    !     array1        First array
    !     array2        Second array
    !     test_description          Text describing the assertion
    ! Side effects:
    !     If the assertion fails, this is reported to standard
    !     output. Also, nofails is increased by one.
    !
    subroutine assert_comparable_real2d(array1, array2, margin, test_description)
        real, dimension(:, :), intent(in) :: array1
        real, dimension(:, :), intent(in) :: array2
        real, intent(in) :: margin
        character(len = *), intent(in) :: test_description

        integer :: i
        integer :: j
        integer :: count
        logical :: addtext

        addtext = .false.

        if (any(shape(array1) /= shape(array2))) then
            nofails = nofails + 1
            write(*, *) '    Arrays have different shapes: "', trim(test_description), '" - assertion failed'
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Arrays have different shapes")
        else
            if (any(abs(array1 - array2) > 0.5 * margin * (abs(array1) + abs(array2)))) then
                nofails = nofails + 1
                write(*, *) '    One or more values different: "', trim(test_description), '" - assertion failed'
                call ftnunit_hook_test_assertion_failed(testname, test_description, "One or more values differ more than the margin")
                count = 0
                do j = 1, size(array1, 2)
                    do i = 1, size(array1, 1)
                        if (abs(array1(i, j) - array2(i, j)) > &
                                0.5 * margin * (abs(array1(i, j)) + abs(array2(i, j)))) then
                            count = count + 1
                            write(*, '(a10,2a15)')    '    Index', '          First', '         Second'
                            if (count < 50) then
                                write(*, '(2i5,2e15.5)')    i, j, array1(i, j), array2(i, j)
                                call ftnunit_write_html_failed_real2d(&
                                        test_description, i, j, array1(i, j), array2(i, j), addtext)
                                addtext = .false.
                            endif
                        endif
                    enddo
                enddo
                if (count > 0) then
                    write(*, *) 'Number of differences: ', count
                endif
            endif
        endif
    end subroutine assert_comparable_real2d

    ! assert_comparable_double --
    !     Subroutine to check if two reals are approximately equal
    ! Arguments:
    !     value1        First value
    !     value2        Second value
    !     margin        Allowed margin (relative)
    !     test_description          Text describing the assertion
    ! Side effects:
    !     If the assertion fails, this is reported to standard
    !     output. Also, nofails is increased by one.
    !
    subroutine assert_comparable_double(value1, value2, margin, test_description)
        real(kind = dp), intent(in) :: value1
        real(kind = dp), intent(in) :: value2
        real(kind = dp), intent(in) :: margin
        character(len = *), intent(in) :: test_description

        if (abs(value1 - value2) > 0.5_dp * margin * (abs(value1) + abs(value2))) then
            nofails = nofails + 1
            write(*, *) '    Values not comparable: "', trim(test_description), '" - assertion failed'
            write(*, *) '    Values: ', value1, ' and ', value2
            call ftnunit_hook_test_assertion_failed(testname, test_description, "One or more values differ more than the margin")
            call ftnunit_write_html_failed_double(test_description, value1, value2)
        endif
    end subroutine assert_comparable_double

    ! assert_comparable_double1d --
    !     Subroutine to check if two real arrays are comparable
    ! Arguments:
    !     array1        First array
    !     array2        Second array
    !     test_description          Text describing the assertion
    ! Side effects:
    !     If the assertion fails, this is reported to standard
    !     output. Also, nofails is increased by one.
    !
    subroutine assert_comparable_double1d(array1, array2, margin, test_description)
        real(kind = dp), dimension(:), intent(in) :: array1
        real(kind = dp), dimension(:), intent(in) :: array2
        real(kind = dp), intent(in) :: margin
        character(len = *), intent(in) :: test_description

        integer :: i
        integer :: count
        logical :: addtext

        addtext = .false.

        if (size(array1) /= size(array2)) then
            nofails = nofails + 1
            write(*, *) '    Arrays have different sizes: "', trim(test_description), '" - assertion failed'
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Arrays have different sizes")
        else
            if (any(abs(array1 - array2) > 0.5_dp * margin * (abs(array1) + abs(array2)))) then
                nofails = nofails + 1
                write(*, *) '    One or more values different: "', trim(test_description), '" - assertion failed'
                call ftnunit_hook_test_assertion_failed(testname, test_description, "One or more values differ more than the margin")
                count = 0
                do i = 1, size(array1)
                    if (abs(array1(i) - array2(i)) > &
                            0.5_dp * margin * (abs(array1(i)) + abs(array2(i)))) then
                        count = count + 1
                        write(*, '(a10,2a15)')    '    Index', '          First', '         Second'
                        if (count < 50) then
                            write(*, '(i10,2e15.5)')    i, array1(i), array2(i)
                            call ftnunit_write_html_failed_double1d(&
                                    test_description, i, array1(i), array2(i), addtext)
                            addtext = .false.
                        endif
                    endif
                enddo
                if (count > 0) then
                    write(*, *) 'Number of differences: ', count
                endif
            endif
        endif
    end subroutine assert_comparable_double1d

    ! assert_comparable_double2d --
    !     Subroutine to check if two two-dimensional real arrays are comparable
    ! Arguments:
    !     array1        First array
    !     array2        Second array
    !     test_description          Text describing the assertion
    ! Side effects:
    !     If the assertion fails, this is reported to standard
    !     output. Also, nofails is increased by one.
    !
    subroutine assert_comparable_double2d(array1, array2, margin, test_description)
        real(kind = dp), dimension(:, :), intent(in) :: array1
        real(kind = dp), dimension(:, :), intent(in) :: array2
        real(kind = dp), intent(in) :: margin
        character(len = *), intent(in) :: test_description

        integer :: i
        integer :: j
        integer :: count
        logical :: addtext

        addtext = .false.

        if (any(shape(array1) /= shape(array2))) then
            nofails = nofails + 1
            write(*, *) '    Arrays have different shapes: "', trim(test_description), '" - assertion failed'
            call ftnunit_hook_test_assertion_failed(testname, test_description, "Arrays have different shapes")
        else
            if (any(abs(array1 - array2) > 0.5_dp * margin * (abs(array1) + abs(array2)))) then
                nofails = nofails + 1
                write(*, *) '    One or more values different: "', trim(test_description), '" - assertion failed'
                call ftnunit_hook_test_assertion_failed(testname, test_description, "One or more values differ more than the margin")
                count = 0
                do j = 1, size(array1, 2)
                    do i = 1, size(array1, 1)
                        if (abs(array1(i, j) - array2(i, j)) > &
                                0.5_dp * margin * (abs(array1(i, j)) + abs(array2(i, j)))) then
                            count = count + 1
                            write(*, '(a10,2a15)')    '    Index', '          First', '         Second'
                            if (count < 50) then
                                write(*, '(2i5,2e15.5)')    i, j, array1(i, j), array2(i, j)
                                call ftnunit_write_html_failed_double2d(&
                                        test_description, i, j, array1(i, j), array2(i, j), addtext)
                                addtext = .false.
                            endif
                        endif
                    enddo
                enddo
                if (count > 0) then
                    write(*, *) 'Number of differences: ', count
                endif
            endif
        endif
    end subroutine assert_comparable_double2d

    ! assert_files_comparable --
    !     Compare two files and establish whether they are equal or not
    !
    ! Arguments:
    !     filename1       Name of the first file
    !     filename2       Name of the second file
    !     test_description            Text describing the assertion
    !     tolerance       Relative tolerance for numbers
    !                     (optional; defaults to 1.0e-5)
    !
    ! Note:
    !     The files are compared line by line and the items
    !     within each line are compared too for numbers that
    !     are almost equal.
    !     The lines are read into a variable of length line_length
    !     Each line may contain up to max_items items (character
    !     strings) of length item_length.
    !
    subroutine assert_files_comparable(filename1, filename2, &
            test_description, tolerance)

        integer, parameter :: line_length = 200
        integer, parameter :: item_length = 20
        integer, parameter :: max_items = 100

        character(len = *), intent(in) :: filename1
        character(len = *), intent(in) :: filename2
        character(len = *), intent(in) :: test_description
        real, intent(in), optional :: tolerance

        real :: tol
        real :: value1
        real :: value2
        integer :: i
        integer :: lun1
        integer :: lun2
        integer :: differences
        integer :: ierr1
        integer :: ierr2
        logical :: header
        character(len = line_length) :: line1
        character(len = line_length) :: line2
        character(len = item_length), dimension(max_items) :: item1
        character(len = item_length), dimension(max_items) :: item2

        header = .true.
        differences = 0
        tol = 1.0e-5
        if (present(tolerance)) tol = tolerance

        open(newunit = lun1, file = filename1, status = 'old', iostat = ierr1)

        open(newunit = lun2, file = filename2, status = 'old', iostat = ierr2)

        if (ierr1 /= 0 .or. ierr2 /= 0) then
            nofails = nofails + 1
            call write_header

            if (ierr1 /= 0) then
                write(*, '(a)') 'Input file ' // trim(filename1) // ' could not be opened'
            endif

            if (ierr2 /= 0) then
                write(*, '(a)') 'Input file ' // trim(filename2) // ' could not be opened'
            endif

            write(*, '(a)') 'Comparison failed'
            call ftnunit_write_html_failed_files(test_description, filename1, filename2, &
                    'One or both files could not be opened')
            call ftnunit_hook_test_assertion_failed(testname, test_description, "One or both files could not be opened")

            return
        endif

        do
            read(lun1, '(a)', iostat = ierr1) line1
            read(lun2, '(a)', iostat = ierr2) line2

            !
            ! End of file for both?
            !
            if (ierr1 < 0 .and. ierr2 < 0) then
                exit
            endif

            !
            ! End of file or error for only one?
            !
            if (ierr1 /= 0 .or. ierr2 /= 0) then
                call write_header

                if (ierr1 /= 0) then
                    write(*, '(a)') 'Error/end-of-file reading file ' // trim(filename1)
                endif

                if (ierr2 /= 0) then
                    write(*, '(a)') 'Error/end-of-file reading file ' // trim(filename2)
                endif

                write(*, '(a)') 'Comparison failed'
                call ftnunit_write_html_failed_files(test_description, filename1, filename2, &
                        'Error/premature end-of-file reading the files')
                call ftnunit_hook_test_assertion_failed(testname, test_description, "Error/premature end-of-file reading the files")
                return
            endif

            !
            ! If the lines are equal, there is nothing to do
            !
            if (line1 == line2) then
                cycle
            endif

            !
            ! The lines are different - but sufficiently different?
            !
            item1 = '?'
            item2 = '?'

            read(line1, *, iostat = ierr1) item1
            read(line2, *, iostat = ierr2) item2

            !
            ! We are not interested in the error codes, but in
            ! the contents of the items
            !
            do i = 1, max_items
                if (item1(i) /= item2(i)) then
                    read(item1(i), *, iostat = ierr1) value1
                    read(item2(i), *, iostat = ierr2) value2

                    if (ierr1 == 0 .and. ierr2 == 0) then
                        if (abs(value1 - value2) > 0.5 * tol * (abs(value1) + abs(value2))) then
                            nofails = nofails + 1
                            call write_header

                            write(*, '(a)') 'File 1: ' // trim(line1)
                            write(*, '(a)') 'File 2: ' // trim(line2)
                            write(*, '(a,i5,a,e12.4)') &
                                    'Difference in item ', i, ': ', abs(value1 - value2)
                            write(*, '(a,e12.4)') &
                                    'Mean absolute value       : ', 0.5 * (abs(value1) + abs(value2))

                            differences = differences + 1
                            if (differences == 1) then
                                call ftnunit_write_html_failed_files(test_description, line1, line2, &
                                        'Differences in one or more numerical items')
                                call ftnunit_hook_test_assertion_failed(testname, test_description, "Differences in one or more numerical items")
                            endif
                            exit
                        endif

                    elseif (ierr1 == 0 .neqv. ierr2 == 0) then
                        !
                        ! Only one is a number?
                        !
                        nofails = nofails + 1
                        call write_header

                        write(*, '(a)') 'File 1: ' // trim(line1)
                        write(*, '(a)') 'File 2: ' // trim(line2)
                        write(*, '(a,i5,a,e12.4)') &
                                'Item ', i, ' is a number in one file and not in the other'

                        differences = differences + 1
                        if (differences == 1) then
                            call ftnunit_write_html_failed_files(test_description, line1, line2, &
                                    'Corresponding items not both numerical')
                            call ftnunit_hook_test_assertion_failed(testname, test_description, "Corresponding items not both numerical")
                        endif
                        exit
                    else
                        !
                        ! Unequal substrings
                        !
                        nofails = nofails + 1
                        call write_header

                        write(*, '(a)') 'File 1: ' // trim(line1)
                        write(*, '(a)') 'File 2: ' // trim(line2)
                        write(*, '(a,i5,a,e12.4)') &
                                'Item ', i, ' differs in the two lines'
                        differences = differences + 1
                        if (differences == 1) then
                            call ftnunit_write_html_failed_files(test_description, line1, line2, &
                                    'Differences in one or more string items')
                            call ftnunit_hook_test_assertion_failed(testname, test_description, "Differences in one or more string items")
                        endif
                        exit
                    endif
                endif
            enddo
        enddo

        close(lun1)
        close(lun2)
    contains
        subroutine write_header
            if (header) then
                header = .false.
                write(*, '(a)') 'Comparing files:'
                write(*, '(a)') '    File 1:' // trim(filename1)
                write(*, '(a)') '    File 2:' // trim(filename2)
            endif
        end subroutine write_header
    end subroutine assert_files_comparable

    logical function ftnunit_file_exists(filename)
        !! ftnunit_file_exists --
        !!     Auxiliary function to see if a file exists
        !! Arguments:
        !!     filename      Name of the file to check
        !! Returns:
        !!     .true. if the file exists, .false. otherwise
        character(len = *), intent(in) :: filename

        inquire(file = filename, exist = ftnunit_file_exists)
    end function ftnunit_file_exists

    ! ftnunit_remove_file --
    !     Auxiliary subroutine to remove a file
    ! Arguments:
    !     filename      Name of the file to be removed
    !
    subroutine ftnunit_remove_file(filename)
        character(len = *), intent(in) :: filename

        integer :: log_unit
        integer :: ierr

        open(newunit = log_unit, file = filename, iostat = ierr)
        if (ierr /= 0) then
            write(*, '(10a)') '    Could not open file for removal: ', trim(filename)
            nofails = nofails + 1
        else
            close(log_unit, status = 'delete')
            if (ftnunit_file_exists(filename)) then
                write(*, '(10a)') '    Removal of file unsuccessful: ', trim(filename)
                nofails = nofails + 1
            endif
        endif

    end subroutine ftnunit_remove_file

    ! ftnunit_make_empty_file --
    !     Auxiliary subroutine to make an empty file
    ! Arguments:
    !     filename      Name of the file to be created
    !
    subroutine ftnunit_make_empty_file(filename)
        character(len = *), intent(in) :: filename

        integer :: log_unit
        integer :: ierr

        if (ftnunit_file_exists(filename)) then
            call ftnunit_remove_file(filename)
        endif
        open(newunit = log_unit, file = filename, iostat = ierr, status = 'new')
        if (ierr /= 0) then
            write(*, '(10a)') '    Failed to create empty file: ', trim(filename)
            nofails = nofails + 1
        else
            close(log_unit)
        endif

    end subroutine ftnunit_make_empty_file

    ! ftnunit_write_html_header --
    !     Auxiliary subroutine to write the header of the HTML file
    ! Arguments:
    !     None
    !
    subroutine ftnunit_write_html_header
        integer :: log_unit

        open(newunit = log_unit, file = html_file)
        write(log_unit, '(a)') &
                '<html>', &
                '<header>', &
                '<title>Results of unit tests</title>', &
                '<style type="text/css">', &
                'span.indent {', &
                '   text-indent: 30;', &
                '}', &
                'span.red {', &
                '   background: red;', &
                '}', &
                'span.green {', &
                '   background: lime;', &
                '}', &
                '</style>', &
                '</header>', &
                '<body>', &
                '<h3>Result of unit tests</h3>', &
                '<table>'
        close(log_unit)

    end subroutine ftnunit_write_html_header

    ! ftnunit_write_html_footer --
    !     Auxiliary subroutine to write the footer of the HTML file
    ! Arguments:
    !     None
    !
    subroutine ftnunit_write_html_footer
        integer :: log_unit

        if (.not. has_run) then
            call ftnunit_write_html_previous_failed
        endif

        open(newunit = log_unit, file = html_file, position = 'append')

        if (has_run .and. failed_asserts == 0) then
            call ftnunit_write_html_close_row(log_unit)
        endif

        write(log_unit, '(a)') &
                '</tr>', &
                '</table>'
        write(log_unit, '(a,i5)') '<p>Number of failed assertions: ', nofails
        write(log_unit, '(a,i5)') '<br>Number of runs needed to complete the tests:', noruns
        write(log_unit, '(a)') &
                '</body>', &
                '</html>'
        close(log_unit)

    end subroutine ftnunit_write_html_footer

    ! ftnunit_write_html_test_begin --
    !     Auxiliary subroutine to write the test text to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !
    subroutine ftnunit_write_html_test_begin(test_description)
        character(len = *) :: test_description

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        if (previous) then
            if (failed_asserts == 0) then
                write(log_unit, '(a)') &
                        '<td><span class="green">OK</span></td></tr>'
            else
                write(log_unit, '(a)') &
                        '</tr>'
            endif
        endif

        failed_asserts = 0

        write(log_unit, '(a)') &
                '<tr>', &
                '<td>', trim(test_description), '</td>'
        close(log_unit)

        previous = .true.

    end subroutine ftnunit_write_html_test_begin

    ! ftnunit_write_html_previous_failed --
    !     Auxiliary subroutine to write the closing of the failed test
    !     from the previous run to the HTML file
    ! Arguments:
    !     None
    !
    ! Note:
    !     Apparently the test caused a run-time error, so most probably no
    !     assertions were checked.
    !
    subroutine ftnunit_write_html_previous_failed

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        write(log_unit, '(a)') &
                '<td><span class="red">Crashed</span></td></tr>', &
                '<tr><td><span class="indent">Possible run-time failure: check the log file</span></td></tr>'

        close(log_unit)

        failed_asserts = 1 ! Implicit assertion failed that the test will complete

    end subroutine ftnunit_write_html_previous_failed

    ! ftnunit_write_html_close_row --
    !     Auxiliary subroutine to write the closing of a row to the HTML file
    ! Arguments:
    !     log_unit          LU-number for the HTML file
    !
    subroutine ftnunit_write_html_close_row(log_unit)
        integer :: log_unit

        if (failed_asserts > 0) then
            if (failed_asserts == 1) then
                write(log_unit, '(a)') &
                        '<td><span class="red">Failed</span></td>'
            endif
            write(log_unit, '(a)') &
                    '</tr><tr>'
        else
            write(log_unit, '(a)') &
                    '<td><span class="green">OK</span></td>'
        endif

    end subroutine ftnunit_write_html_close_row

    ! ftnunit_write_html_failed_logic --
    !     Auxiliary subroutine to write a failed logic assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     expected     Expected value
    !
    subroutine ftnunit_write_html_failed_logic(test_description, expected)
        character(len = *) :: test_description
        logical :: expected

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        write(log_unit, '(a)') &
                '<td><span class="indent">', trim(test_description), '</span></td>', &
                '<td>Value should have been "', trim(merge("true ", "false", expected)), '"</td>'
        close(log_unit)

    end subroutine ftnunit_write_html_failed_logic

    ! ftnunit_write_html_failed_equivalent --
    !     Auxiliary subroutine to write a failed equivalent assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     expected     Expected value
    !
    subroutine ftnunit_write_html_failed_equivalent(test_description)
        character(len = *) :: test_description
        logical :: expected

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        write(log_unit, '(a)') &
                '<td><span class="indent">', trim(test_description), '</span></td>', &
                '<td>Logical values should have been equal</td>'
        close(log_unit)

    end subroutine ftnunit_write_html_failed_equivalent

    ! ftnunit_write_html_failed_string --
    !     Auxiliary subroutine to write a failed string assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     expected     Expected value
    !
    subroutine ftnunit_write_html_failed_string(test_description, value1, value2)
        character(len = *) :: test_description
        character(len = *) :: value1
        character(len = *) :: value2

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        write(log_unit, '(a)') &
                '<td><span class="indent">', trim(test_description), '</span></td>', &
                '<td>Strings should have been equal:<br>', &
                'String 1: ', value1, '<br>', &
                'String 2: ', value2, '</td>'
        close(log_unit)

    end subroutine ftnunit_write_html_failed_string

    ! ftnunit_write_html_failed_int --
    !     Auxiliary subroutine to write a failed integer assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     value1       First value
    !     value2       Second value
    !
    subroutine ftnunit_write_html_failed_int(test_description, value1, value2)
        character(len = *) :: test_description
        integer :: value1
        integer :: value2

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        write(log_unit, '(a)') &
                '<td><span class="indent">', trim(test_description), '</span></td>', &
                '<td>Values: '
        write(log_unit, '(i0,a,i0,a)') &
                value1, ' -- ', value2, '</td>'
        close(log_unit)

    end subroutine ftnunit_write_html_failed_int

    ! ftnunit_write_html_failed_int1d --
    !     Auxiliary subroutine to write a failed integer assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     idx          Index
    !     value1       First value
    !     value2       Second value
    !     addtext      Add the test_description or not
    !
    subroutine ftnunit_write_html_failed_int1d(test_description, idx, value1, value2, addtext)
        character(len = *) :: test_description
        integer :: idx
        integer :: value1
        integer :: value2
        logical :: addtext

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        if (addtext) then
            write(log_unit, '(a)') &
                    '<td><span class="indent">', trim(test_description), '</span></td>'
        else
            write(log_unit, '(a)') &
                    '<td></td>'
        endif

        write(log_unit, '(a)') &
                '<td>Values at index: '
        write(log_unit, '(i0,a,i0,a,i0,a)') &
                idx, ':', &
                value1, ' -- ', value2, '</td>'
        close(log_unit)

    end subroutine ftnunit_write_html_failed_int1d

    ! ftnunit_write_html_failed_real --
    !     Auxiliary subroutine to write a failed real assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     value1       First value
    !     value2       Second value
    !
    subroutine ftnunit_write_html_failed_real(test_description, value1, value2)
        character(len = *) :: test_description
        real :: value1
        real :: value2

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        write(log_unit, '(a)') &
                '<td><span class="indent">', trim(test_description), '</span></td>', &
                '<td>Difference in values: '
        write(log_unit, '(a,e15.7,a,e15.7,a)') &
                'Actual = ', value1, ' vs Expected = ', value2, '</td>'
        close(log_unit)

    end subroutine ftnunit_write_html_failed_real

    ! ftnunit_write_html_failed_real1d --
    !     Auxiliary subroutine to write a failed real assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     idx          Index
    !     value1       First value
    !     value2       Second value
    !     addtext      Add the test_description or not
    !
    subroutine ftnunit_write_html_failed_real1d(test_description, idx, value1, value2, addtext)
        character(len = *) :: test_description
        integer :: idx
        real :: value1
        real :: value2
        logical :: addtext

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        if (addtext) then
            write(log_unit, '(a)') &
                    '<td><span class="indent">', trim(test_description), '</span></td>'
        else
            write(log_unit, '(a)') &
                    '<td></td>'
        endif

        write(log_unit, '(a)') &
                '<td>Values at index: '
        write(log_unit, '(i0,a,g15.5,a,g15.5,a)') &
                idx, ':', &
                value1, ' -- ', value2, '</td>'
        close(log_unit)
    end subroutine ftnunit_write_html_failed_real1d

    ! ftnunit_write_html_failed_real2d --
    !     Auxiliary subroutine to write a failed real assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     idx1         Index 1
    !     idx2         Index 2
    !     value1       First value
    !     value2       Second value
    !     addtext      Add the test_description or not
    !
    subroutine ftnunit_write_html_failed_real2d(test_description, idx1, idx2, value1, value2, addtext)
        character(len = *) :: test_description
        integer :: idx1
        integer :: idx2
        real :: value1
        real :: value2
        logical :: addtext

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        if (addtext) then
            write(log_unit, '(a)') &
                    '<td><span class="indent">', trim(test_description), '</span></td>'
        else
            write(log_unit, '(a)') &
                    '<td></td>'
        endif

        write(log_unit, '(a)') &
                '<td>Values at index: '
        write(log_unit, '(i0,a,i0,a,g15.5,a,g15.5,a)') &
                idx1, ',', idx2, ':', &
                value1, ' -- ', value2, '</td>'
        close(log_unit)
    end subroutine ftnunit_write_html_failed_real2d

    ! ftnunit_write_html_failed_double --
    !     Auxiliary subroutine to write a failed real assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     value1       First value
    !     value2       Second value
    !
    subroutine ftnunit_write_html_failed_double(test_description, value1, value2)
        character(len = *) :: test_description
        real(kind = dp) :: value1
        real(kind = dp) :: value2

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        write(log_unit, '(a)') &
                '<td><span class="indent">', trim(test_description), '</span></td>', &
                '<td>Values: '
        write(log_unit, '(e15.7,a,e15.7,a)') &
                value1, ' -- ', value2, '</td>'
        close(log_unit)

    end subroutine ftnunit_write_html_failed_double

    ! ftnunit_write_html_failed_double1d --
    !     Auxiliary subroutine to write a failed real assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     idx          Index
    !     value1       First value
    !     value2       Second value
    !     addtext      Add the test_description or not
    !
    subroutine ftnunit_write_html_failed_double1d(test_description, idx, value1, value2, addtext)
        character(len = *) :: test_description
        integer :: idx
        real(kind = dp) :: value1
        real(kind = dp) :: value2
        logical :: addtext

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        if (addtext) then
            write(log_unit, '(a)') &
                    '<td><span class="indent">', trim(test_description), '</span></td>'
        else
            write(log_unit, '(a)') &
                    '<td></td>'
        endif

        write(log_unit, '(a)') &
                '<td>Values at index: '
        write(log_unit, '(i0,a,g15.5,a,g15.5,a)') &
                idx, ':', &
                value1, ' -- ', value2, '</td>'
        close(log_unit)
    end subroutine ftnunit_write_html_failed_double1d

    ! ftnunit_write_html_failed_double2d --
    !     Auxiliary subroutine to write a failed real assertion to the HTML file
    ! Arguments:
    !     test_description         Description of the test
    !     idx1         Index 1
    !     idx2         Index 2
    !     value1       First value
    !     value2       Second value
    !     addtext      Add the test_description or not
    !
    subroutine ftnunit_write_html_failed_double2d(test_description, idx1, idx2, value1, value2, addtext)
        character(len = *) :: test_description
        integer :: idx1
        integer :: idx2
        real(kind = dp) :: value1
        real(kind = dp) :: value2
        logical :: addtext

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        if (addtext) then
            write(log_unit, '(a)') &
                    '<td><span class="indent">', trim(test_description), '</span></td>'
        else
            write(log_unit, '(a)') &
                    '<td></td>'
        endif

        write(log_unit, '(a)') &
                '<td>Values at index: '
        write(log_unit, '(i0,a,i0,a,g15.5,a,g15.5,a)') &
                idx1, ',', idx2, ':', &
                value1, ' -- ', value2, '</td>'
        close(log_unit)
    end subroutine ftnunit_write_html_failed_double2d


    subroutine ftnunit_write_html_failed_files(test_description, string1, string2, string3)
        character(len = *) :: test_description
        integer :: type
        character(len = *) :: string1
        character(len = *) :: string2
        character(len = *) :: string3

        integer :: log_unit

        open(newunit = log_unit, file = html_file, position = 'append')

        failed_asserts = failed_asserts + 1

        call ftnunit_write_html_close_row(log_unit)

        write(log_unit, '(a)') &
                '<td><span class="indent">', trim(test_description), '</span></td>', &
                '<td>Failure: '

        write(log_unit, '(3a)') string3, '<br>'
        write(log_unit, '(3a)') 'File 1: ', trim(string1), '<br>'
        write(log_unit, '(3a)') 'File 2: ', trim(string2), '</td>'

        close(log_unit)
    end subroutine ftnunit_write_html_failed_files

end module ftnunit
