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

program test_write_restart_map_file
    !! tests_write_restart_map_file.f90 --
    !! Run unit tests for routines in Delft3D-WAQ

    use ftnunit, only: runtests_init, runtests, runtests_final, assert_true, assert_files_comparable, test, &
            prepare_tests
    use m_write_restart_map_file, only: write_restart_map_file

    implicit none
    character(len = 200) :: cmd_arg
    integer :: iargc

    ! Determine the number of command line arguments
    iargc = command_argument_count()

    call prepare_tests
    call runtests_init

    ! Run a specific test based on the first argument
    if (iargc > 0) then
        call get_command_argument(1, cmd_arg)

        select case (trim(cmd_arg))
        case('test_write_restart_map_file_recognise_nans')
            write(*, *) "Running test_write_restart_map_file_recognise_nans"
            call runtests(call_test_write_restart_map_file_recognise_nans)
        case ('test_write_restart_map_file_no_nans')
            write(*, *) "Running test_write_restart_map_file_no_nans"
            call runtests(call_test_write_restart_map_file_no_nans)
        case ('test_write_restart_map_file_with_nans')
            write(*, *) "Running test_write_restart_map_file_with_nans"
            call runtests(call_test_write_restart_map_file_with_nans)
        end select
    else
        write(*, *) "No test specified, running all tests"
        call runtests(call_test_write_restart_map_file_recognise_nans)
        call runtests(call_test_write_restart_map_file_no_nans)
        call runtests(call_test_write_restart_map_file_with_nans)
    end if

    call runtests_final


contains

    subroutine call_test_write_restart_map_file_recognise_nans
        call test(test_write_restart_map_file_recognise_nans, 'Recognising NaNs')
    end subroutine call_test_write_restart_map_file_recognise_nans

    subroutine call_test_write_restart_map_file_no_nans
        call test(test_write_restart_map_file_no_nans, 'write_restart_map_file: final result without NaNs')
    end subroutine call_test_write_restart_map_file_no_nans

    subroutine call_test_write_restart_map_file_with_nans
        call test(test_write_restart_map_file_with_nans, 'write_restart_map_file: final result with NaNs')
    end subroutine call_test_write_restart_map_file_with_nans

    subroutine test_write_restart_map_file_recognise_nans
        ! test_write_restart_map_file_recognise_nans --
        !     Unit test for recognising NaNs (interference from optimiser?)

        real :: x, y

        y = -1.0
        x = log10(y)

        call assert_true(x /= x, 'X recognised as NaN')

    end subroutine test_write_restart_map_file_recognise_nans

    subroutine test_write_restart_map_file_no_nans
        ! test_write_restart_map_file_no_nans --
        !     Unit test for write_restart_map_file (write restart file)
        ! Note:
        !     There should be no error message

        integer, parameter :: num_substances_total = 10
        integer, parameter :: num_cells = 23
        real, dimension(num_substances_total, num_cells) :: conc
        integer :: itime
        integer, dimension(30) :: lun
        character(len = 200) :: dataPath
        character(len = 255), dimension(30) :: file_name_list
        character(len = 40), dimension(4) :: mname
        character(len = 20), dimension(10) :: sname

        conc = 1.0

        ! Get the DATA_PATH environment variable
        call get_environment_variable("DATA_PATH", dataPath)

        file_name_list(18) = trim(dataPath) // '/test_write_restart_map_file_no_nans.ref' ! Not used in write_restart_map_file
        file_name_list(19) = trim(dataPath) // '/test_write_restart_map_file_no_nans.mon'
        file_name_list(23) = trim(dataPath) // '/test_write_restart_map_file_no_nans.res'

        open (newunit = lun(19), file = file_name_list(19))

        sname = (/' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10'/)

        call write_restart_map_file(lun, file_name_list, conc, itime, mname, sname, num_substances_total, num_cells)

        close (lun(19))

        call assert_files_comparable(file_name_list(19), file_name_list(18), 'Monitor file contains no messages', 1.0e-7)
        call assert_true(all(conc == 1.0), 'Concentration array unchanged')

    end subroutine test_write_restart_map_file_no_nans

    subroutine test_write_restart_map_file_with_nans
        ! test_write_restart_map_file_with_nans --
        !     Unit test for write_restart_map_file (write restart file)
        !
        ! Note:
        !     There should be no error message

        integer, parameter :: num_substances_total = 10
        integer, parameter :: num_cells = 23
        real, dimension(num_substances_total, num_cells) :: conc
        integer :: itime
        integer, dimension(30) :: lun
        character(len = 200) :: dataPath
        character(len = 255), dimension(30) :: file_name_list
        character(len = 40), dimension(4) :: mname
        character(len = 20), dimension(10) :: sname
        real :: x

        conc = 1.0
        x = -1.0
        conc(1, 1) = log10(x)

        ! Get the DATA_PATH environment variable
        call get_environment_variable("DATA_PATH", dataPath)

        file_name_list(18) = trim(dataPath) // '/test_write_restart_map_file_with_nans.ref' ! Not used in write_restart_map_file
        file_name_list(19) = trim(dataPath) // '/test_write_restart_map_file_with_nans.mon'
        file_name_list(23) = trim(dataPath) // '/test_write_restart_map_file_with_nans.res'

        open (newunit = lun(19), file = file_name_list(19))

        sname = (/' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10'/)

        call write_restart_map_file(lun, file_name_list, conc, itime, mname, sname, num_substances_total, num_cells)

        close (lun(19))

        call assert_files_comparable(file_name_list(19), file_name_list(18), 'Monitor file contains no messages', 1.0e-7)
        call assert_true(any(conc == 0.0), 'NaNs in concentration array replaced by 0')

    end subroutine test_write_restart_map_file_with_nans

end program test_write_restart_map_file
