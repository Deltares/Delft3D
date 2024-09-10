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

program tests_connection_data_mapping

    use m_connection_data_mapping
    use m_connection_parser
    use m_waq_api_categories
    use m_connection_data
    use m_waq_precision
    use ftnunit, only: runtests_init, &
                       runtests, &
                       runtests_final, &
                       test, &
                       assert_true, &
                       assert_false, &
                       assert_equal

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
        case ('test_set_connection_data_wasteload_with_index')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_set_connection_data_wasteload_with_index)
        case ('test_set_connection_data_wasteload_with_location_id')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_set_connection_data_wasteload_with_location_id)
        case ('test_set_connection_data_incoming_wasteload')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_set_connection_data_incoming_wasteload)
        case ('test_set_connection_data_segment_with_index')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_set_connection_data_segment_with_index)
        case ('test_set_connection_data_observation_with_index')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_set_connection_data_observation_with_index)
        case ('test_set_connection_data_for_constant')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_set_connection_data_for_constant)
        end select
    else
        write (*, *) "No test specified, running all tests"
        call runtests(call_test_set_connection_data_wasteload_with_index)
        call runtests(call_test_set_connection_data_wasteload_with_location_id)
        call runtests(call_test_set_connection_data_incoming_wasteload)
        call runtests(call_test_set_connection_data_segment_with_index)
        call runtests(call_test_set_connection_data_observation_with_index)
        call runtests(call_test_set_connection_data_for_constant)
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

    subroutine call_test_set_connection_data_wasteload_with_index
        call test(test_set_connection_data_wasteload_with_index, 'Test setting data for a a waste load connection')
    end subroutine

    subroutine call_test_set_connection_data_wasteload_with_location_id
        call test(test_set_connection_data_wasteload_with_location_id, 'Test setting data for a waste load connection')
    end subroutine

    subroutine call_test_set_connection_data_incoming_wasteload
        call test(test_set_connection_data_incoming_wasteload, 'Test setting data for a incoming waste load connection')
    end subroutine

    subroutine call_test_set_connection_data_segment_with_index
        call test(test_set_connection_data_segment_with_index, 'Test setting data for a segment connection')
    end subroutine

    subroutine call_test_set_connection_data_observation_with_index
        call test(test_set_connection_data_observation_with_index, 'Test setting data for a observation connection')
    end subroutine

    subroutine call_test_set_connection_data_for_constant
        call test(test_set_connection_data_for_constant, 'Test setting data for a constant connection')
    end subroutine

    subroutine test_set_connection_data_wasteload_with_index()
        use delwaq2_global_data, only: load_name
        use m_real_array_indices, only: iwste
        use m_waq_memory_dimensions, only: num_substances_total

        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "FROM_DELWAQ|WASTE|3|FLOW"

        ! arrange
        iwste = 10
        num_substances_total = 2

        ! act
        new_connection = parse_connection_string(exchange_name)
        call set_connection_data(new_connection)

        ! assert
        ! buffer_idx should be the offset for wasteload data (iwast = 10 -1) +
        ! index of the substance (flow = 1) + the provided location index (3 -1) *
        ! number of substances (2 + 1 for flow)
        ! 10 -1 + 1 + (3-1) * 3 = 12
        call assert_equal(new_connection%data_index, 3, 'connection data_index should be correct')
        call assert_equal(new_connection%buffer_idx, 16, 'connection buffer_idx should be correct')

    end subroutine

    subroutine test_set_connection_data_wasteload_with_location_id()
        use delwaq2_global_data, only: load_name
        use m_real_array_indices, only: iwste
        use m_waq_memory_dimensions, only: num_substances_total

        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "FROM_DELWAQ|WASTE|wasteload_2|FLOW"

        ! arrange
        load_name = ["wasteload_1", "wasteload_2"]
        iwste = 10
        num_substances_total = 2

        ! act
        new_connection = parse_connection_string(exchange_name)
        call set_connection_data(new_connection)

        ! assert
        ! buffer_idx should be the offset for wasteload data (iwast = 10 -1) +
        ! index of the substance (flow = 1) + the provided location index (2 (index in load_name array) -1) *
        ! number of substances (2 + 1 for flow)
        ! 10 -1 + 1 + (2-1) * 3 = 12
        call assert_equal(new_connection%data_index, 2, 'connection data_index should be correct')
        call assert_equal(new_connection%buffer_idx, 13, 'connection buffer_idx should be correct')

    end subroutine

    subroutine test_set_connection_data_incoming_wasteload()
        use delwaq2_global_data, only: load_name
        use m_real_array_indices, only: iwste
        use m_waq_memory_dimensions, only: num_substances_total

        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "TO_DELWAQ|WASTE|wasteload_2|FLOW"

        ! arrange
        load_name = ["wasteload_1", "wasteload_2"]
        iwste = 10
        num_substances_total = 2

        ! act
        new_connection = parse_connection_string(exchange_name)
        call set_connection_data(new_connection)

        ! assert
        ! buffer_idx should be the same as data_index when using an incoming connection (TO_DELWAQ)
        call assert_equal(new_connection%data_index, 2, 'connection data_index should be correct')
        call assert_equal(new_connection%buffer_idx, new_connection%data_index, 'connection buffer_idx should be correct')

    end subroutine

    subroutine test_set_connection_data_segment_with_index()
        use m_real_array_indices, only: iconc
        use m_waq_memory_dimensions, only: num_substances_total
        use delwaq2_global_data, only: substance_name

        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "FROM_DELWAQ|SEGMN|3|OXY"

        ! arrange
        iconc = 20
        num_substances_total = 3
        substance_name = ["substance_1", "substance_2", "OXY        "]

        ! act
        new_connection = parse_connection_string(exchange_name)
        call set_connection_data(new_connection)

        ! assert
        ! buffer_idx should be the offset for segment data (iconc = 20 -1) +
        ! index of the substance (OXY = 3) + the provided location index (3 -1) *
        ! number of substances (3)
        ! 20 -1 + 3 + (3-1) * 3 = 28
        call assert_equal(new_connection%data_index, 3, 'connection data_index should be correct')
        call assert_equal(new_connection%buffer_idx, 28, 'connection buffer_idx should be correct')

    end subroutine

    subroutine test_set_connection_data_observation_with_index()
        use m_real_array_indices, only: iconc
        use m_waq_memory_dimensions, only: num_substances_total
        use delwaq2_global_data, only: substance_name, monitor_cell

        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "FROM_DELWAQ|OBSRV|3|OXY"

        ! arrange
        iconc = 20
        num_substances_total = 3
        substance_name = ["substance_1", "substance_2", "OXY        "]
        monitor_cell = [8, 9, 10, 11, 12] !< mapping of cell index to location id

        ! act
        new_connection = parse_connection_string(exchange_name)
        call set_connection_data(new_connection)

        ! assert
        ! buffer_idx should be the offset for observation data (iconc = 20 -1) +
        ! index of the substance (OXY = 3) + the provided location index (10 (index 3 of the monitor_cell) -1) *
        ! number of substances (3)
        ! 20 -1 + 3 + (10-1) * 3 = 49
        call assert_equal(new_connection%data_index, 10, 'connection data_index should be correct')
        call assert_equal(new_connection%buffer_idx, 49, 'connection buffer_idx should be correct')

    end subroutine

    subroutine test_set_connection_data_for_constant()
        use m_real_array_indices, only: icons
        use m_waq_memory_dimensions, only: num_substances_total
        use delwaq2_global_data, only: procparam_const

        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "FROM_DELWAQ|CONST|def"

        ! arrange
        icons = 5
        procparam_const = ["abc", "def", "ghi", "jkl"]

        ! act
        new_connection = parse_connection_string(exchange_name)
        call set_connection_data(new_connection)

        ! assert
        ! buffer_idx should be the offset for constant data (icons = 5 - 1) +
        ! the provided location index (2)
        ! 5 - 1 + 2 = 6
        call assert_equal(new_connection%data_index, 2, 'connection data_index should be correct')
        call assert_equal(new_connection%buffer_idx, 6, 'connection buffer_idx should be correct')

    end subroutine
end program

