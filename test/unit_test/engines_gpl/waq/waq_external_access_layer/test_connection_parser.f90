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

program tests_connection_parser

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
        case ('test_parse_connection_string_wasteload_flow_incoming')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_parse_connection_string_wasteload_flow_incoming)
        case ('test_parse_connection_string_incoming_const_VWind')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_parse_connection_string_incoming_const_VWind)
        case ('test_parse_connection_string_outgoing_cell_oxy_with_index')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_parse_connection_string_outgoing_cell_oxy_with_index)
        case ('test_parse_connection_string_outgoing_cell_oxy_with_id')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_parse_connection_string_outgoing_cell_oxy_with_id)
        end select
    else
        write (*, *) "No test specified, running all tests"
        call runtests(call_test_parse_connection_string_wasteload_flow_incoming)
        call runtests(call_test_parse_connection_string_incoming_const_VWind)
        call runtests(call_test_parse_connection_string_outgoing_cell_oxy_with_index)
        call runtests(call_test_parse_connection_string_outgoing_cell_oxy_with_id)
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

    subroutine call_test_parse_connection_string_wasteload_flow_incoming
        call test(test_parse_connection_string_wasteload_flow_incoming, 'Test parsing an connection string')
    end subroutine

    subroutine call_test_parse_connection_string_incoming_const_VWind
        call test(test_parse_connection_string_incoming_const_VWind, 'Test parsing an connection string')
    end subroutine

    subroutine call_test_parse_connection_string_outgoing_cell_oxy_with_index
        call test(test_parse_connection_string_outgoing_cell_oxy_with_index, 'Test parsing an connection string')
    end subroutine

    subroutine call_test_parse_connection_string_outgoing_cell_oxy_with_id
        call test(test_parse_connection_string_outgoing_cell_oxy_with_id, 'Test parsing an connection string')
    end subroutine

    subroutine test_parse_connection_string_wasteload_flow_incoming()
        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "TO_DELWAQ|WASTE|1|FLOW"

        ! act
        new_connection = parse_connection_string(exchange_name)

        ! assert
        call assert_true(allocated(new_connection), 'new connection should be made')
        call assert_equal(new_connection%category, category_wasteload, 'connection category should be correct')
        call assert_true(new_connection%incoming, 'connection should be incoming')
        call assert_equal(new_connection%location_text, "1", 'connection location_text should be correct')
        call assert_equal(new_connection%location_index, 1, 'connection location_index should be correct')
        call assert_equal(new_connection%quantity_name, "FLOW", 'connection quantity_name should be correct')

    end subroutine

    subroutine test_parse_connection_string_incoming_const_VWind()
        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "TO_DELWAQ|CONST|VWind"

        ! act
        new_connection = parse_connection_string(exchange_name)

        ! assert
        call assert_true(allocated(new_connection), 'new connection should be made')
        call assert_equal(new_connection%category, category_procparam, 'connection category should be correct')
        call assert_true(new_connection%incoming, 'connection should be incoming')
        call assert_false(allocated(new_connection%location_text), 'connection location_text should be correct')
        call assert_equal(new_connection%location_index, 0, 'connection location_index should be correct')
        call assert_equal(new_connection%quantity_name, "VWind", 'connection quantity_name should be correct')

    end subroutine

    subroutine test_parse_connection_string_outgoing_cell_oxy_with_index()
        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "FROM_DELWAQ|SEGMN|9|OXY"

        ! act
        new_connection = parse_connection_string(exchange_name)

        ! assert
        call assert_true(allocated(new_connection), 'new connection should be made')
        call assert_equal(new_connection%category, category_segment, 'connection category should be correct')
        call assert_false(new_connection%incoming, 'connection should be incoming')
        call assert_equal(new_connection%location_text, "9", 'connection location_text should be correct')
        call assert_equal(new_connection%location_index, 9, 'connection location_index should be correct')
        call assert_equal(new_connection%quantity_name, "OXY", 'connection quantity_name should be correct')

    end subroutine

    subroutine test_parse_connection_string_outgoing_cell_oxy_with_id()
        type(connection_data), allocatable :: new_connection
        character(*), parameter :: exchange_name = "FROM_DELWAQ|SEGMN|id1|OXY"

        ! act
        new_connection = parse_connection_string(exchange_name)

        ! assert
        call assert_true(allocated(new_connection), 'new connection should be made')
        call assert_equal(new_connection%category, category_segment, 'connection category should be correct')
        call assert_false(new_connection%incoming, 'connection should be incoming')
        call assert_equal(new_connection%location_text, "id1", 'connection location_text should be correct')
        call assert_equal(new_connection%location_index, -1, 'connection location_index should be correct')
        call assert_equal(new_connection%quantity_name, "OXY", 'connection quantity_name should be correct')

    end subroutine
end program
