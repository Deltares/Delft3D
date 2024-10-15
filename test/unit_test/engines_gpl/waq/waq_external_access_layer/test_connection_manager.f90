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

program tests_connection_manager

    use m_connection_manager
    use m_waq_api_categories
    use m_connection_data
    use m_waq_precision
    use ftnunit, only: runtests_init, &
                       runtests, &
                       runtests_final, &
                       assert_comparable, &
                       test, &
                       assert_equal, &
                       assert_true, &
                       assert_false

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
        case ('test_add_connection')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_add_connection)
        case ('test_get_connection_by_exchange_name')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_get_connection_by_exchange_name)
        case ('test_get_incoming_connections_by_category')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_get_incoming_connections_by_category)
        case ('test_add_multiple_connections')
            write (*, *) "Running "//cmd_arg
            call runtests(call_test_add_multiple_connections)
        end select
    else
        write (*, *) "No test specified, running all tests"
        !call runtests(call_test_add_connection)
        !call runtests(call_test_get_connection_by_exchange_name)
        call runtests(call_test_get_incoming_connections_by_category)
        !call runtests(call_test_add_multiple_connections)
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

    subroutine call_test_add_connection
        call test(test_add_connection, 'Test adding a connection with the connection manager')
    end subroutine

    subroutine call_test_get_connection_by_exchange_name
        call test(test_get_connection_by_exchange_name, 'Test retrieving a connection with the connection manager')
    end subroutine

    subroutine call_test_get_incoming_connections_by_category
        call test(test_get_incoming_connections_by_category, 'Test retrieving an incoming connection with the connection manager')
    end subroutine

    subroutine call_test_add_multiple_connections
        call test(test_add_multiple_connections, 'Test adding multiple connections with the connection manager')
    end subroutine

    subroutine test_add_connection()
        type(connection_manager) :: connections
        type(connection_data), allocatable :: new_connection
        type(connection_data), pointer :: found_connection
        type(connection_data), pointer :: new_connection_pointer
        real(kind=dp), pointer :: dummy_pointer
        character(*), parameter :: exchange_name = "TO_DELWAQ|WASTE|1|FLOW"

        found_connection => connections%get_connection_by_exchange_name(exchange_name)
        call assert_false(associated(found_connection), 'no connections after initialization')

        ! create and add new connection
        new_connection = create_default_connection(exchange_name)
        new_connection_pointer => connections%add_connection(new_connection)

        found_connection => connections%get_connection_by_exchange_name(exchange_name)
        call assert_true(associated(found_connection), 'new connection should be found')

    end subroutine

    subroutine test_add_multiple_connections()
        type(connection_manager) :: connections
        type(connection_data), allocatable :: new_connection1
        type(connection_data), allocatable :: new_connection2
        type(connection_data), allocatable :: new_connection3

        type(connection_data), pointer :: new_connection1_pointer
        type(connection_data), pointer :: new_connection2_pointer
        type(connection_data), pointer :: new_connection3_pointer

        type(connection_data), pointer :: found_connection1
        type(connection_data), pointer :: found_connection2
        type(connection_data), pointer :: found_connection3
        character(*), parameter :: exchange_name1 = "TO_DELWAQ|WASTE|1|FLOW"
        character(*), parameter :: exchange_name2 = "FROM_DELWAQ|SEGMN|9|OXY"
        character(*), parameter :: exchange_name3 = "TO_DELWAQ|CONST|VWind"

        ! create and add new connections
        new_connection1 = create_default_connection(exchange_name1)
        new_connection2 = create_default_connection(exchange_name2)
        new_connection3 = create_default_connection(exchange_name3)

        new_connection1_pointer => connections%add_connection(new_connection1)
        new_connection2_pointer => connections%add_connection(new_connection2)
        new_connection3_pointer => connections%add_connection(new_connection3)

        ! find the added connections
        found_connection1 => connections%get_connection_by_exchange_name(exchange_name1)
        found_connection2 => connections%get_connection_by_exchange_name(exchange_name2)
        found_connection3 => connections%get_connection_by_exchange_name(exchange_name3)

        ! assert
        call assert_true(associated(found_connection1), 'connection 1 should be found')
        call assert_equal(found_connection1%exchange_name, exchange_name1, 'connection 1 should have the correct exchange name')

        call assert_true(associated(found_connection2), 'connection 2 should be found')
        call assert_equal(found_connection2%exchange_name, exchange_name2, 'connection 2 should have the correct exchange name')

        call assert_true(associated(found_connection3), 'connection 3 should be found')
        call assert_equal(found_connection3%exchange_name, exchange_name3, 'connection 3 should have the correct exchange name')
    end subroutine

    subroutine test_get_connection_by_exchange_name()
        type(connection_manager) :: connections
        type(connection_data), allocatable :: new_connection
        type(connection_data), pointer :: new_connection_pointer
        type(connection_data), pointer :: found_connection_correct_name
        type(connection_data), pointer :: found_connection_incorrect_name
        character(*), parameter :: exchange_name = "TO_DELWAQ|WASTE|1|FLOW"

        ! arrange
        new_connection = create_default_connection(exchange_name)
        new_connection_pointer => connections%add_connection(new_connection)

        ! act
        found_connection_correct_name => connections%get_connection_by_exchange_name(exchange_name)
        found_connection_incorrect_name => connections%get_connection_by_exchange_name("incorrect_name")

        ! assert
        call assert_true(associated(found_connection_correct_name), 'connection should be found')
        call assert_false(associated(found_connection_incorrect_name), 'connection should not be found')

        ! found_connection_correct_name should be a reference to the connection (not a copy)
        found_connection_correct_name%subst_name = "abc"
        call assert_equal(new_connection_pointer%subst_name, "abc", 'connection should have be a reference to the connection')
    end subroutine

    subroutine test_get_incoming_connections_by_category()
        type(connection_manager) :: connections
        type(connection_data), allocatable :: new_connection1
        type(connection_data), allocatable :: new_connection2
        type(connection_data), allocatable :: new_connection3

        type(connection_data), pointer :: new_connection1_pointer
        type(connection_data), pointer :: new_connection2_pointer
        type(connection_data), pointer :: new_connection3_pointer

        type(connection_wrapper), allocatable, dimension(:) :: found_connections
        real(kind=dp), pointer :: dummy_pointer
        character(*), parameter :: exchange_name = "TO_DELWAQ|WASTE|1|FLOW"

        ! arrange
        new_connection1 = create_default_connection("TO_DELWAQ|WASTE|1|FLOW")
        new_connection1_pointer => connections%add_connection(new_connection1)

        new_connection2 = create_default_connection("FROM_DELWAQ|SEGMN|9|OXY")
        new_connection2%incoming = .false.
        new_connection2%category = category_segment
        new_connection2_pointer => connections%add_connection(new_connection2)

        new_connection3 = create_default_connection("TO_DELWAQ|CONST|VWind")
        new_connection3%category = category_procparam
        new_connection3_pointer => connections%add_connection(new_connection3)

        ! act
        found_connections = connections%get_incoming_connections_by_category(category_procparam)

        ! assert
        call assert_true(size(found_connections) == 1, 'a connection should be found')
        call assert_equal(found_connections(1)%connection_ptr%exchange_name, new_connection3%exchange_name, 'connection should be the correct connection')

        ! found_connections should contain a reference to the connection (not a copy)
        found_connections(1)%connection_ptr%subst_name = "abc"
        call assert_equal(new_connection3_pointer%subst_name, "abc", 'connection should have be a reference to the connection')
    end subroutine

    function create_default_connection(exchange_name) result(new_connection)
        character(*) :: exchange_name
        type(connection_data), allocatable :: new_connection

        real(kind=dp), pointer :: dummy_pointer

        new_connection = connection_data( &
                         exchange_name=exchange_name, &
                         incoming=.true., &
                         has_location_filter=.true., &
                         category=category_wasteload, &
                         data_index=0, &
                         substance_index=0, &
                         location_index=0, &
                         buffer_idx=0, &
                         p_value=dummy_pointer &
                         )

    end function

end program
