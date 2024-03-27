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
module boundary_condition_utils

    use timers, only : timon, timstrt, timstop
    use m_waq_precision
    use m_array_manipulation, only : shift_array_right
    use m_string_manipulation, only : shift_char_subarray
    use m_error_status
    implicit none

    private
    public :: parse_boundary_condition_data, read_boundary_conditions_from_ods_file, read_time_series_table

contains

    subroutine parse_boundary_condition_data(lunut, input_file_start_position, num_significant_char, &
            comment_charachter, char_arr, int_array, max_char_size, max_int_size, all_names, all_types, &
            num_bc_waste, num_bc_waste_types, parsed_items_count, noits, chkflg, caller, ilun, file_name_list, &
            lstack, itype, real_array, nconst, itmnr, parsed_str, output_verbose_level, error_ind, status)

        use m_string_utils, only : index_in_array, join_strings, string_equals

        integer(kind = int_wp), intent(in) :: max_char_size        !< Max. Char workspace dimension
        integer(kind = int_wp), intent(in) :: max_int_size        !< Max. Int. Workspace dimension
        integer(kind = int_wp), intent(in) :: chkflg       !< Check on input or add items
        integer(kind = int_wp), intent(in) :: lunut        !< Unit Formatted Output File
        integer(kind = int_wp), intent(inout) :: input_file_start_position        !< Start position on input line
        integer(kind = int_wp), intent(in) :: num_significant_char         !< Nr of significant characters
        integer(kind = int_wp), intent(out) :: int_array(:)       !< Integer workspace
        integer(kind = int_wp), intent(inout) :: num_bc_waste  !< Number of bounds/wastes
        integer(kind = int_wp), intent(in) :: num_bc_waste_types       !< Number of bound/waste types
        integer(kind = int_wp), intent(out) :: parsed_items_count        !< Number of items read
        integer(kind = int_wp), intent(out) :: noits        !< Number of items for scale
        integer(kind = int_wp), intent(inout) :: ilun(lstack) !< Unitnumb include stack
        integer(kind = int_wp), intent(in) :: lstack       !< Include file stack size
        integer(kind = int_wp), intent(out) :: itype        !< Type of the token read ('at exit')
        integer(kind = int_wp), intent(out) :: nconst       !< Number of values in real_array
        integer(kind = int_wp), intent(in) :: output_verbose_level       !< Output file option
        integer(kind = int_wp), intent(out) :: error_ind    !< Error indicator

        real(kind = real_wp), intent(out) :: real_array(:)       !< Array with real values

        character(1), intent(in) :: comment_charachter        !< Comment character
        character(*), intent(out) :: char_arr(:)       !< Character workspace
        character(*), intent(inout) :: all_names(:)     !< Id's of the boundaries/wastes
        character(*), intent(in) :: all_types(:)     !< Types of the boundaries/wastes
        character(10), intent(in) :: caller       !< Calling subject
        character(*), intent(inout) :: file_name_list(lstack)  !< File name stack, 4 deep
        character(*), intent(out) :: parsed_str   !< Input string at end of routine

        type(error_status), intent(inout) :: status !< current error status

        !   Local variables
        logical :: usefor_on, substitution_on, can_compute, operator_on
        logical :: logging_on
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: i, parsed_int, ifound, i2, name_index, icm
        integer(kind = int_wp) :: itmnr, ioff, ioffc, ioffi
        real(kind = real_wp) :: parsed_real
        character(3), parameter :: operations(6) = ['*  ', '/  ', '+  ', '-  ', 'MIN', 'MAX']
        character(14), parameter :: keywords(25) = &
                ['BLOCK         ', &
                        'BINARY_FILE   ', &
                        'CONCENTRATIONS', &
                        'CONCENTRATION ', &
                        'CONSTANTS     ', &
                        'DATA          ', &
                        'DATA_ITEM     ', &
                        'LINEAR        ', &
                        'ITEM          ', &
                        'IDENTICALITEM ', &
                        'USEDATA_ITEM  ', &
                        'FORITEM       ', &
                        'TIME_DELAY    ', &
                        'ODS_FILE      ', &
                        'ABSOLUTE      ', &
                        'TIME          ', &
                        'HARMONICS     ', &
                        'FOURIERS      ', &
                        'SCALE         ', &
                        'DEFAULTS      ', &
                        'ALL           ', &
                        'SEGMENTS      ', &
                        'PARAMETERS    ', &
                        'FUNCTIONS     ', &
                        'SEG_FUNCTIONS '  ]

        if (timon) call timstrt("parse_boundary_condition_data", ithndl)

        ! some initialisations
        usefor_on = .false.
        substitution_on = .false.
        can_compute = .false.
        operator_on = .false.
        logging_on = (output_verbose_level >= 3)
        parsed_items_count = 0
        noits = 0
        itmnr = 0
        ioff = 0
        ioffc = 0
        ioffi = 0
        nconst = 0
        ! Get a token string (and return if any error was found)
        read_and_process : do
            itype = -3
            if (operator_on .or. (usefor_on .and. substitution_on)) itype = 0
            call rdtok1(lunut, ilun, file_name_list, lstack, comment_charachter, &
                    input_file_start_position, num_significant_char, parsed_str, parsed_int, parsed_real, &
                    itype, error_ind)
            if (error_ind /= 0) then
                call finish(ithndl)
                return
            end if

            if (itype == 1) then
                ! Scenario: type==1 and a keyword was met
                if (any(keywords == trim(parsed_str))) then
                    if (usefor_on) then
                        write (lunut, 1035) parsed_str
                        call error_and_finish(error_ind, ithndl)
                        return
                    else
                        call finish(ithndl)
                        return
                    end if
                end if

                ! Scenario: type==1 and computation was met
                if (any(operations == trim(parsed_str))) then
                    if (.not. can_compute) then
                        write (lunut, 1070)
                        call error_and_finish(error_ind, ithndl)
                        return
                    end if
                    if (operator_on) then
                        write (lunut, '(A)') ' ERROR: arithmetics should be separated by items !'
                        call error_and_finish(error_ind, ithndl)
                        return
                    end if
                    parsed_items_count = parsed_items_count + 1
                    noits = noits + 1
                    call shift_array_right(int_array, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    int_array(itmnr + parsed_items_count + parsed_items_count) = 0
                    select case(parsed_str)
                    case ('*')
                        int_array(itmnr + parsed_items_count) = -1000000
                    case ('/')
                        int_array(itmnr + parsed_items_count) = -10000000
                    case ('+')
                        int_array(itmnr + parsed_items_count) = -100000000
                    case ('-')
                        int_array(itmnr + parsed_items_count) = -1000000000
                    case ('MIN')
                        int_array(itmnr + parsed_items_count) = -1100000000
                    case ('MAX')
                        int_array(itmnr + parsed_items_count) = -1200000000
                    end select
                    operator_on = .true.
                    cycle read_and_process
                end if

                ! Scenario: an item used in computations
                if (operator_on) then
                    do i = 1, itmnr - 1
                        if (int_array(i) == -1300000000) cycle
                        ifound = index_in_array(parsed_str, char_arr(i + ioff:i + ioff))
                        if (ifound == 1) then
                            noits = noits - 1
                            i2 = int_array(itmnr + parsed_items_count)
                            call log_item_number_name(i2, lunut, i, parsed_str)
                            int_array(itmnr + parsed_items_count) = i2 + i
                            char_arr(itmnr + parsed_items_count + ioff) = '&$&$SYSTEM_NAME&$&$!'
                            operator_on = .false.
                            cycle read_and_process
                        end if
                    end do
                    i2 = int_array(itmnr + parsed_items_count)
                    call log_local_substitution(i2, lunut, parsed_str)
                    int_array (itmnr + parsed_items_count + parsed_items_count) = noits
                    char_arr (itmnr + parsed_items_count + ioff) = parsed_str
                    operator_on = .false.
                    cycle read_and_process
                end if

                ! Scenario: a local redirection of the name of an item or substance
                if (parsed_str == 'USEFOR') then
                    if (usefor_on) then
                        write (lunut, 1035) parsed_str
                        call error_and_finish(error_ind, ithndl)
                        return
                    else
                        usefor_on = .true.
                        substitution_on = .false.
                        cycle read_and_process
                    end if
                end if

                ! Scenario:
                if (usefor_on .and. substitution_on) then
                    name_index = int_array(itmnr)
                    if (logging_on) then
                        call log_name_substitution(name_index, lunut, all_names, caller, itmnr, all_types, parsed_real, parsed_str, .false.)
                    end if
                    int_array(itmnr + parsed_items_count + parsed_items_count) = noits
                    char_arr(itmnr + parsed_items_count + ioff) = parsed_str
                    usefor_on = .false.
                    substitution_on = .false.
                    ! it is now possible to compute
                    can_compute = .true.
                    cycle read_and_process
                end if

                ! fill in a string value if an empty string is provided
                if (chkflg == -1 .and. parsed_str(1:20) == repeat(' ', 20)) then
                    parsed_str = 'Item-'
                    write (parsed_str(6:12), '(I7)') parsed_items_count + 1
                end if
                ! FLOW is only valid as CONCENTR. and item number is 0
                if (string_equals(trim(parsed_str), 'FLOW') .and. caller == 'CONCENTR. ') then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(int_array, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(int_array, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(char_arr, itmnr + ioff, icm)
                    int_array (itmnr) = 0
                    int_array (itmnr + parsed_items_count) = itmnr
                    int_array (itmnr + parsed_items_count + parsed_items_count) = noits
                    char_arr (itmnr + ioff) = parsed_str
                    char_arr (itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1020) caller, itmnr, caller, 0, 'FLOW'
                    end if
                    cycle read_and_process
                end if

                ! parsed_str == item-NAME
                ifound = index_in_array(parsed_str(1:len(all_names(1))), all_names(1:num_bc_waste))
                if (ifound >= 1) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(int_array, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(int_array, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(char_arr, itmnr + ioff, icm)
                    int_array(itmnr) = ifound
                    int_array(itmnr + parsed_items_count) = itmnr
                    int_array(itmnr + parsed_items_count + parsed_items_count) = noits
                    char_arr(itmnr + ioff) = parsed_str
                    char_arr(itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1020) caller, itmnr, caller, ifound, all_names(ifound)
                    end if
                    cycle read_and_process
                end if

                ! parsed_str == item-TYPE. int_array now is negative.
                ifound = index_in_array(parsed_str(1:len(all_types(1))), all_types(1:num_bc_waste_types))
                if (ifound >= 1) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(int_array, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(int_array, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(char_arr, itmnr + ioff, icm)
                    int_array(itmnr) = -ifound
                    int_array(itmnr + parsed_items_count) = itmnr
                    int_array(itmnr + parsed_items_count + parsed_items_count) = noits
                    char_arr(itmnr + ioff) = parsed_str
                    char_arr(itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1030) caller, itmnr, caller, ifound, all_types(ifound)
                    end if
                    cycle read_and_process
                end if

                ! If only existing names or types are allowed then
                !        this is the place for an error massage
                ! JVB stick to just a warning keep on reading int_array = 0?, or used for flow??

                if (chkflg == 1) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(int_array, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(int_array, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(char_arr(:icm + 1), itmnr + ioff, icm)
                    int_array (itmnr) = -1300000000 ! pointer should be ignored
                    int_array (itmnr + parsed_items_count) = 1300000000 ! item number should be ignored
                    int_array (itmnr + parsed_items_count + parsed_items_count) = noits
                    char_arr (itmnr + ioff) = parsed_str
                    char_arr (itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    write(lunut, 1040) caller, itmnr, parsed_str

                    call status%increase_warning_count()
                else
                    ! Now a new name is added to the list of names
                    !        the rest is moved upward since it is all 1 array
                    num_bc_waste = num_bc_waste + 1
                    ioff = ioff + 1
                    icm = max_char_size + num_bc_waste
                    call shift_char_subarray(all_names, num_bc_waste, icm)
                    all_names(num_bc_waste) = parsed_str
                    ! plus normal procedure
                    parsed_items_count = parsed_items_count + 1
                    noits = noits + 1
                    itmnr = itmnr + 1
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(int_array, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(int_array, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(char_arr, itmnr + ioff, icm)
                    int_array(itmnr) = num_bc_waste
                    int_array(itmnr + parsed_items_count) = itmnr
                    int_array(itmnr + parsed_items_count + parsed_items_count) = noits
                    char_arr(itmnr + ioff) = parsed_str
                    char_arr(itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1020) caller, itmnr, caller, num_bc_waste, all_names(num_bc_waste)
                    end if
                end if
                cycle read_and_process
            end if

            ! Scenario: a number (int, 2, or real, 3) is used in computations
            if (itype == 2 .or. itype == 3) then
                if (substitution_on .or. operator_on) then
                    nconst = nconst + 1
                    real_array(nconst) = parsed_real
                    noits = noits - 1
                    i2 = int_array(itmnr + parsed_items_count)
                    char_arr(itmnr + parsed_items_count + ioff) = '&$&$SYSTEM_NAME&$&$!'
                    if (operator_on) then
                        if (logging_on) then
                            call log_number_in_operation(i2, lunut, parsed_real)
                        end if
                        int_array(itmnr + parsed_items_count) = i2 - nconst
                        operator_on = .false.
                    end if
                    if (substitution_on) then
                        name_index = int_array(itmnr)
                        if (logging_on) then
                            call log_name_substitution(name_index, lunut, all_names, caller, itmnr, all_types, parsed_real, parsed_str, .true.)
                        end if
                        int_array(itmnr + parsed_items_count) = -nconst
                        int_array(itmnr + parsed_items_count + parsed_items_count) = 0
                        usefor_on = .false.
                        substitution_on = .false.
                        can_compute = .true.
                    end if
                    cycle read_and_process
                end if
            end if

            ! Scenario: no item name was given, but an item number
            if (itype == 2) then
                if (parsed_int <=  num_bc_waste .and. parsed_int >= -num_bc_waste_types) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff

                    call shift_array_right(int_array, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(int_array, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(char_arr, itmnr + ioff, icm)

                    int_array (itmnr) = parsed_int
                    int_array (itmnr + parsed_items_count) = itmnr
                    int_array (itmnr + parsed_items_count + parsed_items_count) = noits
                    if (caller == 'segment') then
                        if (parsed_int <= 0) then
                            write (lunut, 1060) parsed_int
                            call error_and_finish(error_ind, ithndl)
                            return
                        end if
                        if (logging_on .and. .not. usefor_on) then
                            write (lunut, 1015) caller, itmnr, caller, parsed_int
                        end if
                        write (parsed_str, '(''Segment '',I8)') parsed_int
                    else if (parsed_int == 0 .and. caller /= 'CONCENTR. ') then
                        write (lunut, 1060) parsed_int
                        call error_and_finish(error_ind, ithndl)
                        return
                    else if (parsed_int > 0) then
                        if (logging_on .and. .not. usefor_on) then
                            write (lunut, 1020) caller, itmnr, caller, parsed_int, &
                                    all_names(parsed_int)
                        end if
                        parsed_str = all_names(parsed_int)
                    else if (parsed_int == 0 .and. caller == 'CONCENTR. ') then
                        if (logging_on .and. .not. usefor_on) then
                            write (lunut, 1020) caller, itmnr, caller, parsed_int, &
                                    'FLOW'
                        end if
                        parsed_str = 'FLOW'
                    else
                        if (logging_on .and. .not. usefor_on) then
                            write (lunut, 1030) caller, itmnr, caller, -parsed_int, &
                                    all_types(-parsed_int)
                        end if
                        parsed_str = all_types(-parsed_int)
                    end if
                    char_arr (itmnr + ioff) = parsed_str
                    char_arr (itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    cycle read_and_process
                else
                    write (lunut, 1060) parsed_int
                    call error_and_finish(error_ind, ithndl)
                    return
                end if
            end if

        end do read_and_process

        1015 format(' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5)
        1020 format(' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with ID  : ', A20)
        1030 format(' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with type: ', A20)
        1035 format(' ERROR: no reserved keyword expected: ', A20)
        1040 format(' WARNING: Input ', A, ' nr:', I5, ' with name: ', A20, ' is not a valid ID, data ignored')
        1060 format(' ERROR: number: ', I5, ' is not a valid item number !')
        1070 format(' ERROR: multiplication is only allowed in USEFOR', ' context !')

    end subroutine parse_boundary_condition_data

    subroutine update_counters(counter_a, counter_b, counter_c)
        integer(kind = int_wp), intent(inout) :: counter_a, counter_b, counter_c

        integer i

        counter_a = counter_a + 1
        counter_b = counter_b + 1
        counter_c = counter_c + 1
    end subroutine update_counters

    subroutine log_local_substitution(index, log_unit, string)
        integer(kind = int_wp), intent(in) :: index
        integer(kind = int_wp), intent(in) :: log_unit
        character(20), intent(in) :: string

        character(20) :: message_start

        select case(index)
        case (-1000000)
            message_start = ' Multiplied by'
        case (-10000000)
            message_start = ' Divided by'
        case (-100000000)
            message_start = ' Added with'
        case (-1000000000)
            message_start = ' Subtracted by'
        case (-1100000000)
            message_start = ' Minimum value is'
        case (-1200000000)
            message_start = ' Maximum value is'
        end select
        write(log_unit, '(2A,A20)') trim(message_start), ' local substitution: ', string
    end subroutine log_local_substitution

    subroutine log_item_number_name(index, log_unit, item_number, string)
        integer(kind = int_wp), intent(in) :: index
        integer(kind = int_wp), intent(in) :: log_unit
        integer(kind = int_wp), intent(in) :: item_number
        character(20), intent(in) :: string

        character(20) :: message_start

        select case(index)
        case (-1000000)
            message_start = ' Multiplied by'
        case (-10000000)
            message_start = ' Divided by'
        case (-100000000)
            message_start = ' Added with'
        case (-1000000000)
            message_start = ' Subtracted by'
        case (-1100000000)
            message_start = ' Minimum value is'
        case (-1200000000)
            message_start = ' Maximum value is'
        end select
        write(log_unit, '(2A,I6,A7, A20)') trim(message_start), ' item nr: ', item_number, ' Name: ', string
    end subroutine log_item_number_name

    subroutine log_number_in_operation(index, log_unit, number)
        integer(kind = int_wp), intent(in) :: index
        integer(kind = int_wp), intent(in) :: log_unit
        real(kind = real_wp), intent(in) :: number

        character(20) :: message_start

        select case(index)
        case (-1000000)
            message_start = ' Multiplied by: '
        case (-10000000)
            message_start = ' Divided by: '
        case (-100000000)
            message_start = ' Added with: '
        case (-1000000000)
            message_start = ' Subtracted by: '
        case (-1100000000)
            message_start = ' Minimum value is: '
        case (-1200000000)
            message_start = ' Maximum value is: '
        end select
        write(log_unit, '(A,E15.6,A20)') trim(message_start), number
    end subroutine log_number_in_operation

    subroutine log_name_substitution(index, log_unit, all_names, caller, itmnr, all_types, target_real, target_char, real_substitution)
        integer(kind = int_wp), intent(in) :: index
        integer(kind = int_wp), intent(in) :: log_unit
        integer(kind = int_wp), intent(in) :: itmnr
        real(kind = real_wp), intent(in) :: target_real
        character(*), intent(in) :: caller
        character(*), intent(in) :: target_char
        logical, intent(in) :: real_substitution
        character(*), intent(inout) :: all_names(*)     !< Names (id's) of the boundaries/wastes
        character(*), intent(in) :: all_types(*)     !< Types of the boundaries/wastes

        character(20) :: name
        character(28) :: substitution
        character(4) :: id_or_type
        integer :: name_index

        id_or_type = 'ID  '
        name_index = index
        select case(index)
        case(1:)
            name = all_names(name_index)
        case(0)
            name = 'FLOW'
        case(-1300000000)
            name = 'Ignored'
        case default
            id_or_type = 'type'
            name_index = -index
            name = all_types(name_index)
        end select
        if (real_substitution) then
            write(substitution, '(A20)') target_real
        else
            substitution = target_char
        end if
        write(log_unit, '(3A,I5,2A,I5,3A,A20,A,A20)') ' Input ', caller, ' nr:', itmnr, ' is ', caller, &
                name_index, ' with ', trim(id_or_type), '  : ', name, ' and local substitution: ', substitution
    end subroutine log_name_substitution

    subroutine error_and_finish(error_ind, ithndl)
        integer(kind = int_wp), intent(inout) :: error_ind !< Error index
        integer(kind = int_wp), intent(in) :: ithndl    !< Handle to stop timing

        error_ind = 1
        call finish(ithndl)
    end subroutine error_and_finish

    subroutine finish(ithndl)
        integer(kind = int_wp), intent(in) :: ithndl    !< Handle to stop timing

        if (timon) call timstop(ithndl)
    end subroutine finish

    subroutine read_boundary_conditions_from_ods_file(file_name, file_unit, char_arr, int_array, real_array, &
            max_char_size, max_int_size, max_real_size, dp_array, num_items, &
            num_dims, input_order, scale, itmnr, idmnr, &
            missing_value, num_records, ierr, status)

        ! Boundary and waste data new style Data retrieval from an ODS file
        !     char_arr     character  *         local   character workspace
        !     num_dims   integer    1         input
        !     input_order  integer    1         input   order of the input
        !     scale   logical    1         input   true if scale values are stored
        !     ioffc   integer    1         input   offset of the concentrations
        !     ioffi   integer    1         input   offset in the integer array
        !     ioffi   integer    1         input   offset of the items
        !     ioffi   integer    1         input   offset in the integer array
        !     missing_value   real       1         input   missing value indicator
        !     num_records   integer    1         output  number of time steps found
        !     ierr    integer    1         output  error flag
        !     iwar    integer    1         in/out  cumulative warning count
        !
        !     in the common block:
        !
        !     Name    Kind     Length     Funct.  Description
        !     ---------------------------------------------------------
        !     block sysi.inc
        !     itstrt  integer    1         input   simulation start time ( scu )
        !     itstop  integer    1         input   simulation stop time ( scu )
        !     isfact  integer    1         input   system clock in seconds
        !     otime   real*8     1         input   julian offset of the real time
        !
        !     The map of the Character array is:
        !     First num_items + num_dims entries the names of ITEMS and SUBSTANCES
        !         IF input_order = 1 then ITEMS first IF 2 then CONCENTRATIONS first
        !     The rest of the array is free working space for this routine
        !         Next num_items + num_dims entries reserved for names of values to be
        !                                                             retrieved
        !         Further locations is workspace
        !
        !     The map of the Integer array is:
        !     First num_items + num_dims entries the names of ITEMS and SUBSTANCES
        !         IF input_order = 1 then ITEMS first IF 2 then CONCENTRATIONS first
        !     Then num_records time breakpoints to be read in eg in this routine
        !     The rest of the array is free working space
        !         Initially next num_items + num_dims entries reserved for numbers of
        !              locations or substances as stored in the character array.
        !              In the corresponding location below num_items + num_dims a
        !                                  reference is made to this location.
        !         Initially next num_items + num_dims entries reserved for numbers of
        !              locations or substances as stored in the ODS file for
        !                                  retrieval
        !         Initially next num_records values are for retrieved time values
        !
        !     The map of the Integer array is:
        !     IF (SCALE) First num_dims entries the scale factors
        !     Then the matrix of values to be read in eg in this routine
        !
        use m_usefor, only : compact_usefor_list
        use open_data_structure, only : get_time, get_parameter, get_matrix_1, get_loc, get_dimension
        use m_sysi          ! Timer characteristics
        use time_module
        use m_string_utils

        integer(kind = int_wp), intent(in) :: max_char_size, max_int_size, max_real_size
        character(len = *), intent(in) :: file_name
        character(len = *), intent(inout) :: char_arr(:)
        integer(kind = int_wp), intent(inout) :: int_array(:)
        real(kind = real_wp), intent(inout) :: real_array(:)
        integer(kind = int_wp), intent(inout) :: num_items          !! number of bounds/wastes
        integer(kind = int_wp), intent(out) :: num_records
        logical :: scale
        real(kind = dp) :: dp_array(*)
        character :: cfile(3)*256
        real(kind = real_wp) :: missing_value

        type(error_status), intent(inout) :: status !< current error status

        ! local declarations
        dimension     loc(3)
        real(kind = dp) :: afact, a1, a2, d_beg, d_end, dummy
        character*3   cdummy
        integer(kind = int_wp) :: num_dims              !! number of concentrations
        integer(kind = int_wp) :: input_order, ioffa, ioffb, ioffc, ioffd, nscle, file_unit
        integer(kind = int_wp) :: k1, ierror, nsubs, nlocs, ntims, j1, j2, j3, k2, k3
        integer(kind = int_wp) :: ierr, noloc, noit2, noitv, j
        integer(kind = int_wp) :: nottt, itmnr, notim, idmnr, i, ishft, ltot
        integer(kind = int_wp) :: nshft, nopar, icnt, k5, nitm, k, k4, k6
        integer(kind = int_wp) :: iy1, im1, id1, ih1, in1, is1
        integer(kind = int_wp) :: iy2, im2, id2, ih2, in12
        integer(kind = int_wp) :: i1, i2, in2, is2, nt1, nt2, is, maxd, loc, ig, igs, kp
        integer(kind = int_wp) :: kl, ig2

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_boundary_conditions_from_ods_file", ithndl)

        ! array offsets
        nottt = itmnr + num_items + idmnr + num_dims
        if (input_order == 1) then
            ioffa = itmnr
            ioffb = itmnr + num_items + idmnr
            ioffc = 0
            ioffd = itmnr + num_items
            nscle = num_dims
        else if (input_order == 2) then
            ioffa = idmnr + num_dims + itmnr
            ioffb = idmnr
            ioffc = idmnr + num_dims
            ioffd = 0
            nscle = num_items
        end if

        ! write the ods file name
        write (file_unit, 1000) file_name

        ! get the dimensions of the ods file
        cfile(1) = file_name
        cfile(3) = ' '
        k1 = nottt + 1
        call get_dimension (cfile, 0, cdummy, 0, 0, &
                0, int_array(k1:k1), ierror, cfile(3))
        nsubs = int_array(k1)
        nlocs = int_array(k1 + 1)
        ntims = int_array(k1 + 2)

        !  deal with locations ( j for characters, k for integers )
        j1 = nottt + 1
        j2 = j1 + 1
        k1 = nottt + num_items + 1
        j3 = j2 + nlocs
        k2 = k1 + nlocs

        !  see if storage is available
        k3 = min ((max_int_size - k2), (max_char_size - j3))
        if (k3 < nlocs) then
            write (file_unit, 1010) k3, nlocs
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if

        ! get the available locations
        char_arr(j1) = '*'
        call get_loc (cfile, 0, char_arr(j1), 1, 0, &
                0, k3, char_arr(j2), int_array(k1:k1), int_array(k2:k2), &
                noloc, ierror, cfile(3))

        ! fill an array with wanted locations
        noit2 = 0
        noitv = 0
        do j = 1, num_items
            if (char_arr(ioffa + j) == '&$&$SYSTEM_NAME&$&$!') then
                noit2 = noit2 + 1
                noitv = noitv + 1
                char_arr(ioffa + noit2) = char_arr(ioffa + j)
                char_arr(ioffc + noit2) = char_arr(ioffc + j)
                int_array(ioffa + noit2) = int_array(ioffa + j)
                int_array(ioffc + noit2) = int_array(ioffc + j)
                int_array(nottt + noit2) = -1
                if (scale .and. input_order == 2) real_array(noit2) = real_array(j)
                cycle
            end if
            i = index_in_array(char_arr(ioffa + j)(:20), char_arr(j1 + 1:noloc))
            if (i >= 1) then
                noit2 = noit2 + 1
                char_arr(ioffa + noit2) = char_arr(ioffa + j)
                char_arr(ioffc + noit2) = char_arr(ioffc + j)
                int_array(ioffa + noit2) = int_array(ioffa + j)
                int_array(ioffc + noit2) = int_array(ioffc + j)
                int_array(nottt + noit2) = i
                cycle
            end if
            write (file_unit, 1070) int_array(ioffa + j), char_arr(ioffa + j)
            call status%increase_warning_count()
            if (int_array(ioffa + j) < 0 .or. (int_array(ioffa + j + 1) < 0 .and. j /= num_items)) then
                write (file_unit, 1080)
                ierr = 2
                if (timon) call timstop(ithndl)
                return
            end if
        end do

        ! compact the pointers for unresolved externals
        ishft = num_items - noit2
        if (input_order == 1) then
            ltot = idmnr + num_dims
        else
            ltot = 0
        end if
        do i = ioffa + noit2 + 1, ioffa + noit2 + ltot + noit2
            char_arr(i) = char_arr(i + ishft)
            int_array(i) = int_array(i + ishft)
        end do
        do i = ioffc + noit2 + 1, ioffc + noit2 + ltot + noit2 * 2
            char_arr(i) = char_arr(i + ishft)
            int_array(i) = int_array(i + ishft)
        end do
        nottt = nottt - ishft * 2
        itmnr = itmnr - ishft
        num_items = num_items - ishft
        if (input_order == 1) then
            ioffb = itmnr + num_items + idmnr
            ioffd = itmnr + num_items
            nshft = 0
        else
            nshft = itmnr + num_items
        end if

        ! deal with substances
        j1 = nottt + 1
        j2 = j1 + 1
        k1 = nottt + num_items + 1
        j3 = j2 + nsubs
        k2 = k1 + nsubs

        ! see if storage is available
        k3 = min ((max_int_size - k2), (max_char_size - j3))
        if (k3 < nsubs) then
            write (file_unit, 1010) k3, nsubs
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if

        ! get the available substances
        call get_parameter (cfile, 0, char_arr(j1), 1, 0, &
                0, k3, 0, char_arr(j2), char_arr(j3), &
                int_array(k1:k1), int_array(k2:k2), nopar, ierror, cfile(3))

        ! fill an array with wanted substances
        icnt = 0
        k5 = nottt + num_items
        nitm = num_dims
        do j = 1, nitm
            k = j - icnt
            int_array(k5 + k) = 0
            if (char_arr(ioffb + j) == '&$&$SYSTEM_NAME&$&$!') cycle
            i = index_in_array(char_arr(ioffb + k)(1:20), char_arr(j1 + 1:nopar))
            if (i >= 1) then
                int_array(k5 + k) = i
                cycle
            end if
            call compact_usefor_list(file_unit, int_array, itmnr, num_items, idmnr, &
                    num_dims, input_order, char_arr, k5, ioffb, &
                    nshft, ioffd, k, icnt, ierr, status)
            if (timon) call timstop(ithndl)
            return
        end do
        k1 = k1 + num_dims

        ! get the time values
        k3 = max_int_size - k1
        ! first num_dims real*4 can be scale values
        k2 = 1
        if (scale) k2 = nscle / 2 + 2
        k5 = k2 + 3
        k4 = (max_real_size - k5 * 2) / 2
        ! see if there is space enough
        k4 = min (k3, k4)
        ! see if storage is available
        if (k4 < ntims) then
            write (file_unit, 1010) k4, ntims
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if
        afact = isfact / 864.0d+02
        if (isfact < 0) afact = -1.0d+00 / isfact / 864.0d+02

        ! get the available time values
        dp_array(k2) = 0
        call get_time(cfile, 0, dp_array(k2), 1, 0, &
                0, k4, dp_array(k5), int_array(k1:k1), num_records, &
                ierror, cfile(3))
        ! see if the found time values are within the range
        if (num_records >= 1) then
            write (file_unit, 1020)
            a1 = deltim + itstrt * afact
            a2 = deltim + itstop * afact
            i1 = 1
            i2 = 1
            do i = 1, num_records
                if (dp_array(k5 + i - 1) <= a1) i1 = i
                if (dp_array(k5 + i - 1) < a2) i2 = i
            end do
            if (i2 /= num_records) i2 = i2 + 1
            k6 = k5 + num_records - 1
            if (dp_array(k6) < a1) i2 = 1
            !        errors and warnings
            if (dp_array(k5) > a1) then
                call gregor (dp_array(k5), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a1, iy2, im2, id2, ih2, in2, is2, dummy)
                write (file_unit, 1030)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
                call status%increase_warning_count()
            end if
            if (dp_array(k6) < a2) then
                call gregor (dp_array(k6), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a2, iy2, im2, id2, ih2, in2, is2, dummy)
                write (file_unit, 1040)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
                call status%increase_warning_count()
            end if
            num_records = i2 - i1 + 1
        end if
        write (file_unit, 1050) num_records
        if (num_records == 1)    write (file_unit, 1060)
        ! times are converted to delwaq times
        do i = i1, i2
            a2 = dp_array(k5 + i - 1) - deltim
            int_array(k1 + i - i1) = a2 / afact + 0.5
        end do
        ! see if enough space is available
        ! nr substances  nr locations for retrieval
        nt1 = num_dims * num_items
        ! nr substances  nr locations for storage
        nt2 = num_dims + num_items
        ! real retrieval space + 1
        is = nt1 * num_records + 1
        if (scale) is = is + nscle
        ! convert for double precission,
        ! num_records is max number of retrievals per invocation
        is2 = (is + num_records + 1) / 2 + 1
        ! then the offset increases
        maxd = max_real_size - is
        if (maxd < num_records) then
            write (file_unit, 1010) is + num_records, max_real_size
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if
        ! set the time margins for retrieval
        d_beg = dp_array(k5 + i1 - 1) - afact / 2.0
        d_end = dp_array(k5 + i2 - 1) + afact / 2.0
        dp_array(is2) = d_beg
        dp_array(is2 + 1) = d_end

        ! get the data themselves
        loc(3) = 1
        igs = 1
        ig = 1
        if (scale) ig = ig + nscle
        do i = 1, num_dims
            ! this should correspond with the found substance numbers
            kp = int_array(nottt + num_items + i)
            if (kp < 0) cycle
            if (input_order == 1) then
                ig = igs
                igs = igs + 1
                if (scale) ig = ig + nscle
            end if
            do j = 1, num_items
                ! this should correspond with the found location numbers
                kl = int_array(nottt + j)
                if (kl > 0) then
                    loc(1) = kl
                    loc(2) = kl
                    call get_matrix_1 (cfile, 0, kp, loc, dp_array(is2), &
                            missing_value, maxd, real_array(is:is), ierror, &
                            cfile(3))
                end if
                ig2 = ig
                !  this loop is per location, so skip the amount of substances if input_order is 1
                if (input_order == 1) then
                    ig = ig + num_dims
                else
                    ig = ig + 1
                end if
                do k = 0, num_records - 1
                    real_array(ig2) = real_array(is + k)
                    ! skip a full matrix further, because this is this substance for all breakpoints
                    ig2 = ig2 + nt1
                end do ! k = 0 , num_records-1
            end do ! do j = 1 , num_items
        end do ! do i = 1 , num_dims
        do i = 1, nscle
            int_array(nottt + i) = i
        end do
        do i = 1, num_records
            int_array(nottt + nscle + i) = int_array(k1 + i - 1)
        end do
        if (timon) call timstop(ithndl)
        return

        1000 format(' DATA will be retrieved from ODS-file: ', A)
        1010 format(' ERROR: Insufficient memory ! Available:', I10, ', needed:', I10, ' !')
        1020 format(' This block consists of a time function.')
        1030 format(' WARNING: file start time   : ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, / &
                ' after simulation start time: ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1040 format(' WARNING: file stop  time   : ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, /  &
                ' before simulation stop time: ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1050 format(' Number of valid time steps found: ', I6)
        1060 format(' This block consists of constant data.')
        1070 format(' WARNING: location : ', I8, ' not found. Name is: ', A)
        1080 format(' ERROR  : location is used in a computation', ' that will become corrupted !')

    end subroutine read_boundary_conditions_from_ods_file

    subroutine read_time_series_table(file_unit, int_array, real_array, max_int_size, max_real_size, &
            input_file_start_position, num_significant_char, ilun, file_name_list, lstack, &
            comment_charachter, charachter_output, notot, nototc, time_dependent, num_records, &
            time_function_type, is_date_format, is_yyddhh_format, itfact, itype, &
            int_output, real_output, ierr, ierr3)
        !! Boundary and waste data new style

        ! LOGICAL UNITS: LUN(27) = unit stripped DELWAQ input file
        !                LUN(29) = unit formatted output file
        !                LUN( 2) = unit intermediate file (system)
        !                LUN(14) = unit intermediate file (boundaries)
        !                LUN(15) = unit intermediate file (wastes)
        !
        !     ILUN    INTEGER   LSTACK     INPUT   unitnumb include stack
        !     LSTACK  INTEGER    1         INPUT   include file stack size
        !     charachter_output   CHAR*(*)   1         OUTPUT  space for limiting token
        !     NOTOT   INTEGER    1         INPUT   size of the matrix to be read
        !     ITTIM   INTEGER    1         INPUT   0 if steady, 1 if time function
        !     ITFACT  INTEGER    1         INPUT   factor between clocks
        !     ITYPE   INTEGER    1         OUTPUT  type of info at end
        !     IERR    INTEGER    1         OUTPUT  return code
        !     IERR3   INTEGER    1         OUTPUT  actual error indicator

        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        !< true if the bc or waste load definition is time dependent (linear, harmonic or fourier), and false if it
        !! is constant.
        logical, intent(in) :: time_dependent
        integer(kind = int_wp), intent(in) :: max_int_size, max_real_size, file_unit
        character(len = *), intent(in) :: file_name_list(lstack), charachter_output     !! file name stack, 4 deep
        character(len = 1), intent(in) :: comment_charachter
        dimension     ilun(lstack)
        logical :: newrec, ignore
        logical, intent(in) :: is_date_format                     !! True if time in 'date' format
        logical, intent(in) :: is_yyddhh_format                   !! True if YYetc instead of DDetc
        integer(kind = int_wp) :: int_output
        integer(kind = int_wp), intent(out) :: num_records        !! number of records read
        integer(kind = int_wp), intent(inout) :: input_file_start_position  !!      Start position on input line
        integer(kind = int_wp), intent(in) :: num_significant_char                      !! nr of significant characters
        integer(kind = int_wp) :: ilun, ierr, itfact
        integer(kind = int_wp), intent(inout) :: int_array(*)
        real(kind = real_wp), intent(inout) :: real_array(:)
        integer(kind = int_wp), intent(in) :: notot
        integer(kind = int_wp) :: nototc
        integer(kind = int_wp) :: lstack
        integer(kind = int_wp), intent(in) :: time_function_type        !! 3 is harmonics, 4 is fourier

        real :: real_output
        integer(kind = int_wp) :: ithndl = 0, i, itel, itel2, ierr3, itype
        if (timon) call timstrt("read_time_series_table", ithndl)

        ignore = .false.
        newrec = .false.
        if (time_dependent) newrec = .true.                    ! it is a time function
        num_records = 0
        itel = 1
        itel2 = 1
        ierr3 = 0
        if (itype /= 0) goto 20                                ! it was called with an argument

        ! read loop
        10 if (newrec) then
            itype = 0                                          ! everything is valid
        else
            itype = 3                                          ! a real value schould follow
        endif
        call rdtok1 (file_unit, ilun, file_name_list, lstack, comment_charachter, &
                input_file_start_position, num_significant_char, charachter_output, int_output, real_output, &
                itype, ierr)
        ! a read error
        if (ierr  /= 0) goto 9999
        ! a token has arrived
        if (itype == 1) then                                   ! that must be an absolute timer string
            !  2^31 =  2147483648
            call convert_string_to_time_offset (charachter_output, int_output, .false., .false., ierr)
            ! yyyydddhhmmss so 64 bits integer
            if (int_output == -999) then
                ierr = 1
                write (file_unit, 1020) trim(charachter_output)
                goto 9999
            endif
            if (ierr /= 0) then                                ! the found entry is not a new time value
                if (num_records <= 1) then
                    write (file_unit, 1040) num_records
                    !ierr3 = ierr3 + 1
                endif
                ierr = 0
                goto 9999
            endif
            int_output = itfact * int_output
        elseif (itype == 2) then
            call convert_relative_time (int_output, 1, is_date_format, is_yyddhh_format)
        else
            int_output = 0
        endif
        ! getting the data of this block (no strings any more)
        20 if (time_dependent .and. newrec) then
            !          it was a non-real and characters has been caught
            if (int_output == -999) then
                ignore = .true.
            else                                                      ! a new breakpoint found
                ignore = .false.
                num_records = num_records + 1
                if (num_records <= max_int_size) then
                    int_array(num_records) = int_output
                    if (num_records > 1) then
                        if (int_output <= int_array(num_records - 1)) then ! times not strinctly ascending
                            write (file_unit, 1030) int_output, int_array(num_records - 1)
                            ierr3 = ierr3 + 1
                        endif
                    endif
                else
                    write (file_unit, 1000) max_int_size
                    ierr = 100
                    goto 9999
                endif
            endif
            newrec = .false.
            goto 10
        endif

        if (.not. ignore) then
            do i = 1, notot / nototc
                real_array(itel + (i - 1) * nototc) = real_output
            end do
        endif
        ! are we to expect a new record ?
        if (mod(itel2, nototc) == 0) then
            newrec = .true.
            itel = itel + notot - nototc
        end if
        !        it was a constant, so we can now return.
        if (newrec .and. (.not. time_dependent)) then
            num_records = 1
            int_array(1) = 0
            goto 9999
        endif
        !        increase the counter for the next real and go to input
        if (.not. ignore) itel = itel + 1
        itel2 = itel2 + 1
        goto 10
        9999 if (timon) call timstop(ithndl)
        return

        1000 FORMAT (' ERROR ! Number of breakpoints exceeds system', &
                ' maximum of: ', I10)
        1020 FORMAT (' ERROR ! Absolute timer does not fit in timer ', &
                'format: ', A, / &
                ' Is your T0 setting in block #1 correct?'/, &
                ' Allowed difference with T0 is usually ca. 68 years.')
        1030 FORMAT (/' ERROR ! Time value ', I10, ' not larger than previous time value ', I10)
        1040 FORMAT (/' WARNING ! There are only ', I2, ' breakpoints found for this time series')

    end subroutine read_time_series_table

end module boundary_condition_utils
