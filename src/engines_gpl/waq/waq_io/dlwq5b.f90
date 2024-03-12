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
module m_dlwq5b

    use timers, only : timon, timstrt, timstop
    use m_waq_precision
    use m_array_manipulation, only : shift_array_right
    use m_string_manipulation, only : shift_char_subarray
    use m_error_status
    implicit none

    private
    public :: dlwq5b

contains


    subroutine dlwq5b(lunut, iposr, npos, cchar, car, &
            iar, icmax, iimax, all_names, all_types, &
            bc_wl_count, bc_wl_types_count, parsed_items_count, noits, chkflg, &
            caller, ilun, lch, lstack, itype, &
            rar, nconst, itmnr, parsed_str, ioutpt, &
            error_ind, status)

        use m_string_utils, only : index_in_array, join_strings, string_equals

        !   Arguments
        integer(kind = int_wp), intent(in) :: icmax        !< Max. Char workspace dimension
        integer(kind = int_wp), intent(in) :: iimax        !< Max. Int. Workspace dimension
        integer(kind = int_wp), intent(in) :: chkflg       !< Check on input or add items
        integer(kind = int_wp), intent(in) :: lunut        !< Unit Formatted Output File
        integer(kind = int_wp), intent(inout) :: iposr        !< Start position on input line
        integer(kind = int_wp), intent(in) :: npos         !< Nr of significant characters
        integer(kind = int_wp), intent(out) :: iar(:)       !< Integer workspace
        integer(kind = int_wp), intent(inout) :: bc_wl_count  !< Number of bounds/wastes
        integer(kind = int_wp), intent(in) :: bc_wl_types_count       !< Number of bound/waste types
        integer(kind = int_wp), intent(out) :: parsed_items_count        !< Number of items read
        integer(kind = int_wp), intent(out) :: noits        !< Number of items for scale
        integer(kind = int_wp), intent(inout) :: ilun(lstack) !< Unitnumb include stack
        integer(kind = int_wp), intent(in) :: lstack       !< Include file stack size
        integer(kind = int_wp), intent(out) :: itype        !< Type of the token read ('at exit')
        integer(kind = int_wp), intent(out) :: nconst       !< Number of values in rar
        integer(kind = int_wp), intent(in) :: ioutpt       !< Output file option
        integer(kind = int_wp), intent(out) :: error_ind    !< Error indicator

        real(kind = real_wp), intent(out) :: rar(:)       !< Array with real values

        character(1), intent(in) :: cchar        !< Comment character
        character(*), intent(out) :: car(:)       !< Character workspace
        character(*), intent(inout) :: all_names(:)     !< Id's of the boundaries/wastes
        character(*), intent(in) :: all_types(:)     !< Types of the boundaries/wastes
        character(10), intent(in) :: caller       !< Calling subject
        character(*), intent(inout) :: lch(lstack)  !< File name stack, 4 deep
        character(*), intent(out) :: parsed_str   !< Input string at end of routine

        type(error_status), intent(inout) :: status !< current error status

        !   Local variables
        logical :: usefor_on, substitution_on, can_compute, operator_on
        logical :: logging_on
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: i, parsed_int, ifound, i2, name_index, icm
        integer(kind = int_wp) :: itmnr, ioff, ioffc, ioffi
        real(kind = real_wp) :: parsed_real
        character(*), parameter :: operations(6) = ['*', '/', '+', '-', 'MIN', 'MAX']
        character(*), parameter :: keywords(25) = ['BLOCK', &
                'BINARY_FILE', &
                'CONCENTRATIONS', &
                'CONCENTRATION', &
                'CONSTANTS', &
                'DATA', &
                'DATA_ITEM', &
                'LINEAR', &
                'ITEM', &
                'IDENTICALITEM', &
                'USEDATA_ITEM', &
                'FORITEM', &
                'TIME_DELAY', &
                'ODS_FILE', &
                'ABSOLUTE', &
                'TIME', &
                'HARMONICS', &
                'FOURIERS', &
                'SCALE', &
                'DEFAULTS', &
                'ALL', &
                'SEGMENTS', &
                'PARAMETERS', &
                'FUNCTIONS', &
                'SEG_FUNCTIONS'  ]

        if (timon) call timstrt("dlwq5b", ithndl)

        ! some initialisations
        usefor_on = .false.
        substitution_on = .false.
        can_compute = .false.
        operator_on = .false.
        logging_on = (ioutpt >= 3)
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
            call rdtok1(lunut, ilun, lch, lstack, cchar, &
                    iposr, npos, parsed_str, parsed_int, parsed_real, &
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
                    call shift_array_right(iar, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    iar(itmnr + parsed_items_count + parsed_items_count) = 0
                    select case(parsed_str)
                    case ('*')
                        iar(itmnr + parsed_items_count) = -1000000
                    case ('/')
                        iar(itmnr + parsed_items_count) = -10000000
                    case ('+')
                        iar(itmnr + parsed_items_count) = -100000000
                    case ('-')
                        iar(itmnr + parsed_items_count) = -1000000000
                    case ('MIN')
                        iar(itmnr + parsed_items_count) = -1100000000
                    case ('MAX')
                        iar(itmnr + parsed_items_count) = -1200000000
                    end select
                    operator_on = .true.
                    cycle read_and_process
                end if

                ! Scenario: an item used in computations
                if (operator_on) then
                    do i = 1, itmnr - 1
                        if (iar(i) == -1300000000) cycle
                        ifound = index_in_array(parsed_str, car(i + ioff:i + ioff))
                        if (ifound == 1) then
                            noits = noits - 1
                            i2 = iar(itmnr + parsed_items_count)
                            call log_item_number_name(i2, lunut, i, parsed_str)
                            iar(itmnr + parsed_items_count) = i2 + i
                            car(itmnr + parsed_items_count + ioff) = '&$&$SYSTEM_NAME&$&$!'
                            operator_on = .false.
                            cycle read_and_process
                        end if
                    end do
                    i2 = iar(itmnr + parsed_items_count)
                    call log_local_substitution(i2, lunut, parsed_str)
                    iar (itmnr + parsed_items_count + parsed_items_count) = noits
                    car (itmnr + parsed_items_count + ioff) = parsed_str
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
                    name_index = iar(itmnr)
                    if (logging_on) then
                        call log_name_substitution(name_index, lunut, all_names, caller, itmnr, all_types, parsed_real, parsed_str, .false.)
                    end if
                    iar(itmnr + parsed_items_count + parsed_items_count) = noits
                    car(itmnr + parsed_items_count + ioff) = parsed_str
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
                    call shift_array_right(iar, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(iar, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(car, itmnr + ioff, icm)
                    iar (itmnr) = 0
                    iar (itmnr + parsed_items_count) = itmnr
                    iar (itmnr + parsed_items_count + parsed_items_count) = noits
                    car (itmnr + ioff) = parsed_str
                    car (itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1020) caller, itmnr, caller, 0, 'FLOW'
                    end if
                    cycle read_and_process
                end if

                ! parsed_str == item-NAME
                ifound = index_in_array(parsed_str(1:len(all_names(1))), all_names(1:bc_wl_count))
                if (ifound >= 1) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(iar, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(iar, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(car, itmnr + ioff, icm)
                    iar(itmnr) = ifound
                    iar(itmnr + parsed_items_count) = itmnr
                    iar(itmnr + parsed_items_count + parsed_items_count) = noits
                    car(itmnr + ioff) = parsed_str
                    car(itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1020) caller, itmnr, caller, ifound, all_names(ifound)
                    end if
                    cycle read_and_process
                end if

                ! parsed_str == item-TYPE. IAR now is negative.
                ifound = index_in_array(parsed_str(1:len(all_types(1))), all_types(1:bc_wl_types_count))
                if (ifound >= 1) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(iar, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(iar, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(car, itmnr + ioff, icm)
                    iar(itmnr) = -ifound
                    iar(itmnr + parsed_items_count) = itmnr
                    iar(itmnr + parsed_items_count + parsed_items_count) = noits
                    car(itmnr + ioff) = parsed_str
                    car(itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1030) caller, itmnr, caller, ifound, all_types(ifound)
                    end if
                    cycle read_and_process
                end if

                ! If only existing names or types are allowed then
                !        this is the place for an error massage
                ! JVB stick to just a warning keep on reading IAR = 0?, or used for flow??

                if (chkflg == 1) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(iar, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(iar, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(car(:icm + 1), itmnr + ioff, icm)
                    iar (itmnr) = -1300000000 ! pointer should be ignored
                    iar (itmnr + parsed_items_count) = 1300000000 ! item number should be ignored
                    iar (itmnr + parsed_items_count + parsed_items_count) = noits
                    car (itmnr + ioff) = parsed_str
                    car (itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    write(lunut, 1040) caller, itmnr, parsed_str

                    call status%increase_warning_count()
                else
                    ! Now a new name is added to the list of names
                    !        the rest is moved upward since it is all 1 array
                    bc_wl_count = bc_wl_count + 1
                    ioff = ioff + 1
                    icm = icmax + bc_wl_count
                    call shift_char_subarray(all_names, bc_wl_count, icm)
                    all_names(bc_wl_count) = parsed_str
                    ! plus normal procedure
                    parsed_items_count = parsed_items_count + 1
                    noits = noits + 1
                    itmnr = itmnr + 1
                    icm = itmnr + parsed_items_count + ioff
                    call shift_array_right(iar, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(iar, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(car, itmnr + ioff, icm)
                    iar(itmnr) = bc_wl_count
                    iar(itmnr + parsed_items_count) = itmnr
                    iar(itmnr + parsed_items_count + parsed_items_count) = noits
                    car(itmnr + ioff) = parsed_str
                    car(itmnr + parsed_items_count + ioff) = parsed_str
                    if (usefor_on) substitution_on = .true.
                    if (logging_on .and. .not. usefor_on) then
                        write (lunut, 1020) caller, itmnr, caller, bc_wl_count, all_names(bc_wl_count)
                    end if
                end if
                cycle read_and_process
            end if

            ! Scenario: a number (int, 2, or real, 3) is used in computations
            if (itype == 2 .or. itype == 3) then
                if (substitution_on .or. operator_on) then
                    nconst = nconst + 1
                    rar(nconst) = parsed_real
                    noits = noits - 1
                    i2 = iar(itmnr + parsed_items_count)
                    car(itmnr + parsed_items_count + ioff) = '&$&$SYSTEM_NAME&$&$!'
                    if (operator_on) then
                        if (logging_on) then
                            call log_number_in_operation(i2, lunut, parsed_real)
                        end if
                        iar(itmnr + parsed_items_count) = i2 - nconst
                        operator_on = .false.
                    end if
                    if (substitution_on) then
                        name_index = iar(itmnr)
                        if (logging_on) then
                            call log_name_substitution(name_index, lunut, all_names, caller, itmnr, all_types, parsed_real, parsed_str, .true.)
                        end if
                        iar(itmnr + parsed_items_count) = -nconst
                        iar(itmnr + parsed_items_count + parsed_items_count) = 0
                        usefor_on = .false.
                        substitution_on = .false.
                        can_compute = .true.
                    end if
                    cycle read_and_process
                end if
            end if

            ! Scenario: no item name was given, but an item number
            if (itype == 2) then
                if (parsed_int <=  bc_wl_count .and. parsed_int >= -bc_wl_types_count) then
                    call update_counters(parsed_items_count, noits, itmnr)
                    icm = itmnr + parsed_items_count + ioff

                    call shift_array_right(iar, itmnr, itmnr + parsed_items_count * 2)
                    call shift_array_right(iar, itmnr + parsed_items_count, itmnr + parsed_items_count * 2)
                    call shift_char_subarray(car, itmnr + ioff, icm)

                    iar (itmnr) = parsed_int
                    iar (itmnr + parsed_items_count) = itmnr
                    iar (itmnr + parsed_items_count + parsed_items_count) = noits
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
                    car (itmnr + ioff) = parsed_str
                    car (itmnr + parsed_items_count + ioff) = parsed_str
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
        1020 format(' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with ID  : ', &
                A20)
        1030 format(' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with type: ', &
                A20)
        1035 format(' ERROR: no reserved keyword expected: ', A20)
        1040 format(' WARNING: Input ', A, ' nr:', I5, ' with name: ', A20, &
                ' is not a valid ID, data ignored')
        1060 format(' ERROR: number: ', I5, ' is not a valid item number !')
        1070 format(' ERROR: multiplication is only allowed in USEFOR', &
                ' context !')

    end subroutine dlwq5b

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


end module m_dlwq5b
