!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
module data_processing
    use m_waq_precision
    use m_string_utils, only : string_equals, index_in_array

    implicit none

    private
    public :: close_files, delete_file, extract_value_from_group, extract_integer, extract_real, &
            extract_logical, read_substances

contains

    subroutine close_files(logical_units)
        !! Close all files except the monitoring file (except for the one at 19th position)
        !! the subroutine assumes that the monitoring file is at the 19th position in the logical_units array
        !! and that the array has a length of 22

        integer(kind = int_wp), dimension(*), intent(in) :: logical_units !! Array of logical unit numbers

        logical :: is_file_open
        integer(kind = int_wp) :: exclude_index = 19 ! the monotoring file at the 19th position in the logical_units
        integer(kind = int_wp) :: i

        do i = 1, 22
            inquire (logical_units(i), opened = is_file_open)
            if (is_file_open .and. i /= exclude_index) then
                close (logical_units(i))
            endif
        enddo

    end subroutine close_files

    subroutine delete_file (file_name, error_flag)
        !! Deletes a file by its name and returns an error flag if the deletion fails
        character(len = *), intent(in)      :: file_name       !! Name of the file to be deleted
        integer(kind = int_wp), intent(out) :: error_flag      !! Error indicator (0 = no error, 1 = error)

        integer(kind = int_wp) :: io_logical_unit, free_logical_unit
        logical :: is_file_open, file_exists

        error_flag = 0
        io_logical_unit = 0

        ! Check if the file exists
        INQUIRE (FILE = file_name, EXIST = file_exists)
        ! If file doesn't exist, exit subroutine
        IF (.NOT. file_exists) RETURN

        ! Find an available logical unit number
        DO free_logical_unit = 10, 99
            INQUIRE (UNIT = free_logical_unit, OPENED = is_file_open)
            IF (.NOT. is_file_open) THEN
                io_logical_unit = free_logical_unit
                ! Exit the loop once a free unit is found
                exit
            ENDIF
        end do

        ! Open and then delete the file
        IF (io_logical_unit /= 0) THEN
            ! OPEN(io_logical_unit, FILE = file_name, ERR = 900)
            OPEN(UNIT = io_logical_unit, FILE = file_name, STATUS = 'OLD', IOSTAT = error_flag)
            IF (error_flag == 0) THEN
                CLOSE(UNIT = io_logical_unit, STATUS = 'DELETE', IOSTAT = error_flag)
            END IF
        ELSE
            ! Set error flag if no free logical unit is found
            error_flag = 1
        ENDIF

        RETURN

        ! Error handling
        IF (error_flag /= 0) THEN
            ! Ensure the file is closed in case of error
            CLOSE (UNIT = io_logical_unit, STATUS = 'KEEP')
        END IF

    end subroutine delete_file

    subroutine extract_value_from_group (file_unit, group, keyword, value)
        !! read and parse a configuration or data file to extract a value associated with a specified keyword within
        !! a specified group.
        integer(kind = int_wp), intent(in) :: file_unit  ! Logical unit number for the file
        character(len = *), intent(in) :: group, keyword  ! Group and keyword to search
        character(len = *), intent(out) :: value  ! Value associated with the keyword

        logical :: group_open, group_closed
        character(len = 256) :: line, group_extracted, keyword_extracted, value_extracted
        integer(kind = int_wp) :: group_len, keyword_len, value_len
        integer(kind = int_wp) :: str_len, error_code, str_comp, index
        integer(kind = int_wp) :: timer_handle = 0

        rewind (file_unit)

        group_open = .false.
        group_closed = .false.
        group_len = len(group)
        keyword_len = len(keyword)
        value_len = len(value)
        value = ' '

        do
            line = ' '
            read (file_unit, '(a)', end = 90, err = 90) line

            ! check for group separator
            call extract_string_by_separator (line, 256, '[', ']', group_extracted, str_len, error_code)
            if (error_code == 0 .and. str_len > 0) then
                ! Group separator found
                str_comp = min (str_len, group_len)
                if (string_equals(group_extracted(1:str_comp), group(1:str_comp))) then
                    ! group name equals requested group
                    group_open = .true.
                else
                    ! Different group name.
                    if (group_open) group_closed = .true.
                endif

                ! Finish if requested group is passed.
                if (group_closed) exit
            else
                ! check for keyword if group is open
                if (group_open) then
                    call extract_string_by_separator (line, 256, '*', '=', keyword_extracted, str_len, error_code)
                    if (error_code == 0 .and. str_len > 0) then

                        ! keyword found
                        str_comp = min (str_len, keyword_len)
                        if (string_equals(keyword_extracted(1:str_comp), keyword(1:str_comp))) then

                            ! keyword equals requested keyword
                            call extract_string_by_separator(line, 256, '=', '*', value_extracted, str_len, error_code)
                            if (error_code == 0 .and. str_len > 0) then
                                ! value succesfully read
                                value = ' '
                                str_comp = min (str_len, value_len)
                                value(1:str_comp) = value_extracted(1:str_comp)
                                exit
                            endif
                        endif
                    endif
                endif
            endif

        end do
        90 continue

    end subroutine extract_value_from_group

    subroutine extract_string_by_separator(string, string_length, lead_separator, trail_separator, sub_string, &
            sub_string_length, error_code)
        !! extracts a substring from a given string based on provided leading and trailing separators.
        character(len = *), intent(in) :: string          !! input string
        character(len = 1), intent(in) :: trail_separator !! trailing separator ( '*' is till end of string)
        character(len = 1), intent(in) :: lead_separator  !! leading separator ( '*' is from begin of string)
        integer, intent(in) :: string_length              !! length of the input string

        integer(kind = int_wp), intent(out) :: error_code
        character(len = *), intent(out) :: sub_string     !! string between separators
        integer, intent(out) :: sub_string_length         !! length of string between separators

        character(len = 10) :: format_string              ! format string for write statement
        integer(kind = int_wp) :: index_start, index_end
        integer(kind = int_wp) :: timer_handle = 0
        logical :: found_lead                             ! flag for finding leading separator
        logical :: found_trail                            ! flag for finding trailing separator

        sub_string = ' '
        sub_string_length = 0
        error_code = 0
        index_start = 1
        index_end = string_length
        found_lead = .false.
        found_trail = .false.

        ! find the position of the leading separator if specified
        if (lead_separator /= '*') then
            do while (index_start <= string_length .and. .not. found_lead)
                if (string(index_start:index_start) == lead_separator) then
                    index_start = index_start + 1
                    found_lead = .true.
                    exit
                endif
                index_start = index_start + 1
            enddo

            if (.not. found_lead) then
                error_code = 1
                goto 9999
            endif

        endif

        ! skip leading blanks/whitespaces
        do while (index_start < string_length .and. string(index_start:index_start) == ' ')
            index_start = index_start + 1
        end do

        if (index_start > string_length) then
            error_code = 2
            goto 9999
        endif

        ! Find the position of the trailing separator if specified
        if (trail_separator /= '*') then
            do while (index_end > index_start .and. .not. found_trail)
                if (string(index_end:index_end) == trail_separator) then
                    index_end = index_end - 1
                    found_trail = .true.
                    exit
                endif
                index_end = index_end - 1
            end do

            if (.not. found_trail) then
                error_code = 3
                goto 9999
            endif

        endif

        ! Skip trailing whitespace
        do while (index_end >= index_start .and. string(index_end:index_end) == ' ')
            index_end = index_end - 1
        end do

        if (index_end <= index_start) then
            error_code = 4
            goto 9999
        endif

        sub_string_length = index_end - index_start + 1
        call create_format_string (format_string, sub_string_length)
        write (sub_string(1:sub_string_length), format_string) string(index_start:index_end)

        9999 continue

    end subroutine extract_string_by_separator

    subroutine create_format_string (format_string, sub_string_length)
        ! Generates a dynamic format string for a character string of a given length.
        character(len = *) format_string
        integer(kind = int_wp), intent(in) :: sub_string_length ! length of the string to be formatted
        format_string = ' '
        write (format_string, '(''(a'',i3.3,'')'')') sub_string_length
    end subroutine create_format_string

    subroutine extract_integer (file_unit, group, keyword, integer_value, is_found)
        !! initialize an integer variable (integer_value) by reading a value associated with a specific keyword
        !! (keyword) from a specified group (group) in a data source identified by the logical unit number (file_unit).

        integer(kind = int_wp), intent(in) :: file_unit    ! Logical unit number
        character(len = *), intent(in) :: group, keyword   ! Group and keyword names
        integer(kind = int_wp), intent(out) :: integer_value      ! Integer value to be initialized
        logical, optional, intent(out) :: is_found         ! Flag indicating if keyword was found

        character(len = 256) :: value_str                  ! String to hold the retrieved value
        integer(kind = int_wp) :: error_code               ! Error code from reading the value

        call extract_value_from_group (file_unit, group, keyword, value_str)
        if (value_str /= ' ') then
            if (present(is_found)) is_found = .true.
            read(value_str, '(i256)', iostat = error_code) integer_value
            if (error_code /= 0) then
                integer_value = -999
            endif
        else
            if (present(is_found)) is_found = .false.
            integer_value = -999
        endif

    end subroutine extract_integer

    subroutine extract_real (file_unit, group, keyword, real_value, is_found)
        !! Initializes a real value from a specified group and keyword in a file.

        integer(kind = int_wp), intent(in) :: file_unit   ! File unit number
        character(len = *), intent(in) :: group, keyword ! Group and keyword to search
        real(kind = real_wp), intent(out) :: real_value       ! Real value to be initialized
        logical, optional, intent(out) :: is_found     ! Flag indicating if keyword was found

        character(len = 256) :: value_str                ! String to hold the retrieved value
        integer(kind = int_wp) :: error_code              ! Error code from reading the value

        call extract_value_from_group (file_unit, group, keyword, value_str)
        if (value_str /= ' ') then
            if (present(is_found)) is_found = .true.
            read(value_str, '(f256.0)', iostat = error_code) real_value
            if (error_code /= 0) then
                real_value = -999.
            endif
        else
            if (present(is_found)) is_found = .false.
            real_value = -999.
        endif

    end subroutine extract_real

    subroutine extract_logical  (file_unit, group, keyword, logical_value, is_found)
        ! Initializes a logical value from a specified group and keyword in a file.
        integer(kind = int_wp), intent(in) :: file_unit   ! File unit number
        character(len = *), intent(in) :: group, keyword ! Group and keyword to search
        logical, intent(out) :: logical_value                 ! Logical value to be initialized
        logical, optional, intent(out) :: is_found     ! Flag indicating if keyword was found

        character(len = 256) :: value_str                ! String to hold the retrieved value

        call extract_value_from_group (file_unit, group, keyword, value_str)

        logical_value = .false.
        if (value_str == ' ') then
            if (present(is_found)) is_found = .false.
            return
        endif

        ! Set is_found to true as value_str is not empty
        if (present(is_found)) is_found = .true.

        ! Check if the value_str corresponds to logical true values
        if (string_equals('true ', value_str) &
                .or. string_equals('yes  ', value_str) &
                .or. string_equals('1    ', value_str)) then
            logical_value = .true.
        endif
    end subroutine extract_logical

    subroutine read_substances(is_allocated, input_file, num_active_substance, num_total_substances, num_constants, &
            num_outputs, substance_names, substance_units, constant_names, constant_values, output_names, &
            output_descriptions, error_status, error_message)

        use m_monsys
        use rd_token, only : ilun, lch, lstack, gettoken, lunut, npos, cchar

        implicit none

        logical, intent (in) :: is_allocated                                !< only store actual data when arrays are is_allocated
        character(len = 256), intent (in) :: input_file                     !< input filename
        integer(kind = int_wp), intent (inout) :: num_active_substance        !< Number of active systems
        integer(kind = int_wp), intent (inout) :: num_total_substances         !< Number of systems
        integer(kind = int_wp), intent (inout) :: num_constants             !< Number of constants used
        integer(kind = int_wp), intent (inout) :: num_outputs               !< Number of outputs
        character(20), intent (inout) :: substance_names(*)                    !< substance names
        character(20), intent (inout) :: substance_units(*)                    !< substance names
        character(20), intent (inout) :: constant_names(*)                  !< constant names
        real(kind = real_wp), intent (inout) :: constant_values(*)          !< constant values
        character(20), intent (inout) :: output_names(*)                    !< output names
        character(80), intent (inout) :: output_descriptions(*)             !< output descriptions
        integer(kind = int_wp), intent (out) :: error_status                !< error status
        character(256), intent (out) :: error_message                       !< error message

        character(20) :: inactive_substance_names(10000)   !< inactive substance names
        character(20) :: inactive_substance_unit(10000)   !< inactive substance names

        integer(kind = int_wp), parameter :: num_tags = 4
        integer(kind = int_wp), parameter :: num_attributes = 8
        integer(kind = int_wp) :: substance_index
        integer(kind = int_wp) :: tag_index
        integer(kind = int_wp) :: attribute_index
        logical :: is_active
        character(20) :: start_tag (num_tags)
        character(20) :: end_tag   (num_tags)
        character(20) :: attribute(num_attributes)

        character(20) :: current_substance
        character(20) :: current_parameter
        character(20) :: current_output
        character(20) :: current_process
        character(20) :: current_tag
        character(20) :: current_attribute
        character(256) :: current_string
        real(kind = real_wp) :: real_data

        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: iostat

        integer(kind = int_wp) :: temp_int
        real(kind = real_wp) :: temp_real
        integer(kind = int_wp) :: data_type

        ! Initialize tags and attributes
        data start_tag  / 'substance           ', 'parameter           ', &
                'output              ', 'active-processes    ' /
        data end_tag    / 'end-substance       ', 'end-parameter       ', &
                'end-output          ', 'end-active-processes' /
        data attribute / 'active              ', 'inactive            ', &
                'description         ', 'concentration-unit  ', &
                'waste-load-unit     ', 'unit                ', &
                'value               ', 'name                ' /

        ! Open input file
        call getmlu(lunut)
        ilun = 0
        lch (1) = input_file
        open (newunit = ilun(1), file = lch(1), status = 'old', iostat = error_status)
        if(error_status /= 0) then
            error_message = 'Error reading file: ' // trim(lch(1))
            return
        endif
        npos = 1000
        cchar = '#'

        ! Initialize counters
        num_active_substance = 0
        num_total_substances = 0
        num_constants = 0
        num_outputs = 0
        error_message = 'Error: Something went wrong during the reading of the substance file.'

        do
            if (gettoken (current_tag, temp_int, temp_real, data_type, error_status) /= 0) exit
            if (data_type /= 1) then
                error_status = 101
                error_message = 'Error: Expected a keyword, but found a real or integer while reading file: ' // trim(lch(1))
                return
            endif

            tag_index = index_in_array(current_tag, start_tag)
            select case (tag_index)

            case (1)

                ! case 1  substance

                !  substance 'name' active/inactive
                !     description        'text'
                !     concentration-unit '(text)'
                !     waste-load-unit    '(text)'
                !  end-substance

                if (gettoken (current_substance, error_status) /= 0) exit
                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index == 1) then
                    num_active_substance = num_active_substance + 1
                    if (is_allocated) substance_names(num_active_substance) = current_substance
                    is_active = .true.
                else if (attribute_index == 2) then
                    num_total_substances = num_total_substances + 1
                    if (is_allocated) inactive_substance_names(num_total_substances) = current_substance
                    is_active = .false.
                else
                    error_status = 101
                    error_message = 'Expected attribute ''active'' or ''inactive'' for substance ' // trim(current_substance) // ', but read: ' // trim(current_attribute)
                    exit
                endif

                !  substance attributes

                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index /= 3) then
                    error_status = 101
                    error_message = 'Expected attribute ''description'' for substance ' // trim(current_substance) // ', but read: ' // trim(current_attribute)
                    exit
                endif
                if (gettoken (current_string, error_status) /= 0) exit
                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index /= 4) then
                    error_status = 101
                    error_message = 'Expected attribute ''concentration-unit'' for substance ' // trim(current_substance) // ', but read: ' // trim(current_attribute)
                    exit
                endif
                if (gettoken (current_string, error_status) /= 0) exit
                if (is_active) then
                    if (is_allocated) substance_units(num_active_substance) = current_string
                else
                    if (is_allocated) inactive_substance_unit(num_total_substances) = current_string
                endif
                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index /= 5) then
                    error_status = 101
                    error_message = 'Expected attribute ''waste-load-unit'' for substance ' // trim(current_substance) // ', but read: ' // trim(current_attribute)
                    exit
                endif
                if (gettoken (current_string, error_status) /= 0) exit

                !  substance end-tag

                if (gettoken (current_tag, error_status) /= 0) exit
                tag_index = index_in_array(current_tag, end_tag)
                if (tag_index /= 1) then
                    error_status = 101
                    error_message = 'Expected end-tag ''end-substance'' for substance ' // trim(current_substance) // ', but read: ' // trim(current_attribute)
                    exit
                endif

            case (2)

                ! case 2  parameter

                !  parameter 'name'
                !     description   'text'
                !     unit          'text'
                !     value          0.0E+00
                !  end-parameter

                if (gettoken (current_parameter, error_status) /= 0) exit
                num_constants = num_constants + 1
                if (is_allocated) constant_names(num_constants) = current_parameter

                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index /= 3) then
                    error_status = 101
                    error_message = 'Expected attribute ''description'' for parameter ' // trim(current_parameter) // ', but read: ' // trim(current_attribute)
                    exit
                endif
                if (gettoken (current_string, error_status) /= 0) exit

                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index /= 6) then
                    error_status = 101
                    error_message = 'Expected attribute ''unit'' for parameter ' // trim(current_parameter) // ', but read: ' // trim(current_attribute)
                    exit
                endif
                if (gettoken (current_string, error_status) /= 0) exit

                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index /= 7) then
                    error_status = 101
                    error_message = 'Expected attribute ''value'' for parameter ' // trim(current_parameter) // ', but read: ' // trim(current_attribute)
                    exit
                endif
                if (gettoken (real_data, error_status) /= 0) then
                    error_message = 'Error: Couldn''t read value for parameter ' // trim(current_parameter) // ' in the substance file'
                    exit
                endif
                if (is_allocated) constant_values(num_constants) = real_data
                !  parameter end-tag

                if (gettoken (current_tag, error_status) /= 0) exit
                tag_index = index_in_array(current_tag, end_tag)
                if (tag_index /= 2) then
                    error_status = 101
                    error_message = 'Expected end-tag ''end-parameter'' for parameter ' // trim(current_parameter) // ', but read: ' // trim(current_attribute)
                    exit
                endif

            case(3)

                !  case 3  output

                !  output 'MrtToEColi'
                !     description   'overall mortality rate EColi'
                !  end-output

                if (gettoken (current_output, error_status) /= 0) exit
                num_outputs = num_outputs + 1
                if (is_allocated) output_names(num_outputs) = current_output

                if (gettoken (current_attribute, error_status) /= 0) exit
                attribute_index = index_in_array(current_attribute, attribute)
                if (attribute_index /= 3) then
                    error_status = 101
                    error_message = 'Expected attribute ''description'' for output ' // trim(current_output) // ', but read: ' // trim(current_attribute)
                    exit
                endif
                if (gettoken (current_string, error_status) /= 0) exit
                if (is_allocated) output_descriptions(num_outputs) = current_string

                !  output end-tag

                if (gettoken (current_tag, error_status) /= 0) exit
                tag_index = index_in_array(current_tag, end_tag)
                if (tag_index /= 3) then
                    error_status = 101
                    error_message = 'Expected end-tag ''end-output'' for output ' // trim(current_output) // ', but read: ' // trim(current_attribute)
                    exit
                endif

            case (4)
                !  case 4  active-processes
                !
                !  active-processes
                !     name  'EColiMrt' 'Mortality EColi bacteria'
                !     name  'Extinc_VLG' 'Extinction of visible-light (370-680nm) DLWQ-G'
                !     name  'DynDepth' 'dynamic calculation of the depth'
                !  end-active-processes

                !     first add some default processes
                num_constants = num_constants + 1
                if (is_allocated) constant_names(num_constants) = 'ONLY_ACTIVE'
                if (is_allocated) constant_values(num_constants) = 1.0e0
                num_constants = num_constants + 1
                if (is_allocated) constant_names(num_constants) = 'ACTIVE_DynDepth'
                if (is_allocated) constant_values(num_constants) = 1.0e0

                do
                    if (gettoken (current_attribute, error_status) /= 0) exit
                    attribute_index = index_in_array(current_attribute, attribute)
                    if (attribute_index /= 8) then
                        current_tag = current_attribute
                        tag_index = index_in_array(current_tag, end_tag)
                        if (tag_index /= 4) then
                            error_status = 102
                            error_message = 'Expected keyword ''name'' or end-tag ''end-active-processes'' to close active-processes list, but read: ' // trim(current_attribute)
                            exit
                        endif
                        exit
                    endif
                    if (gettoken (current_process, error_status) /= 0) exit
                    num_constants = num_constants + 1
                    if (is_allocated) constant_names(num_constants) = 'ACTIVE_' // current_process
                    if (gettoken (current_string, error_status) /= 0) exit
                end do
                exit

                !  case default: no valid tag found!

            case default
                error_status = 100
                error_message = 'expected tag ''substance'', ''parameter'', ''output'' or ''active-processes'', but read: ' // current_tag
                exit
            end select
        end do

        if (error_status == 3) then
            ! end of file
            error_status = 0
        endif
        !  append inactive substances to substances list
        if (is_allocated) then
            do substance_index = 1, num_total_substances
                substance_names(num_active_substance + substance_index) = inactive_substance_names(substance_index)
                substance_units(num_active_substance + substance_index) = inactive_substance_unit(substance_index)
            end do
        end if
        num_total_substances = num_total_substances + num_active_substance

        do i = 1, lstack
            close (unit = ilun(i), err = 1)
            1    continue
        end do

        return
    end subroutine read_substances

end module data_processing
