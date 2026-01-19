module m_waqpb_import_utils
    use m_string_utils, only: split_string_non_empty, join_strings
    implicit none
contains

    !< Checks for read errors and prints an error message if an error occurred
    !< indicating the item that was being read and the line number.
    subroutine check_read_error(io_mes, ierr, linecount, read_item)
        integer, intent(in) :: ierr, linecount, io_mes
        character(len=*), intent(in) :: read_item
        if (ierr /= 0) then
            write (*, '(A, A, A, I0)') 'Error reading ', trim(read_item), ' at line ', linecount
            write(io_mes, '(A, A, A, I0)') 'Error reading ', trim(read_item), ' at line ', linecount
            write (*, '(A)') 'Please check the input file for correctness.'
            error stop
        end if
    end subroutine check_read_error


    !< Returns a character indicating the type of input based on the value.
    function get_input_type(value) result(typechar)
      real, intent(in) :: value
      character(len=1) :: typechar

      if (abs(value + 999.) < 1e-6) then
         typechar = 'N'
      else if (abs(value + 888.) < 1e-6) then
         typechar = 'G'
      else if (abs(value + 101.) < 1e-6) then
         typechar = 'B'
      else if (abs(value + 11.) < 1e-6) then
         typechar = 'M'
      else if (abs(value + 1.) < 1e-6) then
         typechar = 'O'
      else
         typechar = 'Y'
      end if
    end function get_input_type

    subroutine parse_item_line(io_unit, log_unit, linecount, &
                                     name, def_value, show_in_plct, &
                                     description, units, is_input)
        integer, intent(in) :: io_unit !< Input unit number to read from
        integer, intent(in) :: log_unit !< Log unit number
        integer, intent(in) :: linecount !< Line number being read
        character(len=10), intent(out) :: name !< Name of the input item
        real, intent(out) :: def_value !< Default value of the input item
        character(len=1), intent(out) :: show_in_plct !< Indicates if the item should be shown in PLCT ('x') or not (' ')
        character(len=50), intent(out) :: description !< Description of the input item
        character(len=20), intent(out) :: units !< Units of the input item
        logical, intent(in), optional :: is_input !< Optional flag to indicate if the item is an input (.true.) or output (.false.)

        character(len=255) :: line_buffer !< Buffer to read the line
        character(len=255) :: temp_string !< Temporary string for processing
        integer :: ierr, idx_field
        character(:), dimension(:), allocatable :: substrings
        logical :: item_is_input

        if ( present(is_input) ) then
            item_is_input = is_input
        else
            item_is_input = .true.
        endif

        read(io_unit, '(A)', iostat=ierr) line_buffer
        if (ierr /= 0) then
            write (*, '(A, I0)') 'Error reading line ', linecount, ' from input file.'
            error stop
        end if

        ! first read the name (or ID) of the item
        temp_string = adjustl(line_buffer(1:11))
        if (len_trim(temp_string) > 10) then
            write (*, '(A, A, A, I0)') 'Error: item name "', trim(temp_string), '" exceeds 10 characters at line ', linecount
            error stop
        end if
        name = temp_string !< First 10 characters are the name

        substrings = split_string_non_empty(adjustl(line_buffer(11:len_trim(line_buffer))), ' ') !< Split the line into substrings based on spaces

        idx_field = 1
        if (item_is_input) then
            read(substrings(idx_field), *, iostat=ierr) def_value
            call check_read_error(log_unit, ierr, linecount, 'default value')
            idx_field = idx_field + 1
        else
            def_value = 0.0 !< Unused default value is set to 0.0
        end if

        if (trim(adjustl(substrings(idx_field))) == 'x') then
            show_in_plct = 'x'
            idx_field = idx_field + 1 !< Start of description
        else
            show_in_plct = ' '
        end if

        temp_string = join_strings(substrings(idx_field:size(substrings) - 1), ' ') !< Join the remaining substrings except the last one for description
        if (len_trim(temp_string) > 50) then
            write (*, '(A, A, A, I0)') 'Error: description "', trim(temp_string), '" exceeds 50 characters at line ', linecount
            error stop
        end if
        description = temp_string !< Remaining substrings except units form the description

        temp_string = substrings(size(substrings)) !< Last substring is the units
        if (len_trim(temp_string) > 20) then
            write (*, '(A, A, A, I0)') 'Error: units "', trim(temp_string), '" exceed 20 characters at line ', linecount
            error stop
        end if
        units = temp_string !< Last substring is the units

    end subroutine parse_item_line

end module m_waqpb_import_utils
