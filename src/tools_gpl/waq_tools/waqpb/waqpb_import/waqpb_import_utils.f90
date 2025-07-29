module m_waqpb_import_utils
contains

    !< Checks for read errors and prints an error message if an error occurred
    !< indicating the item that was being read and the line number.
    subroutine check_read_error(io_mes, ierr, linecount, read_item)
        integer, intent(in) :: ierr, linecount, io_mes
        character(len=*), intent(in) :: read_item
        if (ierr /= 0) then
            write (*, '(A, A, A, I0)') 'Error reading ', trim(read_item), ' at line', linecount
            write(io_mes, '(A, A, A, I0)') 'Error reading ', trim(read_item), ' at line', linecount
            write (*, '(A)') 'Please check the input file for correctness.'
            error stop
        end if
    end subroutine check_read_error


    !< Returns a character indicating the type of input based on the value.
    function get_input_type(value) result(typechar)
      real, intent(in) :: value
      character(len=1) :: typechar

      if (abs(value + 999.) < 1e-10) then
         typechar = 'N'
      else if (abs(value + 888.) < 1e-10) then
         typechar = 'G'
      else if (abs(value + 101.) < 1e-10) then
         typechar = 'B'
      else if (abs(value + 11.) < 1e-10) then
         typechar = 'M'
      else if (abs(value + 1.) < 1e-10) then
         typechar = 'O'
      else
         typechar = 'Y'
      end if
    end function get_input_type

    subroutine parse_input_item_line(io_unit, linecount, name, def_value, show_in_plct, description, units)
        integer, intent(in) :: io_unit !< Input unit number to read from
        integer, intent(in) :: linecount !< Line number being read
        character(len=10), intent(out) :: name !< Name of the input item
        real, intent(out) :: def_value !< Default value of the input item
        character(len=1), intent(out) :: show_in_plct !< Indicates if the item should be shown in PLCT ('x') or not (' ')
        character(len=50), intent(out) :: description !< Description of the input item
        character(len=20), intent(out) :: units !< Units of the input item
        
        character(len=255) :: line_buffer !< Buffer to read the line
        character(len=52), parameter :: letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
        character(len=10), parameter :: digits = '0123456789'
        character(len=10), parameter :: plus_minus = '+-'
        integer :: ierr, istart, iend, len_line

        read(io_unit, '(A)', iostat=ierr) line_buffer
        line_buffer = adjustl(line_buffer) !< Remove leading spaces
        istart = scan(line_buffer, letters // digits // plus_minus) !< Find the start of the name
        len_line = len_trim(line_buffer) !< Get the length of the line

        ! field 1 - name (no spaces allowed)
        iend = scan(line_buffer(istart:), ' ') + istart - 1
        if (iend < istart) iend = len_line
        name = line_buffer(istart:iend)

        ! field 2 - default value (real number)
        istart = iend + 1
        istart = scan(line_buffer(istart:), digits // plus_minus) + istart - 1
        iend = scan(line_buffer(istart:), ' ') + istart - 1
        if (iend < istart) iend = len_line
        read(line_buffer(istart:iend), *, iostat=ierr) def_value

        ! field 3 - show in PLCT (character, 'x' or ' ')
        istart = iend + 1
        istart = scan(line_buffer(istart:), 'x' // letters) + istart - 1
        if (line_buffer(istart:istart) == 'x') then
            show_in_plct = 'x'
        else
            show_in_plct = ' '
        end if

        ! field 5 - units (in parentheses)
        iend = index(line_buffer, '(', back = .true.)
        if (iend > 0) then
            units = line_buffer(iend + 1:len_line - 1) !< Extract units from parentheses
            description = line_buffer(istart:iend - 1) !< Extract description before units
        else
            units = ''
            description = adjustl(line_buffer(istart:len_line)) !< No units, take the rest as description
        end if
        print *, 'Parsed input item name = ', trim(name), ', default value = ', def_value, ' show in PLCT = ', show_in_plct, ' description = ', trim(description), ' units = ', trim(units)
        pause

        ! read(io_unit, '(A10, F18.0, A1, A50, A20)', iostat=ierr) name, def_value, show_in_plct, description, units
        ! call check_read_error(io_mes, ierr, linecount, 'input line')

    end subroutine parse_input_item_line

end module m_waqpb_import_utils
