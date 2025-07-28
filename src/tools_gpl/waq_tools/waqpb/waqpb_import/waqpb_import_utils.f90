module m_waqpb_import_utils
contains

    !< Checks for read errors and prints an error message if an error occurred
    !< indicating the item that was being read and the line number.
    subroutine check_read_error(io_mes, ierr, linecount, read_item)
        integer, intent(in) :: ierr, linecount, io_mes
        character(len=*), intent(in) :: read_item
        if (ierr /= 0) then
            write (*, *) 'Error reading ', trim(read_item), ' at line', linecount
            write(io_mes, *) 'Error reading ', trim(read_item), ' at line', linecount
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

end module m_waqpb_import_utils
