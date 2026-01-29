module iso_c_utils
  use iso_c_binding
  implicit none

  ! always use max stringlen for c arrays otherwise you have to specify lengths by hand
  integer(c_int), parameter :: MAXSTRINGLEN = 1024
contains
  ! Utility functions, move these to interop module
  ! Make functions pure so they can be used as input arguments.
  integer(c_int) pure function strlen(char_array)
    character(c_char), intent(in) :: char_array(MAXSTRINGLEN)
    integer :: i
    strlen = 0
    do i = 1, size(char_array)
       if (char_array(i) == C_NULL_CHAR) then
          strlen = i-1
          exit
       end if
    end do
  end function strlen


  integer(c_int) pure function strcmp(char_array1, char_array2)
    character(c_char), intent(in) :: char_array1(MAXSTRINGLEN)
    character(c_char), intent(in) :: char_array2(MAXSTRINGLEN)

    character(len=strlen(char_array1)) :: string1
    character(len=strlen(char_array2)) :: string2

    string1 = trim(char_array_to_string(char_array1))
    string2 = trim(char_array_to_string(char_array1))
    strcmp = 0
    if (llt(string1,string2)) strcmp = -1
    if (lgt(string1,string2)) strcmp = 1

  end function strcmp



  subroutine strcpy(str1, str2)
    character(c_char), intent(in) :: str1(MAXSTRINGLEN)
    character(c_char), intent(out) :: str2(MAXSTRINGLEN)

    integer :: i
    str2 = ' '
    do i=1,(strlen(str1) + 1)
       str2(i) = str1(i)
    end do
  end subroutine strcpy

  pure function char_array_to_string(char_array) result(string)
    character(c_char),intent(in) :: char_array(MAXSTRINGLEN)
    character(len=strlen(char_array)) :: string
    integer :: i
    do i = 1, strlen(char_array)
       string(i:i) = char_array(i)
    enddo
  end function char_array_to_string

  pure function string_to_char_array(string) result(char_array)
    ! pass only trimmed strings to this one
    character(len=*), intent(in) :: string
    character(kind=c_char,len=1) :: char_array(MAXSTRINGLEN)
    integer :: i
    do i = 1, min(MAXSTRINGLEN, len(string))
       char_array(i) = string(i:i)
    enddo
    char_array(min(MAXSTRINGLEN, len(string)+1)) = C_NULL_CHAR
  end function string_to_char_array

end module iso_c_utils
