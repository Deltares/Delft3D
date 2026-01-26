!!  Copyright (C)  Stichting Deltares, 2012-2026.
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

module test_string_module
    use string_module
    use ftnunit
    use precision_basics, only: comparereal, sp, dp

    implicit none

    private
    public:: tests_string_module

contains
subroutine tests_string_module
    call test( test_string_case,  'Test upper and lower string conversion' )
    call test( test_string_token, 'Test tokenizer routine' )
    call test( test_string_compare, 'Test string comparison' )
    call test( test_string_count_words, 'Test word counting' )
    call test( test_string_split, 'Test splitting a string into an array of words' )
    call test( test_remove_chars, 'Test Removing characters given set' )
    call test( test_convert_to_logical, 'Test converting string to logical' )
    call test( test_convert_to_real, 'Test converting string to real' )
end subroutine tests_string_module

subroutine count_differences( string1, string2, number )
    character(len=*), intent(in) :: string1, string2
    integer, intent(out)         :: number

    integer                      :: i

    if ( len(string1) /= len(string2) ) then
        number = -1
        return
    endif

    number = 0
    do i = 1,len(string1)
        if ( string1(i:i) /= string2(i:i) ) then
            number = number + 1
        endif
    enddo
end subroutine count_differences

subroutine test_string_case
    character(len=256) :: full_ascii_table_lower
    character(len=256) :: full_ascii_table_upper
    character(len=256) :: string
    integer            :: i
    integer            :: number_differences

    do i = 1,len(full_ascii_table_lower)
        full_ascii_table_lower(i:i) = achar(i)
        full_ascii_table_upper(i:i) = achar(i)
    enddo

    !
    ! Test the subroutine versions
    !
    ! TODO: use the length argument
    !
    string = full_ascii_table_lower
    call str_lower(string)
    call count_differences( string, full_ascii_table_lower, number_differences )
    call assert_equal( number_differences, 26, "The number of different characters should have been 26 (lowercase)" )
    call count_differences( string, full_ascii_table_upper, number_differences )
    call assert_equal( number_differences, 26, "The number of different characters should have been 26 (uppercase)" )

    string = full_ascii_table_lower
    call str_upper(string)
    call count_differences( string, full_ascii_table_lower, number_differences )
    call assert_equal( number_differences, 26, "The number of different characters should have been 26 (lowercase)" )
    call count_differences( string, full_ascii_table_upper, number_differences )
    call assert_equal( number_differences, 26, "The number of different characters should have been 26 (uppercase)" )

    !         123456789.123456789.123456789.1234
    string = 'The Number Of Different Characters'
    call count_differences( string, str_tolower(string), number_differences )
    call assert_equal( number_differences, 5, "The number of different characters should have been 5 (lowercase)" )
    !
    ! No equivalent for uppercase?
    !call count_differences( string, str_toupper(string), number_differences )
    !call assert_equal( number_differences, 29, "The number of different characters should have been 29 (uppercase)" )
end subroutine test_string_case

subroutine test_string_token
    character(len=50)               :: string = 'The Number Of Words and "Quoted words"'
    character(len=50)               :: token
    character(len=20), dimension(6) :: word   = ['The                 ', &
                                                 'Number              ', &
                                                 'Of                  ', &
                                                 'Words               ', &
                                                 'and                 ', &
                                                 'Quoted words        ']
    integer                         :: i

    !
    ! Note: if the quote character is not given, quotation is ignored
    !
    do i = 1,size(word)
        call str_token( string, token, '"' )
        call assert_equal( token, word(i), "The returned token is incorrect (default characters)" )
    enddo

    !
    ! Use our own separator characters
    !
    string = 'The/Number^Of@Words!and$*Quoted words*'

    do i = 1,size(word)
        call str_token( string, token, "*", "/^@!$" )
        call assert_equal( token, word(i), "The returned token is incorrect (user-defined characters)" )
    enddo

    !
    ! Incorrectly quoted words
    !
    string = '"The Number Of Words and Quoted words'

    call str_token( string, token, '"' )
    call assert_equal( token, '"The Number Of Words and Quoted words             ', "The returned token is incorrect (user-defined characters)" )

end subroutine test_string_token

subroutine test_string_compare
    character(len=50)               :: string_mixed     = 'The Number Of Words and "Quoted words"'
    character(len=50)               :: string_lower     = 'the number of words and "quoted words"'
    character(len=50)               :: string_truncated = 'the number'
                                                          !1234567890
    logical                         :: are_equal

    !
    ! Comparison ignoring case ...
    !
    are_equal = strcmpi( string_mixed, string_lower )
    call assert_true( are_equal, "The strings should be considered equal (mixed/lower)" )

    are_equal = strcmpi( string_mixed, string_truncated, 10 )
    call assert_true( are_equal, "The strings should be considered equal (mixed/truncated)" )

    are_equal = strcmpi( string_mixed, string_truncated, 15 )
    call assert_false( are_equal, "The strings should not be considered equal (mixed/truncated)" )

    are_equal = strcmpi( string_lower, string_truncated, 15 )
    call assert_false( are_equal, "The strings should not be considered equal (lower/truncated)" )

end subroutine test_string_compare

subroutine test_string_count_words
    character(len=50) :: string = 'The Number Of Words and "Quoted words"'
    integer           :: number

    !
    ! Note: only whitespace considered, no quotation
    !
    number = count_words( string )
    call assert_equal( number, 7, "The number of words must be based on whitespace alone" )
end subroutine test_string_count_words

subroutine test_string_split
    character(len=50)                            :: string = 'The Number Of Words and "Quoted words"'
    character(len=50), dimension(:), allocatable :: element

    character(len=20), dimension(6) :: word   = ['The                 ', &
                                                 'Number              ', &
                                                 'Of                  ', &
                                                 'Words               ', &
                                                 'and                 ', &
                                                 'Quoted words        ']
    integer                         :: i

    call strsplit( string, 1, element, 1 )

    do i = 1,size(element)
        call assert_equal( element(i), word(i), "The returned element is incorrect" )
    enddo
end subroutine test_string_split

subroutine test_remove_chars
    character(len=50)                            :: string = 'The [Number] __Of Words [And] _"Quoted Words"'
    call remove_chars( string, '[]"_- ')
    call assert_equal( string, 'TheNumberOfWordsAndQuotedWords', "")
end subroutine test_remove_chars

subroutine test_convert_to_logical
    character(len=20), dimension(:), allocatable :: strings
    logical           :: value
    integer           :: ierr
    integer           :: i
    !
    ! True values
    !
    strings = ['TRUE', 'TRUE with comments', 'T', '1', '1.0', '2.0', '-1e5', 'Yes', 'Y', 'ja', 'j']
    do i = 1, size(strings)
       call convert_to_logical( strings(i), value, ierr )
       call assert_true( value, "The value should be true when parsing: "//trim(strings(i)) )
       call assert_equal( ierr, no_error, "No error should have occurred when parsing: "//trim(strings(i)) )
    end do
    !
    ! False values
    !
    strings = ['FALSE', 'FALSE with comments', 'F', '0', '0.0', '0e0', 'No', 'nee', 'n' ]
    do i = 1, size(strings)
       call convert_to_logical( strings(i), value, ierr )
       call assert_false( value, "The value should be false when parsing: "//trim(strings(i)) )
       call assert_equal( ierr, no_error, "No error should have occurred when parsing: "//trim(strings(i)) )
    end do
    !
    ! Invalid value
    !
    strings = [' ', 'INVALID', '1.0_dp', 'TRUEwith_comments_attached' ]
    do i = 1, size(strings)
       call convert_to_logical( strings(i), value, ierr )
       if (i == 1) then
          call assert_equal( ierr, empty_string, "Empty-string error should have occurred when parsing: "//trim(strings(i)) )
       else
          call assert_equal( ierr, conversion_error, "Conversion error should have occurred when parsing: "//trim(strings(i)) )
       end if
    end do
end subroutine test_convert_to_logical

subroutine test_convert_to_real
    character(len=20), dimension(:), allocatable :: strings
    real(sp), dimension(:), allocatable :: values_sp
    real(dp), dimension(:), allocatable :: values_dp
    real(sp) :: value_sp
    real(dp) :: value_dp
    integer          :: ierr
    integer          :: i
    character(len=200) :: errmsg
    !
    ! Valid real values
    !
    strings = ['1.0', '-2.5', '3.14159', '0.0', '1e3', '-4.2E-2', '  7.5  ', '2.5 with comments', '42', '.42', '0.025', '2.3d2' ]
    values_sp = [ 1.0_sp, -2.5_sp, 3.14159_sp, 0.0_sp, 1e3_sp, -4.2e-2_sp, 7.5_sp, 2.5_sp, 42.0_sp, 0.42_sp, 0.025_sp, 2.3e2_sp ]
    values_dp = [ 1.0_dp, -2.5_dp, 3.14159_dp, 0.0_dp, 1e3_dp, -4.2e-2_dp, 7.5_dp, 2.5_dp, 42.0_dp, 0.42_dp, 0.025_dp, 2.3e2_dp ]
    do i = 1, size(strings)
       ! single precision
       value_sp = -999.0_sp
       call convert_to_real( strings(i), value_sp, ierr )
       call assert_equal( ierr, no_error, "No error should have occurred when parsing: "//trim(strings(i)) )
       write(errmsg, '(A,G0,A,G0)', iostat=ierr ) "Obtained value ", value_sp, " equals expected value ", values_sp(i)
       call assert_true( comparereal(value_sp, values_sp(i)) == 0, trim(errmsg) )
       
       ! double precision
       value_dp = -999.0_dp
       call convert_to_real( strings(i), value_dp, ierr )
       call assert_equal( ierr, no_error, "No error should have occurred when parsing: "//trim(strings(i)) )
       write(errmsg, '(A,G0,A,G0)', iostat=ierr ) "Obtained value ", value_dp, " equals expected value ", values_dp(i)
       call assert_true( comparereal(value_dp, values_dp(i)) == 0, trim(errmsg) )
    end do
    
    !
    ! Invalid real values
    !
    strings = [' ', 'INVALID', '1.0_dp', '3.14pi' ]
    do i = 1, size(strings)
       call convert_to_real( strings(i), value_dp, ierr )
       if (i == 1) then
          call assert_equal( ierr, empty_string, "Empty-string error should have occurred when parsing: '"//trim(strings(i))//"'" )
       else
          call assert_equal( ierr, conversion_error, "Conversion error should have occurred when parsing: "//trim(strings(i)) )
       end if
    end do
end subroutine test_convert_to_real

end module test_string_module
