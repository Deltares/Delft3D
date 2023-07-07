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

module basic_tests
    use ftnunit
    use m_zoek

    implicit none

contains
subroutine tests_zoek
    call test( test_zoek, 'Tests for ZOEK routines - arrays of strings' )
end subroutine tests_zoek

subroutine test_zoek

    character(len=10), dimension(5) :: names = &
        ['Name 1    ', 'Name 2    ', 'Name 3    ', 'Name 4    ', 'NAME 3    ']
    character(len=10) :: lookup
    integer           :: i, idx, string_length

    !
    ! Case-sensitive search
    !
    lookup        = 'Name 3'
    string_length = len(names)
    call zoekcs( lookup, size(names), names, string_length, idx )

    call assert_equal( idx, 3, 'Returned location correct (full string, case-sensitive)' )

    ! Truncate the search to a short prefix
    string_length = 4
    call zoekcs( lookup, size(names), names, string_length, idx )

    call assert_equal( idx, 1, 'Returned location correct (short prefix, case-sensitive)' )

    ! Unknown string
    lookup        = 'Unknown'
    string_length = len(names)
    call zoekcs( lookup, size(names), names, string_length, idx )

    call assert_equal( idx, -1, 'Returned location correct (unknown string, case-sensitive)' )

    !
    ! Case-insensitive search
    !
    lookup        = 'NAME 3'
    string_length = len(names)
    call zoekns( lookup, size(names), names, string_length, idx )

    call assert_equal( idx, 3, 'Returned location correct (full string, case-insensitive)' )

    ! Truncate the search to a short prefix
    string_length = 4
    call zoekns( lookup, size(names), names, string_length, idx )

    call assert_equal( idx, 1, 'Returned location correct (short prefix, case-insensitive))' )

    ! Unknown string
    lookup        = 'Unknown'
    string_length = len(names)
    call zoekcs( lookup, size(names), names, string_length, idx )

    call assert_equal( idx, -1, 'Returned location correct (unknown string, case-insensitive)' )

    !
    ! Configured case-(in)sensitive search
    !
    do i = 0,1
        call setzmo( i )

        lookup        = 'NAME 3'
        string_length = len(names)
        call zoek( lookup, size(names), names, string_length, idx )

        if ( i == 0 ) then
            call assert_equal( idx, 3, 'Returned location correct (full string, case-insensitive)' )
        else
            call assert_equal( idx, 5, 'Returned location correct (full string, case-sensitive)' )
        endif
    enddo
end subroutine test_zoek
end module basic_tests
