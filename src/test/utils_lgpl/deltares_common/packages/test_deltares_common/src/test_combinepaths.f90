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

module test_combinepaths
    use ftnunit

    implicit none

    private
    public :: tests_combinepaths

contains
subroutine tests_combinepaths
    call test( test_combine, 'Combining path names' )
end subroutine tests_combinepaths

!
! The comments in the source file define a nice set of test cases - use these to test the code
!
subroutine test_combine
    character(len=20), dimension(9) :: firstname, secondname, expected

    integer                               :: i

    !              firstname             secondname IN                          secondname OUT
    firstname(1) = 'file1.txt'         ; secondname(1) = 'file2.txt'         ;  expected(1) = 'file2.txt'
    firstname(2) = 'dir\file1.txt'     ; secondname(2) = 'file2.txt'         ;  expected(2) = 'dir\file2.txt'
    firstname(3) = 'dir/file1.txt'     ; secondname(3) = 'file2.txt'         ;  expected(3) = 'dir/file2.txt'
    firstname(4) = 'c:\dir\file1.txt'  ; secondname(4) = 'file2.txt'         ;  expected(4) = 'c:\dir\file2.txt'
    firstname(5) = '//dir/file1.txt'   ; secondname(5) = 'file2.txt'         ;  expected(5) = '//dir/file2.txt'
    firstname(6) = 'c:\dir\file1.txt'  ; secondname(6) = '..\dir2\file2.txt' ;  expected(6) = 'c:\dir\..\dir2\file2.txt'
    firstname(7) = '//dir/file1.txt'   ; secondname(7) = '../dir2/file2.txt' ;  expected(7) = '//dir/../dir2/file2.txt'
    firstname(8) = 'c:\dir\file1.txt'  ; secondname(8) = 'd:\dir2\file2.txt' ;  expected(8) = 'd:\dir2\file2.txt'
    firstname(9) = '//dir/file1.txt'   ; secondname(9) = '//dir2/file2.txt'  ;  expected(9) = '//dir2/file2.txt'

    !
    ! Check each case
    !
    do i = 1,size(firstname)
        call combinepaths( firstname(i), secondname(i) )

        call assert_equal( secondname(i), expected(i), "Constructed name is as expected" )
    enddo
end subroutine test_combine

end module test_combinepaths

