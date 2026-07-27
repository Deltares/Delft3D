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
    use m_combinepaths, only: combinepaths
    
    character(len=30), dimension(12) :: firstname, secondname, expected
    character(len=:), allocatable   :: combined_name

    integer                               :: i

    !              firstname                    secondname IN                                 secondname OUT
    firstname(1) = 'file1.txt'                ; secondname(1) = 'file2.txt'                ;  expected(1) = 'file2.txt'
    firstname(2) = 'dir\file1.txt'            ; secondname(2) = 'file2.txt'                ;  expected(2) = 'dir\file2.txt'
    firstname(3) = 'dir/file1.txt'            ; secondname(3) = 'file2.txt'                ;  expected(3) = 'dir/file2.txt'
    firstname(4) = 'c:\dir\file1.txt'         ; secondname(4) = 'file2.txt'                ;  expected(4) = 'c:\dir\file2.txt'
    firstname(5) = '\\agent\dir\file1.txt'    ; secondname(5) = 'file2.txt'                ;  expected(5) = '\\agent\dir\file2.txt'
    firstname(6) = '/dir/file1.txt'           ; secondname(6) = 'file2.txt'                ;  expected(6) = '/dir/file2.txt'
    firstname(7) = 'c:\dir\file1.txt'         ; secondname(7) = '..\dir2\file2.txt'        ;  expected(7) = 'c:\dir\..\dir2\file2.txt'
    firstname(8) = '\\agent\dir\file1.txt'    ; secondname(8) = '..\dir2\file2.txt'        ;  expected(8) = '\\agent\dir\..\dir2\file2.txt'
    firstname(9) = '/dir/file1.txt'           ; secondname(9) = '../dir2/file2.txt'        ;  expected(9) = '/dir/../dir2/file2.txt'
    firstname(10) = 'c:\dir\file1.txt'        ; secondname(10) = 'd:\dir2\file2.txt'       ;  expected(10) = 'd:\dir2\file2.txt'
    firstname(11) = '/dir/file1.txt'          ; secondname(11) = '/dir2/file2.txt'         ;  expected(11) = '/dir2/file2.txt'
    firstname(12) = '\\agent\dir\file1.txt'   ; secondname(12) = '\\agent2\dir2\file2.txt' ;  expected(12) = '\\agent2\dir2\file2.txt'

    !
    ! Check each case
    !
    do i = 1,size(firstname)
        combined_name = combinepaths( firstname(i), secondname(i) )

        call assert_equal( combined_name, expected(i), "Constructed name is as expected" )
    enddo
end subroutine test_combine

end module test_combinepaths

