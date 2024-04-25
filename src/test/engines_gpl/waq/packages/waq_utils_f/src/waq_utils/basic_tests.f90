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

module basic_tests
    use ftnunit

    implicit none

contains
    subroutine tests_zoek
        call test(test_zoek, 'Tests for ZOEK routines - arrays of strings')
    end subroutine tests_zoek

    subroutine test_zoek

        character(len=10), dimension(5) :: names = &
                                           ['Name 1    ',
                                            'Name 2    ',
                                            'Name 3    ',
                                            'Name 4    ',
                                            'NAME 3    ']
        character(len=10) :: lookup
        integer           :: idx

        !
        ! Case-insensitive search
        !
        idx = index_in_array('NAME 3', names)

        call assert_equal(idx, 3, 'Returned location correct (full string, case-insensitive)')

        ! Truncate the search to a short prefix
        idx = index_in_array(lookup(1:4), names)

        call assert_equal(idx, 1, 'Returned location correct (short prefix, case-insensitive))')

    end subroutine test_zoek
end module basic_tests
