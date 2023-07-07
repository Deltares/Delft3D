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

module test_sorting_algorithms
    use ftnunit
    use sorting_algorithms

    implicit none

contains
subroutine tests_sorting
    call test( test_sort, 'Sorting double precision reals' )
end subroutine tests_sorting

subroutine test_sort
    real(kind=kind(1.0d0)), dimension(1)  :: single_value, sorted_value
    real(kind=kind(1.0d0)), dimension(10) :: values, sorted_values

    integer, dimension(10)                :: work
    integer                               :: i

    !
    ! Check that values are properly ordered
    !
    call random_number( values )

    call sort( size(values), values, sorted_values, work )

    do i = 1,size(values)
        call assert_true( any( values(i) == sorted_values ), "Original value found in sorted array" )
    enddo

    do i = 2,size(values)
        call assert_true( sorted_values(i) >= sorted_values(i-1), "Values are sorted in increasing order" )
    enddo

    !
    ! Check that a single value is properly retained
    !
    call random_number( single_value )

    call sort( size(single_value), single_value, sorted_value, work )

    call assert_true( single_value(1) == sorted_value(1), "Single value is simply retained when sorting" )
end subroutine test_sort

! TODO: INDEXI and INDEXX?
end module test_sorting_algorithms

