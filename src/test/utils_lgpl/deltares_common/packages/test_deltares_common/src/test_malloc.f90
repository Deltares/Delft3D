!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

module test_m_alloc
   use ftnunit
   use m_alloc

   implicit none
   private
   public tests_alloc

   contains

   !> Allocate and fill an uninitialized array with 1 ... array_size
   subroutine initialize_int_iota(array, array_size)
      integer, allocatable, dimension(:), intent(inout) :: array
      integer,                            intent(in   ) :: array_size

      integer :: i
      call assert_false(allocated(array), 'array was already allocated')
      allocate(array(array_size))
      do i = 1, array_size
         array(i) = i
      end do
   end subroutine initialize_int_iota

   !> Make sure that the status returned by realloc is 0
   subroutine check_status(status)
      integer, intent(in) :: status
      call assert_equal(status, 0, 'realloc returned with nonzero error status')
   end subroutine check_status

   subroutine tests_alloc()
      call test(test_realloc_int_increase_size, 'realloc: increase int array size')
      call test(test_realloc_int_decrease_size, 'realloc: decrease int array size')
      call test(test_realloc_wrong_size, 'realloc: provide wrong array size')
      call test(test_realloc_int_equal_size, 'realloc: int array with initial size')
      call test(test_realloc_int_unallocated, 'realloc: unallocated int array')
      call test(test_realloc_int_fill, 'realloc: grow int array and fill new elements')
      call test(test_realloc_int_keep_existing, 'realloc: do not keep elements of int array')
      call test(test_realloc_int_keep_existing_equal_size, 'realloc: int array to same size but do not keep elements')
      call test(test_realloc_int_no_fill_equal_size, 'realloc: int array to same size without fill')
      call test(test_realloc_int_boundaries, 'realloc: provide custom array boundaries')
      call test(test_realloc_int_shift_copied_elements, 'realloc: int array shift copied elements')
   end subroutine tests_alloc

   subroutine test_realloc_int_increase_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: i

      initial_size = 5
      call initialize_int_iota(array, initial_size)

      new_size = 10
      call realloc(array, new_size, stat=status)

      call check_status(status)
      call assert_equal(ubound(array, dim=1), new_size, 'new array has wrong bounds')
      call assert_equal(array(1:initial_size), [(i, i = 1, initial_size)], 'elements were not properly copied')
   end subroutine test_realloc_int_increase_size

   subroutine test_realloc_int_decrease_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: i

      initial_size = 5
      call initialize_int_iota(array, initial_size)

      new_size = 3
      call realloc(array, new_size, stat=status)

      call check_status(status)
      call assert_equal(ubound(array, dim=1), new_size, 'new array has wrong bounds')
      call assert_equal(array(1:new_size), [(i, i = 1, new_size)], 'elements were not properly copied')
   end subroutine test_realloc_int_decrease_size

   subroutine test_realloc_wrong_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_size

      initial_size = 3
      call initialize_int_iota(array, initial_size)

      new_size = -2
      call realloc(array, new_size, stat=status)

      call check_status(status)
      call assert_equal(size(array), 0, 'new array is not empty')
   end subroutine test_realloc_wrong_size

   subroutine test_realloc_int_equal_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: array_size
      integer :: i

      array_size = 5
      call initialize_int_iota(array, array_size)

      call realloc(array, array_size, stat=status)

      call check_status(status)
      call assert_equal(ubound(array, dim=1), array_size, 'new array has wrong bounds')
      call assert_equal(array, [(i, i = 1, array_size)], 'elements were not properly copied')
   end subroutine test_realloc_int_equal_size

   subroutine test_realloc_int_unallocated()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: array_size

      array_size = 4
      call realloc(array, array_size, stat=status)

      call check_status(status)
      call assert_equal(ubound(array, dim=1), array_size, 'new array has wrong bounds')
   end subroutine test_realloc_int_unallocated

   subroutine test_realloc_int_fill()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: fill_value
      integer :: i
      integer, allocatable, dimension(:) :: expected_sub_array

      initial_size = 3
      call initialize_int_iota(array, initial_size)

      new_size = 5
      fill_value = -42
      call realloc(array, new_size, fill=fill_value, stat=status)

      call check_status(status)
      call assert_equal(array(1:initial_size), [(i, i = 1, initial_size)], 'elements were not properly copied')
      allocate(expected_sub_array(new_size - initial_size))
      expected_sub_array = fill_value
      call assert_equal(array(initial_size + 1 : new_size), expected_sub_array, 'padded elements were not filled properly')
   end subroutine test_realloc_int_fill

   subroutine test_realloc_int_keep_existing()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: fill_value
      integer, allocatable, dimension(:) :: expected_array

      initial_size = 3
      call initialize_int_iota(array, initial_size)

      new_size = 5
      fill_value = 0
      call realloc(array, new_size, fill=fill_value, keepExisting=.false., stat=status)

      call check_status(status)

      allocate(expected_array(new_size))
      expected_array = fill_value
      call assert_equal(array, expected_array, 'keepExisting false did not throw away original elements')
   end subroutine test_realloc_int_keep_existing

   subroutine test_realloc_int_keep_existing_equal_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: array_size
      integer :: fill_value
      integer, allocatable, dimension(:) :: expected_array

      array_size = 3
      call initialize_int_iota(array, array_size)

      fill_value = 0
      call realloc(array, array_size, fill=fill_value, keepExisting=.false., stat=status)

      call check_status(status)

      allocate(expected_array(array_size))
      expected_array = fill_value
      call assert_equal(array, expected_array, 'keepExisting false did not throw away original elements')
   end subroutine test_realloc_int_keep_existing_equal_size

   subroutine test_realloc_int_no_fill_equal_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: array_size
      integer :: i

      array_size = 2
      call initialize_int_iota(array, array_size)

      call realloc(array, array_size, keepExisting=.false., stat=status)

      call check_status(status)
      call assert_equal(array, [(i, i = 1, array_size)], 'keepExisting false with no fill did not keep the elements')
   end subroutine test_realloc_int_no_fill_equal_size

   subroutine test_realloc_int_boundaries()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_lower_index
      integer :: new_upper_index
      integer :: i

      initial_size = 10
      call initialize_int_iota(array, initial_size)

      new_lower_index = 5
      new_upper_index = initial_size - 1
      call realloc(array, new_upper_index, new_lower_index, stat=status)

      call check_status(status)
      call assert_equal(lbound(array, dim=1), new_lower_index, 'lower index does not match the one specified')
      call assert_equal(ubound(array, dim=1), new_upper_index, 'upper index does not match the one specified')
      call assert_equal(array, [(i, i = new_lower_index, new_upper_index)], 'copied elements are not where they were expected')
   end subroutine test_realloc_int_boundaries

   subroutine test_realloc_int_shift_copied_elements()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_lower_index
      integer :: new_upper_index
      integer :: shift
      integer :: lower_index_shifted_data
      integer :: upper_index_shifted_data
      integer :: i

      initial_size = 10
      call initialize_int_iota(array, initial_size)

      new_lower_index = -5
      new_upper_index = 2
      shift = -4
      call realloc(array, new_upper_index, new_lower_index, shift=shift, stat=status)

      call check_status(status)

      lower_index_shifted_data = max(new_lower_index, 1 + shift)
      upper_index_shifted_data = min(new_upper_index, initial_size + shift)
      call assert_equal(array(lower_index_shifted_data : upper_index_shifted_data), &
                        [(i, i = lower_index_shifted_data - shift, upper_index_shifted_data - shift)], &
                        'copied elements are not where they were expected')
   end subroutine test_realloc_int_shift_copied_elements

end module test_m_alloc
