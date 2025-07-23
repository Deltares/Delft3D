!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2023-2025.                                
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

   interface iota_initialize
      module procedure iota_initialize_int_1
      module procedure iota_initialize_int_2
      module procedure iota_initialize_string
   end interface iota_initialize

   contains

   !> Allocate and fill an uninitialized array with 1 ... array_size
   subroutine iota_initialize_int_1(array, array_size)
      integer, allocatable, dimension(:), intent(inout) :: array
      integer,                            intent(in   ) :: array_size

      integer :: i
      call assert_false(allocated(array), 'array was already allocated')
      allocate(array(array_size))
      do i = 1, array_size
         array(i) = i
      enddo
   end subroutine iota_initialize_int_1

   subroutine iota_initialize_int_2(array, array_size)
      integer, allocatable, dimension(:,:), intent(inout) :: array
      integer, dimension(2),                intent(in   ) :: array_size

      integer :: i, j
      call assert_false(allocated(array), 'array was already allocated')
      allocate(array(array_size(1), array_size(2)))
      do j = 1, array_size(2)
         do i = 1, array_size(1)
            array(i, j) = i + (j - 1) * array_size(1)
         enddo
      enddo
   end subroutine iota_initialize_int_2

   !> Create a string of the first string_size letters of the alphabet
   function create_iota_string(upper_bound, lower_bound) result(result_)
      character(len=:), allocatable :: result_
      integer, intent(in) :: upper_bound
      integer, intent(in), optional :: lower_bound
      integer :: i, code_a, lower_bound_

      if (present(lower_bound)) then
         lower_bound_ = lower_bound
      else
         lower_bound_ = 1
      endif

      call assert_true(lower_bound_ >= 1 .and. upper_bound >= lower_bound_ .and. upper_bound <= 26, 'String length is outside of alphabet')
      allocate(character(len=upper_bound - lower_bound_ + 1) :: result_)
      code_a = iachar('a')
      do i = lower_bound_, upper_bound
         result_(i:i) = char(code_a + i - 1)
      enddo
   end function create_iota_string

   subroutine iota_initialize_string(string, string_size)
      character(len=:), allocatable, intent(inout) :: string
      integer,                       intent(in   ) :: string_size

      call assert_false(allocated(string), 'string was already allocated')
      string = create_iota_string(string_size)
   end subroutine iota_initialize_string

   !> Make sure that the status returned by realloc is 0
   subroutine check_status(status)
      integer, intent(in) :: status
      call assert_equal(status, 0, 'realloc returned with nonzero error status')
   end subroutine check_status

   subroutine tests_alloc()
      call test(test_realloc_int_increase_size, 'realloc: increase int array size')
      call test(test_realloc_int_decrease_size, 'realloc: decrease int array size')
      call test(test_realloc_int_wrong_size, 'realloc: provide wrong int array size')
      call test(test_realloc_int_equal_size, 'realloc: int array with initial size')
      call test(test_realloc_int_unallocated, 'realloc: unallocated int array')
      call test(test_realloc_int_fill, 'realloc: grow int array and fill new elements')
      call test(test_realloc_int_keep_existing, 'realloc: do not keep elements of int array')
      call test(test_realloc_int_keep_existing_equal_size, 'realloc: int array to same size but do not keep elements')
      call test(test_realloc_int_no_fill_equal_size, 'realloc: int array to same size without fill')
      call test(test_realloc_int_boundaries, 'realloc: provide custom int array boundaries')
      call test(test_realloc_int_shift_copied_elements, 'realloc: int array shift copied elements')
      call test(test_realloc_int_rank_2_shift_copied_elements, 'realloc: rank 2 int array shift copied elements')
      call test(test_realloc_string_increase_size, 'realloc: increase string size')
      call test(test_realloc_string_decrease_size, 'realloc: decrease string size')
      call test(test_realloc_string_wrong_size, 'realloc: provide wrong string size')
      call test(test_realloc_string_equal_size, 'realloc: string with initial size')
      call test(test_realloc_string_unallocated, 'realloc: unallocated string')
      call test(test_realloc_string_fill, 'realloc: grow string and fill new elements')
      call test(test_realloc_string_empty_fill, 'realloc: grow string and provide empty fill')
      call test(test_realloc_string_fill_length_2, 'realloc: grow string and fill new elements with length 2 string')
      call test(test_realloc_string_keep_existing, 'realloc: do not keep elements of string')
      call test(test_realloc_string_keep_existing_equal_size, 'realloc: string to same size but do not keep elements')
      call test(test_realloc_string_no_fill_equal_size, 'realloc: string to same size without fill')
      call test(test_realloc_string_shift_copied_elements, 'realloc: string shift copied elements')
      call test(test_realloc_string_negative_shift_copied_elements, 'realloc: string negative shift copied elements')
      call test(test_realloc_string_shift_copy_and_fill, 'realloc: string shift copied elements and filled rest')
   end subroutine tests_alloc

   subroutine test_realloc_int_increase_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: i

      initial_size = 5
      call iota_initialize(array, initial_size)

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
      call iota_initialize(array, initial_size)

      new_size = 3
      call realloc(array, new_size, stat=status)

      call check_status(status)
      call assert_equal(ubound(array, dim=1), new_size, 'new array has wrong bounds')
      call assert_equal(array(1:new_size), [(i, i = 1, new_size)], 'elements were not properly copied')
   end subroutine test_realloc_int_decrease_size

   subroutine test_realloc_int_wrong_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: initial_size
      integer :: new_size

      initial_size = 3
      call iota_initialize(array, initial_size)

      new_size = -2
      call realloc(array, new_size, stat=status)

      call check_status(status)
      call assert_equal(size(array), 0, 'new array is not empty')
   end subroutine test_realloc_int_wrong_size

   subroutine test_realloc_int_equal_size()
      integer, allocatable, dimension(:) :: array
      integer :: status
      integer :: array_size
      integer :: i

      array_size = 5
      call iota_initialize(array, array_size)

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
      call iota_initialize(array, initial_size)

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
      call iota_initialize(array, initial_size)

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
      call iota_initialize(array, array_size)

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
      call iota_initialize(array, array_size)

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
      call iota_initialize(array, initial_size)

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
      call iota_initialize(array, initial_size)

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

   subroutine test_realloc_int_rank_2_shift_copied_elements()
      integer, allocatable, dimension(:,:) :: array
      integer :: status
      integer, dimension(2) :: initial_size
      integer, dimension(2) :: new_lower_index
      integer, dimension(2) :: new_upper_index
      integer, dimension(2) :: shift
      integer               :: fill
      integer, dimension(2) :: lower_index_shifted_data
      integer, dimension(2) :: upper_index_shifted_data
      integer, allocatable, dimension(:, :) :: expected_array
      integer :: i, j

      initial_size = [6, 5]
      call iota_initialize(array, initial_size)

      new_lower_index = [-5, 2]
      new_upper_index = [2, 5]
      shift = [-4, 3]
      fill = 13
      call realloc(array, new_upper_index, new_lower_index, shift=shift, fill=fill, stat=status)

      call check_status(status)

      lower_index_shifted_data = max(new_lower_index, 1 + shift)
      upper_index_shifted_data = min(new_upper_index, initial_size + shift)
      allocate(expected_array(new_lower_index(1):new_upper_index(1), new_lower_index(2):new_upper_index(2)))
      expected_array = fill
      do j = lower_index_shifted_data(2), upper_index_shifted_data(2)
         do i = lower_index_shifted_data(1), upper_index_shifted_data(1)
            expected_array(i, j) = (i - shift(1)) + (j - shift(2) - 1) * initial_size(1)
         enddo
      enddo

      do j = new_lower_index(2), new_upper_index(2)
         call assert_equal(array(:,j), expected_array(:,j), 'copied elements are not where they were expected')
      enddo

   end subroutine test_realloc_int_rank_2_shift_copied_elements

   subroutine test_realloc_string_increase_size()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size

      initial_size = 5
      call iota_initialize(string, initial_size)

      new_size = 10
      call realloc(string, new_size, stat=status)

      call check_status(status)
      call assert_equal(len(string), new_size, 'new string has wrong bounds')
      call assert_equal(string(1:initial_size), create_iota_string(initial_size), 'elements were not properly copied')
   end subroutine test_realloc_string_increase_size

   subroutine test_realloc_string_decrease_size()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size

      initial_size = 5
      call iota_initialize(string, initial_size)

      new_size = 3
      call realloc(string, new_size, stat=status)

      call check_status(status)
      call assert_equal(len(string), new_size, 'new string has wrong bounds')
      call assert_equal(string(1:new_size), create_iota_string(new_size), 'elements were not properly copied')
   end subroutine test_realloc_string_decrease_size

   subroutine test_realloc_string_wrong_size()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size

      initial_size = 3
      call iota_initialize(string, initial_size)

      new_size = -2
      call realloc(string, new_size, stat=status)

      call check_status(status)
      call assert_equal(len(string), 0, 'new string is not empty')
   end subroutine test_realloc_string_wrong_size

   subroutine test_realloc_string_equal_size()
      character(len=:), allocatable :: string
      integer :: status
      integer :: string_size

      string_size = 5
      call iota_initialize(string, string_size)

      call realloc(string, string_size, stat=status)

      call check_status(status)
      call assert_equal(len(string), string_size, 'new string has wrong bounds')
      call assert_equal(string, create_iota_string(string_size), 'elements were not properly copied')
   end subroutine test_realloc_string_equal_size

   subroutine test_realloc_string_unallocated()
      character(len=:), allocatable :: string
      integer :: status
      integer :: string_size

      string_size = 4
      call realloc(string, string_size, stat=status)

      call check_status(status)
      call assert_equal(len(string), string_size, 'new string has wrong bounds')
   end subroutine test_realloc_string_unallocated

   subroutine test_realloc_string_fill()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size

      initial_size = 3
      call iota_initialize(string, initial_size)

      new_size = 7
      call realloc(string, new_size, fill='x', stat=status)

      call check_status(status)
      call assert_equal(string, 'abcxxxx', 'elements were not properly copied and filled')
   end subroutine test_realloc_string_fill

   subroutine test_realloc_string_empty_fill()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size
      character(len=:), allocatable :: fill_value

      initial_size = 3
      call iota_initialize(string, initial_size)

      new_size = 7
      fill_value = ''
      call realloc(string, new_size, fill=fill_value, stat=status)

      call check_status(status)
      call assert_equal(string(1:initial_size), 'abc', 'elements were not properly copied')
   end subroutine test_realloc_string_empty_fill

   subroutine test_realloc_string_fill_length_2()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size
      character(len=:), allocatable :: fill_value

      initial_size = 3
      call iota_initialize(string, initial_size)

      new_size = 7
      fill_value = 'xy'
      call realloc(string, new_size, fill=fill_value, stat=status)

      call check_status(status)
      call assert_equal(string, 'abcyxyx', 'elements were not properly copied and filled')
   end subroutine test_realloc_string_fill_length_2

   subroutine test_realloc_string_keep_existing()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size

      initial_size = 3
      call iota_initialize(string, initial_size)

      new_size = 5
      call realloc(string, new_size, fill='a', keepExisting=.false., stat=status)

      call check_status(status)
      call assert_equal(string, 'aaaaa', 'keepExisting false did not throw away original elements')
   end subroutine test_realloc_string_keep_existing

   subroutine test_realloc_string_keep_existing_equal_size()
      character(len=:), allocatable :: string
      integer :: status
      integer :: string_size

      string_size = 3
      call iota_initialize(string, string_size)

      call realloc(string, string_size, fill='a', keepExisting=.false., stat=status)

      call check_status(status)
      call assert_equal(string, 'aaa', 'keepExisting false did not throw away original elements')
   end subroutine test_realloc_string_keep_existing_equal_size

   subroutine test_realloc_string_no_fill_equal_size()
      character(len=:), allocatable :: string
      integer :: status
      integer :: string_size

      string_size = 2
      call iota_initialize(string, string_size)

      call realloc(string, string_size, keepExisting=.false., stat=status)

      call check_status(status)
      call assert_equal(string, create_iota_string(string_size), 'keepExisting false with no fill did not keep the elements')
   end subroutine test_realloc_string_no_fill_equal_size

   subroutine test_realloc_string_shift_copied_elements()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: shift

      initial_size = 10
      call iota_initialize(string, initial_size)

      new_size = 4
      shift = 2
      call realloc(string, new_size, fill='x', shift=shift, stat=status)

      call check_status(status)
      call assert_equal(string, 'xxab', 'copied elements are not where they were expected')
   end subroutine test_realloc_string_shift_copied_elements

   subroutine test_realloc_string_negative_shift_copied_elements()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: shift

      initial_size = 10
      call iota_initialize(string, initial_size)

      new_size = 2
      shift = -4
      call realloc(string, new_size, shift=shift, stat=status)

      call check_status(status)
      call assert_equal(string, 'ef', 'copied elements are not where they were expected')
   end subroutine test_realloc_string_negative_shift_copied_elements

   subroutine test_realloc_string_shift_copy_and_fill()
      character(len=:), allocatable :: string
      integer :: status
      integer :: initial_size
      integer :: new_size
      integer :: shift

      initial_size = 5
      call iota_initialize(string, initial_size)

      new_size = 10
      shift = 2
      call realloc(string, new_size, fill='xyz', shift=shift, stat=status)

      call check_status(status)
      call assert_equal(string, 'xyabcdeyzx', 'elements were not copied and filled properly')
   end subroutine test_realloc_string_shift_copy_and_fill

end module test_m_alloc
