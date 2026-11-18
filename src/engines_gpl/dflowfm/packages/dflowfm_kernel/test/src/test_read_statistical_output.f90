!!  Copyright (C)  Stichting Deltares, 2024-2026.
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

module test_read_statistical_output
   use assertions_gtest, only: f90_expect_eq, f90_expect_streq
   use precision, only: dp
   use m_read_statistical_output
   use m_statistical_output_types, only: SO_UNKNOWN, SO_NONE, SO_CURRENT, SO_MAX, SO_MIN
   implicit none(type, external)
   private

   real(dp), parameter :: test_tolerance = 1e-3_dp

contains

!$gtest
   subroutine test_parse_current() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr

      input = 'current'
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(''), '')
      call f90_expect_eq(operation_type, SO_CURRENT, '')
      call f90_expect_eq(moving_average_window, 1, '')
   end subroutine test_parse_current

!$gtest
   subroutine test_parse_one() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr

      input = '1'
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(''), '')
      call f90_expect_eq(operation_type, SO_CURRENT, '')
      call f90_expect_eq(moving_average_window, 1, '')
   end subroutine test_parse_one

!> Remove leading commas and space and test parsing of 'none'
!$gtest
   subroutine test_parse_none() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr

      input = ', ,,none'
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(''), '')
      call f90_expect_eq(operation_type, SO_NONE, '')
      call f90_expect_eq(moving_average_window, 1, '')
   end subroutine test_parse_none

!> Remove leading spaces and parse 0
!$gtest
   subroutine test_parse_zero() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr

      input = '  0'
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(''), '')
      call f90_expect_eq(operation_type, SO_NONE, '')
      call f90_expect_eq(moving_average_window, 1, '')
   end subroutine test_parse_zero

!$gtest
   subroutine test_parse_max() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr

      input = 'max'
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(''), '')
      call f90_expect_eq(operation_type, SO_MAX, '')
      call f90_expect_eq(moving_average_window, 1, '')
   end subroutine test_parse_max

!> Remove leading spaces and comma and parse 'min'
!$gtest
   subroutine test_parse_min() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr

      input = '  , min'
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(''), '')
      call f90_expect_eq(operation_type, SO_MIN, '')
      call f90_expect_eq(moving_average_window, 1, '')
   end subroutine test_parse_min

!> Check parsing and filling of moving average window, and subsequently parsing current
!$gtest
   subroutine test_parse_max_window() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr

      input = 'max(5), current, min'
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(', current, min'), '')
      call f90_expect_eq(operation_type, SO_MAX, '')
      call f90_expect_eq(moving_average_window, 5, '')
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_NOERR, '')
      call f90_expect_streq(trim(input), trim(', min'), '')
      call f90_expect_eq(operation_type, SO_CURRENT, '')
      call f90_expect_eq(moving_average_window, 1, '')
   end subroutine test_parse_max_window

!$gtest
   subroutine test_parse_empty_string() bind(C)
      character(:), allocatable :: input
      integer :: operation_type, moving_average_window, ierr
      input = ''
      ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
      call f90_expect_eq(ierr, SO_EOR, '')
      call f90_expect_streq(trim(input), trim(''), '')
      call f90_expect_eq(operation_type, SO_UNKNOWN, '')
   end subroutine

!$gtest
   subroutine test_output_requested() bind(C)
      call f90_expect_eq(is_output_requested_in_value_string('none, max'), .true., '')
   end subroutine test_output_requested

!$gtest
   subroutine test_no_output_requested() bind(C)
      call f90_expect_eq(is_output_requested_in_value_string('none, none'), .false., '')
   end subroutine test_no_output_requested

   function create_dummy_tree() result(tree)
      use tree_structures, only: tree_data, tree_create
      type(tree_data), pointer :: tree

      call tree_create('my_tree', tree)
   end function create_dummy_tree

!> Test that the value for the toggle that is read from the tree equals the default value when it is not supplied,
!! and test that is is set into the tree if the default value is 1
   subroutine generic_test_read_output_parameter_toggle_default(default_value)
      use tree_structures, only: tree_data
      use properties, only: prop_get

      integer, intent(in) :: default_value !< The value if the property is not set in the tree, either 0 or 1
      type(tree_data), pointer :: my_tree
      integer :: value
      logical :: success
      character(:), allocatable :: chapter_name, variable_name

      chapter_name = 'my_chap'
      variable_name = 'write_my_var'
      value = default_value
      my_tree => create_dummy_tree()

      call read_output_parameter_toggle(my_tree, chapter_name, variable_name, value, success)
      call f90_expect_eq(success, .false., '')
      call f90_expect_eq(value, default_value, '')

      ! Since the default is zero, it is not set into the tree to prevent the variable to end up in the dia file
      call prop_get(my_tree, chapter_name, variable_name, value, success)
      call f90_expect_eq(success, default_value == 1, '')
      call f90_expect_eq(value, default_value, '')
   end subroutine generic_test_read_output_parameter_toggle_default

!$gtest
   subroutine test_read_output_parameter_toggle_default_zero() bind(C)
      call generic_test_read_output_parameter_toggle_default(0)
   end subroutine test_read_output_parameter_toggle_default_zero

!$gtest
   subroutine test_read_output_parameter_toggle_default_one() bind(C)
      call generic_test_read_output_parameter_toggle_default(1)
   end subroutine test_read_output_parameter_toggle_default_one

!$gtest
   subroutine test_read_output_parameter_toggle_from_tree() bind(C)
      use tree_structures, only: tree_data
      use properties, only: prop_set

      type(tree_data), pointer :: my_tree
      integer :: value, default_value, actual_value
      logical :: success
      character(:), allocatable :: chapter_name, variable_name

      chapter_name = 'my_chap'
      variable_name = 'write_my_var'
      default_value = 0
      actual_value = 1
      value = default_value
      my_tree => create_dummy_tree()
      call prop_set(my_tree, chapter_name, variable_name, actual_value, success=success)
      call f90_expect_eq(success, .true., '')

      call read_output_parameter_toggle(my_tree, chapter_name, variable_name, value, success)
      call f90_expect_eq(success, .true., '')
      call f90_expect_eq(value, actual_value, '')
   end subroutine test_read_output_parameter_toggle_from_tree

!$gtest
   subroutine test_read_output_parameter_toggle_from_alternative_key() bind(C)
      use tree_structures, only: tree_data
      use properties, only: prop_get, prop_set

      type(tree_data), pointer :: my_tree
      integer :: value, default_value, actual_value
      logical :: success
      character(:), allocatable :: chapter_name, variable_name, alternative_variable_name

      chapter_name = 'my_chap'
      variable_name = 'write_my_var'
      alternative_variable_name = 'write_your_var'
      default_value = 0
      actual_value = 1
      value = default_value
      my_tree => create_dummy_tree()
      call prop_set(my_tree, chapter_name, alternative_variable_name, actual_value, success=success)
      call f90_expect_eq(success, .true., '')

      call read_output_parameter_toggle(my_tree, chapter_name, variable_name, value, success, alternative_key=alternative_variable_name)
      call f90_expect_eq(success, .true., '')
      call f90_expect_eq(value, actual_value, '')

      ! Ensure that the variable_name is not added to the tree
      call prop_get(my_tree, chapter_name, variable_name, value, success)
      call f90_expect_eq(success, .false., '')
   end subroutine test_read_output_parameter_toggle_from_alternative_key
end module test_read_statistical_output
