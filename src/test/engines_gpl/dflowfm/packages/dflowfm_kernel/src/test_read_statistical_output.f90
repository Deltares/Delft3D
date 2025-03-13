!!  Copyright (C)  Stichting Deltares, 2024-2024.
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
   use ftnunit
   use precision, only: dp
   use m_read_statistical_output
   use m_statistical_output_types, only: SO_UNKNOWN, SO_NONE, SO_CURRENT, SO_MAX, SO_MIN
   implicit none
   private

   real(dp), parameter :: test_tolerance = 1e-3_dp

   public :: tests_read_statistical_output

contains

subroutine tests_read_statistical_output
    call test(test_parse_current, 'Tests parsing of mdu current string setting for statistical output')
    call test(test_parse_one, 'Tests parsing of mdu one string setting as current for statistical output')
    call test(test_parse_none, 'Tests parsing of mdu none string setting for statistical output')
    call test(test_parse_zero, 'Tests parsing of mdu zero string setting as none for statistical output')
    call test(test_parse_max, 'Tests parsing of mdu max string setting for statistical output')
    call test(test_parse_min, 'Tests parsing of mdu min string setting for statistical output')
    call test(test_parse_max_window, 'Tests parsing of mdu max(5) + current string setting for statistical output')
    call test(test_parse_empty_string, 'Tests parsing of mdu empty string setting for statistical output')
    call test(test_output_requested, 'Tests that data collection is  turned on when some output is requested')
    call test(test_no_output_requested, 'Tests that data collection is not turned on when no output is requested')
    call test(test_read_output_parameter_toggle_default_zero, 'Tests that unset key in tree results in default zero value')
    call test(test_read_output_parameter_toggle_default_one, 'Tests that unset key in tree results in default unit value and is set in tree')
    call test(test_read_output_parameter_toggle_from_tree, 'Tests that set key in tree is read properly')
    call test(test_read_output_parameter_toggle_from_alternative_key, 'Tests that alternative key can be read')
end subroutine tests_read_statistical_output

subroutine test_parse_current()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr

   input = 'current'
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, '', '')
   call assert_equal(operation_type, SO_CURRENT, '')
   call assert_equal(moving_average_window, 1, '')
end subroutine test_parse_current

subroutine test_parse_one()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr

   input = '1'
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, '', '')
   call assert_equal(operation_type, SO_CURRENT, '')
   call assert_equal(moving_average_window, 1, '')
end subroutine test_parse_one

!> Remove leading commas and space and test parsing of 'none'
subroutine test_parse_none()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr

   input = ', ,,none'
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, '', '')
   call assert_equal(operation_type, SO_NONE, '')
   call assert_equal(moving_average_window, 1, '')
end subroutine test_parse_none

!> Remove leading spaces and parse 0
subroutine test_parse_zero()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr

   input = '  0'
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, '', '')
   call assert_equal(operation_type, SO_NONE, '')
   call assert_equal(moving_average_window, 1, '')
end subroutine test_parse_zero

subroutine test_parse_max()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr

   input = 'max'
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, '', '')
   call assert_equal(operation_type, SO_MAX, '')
   call assert_equal(moving_average_window, 1, '')
end subroutine test_parse_max

!> Remove leading spaces and comma and parse 'min'
subroutine test_parse_min()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr

   input = '  , min'
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, '', '')
   call assert_equal(operation_type, SO_MIN, '')
   call assert_equal(moving_average_window, 1, '')
end subroutine test_parse_min

!> Check parsing and filling of moving average window, and subsequently parsing current
subroutine test_parse_max_window()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr

   input = 'max(5), current, min'
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, ', current, min', '')
   call assert_equal(operation_type, SO_MAX, '')
   call assert_equal(moving_average_window, 5, '')
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_NOERR, '')
   call assert_equal(input, ', min', '')
   call assert_equal(operation_type, SO_CURRENT, '')
   call assert_equal(moving_average_window, 1, '')
end subroutine test_parse_max_window

subroutine test_parse_empty_string()
   character(:), allocatable :: input
   integer :: operation_type, moving_average_window, ierr
   input = ''
   ierr = parse_next_stat_type_from_value_string(input, operation_type, moving_average_window)
   call assert_equal(ierr, SO_EOR, '')
   call assert_equal(input, '', '')
   call assert_equal(operation_type, SO_UNKNOWN, '')
end subroutine

subroutine test_output_requested()
   call assert_equal(is_output_requested_in_value_string('none, max'), .true., '')
end subroutine test_output_requested

subroutine test_no_output_requested()
   call assert_equal(is_output_requested_in_value_string('none, none'), .false., '')
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
   call assert_equal(success, .false., '')
   call assert_equal(value, default_value, '')

   ! Since the default is zero, it is not set into the tree to prevent the variable to end up in the dia file
   call prop_get(my_tree, chapter_name, variable_name, value, success)
   call assert_equal(success, default_value == 1, '')
   call assert_equal(value, default_value, '')
end subroutine generic_test_read_output_parameter_toggle_default

subroutine test_read_output_parameter_toggle_default_zero()
   call generic_test_read_output_parameter_toggle_default(0)
end subroutine test_read_output_parameter_toggle_default_zero

subroutine test_read_output_parameter_toggle_default_one()
   call generic_test_read_output_parameter_toggle_default(1)
end subroutine test_read_output_parameter_toggle_default_one

subroutine test_read_output_parameter_toggle_from_tree()
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
   call prop_set(my_tree, chapter_name, variable_name, actual_value, success = success)
   call assert_equal(success, .true., '')

   call read_output_parameter_toggle(my_tree, chapter_name, variable_name, value, success)
   call assert_equal(success, .true., '')
   call assert_equal(value, actual_value, '')
end subroutine test_read_output_parameter_toggle_from_tree

subroutine test_read_output_parameter_toggle_from_alternative_key()
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
   call prop_set(my_tree, chapter_name, alternative_variable_name, actual_value, success = success)
   call assert_equal(success, .true., '')

   call read_output_parameter_toggle(my_tree, chapter_name, variable_name, value, success, alternative_key = alternative_variable_name)
   call assert_equal(success, .true., '')
   call assert_equal(value, actual_value, '')

   ! Ensure that the variable_name is not added to the tree
   call prop_get(my_tree, chapter_name, variable_name, value, success)
   call assert_equal(success, .false., '')
end subroutine test_read_output_parameter_toggle_from_alternative_key
end module test_read_statistical_output
