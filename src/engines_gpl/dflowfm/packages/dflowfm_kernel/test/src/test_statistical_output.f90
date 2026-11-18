!!  Copyright (C)  Stichting Deltares, 2025-2026.
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

module test_statistical_output
   use m_statistical_output
   use assertions_gtest, only: f90_expect_eq, f90_expect_streq
   use test_relative_assertions, only: expect_relative_near
   use precision, only: dp
   implicit none(type, external)
   private

   real(kind=dp), parameter :: test_tolerance = 1e-5_dp

contains

   subroutine double_source_input_function(data_pointer)
      use precision, only: dp
      real(kind=dp), pointer, dimension(:), intent(inout) :: data_pointer
      data_pointer = data_pointer * 2.0_dp
   end subroutine double_source_input_function

   function create_output_item(source_input, operation_type) result(output_item)
      real(kind=dp), target, dimension(:), intent(inout) :: source_input
      integer, intent(in) :: operation_type
      type(t_output_variable_item) :: output_item
      output_item%id_var = 42
      output_item%operation_type = operation_type
      output_item%source_input => source_input
      call initialize_statistical_output(output_item)
   end function create_output_item

   function create_simple_output_variable_set(source_input) result(output_set)
      real(kind=dp), target, dimension(:), intent(inout) :: source_input
      type(t_output_variable_set) :: output_set
      output_set%count = 1
      call realloc(output_set)
      output_set%statout(1) = create_output_item(source_input, SO_CURRENT)
   end function create_simple_output_variable_set

   !$gtest
   subroutine test_realloc_unallocated() bind(C)
      type(t_output_variable_set) :: output_set
      call f90_expect_eq(allocated(output_set%statout), .false., '')
      call f90_expect_eq(output_set%capacity, 0, '')

      call realloc(output_set)
      call f90_expect_eq(allocated(output_set%statout), .true., '')
      call f90_expect_eq(output_set%capacity > 0, .true., '')
   end subroutine test_realloc_unallocated

   !$gtest
   subroutine test_realloc_allocated() bind(C)
      type(t_output_variable_set) :: output_set
      integer, parameter :: value_to_test = 496

      output_set%count = 3
      call realloc(output_set)
      output_set%statout(3)%id_var = value_to_test

      output_set%count = 300
      call realloc(output_set)
      call f90_expect_eq(allocated(output_set%statout), .true., '')
      call f90_expect_eq(output_set%capacity >= 300, .true., '')
      call f90_expect_eq(output_set%statout(3)%id_var, value_to_test, '')
   end subroutine test_realloc_allocated

   !$gtest
   subroutine test_realloc_crop_no_elements() bind(C)
      type(t_output_variable_set) :: output_set
      call realloc(output_set, .true.)
      call f90_expect_eq(allocated(output_set%statout), .true., '')
      call f90_expect_eq(output_set%capacity, output_set%count, '')
   end subroutine test_realloc_crop_no_elements

   !$gtest
   subroutine test_realloc_crop_one_element() bind(C)
      type(t_output_variable_set) :: output_set
      output_set%count = 1
      call realloc(output_set, .true.)
      call f90_expect_eq(allocated(output_set%statout), .true., '')
      call f90_expect_eq(output_set%capacity, output_set%count, '')
   end subroutine test_realloc_crop_one_element

   !$gtest
   subroutine test_dealloc() bind(C)
      type(t_output_variable_set) :: output_set
      call realloc(output_set)
      call f90_expect_eq(allocated(output_set%statout), .true., '')
      call dealloc(output_set)
      call f90_expect_eq(allocated(output_set%statout), .false., '')
   end subroutine test_dealloc

   !$gtest
   subroutine test_update_source_input() bind(C)
      type(t_output_variable_set) :: output_set
      real(kind=dp), dimension(3), target :: source_input
      source_input = [1.0_dp, 2.0_dp, 3.0_dp]
      output_set = create_simple_output_variable_set(source_input)
      output_set%statout(1)%source_input_function_pointer => double_source_input_function
      call update_source_input(output_set)
      call expect_relative_near(output_set%statout(1)%source_input, [2.0_dp, 4.0_dp, 6.0_dp], test_tolerance, '')
   end subroutine test_update_source_input

   !$gtest
   subroutine test_update_statistical_output_average() bind(C)
      type(t_output_variable_item) :: output_item
      real(kind=dp), dimension(3), target :: source_input
      real(kind=dp) :: time_step
      source_input = [-1.0_dp, 2.0_dp, -3.0_dp]
      output_item = create_output_item(source_input, SO_AVERAGE)
      time_step = 2.0_dp
      call update_statistical_output(output_item, time_step)
      source_input = [2.0_dp, 1.0_dp, 0.0_dp]
      call update_statistical_output(output_item, time_step)

      call expect_relative_near(output_item%stat_output, [2.0_dp, 6.0_dp, -6.0_dp], test_tolerance, '')
      call expect_relative_near(output_item%time_step_sum, 4.0_dp, test_tolerance, '')
   end subroutine test_update_statistical_output_average

   !$gtest
   subroutine test_update_statistical_output_max() bind(C)
      type(t_output_variable_item) :: output_item
      real(kind=dp), dimension(3), target :: source_input
      real(kind=dp) :: time_step
      source_input = [-1.0_dp, 1.0_dp, -3.0_dp]
      output_item = create_output_item(source_input, SO_MAX)
      time_step = 2.0_dp
      call update_statistical_output(output_item, time_step)
      source_input = [2.0_dp, 1.0_dp, 0.0_dp]
      call update_statistical_output(output_item, time_step)

      call expect_relative_near(output_item%stat_output, [2.0_dp, 1.0_dp, 0.0_dp], test_tolerance, '')
   end subroutine test_update_statistical_output_max

   !$gtest
   subroutine test_update_statistical_output_min() bind(C)
      type(t_output_variable_item) :: output_item
      real(kind=dp), dimension(3), target :: source_input
      real(kind=dp) :: time_step
      source_input = [-1.0_dp, 1.0_dp, -3.0_dp]
      output_item = create_output_item(source_input, SO_MIN)
      time_step = 2.0_dp
      call update_statistical_output(output_item, time_step)
      source_input = [2.0_dp, 1.0_dp, 0.0_dp]
      call update_statistical_output(output_item, time_step)

      call expect_relative_near(output_item%stat_output, [-1.0_dp, 1.0_dp, -3.0_dp], test_tolerance, '')
   end subroutine test_update_statistical_output_min

   !$gtest
   subroutine test_finalize_average() bind(C)
      type(t_output_variable_item) :: output_item
      real(kind=dp), dimension(3), target :: source_input
      real(kind=dp) :: time_step
      source_input = [-1.0_dp, 1.0_dp, -3.0_dp]
      output_item = create_output_item(source_input, SO_AVERAGE)
      time_step = 2.0_dp
      call update_statistical_output(output_item, time_step)
      source_input = [2.0_dp, 1.0_dp, 0.0_dp]
      call update_statistical_output(output_item, time_step)
      call finalize_average(output_item)

      call expect_relative_near(output_item%stat_output, [0.5_dp, 1.0_dp, -1.5_dp], test_tolerance, '')
   end subroutine test_finalize_average

   !$gtest
   subroutine test_add_stat_output_items() bind(C)
      use fm_location_types, only: UNC_LOC_UNKNOWN
      type(t_output_variable_set) :: output_set
      type(t_output_quantity_config) :: output_config
      real(dp), dimension(3), target :: source_input

      output_config%key = 'wrihis_test'
      output_config%input_value = 'current,max'
      output_config%location_specifier = UNC_LOC_UNKNOWN

      source_input = [2.5_dp, 0.0_dp, -8.1_dp]

      call add_stat_output_items(output_set, output_config, source_input)

      call f90_expect_eq(output_set%count, 2, '')
      call f90_expect_eq(output_set%statout(1)%operation_type, SO_CURRENT, '')
      call f90_expect_eq(output_set%statout(2)%operation_type, SO_MAX, '')
      call expect_relative_near(output_set%statout(1)%source_input, source_input, test_tolerance, '')
      call expect_relative_near(output_set%statout(2)%source_input, source_input, test_tolerance, '')
      call f90_expect_streq(trim(output_set%statout(1)%output_config%key), trim('wrihis_test'), '')
   end subroutine test_add_stat_output_items
end module test_statistical_output
