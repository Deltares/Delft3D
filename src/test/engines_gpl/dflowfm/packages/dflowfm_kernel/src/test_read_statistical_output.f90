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
   use stdlib_kinds, only: dp
   use m_read_statistical_output
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
end module test_read_statistical_output
