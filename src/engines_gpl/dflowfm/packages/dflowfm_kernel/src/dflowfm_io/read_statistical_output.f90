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

!> Methods for reading the MDU keywords related to statistical output.
module m_read_statistical_output
   implicit none
   private

   ! Error/result state constants for several utility functions:
   integer, parameter, public :: SO_NOERR          =  0 !< Function successful
   integer, parameter, public :: SO_INVALID_CONFIG = -1 !< Wrong value string provided in the MDU output configuration
   integer, parameter, public :: SO_EOR            = -2 !< end-of-record reached while reading a value string provided in the MDU output configuration (= no error)

   public parse_next_stat_type_from_value_string
   public is_output_requested_in_value_string
   public read_output_parameter_toggle

   contains

   !> Parse the a statistical operation type from the start of a value string.
   !! Example input: "Wrihis_waterlevel = Current, Max(10)"
   !! The value string may contain multiple comma-separated operations: only the
   !! first one is read, and removed from the front of the input string, such that
   !! this function can be called in a loop until SO_EOR is reached.
   function parse_next_stat_type_from_value_string(value_string, operation_type, moving_average_window) result(ierr)
      use string_module, only: str_token
      use m_statistical_output_types, only: SO_UNKNOWN

      character(len=*), intent(inout) :: value_string      !< value_string in which to read the first entry. After reading, that piece will be removed from the front of the string, to enable repeated calls.
      integer,          intent(  out) :: operation_type    !< The parsed operation_type (one of SO_CURRENT/AVERAGE/MAX/MIN/ALL)
      integer,          intent(  out) :: moving_average_window !< Optional value for number of timesteps in moving average (only for max and min), 0 when unspecified in input.
      integer                         :: ierr              !< Result status: SO_NOERR on successful read, SO_INVALID_CONFIG for invalid value_string, SO_EOR if no further entries in string.

      character(len=16) :: operation_string
      integer :: len_token, iostat, i1, i2

      ierr = SO_NOERR
      operation_type = SO_UNKNOWN
      moving_average_window = 1

      call str_token(value_string, operation_string, DELIMS=', ')

      len_token = len_trim(operation_string)

      if (len_token == 0) then
         ierr = SO_EOR
         return
      else
         i1 = index(operation_string, '(')
         if (i1 > 0) then
            i2 = index(operation_string, ')')
            if (i2 > i1) then
               read(operation_string(i1+1:i2-1), *, iostat = iostat) moving_average_window
               if (iostat > 0) then
                  ierr = SO_INVALID_CONFIG
                  return
               end if
            else
               ierr = SO_INVALID_CONFIG
               return
            end if
         else
            i1 = len_token+1
         end if

         operation_type = get_operation_type(operation_string(1:i1-1))
         if (operation_type == SO_UNKNOWN) then
            ierr = SO_INVALID_CONFIG
            return
         end if
      end if
   end function parse_next_stat_type_from_value_string

   !> Determine integer operation_type given a string value.
   function get_operation_type(value_string) result(operation_type)
      use string_module, only: strcmpi
      use MessageHandling, only: msgbuf, err_flush
      use m_statistical_output_types, only: SO_UNKNOWN, SO_NONE, SO_CURRENT, SO_AVERAGE, SO_MAX, SO_MIN

      character(len=*) :: value_string !<The input value string, typically stored in an output_config item
      integer          :: operation_type !< Corresponding operation type (one of: SO_CURRENT/AVERAGE/MAX/MIN/NONE/UNKNOWN).

      operation_type = SO_UNKNOWN

      if (strcmpi(value_string, 'current') .or. strcmpi(value_string, '1')) then
         operation_type = SO_CURRENT
      else if (strcmpi(value_string, 'average')) then
         operation_type = SO_AVERAGE
      else if (strcmpi(value_string, 'max')) then
         operation_type = SO_MAX
      else if (strcmpi(value_string, 'min')) then
         operation_type = SO_MIN
      else if (strcmpi(value_string, 'none') .or. strcmpi(value_string, '0')) then
         operation_type = SO_NONE
      else
         write (msgbuf,'(a,i0,a,a,a)') 'invalid operation_type ', operation_type, '. Cannot parse input ', value_string, '.'
         call err_flush()
      end if
   end function get_operation_type

   !> Test if any output is requested in the value string
   function is_output_requested_in_value_string(value_string) result(res)
      use m_statistical_output_types, only: SO_UNKNOWN, SO_NONE
      character(*), value :: value_string !< The string provided as a value in the MDU file
      logical             :: res

      integer :: ierr, operation_type, moving_average_window
      res = .false.
      do
         ierr = parse_next_stat_type_from_value_string(value_string, operation_type, moving_average_window)
         if (ierr /= SO_NOERR) then
            ! Either an error or the end of the record. Other parts are responsible to catch these errors
            exit
         end if
         if (operation_type /= SO_NONE .and. operation_type /= SO_UNKNOWN) then
            res = .true.
         end if
      end do
   end function is_output_requested_in_value_string

   !> Get the integer value for the toggle whether to write a certain output variable.
   !! Set it to the supplied default value in the tree if the keyword is not found and the default is 1 (on).
   !! Valid strings such as 'current','average', etc. will return a value of 1.
   subroutine read_output_parameter_toggle(tree, chapter, key, value, success, alternative_key)
      use tree_structures, only: tree_data
      use properties, only: prop_get_string, prop_set
      implicit none
      type(tree_data), pointer, intent(in   ) :: tree            !< The property tree
      character(*),             intent(in   ) :: chapter         !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*),             intent(in   ) :: key             !< Name of the key (case-insensitive)
      integer,                  intent(inout) :: value           !< If key is found value will be read from tree. If not found value will be written to tree.
      logical,                  intent(  out) :: success         !< Whether successful or not
      character(*), optional,   intent(in   ) :: alternative_key !< Old alternative key name that can still be used

      character(len=255) :: value_string

      call prop_get_string(tree, chapter, key, value_string, success)
      if (.not. success .and. present(alternative_key)) then
         call prop_get_string(tree, chapter, alternative_key, value_string, success)
      end if
      if (success) then
         value = merge(1, 0, is_output_requested_in_value_string(value_string))
      else if (value /= 0) then
         call prop_set(tree, chapter, key, value, '')
      end if
   end subroutine read_output_parameter_toggle
end module m_read_statistical_output
