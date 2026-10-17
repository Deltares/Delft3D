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

!> Module with input checks for D-Emissions
module m_demissions_input_checks

   use m_waq_precision
   use m_logger_helper, only: write_log_message, write_error_message

   implicit none

   private
   public :: check_fraction, warn_below_minimum

contains

   !> Function to check if the value is a fraction in the range [0.0, 1.0]
   !!
   !! gives an error and stops the calculation when value is not a fraction
   subroutine check_fraction(value, name, iseg)

      real(kind=int_wp), intent(in) :: value !> Value to be checked
      character(*), intent(in) :: name !> Name of the parameter to be checked
      integer, intent(in) :: iseg !> Segment number where the value was found

      character(len=20) :: value_string ! value as a string
      character(len=20) :: iseg_string ! iseg as a string
      character(len=200) :: message ! composite message

      if (value < 0.0 .or. value > 1.0) then
         write (value_string, '(es12.4e3)') value
         write (iseg_string, '(i0)') iseg
         message = 'ERROR in D-Emissions: '//trim(name)//' in segment '//trim(iseg_string)//' is '//trim(value_string)// &
                   ', but should be a fraction in the range [0.0-1.0]'
         call write_error_message(message)
      end if
   end subroutine check_fraction

   !> Function to warn if a value is greater than a certain expected minimum
   !!
   !! gives a warning when the value is lower than expected
   subroutine warn_below_minimum(value, minimum, name, iseg)

      real(kind=int_wp), intent(in) :: value !>  value to be checked
      real(kind=int_wp), intent(in) :: minimum !>   expected minimum
      character(*), intent(in) :: name !> name of the parameter to be checked
      integer, intent(in) :: iseg !> Segment number where the value was found

      integer(kind=int_wp) :: lunrep ! value as a string
      character(len=20) :: value_string ! value as a string
      character(len=20) :: minimum_string ! minimum as a string
      character(len=20) :: iseg_string ! iseg as a string
      character(len=200) :: message ! composite message

      if (value < minimum) then
         write (value_string, '(es12.4e3)') value
         write (minimum_string, '(es12.4e3)') minimum
         write (iseg_string, '(i0)') iseg
         message = 'WARNING in D-Emissions: '//trim(name)//' has a value of '//trim(value_string)// &
                   ', but is expected to be larger than '//trim(minimum_string)//' in segment '//trim(iseg_string)
         call write_log_message(message)
      end if
   end subroutine warn_below_minimum
end module m_demissions_input_checks
