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

module m_statistical_output_types
   use stdlib_kinds, only: dp
   use m_output_config, only: t_output_quantity_config
   use m_temporal_statistics, only: t_moving_average_data
   implicit none
   private

   integer, parameter, public :: SO_UNKNOWN = -1 !< Unknown operation type (e.g., input error)
   integer, parameter, public :: SO_NONE    = 0  !< Dummy value for 'None', to switch off output
   integer, parameter, public :: SO_CURRENT = 1
   integer, parameter, public :: SO_AVERAGE = 2
   integer, parameter, public :: SO_MAX     = 3
   integer, parameter, public :: SO_MIN     = 4

   abstract interface
      !> subroutine to be called by update_source_input when advanced operations are required and the data to be
      !! written to the his/map file cannot be a pointer but must be calculated and stored every timestep.
      !!
      !! NOTE: these callback subroutines are also called once during init_statistical_output();
      !!       if %source_input must point to newly allocated memory, that is the time to do it once,
      !!       and should never be reallocated after that.
      subroutine process_data_interface_double(data_pointer)
         double precision, pointer, dimension(:), intent(inout) :: data_pointer !< pointer to function in-output data
      end subroutine process_data_interface_double
   end interface

   public :: process_data_interface_double

   !> Derived type for the statistical output items.
   type, public :: t_output_variable_item
      type(t_output_quantity_config), pointer                   :: output_config         !< Pointer to output configuration item.
      integer                                                   :: operation_type        !< Specifies the kind of operation to perform on the output variable.
      integer                                                   :: id_var                !< NetCDF variable ID, to be set and used by actual writing functions.
      real(dp), pointer, dimension(:)                           :: stat_output           !< Array that is to be written to the Netcdf file. In case the current values are
                                                                                         !! required this variable points to the basic variable (e.g. s1).
                                                                                         !! Otherwise during the simulation the intermediate results are stored.
      real(dp), pointer, dimension(:)                           :: source_input          !< The (possibly transformed) data over which statistics are gathered
      procedure(process_data_interface_double), nopass, pointer :: source_input_function_pointer => null() !< Function pointer for operation that needs to be performed to produce source_input
      real(dp)                                                  :: time_step_sum         !< Sum of time steps since the last output interval, used for average calculation
      type(t_moving_average_data), allocatable                  :: moving_average_data   !< Data stored for keeping track of a moving average
      integer                                                   :: moving_average_window !< Number of time steps over which a moving average is calculated
   end type t_output_variable_item

   !> Derived type to store the cross-section set
   type, public :: t_output_variable_set
      integer                                                  :: capacity = 0  !< allocated size of output variable set
      integer                                                  :: count = 0     !< count of actual items in output variable set
      type(t_output_variable_item), allocatable,  dimension(:) :: statout       !< pointer to array of output variable items
   end type t_output_variable_set

end module m_statistical_output_types
