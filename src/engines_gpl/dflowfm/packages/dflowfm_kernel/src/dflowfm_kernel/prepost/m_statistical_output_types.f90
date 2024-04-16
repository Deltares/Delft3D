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
   
module m_statistical_callback
   abstract interface
      !> function pointer to be called by update_source_data when advanced operations are required and the data to be
      !! written to the his/map file cannot be a pointer but must be calculated and stored every timestep.
      !!
      !! NOTE: these callback functions are also called once during init_statistical_output();
      !!       if %source_input must point to newly allocated memory, that is the time to do it once,
      !!       and should never be reallocated after that.
      subroutine process_data_double_interface(datapointer)
         double precision, pointer, dimension(:), intent(inout) :: datapointer !< pointer to function in-output data
      end subroutine process_data_double_interface
   end interface
end module m_statistical_callback
   
module m_statistical_output_types

   use m_output_config, only: t_output_quantity_config
   use m_statistical_callback, only: process_data_double_interface
   
   !> Derived type for the statistical output items. 
   type, public :: t_output_variable_item
      type(t_output_quantity_config), pointer   :: output_config        !< Pointer to output configuration item.
      integer                                   :: operation_type       !< Specifies the kind of operation to perform on the output variable.
      integer                                   :: current_step         !< Latest entry in the work array. MOD(current_step+1,moving_average_window) is the next 
      integer                                   :: moving_average_window    !< Number of steps inside the moving average
      !< item to remove.   
      integer                                   :: id_var               !< NetCDF variable ID, to be set and used by actual writing functions.
      double precision, pointer, dimension(:)   :: stat_output          !< Array that is to be written to the Netcdf file. In case the current values are
                                                                        !< required this variable points to the basic variable (e.g. s1).
                                                                        !< Otherwise during the simulation the intermediate results are stored.
      double precision, pointer, dimension(:)   :: stat_input           !< In case a statistical operation is requested. This pointer points to the
                                                                        !< source_input.
      double precision, pointer    , dimension(:)   :: source_input         !< pointer to the basic variable
      double precision, allocatable, dimension(:,:) :: samples              !< In case a moving average is requested. This pointer points to the
                                                                        !< work array, where the different samples are stored.
      double precision, allocatable, dimension(:)   :: moving_average_sum !< In case a moving average is requested. This pointer points to the
                                                                        !< actual average values.
      double precision                          :: timestep_sum         !< sum of timesteps (for moving average/ average calculation)
      
      double precision, allocatable, dimension(:)   :: timesteps            !< array of timesteps belonging to samples in samples array
      procedure(process_data_double_interface), nopass, pointer      :: source_input_function_pointer => null()          !< function pointer for operation that needs to be performed to produce source_input 
      
   end type t_output_variable_item
   
   !> Derived type to store the cross-section set
   type, public :: t_output_variable_set
      integer                                                :: size = 0      !< size of output variable set
      integer                                                :: growsby = 200 !< increment of output variable set
      integer                                                :: count= 0      !< count of items in output variable set
      type(t_output_variable_item), pointer, dimension(:)    :: statout       !< pointer to array of output variable items
   end type t_output_variable_set

end module m_statistical_output_types
