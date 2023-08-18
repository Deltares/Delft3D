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
      subroutine process_data_double_interface(datapointer)
         double precision, pointer, dimension(:), intent(inout) :: datapointer !< pointer to function in-output data
      end subroutine process_data_double_interface
   end interface
end module m_statistical_callback
   
!> This module implements the statistical output in D-Flow FM.
module m_statistical_output
   use MessageHandling
   use m_output_config
   use m_statistical_callback
   
   implicit none
   
private

   public realloc
   public dealloc
   public update_statistical_output, update_source_data, add_stat_output_item, initialize_statistical_output

   !> Realloc memory cross-section definition or cross-sections
   interface realloc
      module procedure realloc_stat_output
   end interface

   !> Free the memory of cross-section definition or cross-sections
   interface dealloc
      module procedure dealloc_stat_output
   end interface dealloc

   integer, parameter :: SO_CURRENT = 1
   integer, parameter :: SO_AVERAGE = 2
   integer, parameter :: SO_MAX     = 3
   integer, parameter :: SO_MIN     = 4
   
   integer :: window_size !< The size of the moving average window, in samples.
   
   !> Derived type for the statistical output items. 
   type, public :: t_output_variable_item
      type(t_output_quantity_config), pointer   :: output_config        !< Pointer to output configuration item.
      integer                                   :: operation_type       !< Specifies the kind of operation to perform on the output variable.
      integer                                   :: current_step         !< Latest entry in the work array. MOD(current_step+1,total_steps_count) is the next 
      integer                                   :: total_steps_count
      !< item to remove.   
      double precision, pointer, dimension(:)   :: stat_output          !< Array that is to be written to the Netcdf file. In case the current values are
                                                                        !< required this variable points to the basic variable (e.g. s1).
                                                                        !< Otherwise during the simulation the intermediate results are stored.
      double precision, pointer, dimension(:)   :: stat_input           !< In case a statistical operation is requested. This pointer points to the
                                                                        !< basic variable.
      double precision, pointer, dimension(:)   :: source_input         !< pointer to the basic variable
      double precision, pointer, dimension(:,:) :: samples              !< In case a moving average is requested. This pointer points to the
                                                                        !< work array, where the different samples are stored.
      double precision, pointer, dimension(:)   :: moving_average_sum !< In case a moving average is requested. This pointer points to the
                                                                        !< actual average values.
      double precision                          :: timestep_sum         !< sum of timesteps (for moving average/ average calculation)
      
      double precision, pointer, dimension(:)   :: timesteps            !< array of timesteps belonging to samples in samples array
      procedure(process_data_double_interface), nopass, pointer      :: source_input_function_pointer => null()          !< function pointer for operation that needs to be performed to produce source_input 
      
   end type t_output_variable_item
   
   !> Derived type to store the cross-section set
   type, public :: t_output_variable_set
      integer                                                :: size = 0      !< size of output variable set
      integer                                                :: growsby = 200 !< increment of output variable set
      integer                                                :: count= 0      !< count of items in output variable set
      type(t_output_variable_item), pointer, dimension(:)    :: statout       !< pointer to array of output variable items
   end type t_output_variable_set

contains

   subroutine realloc_stat_output(statoutput)
      ! Modules
      use m_alloc

      implicit none
      ! Input/output parameters
      type(t_output_variable_set), intent(inout)   :: statoutput !< Current cross-section definition
      
   
      ! Local variables
      integer                   :: ierr
      type(t_output_variable_item), pointer, dimension(:)    :: oldstats
   
      ! Program code
   
      if (statoutput%size > 0) then
         oldstats=>statoutput%statout
      endif
   
      if (statoutput%growsBy <=0) then
         statoutput%growsBy = 200
      endif
      allocate(statoutput%statout(statoutput%size+statoutput%growsBy),stat=ierr)
      call aerr('statoutput%statout(statoutput%size+statoutput%growsBy)',ierr,statoutput%size+statoutput%growsBy)
   
      if (statoutput%size > 0) then
         statoutput%statout(1:statoutput%size) = oldstats(1:statoutput%size)
         deallocate(oldstats)
      endif
      statoutput%size = statoutput%size+statoutput%growsBy
   end subroutine realloc_stat_output

   subroutine dealloc_stat_output(statoutput)
      implicit none
      ! Input/output parameters
      type(t_output_variable_set), intent(inout)   :: statoutput !< Current cross-section definition

      if (statoutput%size> 0) then
         deallocate(statoutput%statout)
      endif
   end subroutine dealloc_stat_output

   !> updates the moving average of a stat_out_item by removing the oldest and adding the newest value
   elemental subroutine update_moving_average(item)

      type(t_output_variable_item), intent(inout) :: item !< statistical output item to update
      
      integer :: jnew, jold !< Index to newest and oldest timestep in samples array

      jnew = item%current_step
      jold = MOD(item%current_step,item%total_steps_count)+1
   
      !when timestep < windowsize, no samples need to be removed. The timesteps array and samples array will be initialized to 0 so that we can keep the same expression.
      item%moving_average_sum = item%moving_average_sum - item%samples(:,jold)*item%timesteps(jold) + item%samples(:,jnew)*item%timesteps(jnew)
      item%timestep_sum = item%timestep_sum - item%timesteps(jold) + item%timesteps(jnew)
      item%stat_input = item%moving_average_sum/item%timestep_sum
   
   end subroutine update_moving_average

   !> adds a new sample, and its timestep to the samples array. Only needed for moving average calculation.
   elemental subroutine add_statistical_output_sample(item,timestep)

      type(t_output_variable_item), intent(inout) :: item      !< statistical output item to update
      double precision, intent(in)                :: timestep  !< this is usually dts

      item%timesteps(item%current_step) = timestep
      item%samples(:,item%current_step) = item%source_input

   end subroutine add_statistical_output_sample

   !> Update the variables that need a separate subroutine call to update their source_input array
   subroutine update_source_data(output_set)
   type(t_output_variable_set),    intent(inout)   :: output_set    !> output set that we wish to update
   type(t_output_variable_item), pointer  :: item
   
   integer :: j
   
   do j = 1, output_set%count
      item => output_set%statout(j)
      if (associated(item%source_input_function_pointer)) then
         call item%source_input_function_pointer(item%source_input)
      endif
   enddo
   
   end subroutine update_source_data
   
   !> Updates the stat_output of an item using the stat_input array, depending on the operation_type.
   !! stat_input is filled elsewhere and can be a moving average or a pointer to an input variable.
   elemental subroutine update_statistical_output(item, dts)
      type(t_output_variable_item), intent(inout) :: item   !< statistical output item to update
      double precision,             intent(in)    :: dts    !< current timestep
   
      if (item%operation_type == SO_MIN .or. item%operation_type == SO_MAX) then ! max/min of moving average requested
         call add_statistical_output_sample(item,dts)
         call update_moving_average(item)
         item%current_step = mod(item%current_step+1,item%total_steps_count)
      endif

      select case (item%operation_type)
      case (SO_CURRENT)
         continue
      case (SO_AVERAGE) 
         item%stat_output = item%stat_output + item%stat_input * dts
         item%timestep_sum = item%timestep_sum + dts
      case (SO_MAX) 
         item%stat_output = max(item%stat_output,item%stat_input)
      case (SO_MIN) 
         item%stat_output = min(item%stat_output,item%stat_input)
      case default
         return
      end select

   end subroutine update_statistical_output

   !> Perform the final time interval averaging on an item,
   !! after all values haven been summed up in %stat_output.
   subroutine finalize_SO_AVERAGE(item) 

      type(t_output_variable_item), intent(inout) :: item !< The item to be processed. Will be double-checked on its operation type.

      if (item%operation_type == SO_AVERAGE) then 
         item%stat_output = item%stat_output/item%timestep_sum
      endif

   end subroutine finalize_SO_AVERAGE

   !> Reset an item's stat_output array, to be called after every output interval.
   subroutine reset_statistical_output(item)
      type(t_output_variable_item), intent(inout) :: item !< Statistical output item to reset

      select case (item%operation_type)
      case (SO_CURRENT)
         continue
      case (SO_AVERAGE)
         item%stat_output = 0 
         item%timestep_sum = 0 !new sum every output interval
      case (SO_MAX)
         item%stat_output = -huge(1d0)
      case (SO_MIN)
         item%stat_output = huge(1d0)
      case default 
         call mess(LEVEL_ERROR, 'update_statistical_output: invalid operation_type')
      end select

   end subroutine reset_statistical_output
   
   !> Create a new output item and add it to the output set according to output quantity config
   subroutine add_stat_output_item(output_set, output_config, data_pointer, source_input_function_pointer)
   use m_statistical_callback
   
      type(t_output_variable_set), intent(inout) :: output_set             !< Output set that item will be added to
      type(t_output_quantity_config), pointer, intent(in) :: output_config !< Output quantity config linked to this output item
      double precision, pointer, dimension(:), intent(in) :: data_pointer  !< Pointer to output quantity data ("source input")
      procedure(process_data_double_interface), optional, pointer, intent(in) :: source_input_function_pointer !< (optional) Function pointer for producing/processing the source data, if no direct data_pointer is available
      
      type(t_output_variable_item) :: item !> new item to be added
      
      output_set%count = output_set%count + 1
      if (output_set%count > output_set%size) then
         call realloc_stat_output(output_set)
      endif
      
      item%output_config => output_config
      item%operation_type = set_operation_type(output_config)
      item%source_input => data_pointer
      if (present(source_input_function_pointer)) then
         item%source_input_function_pointer => source_input_function_pointer
      endif
      
      output_set%statout(output_set%count) = item
      
   end subroutine add_stat_output_item
   
   integer function set_operation_type(output_config)
   use string_module, only: strcmpi
   
      type(t_output_quantity_config), intent(in) :: output_config          !> output quantity config linked to output item
      
      set_operation_type = -1
      
      if      (strcmpi(output_config%input_value, 'current')) then
         set_operation_type = SO_CURRENT
      else if (strcmpi(output_config%input_value, 'average')) then
         set_operation_type = SO_AVERAGE
      else if (strcmpi(output_config%input_value, 'max')) then
         set_operation_type = SO_MAX             
      else if (strcmpi(output_config%input_value, 'min')) then
         set_operation_type = SO_MIN
      endif
         
   end function set_operation_type
      
   !> For every item in output_set, allocate arrays depending on its operation_type.
   subroutine initialize_statistical_output(output_set)
   
      type(t_output_variable_set), intent(inout) :: output_set !> output set that needs to be initialized

      type(t_output_variable_item), pointer  :: item 
      integer :: j, input_size
      logical :: success
      
      do j = 1, output_set%count
         item => output_set%statout(j)
         input_size = size(item%source_input)
         if (associated(item%source_input_function_pointer)) then
            call item%source_input_function_pointer(item%source_input)
         endif

         select case (item%operation_type)
         case (SO_CURRENT)
            item%stat_output => item%source_input
         case (SO_AVERAGE)
            allocate(item%stat_output(input_size))
            item%stat_input => item%source_input
         case (SO_MIN, SO_MAX)
            allocate(item%stat_output(input_size),item%moving_average_sum(input_size), &
               item%samples(input_size,window_size),item%timesteps(window_size),item%stat_input(input_size))

            item%moving_average_sum = 0
            item%samples = 0
            item%timesteps = 0
            item%timestep_sum = 0
            case default
            call mess(LEVEL_ERROR, 'initialize_statistical_output: invalid operation_type')
         end select

         call reset_statistical_output(item)
      enddo

   end subroutine initialize_statistical_output

end module
