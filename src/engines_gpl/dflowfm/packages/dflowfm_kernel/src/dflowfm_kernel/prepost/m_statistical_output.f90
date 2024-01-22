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
   
!> This module implements the statistical output in D-Flow FM.
module m_statistical_output
   use MessageHandling
   use m_output_config
   use m_statistical_callback
   
   implicit none
   
private

   public realloc
   public dealloc
   public update_statistical_output, update_source_data, add_stat_output_items, initialize_statistical_output, reset_statistical_output, finalize_so_average

   !> Realloc memory cross-section definition or cross-sections
   interface realloc
      module procedure realloc_stat_output
   end interface

   !> Free the memory of cross-section definition or cross-sections
   interface dealloc
      module procedure dealloc_stat_output
   end interface dealloc

   integer, parameter, public :: SO_UNKNOWN = -1 !< Unknown operation type (e.g., input error)
   integer, parameter, public :: SO_NONE    = 0  !< Dummy value for 'None', to switch off output
   integer, parameter, public :: SO_CURRENT = 1
   integer, parameter, public :: SO_AVERAGE = 2
   integer, parameter, public :: SO_MAX     = 3
   integer, parameter, public :: SO_MIN     = 4

   ! Error/result state constants for several utility functions:
   integer, parameter, public :: SO_NOERR          =  0 !< Function successful
   integer, parameter, public :: SO_INVALID_CONFIG = -1 !< Wrong value string provided in the MDU output configuration
   integer, parameter, public :: SO_EOR            = -2 !< end-of-record reached while reading a value string provided in the MDU output configuration (= no error)
   
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

   subroutine realloc_stat_output(statoutput,size)
      ! Modules
      use m_alloc

      implicit none
      ! Input/output parameters
      type(t_output_variable_set), intent(inout)   :: statoutput !< Current cross-section definition
      integer, intent(in), optional                :: size       !< for when a specific size is requested
      
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
      
      if (present(size) .and. size >= statoutput%count) then
         allocate(statoutput%statout(size),stat=ierr)
         call aerr('statoutput%statout(size)',ierr,size)
         
         statoutput%statout(1:size) = oldstats(1:size)
         deallocate(oldstats)
         statoutput%size = size
      else
         allocate(statoutput%statout(statoutput%size+statoutput%growsBy),stat=ierr)
         call aerr('statoutput%statout(statoutput%size+statoutput%growsBy)',ierr,statoutput%size+statoutput%growsBy)
   
         if (statoutput%size > 0) then
            statoutput%statout(1:statoutput%size) = oldstats(1:statoutput%size)
            deallocate(oldstats)
         endif
         statoutput%size = statoutput%size+statoutput%growsBy
      endif
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
      
      if (item%moving_average_window > 1) then ! No need to average with a sample window of 1
         !when timestep < windowsize, no samples need to be removed. The timesteps array and samples array will be initialized to 0 so that we can keep the same expression.
         jold = MOD(item%current_step,item%moving_average_window)+1
         item%moving_average_sum = item%moving_average_sum - item%samples(:,jold)*item%timesteps(jold) + item%samples(:,jnew)*item%timesteps(jnew)
         item%timestep_sum = item%timestep_sum - item%timesteps(jold) + item%timesteps(jnew)
         item%stat_input = item%moving_average_sum/item%timestep_sum
      else
         item%stat_input = item%samples(:,item%current_step)
      endif
         
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
         item%current_step = mod(item%current_step,item%moving_average_window)+1 ! shift current step by 1
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
   elemental subroutine finalize_SO_AVERAGE(item) 

      type(t_output_variable_item), intent(inout) :: item !< The item to be processed. Will be double-checked on its operation type.

      if (item%operation_type == SO_AVERAGE) then 
         item%stat_output = item%stat_output/item%timestep_sum
      endif

   end subroutine finalize_SO_AVERAGE

   !> Reset an item's stat_output array, to be called after every output interval.
   elemental subroutine reset_statistical_output(item)
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
         !call mess(LEVEL_ERROR, 'update_statistical_output: invalid operation_type')
      end select

   end subroutine reset_statistical_output
   
   !> Create a new output item and add it to the output set according to output quantity config
   subroutine add_stat_output_items(output_set, output_config, data_pointer, source_input_function_pointer)
   use m_statistical_callback
   
      type(t_output_variable_set),                                 intent(inout) :: output_set    !< Output set that item will be added to
      type(t_output_quantity_config), target,                      intent(in   ) :: output_config !< Output quantity config linked to this output item, a pointer to it will be stored in the new output item.
      double precision, pointer, dimension(:),                     intent(in   ) :: data_pointer  !< Pointer to output quantity data ("source input")
      procedure(process_data_double_interface), optional, pointer, intent(in   ) :: source_input_function_pointer !< (optional) Function pointer for producing/processing the source data, if no direct data_pointer is available
      
      type(t_output_variable_item) :: item ! new item to be added
      character(len=len_trim(output_config%input_value)) :: valuestring
      integer :: ierr
      valuestring = output_config%input_value

      do while (len_trim(valuestring) > 0) 
         ierr = parse_next_stat_type_from_valuestring(valuestring, item%operation_type, item%moving_average_window)
         if (ierr /= SO_NOERR) then
            goto 999
         else if (item%operation_type == SO_NONE) then
            cycle
         else
            output_set%count = output_set%count + 1
            if (output_set%count > output_set%size) then
               call realloc_stat_output(output_set)
            endif

            item%output_config => output_config
            item%source_input => data_pointer
            if (present(source_input_function_pointer)) then
               item%source_input_function_pointer => source_input_function_pointer
            endif

            output_set%statout(output_set%count) = item
         end if

      enddo
      
      return ! No error
      
999   continue ! Some error occurred
      write (msgbuf, '(a,a,a,a,a,a)') 'add_stat_output_items: error while adding items for ', trim(output_config%name), '. Original input was: ', trim(output_config%key), ' = ', trim(output_config%input_value)
      call err_flush()

   end subroutine add_stat_output_items

   !> Parse the a statistical operation type from the start of a value string.
   !! Example input: "Wrihis_waterlevel = Current, Max(10)"
   !! The value string may contain multiple comma-separated operations: only the
   !! first one is read, and removed from the front of the input string, such that
   !! this function can be called in a loop until SO_EOR is reached.
   function parse_next_stat_type_from_valuestring(valuestring, operation_type, moving_average_window) result(ierr)
      use string_module, only: str_token

      character(len=*), intent(inout) :: valuestring       !< Valuestring in which to read the first entry. After reading, that piece will be removed from the front of the string, to enable repeated calls.
      integer,          intent(  out) :: operation_type    !< The parsed operation_type (one of SO_CURRENT/AVERAGE/MAX/MIN/ALL)
      integer,          intent(  out) :: moving_average_window !< Optional value for number of timesteps in moving average (only for max and min), 0 when unspecified in input.
      integer                         :: ierr              !< Result status: SO_NOERR on successful read, SO_INVALID_CONFIG for invalid valuestring, SO_EOR if no further entries in string.

      character(len=16) :: operation_string
      integer :: len_token, iostat, i1, i2

      ierr = SO_NOERR

      call str_token(valuestring, operation_string, DELIMS=', ')
      
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
            moving_average_window = 1 ! Needs minimum size one to allocate samples array etc.
            i1 = len_token+1
         end if

         operation_type = get_operation_type(operation_string(1:i1-1))
         if (operation_type == SO_UNKNOWN) then
            ierr = SO_INVALID_CONFIG
            return
         end if
      end if

   end function parse_next_stat_type_from_valuestring
         
            
            
   !> Determine integer operation_type given a string value.
   function get_operation_type(valuestring) result(operation_type)
   use string_module, only: strcmpi
   
      character(len=*) :: valuestring !<The input value string, typically stored in an output_config item
      integer          :: operation_type !< Corresponding operation type (one of: SO_CURRENT/AVERAGE/MAX/MIN/NONE/UNKNOWN).      

      operation_type = SO_UNKNOWN
      
      if      (strcmpi(valuestring, 'current') .or. strcmpi(valuestring, '1')) then
         operation_type = SO_CURRENT
      else if (strcmpi(valuestring, 'average')) then
         operation_type = SO_AVERAGE
      else if (strcmpi(valuestring, 'max')) then
         operation_type = SO_MAX             
      else if (strcmpi(valuestring, 'min')) then
         operation_type = SO_MIN
      else if (strcmpi(valuestring, 'none') .or. strcmpi(valuestring, '0')) then
         operation_type = SO_NONE
      endif

   end function get_operation_type
      
   !> For every item in output_set, allocate arrays depending on its operation_type.
   subroutine initialize_statistical_output(output_set)
   
      type(t_output_variable_set), intent(inout) :: output_set !> output set that needs to be initialized

      type(t_output_variable_item), pointer  :: item 
      integer :: j, input_size
      logical :: success
      
      if (output_set%count > 0) then
         call realloc_stat_output(output_set,output_set%count) ! set size to count
      endif
      
      do j = 1, output_set%count
         item => output_set%statout(j)

         ! First "init" call to callback functions, such that %source_input is allocated
         if (associated(item%source_input_function_pointer)) then
            call item%source_input_function_pointer(item%source_input)
         endif

         input_size = size(item%source_input)

         select case (item%operation_type)
         case (SO_CURRENT)
            item%stat_output => item%source_input
         case (SO_AVERAGE)
            allocate(item%stat_output(input_size))
            item%stat_input => item%source_input
         case (SO_MIN, SO_MAX)
            allocate(item%stat_output(input_size),item%moving_average_sum(input_size), &
               item%samples(input_size,item%moving_average_window),item%timesteps(item%moving_average_window),item%stat_input(input_size))

            item%moving_average_sum = 0
            item%samples = 0
            item%timesteps = 0
            item%timestep_sum = 0
            item%current_step = 1
         case (SO_NONE)
            continue
         case default
            write (msgbuf,'(a,i0,a,a,a,a)') 'initialize_statistical_output: invalid operation_type ', item%operation_type, '. Original input for item was: ', trim(item%output_config%key), ' = ', trim(item%output_config%input_value)
            call err_flush()
         end select

         call reset_statistical_output(item)
      enddo

   end subroutine initialize_statistical_output

end module
