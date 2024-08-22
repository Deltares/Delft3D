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

!> This module implements the statistical output in D-Flow FM.
module m_statistical_output

   use MessageHandling
   use m_output_config
   use m_statistical_output_types, only: t_output_variable_item, t_output_variable_set, SO_NONE, SO_CURRENT, SO_AVERAGE, SO_MAX, SO_MIN
   use m_read_statistical_output
   use m_temporal_statistics
   use precision, only: dp
   use fm_location_types

   implicit none

   private

   public realloc
   public dealloc
   public update_statistical_output, update_source_input, add_stat_output_items, &
      initialize_statistical_output, reset_statistical_output, finalize_average

   !> Realloc memory cross-section definition or cross-sections
   interface realloc
      module procedure reallocate_output_set
   end interface

   !> Free the memory of cross-section definition or cross-sections
   interface dealloc
      module procedure deallocate_output_set
   end interface dealloc

contains

   subroutine reallocate_output_set(output_set, crop)
      use m_alloc

      type(t_output_variable_set), intent(inout) :: output_set !< output variable set to reallocate
      logical, intent(in), optional :: crop !< crop output set to number of valid items

      logical :: crop_
      type(t_output_variable_item), allocatable, dimension(:) :: new_statout

      crop_ = .false.
      if (present(crop)) then
         crop_ = crop
      end if

      if (crop_ .and. output_set%count < output_set%capacity) then
         allocate (new_statout(output_set%count))
         new_statout(1:output_set%count) = output_set%statout(1:output_set%count)
         call move_alloc(new_statout, output_set%statout)
         output_set%capacity = output_set%count
      else
         if (allocated(output_set%statout)) then
            if (output_set%count > output_set%capacity) then ! only increase size if necessary
               output_set%capacity = output_set%capacity * 2
               allocate (new_statout(output_set%capacity))
               new_statout(1:size(output_set%statout)) = output_set%statout
               call move_alloc(new_statout, output_set%statout)
            end if
         else
            output_set%capacity = 200
            allocate (output_set%statout(output_set%capacity))
         end if
      end if

   end subroutine reallocate_output_set

   subroutine deallocate_output_set(output_set)
      implicit none
      ! Input/output parameters
      type(t_output_variable_set), intent(inout) :: output_set !< Current cross-section definition

      if (allocated(output_set%statout)) then
         deallocate (output_set%statout)
      end if
   end subroutine deallocate_output_set

   !> Update the variables that need a separate subroutine call to update their source_input array
   subroutine update_source_input(output_set)
      type(t_output_variable_set), intent(inout) :: output_set !< output set that we wish to update

      integer :: j

      do j = 1, output_set%count
         associate (item => output_set%statout(j))
            if (associated(item%source_input_function_pointer)) then
               call item%source_input_function_pointer(item%source_input)
            end if
         end associate
      end do

   end subroutine update_source_input

   !> Update the stat_output of an item, depending on the operation_type.
   elemental subroutine update_statistical_output(item, dts)
      type(t_output_variable_item), intent(inout) :: item !< statistical output item to update
      double precision, intent(in) :: dts !< current timestep

      if (item%operation_type == SO_MIN .or. item%operation_type == SO_MAX) then ! max/min of moving average requested
         call update_moving_average_data(item%moving_average_data, item%source_input, dts)
      end if

      select case (item%operation_type)
      case (SO_CURRENT)
         return
      case (SO_AVERAGE)
         item%stat_output = item%stat_output + item%source_input * dts
         item%time_step_sum = item%time_step_sum + dts
      case (SO_MAX)
         item%stat_output = max(item%stat_output, calculate_moving_average(item%moving_average_data))
      case (SO_MIN)
         item%stat_output = min(item%stat_output, calculate_moving_average(item%moving_average_data))
      case default
         return
      end select
   end subroutine update_statistical_output

   !> Perform the final time interval averaging on an item,
   !! after all values haven been summed up in %stat_output.
   elemental subroutine finalize_average(item)

      type(t_output_variable_item), intent(inout) :: item !< The item to be processed. Will be double-checked on its operation type.

      if (item%operation_type == SO_AVERAGE) then
         item%stat_output = item%stat_output / item%time_step_sum
      end if

   end subroutine finalize_average

   !> Reset an item's stat_output array, to be called after every output interval.
   elemental subroutine reset_statistical_output(item)
      type(t_output_variable_item), intent(inout) :: item !< Statistical output item to reset

      select case (item%operation_type)
      case (SO_CURRENT)
         return
      case (SO_AVERAGE)
         item%stat_output = 0
         item%time_step_sum = 0 !new sum every output interval
      case (SO_MAX)
         item%stat_output = -huge(1.0_dp)
         item%moving_average_data = create_moving_average_data(size(item%source_input), item%moving_average_window)
      case (SO_MIN)
         item%stat_output = huge(1.0_dp)
         item%moving_average_data = create_moving_average_data(size(item%source_input), item%moving_average_window)
      case default
         return
      end select
   end subroutine reset_statistical_output

   !> Create a new output item and add it to the output set according to output quantity config
   subroutine add_stat_output_items(output_set, output_config, data_pointer, source_input_function_pointer)
      use m_statistical_output_types, only: process_data_interface_double
      use MessageHandling, only: mess, LEVEL_WARN

      type(t_output_variable_set), intent(inout) :: output_set !< Output set that item will be added to
      type(t_output_quantity_config), target, intent(in) :: output_config !< Output quantity config linked to this output item, a pointer to it will be stored in the new output item.
      double precision, pointer, dimension(:), intent(in) :: data_pointer !< Pointer to output quantity data ("source input")
      procedure(process_data_interface_double), optional, pointer, intent(in) :: source_input_function_pointer !< (optional) Function pointer for producing/processing the source data, if no direct data_pointer is available

      type(t_output_variable_item) :: item ! new item to be added
      character(len=len_trim(output_config%input_value)) :: valuestring
      integer :: ierr

      valuestring = output_config%input_value

      do while (len_trim(valuestring) > 0)
         ierr = parse_next_stat_type_from_value_string(valuestring, item%operation_type, item%moving_average_window)
         if (ierr /= SO_NOERR) then
            goto 999
         else if (item%operation_type == SO_NONE) then
            cycle
         else

            ! Disable statistics in time on structures of this type if any of them lie across multiple partitions
            if (model_has_structures_across_partitions(output_config%location_specifier) .and. item%operation_type /= SO_CURRENT) then
               call mess(LEVEL_WARN, 'Disabling output item "'//trim(output_config%name)//'('//trim(operation_type_to_string(item%operation_type))//')"'// &
                         ' as at least one '//trim(get_location_specifier_string(output_config%location_specifier))// &
                         ' lies across multiple partitions, which could produce invalid output')
               cycle
            end if

            output_set%count = output_set%count + 1
            if (output_set%count > output_set%capacity) then
               call reallocate_output_set(output_set)
            end if

            item%output_config => output_config
            item%source_input => data_pointer
            if (present(source_input_function_pointer)) then
               if (associated(source_input_function_pointer)) then
                  item%source_input_function_pointer => source_input_function_pointer
                  ! First "init" call to callback functions, such that %source_input is allocated
                  call item%source_input_function_pointer(item%source_input)
               end if
            end if

            output_set%statout(output_set%count) = item
         end if

      end do

      return ! No error

999   continue ! Some error occurred
      write (msgbuf, '(a,a,a,a,a,a)') 'add_stat_output_items: error while adding items for ', trim(output_config%name), '. Original input was: ', trim(output_config%key), ' = ', trim(output_config%input_value)
      call err_flush()

   end subroutine add_stat_output_items

   !> Check if any structures of the indicated type lie across multiple partitions
   function model_has_structures_across_partitions(location_specifier) result(res)
      use m_structures, only: model_has_weirs_across_partitions, &
                              model_has_general_structures_across_partitions, &
                              model_has_orifices_across_partitions, &
                              model_has_universal_weirs_across_partitions, &
                              model_has_culverts_across_partitions, &
                              model_has_pumps_across_partitions, &
                              model_has_bridges_across_partitions, &
                              model_has_long_culverts_across_partitions, &
                              model_has_dams_across_partitions, &
                              model_has_dambreaks_across_partitions, &
                              model_has_gates_across_partitions, &
                              model_has_compound_structures_across_partitions
      use m_dad, only: model_has_dredge_links_across_partitions
      use m_partitioninfo, only: model_has_crosssections_across_partitions
      use m_laterals, only: model_has_laterals_across_partitions
      integer, intent(in) :: location_specifier !< The location specifier indicating the type of structure (UNC_LOC_XXX)
      logical :: res !< Whether or not any structures of this type lie across multiple partitions

      res = .false.

      select case (location_specifier)
      case default
         return
      case (UNC_LOC_OBSCRS) ! Cross-sections
         res = model_has_crosssections_across_partitions()
      case (UNC_LOC_WEIRGEN) ! Weirs
         res = model_has_weirs_across_partitions
      case (UNC_LOC_GENSTRU) ! General structures
         res = model_has_general_structures_across_partitions
      case (UNC_LOC_ORIFICE) ! Orifices
         res = model_has_orifices_across_partitions
      case (UNC_LOC_UNIWEIR) ! Universal weirs
         res = model_has_universal_weirs_across_partitions
      case (UNC_LOC_CULVERT) ! Culverts
         res = model_has_culverts_across_partitions
      case (UNC_LOC_PUMP) ! Pumps
         res = model_has_pumps_across_partitions
      case (UNC_LOC_BRIDGE) ! Bridges
         res = model_has_bridges_across_partitions
      case (UNC_LOC_LONGCULVERT) ! Long culverts
         res = model_has_long_culverts_across_partitions
      case (UNC_LOC_DRED_LINK) ! Dredge links
         res = model_has_dredge_links_across_partitions
      case (UNC_LOC_DAM) ! Dams
         res = model_has_dams_across_partitions
      case (UNC_LOC_DAMBREAK) ! Dam breaks
         res = model_has_dambreaks_across_partitions
      case (UNC_LOC_GATE) ! Gates
         res = model_has_gates_across_partitions
      case (UNC_LOC_CMPSTRU) ! Compound structures
         res = model_has_compound_structures_across_partitions
      case (UNC_LOC_LATERAL) ! Laterals
         res = model_has_laterals_across_partitions
      end select

   end function model_has_structures_across_partitions

   !> For every item in output_set, allocate arrays depending on its operation_type.
   elemental subroutine initialize_statistical_output(item)
      type(t_output_variable_item), intent(inout) :: item !> output variable item that needs to be initialized

      integer :: input_size

      input_size = size(item%source_input)

      select case (item%operation_type)
      case (SO_CURRENT)
         item%stat_output => item%source_input
      case (SO_AVERAGE)
         allocate (item%stat_output(input_size))
      case (SO_MIN, SO_MAX)
         allocate (item%stat_output(input_size))
      case (SO_NONE)
         continue
      case default
         return
      end select

      call reset_statistical_output(item)
   end subroutine initialize_statistical_output

   !> Obtain a character string describing the statistics operation (for writing to screen)
   function operation_type_to_string(operation_type) result(operation_string)
      integer, intent(in) :: operation_type !< Integer representing the operation type
      character(len=256) :: operation_string !> Character string describing the operation type

      select case (operation_type)
      case default
         operation_string = 'UNKNOWN'
      case (SO_CURRENT)
         operation_string = 'current'
      case (SO_MIN)
         operation_string = 'min'
      case (SO_MAX)
         operation_string = 'max'
      case (SO_AVERAGE)
         operation_string = 'average'
      end select
   end function operation_type_to_string

end module
