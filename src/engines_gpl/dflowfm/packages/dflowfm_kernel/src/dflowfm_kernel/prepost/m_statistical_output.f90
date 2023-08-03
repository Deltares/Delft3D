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
   
module QSCallBack
   abstract interface
      !< function pointer to be called by update_source_data when advanced operations are required and the data to be
      !< written to the his/map file cannot be a pointer but must be calculated and stored every timestep.
      subroutine QS_callbackiface(datapointer)
         double precision, pointer, dimension(:), intent(inout) :: datapointer !< pointer to function in-output data
      end subroutine QS_callbackiface
   end interface
end module QSCallBack
   
!> This module implements the statistical output in D-Flow FM.
module m_statistical_output
   use MessageHandling
   use m_output_config
   use QsCallBack
   
private

   public realloc
   public dealloc
   public update_statistical_output, update_source_data

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
      integer                                   :: operation_id         !< Specifies the kind of operation to perform on the output variable.
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
      procedure(QS_callbackiface), nopass, pointer      :: function_pointer => NULL()          !< function pointer for operation that needs to be performed to produce source_input 
      
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
   elemental subroutine update_moving_average(i)

      type(t_output_variable_item), intent(inout) :: i !< statistical output item to update
      
      integer :: jnew, jold !< Index to newest and oldest timestep in samples array

      jnew = i%current_step
      jold = MOD(i%current_step,i%total_steps_count)+1
   
      !when timestep < windowsize, no samples need to be removed. The timesteps array and samples array will be initialized to 0 so that we can keep the same expression.
      i%moving_average_sum = i%moving_average_sum - i%samples(:,jold)*i%timesteps(jold) + i%samples(:,jnew)*i%timesteps(jnew)
      i%timestep_sum = i%timestep_sum - i%timesteps(jold) + i%timesteps(jnew)
      i%stat_input = i%moving_average_sum/i%timestep_sum
   
   end subroutine update_moving_average

   !> adds a new sample, and its timestep to the samples array. Only needed for moving average calculation.
   elemental subroutine add_statistical_output_sample(i,timestep)

      type(t_output_variable_item), intent(inout) :: item      !< statistical output item to update
      double precision, intent(in)                :: timestep  !< this is usually dts

      i%timesteps(item%current_step) = timestep
      i%samples(:,item%current_step) = i%source_input

   end subroutine add_statistical_output_sample

   ! some variables without a pointer need a separate subroutine call to update their source_data array
   subroutine update_source_data(output_set)
   type(t_output_variable_set),    intent(inout)   :: output_set    !> output set that we wish to update
   type(t_output_variable_item), pointer  :: i
   
   integer :: j
   
   do j = 1, output_set%count
      i => output_set%statout(j)
      if (associated(i%function_pointer)) then
         call i%function_pointer(i%source_input)
      endif
   enddo
   
   end subroutine update_source_data
   
   !> updates a stat_output_item using the stat_input array, depending on the operation_id
   !  stat_input is filled elsewhere and can be a moving average or a pointer to an input variable.
   elemental subroutine update_statistical_output(i,dts)
      
      type(t_output_variable_item), intent(inout) :: i   !< statistical output item to update
      double precision,             intent(in)    :: dts !< current timestep
   
      if (i%operation_id == SO_MIN .or. i%operation_id == SO_MAX ) then ! max/min of moving average requested
         call add_statistical_output_sample(i,dts)
         call update_moving_average(i)
         i%current_step = mod(i%current_step+1,i%total_steps_count)
      endif

      select case (i%operation_id)
      case (SO_CURRENT)
         continue
      case (SO_AVERAGE) 
         i%stat_output = i%stat_output + i%stat_input * dts
         i%timestep_sum = i%timestep_sum + dts
      case (SO_MAX) 
         i%stat_output = max(i%stat_output,i%stat_input)
      case (SO_MIN) 
         i%stat_output = min(i%stat_output,i%stat_input)
      case default
         return
      end select

   end subroutine update_statistical_output

   !> in case average is requested, a division is required before the average can be written
   subroutine finalize_SO_AVERAGE(i) 

      type(t_output_variable_item), intent(inout) :: i !>

      if (i%operation_id == SO_AVERAGE) then 
         i%stat_output = i%stat_output/i%timestep_sum
      endif

   end subroutine finalize_SO_AVERAGE

   !> every output interval the stat_output needs to be reset.
   subroutine reset_statistical_output(i)

      type(t_output_variable_item), intent(inout) :: i !< statistical output item to reset
      
      select case (i%operation_id)
      case (SO_CURRENT)
         continue
      case (SO_AVERAGE)
         i%stat_output = 0 
         i%timestep_sum = 0 !new sum every output interval
      case (SO_MAX)
         i%stat_output = -huge
      case (SO_MIN)
         i%stat_output = huge
      case default 
         call mess(LEVEL_ERROR, 'update_statistical_output: invalid operation_id')
      end select

   end subroutine reset_statistical_output
   
   !> create a new output item and add it to the output set according to output quantity config
   subroutine add_stat_output_item(output_set, output_config, data_pointer, function_pointer)
   use QSCallBack
   
      type(t_output_variable_set), intent(inout) :: output_set             !> output set that items need to be added to
      type(t_output_quantity_config), pointer, intent(in) :: output_config !> output quantity config linked to output item
      double precision, pointer, dimension(:), intent(in) :: data_pointer  !> pointer to output quantity data
      procedure(QS_callbackiface), optional, pointer, intent(in) :: function_pointer !< optional pointer to function that will produce source_input data
      
      type(t_output_variable_item) :: item !> new item to be added
      
      output_set%count = output_set%count + 1
      if (output_set%count > output_set%size) then
         call realloc_stat_output(output_set)
      endif
      
      item%output_config => output_config
      item%operation_id = set_operation_type(output_config)
      item%source_input => data_pointer
      if (present(function_pointer)) then
         item%function_pointer => function_pointer
      endif
      
      output_set%statout(output_set%count) = item
      
   end subroutine add_stat_output_item
   
   integer function set_operation_type(output_config)
   use string_module, only: str_tolower
   
      type(t_output_quantity_config), intent(in) :: output_config          !> output quantity config linked to output item
      
      set_operation_type = -1
      
      if      (trim(str_tolower(output_config%input_value)) == 'current' ) then
         set_operation_type = SO_CURRENT
      else if (trim(str_tolower(output_config%input_value)) == 'average' ) then
         set_operation_type = SO_AVERAGE
      else if (trim(str_tolower(output_config%input_value)) == 'max' ) then
         set_operation_type = SO_MAX
      else if (trim(str_tolower(output_config%input_value)) == 'min' ) then
         set_operation_type = SO_MIN
      endif
         
   end function set_operation_type
      
   !> For every item in output_set, allocate arrays depending on operation id
   subroutine initialize_statistical_output(output_set)
   
      type(t_output_variable_set), intent(inout) :: output_set !> output set that needs to be initialized

      type(t_output_variable_item), pointer  :: i
      integer :: j, inputsize
      logical :: success
      
      do j = 1, output_set%count
         i => output_set%statout(j)
         input_size = size(i%source_input)
         if (associated(i%function_pointer)) then
            call i%function_pointer(i%source_input)
         endif

         select case (i%operation_id)
         case (SO_CURRENT)
            i%stat_output => i%source_input
         case (SO_AVERAGE)
            allocate(i%stat_output(input_size))
            i%stat_input => i%source_input
         case (SO_MIN, SO_MAX)
            allocate(i%stat_output(input_size),i%moving_average_sum(input_size), &
               i%samples(input_size,window_size),i%timesteps(window_size),i%stat_input(input_size))

            i%moving_average_sum = 0
            i%samples = 0
            i%timesteps = 0
            i%timestep_sum = 0
            case default
            call mess(LEVEL_ERROR, 'initialize_statistical_output: invalid operation_id')
         end select

         call reset_statistical_output(i)
      enddo

   end subroutine initialize_statistical_output
   
      
   subroutine flow_init_statistical_output_his(output_config,output_set)
   use m_flow
   use m_flowexternalforcings
   use m_structures
   use m_observations
   use m_sediment, only: stm_included
   use fm_statistical_output, only: aggregate_constituent_data
   USE, INTRINSIC :: ISO_C_BINDING
   
      type(t_output_variable_set),    intent(inout)   :: output_set    !> output set that items need to be added to
      type(t_output_quantity_config_set), intent(in)  :: output_config !> output config for which an output set is needed.
      double precision, pointer, dimension(:) :: temp_pointer
      procedure(QS_callbackiface),  pointer :: function_pointer => NULL()
      
      integer :: i, ntot
      
      ntot = numobs + nummovobs
    
      !
      ! Mass balance variables
      !
      if (jahisbal > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VOLTOT                     ),voltot(IDX_VOLTOT:IDX_VOLTOT)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_STOR                       ),voltot(2:2)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VOLERR                     ),voltot(3:3)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDIN                      ),voltot(4:4)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDOUT                     ),voltot(5:5)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDTOT                     ),voltot(6:6)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHIN                     ),voltot(7:7)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHOUT                    ),voltot(8:8)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHTOT                    ),voltot(9:9)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PRECIP_TOTAL               ),voltot(10:10)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EVAP                       ),voltot(11:11)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOUR                       ),voltot(12:12)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INTERNALTIDESDISSIPATION   ),voltot(13:13)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GravInput                  ),voltot(14:14)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SalInput                   ),voltot(15:15)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SalInput2                  ),voltot(16:16)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWIN                      ),voltot(17:17)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWOUT                     ),voltot(18:18)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWTOT                     ),voltot(19:19)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN                      ),voltot(20:20)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT                     ),voltot(21:21)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT                     ),voltot(22:22)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN1D                    ),voltot(23:23)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT1D                   ),voltot(24:24)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT1D                   ),voltot(25:25)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN2D                    ),voltot(26:26)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT2D                   ),voltot(27:27)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT2D                   ),voltot(28:28)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN                      ),voltot(29:29)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT                     ),voltot(30:30)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT                     ),voltot(31:31)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN1D                    ),voltot(32:32)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT1D                   ),voltot(33:33)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT1D                   ),voltot(34:34)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN2D                    ),voltot(35:35)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT2D                   ),voltot(36:36)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT2D                   ),voltot(37:37)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ICEPT                      ),voltot(38:38)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EVAP_ICEPT                 ),voltot(39:39)                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PRECIP_GROUND              ),voltot(40:40)                                  )
      endif

      !
      ! Source-sink variables
      !
      if (jahissourcesink > 0 .and. numsrc > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_DISCHARGE),               qstss(1:(numconst+1)*numsrc:(numconst+1)))
         i = 1
         if (isalt > 0) then
            i = i + 1
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_SALINITY_INCREMENT),   qstss(i:(numconst+1)*numsrc:(numconst+i)))
         endif
         if (itemp > 0) then
            i = i + 1
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_TEMPERATURE_INCREMENT),qstss(i:(numconst+1)*numsrc:(numconst+i)))
         endif
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_CURRENT_DISCHARGE),qsrc)
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_CUMULATIVE_VOLUME),vsrccum)
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_DISCHARGE_AVERAGE),qsrcavg)
      endif

      !
      ! Hydraulic structures variables
      !
      if (jahiscgen > 0 .and. ngenstru > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE            ),valgenstru(IVAL_DIS   ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_CREST_LEVEL          ),valgenstru(IVAL_CRESTL,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_LOWER_EDGE_LEVEL),valgenstru(IVAL_EDGEL ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_WIDTH   ),valgenstru(IVAL_OPENW ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1UP                 ),valgenstru(IVAL_S1UP  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1DN                 ),valgenstru(IVAL_S1DN  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_HEAD                 ),valgenstru(IVAL_HEAD  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA            ),valgenstru(IVAL_AREA  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY             ),valgenstru(IVAL_VEL   ,1:ngenstru))
      endif
      if (network%sts%numGeneralStructures > 0) then ! write extra fields for new general structure
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_CREST_WIDTH                   ),valgenstru(IVAL_CRESTW   ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_THROUGH_GATE_OPENING),valgenstru(IVAL_DIS_OPEN ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_OVER_GATE           ),valgenstru(IVAL_DIS_OVER ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_UNDER_GATE          ),valgenstru(IVAL_DIS_UNDER,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_HEIGHT           ),valgenstru(IVAL_OPENH    ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_UPPER_EDGE_LEVEL         ),valgenstru(IVAL_UPPL     ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_THROUGH_GATE_OPENING ),valgenstru(IVAL_VEL_OPEN ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_OVER_GATE            ),valgenstru(IVAL_VEL_OVER ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_UNDER_GATE           ),valgenstru(IVAL_VEL_UNDER,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_IN_GATE_OPENING     ),valgenstru(IVAL_AREA_OPEN ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_OVER_GATE           ),valgenstru(IVAL_AREA_OVER ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_UNDER_GATE          ),valgenstru(IVAL_AREA_UNDER,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_STATE                         ),valgenstru(IVAL_STATE     ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1_ON_CREST                   ),valgenstru(IVAL_S1ONCREST ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FORCE_DIFFERENCE              ),valgenstru(IVAL_FORCEDIF  ,1:network%sts%numGeneralStructures))
      endif
      if (jahiscdam > 0 .and. ncdamsg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_DISCHARGE           ), valcdam(2,:)                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_CREST_LEVEL         ), zcdam                      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_S1UP                ), valcdam(3,:)                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_S1DN                ), valcdam(4,:)                  )
      endif
      if (jahispump > 0 .and. npumpsg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_STRUCTURE_DISCHARGE),valgenstru(IVAL_DIS       ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_CAPACITY           ),valgenstru(IVAL_PP_CAP    ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_DISCHARGE_DIR      ),valgenstru(IVAL_PP_DISDIR ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1UP               ),valgenstru(IVAL_S1UP      ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1DN               ),valgenstru(IVAL_S1DN      ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_STRUCTURE_HEAD     ),valgenstru(IVAL_PP_HEAD   ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_ACTUAL_STAGE       ),valgenstru(IVAL_PP_STAG   ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_HEAD               ),valgenstru(IVAL_PP_HEAD   ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_REDUCTION_FACTOR   ),valgenstru(IVAL_PP_RED    ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1_DELIVERY_SIDE   ),valgenstru(IVAL_PP_S1DEL  ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1_SUCTION_SIDE    ),valgenstru(IVAL_PP_S1SUC  ,1:npumpsg))
      endif                                                                                          
      if (jahisgate > 0 .and. ngatesg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_DISCHARGE       ),valgate(2,:)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_LOWER_EDGE_LEVEL),zgate                                          )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_S1UP            ),valgate(3,:)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_S1DN            ),valgate(4,:)                                   )
      endif
      if (jahisgate > 0 .and. ngategen > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_DISCHARGE            ),valgategen(IVAL_DIS,:)                                 )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_CREST_LEVEL          ),valgategen(IVAL_GATE_SILLH,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_CREST_WIDTH          ),valgategen(IVAL_WIDTH,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_GATE_LOWER_EDGE_LEVEL),valgategen(IVAL_GATE_EDGEL,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_FLOW_THROUGH_HEIGHT  ),valgategen(IVAL_GATE_FLOWH,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_GATE_OPENING_WIDTH   ),valgategen(IVAL_GATE_OPENW,:)                                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_S1UP                 ),valgategen(IVAL_S1UP,:)                                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_S1DN                 ),valgategen(IVAL_S1DN,:)    )
      endif
      if (jahisweir > 0 .and. nweirgen > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_DISCHARGE       ),valweirgen(IVAL_DIS       ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_CREST_LEVEL     ),valweirgen(IVAL_CRESTL    ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_CREST_WIDTH     ),valweirgen(IVAL_CRESTW    ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1UP            ),valweirgen(IVAL_S1UP      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1DN            ),valweirgen(IVAL_S1DN      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_STRUCTURE_HEAD  ),valweirgen(IVAL_HEAD      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_VELOCITY        ),valweirgen(IVAL_VEL       ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_FLOW_AREA       ),valweirgen(IVAL_AREA      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_STATE           ),valweirgen(IVAL_STATE     ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_FORCE_DIFFERENCE),valweirgen(IVAL_FORCEDIF  ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1_ON_CREST     ),valweirgen(IVAL_S1ONCREST ,1:nweirgen))
      endif
      if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_DISCHARGE            ),valorifgen(IVAL_DIS       ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_CREST_LEVEL          ),valorifgen(IVAL_CRESTL    ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_CREST_WIDTH          ),valorifgen(IVAL_CRESTW    ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_GATE_LOWER_EDGE_LEVEL),valorifgen(IVAL_S1UP      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1UP                 ),valorifgen(IVAL_S1DN      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1DN                 ),valorifgen(IVAL_HEAD      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_GATE_OPENING_HEIGHT  ),valorifgen(IVAL_VEL       ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_HEAD                 ),valorifgen(IVAL_AREA      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_FLOW_AREA            ),valorifgen(IVAL_STATE     ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_STATE                ),valorifgen(IVAL_FORCEDIF  ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1_ON_CREST          ),valorifgen(IVAL_S1ONCREST ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_VELOCITY             ),valorifgen(IVAL_S1ONCREST ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_FORCE_DIFFERENCE     ),valorifgen(IVAL_S1ONCREST ,1:network%sts%numOrifices))
      endif
      if (jahisbridge > 0 .and. network%sts%numBridges > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_DISCHARGE ),valbridge(IVAL_DIS,     1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_S1UP      ),valbridge(IVAL_S1UP,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_S1DN      ),valbridge(IVAL_S1DN,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_HEAD      ),valbridge(IVAL_HEAD,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_FLOW_AREA ),valbridge(IVAL_AREA,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_VELOCITY  ),valbridge(IVAL_VEL,     1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BLUP      ),valbridge(IVAL_BLUP,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BLDN      ),valbridge(IVAL_BLDN,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BL_ACTUAL ),valbridge(IVAL_BLACTUAL,1:network%sts%numBridges)        )
      endif
      if (jahisculv > 0 .and. network%sts%numCulverts > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_DISCHARGE            ),valculvert(IVAL_DIS        ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_CREST_LEVEL          ),valculvert(IVAL_CL_CRESTL  ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_GATE_LOWER_EDGE_LEVEL),valculvert(IVAL_CL_EDGEL   ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_S1UP                 ),valculvert(IVAL_S1UP       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_S1DN                 ),valculvert(IVAL_S1DN       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_GATE_OPENING_HEIGHT  ),valculvert(IVAL_CL_OPENH   ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_HEAD                 ),valculvert(IVAL_HEAD       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_FLOW_AREA            ),valculvert(IVAL_AREA       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_VELOCITY             ),valculvert(IVAL_VEL        ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_STATE                ),valculvert(IVAL_CL_STATE   ,1:network%sts%numCulverts)            )
      endif
      if (jahisdambreak > 0 .and. ndambreak > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_S1UP                        ),valdambreak(IVAL_S1UP,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_S1DN                        ),valdambreak(IVAL_S1DN,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_DISCHARGE                   ),valdambreak(IVAL_DIS,1:ndambreak)       )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CUMULATIVE_DISCHARGE        ),valdambreak(IVAL_DB_DISCUM,1:ndambreak) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_VELOCITY                    ),valdambreak(IVAL_VEL,1:ndambreak)       )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_HEAD                        ),valdambreak(IVAL_HEAD,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_FLOW_AREA                   ),valdambreak(IVAL_AREA,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CREST_LEVEL                 ),valdambreak(IVAL_DB_CRESTH,1:ndambreak) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CREST_WIDTH                 ),valdambreak(IVAL_DB_CRESTW,1:ndambreak) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_BREACH_WIDTH_TIME_DERIVATIVE),valdambreak(IVAL_DB_TIMEDIV,1:ndambreak))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_WATER_LEVEL_JUMP            ),valdambreak(IVAL_DB_JUMP,1:ndambreak)   )
      endif
      if (jahisuniweir > 0 .and. network%sts%numuniweirs > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_DISCHARGE  ),valuniweir(IVAL_DIS,1:network%sts%numuniweirs)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_CREST_LEVEL),valuniweir(IVAL_UW_CRESTL,1:network%sts%numuniweirs))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_S1UP       ),valuniweir(IVAL_S1DUP,1:network%sts%numuniweirs)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_S1DN       ),valuniweir(IVAL_S1DN,1:network%sts%numuniweirs)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_HEAD       ),valuniweir(IVAL_HEAD,1:network%sts%numuniweirs)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_FLOW_AREA  ),valuniweir(IVAL_AREA,1:network%sts%numuniweirs)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_VELOCITY   ),valuniweir(IVAL_VEL,1:network%sts%numuniweirs)      )
      endif
      if (jahiscmpstru > 0 .and. network%cmps%count > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_DISCHARGE),valcmpstru(IVAL_DIS,1:network%cmps%count) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_S1UP     ),valcmpstru(IVAL_S1UP,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_S1DN     ),valcmpstru(IVAL_S1DN,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_HEAD     ),valcmpstru(IVAL_HEAD,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_FLOW_AREA),valcmpstru(IVAL_AREA,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_VELOCITY ),valcmpstru(IVAL_VEL,1:network%cmps%count) )
      endif
      if (jahislongculv > 0 .and. nlongculverts > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_DISCHARGE             ),vallongculvert(IVAL_DIS,1:nlongculverts)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_S1UP                  ),vallongculvert(IVAL_S1UP,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_S1DN                  ),vallongculvert(IVAL_S1DN,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_HEAD                  ),vallongculvert(IVAL_HEAD,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_FLOW_AREA             ),vallongculvert(IVAL_AREA,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_VELOCITY              ),vallongculvert(IVAL_VEL,1:nlongculverts)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_VALVE_RELATIVE_OPENING),vallongculvert(IVAL_LC_VALVE,1:nlongculverts))
      endif
      !
      ! Variables on observation stations
      !
      if ( kmx.gt.0 ) then
         if (iturbulencemodel >= 3 .and. jahistur > 0) then
            call c_f_pointer (c_loc(valobs(IPNT_TKIN:IPNT_TKIN+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TKE      ),temp_pointer                            )
         endif
         if (iturbulencemodel == 3 .and. jahistur >0) then
            call c_f_pointer (c_loc(valobs(IDX_HIS_EPS:IDX_HIS_EPS+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EPS      ),temp_pointer                     )
         endif
         if (iturbulencemodel > 1) then
            call c_f_pointer (c_loc(valobs(IPNT_VICWW:IPNT_VICWW+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VICWW    ),temp_pointer                         )
         endif
         if (iturbulencemodel == 4 .and. jahistur > 0) then
            call c_f_pointer (c_loc(valobs(id_tureps:id_tureps+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAU     ),temp_pointer                      )
         endif
      endif
      if (jawind > 0 .and. jahiswind > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDX         ),valobs(IPNT_wx,:)                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDX_SFERIC  ),valobs(IPNT_wx,:)                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDY         ),valobs(IPNT_wy,:)                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDY_SFERIC  ),valobs(IPNT_wy,:)                )
      endif
      if (jarain > 0 .and. jahisrain > 0) then
      call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RAIN            ),valobs(IPNT_rain,:)                      )
      endif
      if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INFILTRATION_CAP)                 ,valobs(IPNT_infiltcap,:)                                          )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL) ,valobs(IPNT_infiltact,:)            )
      endif
      if (jatem > 1 .and. jahisheatflux > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WIND),valobs(IPNT_WIND,:)                               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAIR),valobs(IPNT_TAIR,:)                               )
         if (jatem == 5 .and. allocated(Rhum) .and. allocated(Clou) ) then
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RHUM),valobs(IPNT_RHUM,:)                                    )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CLOU),valobs(IPNT_CLOU,:)                                    )
         endif
         if (jatem == 5 ) then
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QSUN  ),valobs(IPNT_QSUN,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QEVA  ),valobs(IPNT_QEVA,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QCON  ),valobs(IPNT_QCON,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QLONG ),valobs(IPNT_QLON,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QFREVA),valobs(IPNT_QFRE,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QFRCON),valobs(IPNT_QFRC,:)                                     )
            endif
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QTOT  ),valobs(IPNT_RHUM,:)                                     )
      endif
      if (jasal > 0 .and. jahissal > 0) then
         if (kmx>0) then
            call c_f_pointer (c_loc(valobs(IPNT_SA1:IPNT_SA1+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SALINITY   ),temp_pointer                                )
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SALINITY   ),valobs(IPNT_SA1,:)                                )
         endif
      endif
      if( (jasal > 0 .or. jatem > 0 .or. jased > 0 )  .and. jahisrho > 0) then
         if (kmx>0) then
            call c_f_pointer (c_loc(valobs(IPNT_RHOP:IPNT_RHOP+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_POTENTIAL_DENSITY   ),temp_pointer                                )
            
            call c_f_pointer (c_loc(valobs(IPNT_BRUV:IPNT_BRUV+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRUNT_VAISALA_N2),temp_pointer                              )
            if (idensform > 10) then
               call c_f_pointer (c_loc(valobs(IPNT_RHO:IPNT_RHO+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DENSITY),temp_pointer                                )
            endif
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_POTENTIAL_DENSITY),valobs(IPNT_RHOP,:)                              )
         endif
      endif
      if (ntot > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WATERLEVEL),valobs(IPNT_S1,:)                               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BEDLEVEL)  ,valobs(IPNT_BL,:)                             )
         if (jahiswatdep > 0) then
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WATERDEPTH),valobs(IPNT_HS,:)                               )
         endif
      endif
      if (jawave>0 .and. jahiswav > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_HWAV    ),valobs(IPNT_WAVEH,:)                                    )
         !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_HWAV_SIG),valobs(IPNT_HS,:)                                    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TWAV    ),valobs(IPNT_WAVET,:)                                    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PHIWAV  ),valobs(IPNT_WAVED,:)                                    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RLABDA  ),valobs(IPNT_WAVEL,:)                              )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UORB    ),valobs(IPNT_WAVEU,:)                              )
         if ( kmx>0 .and. .not. flowwithoutwaves) then
            call c_f_pointer (c_loc(valobs(IPNT_UCXST:IPNT_UCXST+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_USTOKES),temp_pointer)
            
            call c_f_pointer (c_loc(valobs(IPNT_UCYST:IPNT_UCYST+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VSTOKES),temp_pointer)
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_USTOKES),valobs(IPNT_UCXST,:)                                 )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VSTOKES),valobs(IPNT_UCYST,:)                                 )
         endif
      endif
      if( jahisvelvec > 0 ) then
         if (numobs+nummovobs > 0) then
            if ( kmx>0 ) then
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DEPTH_AVERAGED_X_VELOCITY),valobs(IPNT_UCXQ,:)             )
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DEPTH_AVERAGED_Y_VELOCITY),valobs(IPNT_UCYQ,:)             )

               call c_f_pointer (c_loc(valobs(IPNT_UCX:IPNT_UCX+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_X_VELOCITY),temp_pointer)
               
               call c_f_pointer (c_loc(valobs(IPNT_UCY:IPNT_UCY+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Y_VELOCITY),temp_pointer)
               
               call c_f_pointer (c_loc(valobs(IPNT_UCZ:IPNT_UCZ+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Z_VELOCITY),temp_pointer)
            else
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_X_VELOCITY),valobs(IPNT_UCX,:)                             )
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Y_VELOCITY),valobs(IPNT_UCY,:)                             )
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Z_VELOCITY),valobs(IPNT_UCZ,:)                            )
            endif
         endif
      endif
      if (jased > 0 .and. .not. stm_included) then
         if (kmx >0) then
               call c_f_pointer (c_loc(valobs(IPNT_SED:IPNT_SED+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SED),temp_pointer)
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SED),valobs(IPNT_SED,:)                                  )
         endif
      endif
      if (IVAL_WS1 > 0) then
         if (kmx > 0) then
            call c_f_pointer (c_loc(valobs(IPNT_WS1:IPNT_WS1+(IVAL_WSN-IVAL_WS1*kmx),1:ntot)), temp_pointer, [(IVAL_WSN-IPNT_WS1)*kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WS),temp_pointer                                                        )
         else
            call c_f_pointer (c_loc(valobs(IPNT_WS1:IVAL_WSN,1:ntot)), temp_pointer, [(IVAL_WSN-IPNT_WS1)*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WS),temp_pointer                                                        )
         endif
      endif
      if (IVAL_SEDDIF1 > 0) then
         call c_f_pointer (c_loc(valobs(IPNT_WS1:IPNT_WS1+(IVAL_WSN-IVAL_WS1*kmx),1:ntot)), temp_pointer, [(IVAL_WSN-IPNT_WS1)*kmx*ntot])
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SEDDIF),temp_pointer                                          )
      endif

      !
      ! Variables on observation cross sections
      !
      if (ncrs > 0 .and. NUMCONST_MDU > 0) then
         function_pointer => aggregate_constituent_data
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CONSTITUENTS),NULL(),function_pointer                                 )
      endif
      

      !
      ! Variables on lateral discharges
      !
      if (jahislateral > 0 .and. numlatsg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS),qplat               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE      ),qplatAve               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS  ),qLatReal               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE        ),qLatRealAve               )
      endif
      if (jahistaucurrent>0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAUSX),valobs(IPNT_TAUX,:)                                                    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAUSY),valobs(IPNT_TAUY,:)                                                    )
      endif
      if (jahisvelocity > 0) then
         if (jaeulervel==0) then
            if(kmx>0) then
               call c_f_pointer (c_loc(valobs(IPNT_UMAG:IPNT_UMAG+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE),temp_pointer)
            else
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE),valobs(IPNT_UMAG,:)                                        )
            endif
         else
            if(kmx>0) then
               call c_f_pointer (c_loc(valobs(IPNT_UMAG:IPNT_UMAG+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN),temp_pointer)
            else
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN),valobs(IPNT_UMAG,:)                                        )
            endif
         endif
      endif
      if (jahisdischarge > 0) then
         if(kmx>0) then
            call c_f_pointer (c_loc(valobs(IPNT_QMAG:IPNT_QMAG+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DISCHARGE_MAGNITUDE),temp_pointer)
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DISCHARGE_MAGNITUDE),valobs(IPNT_QMAG,:)                                       )
         endif
      endif
      if (idensform > 0 .and. jaRichardsononoutput > 0 .and. kmx > 0) then
         call c_f_pointer (c_loc(valobs(IPNT_RICH:IPNT_RICH+kmx,1:ntot)), temp_pointer, [kmx*ntot])
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RICH),temp_pointer)
      endif

         
         
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_S0                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_S1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_POTEVAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_ACTEVAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_PRESCREVAP                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VOL1                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATERDEPTH                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HU                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NEGDPT                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NEGDPT_CUM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NOITER                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NOITER_CUM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_LIMTSTEP                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_LIMTSTEP_CUM                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_COURANT                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_AU                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_U1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_U0                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCXQ_EULERIAN                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCYQ_EULERIAN                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCXQ                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCYQ                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAG_EULER                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAGA_GLM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAGA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WW1                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RHO                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VIU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DIU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Q1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Q1_MAIN                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FIXED_WEIR_ENERGY_LOSS                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SPIRCRV                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SPIRINT                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUMLIMDT                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUS                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSMAX                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Z0UCUR                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Z0UROU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SA1                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CZS                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CZU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFUTYP                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TEM1                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CONST                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MORS                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TURKIN1                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VICWWU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1_3                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1_4                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_0                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_1                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_2                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFCL                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RAINFALL_RATE                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_INTERCEPTION_WATERDEPTH                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_PATM                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDXU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDYU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDX_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDY_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDXU_SFERIC                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDYU_SFERIC                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSX                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSY                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSX_SFERIC                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSY_SFERIC                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAIR                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RHUM                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CLOU                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QSUN                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QEVA                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QCON                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QLONG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QFREVA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QFRCON                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QTOT                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TIDALPOTENTIAL                                            )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SALPOTENTIAL                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_INTERNAL_TIDES_DISSIPATION                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TNUDGE                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_TEM                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_SAL                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_DTEM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_DSAL                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HWAV_SIG                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TP                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DIR                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXBWAV                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYBWAV                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MX                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MY                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DISSURF                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DISWCAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UORB                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_E                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_R                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DR                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_D                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DF                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXX                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYY                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXY                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CGWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SIGMWAV                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_KWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CTHETA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_L1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SWE                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SWT                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UST_CC                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VST_CC                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_USTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VSTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_THETAMEAN                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FX                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FY                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WAVFU                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WAVFV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DTCELL                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TIME_WATER_ON_GROUND                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FREEBOARD                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATERDEPTH_ON_GROUND                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VOLUME_ON_GROUND                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_1D2D                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_1D2D                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_LATERAL                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_LATERAL                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATER_LEVEL_GRADIENT                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QIN                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_S1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_WATERDEPTH                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCMAG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCMAG_EULER                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCDIR                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCDIR_EULER                                               )
      !
      
      call initialize_statistical_output(output_set)
         
   end subroutine flow_init_statistical_output_his

end module
