!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

submodule(m_dambreak_breach) m_dambreak_breach_submodule
   use precision, only: dp

   implicit none

   integer, parameter :: UPSTREAM = 1
   integer, parameter :: DOWNSTREAM = 2
   integer, dimension(2) :: n_averaging !< nr of dambreak signals with averaging (upstream 1st value, downstream 2nd value)
   integer, dimension(:, :), allocatable :: averaging_mapping !< mapping of dambreak averaging (upstream in 1st row, downstream in 2nd row)
   real(kind=dp), dimension(2, 1) :: weighted_sum !< (1,:) weighted sum of water level or velocity over dambreak
                                                  !! (2,:) weighted sum of unity over dambreak
   real(kind=dp), allocatable, target :: levels_widths_from_table(:) !< dambreak heights and widths
   integer, dimension(:), allocatable :: first_link !< first dambreak link for each signal
   integer, dimension(:), allocatable :: last_link !< last dambreak link for each signal

   type :: t_dambreak !< dambreak data for a single dambreak
      integer :: algorithm = 0
      integer :: breach_start_link = -1
      integer :: index_structure = 0
      integer :: number_of_links = 0
      integer :: phase = 0
      integer :: shift_in_link_array = 0
      integer, dimension(:), allocatable :: link_indices
      integer, dimension(:), allocatable :: active_links
      integer, dimension(:), allocatable :: upstream_link_ids
      integer, dimension(:), allocatable :: downstream_link_ids
      character(len=128) :: name = ""
      real(kind=dp) :: breach_depth = 0.0_dp
      real(kind=dp) :: breach_width = 0.0_dp
      real(kind=dp) :: level_from_table = 0.0_dp
      real(kind=dp) :: width_from_table = 0.0_dp
      real(kind=dp) :: upstream_level
      real(kind=dp) :: downstream_level
      real(kind=dp) :: t0 = 0.0_dp
      real(kind=dp) :: width = 0.0_dp
      real(kind=dp) :: maximum_width = 0.0_dp
      real(kind=dp) :: crest_level
      real(kind=dp) :: crest_level_ini
      real(kind=dp) :: normal_velocity
      real(kind=dp) :: breach_width_derivative
      real(kind=dp) :: water_level_jump
   end type

   type(t_dambreak), target, dimension(:), allocatable :: dambreaks(:) !< dambreak data for all dambreaks

   integer :: number_of_mappings !< number of mappings for upstream and downstream water levels
   type :: t_mapping !< mapping of water level
      integer :: location = 0
      real(kind=dp), pointer :: water_level
   end type
   type(t_mapping), dimension(:), allocatable :: mappings !< mapping of water levels

   procedure(calculate_dambreak_widening_any), pointer :: calculate_dambreak_widening

   abstract interface
      subroutine calculate_dambreak_widening_any(remainder, left_side, right_side, left_breach_width, right_breach_width)
         use precision, only: dp
         real(kind=dp), intent(in) :: remainder !< remaining width
         real(kind=dp), intent(in) :: left_side !< left side of the breach
         real(kind=dp), intent(in) :: right_side !< right side of the breach
         real(kind=dp), intent(inout) :: left_breach_width !< width of the breach on the "left" [m]
         real(kind=dp), intent(inout) :: right_breach_width !< width of the breach on the "right" [m]
      end subroutine
   end interface

   integer, parameter :: KX = 2
   integer, parameter, dimension(1) :: KDUM = 1
   real(kind=dp), parameter, dimension(1) :: XDUM = 1.0_dp, YDUM = 1.0_dp
   character(len=*), parameter :: QID = 'dambreakLevelsAndWidths'
   integer, dimension(1), parameter :: UNITY_INDEX = 1
   integer, dimension(1) :: last_index

contains

   !> allocate arrays and initialize variables
   subroutine allocate_and_initialize_dambreak_data()
      use m_alloc, only: realloc

      integer :: n !< loop index

      if (allocated(dambreaks)) then
         do n = 1, size(dambreaks)
            if (allocated(dambreaks(n)%link_indices)) then
               deallocate (dambreaks(n)%link_indices)
               deallocate (dambreaks(n)%active_links)
               deallocate (dambreaks(n)%upstream_link_ids)
               deallocate (dambreaks(n)%downstream_link_ids)
            end if
         end do
         deallocate (dambreaks)
      end if

      allocate (dambreaks(n_db_signals))
      do n = 1, n_db_signals
         dambreaks(n)%shift_in_link_array = first_link(n) - 1
         if (first_link(n) > last_link(n)) then
            allocate (dambreaks(n)%link_indices(0))
            allocate (dambreaks(n)%active_links(0))
            allocate (dambreaks(n)%upstream_link_ids(0))
            allocate (dambreaks(n)%downstream_link_ids(0))
         else
            allocate (dambreaks(n)%link_indices(last_link(n) - first_link(n) + 1), source=0)
            allocate (dambreaks(n)%active_links(last_link(n) - first_link(n) + 1), source=0)
            allocate (dambreaks(n)%upstream_link_ids(last_link(n) - first_link(n) + 1), source=0)
            allocate (dambreaks(n)%downstream_link_ids(last_link(n) - first_link(n) + 1), source=0)
         end if
         dambreaks(n)%number_of_links = size(dambreaks(n)%link_indices)
      end do

      if (allocated(mappings)) then
         deallocate (mappings)
      end if
      allocate (mappings(2 * n_db_signals))

      call realloc(levels_widths_from_table, n_db_signals * 2, fill=0.0_dp)
      call realloc(averaging_mapping, [n_db_signals, 2], fill=0)
      number_of_mappings = 0
      n_averaging(:) = 0

   end subroutine allocate_and_initialize_dambreak_data

   !> updates dambreak breach by updating water levels upstream and downstream and calculating dambreak widths
   module function update_dambreak_breach(start_time, delta_time) result(error)
      use m_flow, only: s1, hu, au, u1
      use m_missing, only: dmiss
      use m_partitioninfo, only: get_average_quantity_from_links

      real(kind=dp), intent(in) :: start_time !< start time
      real(kind=dp), intent(in) :: delta_time !< delta time
      integer :: error !< error code

      integer :: n
      integer :: signal

      error = 0

      if (n_db_signals <= 0) then
         return
      end if
      ! Variable n_db_signals is >0 for all partitions if there is a dambreak, even if it is outside
      ! of a partition. In a parallel simulation, we need to run this subroutine even in a special situation that there is
      ! no dambreak on the current subdomain (i.e. n_db_links == 0), because the following function get_average_quantity_from_links
      ! involves mpi communication among all subdomains. However, in this special situation,
      ! all the necessary variables are set to 0 and do not participate the dambreak related computation in this subroutine.

      call reset_dambreak_variables()

      do n = 1, number_of_mappings
         mappings(n)%water_level = s1(mappings(n)%location)
      end do

      do n = 1, n_averaging(UPSTREAM)
         signal = averaging_mapping(n, UPSTREAM)
         call update_water_levels_with_averaging(start_time, signal, dambreaks(signal)%upstream_link_ids,&
             dambreaks(signal)%active_links, dambreaks(signal)%upstream_level, error)
         if (error /= 0) then
            return
         end if
      end do

      do n = 1, n_averaging(DOWNSTREAM)
         signal = averaging_mapping(n, DOWNSTREAM)
         call update_water_levels_with_averaging(start_time, signal, dambreaks(signal)%downstream_link_ids, &
             dambreaks(signal)%active_links, dambreaks(signal)%downstream_level, error)
         if (error /= 0) then
            return
         end if
      end do

      do n = 1, n_db_signals
         ! u1 velocity on the flowlinks (averaged by the wetted area). The mask is the water level itself
         last_index = dambreaks(n)%number_of_links
         weighted_sum(:, :) = 0.0_dp
         error = get_average_quantity_from_links(UNITY_INDEX, last_index, au, dambreaks(n)%link_indices, u1, &
                                                 dambreaks(n)%link_indices, weighted_sum, 1, hu, dmiss, &
                                                 dambreaks(n)%active_links, 0)
         if (error /= 0) then
            return
         end if

         if (dambreaks(n)%number_of_links > 0) then
            if (dambreaks(n)%index_structure /= 0 .and. weighted_sum(2, 1) > 0.0_dp) then
               dambreaks(n)%normal_velocity = weighted_sum(1, 1) / weighted_sum(2, 1)
            end if
         end if
      end do

      if (n_db_links > 0) then
         call calculate_dambreak_widths(start_time, delta_time)
      end if

   end function update_dambreak_breach

   !> reset dambreak variables like water levels, averaged values etc.
   subroutine reset_dambreak_variables()

      integer :: n !< index of the current dambreak signal

      do n = 1, n_db_signals
         if (dambreaks(n)%index_structure <= 0) then
            continue
         end if
         dambreaks(n)%normal_velocity = 0.0_dp
         dambreaks(n)%breach_width_derivative = 0.0_dp
         dambreaks(n)%water_level_jump = 0.0_dp
         dambreaks(n)%upstream_level = 0.0_dp
         dambreaks(n)%downstream_level = 0.0_dp
      end do
   end subroutine reset_dambreak_variables

   !> update water levels using averaging
   subroutine update_water_levels_with_averaging(start_time, signal, link_ids, active_link_ids, water_level, error)
      use m_flow, only: s1, hu
      use m_partitioninfo, only: get_average_quantity_from_links
      use m_flowgeom, only: wu
      use m_missing, only: dmiss

      real(kind=dp), intent(in) :: start_time !< start time
      integer, intent(in) :: signal !< current dambreak signal
      integer, dimension(:), intent(in) :: link_ids !< upstream or downstream link ids
      integer, dimension(:), intent(in) :: active_link_ids !< active link ids
      real(kind=dp), intent(inout) :: water_level !< water level
      integer, intent(out) :: error !< error code

      error = 0

      weighted_sum(:, :) = 0.0_dp
      last_index = dambreaks(signal)%number_of_links
      error = get_average_quantity_from_links(UNITY_INDEX, last_index, wu, &
                                              link_ids, s1, link_ids, weighted_sum, &
                                              0, hu, dmiss, active_link_ids, 0)
      if (error /= 0) then
         return
      end if
      if (dambreaks(signal)%number_of_links > 0) then
         if (weighted_sum(2, 1) > 0.0_dp) then
            water_level = weighted_sum(1, 1) / weighted_sum(2, 1)
         else if (abs(start_time - dambreaks(signal)%T0) < 1e-10_dp) then
            water_level = s1(link_ids(dambreaks(signal)%breach_start_link-dambreaks(signal)%shift_in_link_array))
         else
            continue
         end if
      end if

   end subroutine update_water_levels_with_averaging

   !> calculate dambreak widths
   subroutine calculate_dambreak_widths(start_time, delta_time)
      use unstruc_channel_flow, only: network
      use m_dambreak, only: BREACH_GROWTH_VDKNAAP, BREACH_GROWTH_VERHEIJVDKNAAP, BREACH_GROWTH_TIMESERIES
      use m_meteo, only: ec_gettimespacevalue_by_itemID, ecInstancePtr, item_db_levels_widths_table
      use m_flowtimes, only: irefdate, tunit, tzone

      real(kind=dp), intent(in) :: start_time !< start_time
      real(kind=dp), intent(in) :: delta_time !< delta_time

      integer :: n !< index of the current dambreak signal
      integer :: i_structure !< index of the structure
      logical :: success !< success flag

      do n = 1, n_db_signals
         i_structure = dambreaks(n)%index_structure
         if (i_structure == 0) then
            continue
         end if
         if (dambreaks(n)%algorithm == BREACH_GROWTH_VDKNAAP .or. &
             dambreaks(n)%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then
            call prepare_dambreak_calculation(network%sts%struct(i_structure)%dambreak, dambreaks(n), &
                                              dambreaks(n)%upstream_level, dambreaks(n)%downstream_level, start_time, delta_time)
         end if
         if (dambreaks(n)%algorithm == BREACH_GROWTH_TIMESERIES .and. &
             start_time > dambreaks(n)%t0) then
            !Time in the tim file is relative to the start time
            success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_db_levels_widths_table, &
                                                     irefdate, tzone, tunit, start_time - dambreaks(n)%t0, &
                                                     levels_widths_from_table)
            ! NOTE: AvD: the code above works correctly, but is dangerous:
            ! the addtimespace for dambreak has added each dambreak separately with a targetoffset.
            ! The gettimespace above, however, gets the values for *all* dambreaks, but with the relative time
            ! of the *current* dambreak #n.
            ! This means that if t0 values for all dambreaks are different, then the levels_widths_from_table(1:n-1) have become obsolete now.
            ! It works, because in the previous loop iterations the values that were then still correct
            ! have already been set into the %crest_level and %width values.
            if (success) then
               dambreaks(n)%crest_level = levels_widths_from_table((n - 1) * 2 + 1)
               dambreaks(n)%width = levels_widths_from_table((n - 1) * 2 + 2)
            else
               return
            end if
         end if

         if (dambreaks(n)%algorithm /= BREACH_GROWTH_VERHEIJVDKNAAP) then
            dambreaks(n)%breach_width_derivative = &
               (dambreaks(n)%width - dambreaks(n)%breach_width) / delta_time
         end if

         dambreaks(n)%breach_width = dambreaks(n)%width
         dambreaks(n)%breach_depth = dambreaks(n)%crest_level

         if (dambreaks(n)%algorithm == BREACH_GROWTH_TIMESERIES) then
            dambreaks(n)%water_level_jump = calculate_water_level_jump(dambreaks(n)%upstream_level, &
                                                                       dambreaks(n)%downstream_level, dambreaks(n)%breach_depth)
         end if
      end do

   end subroutine calculate_dambreak_widths

   !> This routine sets dambreak%crest_level and dambreak%width, these varuables are needed
   !! in the actual dambreak computation in dflowfm_kernel
   subroutine prepare_dambreak_calculation(dambreak_settings, dambreak, upstream_water_level, &
                                           downstream_water_level, time, time_step)
      use ieee_arithmetic, only: ieee_is_nan
      use m_dambreak, only: t_dambreak_settings, BREACH_GROWTH_VDKNAAP, BREACH_GROWTH_VERHEIJVDKNAAP
      use m_physcoef, only: gravity => ag

      type(t_dambreak_settings), pointer, intent(in) :: dambreak_settings !< dambreak settings for a single dambreak
      type(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
      real(kind=dp), intent(in) :: upstream_water_level !< waterlevel at upstream link from dambreak position
      real(kind=dp), intent(in) :: downstream_water_level !< waterlevel at downstream link from dambreak position
      real(kind=dp), intent(in) :: time !< current time
      real(kind=dp), intent(in) :: time_step !< time step

      real(kind=dp), parameter :: SECONDS_IN_HOUR = 3600.0d0
      real(kind=dp) :: delta_level
      real(kind=dp) :: breach_width
      real(kind=dp) :: actual_maximum_width
      real(kind=dp) :: time_from_breaching
      real(kind=dp) :: time_from_first_phase
      real(kind=dp) :: width_increment
      real(kind=dp) :: water_level_jump_dambreak
      real(kind=dp) :: breach_width_derivative

      time_from_breaching = time - dambreak%t0

      ! breaching not started
      if (time_from_breaching < 0) return

      breach_width_derivative = 0.d0
      water_level_jump_dambreak = 0.d0
      width_increment = 0.0d0

      !vdKnaap(2000) formula: to do: implement table
      if (dambreak%algorithm == BREACH_GROWTH_VDKNAAP) then

         ! The linear part
         if (time_from_breaching < dambreak_settings%time_to_breach_to_maximum_depth) then
            dambreak%crest_level = dambreak%crest_level_ini - &
                                   time_from_breaching / dambreak_settings%time_to_breach_to_maximum_depth * &
                                   (dambreak%crest_level_ini - dambreak_settings%crest_level_min)
            breach_width = dambreak_settings%breach_width_ini
         else
            ! The logarithmic part, time_from_breaching in seconds
            breach_width = dambreak_settings%a_coeff * log(time_from_breaching / dambreak_settings%b_coeff)
         end if

         ! breach width must increase monotonically
         if (breach_width > dambreak%width) then
            dambreak%width = breach_width
         end if

         ! Verheij-vdKnaap(2002) formula
      else if (dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then

         if (time <= dambreak_settings%end_time_first_phase) then
            ! phase 1: lowering
            dambreak%crest_level = dambreak%crest_level_ini - &
                                   time_from_breaching / dambreak_settings%time_to_breach_to_maximum_depth * &
                                   (dambreak%crest_level_ini - dambreak_settings%crest_level_min)
            dambreak%width = dambreak_settings%breach_width_ini
            dambreak%phase = 1
         else
            ! phase 2: widening
            dambreak%crest_level = dambreak_settings%crest_level_min
            water_level_jump_dambreak = calculate_water_level_jump(upstream_water_level, downstream_water_level, &
                                                                   dambreak%crest_level)
            delta_level = (gravity * water_level_jump_dambreak)**1.5d0
            time_from_first_phase = time - dambreak_settings%end_time_first_phase

            if (dambreak%width < dambreak%maximum_width .and. (.not. ieee_is_nan(dambreak%normal_velocity)) &
                .and. dabs(dambreak%normal_velocity) > dambreak_settings%u_crit) then
               breach_width_derivative = (dambreak_settings%f1 * dambreak_settings%f2 / log(10D0)) * &
                                         (delta_level / (dambreak_settings%u_crit * dambreak_settings%u_crit)) * &
                                         (1.0 / (1.0 + (dambreak_settings%f2 * gravity * time_from_first_phase / &
                                                        (dambreak_settings%u_crit * SECONDS_IN_HOUR))))
               width_increment = breach_width_derivative * (time_step / SECONDS_IN_HOUR)
               !ensure monotonically increasing dambreak%width
               if (width_increment > 0) then
                  dambreak%width = dambreak%width + width_increment
               end if
            end if
         end if
         dambreak%breach_width_derivative = breach_width_derivative
         dambreak%water_level_jump = water_level_jump_dambreak
      end if

      ! in vdKnaap(2000) the maximum allowed branch width is limited (see sobek manual and set_dambreak_coefficients subroutine below)
      if (dambreak%algorithm == BREACH_GROWTH_VDKNAAP) then
         actual_maximum_width = min(dambreak_settings%maximum_allowed_width, dambreak%maximum_width)
      else
         actual_maximum_width = dambreak%maximum_width
      end if

      !width cannot exceed the width of the snapped polyline
      if (dambreak%width > actual_maximum_width) then
         dambreak%width = actual_maximum_width
      end if

   end subroutine prepare_dambreak_calculation

   !> calculate the water level jump for dambreaks
   pure function calculate_water_level_jump(upstream_level, downstream_level, crest_level) result(water_level_jump)

      real(kind=dp), intent(in) :: upstream_level !< upstream water level [m]
      real(kind=dp), intent(in) :: downstream_level !< downstream water level [m]
      real(kind=dp), intent(in) :: crest_level !< crest level [m]

      real(kind=dp) :: water_level_jump !< water level jump [m]

      real(kind=dp) :: h_max, h_min

      h_max = max(upstream_level, downstream_level) - crest_level
      h_min = min(upstream_level, downstream_level) - crest_level
      water_level_jump = max(0.0_dp, h_max) - max(0.0_dp, h_min)

    end function calculate_water_level_jump

   !> update the crest/bed levels for dambreak breach
      subroutine adjust_bobs_on_dambreak_breach(dambreak, left_link, right_link)
      use m_flowgeom, only: bob, bob0
      use messagehandling, only: msgbuf, LEVEL_WARN, SetMessage

      type(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
      integer, intent(in) :: left_link !< last flow link on the "left"
      integer, intent(in) :: right_link !< last flow link on the "right"

      integer :: k !< index of the dambreak flow link (range left_link to right_link)
      integer :: flow_link !< index of flow link
      real(kind=dp) :: left_breach_width !< width of the breach on the "left" [m]
      real(kind=dp) :: left_side !< total dambreak structure width on the "left" [m]
      real(kind=dp) :: remainder !< remaining breach width [m]
      real(kind=dp) :: right_breach_width !< width of the breach on the "right" [m]
      real(kind=dp) :: right_side !< total dambreak structure width on the "right" [m]

      ! process the breach at the starting link
      flow_link = abs(dambreak%link_indices(dambreak%breach_start_link-dambreak%shift_in_link_array))
      if (flow_link > 0 .and. dambreak%width > 0.0_dp) then
         ! some breach, set to breached crest level
         bob(1, flow_link) = max(bob0(1, flow_link), dambreak%crest_level)
         bob(2, flow_link) = max(bob0(2, flow_link), dambreak%crest_level)
         dambreak%active_links(dambreak%breach_start_link-dambreak%shift_in_link_array) = 1
      else
         ! no breach
      end if

      ! distribute remaining breach width
      if (dambreak%width <= link_effective_width(dambreak%breach_start_link)) then
         ! breach width still less than width of starting link
         link_actual_width(dambreak%breach_start_link) = max(dambreak%width, 0.0_dp)
         left_breach_width = 0.0_dp
         right_breach_width = 0.0_dp
      else
         ! breach width larger than width of starting link
         link_actual_width(dambreak%breach_start_link) = link_effective_width(dambreak%breach_start_link)
         left_side = sum(link_effective_width(left_link:dambreak%breach_start_link - 1))
         right_side = sum(link_effective_width(dambreak%breach_start_link + 1:right_link))
         remainder = dambreak%width - link_effective_width(dambreak%breach_start_link)
         call calculate_dambreak_widening(remainder, left_side, right_side, left_breach_width, right_breach_width)
      end if

      ! process dam "left" of initial breach segment
      do k = dambreak%breach_start_link - 1, left_link, -1
         flow_link = abs(dambreak%link_indices(k-dambreak%shift_in_link_array))
         if (left_breach_width > 0.0_dp) then
            ! some breach, set to breached crest level
            if (flow_link > 0) then
               bob(1, flow_link) = max(bob0(1, flow_link), dambreak%crest_level)
               bob(2, flow_link) = max(bob0(2, flow_link), dambreak%crest_level)
            end if
            dambreak%active_links(k-dambreak%shift_in_link_array) = 1
         else
            ! no breach
         end if
         if (left_breach_width >= link_effective_width(k)) then
            link_actual_width(k) = link_effective_width(k)
            left_breach_width = left_breach_width - link_effective_width(k)
         else
            link_actual_width(k) = left_breach_width
            left_breach_width = 0.0_dp
         end if
      end do

      ! process dam "right" of initial breach segment
      do k = dambreak%breach_start_link + 1, right_link
         flow_link = abs(dambreak%link_indices(k-dambreak%shift_in_link_array))
         if (right_breach_width > 0.0_dp) then
            ! some breach, set to breached crest level
            if (flow_link > 0) then
               bob(1, flow_link) = max(bob0(1, flow_link), dambreak%crest_level)
               bob(2, flow_link) = max(bob0(2, flow_link), dambreak%crest_level)
            end if
            dambreak%active_links(k-dambreak%shift_in_link_array) = 1
         else
            ! no breach
         end if
         if (right_breach_width >= link_effective_width(k)) then
            link_actual_width(k) = link_effective_width(k)
            right_breach_width = right_breach_width - link_effective_width(k)
         else
            link_actual_width(k) = right_breach_width
            right_breach_width = 0.0_dp
         end if
      end do

      ! check for any unprocessed breach width
      if (left_breach_width > 1.0e-6_dp * dambreak%maximum_width .or. & 
          right_breach_width > 1.0e-6_dp * dambreak%maximum_width) then
         write (msgbuf, '(3a)') 'The breach  of dam ''', trim(dambreak%name), &
             ''' exceeds the actual dam width on at least one side of the breach point.'
         call SetMessage(LEVEL_WARN, msgbuf)
      end if

   end subroutine adjust_bobs_on_dambreak_breach

   !< store upstream dambreak information
   subroutine add_dambreaklocation_upstream(n_signal, node)

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak

      call add_dambreak_location(n_signal, node, UPSTREAM)

   end subroutine add_dambreaklocation_upstream

   !> store downstream dambreak information
   subroutine add_dambreaklocation_downstream(n_signal, node)

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak

      call add_dambreak_location(n_signal, node, DOWNSTREAM)

   end subroutine add_dambreaklocation_downstream

   !> store upstream/downstream dambreak information
   subroutine add_dambreak_location(n_signal, node, up_down)
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak
      integer, intent(in) :: up_down !< 1 - upstream, 2 - downstream

      number_of_mappings = number_of_mappings + 1
      if (number_of_mappings > size(mappings)) then
         write (msgbuf, *) 'number of mappings ', number_of_mappings, &
            ' is larger than allocated array size ', size(mappings)
         call SetMessage(LEVEL_ERROR, msgbuf)
      end if
      mappings(number_of_mappings)%location = node
      if (up_down == UPSTREAM) then
         mappings(number_of_mappings)%water_level => dambreaks(n_signal)%upstream_level
      else
         mappings(number_of_mappings)%water_level => dambreaks(n_signal)%downstream_level
      end if

   end subroutine add_dambreak_location

   !> add upstream signal for averaging
   subroutine add_averaging_upstream_signal(n_signal)

      integer, intent(in) :: n_signal !< number of current dambreak signal

      call add_averaging_signal(n_signal, UPSTREAM)

   end subroutine add_averaging_upstream_signal

   !> add downstream signal for averaging
   subroutine add_averaging_downstream_signal(n_signal)

      integer, intent(in) :: n_signal !< number of current dambreak signal

      call add_averaging_signal(n_signal, DOWNSTREAM)

   end subroutine add_averaging_downstream_signal

   !> add downstream signal for averaging
   subroutine add_averaging_signal(n_signal, up_down)

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: up_down !< 1 - upstream, 2 - downstream

      n_averaging(up_down) = n_averaging(up_down) + 1
      averaging_mapping(n_averaging(up_down), up_down) = n_signal

   end subroutine add_averaging_signal

   module subroutine adjust_bobs_for_dambreaks()

      integer :: n !< index of the current dambreak signal

      if (n_db_links > 0) then ! needed, because n_db_signals may be > 0, but n_db_links==0, and then arrays are not available.
         do n = 1, n_db_signals
            if (dambreaks(n)%index_structure == 0 .or. dambreaks(n)%number_of_links == 0) then
               cycle
            end if
            ! Update the crest/bed levels
            call adjust_bobs_on_dambreak_breach(dambreaks(n), first_link(n), last_link(n))
         end do
      end if
   end subroutine adjust_bobs_for_dambreaks

   !> get the starting link of the dambreak breach
   pure function get_dambreak_breach_start_link(n) result(n_start_link)

      integer, intent(in) :: n !< index of the current dambreak signal
      integer :: n_start_link !< index of the starting link

      n_start_link = abs(dambreaks(n)%link_indices( &
                         dambreaks(n)%breach_start_link - dambreaks(n)%shift_in_link_array))

   end function get_dambreak_breach_start_link

   !> set the starting link of the dambreak breach
   subroutine set_breach_start_link(n, Lstart)

      integer, intent(in) :: n !< index of the current dambreak signal
      integer, intent(in) :: Lstart !< index of the starting link

      dambreaks(n)%breach_start_link = dambreaks(n)%shift_in_link_array + Lstart

   end subroutine set_breach_start_link

   !> fill dambreak values into valdambreak array
   module subroutine fill_dambreak_values(time_step, values)
      use m_flow, only: hu, au, q1
      use m_flowgeom, only: bob, ln
      use m_flowparameters, only: epshu
      use m_missing, only: dmiss
      use m_link_ghostdata, only: link_ghostdata
      use m_partitioninfo, only: jampi, my_rank, idomain
      use m_structures_indices, only: NUMVALS_DAMBREAK, IVAL_WIDTH, IVAL_DB_CRESTW, IVAL_WIDTHWET, IVAL_DIS, IVAL_AREA, IVAL_DB_DISCUM, &
                                      IVAL_DB_CRESTH, IVAL_S1UP, IVAL_S1DN, IVAL_HEAD, IVAL_VEL, IVAL_DB_JUMP, IVAL_DB_TIMEDIV

      real(kind=dp), intent(in) :: time_step !< time step
      real(kind=dp), dimension(:, :), intent(inout) :: values !< dambreak values, (1:NUMVALS_DAMBREAK,:), the first dimension of this array contains
                                                              !! NUMVALS_COMMON common variables and NUMEXTVALS_DAMBREAK extra variables.

      integer :: index_structure !< index of the structure
      integer :: n !< index of the current dambreak signal
      integer :: link !< index of the dambreak link
      integer :: flow_link !< index of the flow link
      integer :: is_ghost_link !< flow link is ghost link (1) or not (0)
      integer :: link_domain_number !< flow link domain number

      do n = 1, n_db_signals
         ! values(NUMVALS_DAMBREAK,n) is the cumulative over time, we do not reset it to 0
         values(1:NUMVALS_DAMBREAK - 1, n) = 0.0_dp
         index_structure = dambreaks(n)%index_structure
         do link = first_link(n), last_link(n)
            if (dambreaks(n)%active_links(link-dambreaks(n)%shift_in_link_array) /= 1) then
               cycle
            end if

            flow_link = abs(dambreaks(n)%link_indices(link-dambreaks(n)%shift_in_link_array))
            if (jampi > 0) then
               call link_ghostdata(my_rank, idomain(ln(1, flow_link)), idomain(ln(2, flow_link)), &
                                   is_ghost_link, link_domain_number)
               if (is_ghost_link == 1) cycle
            end if
            values(IVAL_WIDTH, n) = values(IVAL_WIDTH, n) + link_actual_width(link)
            values(IVAL_DB_CRESTW, n) = values(IVAL_DB_CRESTW, n) + link_actual_width(link)
            if (hu(flow_link) > epshu) then
               values(IVAL_WIDTHWET, n) = values(IVAL_WIDTHWET, n) + link_actual_width(link)
               if (ln(1, flow_link) /= dambreaks(n)%upstream_link_ids(link-dambreaks(n)%shift_in_link_array)) then
                  values(IVAL_DIS, n) = values(IVAL_DIS, n) - q1(flow_link)
               else
                  values(IVAL_DIS, n) = values(IVAL_DIS, n) + q1(flow_link)
               end if
               values(IVAL_AREA, n) = values(IVAL_AREA, n) + au(flow_link) ! flow area
            end if
         end do
         if (dambreaks(n)%number_of_links == 0) then ! NOTE: values(IVAL_DB_DISCUM,n) in a parallel simulation already gets values after mpi communication
            ! from the previous timestep. In the case that the dambreak does not exist on the current domain, it should
            ! not contribute to the cumulative discharge in the coming mpi communication so we set it to 0.
            values(IVAL_DB_DISCUM, n) = 0.0_dp
         else
            if (dambreaks(n)%width > 0.0_dp) then
               values(IVAL_DB_CRESTH, n) = dambreaks(n)%crest_level
            else
               values(1:NUMVALS_DAMBREAK - 1, n) = dmiss ! No breach started yet, set FillValue
               flow_link = get_dambreak_breach_start_link(n)
               values(IVAL_DB_CRESTH, n) = bob(1, flow_link) ! No breach started yet, use bob as 'crest'.
               values(IVAL_DB_CRESTW, n) = 0.0_dp ! No breach started yet, set crest width to 0
               cycle
            end if
            ! TODO: UNST-5102: code below needs checking: when dambreak #n not active in current partition,
            ! most values below *are* available (based on other partitions). And in the code ahead, a call to reduce_crs
            ! assumes that all values are present and will be sum-reduced in a flowlinkwidth-weighted manner.
            values(IVAL_S1UP, n) = dambreaks(n)%upstream_level
            values(IVAL_S1DN, n) = dambreaks(n)%downstream_level
            values(IVAL_HEAD, n) = values(IVAL_S1UP, n) - values(IVAL_S1DN, n)
            values(IVAL_VEL, n) = dambreaks(n)%normal_velocity
            values(IVAL_DB_JUMP, n) = dambreaks(n)%water_level_jump
            values(IVAL_DB_TIMEDIV, n) = dambreaks(n)%breach_width_derivative
            values(IVAL_DB_DISCUM, n) = values(IVAL_DB_DISCUM, n) + values(IVAL_DIS, n) * time_step ! cumulative discharge
         end if
      end do

   end subroutine fill_dambreak_values

   !< set dambreak widening method and returns the string with the method name, in case no correct method is specified
   module subroutine set_dambreak_widening_method(method_string)
      character(len=*), intent(inout) :: method_string !< method for dambreak widening

      select case (method_string)
      case ('symmetric')
         calculate_dambreak_widening => calculate_dambreak_widening_symmetric
      case ('proportional')
         calculate_dambreak_widening => calculate_dambreak_widening_proportional
      case ('symmetric-asymmetric')
         calculate_dambreak_widening => calculate_dambreak_widening_symmetric_asymmetric
      case default
         ! default settings if no method is specified
         calculate_dambreak_widening => calculate_dambreak_widening_symmetric_asymmetric
         method_string = 'symmetric-asymmetric'
      end select

   end subroutine set_dambreak_widening_method

   !> original implementation of dambreak widening which triggers a breach too wide error be
   subroutine calculate_dambreak_widening_symmetric(remainder, left_side, right_side, left_breach_width, right_breach_width)
      use precision, only: dp
      real(kind=dp), intent(in) :: remainder !< remaining width
      real(kind=dp), intent(in) :: left_side !< left side of the breach
      real(kind=dp), intent(in) :: right_side !< right side of the breach
      real(kind=dp), intent(inout) :: left_breach_width !< left breach width
      real(kind=dp), intent(inout) :: right_breach_width !< right breach width

      associate (left_side => left_side, right_side => right_side)
      end associate

      left_breach_width = 0.5_dp * remainder
      right_breach_width = 0.5_dp * remainder
   end subroutine

   !> proportional implementation of dambreak widening
   subroutine calculate_dambreak_widening_proportional(remainder, left_side, right_side, left_breach_width, right_breach_width)
      use precision, only: dp
      real(kind=dp), intent(in) :: remainder !< remaining width
      real(kind=dp), intent(in) :: left_side !< left side of the breach
      real(kind=dp), intent(in) :: right_side !< right side of the breach
      real(kind=dp), intent(inout) :: left_breach_width !< left breach width
      real(kind=dp), intent(inout) :: right_breach_width !< right breach width

      real(kind=dp) :: left_frac !< fraction of structure width on the "left" [-]

      left_frac = left_side / (left_side + right_side)
      left_breach_width = left_frac * remainder
      right_breach_width = (1.0_dp - left_frac) * remainder
   end subroutine

   !> symmetric/asymmetric implementation of dambreak widening
   subroutine calculate_dambreak_widening_symmetric_asymmetric(remainder, left_side, right_side, left_breach_width, right_breach_width)
      use precision, only: dp
      real(kind=dp), intent(in) :: remainder !< remaining width
      real(kind=dp), intent(in) :: left_side !< left side of the breach
      real(kind=dp), intent(in) :: right_side !< right side of the breach
      real(kind=dp), intent(inout) :: left_breach_width !< left breach width
      real(kind=dp), intent(inout) :: right_breach_width !< right breach width

      real(kind=dp) :: h_remainder !< half of the remaining breach width [m]

      h_remainder = 0.5_dp * remainder
      if (h_remainder < min(left_side, right_side)) then
         left_breach_width = h_remainder
         right_breach_width = h_remainder
      elseif (left_side <= right_side) then
         left_breach_width = left_side
         right_breach_width = remainder - left_side
      else
         right_breach_width = right_side
         left_breach_width = remainder - right_side
      end if
   end subroutine

   !> Gets the c-pointer (not a fortran pointer) of the dambreak breach depth.
   module function get_dambreak_depth_c_loc(item_index) result(res)
      use iso_c_binding, only: c_loc, c_ptr, c_null_ptr
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: item_index !< index of the item
      type(c_ptr) :: res !< pointer to the breach depth

      if (item_index < 1 .or. item_index > n_db_signals) then
         write (msgbuf, *) 'get_dambreak_depth_c_loc: the item index ', item_index, &
            ' is out of range. The range is 1 to ', n_db_signals
         call SetMessage(LEVEL_ERROR, msgbuf)
         res = c_null_ptr
      else
         res = c_loc(dambreaks(item_index)%breach_depth)
      end if

   end function get_dambreak_depth_c_loc

   !> Gets the c-pointer (not a fortran pointer) of the dambreak breach width.
   module function get_dambreak_breach_width_c_loc(item_index) result(res)
      use iso_c_binding, only: c_loc, c_ptr, c_null_ptr
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: item_index !< index of the item
      type(c_ptr) :: res !< pointer to the breach width

      if (item_index < 1 .or. item_index > n_db_signals) then
         write (msgbuf, *) 'get_dambreak_breach_width_c_loc: the item index ', item_index, &
            ' is out of range. The range is 1 to ', n_db_signals
         call SetMessage(LEVEL_ERROR, msgbuf)
         res = c_null_ptr
      else
         res = c_loc(dambreaks(item_index)%breach_width)
      end if

   end function get_dambreak_breach_width_c_loc

   !> Gets the c-pointer (not a fortran pointer) of the dambreak upstream level.
   module function get_dambreak_upstream_level_c_loc(item_index) result(res)
      use iso_c_binding, only: c_loc, c_ptr, c_null_ptr
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: item_index !< index of the item
      type(c_ptr) :: res !< pointer to the upstream level

      if (item_index < 1 .or. item_index > n_db_signals) then
         write (msgbuf, *) 'get_dambreak_upstream_level_c_loc: the item index ', item_index, &
            ' is out of range. The range is 1 to ', n_db_signals
         call SetMessage(LEVEL_ERROR, msgbuf)
         res = c_null_ptr
      else
         res = c_loc(dambreaks(item_index)%upstream_level)
      end if

   end function get_dambreak_upstream_level_c_loc

   !> Gets the c-pointer (not a fortran pointer) of the dambreak downstream level.
   module function get_dambreak_downstream_level_c_loc(item_index) result(res)
      use iso_c_binding, only: c_loc, c_ptr, c_null_ptr
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: item_index !< index of the item
      type(c_ptr) :: res !< pointer to the downstream level

      if (item_index < 1 .or. item_index > n_db_signals) then
         write (msgbuf, *) 'get_dambreak_downstream_level_c_loc: the item index ', item_index, &
            ' is out of range. The range is 1 to ', n_db_signals
         call SetMessage(LEVEL_ERROR, msgbuf)
         res = c_null_ptr
      else
         res = c_loc(dambreaks(item_index)%downstream_level)
      end if

   end function get_dambreak_downstream_level_c_loc

   !> Update dambreak administration.
   module subroutine update_dambreak_administration(dambridx, lftopol)
      use precision_basics, only: dp
      use messagehandling, only: IDLEN, msgbuf, err_flush
      use m_missing, only: dmiss, dxymis
      use dfm_error, only: DFM_NOERR
      use geometry_module, only: dbdistance, normalout, comp_breach_point
      use gridoperations, only: incells
      use timespace_parameters, only: uniform, spaceandtime
      use network_data, only: xk, yk
      use unstruc_channel_flow, only: network
      use m_cell_geometry, only: xz, yz
      use m_meteo, only: ec_addtimespacerelation
      use m_sferic, only: jsferic, jasfer3D
      use m_flowgeom, only: ln, kcu, wu, lncn, snu, csu
      use m_inquire_flowgeom, only: findnode
      use m_dambreak, only: BREACH_GROWTH_VERHEIJVDKNAAP, BREACH_GROWTH_TIMESERIES
      use m_alloc, only: realloc

      integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
      integer, dimension(:), intent(in) :: lftopol !< the link number of the flow link.

      integer :: ierr
      integer :: n, k, link, index_in_structure
      integer :: k1, k2, k3, k4, kpol
      integer :: lStart
      logical :: success
      real(kind=dp) :: xla, yla, xlb, ylb, xn, yn
      real(kind=dp) :: x_breach, y_breach
      real(kind=dp), allocatable, dimension(:, :) :: xl, yl

      if (n_db_signals <= 0) then
         n_db_links = 0
         return
      end if

      n_db_links = last_link(n_db_signals)
      call allocate_and_initialize_dambreak_data()

      do n = 1, n_db_signals
         do k = first_link(n), last_link(n)
            dambreaks(n)%link_indices(k-dambreaks(n)%shift_in_link_array) = network%sts%struct(dambridx(n))%linknumbers(k - first_link(n) + 1)
            link = abs(dambreaks(n)%link_indices(k-dambreaks(n)%shift_in_link_array))
            if (dambreaks(n)%link_indices(k-dambreaks(n)%shift_in_link_array) > 0) then
               dambreaks(n)%upstream_link_ids(k-dambreaks(n)%shift_in_link_array) = ln(1, link)
               dambreaks(n)%downstream_link_ids(k-dambreaks(n)%shift_in_link_array) = ln(2, link)
            else
               dambreaks(n)%upstream_link_ids(k-dambreaks(n)%shift_in_link_array) = ln(2, link)
               dambreaks(n)%downstream_link_ids(k-dambreaks(n)%shift_in_link_array) = ln(1, link)
            end if
         end do
      end do

      ! number of columns in the dambreak heights and widths tim file
      do n = 1, n_db_signals
         index_in_structure = dambridx(n)
         if (index_in_structure == -1) then
            cycle
         end if

         associate (pstru => network%sts%struct(dambridx(n)))
            associate (dambreak_settings => pstru%dambreak)
               dambreaks(n)%name = network%sts%struct(index_in_structure)%id

               ! mapping
               dambreaks(n)%index_structure = index_in_structure
               ! set initial phase, width, crest level, coefficents if algorithm is 1
               dambreaks(n)%phase = 0
               dambreaks(n)%width = 0.0_dp
               dambreaks(n)%maximum_width = 0.0_dp
               dambreaks(n)%crest_level = dambreak_settings%crest_level_ini
               dambreaks(n)%crest_level_ini = dambreak_settings%crest_level_ini
               dambreaks(n)%algorithm = dambreak_settings%algorithm
               dambreaks(n)%t0 = dambreak_settings%t0
               if (dambreaks(n)%algorithm == BREACH_GROWTH_TIMESERIES) then
                  ! Time-interpolated value will be placed in levels_widths_from_table((n-1)*KX+1) when calling ec_gettimespacevalue.
                  if (index(trim(dambreak_settings%levels_and_widths)//'|', '.tim|') > 0) then
                     success = ec_addtimespacerelation(QID, XDUM, YDUM, KDUM, KX, dambreak_settings%levels_and_widths, uniform, &
                                                       spaceandtime, 'O', targetIndex=n) ! Hook up 1 component at a time, even when target element set has KX
                     if (.not. success) then
                        write (msgbuf, '(5a)') 'Cannot process a tim file for ''', QID, ''' for the dambreak ''', trim(dambreaks(n)%name), '''.'
                        call err_flush()
                     end if
                  end if
               end if

               ! inquire if the water level upstream has to be taken from a location or be a result of averaging
               if (dambreak_settings%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP & ! Needed for computation and output
                   .or. dambreak_settings%algorithm == BREACH_GROWTH_TIMESERIES) then ! Needed for output only.
                  xla = dambreak_settings%water_level_upstream_location_x
                  yla = dambreak_settings%water_level_upstream_location_y
                  if (dambreak_settings%water_level_upstream_node_id /= '') then
                     ierr = findnode(dambreak_settings%water_level_upstream_node_id, k)
                     if (ierr /= DFM_NOERR .or. k <= 0) then
                        write (msgbuf, '(a,a,a,a,a)') 'Cannot find the node for water_level_upstream_node_id = ''', &
                           trim(dambreak_settings%water_level_upstream_node_id), &
                           ''' in dambreak ''', trim(dambreaks(n)%name), '''.'
                        call err_flush()
                     else
                        call add_dambreaklocation_upstream(n, k)
                     end if
                  else if (xla /= dmiss .and. yla /= dmiss) then
                     call incells(xla, yla, k)
                     if (k > 0) then
                        call add_dambreaklocation_upstream(n, k)
                     end if
                  else
                     call add_averaging_upstream_signal(n)
                  end if
               end if

               ! inquire if the water level downstream has to be taken from a location or be a result of averaging
               if (dambreak_settings%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP & ! Needed for computation and output
                   .or. dambreak_settings%algorithm == BREACH_GROWTH_TIMESERIES) then ! Needed for output only.
                  xla = dambreak_settings%water_level_downstream_location_x
                  yla = dambreak_settings%water_level_downstream_location_y
                  if (dambreak_settings%water_level_downstream_node_id /= '') then
                     ierr = findnode(dambreak_settings%water_level_downstream_node_id, k)
                     if (ierr /= DFM_NOERR .or. k <= 0) then
                        write (msgbuf, '(5a)') 'Cannot find the node for water_level_downstream_node_id = ''', &
                           trim(dambreak_settings%water_level_downstream_node_id), &
                           ''' in dambreak ''', trim(dambreaks(n)%name), '''.'
                        call err_flush()
                     else
                        call add_dambreaklocation_downstream(n, k)
                     end if
                  else if (xla /= dmiss .and. yla /= dmiss) then
                     call incells(xla, yla, k)
                     if (k > 0) then
                        call add_dambreaklocation_downstream(n, k)
                     end if
                  else
                     call add_averaging_downstream_signal(n)
                  end if
               end if

               ! Project the start of the breach on the polyline, find xn and yn
               if (.not. associated(pstru%xCoordinates)) cycle
               if (.not. associated(pstru%yCoordinates)) cycle

               ! Create the array with the coordinates of the flow links
               call realloc(xl, [dambreaks(n)%number_of_links, 2])
               call realloc(yl, [dambreaks(n)%number_of_links, 2])
               do k = 1, dambreaks(n)%number_of_links
                  ! compute the mid point
                  link = abs(dambreaks(n)%link_indices(k))
                  k1 = ln(1, link)
                  k2 = ln(2, link)
                  xl(k, 1) = xz(k1)
                  xl(k, 2) = xz(k2)
                  yl(k, 1) = yz(k1)
                  yl(k, 2) = yz(k2)
               end do

               ! comp_breach_point takes plain arrays to compute the breach point (also used in unstruct_bmi)
               call comp_breach_point(dambreak_settings%start_location_x, dambreak_settings%start_location_y, &
                                      pstru%xCoordinates, pstru%yCoordinates, pstru%numCoordinates, xl, &
                                      yl, Lstart, x_breach, y_breach, jsferic, jasfer3D, dmiss)

               call set_breach_start_link(n, Lstart)

               ! compute the normal projections of the start and endpoints of the flow links
               do k = 1, dambreaks(n)%number_of_links
                  link = abs(dambreaks(n)%link_indices(k))
                  if (kcu(link) == 3) then ! 1d2d flow link
                     link_effective_width(k + dambreaks(n)%shift_in_link_array) = wu(link)
                  else
                     k3 = lncn(1, link)
                     k4 = lncn(2, link)
                     kpol = lftopol(k + dambreaks(n)%shift_in_link_array)
                     xla = pstru%xCoordinates(kpol)
                     xlb = pstru%xCoordinates(kpol + 1)
                     yla = pstru%yCoordinates(kpol)
                     ylb = pstru%yCoordinates(kpol + 1)

                     call normalout(xla, yla, xlb, ylb, xn, yn, jsferic, jasfer3D, dmiss, dxymis)
                     link_effective_width(k) = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss) * &
                                               abs(xn * csu(link) + yn * snu(link))
                  end if

                  ! Sum the length of the intersected flow links (required to bound maximum breach width)
                  dambreaks(n)%maximum_width = dambreaks(n)%maximum_width + link_effective_width(k + dambreaks(n)%shift_in_link_array)
               end do

               ! Now we can deallocate the polygon
            end associate
         end associate
      end do
   end subroutine update_dambreak_administration

   module subroutine update_dambreak_administration_old(dambridx, lftopol)
      use dfm_error, only: DFM_NOERR
      use m_hash_search, only: hashsearch
      use m_flowgeom, only: wu, ln, xz, yz, kcu, lncn, snu, csu
      use m_netw, only: xk, yk
      use unstruc_channel_flow, only: addstructure, getstructype_from_string
      use m_structures ! Jan's channel_flow for Sobek's generalstructure (TODO)
      use timespace, only: UNIFORM, SPACEANDTIME
      use m_meteo, only: kedb, ec_addtimespacerelation, dambreakPolygons
      use m_readstructures, only: readdambreak
      use m_sferic, only: jsferic, jasfer3d
      use geometry_module, only: dbdistance, normalout, comp_breach_point
      use gridoperations, only: incells
      use m_inquire_flowgeom, only: findnode
      use m_dambreak, only: BREACH_GROWTH_VERHEIJVDKNAAP, BREACH_GROWTH_TIMESERIES
      use m_missing, only: dmiss, dxymis

      integer :: L, Lf, kb, ierr, k, kbi, n, k1, k2
      integer :: istrtype
      integer :: istrtmp
      integer :: k3, k4, kpol, index_structure, indexInPliset, Lstart
      integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
      integer, dimension(:), intent(in) :: lftopol !< the link number of the flow link.
      real(kind=dp) :: x_breach, y_breach
      real(kind=dp) :: xn, yn
      real(kind=dp) :: xla, xlb, yla, ylb
      real(kind=dp), allocatable :: xl(:, :), yl(:, :)
      type(tree_data), pointer :: str_ptr
      character(len=IdLen) :: strid ! TODO: where to put IdLen (now in MessageHandling)
      character(len=IdLen) :: strtype ! TODO: where to put IdLen (now in MessageHandling)
      logical :: success !< success flag

      if (n_db_signals > 0) then

         call allocate_and_initialize_dambreak_data()

         do n = 1, n_db_signals
            do k = first_link(n), last_link(n)
               L = kedb(k)
               Lf = abs(L)
               if (L > 0) then
                  kb = ln(1, Lf)
                  kbi = ln(2, Lf)
               else
                  kb = ln(2, Lf)
                  kbi = ln(1, Lf)
               end if
               dambreaks(n)%upstream_link_ids(k-dambreaks(n)%shift_in_link_array) = kb
               dambreaks(n)%downstream_link_ids(k-dambreaks(n)%shift_in_link_array) = kbi
               dambreaks(n)%link_indices(k-dambreaks(n)%shift_in_link_array) = L
            end do
         end do

         do n = 1, n_db_signals

            index_structure = dambridx(n)
            if (index_structure == -1) cycle

            str_ptr => strs_ptr%child_nodes(index_structure)%node_ptr

            ! read the id first
            strid = ' '
            call prop_get(str_ptr, '', 'id', strid, success)
            dambreaks(n)%name = strid

            istrtmp = hashsearch(network%sts%hashlist_structure, strid) ! Assumes unique names across all structure types.
            if (istrtmp /= -1) then
               indexInPliset = istrtmp ! dambreakPolygons were already read in network%sts loop.
               success = .true.
            else
               ! Postponed read, because this is with old-style .pli ifile
               indexInPliset = index_structure ! dambreakPolygons were already read in old style .pli count+selectelset loop above.

               ! read the type
               strtype = ' '
               call prop_get(str_ptr, '', 'type', strtype, success)
               istrtype = getStructype_from_string(strtype)
               ! flow1d_io library: add and read SOBEK dambreak
               if (dambreaks(n)%number_of_links > 0) then
                  ! structure is active in current grid on one or more flow links: just use the first link of the the structure (the network%sts%struct(istrtmp)%link_number is not used in computations)
                  k = first_link(n)
                  k1 = dambreaks(n)%upstream_link_ids(k-dambreaks(n)%shift_in_link_array)
                  k2 = dambreaks(n)%downstream_link_ids(k-dambreaks(n)%shift_in_link_array)
                  Lf = abs(dambreaks(n)%link_indices(1))
               else
                  ! Structure is not active in current grid: use dummy calc points and flow links, not used in computations.
                  k1 = 0
                  k2 = 0
                  Lf = 0
               end if
               istrtmp = addStructure(network%sts, k1, k2, Lf, -1, "", strid, istrtype)
               call readDambreak(network%sts%struct(istrtmp)%dambreak, str_ptr, strid, network%forcinglist, success)
            end if

! TODO UNST-3308 ^^^
            if (success) then
               ! new dambreak format
               write (msgbuf, '(a,a,a)') 'Dambreak ''', trim(strid), ''' set to new format.'
               call msg_flush()
               ! mapping
               dambreaks(n)%index_structure = istrtmp
               ! set initial phase, width, crest level, coefficents if algorithm is 1
               dambreaks(n)%phase = 0
               dambreaks(n)%width = 0.0_dp
               dambreaks(n)%maximum_width = 0.0_dp
               dambreaks(n)%crest_level = network%sts%struct(istrtmp)%dambreak%crest_level_ini
               dambreaks(n)%crest_level_ini = network%sts%struct(istrtmp)%dambreak%crest_level_ini
               dambreaks(n)%algorithm = network%sts%struct(istrtmp)%dambreak%algorithm
               dambreaks(n)%t0 = network%sts%struct(istrtmp)%dambreak%t0

               if (network%sts%struct(istrtmp)%dambreak%algorithm == BREACH_GROWTH_TIMESERIES) then
                  ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
                  network%sts%struct(istrtmp)%dambreak%levels_and_widths = trim(network%sts%struct(istrtmp)%dambreak%levels_and_widths)
                  if (index(trim(network%sts%struct(istrtmp)%dambreak%levels_and_widths)//'|', '.tim|') > 0) then
                     success = ec_addtimespacerelation(QID, XDUM, YDUM, KDUM, KX, network%sts%struct(istrtmp)%dambreak%levels_and_widths, uniform, spaceandtime, 'O', targetIndex=n) ! Hook up 1 component at a time, even when target element set has KX=2
                  else
                     write (msgbuf, '(5a)') 'Cannot process a tim file for ''', QID, ''' for the dambreak ''', trim(dambreaks(n)%name), '''.'
                     call err_flush()
                  end if
               end if

               ! inquire if the water level upstream has to be taken from a location or be a result of averaging
               if (network%sts%struct(istrtmp)%dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP & ! Needed for computation and output
                   .or. network%sts%struct(istrtmp)%dambreak%algorithm == BREACH_GROWTH_TIMESERIES) then ! Needed for output only.
                  xla = network%sts%struct(istrtmp)%dambreak%water_level_upstream_location_x
                  yla = network%sts%struct(istrtmp)%dambreak%water_level_upstream_location_y
                  if (network%sts%struct(istrtmp)%dambreak%water_level_upstream_node_id /= '') then
                     ierr = findnode(network%sts%struct(istrtmp)%dambreak%water_level_upstream_node_id, k)
                     if (ierr /= DFM_NOERR .or. k <= 0) then
                        write (msgbuf, '(a,a,a,a,a)') 'Cannot find the node for water_level_upstream_node_id = ''', trim(network%sts%struct(istrtmp)%dambreak%water_level_upstream_node_id), &
                           ''' in dambreak ''', trim(strid), '''.'
                        call err_flush()
                     else
                        call add_dambreaklocation_upstream(n, k)
                     end if
                  else if (xla /= dmiss .and. yla /= dmiss) then
                     call incells(xla, yla, k)
                     if (k > 0) then
                        call add_dambreaklocation_upstream(n, k)
                     end if
                  else
                     call add_averaging_upstream_signal(n)
                  end if
               end if

               ! inquire if the water level downstream has to be taken from a location or be a result of averaging
               if (network%sts%struct(istrtmp)%dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP & ! Needed for computation and output
                   .or. network%sts%struct(istrtmp)%dambreak%algorithm == BREACH_GROWTH_TIMESERIES) then ! Needed for output only.
                  xla = network%sts%struct(istrtmp)%dambreak%water_level_downstream_location_x
                  yla = network%sts%struct(istrtmp)%dambreak%water_level_downstream_location_y
                  if (network%sts%struct(istrtmp)%dambreak%water_level_downstream_node_id /= '') then
                     ierr = findnode(network%sts%struct(istrtmp)%dambreak%water_level_downstream_node_id, k)
                     if (ierr /= DFM_NOERR .or. k <= 0) then
                        write (msgbuf, '(a,a,a,a,a)') 'Cannot find the node for water_level_downstream_node_id = ''', trim(network%sts%struct(istrtmp)%dambreak%water_level_downstream_node_id), &
                           ''' in dambreak ''', trim(strid), '''.'
                        call err_flush()
                     else
                        call add_dambreaklocation_downstream(n, k)
                     end if
                  else if (xla /= dmiss .and. yla /= dmiss) then
                     call incells(xla, yla, k)
                     if (k > 0) then
                        call add_dambreaklocation_downstream(n, k)
                     end if
                  else
                     call add_averaging_downstream_signal(n)
                  end if
               end if

            else
               ! old dambreak format
               write (msgbuf, '(a,a,a)') 'Dambreak ''', trim(strid), ''' could not be read. Perhaps missing fields in structure file?'
               call err_flush()
               cycle
            end if

            ! Project the start of the breach on the polyline, find xn and yn
            if (.not. allocated(dambreakPolygons(indexInPliset)%xp)) cycle
            if (.not. allocated(dambreakPolygons(indexInPliset)%yp)) cycle

            ! Create the array with the coordinates of the flow links
            if (allocated(xl)) then
               deallocate (xl)
            end if
            if (allocated(yl)) then
               deallocate (yl)
            end if
            allocate (xl(dambreaks(n)%number_of_links, 2))
            allocate (yl(dambreaks(n)%number_of_links, 2))
            do k = 1, dambreaks(n)%number_of_links
               ! compute the mid point
               Lf = abs(dambreaks(n)%link_indices(k))
               k1 = ln(1, Lf)
               k2 = ln(2, Lf)
               xl(k, 1) = xz(k1)
               xl(k, 2) = xz(k2)
               yl(k, 1) = yz(k1)
               yl(k, 2) = yz(k2)
            end do

            ! comp_breach_point takes plain arrays to compute the breach point (also used in unstruct_bmi)
            call comp_breach_point(network%sts%struct(istrtmp)%dambreak%start_location_x, &
                                   network%sts%struct(istrtmp)%dambreak%start_location_y, &
                                   dambreakPolygons(indexInPliset)%xp, &
                                   dambreakPolygons(indexInPliset)%yp, &
                                   dambreakPolygons(indexInPliset)%np, &
                                   xl, yl, Lstart, x_breach, y_breach, jsferic, jasfer3D, dmiss)

            call set_breach_start_link(n, Lstart)

            ! compute the normal projections of the start and endpoints of the flow links
            do k = 1, dambreaks(n)%number_of_links
               Lf = abs(dambreaks(n)%link_indices(k))
               if (kcu(Lf) == 3) then ! 1d2d flow link
                  link_effective_width(k) = wu(Lf)
               else
                  k3 = lncn(1, Lf)
                  k4 = lncn(2, Lf)
                  kpol = lftopol(k + dambreaks(n)%shift_in_link_array)
                  xla = dambreakPolygons(indexInPliset)%xp(kpol)
                  xlb = dambreakPolygons(indexInPliset)%xp(kpol + 1)
                  yla = dambreakPolygons(indexInPliset)%yp(kpol)
                  ylb = dambreakPolygons(indexInPliset)%yp(kpol + 1)

                  call normalout(xla, yla, xlb, ylb, xn, yn, jsferic, jasfer3D, dmiss, dxymis)
                  link_effective_width(k) = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)
                  link_effective_width(k) = link_effective_width(k) * abs(xn * csu(Lf) + yn * snu(Lf))
               end if

               ! Sum the length of the intersected flow links (required to bound maximum breach width)
               dambreaks(n)%maximum_width = dambreaks(n)%maximum_width + link_effective_width(k + dambreaks(n)%shift_in_link_array)
            end do

            ! Now we can deallocate the polygon
            deallocate (dambreakPolygons(indexInPliset)%yp)
            deallocate (dambreakPolygons(indexInPliset)%xp)
         end do
      end if

   end subroutine update_dambreak_administration_old

   !> update array of logicals indicating if the link contains dambreaks
   pure module subroutine indicate_links_that_contain_dambreaks(does_link_contain_structures)

      logical, intent(inout) :: does_link_contain_structures(:) !< array of logicals indicating if the link contains structures

      integer :: n !< loop index
      integer :: k !< loop index

      if (have_dambreaks_links()) then
         do n = 1, n_db_signals
            if (dambreaks(n)%index_structure /= 0) then
               do k = 1, dambreaks(n)%number_of_links
                  does_link_contain_structures(abs(dambreaks(n)%link_indices(k))) = .true.
               end do
            end if
         end do
      end if

   end subroutine indicate_links_that_contain_dambreaks

   !> Check if any dambreaks are active in the current grid
   pure module function should_write_dambreaks() result(res)

      logical :: res
      integer :: objects !< total number of objects to write
      integer :: n !< loop index

      ! Count the number of active links for each signal
      objects = n_db_signals
      do n = 1, n_db_signals
         if (dambreaks(n)%number_of_links == 0) then
            objects = objects - 1
         end if
      end do

      res = objects > 0
   end function should_write_dambreaks

   !> set correct flow areas for dambreaks, using the actual flow width
   module subroutine multiply_by_dambreak_link_actual_width(hu, au)

      real(kind=dp), dimension(:), intent(in) :: hu !< source
      real(kind=dp), dimension(:), intent(inout) :: au !< results

      integer :: n !< loop index
      integer :: k !< loop index
      integer :: link !< link index

      do n = 1, n_db_signals
         do k = 1, dambreaks(n)%number_of_links
            link = abs(dambreaks(n)%link_indices(k))
            au(link) = hu(link) * link_actual_width(k + dambreaks(n)%shift_in_link_array)
         end do
      end do

   end subroutine multiply_by_dambreak_link_actual_width

   !> Get the index of the active dambreak for a given dambreak name
   pure module function get_active_dambreak_index(dambreak_name) result(index)
      character(len=*), intent(in) :: dambreak_name !< Id/name of the requested dambreak
      integer :: index !< Returned index of the found dambreak; -1 when not found.

      integer :: i !< loop index

      index = -1
      do i = 1, n_db_signals
         if (trim(dambreaks(i)%name) == trim(dambreak_name)) then
            if (dambreaks(i)%number_of_links > 0) then
               ! Only return this dambreak index if dambreak is active in flowgeom (i.e., at least 1 flow link associated)
               index = i
               exit
            end if
         end if
      end do
   end function get_active_dambreak_index

   !> Get the dambreak links for a given dambreak index
   module function retrieve_set_of_flowlinks_dambreak(index) result(res)
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage
      integer, intent(in) :: index !< index of the dambreak
      integer, dimension(:), allocatable :: res !< the dambreak links

      if (index < 1 .or. index > n_db_signals) then
         write (msgbuf, *) 'get_dambreak_links: the index ', index, &
            ' is out of range. The range is 1 to ', n_db_signals
         call SetMessage(LEVEL_ERROR, msgbuf)
         allocate (res(0))
      else
         res = dambreaks(index)%link_indices
      end if

   end function retrieve_set_of_flowlinks_dambreak

   !> Update the counters for the dambreaks
   module subroutine update_counters_for_dambreaks(id, numgen, dambridx, i, kedb, kegen)
      use m_update_counters_for_structures, only: update_counters_for_dambreak_or_pump
      character(len=*), intent(in) :: id !< the id of the structure.
      integer, intent(in) :: numgen !< the number of flow links.
      integer, dimension(:), allocatable, intent(inout) :: dambridx !< the index of the structure.
      integer, intent(in) :: i !< the index of the structure.
      integer, dimension(:), allocatable, intent(inout) :: kedb !< edge oriented dambreak??? Do we need this array?
      integer, dimension(:), allocatable, intent(in) :: kegen !< placeholder for the link snapping of all structure types.

      call update_counters_for_dambreak_or_pump(id, numgen, n_db_signals, first_link, last_link, dambridx, i)
      kedb(first_link(n_db_signals):last_link(n_db_signals)) = kegen(1:numgen)

   end subroutine update_counters_for_dambreaks

   !> Add a new dambreak signal to the list of signals
   module subroutine add_dambreak_signal(index_in_structure, dambridx, n_dambreak_links, n_current_dambreak_links)
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage
      use m_alloc, only: realloc

      integer, intent(in) :: index_in_structure !< the index of the structure in the structure list.
      integer, dimension(:), intent(inout) :: dambridx !< the index of the dambreak in the structure list.
      integer, intent(inout) :: n_dambreak_links !< the total number of flow links for dambreaks.
      integer, intent(in) :: n_current_dambreak_links !< the number of flow links for the current dambreak signal.

      if (n_dambreak_links /= n_db_links) then
         write (msgbuf, '(a,i8,a,i8)') 'n_dambreak_links = ', n_dambreak_links, ' /= n_db_links = ', n_db_links
         call SetMessage(LEVEL_ERROR, msgbuf)
      end if
      n_db_signals = n_db_signals + 1
      dambridx(n_db_signals) = index_in_structure
      call realloc(first_link, n_db_signals)
      first_link(n_db_signals) = n_dambreak_links + 1
      call realloc(last_link, n_db_signals)
      last_link(n_db_signals) = n_dambreak_links + n_current_dambreak_links
      n_dambreak_links = n_dambreak_links + n_current_dambreak_links
      n_db_links = n_dambreak_links

   end subroutine add_dambreak_signal

   !> provides dambreak names
   pure module function get_dambreak_names() result(names)
      character(len=128), dimension(:), allocatable :: names !< the dambreak names

      if (allocated(dambreaks)) then
         names = dambreaks%name
      else
         allocate (names(0))
      end if

   end function get_dambreak_names

end submodule m_dambreak_breach_submodule
