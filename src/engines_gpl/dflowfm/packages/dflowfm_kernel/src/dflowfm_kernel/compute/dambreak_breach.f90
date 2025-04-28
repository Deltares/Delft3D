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

submodule(m_dambreak_breach) sub_dambreak_breach
   use precision, only: dp

   implicit none

   integer, parameter :: UPSTREAM = 1
   integer, parameter :: DOWNSTREAM = 2
   integer, parameter :: NUMBER_COLUMNS = 2
   integer, dimension(2) :: n_locations !< nr of dambreak locations (upstream 1st value, downstream 2nd value)
   integer, dimension(:, :), allocatable :: locations !< store cell ids for water level (upstream in 1st row, downstream in 2nd row)
   integer, dimension(:, :), allocatable :: location_mapping !< mapping of dambreak locations (upstream in 1st row, downstream in 2nd row)
   integer, dimension(2) :: n_averaging !< nr of dambreak signals with averaging (upstream 1st value, downstream 2nd value)
   integer, dimension(:, :), allocatable :: averaging_mapping !< mapping of dambreak averaging (upstream in 1st row, downstream in 2nd row)
   real(kind=dp), dimension(:, :), allocatable :: db_weight_averaged_values !< (1,:) weight averaged values of waterlevel per dambreaklink
                                                                           !! (2,:) weight per dambreaklink
   real(kind=dp), allocatable, target :: levels_widths_from_table(:) !< dambreak heights and widths
   integer, dimension(:), allocatable :: db_active_links !< db_active_links, open dambreak links
   integer, dimension(:), allocatable :: breach_start_link !< the starting link, the closest to the breach point

   procedure(calculate_dambreak_widening_any), pointer :: calculate_dambreak_widening

   abstract interface
      subroutine calculate_dambreak_widening_any(remainder, left_side, right_side, left_breach_width, right_breach_width)
         use precision, only: dp
         real(kind=dp), intent(in) :: remainder !< remaining width
         real(kind=dp), intent(in) :: left_side !< left side of the breach
         real(kind=dp), intent(in) :: right_side !< right side of the breach
         real(kind=dp), intent(inout) :: left_breach_width !< left breach width
         real(kind=dp), intent(inout) :: right_breach_width !< right breach width
      end subroutine
   end interface

contains

   !> allocate arrays and initialize variables
   module subroutine allocate_and_initialize_dambreak_data(n_db_signals)
      use m_alloc, only: realloc

      integer, intent(in) :: n_db_signals !< number of dambreak signals

      call realloc(dambreaks, n_db_signals, fill=0)
      call realloc(breach_start_link, n_db_signals, fill=-1)
      call realloc(db_breach_depths, n_db_signals, fill=0.0_dp)
      call realloc(db_breach_widths, n_db_signals, fill=0.0_dp)
      call realloc(db_ids, n_db_signals, fill="")
      call realloc(db_active_links, n_db_links, fill=0)
      call realloc(levels_widths_from_table, n_db_signals * 2, fill=0.0_dp)
      call realloc(db_upstream_levels, n_db_signals)
      call realloc(db_downstream_levels, n_db_signals)
      call realloc(db_weight_averaged_values, [NUMBER_COLUMNS, n_db_signals])
      call realloc(location_mapping, [n_db_signals, NUMBER_COLUMNS], fill=0)
      call realloc(locations, [n_db_signals, NUMBER_COLUMNS], fill=0)
      call realloc(averaging_mapping, [n_db_signals, NUMBER_COLUMNS], fill=0)
      call realloc(db_link_ids, n_db_links, fill=0)
      call realloc(db_upstream_link_ids, n_db_links, fill=0)
      call realloc(db_downstream_link_ids, n_db_links, fill=0)
      n_locations(:) = 0
      n_averaging(:) = 0

   end subroutine allocate_and_initialize_dambreak_data

   !> updates dambreak breach by updating water levels upstream and downstream and calculating dambreak widths
   module function update_dambreak_breach(start_time, delta_time) result(error)
      use m_flow, only: hu, au, u1
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network
      use m_partitioninfo, only: get_average_quantity_from_links

      real(kind=dp), intent(in) :: start_time !< start time
      real(kind=dp), intent(in) :: delta_time !< delta time
      integer :: error !< error code

      integer :: n !< index of the current dambreak signal
      integer :: i_structure !< index of the structure

      error = 0

      if (n_db_signals <= 0) then
         return
      end if
      ! Variable n_db_signals is >0 for all partitions if there is a dambreak, even if it is outside
      ! of a partition. In a parallel simulation, we need to run this subroutine even in a special situation that there is
      ! no dambreak on the current subdomain (i.e. n_db_links == 0), because the following function get_average_quantity_from_links
      ! involves mpi communication among all subdomains. However, in this special situation,
      ! all the necessary variables are set to 0 and do not participate the dambreak related computation in this subroutine.

      call reset_dambreak_variables(n_db_signals)

      call update_dambreak_water_levels(start_time, UPSTREAM, db_upstream_link_ids, db_upstream_levels, error)
      if (error /= 0) then
         return
      end if

      call update_dambreak_water_levels(start_time, DOWNSTREAM, db_downstream_link_ids, db_downstream_levels, error)
      if (error /= 0) then
         return
      end if

      ! u1 velocity on the flowlinks (averaged by the wetted area). The mask is the water level itself
      error = get_average_quantity_from_links(db_first_link, db_last_link, au, db_link_ids, u1, &
                                              db_link_ids, db_weight_averaged_values, 1, hu, dmiss, &
                                              db_active_links, 0)
      if (error /= 0) then
         return
      end if

      if (n_db_links > 0) then
         do n = 1, n_db_signals
            i_structure = dambreaks(n)
            if (i_structure /= 0 .and. db_weight_averaged_values(2, n) > 0.0_dp) then
               network%sts%struct(i_structure)%dambreak%normal_velocity = &
                  db_weight_averaged_values(1, n) / db_weight_averaged_values(2, n)
            end if
         end do

         call calculate_dambreak_widths(start_time, delta_time)

      end if

   end function update_dambreak_breach

   !> reset dambreak variables like water levels, averaged values etc.
   subroutine reset_dambreak_variables(n_db_signals)
      use unstruc_channel_flow, only: network

      integer, intent(in) :: n_db_signals !< number of dambreak signals

      integer :: n !< index of the current dambreak signal
      integer :: i_structure !< index of the structure

      db_weight_averaged_values(:, :) = 0.0_dp
      db_upstream_levels(:) = 0.0_dp
      db_downstream_levels(:) = 0.0_dp
      do n = 1, n_db_signals
         i_structure = dambreaks(n)
         if (i_structure <= 0) then
            continue
         end if
         network%sts%struct(i_structure)%dambreak%normal_velocity = 0.0_dp
         network%sts%struct(i_structure)%dambreak%breach_width_derivative = 0.0_dp
         network%sts%struct(i_structure)%dambreak%water_level_jump = 0.0_dp
      end do
   end subroutine reset_dambreak_variables

   !> update water levels for dambreaks
   subroutine update_dambreak_water_levels(start_time, up_down, link_ids, water_levels, error)
      use m_flow, only: s1, hu
      use m_partitioninfo, only: get_average_quantity_from_links
      use m_flowgeom, only: wu
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network

      real(kind=dp), intent(in) :: start_time !< start time
      integer, intent(in) :: up_down !< 1 - upstream, 2 - downstream
      integer, dimension(:), intent(in) :: link_ids !< upstream or downstream link ids
      real(kind=dp), dimension(:), intent(inout) :: water_levels !< water levels
      integer, intent(out) :: error !< error code

      integer :: n !< index of the current dambreak signal

      error = 0

      if (n_locations(up_down) > 0) then
         water_levels(location_mapping(1:n_locations(up_down), up_down)) = s1(locations(1:n_locations(up_down), up_down))
      end if

      !call this code only if something has to be averaged
      if (n_averaging(up_down) > 0) then
         error = get_average_quantity_from_links(db_first_link(averaging_mapping(1:n_averaging(up_down), up_down)), &
                                                 db_last_link(averaging_mapping(1:n_averaging(up_down), up_down)), wu, &
                                                 db_link_ids, s1, link_ids, db_weight_averaged_values, &
                                                 0, hu, dmiss, db_active_links, 0)
         if (error /= 0) then
            return
         end if

         if (n_db_links > 0) then
            do n = 1, n_averaging(up_down)
               if (db_weight_averaged_values(2, n) > 0.0_dp) then
                  water_levels(averaging_mapping(n, up_down)) = &
                     db_weight_averaged_values(1, n) / db_weight_averaged_values(2, n)
               else if (abs(start_time - &
                            network%sts%struct(dambreaks(averaging_mapping(n, up_down)))%dambreak%T0) < 1e-10_dp) then
                  water_levels(averaging_mapping(n, up_down)) = &
                     s1(link_ids(breach_start_link(averaging_mapping(n, up_down))))
               else
                  continue
               end if
            end do
         end if
      end if

   end subroutine update_dambreak_water_levels

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
         i_structure = dambreaks(n)
         if (i_structure == 0) then
            continue
         end if
         associate (dambreak => network%sts%struct(i_structure)%dambreak)
            if (dambreak%algorithm == BREACH_GROWTH_VDKNAAP .or. &
                dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then
               call prepare_dambreak_calculation(network%sts%struct(i_structure)%dambreak, db_upstream_levels(n), &
                                                 db_downstream_levels(n), start_time, delta_time)
            end if
            if (dambreak%algorithm == BREACH_GROWTH_TIMESERIES .and. &
                start_time > dambreak%t0) then
               !Time in the tim file is relative to the start time
               success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_db_levels_widths_table, &
                                                        irefdate, tzone, tunit, start_time - dambreak%t0, &
                                                        levels_widths_from_table)
               ! NOTE: AvD: the code above works correctly, but is dangerous:
               ! the addtimespace for dambreak has added each dambreak separately with a targetoffset.
               ! The gettimespace above, however, gets the values for *all* dambreaks, but with the relative time
               ! of the *current* dambreak #n.
               ! This means that if t0 values for all dambreaks are different, then the levels_widths_from_table(1:n-1) have become obsolete now.
               ! It works, because in the previous loop iterations the values that were then still correct
               ! have already been set into the %crest_level and %width values.
               if (success) then
                  dambreak%crest_level = levels_widths_from_table((n - 1) * 2 + 1)
                  dambreak%width = levels_widths_from_table((n - 1) * 2 + 2)
               else
                  return
               end if
            end if

            if (dambreak%algorithm /= BREACH_GROWTH_VERHEIJVDKNAAP) then
               dambreak%breach_width_derivative = &
                  (dambreak%width - db_breach_widths(n)) / delta_time
            end if

            db_breach_widths(n) = dambreak%width
            db_breach_depths(n) = dambreak%crest_level

            if (dambreak%algorithm == BREACH_GROWTH_TIMESERIES) then
               dambreak%water_level_jump = calculate_water_level_jump(db_upstream_levels(n), &
                                                                      db_downstream_levels(n), db_breach_depths(n))
            end if
         end associate
      end do

   end subroutine calculate_dambreak_widths

   !> This routine sets dambreak%crest_level and dambreak%width, these varuables are needed
   !! in the actual dambreak computation in dflowfm_kernel
   subroutine prepare_dambreak_calculation(dambreak, upstream_water_level, downstream_water_level, time, time_step)
      use ieee_arithmetic, only: ieee_is_nan
      use m_dambreak, only: t_dambreak, BREACH_GROWTH_VDKNAAP, BREACH_GROWTH_VERHEIJVDKNAAP
      use m_physcoef, only: gravity => ag

      type(t_dambreak), pointer, intent(inout) :: dambreak !< dambreak settings for a single dambreak
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
         if (time_from_breaching < dambreak%time_to_breach_to_maximum_depth) then
            dambreak%crest_level = dambreak%crest_level_ini - &
                                   time_from_breaching / dambreak%time_to_breach_to_maximum_depth * (dambreak%crest_level_ini - dambreak%crest_level_min)
            breach_width = dambreak%breach_width_ini
         else
            ! The logarithmic part, time_from_breaching in seconds
            breach_width = dambreak%a_coeff * log(time_from_breaching / dambreak%b_coeff)
         end if

         ! breach width must increase monotonically
         if (breach_width > dambreak%width) then
            dambreak%width = breach_width
         end if

         ! Verheij-vdKnaap(2002) formula
      else if (dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then

         if (time <= dambreak%end_time_first_phase) then
            ! phase 1: lowering
            dambreak%crest_level = dambreak%crest_level_ini - &
                                   time_from_breaching / dambreak%time_to_breach_to_maximum_depth * (dambreak%crest_level_ini - dambreak%crest_level_min)
            dambreak%width = dambreak%breach_width_ini
            dambreak%phase = 1
         else
            ! phase 2: widening
            dambreak%crest_level = dambreak%crest_level_min
            water_level_jump_dambreak = calculate_water_level_jump(upstream_water_level, downstream_water_level, &
                                                                   dambreak%crest_level)
            delta_level = (gravity * water_level_jump_dambreak)**1.5d0
            time_from_first_phase = time - dambreak%end_time_first_phase

            if (dambreak%width < dambreak%maximum_width .and. (.not. ieee_is_nan(dambreak%normal_velocity)) &
                .and. dabs(dambreak%normal_velocity) > dambreak%u_crit) then
               breach_width_derivative = (dambreak%f1 * dambreak%f2 / log(10D0)) * &
                                         (delta_level / (dambreak%u_crit * dambreak%u_crit)) * &
                                         (1.0 / (1.0 + (dambreak%f2 * gravity * time_from_first_phase / (dambreak%u_crit * SECONDS_IN_HOUR))))
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
         actual_maximum_width = min(dambreak%maximum_allowed_width, dambreak%maximum_width)
      else
         actual_maximum_width = dambreak%maximum_width
      end if

      !width cannot exceed the width of the snapped polyline
      if (dambreak%width >= actual_maximum_width) then
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
   subroutine adjust_bobs_on_dambreak_breach(width, max_width, crest_level, starting_link, left_link, right_link, &
                                             structure_id)
      use m_flowgeom, only: bob, bob0
      use messagehandling, only: msgbuf, LEVEL_WARN, SetMessage

      real(kind=dp), intent(in) :: width !< new width of breach [m]
      real(kind=dp), intent(in) :: max_width !< width of dambreak structure, i.e. maximum breach width [m]
      real(kind=dp), intent(in) :: crest_level !< breached crest level [m+REF]
      integer, intent(in) :: starting_link !< index of first link that breaches
      integer, intent(in) :: left_link !< last flow link on the "left"
      integer, intent(in) :: right_link !< last flow link on the "right"
      character(len=*), intent(in) :: structure_id !< name of the dambreak structure, only used in warning message

      integer :: k !< index of the dambreak flow link (range left_link to right_link)
      integer :: flow_link !< index of flow link
      real(kind=dp) :: left_breach_width !< width of the breach on the "left" [m]
      real(kind=dp) :: left_side !< total dambreak structure width on the "left" [m]
      real(kind=dp) :: remainder !< remaining breach width [m]
      real(kind=dp) :: right_breach_width !< width of the breach on the "right" [m]
      real(kind=dp) :: right_side !< total dambreak structure width on the "right" [m]

      ! process the breach at the starting link
      flow_link = abs(db_link_ids(starting_link))
      if (flow_link > 0 .and. width > 0.0_dp) then
         ! some breach, set to breached crest level
         bob(1, flow_link) = max(bob0(1, flow_link), crest_level)
         bob(2, flow_link) = max(bob0(2, flow_link), crest_level)
         db_active_links(starting_link) = 1
      else
         ! no breach
      end if

      ! distribute remaining breach width
      if (width <= db_link_effective_width(starting_link)) then
         ! breach width still less than width of starting link
         db_link_actual_width(starting_link) = max(width, 0.0_dp)
         left_breach_width = 0.0_dp
         right_breach_width = 0.0_dp
      else
         ! breach width larger than width of starting link
         db_link_actual_width(starting_link) = db_link_effective_width(starting_link)
         left_side = sum(db_link_effective_width(left_link:starting_link - 1))
         right_side = sum(db_link_effective_width(starting_link + 1:right_link))
         remainder = width - db_link_effective_width(starting_link)
         call calculate_dambreak_widening(remainder, left_side, right_side, left_breach_width, right_breach_width)
      end if

      ! process dam "left" of initial breach segment
      do k = starting_link - 1, left_link, -1
         flow_link = abs(db_link_ids(k))
         if (left_breach_width > 0.0_dp) then
            ! some breach, set to breached crest level
            if (flow_link > 0) then
               bob(1, flow_link) = max(bob0(1, flow_link), crest_level)
               bob(2, flow_link) = max(bob0(2, flow_link), crest_level)
            end if
            db_active_links(k) = 1
         else
            ! no breach
         end if
         if (left_breach_width >= db_link_effective_width(k)) then
            db_link_actual_width(k) = db_link_effective_width(k)
            left_breach_width = left_breach_width - db_link_effective_width(k)
         else
            db_link_actual_width(k) = left_breach_width
            left_breach_width = 0.0_dp
         end if
      end do

      ! process dam "right" of initial breach segment
      do k = starting_link + 1, right_link
         flow_link = abs(db_link_ids(k))
         if (right_breach_width > 0.0_dp) then
            ! some breach, set to breached crest level
            if (flow_link > 0) then
               bob(1, flow_link) = max(bob0(1, flow_link), crest_level)
               bob(2, flow_link) = max(bob0(2, flow_link), crest_level)
            end if
            db_active_links(k) = 1
         else
            ! no breach
         end if
         if (right_breach_width >= db_link_effective_width(k)) then
            db_link_actual_width(k) = db_link_effective_width(k)
            right_breach_width = right_breach_width - db_link_effective_width(k)
         else
            db_link_actual_width(k) = right_breach_width
            right_breach_width = 0.0_dp
         end if
      end do

      ! check for any unprocessed breach width
      if (left_breach_width > 1.0e-6_dp * max_width .or. right_breach_width > 1.0e-6_dp * max_width) then
         write (msgbuf, '(3a)') 'The breach  of dam ''', trim(structure_id), ''' exceeds the actual dam width on at least one side of the breach point.'
         call SetMessage(LEVEL_WARN, msgbuf)
      end if

   end subroutine adjust_bobs_on_dambreak_breach

   !< store upstream dambreak information
   module subroutine add_dambreaklocation_upstream(n_signal, node)

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak

      call add_dambreak_location(n_signal, node, UPSTREAM)

   end subroutine add_dambreaklocation_upstream

   !> store downstream dambreak information
   module subroutine add_dambreaklocation_downstream(n_signal, node)

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak

      call add_dambreak_location(n_signal, node, DOWNSTREAM)

   end subroutine add_dambreaklocation_downstream

   !> store upstream/downstream dambreak information
   subroutine add_dambreak_location(n_signal, node, up_down)
      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak
      integer, intent(in) :: up_down !< 1 - upstream, 2 - downstream

      n_locations(up_down) = n_locations(up_down) + 1
      location_mapping(n_locations(up_down), up_down) = n_signal
      locations(n_locations(up_down), up_down) = node

   end subroutine add_dambreak_location

   !> add upstream signal for averaging
   module subroutine add_averaging_upstream_signal(n_signal)

      integer, intent(in) :: n_signal !< number of current dambreak signal

      call add_averaging_signal(n_signal, UPSTREAM)

   end subroutine add_averaging_upstream_signal

   !> add downstream signal for averaging
   module subroutine add_averaging_downstream_signal(n_signal)

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
      use unstruc_channel_flow, only: network

      integer :: n !< index of the current dambreak signal

      if (n_db_links > 0) then ! needed, because n_db_signals may be > 0, but n_db_links==0, and then arrays are not available.
         do n = 1, n_db_signals
            if (dambreaks(n) == 0 .or. db_first_link(n) > db_last_link(n)) then
               cycle
            end if
            associate (dambreak => network%sts%struct(dambreaks(n))%dambreak)
               ! Update the crest/bed levels
               call adjust_bobs_on_dambreak_breach(dambreak%width, dambreak%maximum_width, dambreak%crest_level, &
                                                 & breach_start_link(n), db_first_link(n), db_last_link(n), &
                                                 & network%sts%struct(dambreaks(n))%id)
            end associate
         end do
      end if
   end subroutine adjust_bobs_for_dambreaks

   pure function is_not_db_active_link(link) result(res)

      integer, intent(in) :: link !< index of the flow link
      logical :: res !< True if the link is not an active dambreak link

      res = db_active_links(link) /= 1

   end function is_not_db_active_link

   !> get the starting link of the dambreak breach
   pure function get_dambreak_breach_start_link(n) result(n_start_link)

      integer, intent(in) :: n !< index of the current dambreak signal
      integer :: n_start_link !< index of the starting link

      n_start_link = abs(db_link_ids(breach_start_link(n)))

   end function get_dambreak_breach_start_link

   !> set the starting link of the dambreak breach
   module subroutine set_breach_start_link(n, Lstart)

      integer, intent(in) :: n !< index of the current dambreak signal
      integer, intent(in) :: Lstart !< index of the starting link

      breach_start_link(n) = db_first_link(n) - 1 + Lstart

   end subroutine set_breach_start_link

   !> fill dambreak values into valdambreak array
   module subroutine fill_dambreak_values(time_step, values)
      use m_flow, only: hu, au, q1
      use m_flowgeom, only: bob, ln
      use m_flowparameters, only: epshu
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network
      use m_link_ghostdata, only: link_ghostdata
      use unstruc_channel_flow, only: network
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

      do n = 1, n_db_signals_protected
         ! values(NUMVALS_DAMBREAK,n) is the cumulative over time, we do not reset it to 0
         values(1:NUMVALS_DAMBREAK - 1, n) = 0.0_dp
         index_structure = dambreaks(n)
         do link = db_first_link(n), db_last_link(n)
            if (is_not_db_active_link(link)) then
               cycle
            end if

            flow_link = abs(db_link_ids(link))
            if (jampi > 0) then
               call link_ghostdata(my_rank, idomain(ln(1, flow_link)), idomain(ln(2, flow_link)), &
                                   is_ghost_link, link_domain_number)
               if (is_ghost_link == 1) cycle
            end if
            values(IVAL_WIDTH, n) = values(IVAL_WIDTH, n) + db_link_actual_width(link)
            values(IVAL_DB_CRESTW, n) = values(IVAL_DB_CRESTW, n) + db_link_actual_width(link)
            if (hu(flow_link) > epshu) then
               values(IVAL_WIDTHWET, n) = values(IVAL_WIDTHWET, n) + db_link_actual_width(link)
               if (ln(1, flow_link) /= db_upstream_link_ids(link)) then
                  values(IVAL_DIS, n) = values(IVAL_DIS, n) - q1(flow_link)
               else
                  values(IVAL_DIS, n) = values(IVAL_DIS, n) + q1(flow_link)
               end if
               values(IVAL_AREA, n) = values(IVAL_AREA, n) + au(flow_link) ! flow area
            end if
         end do
         if (db_last_link(n) < db_first_link(n)) then ! NOTE: values(IVAL_DB_DISCUM,n) in a parallel simulation already gets values after mpi communication
            ! from the previous timestep. In the case that the dambreak does not exist on the current domain, it should
            ! not contribute to the cumulative discharge in the coming mpi communication so we set it to 0.
            values(IVAL_DB_DISCUM, n) = 0.0_dp
         else
            if (network%sts%struct(index_structure)%dambreak%width > 0.0_dp) then
               values(IVAL_DB_CRESTH, n) = network%sts%struct(index_structure)%dambreak%crest_level
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
            values(IVAL_S1UP, n) = db_upstream_levels(n)
            values(IVAL_S1DN, n) = db_downstream_levels(n)
            values(IVAL_HEAD, n) = values(IVAL_S1UP, n) - values(IVAL_S1DN, n)
            values(IVAL_VEL, n) = network%sts%struct(index_structure)%dambreak%normal_velocity
            values(IVAL_DB_JUMP, n) = network%sts%struct(index_structure)%dambreak%water_level_jump
            values(IVAL_DB_TIMEDIV, n) = network%sts%struct(index_structure)%dambreak%breach_width_derivative
            values(IVAL_DB_DISCUM, n) = values(IVAL_DB_DISCUM, n) + values(IVAL_DIS, n) * time_step ! cumulative discharge
         end if
      end do

   end subroutine fill_dambreak_values

   !< set dambreak widening method and returns the string with the method name, in case no correct method is specified
   module subroutine set_dambreak_widening_method(method_string)
      use messagehandling, only: mess, LEVEL_ERROR

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
      
      associate(left_side=>left_side, right_side => right_side)
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

end submodule sub_dambreak_breach
