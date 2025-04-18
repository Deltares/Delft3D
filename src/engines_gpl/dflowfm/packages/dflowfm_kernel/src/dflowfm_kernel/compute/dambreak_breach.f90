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

module m_dambreak_breach
   use precision, only: dp

   implicit none

   private

   public :: adjust_bobs_on_dambreak_breach
   public :: allocate_and_initialize_dambreak_data
   public :: update_dambreak_breach
   public :: add_dambreaklocation_upstream
   public :: add_dambreaklocation_downstream
   public :: add_averaging_upstream_signal
   public :: add_averaging_downstream_signal

   ! time varying, values can be retrieved via BMI interface
   real(kind=dp), dimension(:), allocatable, target, public :: db_breach_depths !< dambreak breach depths (as a level)
   real(kind=dp), dimension(:), allocatable, target, public :: db_breach_widths !< dambreak breach widths (as a level)
   real(kind=dp), dimension(:), allocatable, target, public :: db_upstream_levels !< upstream water levels computed each time step
   real(kind=dp), dimension(:), allocatable, target, public :: db_downstream_levels !< downstream water levels computed each time step

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

contains

   !> allocate arrays and initialize variables
   subroutine allocate_and_initialize_dambreak_data(n_db_signals)
      use m_alloc, only: realloc

      integer, intent(in) :: n_db_signals !< number of dambreak signals

      call realloc(db_upstream_levels, n_db_signals)
      call realloc(db_downstream_levels, n_db_signals)
      call realloc(db_weight_averaged_values, [NUMBER_COLUMNS, n_db_signals])
      call realloc(location_mapping, [n_db_signals, NUMBER_COLUMNS], fill=0)
      call realloc(locations, [n_db_signals, NUMBER_COLUMNS], fill=0)
      call realloc(averaging_mapping, [n_db_signals, NUMBER_COLUMNS], fill=0)
      n_locations(:) = 0
      n_averaging(:) = 0

   end subroutine allocate_and_initialize_dambreak_data

   !> updates dambreak breach by updating water levels upstream and downstream and calculating dambreak widths
   subroutine update_dambreak_breach(start_time, delta_time)
      use m_flow, only: hu, au, u1
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network
      use m_partitioninfo, only: get_average_quantity_from_links
      use fm_external_forcings_data, only: success, n_db_links, n_db_signals, dambreaks, &
                                           db_first_link, db_last_link, db_link_ids, db_active_links

      real(kind=dp), intent(in) :: start_time !< start time
      real(kind=dp), intent(in) :: delta_time !< delta time

      integer :: error !< error code
      integer :: n !< index of the current dambreak signal
      integer :: i_structure !< index of the structure

      if (n_db_signals <= 0) then
         return
      end if
      ! Variable n_db_signals is >0 for all partitions if there is a dambreak, even if it is outside
      ! of a partition. In a parallel simulation, we need to run this subroutine even in a special situation that there is
      ! no dambreak on the current subdomain (i.e. n_db_links == 0), because the following function get_average_quantity_from_links
      ! involves mpi communication among all subdomains. However, in this special situation,
      ! all the necessary variables are set to 0 and do not participate the dambreak related computation in this subroutine.

      call reset_dambreak_variables(n_db_signals)

      call update_dambreak_water_levels(start_time, UPSTREAM, db_upstream_levels, error)
      if (error /= 0) then
         success = .false.
         return
      end if

      call update_dambreak_water_levels(start_time, DOWNSTREAM, db_downstream_levels, error)
      if (error /= 0) then
         success = .false.
         return
      end if

      ! u1 velocity on the flowlinks (averaged by the wetted area). The mask is the water level itself
      error = get_average_quantity_from_links(db_first_link, db_last_link, au, db_link_ids(3, :), u1, &
                                              db_link_ids(3, :), db_weight_averaged_values, 1, hu, dmiss, &
                                              db_active_links, 0)
      if (error /= 0) then
         success = .false.
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

   end subroutine update_dambreak_breach

   !> reset dambreak variables like water levels, averaged values etc.
   subroutine reset_dambreak_variables(n_db_signals)
      use fm_external_forcings_data, only: dambreaks
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
   subroutine update_dambreak_water_levels(start_time, up_down, water_levels, error)
      use m_flow, only: s1, hu
      use m_partitioninfo, only: get_average_quantity_from_links
      use fm_external_forcings_data, only: n_db_links, dambreaks, breach_start_link, db_first_link, db_last_link, &
                                           db_link_ids, db_active_links
      use m_flowgeom, only: wu
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network

      real(kind=dp), intent(in) :: start_time !< start time
      integer, intent(in) :: up_down !< 1 - upstream, 2 - downstream
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
                                                 db_link_ids(3, :), s1, db_link_ids(up_down, :), db_weight_averaged_values, &
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
                     s1(db_link_ids(up_down, breach_start_link(averaging_mapping(n, up_down))))
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
      use m_dambreak, only: prepare_dambreak_calculation, BREACH_GROWTH_VDKNAAP, BREACH_GROWTH_VERHEIJVDKNAAP, &
                            BREACH_GROWTH_TIMESERIES
      use m_meteo, only: ec_gettimespacevalue_by_itemID, ecInstancePtr, item_db_levels_widths_table
      use fm_external_forcings_data, only: success, n_db_signals, dambreaks, db_levels_widths_table
      use m_flowtimes, only: irefdate, tunit, tzone

      real(kind=dp), intent(in) :: start_time !< start_time
      real(kind=dp), intent(in) :: delta_time !< delta_time

      integer :: n !< index of the current dambreak signal
      integer :: i_structure !< index of the structure
      real(kind=dp) :: s_max, s_min, h_max, h_min

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
                                                        irefdate, tzone, tunit, start_time - dambreak%t0)
               ! NOTE: AvD: the code above works correctly, but is dangerous:
               ! the addtimespace for dambreak has added each dambreak separately with a targetoffset.
               ! The gettimespace above, however, gets the values for *all* dambreaks, but with the relative time
               ! of the *current* dambreak #n.
               ! This means that if t0 values for all dambreaks are different, then the db_levels_widths_table(1:n-1) have become obsolete now.
               ! It works, because in the previous loop iterations the values that were then still correct
               ! have already been set into the %crest_level and %width values.
               if (success) then
                  dambreak%crest_level = db_levels_widths_table((n - 1) * 2 + 1)
                  dambreak%width = db_levels_widths_table((n - 1) * 2 + 2)
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
               s_max = max(db_upstream_levels(n), db_downstream_levels(n))
               s_min = min(db_upstream_levels(n), db_downstream_levels(n))
               h_max = max(0.0_dp, s_max - dambreak%crest_level)
               h_min = max(0.0_dp, s_min - dambreak%crest_level)
               dambreak%water_level_jump = h_max - h_min
            end if
         end associate
      end do

   end subroutine calculate_dambreak_widths

   !> update the crest/bed levels for dambreak breach
   subroutine adjust_bobs_on_dambreak_breach(width, max_width, crest_level, starting_link, left_link, right_link, &
                                             structure_id)
      use m_flowgeom, only: bob, bob0
      use fm_external_forcings_data, only: db_link_ids, db_active_links, db_link_effective_width, db_link_actual_width
      use m_dambreak, only: dambreak_widening, DBW_SYMM, DBW_PROP, DBW_SYMM_ASYMM
      use messagehandling, only: msgbuf, LEVEL_WARN, SetMessage

      real(kind=dp), intent(in) :: width !< new width of breach [m]
      real(kind=dp), intent(in) :: max_width !< width of dambreak structure, i.e. maximum breach width [m]
      real(kind=dp), intent(in) :: crest_level !< breached crest level [m+REF]
      integer, intent(in) :: starting_link !< index of first link that breaches
      integer, intent(in) :: left_link !< last flow link on the "left"
      integer, intent(in) :: right_link !< last flow link on the "right"
      character(len=*), intent(in) :: structure_id !< name of the dambreak structure, only used in warning message

      integer :: k !< index of the dambreak flow link (range left_link to right_link)
      integer :: Lf !< index of flow link
      real(kind=dp) :: h_remainder !< half of the remaining breach width [m]
      real(kind=dp) :: left_breach_width !< width of the breach on the "left" [m]
      real(kind=dp) :: left_frac !< fraction of structure width on the "left" [-]
      real(kind=dp) :: left_side !< total dambreak structure width on the "left" [m]
      real(kind=dp) :: remainder !< remaining breach width [m]
      real(kind=dp) :: right_breach_width !< width of the breach on the "right" [m]
      real(kind=dp) :: right_side !< total dambreak structure width on the "right" [m]

      ! process the breach at the starting link
      Lf = abs(db_link_ids(3, starting_link))
      if (Lf > 0 .and. width > 0.0_dp) then
         ! some breach, set to breached crest level
         bob(1, Lf) = max(bob0(1, Lf), crest_level)
         bob(2, Lf) = max(bob0(2, Lf), crest_level)
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
         select case (dambreak_widening)
         case (DBW_SYMM)
            ! original implementation which triggers a breach too wide error be
            h_remainder = 0.5_dp * remainder
            left_breach_width = h_remainder
            right_breach_width = h_remainder
         case (DBW_PROP)
            ! proportional
            left_frac = left_side / (left_side + right_side)
            left_breach_width = left_frac * remainder
            right_breach_width = (1.0_dp - left_frac) * remainder
         case (DBW_SYMM_ASYMM)
            ! first symmetric, then asymmetric
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
         end select
      end if

      ! process dam "left" of initial breach segment
      do k = starting_link - 1, left_link, -1
         Lf = abs(db_link_ids(3, k))
         if (left_breach_width > 0.0_dp) then
            ! some breach, set to breached crest level
            if (Lf > 0) then
               bob(1, Lf) = max(bob0(1, Lf), crest_level)
               bob(2, Lf) = max(bob0(2, Lf), crest_level)
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
         Lf = abs(db_link_ids(3, k))
         if (right_breach_width > 0.0_dp) then
            ! some breach, set to breached crest level
            if (Lf > 0) then
               bob(1, Lf) = max(bob0(1, Lf), crest_level)
               bob(2, Lf) = max(bob0(2, Lf), crest_level)
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
      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak
      integer, intent(in) :: up_down !< 1 - upstream, 2 - downstream

      n_locations(up_down) = n_locations(up_down) + 1
      location_mapping(n_locations(up_down), up_down) = n_signal
      locations(n_locations(up_down), up_down) = node

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

end module m_dambreak_breach
