!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
   use m_meteo, only: ec_undef_int

   implicit none

   integer, parameter :: UPSTREAM = 1
   integer, parameter :: DOWNSTREAM = 2
   real(kind=dp), dimension(2, 1) :: weighted_sum !< (1,:) weighted sum of water level or velocity over dambreak
                                                  !! (2,:) weighted sum of unity over dambreak
   integer, dimension(:), allocatable :: first_link !< first dambreak link for each signal
   integer, dimension(:), allocatable :: last_link !< last dambreak link for each signal

   type :: t_dambreak !< data for a single dambreak
      integer :: algorithm = 0 !< algorithm for the dambreak breach growth
      integer :: breach_start_link = -1 !< index of the starting link in the breach growth
      integer :: index_structure = 0 !< index of the structure
      integer :: ec_item = ec_undef_int !< item for EC module to get crest level and width from a tim file
      integer :: number_of_links = 0 !< number of links in the dambreak
      integer :: link_map_offset = 0 !< offset of the local array in the global link array
      integer, dimension(:), allocatable :: link_indices !< link indices of the dambreak
      integer, dimension(:), allocatable :: active_links !< active links of the dambreak
      integer, dimension(:), allocatable :: upstream_link_ids !< upstream link indices
      integer, dimension(:), allocatable :: downstream_link_ids !< downstream link indices
      character(len=128) :: name = "" !< name of the dambreak
      real(kind=dp) :: breach_depth = 0.0_dp !< depth of the breach
      real(kind=dp) :: breach_width = 0.0_dp !< width of the breach
      real(kind=dp) :: breach_width_ini = 0.0_dp !< initial width of the breach
      real(kind=dp) :: breach_width_derivative !< derivative of the breach width
      real(kind=dp) :: crest_level !< crest level of the breach
      real(kind=dp) :: crest_level_ini !< initial crest level of the breach
      real(kind=dp) :: crest_level_min !< minimum crest level of the breach
      real(kind=dp) :: upstream_level !< upstream water level
      real(kind=dp) :: downstream_level !< downstream water level
      real(kind=dp), dimension(2) :: crest_level_and_width !< array to communicate with EC module
      real(kind=dp) :: width = 0.0_dp !< width of the breach
      real(kind=dp) :: maximum_width = 0.0_dp !< maximum width of the breach
      real(kind=dp) :: maximum_allowed_width = -1.0_dp !< maximum allowed width of the breach
      real(kind=dp) :: water_level_jump !< water level jump of the breach
      real(kind=dp) :: normal_velocity !< normal velocity of the breach
      real(kind=dp) :: u_crit !< critical velocity for the breach growth
      real(kind=dp) :: t0 = 0.0_dp !< time of the start of the dambreak
      real(kind=dp) :: time_to_breach_to_maximum_depth !< time to breach to maximum depth
      real(kind=dp) :: a_coeff !< coefficient a for the breach growth
      real(kind=dp) :: b_coeff !< coefficient b for the breach growth
      real(kind=dp) :: f1 !< coefficient f1 for the breach width derivative
      real(kind=dp) :: f2 !< coefficient f2 for the breach width derivative
      real(kind=dp), dimension(:), allocatable :: link_actual_width !< actual width of the links in the dambreak
      real(kind=dp), dimension(:), allocatable :: link_effective_width !< effective width of the links in the dambreak
      procedure(calculate_breach_growth_using_any_model), pointer :: calculate_breach_growth => null()
   contains
      procedure :: array_allocation => allocate_arrays
      final :: deallocate_arrays
   end type

   type(t_dambreak), target, dimension(:), allocatable :: dambreaks(:) !< dambreak data for all dambreaks

   abstract interface
      subroutine calculate_breach_growth_using_any_model(dambreak, time, time_step)
         use precision, only: dp
         import t_dambreak
         class(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
         real(kind=dp), intent(in) :: time !< current time
         real(kind=dp), intent(in) :: time_step !< time step
      end subroutine calculate_breach_growth_using_any_model
   end interface

   integer :: number_of_mappings !< number of mappings for upstream and downstream water levels
   type :: t_mapping !< mapping of water level
      integer :: location = 0 !< location of the water level
      real(kind=dp), pointer :: water_level !< dambreak water level
   end type
   type(t_mapping), dimension(:), allocatable :: mappings !< mapping of water levels

   integer :: number_of_averagings !< number of averagings for upstream and downstream water levels
   type :: t_averaging !< data to average a water level over links that are upstream or downstream to the dambreak
      type(t_dambreak), pointer :: dambreak => null() !< dambreak
      integer, dimension(:), pointer :: link_ids !< upstream or downstream link indices
      real(kind=dp), pointer :: water_level !< upstream or downstream water level
   end type
   type(t_averaging), dimension(:), allocatable :: averagings !< array of data to average water levels upstream or downstream of dambreaks

   procedure(calculate_dambreak_widening_any), pointer :: calculate_dambreak_widening

   abstract interface
      subroutine calculate_dambreak_widening_any(remainder, left_side, right_side, breach_left_width, breach_right_width)
         use precision, only: dp
         real(kind=dp), intent(in) :: remainder !< remaining width
         real(kind=dp), intent(in) :: left_side !< left side of the breach
         real(kind=dp), intent(in) :: right_side !< right side of the breach
         real(kind=dp), intent(inout) :: breach_left_width !< width of the breach on the "left" [m]
         real(kind=dp), intent(inout) :: breach_right_width !< width of the breach on the "right" [m]
      end subroutine
   end interface

   integer, dimension(1), parameter :: UNITY_INDEX = 1
   integer, dimension(1) :: last_index

contains

   !> allocate arrays and initialize variables
   subroutine allocate_and_initialize_dambreak_data()

      integer :: n !< loop index

      if (allocated(dambreaks)) then
         deallocate (dambreaks)
      end if

      allocate (dambreaks(n_db_signals))
      do n = 1, n_db_signals
         dambreaks(n)%link_map_offset = first_link(n) - 1
         if (first_link(n) > last_link(n)) then
            dambreaks(n)%number_of_links = 0
         else
            dambreaks(n)%number_of_links = last_link(n) - first_link(n) + 1
         end if
         call dambreaks(n)%array_allocation()
      end do

      if (allocated(mappings)) then
         deallocate (mappings)
      end if
      allocate (mappings(2 * n_db_signals))

      if (allocated(averagings)) then
         deallocate (averagings)
      end if
      allocate (averagings(2 * n_db_signals))

      number_of_mappings = 0
      number_of_averagings = 0

   end subroutine allocate_and_initialize_dambreak_data

   !> alllocate internal arrays for a dambreak
   subroutine allocate_arrays(this)
      class(t_dambreak), intent(inout) :: this

      allocate (this%link_indices(this%number_of_links), source=0)
      allocate (this%active_links(this%number_of_links), source=0)
      allocate (this%upstream_link_ids(this%number_of_links), source=0)
      allocate (this%downstream_link_ids(this%number_of_links), source=0)
      allocate (this%link_actual_width(this%number_of_links), source=0.0_dp)
      allocate (this%link_effective_width(this%number_of_links), source=0.0_dp)

   end subroutine allocate_arrays

   !> dealllocate internal arrays for a dambreak
   subroutine deallocate_arrays(this)
      type(t_dambreak), intent(inout) :: this

      if (allocated(this%link_indices)) then
         deallocate (this%link_indices)
         deallocate (this%active_links)
         deallocate (this%upstream_link_ids)
         deallocate (this%downstream_link_ids)
         deallocate (this%link_actual_width)
         deallocate (this%link_effective_width)
      end if
   end subroutine deallocate_arrays

   !> updates dambreak breach by updating water levels upstream and downstream and calculating dambreak widths
   module function update_dambreak_breach(current_time, time_step) result(error)
      use m_flow, only: s1, hu, au, u1
      use m_missing, only: dmiss
      use m_partitioninfo, only: get_average_quantity_from_links

      real(kind=dp), intent(in) :: current_time !< current time
      real(kind=dp), intent(in) :: time_step !< time step
      integer :: error !< error code

      integer :: n

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

      do n = 1, number_of_averagings
         call update_water_level_using_averaging(current_time, averagings(n)%dambreak, averagings(n)%link_ids, &
                                                 averagings(n)%water_level, error)
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
         do n = 1, n_db_signals
            if (dambreaks(n)%index_structure /= 0) then
               call dambreaks(n)%calculate_breach_growth(current_time, time_step)
            end if
         end do
      end if

   end function update_dambreak_breach

   !> reset dambreak variables like water levels, averaged values etc.
   subroutine reset_dambreak_variables()

      integer :: n !< index of the current dambreak signal

      do n = 1, n_db_signals
         if (dambreaks(n)%index_structure > 0) then
            dambreaks(n)%normal_velocity = 0.0_dp
            dambreaks(n)%breach_width_derivative = 0.0_dp
            dambreaks(n)%water_level_jump = 0.0_dp
            dambreaks(n)%upstream_level = 0.0_dp
            dambreaks(n)%downstream_level = 0.0_dp
         end if
      end do
   end subroutine reset_dambreak_variables

   !> update water levels using averaging
   subroutine update_water_level_using_averaging(current_time, dambreak, link_ids, water_level, error)
      use m_flow, only: s1, hu
      use m_partitioninfo, only: get_average_quantity_from_links
      use m_flowgeom, only: wu
      use m_missing, only: dmiss

      real(kind=dp), intent(in) :: current_time !< current time
      type(t_dambreak), intent(inout) :: dambreak !< dambreak
      integer, dimension(:), intent(in) :: link_ids !< upstream or downstream link ids
      real(kind=dp), intent(inout) :: water_level !< water level
      integer, intent(out) :: error !< error code

      error = 0

      weighted_sum(:, :) = 0.0_dp
      last_index = dambreak%number_of_links
      error = get_average_quantity_from_links(UNITY_INDEX, last_index, wu, link_ids, s1, link_ids, weighted_sum, &
                                              0, hu, dmiss, dambreak%active_links, 0)
      if (error /= 0) then
         return
      end if
      if (dambreak%number_of_links > 0) then
         if (weighted_sum(2, 1) > 0.0_dp) then
            water_level = weighted_sum(1, 1) / weighted_sum(2, 1)
         else if (abs(current_time - dambreak%T0) < 1e-10_dp) then
            water_level = s1(link_ids(dambreak%breach_start_link))
         else
            continue
         end if
      end if

   end subroutine update_water_level_using_averaging

   !> Calculate breach growth using vdKnaap model
   subroutine calculate_breach_growth_using_vdKnaap_model(dambreak, time, time_step)
      import t_dambreak

      class(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
      real(kind=dp), intent(in) :: time !< current time
      real(kind=dp), intent(in) :: time_step !< time step

      real(kind=dp) :: breach_width
      real(kind=dp) :: time_since_breaching

      time_since_breaching = time - dambreak%t0

      ! breaching not started
      if (time_since_breaching < 0.0_dp) then
         return
      end if

      !vdKnaap(2000) formula: to do: implement table
      if (time_since_breaching < dambreak%time_to_breach_to_maximum_depth) then
         ! The linear part
         dambreak%crest_level = dambreak%crest_level_ini - &
                                time_since_breaching / dambreak%time_to_breach_to_maximum_depth * &
                                (dambreak%crest_level_ini - dambreak%crest_level_min)
         breach_width = dambreak%breach_width_ini
      else
         ! The logarithmic part, time_since_breaching in seconds
         breach_width = dambreak%a_coeff * log(time_since_breaching / dambreak%b_coeff)
      end if

      ! breach width must increase monotonically
      dambreak%width = max(dambreak%width, breach_width)

      ! in vdKnaap(2000) the maximum allowed branch width is limited (see sobek manual and set_dambreak_coefficients subroutine below)
      dambreak%width = min(dambreak%width, dambreak%maximum_allowed_width, dambreak%maximum_width)

      dambreak%breach_width_derivative = (dambreak%width - dambreak%breach_width) / time_step

      dambreak%breach_width = dambreak%width
      dambreak%breach_depth = dambreak%crest_level

   end subroutine calculate_breach_growth_using_vdKnaap_model

   !> Calculate breach growth using Verheij-vdKnaap(2002) formula
   subroutine calculate_breach_growth_using_Verheij_vdKnaap_model(dambreak, time, time_step)
      use ieee_arithmetic, only: ieee_is_nan
      use m_physcoef, only: gravity => ag
      import t_dambreak

      class(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
      real(kind=dp), intent(in) :: time !< current time
      real(kind=dp), intent(in) :: time_step !< time step

      real(kind=dp), parameter :: SECONDS_IN_HOUR = 3600.0_dp
      real(kind=dp) :: delta_level
      real(kind=dp) :: time_since_breaching
      real(kind=dp) :: time_after_first_phase
      real(kind=dp) :: width_increment
      real(kind=dp) :: water_level_jump
      real(kind=dp) :: breach_width_derivative

      time_since_breaching = time - dambreak%t0

      ! breaching not started
      if (time_since_breaching < 0.0_dp) then
         return
      end if

      breach_width_derivative = 0.0_dp
      water_level_jump = 0.0_dp

      if (time_since_breaching <= dambreak%time_to_breach_to_maximum_depth) then
         ! phase 1: lowering
         dambreak%crest_level = dambreak%crest_level_ini - &
                                time_since_breaching / dambreak%time_to_breach_to_maximum_depth * &
                                (dambreak%crest_level_ini - dambreak%crest_level_min)
         dambreak%width = dambreak%breach_width_ini
      else
         ! phase 2: widening
         dambreak%crest_level = dambreak%crest_level_min
         water_level_jump = calculate_water_level_jump(dambreak)
         delta_level = (gravity * water_level_jump)**1.5_dp
         time_after_first_phase = time_since_breaching - dambreak%time_to_breach_to_maximum_depth

         if (dambreak%width < dambreak%maximum_width .and. (.not. ieee_is_nan(dambreak%normal_velocity)) &
             .and. abs(dambreak%normal_velocity) > dambreak%u_crit) then
            breach_width_derivative = (dambreak%f1 * dambreak%f2 / log(10.0_dp)) * &
                                      (delta_level / (dambreak%u_crit * dambreak%u_crit)) * &
                                      (1.0_dp / (1.0_dp + (dambreak%f2 * gravity * time_after_first_phase / &
                                                           (dambreak%u_crit * SECONDS_IN_HOUR))))
            width_increment = breach_width_derivative * (time_step / SECONDS_IN_HOUR)
            !ensure monotonically increasing dambreak%width
            if (width_increment > 0.0_dp) then
               dambreak%width = dambreak%width + width_increment
            end if
         end if
      end if
      dambreak%breach_width_derivative = breach_width_derivative
      dambreak%water_level_jump = water_level_jump

      !width cannot exceed the width of the snapped polyline
      dambreak%width = min(dambreak%width, dambreak%maximum_width)

      dambreak%breach_width = dambreak%width
      dambreak%breach_depth = dambreak%crest_level

   end subroutine calculate_breach_growth_using_Verheij_vdKnaap_model

   !> Calculate breach growth using tables
   subroutine calculate_breach_growth_using_tables(dambreak, time, time_step)
      use m_meteo, only: ec_gettimespacevalue_by_itemID, ecInstancePtr
      use m_flowtimes, only: irefdate, tunit, tzone
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage
      import t_dambreak

      class(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
      real(kind=dp), intent(in) :: time !< current time
      real(kind=dp), intent(in) :: time_step !< time step

      logical :: success !< success flag

      if (time > dambreak%t0) then
         !Time in the tim file is relative to start time t0
         success = ec_gettimespacevalue_by_itemID(ecInstancePtr, dambreak%ec_item, irefdate, tzone, tunit, &
                                                  time - dambreak%t0, dambreak%crest_level_and_width)
         if (success) then
            dambreak%crest_level = dambreak%crest_level_and_width(1)
            dambreak%width = dambreak%crest_level_and_width(2)
         else
            write (msgbuf, '(3a)') 'Error retrieving crest level and width for dambreak "', trim(dambreak%name), &
               '". Please check timeseries input file.'
            call SetMessage(LEVEL_ERROR, msgbuf)
         end if
      end if

      dambreak%breach_width_derivative = (dambreak%width - dambreak%breach_width) / time_step

      dambreak%breach_width = dambreak%width
      dambreak%breach_depth = dambreak%crest_level

      dambreak%water_level_jump = calculate_water_level_jump(dambreak)

   end subroutine calculate_breach_growth_using_tables

   !> calculate the water level jump for dambreaks
   pure function calculate_water_level_jump(dambreak) result(water_level_jump)

      type(t_dambreak), intent(in) :: dambreak !< dambreak data for a single dambreak
      real(kind=dp) :: water_level_jump !< water level jump [m]

      real(kind=dp) :: height_max, height_min

      height_max = max(dambreak%upstream_level, dambreak%downstream_level) - dambreak%crest_level
      height_min = min(dambreak%upstream_level, dambreak%downstream_level) - dambreak%crest_level
      water_level_jump = max(0.0_dp, height_max) - max(0.0_dp, height_min)

   end function calculate_water_level_jump

   ! Adjust bobs for dambreak
   module subroutine adjust_bobs_for_dambreaks()

      integer :: n !< index of the current dambreak signal

      if (n_db_links > 0) then ! needed, because n_db_signals may be > 0, but n_db_links==0, and then arrays are not available.
         do n = 1, n_db_signals
            if (dambreaks(n)%index_structure /= 0 .and. dambreaks(n)%number_of_links /= 0) then
               call update_crest_bed_levels(dambreaks(n))
            end if
         end do
      end if
   end subroutine adjust_bobs_for_dambreaks

   !> update the crest/bed levels for dambreak breach
   subroutine update_crest_bed_levels(dambreak)
      use messagehandling, only: msgbuf, LEVEL_WARN, SetMessage

      type(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak

      integer :: k !< index of the dambreak flow link (range left_link to right_link)
      real(kind=dp) :: breach_left_width !< width of the breach on the "left" [m]
      real(kind=dp) :: left_side_width !< total dambreak structure width on the "left" [m]
      real(kind=dp) :: remainder !< remaining breach width [m]
      real(kind=dp) :: breach_right_width !< width of the breach on the "right" [m]
      real(kind=dp) :: right_side_width !< total dambreak structure width on the "right" [m]

      ! process the breach at the starting link
      if (dambreak%width > 0.0_dp) then
         call set_bobs_to_crest_level(dambreak%breach_start_link, dambreak)
         dambreak%active_links(dambreak%breach_start_link) = 1
      else
         ! no breach
      end if

      ! distribute remaining breach width
      if (dambreak%width <= dambreak%link_effective_width(dambreak%breach_start_link)) then
         ! breach width still less than width of starting link
         dambreak%link_actual_width(dambreak%breach_start_link) = max(dambreak%width, 0.0_dp)
         breach_left_width = 0.0_dp
         breach_right_width = 0.0_dp
      else
         ! breach width larger than width of starting link
         dambreak%link_actual_width(dambreak%breach_start_link) = dambreak%link_effective_width(dambreak%breach_start_link)
         left_side_width = sum(dambreak%link_effective_width(1:dambreak%breach_start_link - 1))
         right_side_width = sum(dambreak%link_effective_width(dambreak%breach_start_link + 1:dambreak%number_of_links))
         remainder = dambreak%width - dambreak%link_effective_width(dambreak%breach_start_link)
         call calculate_dambreak_widening(remainder, left_side_width, right_side_width, breach_left_width, breach_right_width)
      end if

      ! process dam "left" of initial breach segment
      do k = dambreak%breach_start_link - 1, 1, -1
         call process_breach_side(k, dambreak, breach_left_width)
      end do

      ! process dam "right" of initial breach segment
      do k = dambreak%breach_start_link + 1, dambreak%number_of_links
         call process_breach_side(k, dambreak, breach_right_width)
      end do

      ! check for any unprocessed breach width
      if (breach_left_width > 1.0e-6_dp * dambreak%maximum_width .or. &
          breach_right_width > 1.0e-6_dp * dambreak%maximum_width) then
         write (msgbuf, '(3a)') 'The breach  of dam ''', trim(dambreak%name), &
            ''' exceeds the actual dam width on at least one side of the breach point.'
         call SetMessage(LEVEL_WARN, msgbuf)
      end if

   end subroutine update_crest_bed_levels

   ! process dam "left" or "right" of initial breach segment
   subroutine process_breach_side(k, dambreak, breach_side_width)
      integer, intent(in) :: k !< dam link index
      type(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
      real(kind=dp), intent(inout) :: breach_side_width !< width of the breach on the side [m]

      if (breach_side_width > 0.0_dp) then
         call set_bobs_to_crest_level(k, dambreak)
         dambreak%active_links(k) = 1
      else
         ! no breach
      end if
      if (breach_side_width >= dambreak%link_effective_width(k)) then
         dambreak%link_actual_width(k) = dambreak%link_effective_width(k)
         breach_side_width = breach_side_width - dambreak%link_effective_width(k)
      else
         dambreak%link_actual_width(k) = breach_side_width
         breach_side_width = 0.0_dp
      end if
   end subroutine process_breach_side

   !> set bobs to breach crest level
   subroutine set_bobs_to_crest_level(link, dambreak)
      use m_flowgeom, only: bob, bob0

      integer, intent(in) :: link !< dam link index
      type(t_dambreak), intent(in) :: dambreak !< dambreak data for a single dambreak

      integer :: flow_link

      flow_link = abs(dambreak%link_indices(link))

      if (flow_link > 0) then
         bob(1, flow_link) = max(bob0(1, flow_link), dambreak%crest_level)
         bob(2, flow_link) = max(bob0(2, flow_link), dambreak%crest_level)
      end if
   end subroutine set_bobs_to_crest_level

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

      integer :: n !< index of the current dambreak signal
      integer :: link !< index of the dambreak link
      integer :: flow_link !< index of the flow link
      integer :: is_ghost_link !< flow link is ghost link (1) or not (0)
      integer :: link_domain_number !< flow link domain number

      do n = 1, n_db_signals
         ! values(NUMVALS_DAMBREAK,n) is the cumulative over time, we do not reset it to 0
         values(1:NUMVALS_DAMBREAK - 1, n) = 0.0_dp
         do link = 1, dambreaks(n)%number_of_links
            if (dambreaks(n)%active_links(link) /= 1) then
               cycle
            end if

            flow_link = abs(dambreaks(n)%link_indices(link))
            if (jampi > 0) then
               call link_ghostdata(my_rank, idomain(ln(1, flow_link)), idomain(ln(2, flow_link)), &
                                   is_ghost_link, link_domain_number)
               if (is_ghost_link == 1) then
                  cycle
               end if
            end if
            values(IVAL_WIDTH, n) = values(IVAL_WIDTH, n) + dambreaks(n)%link_actual_width(link)
            values(IVAL_DB_CRESTW, n) = values(IVAL_DB_CRESTW, n) + dambreaks(n)%link_actual_width(link)
            if (hu(flow_link) > epshu) then
               values(IVAL_WIDTHWET, n) = values(IVAL_WIDTHWET, n) + dambreaks(n)%link_actual_width(link)
               if (ln(1, flow_link) /= dambreaks(n)%upstream_link_ids(link)) then
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
               flow_link = abs(dambreaks(n)%link_indices(dambreaks(n)%breach_start_link))
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
   subroutine calculate_dambreak_widening_symmetric(remainder, left_side, right_side, &
                                                    breach_left_width, breach_right_width)
      real(kind=dp), intent(in) :: remainder !< remaining width
      real(kind=dp), intent(in) :: left_side !< left side of the breach
      real(kind=dp), intent(in) :: right_side !< right side of the breach
      real(kind=dp), intent(inout) :: breach_left_width !< left breach width
      real(kind=dp), intent(inout) :: breach_right_width !< right breach width

      associate (left_side => left_side, right_side => right_side)
      end associate

      breach_left_width = 0.5_dp * remainder
      breach_right_width = 0.5_dp * remainder
   end subroutine

   !> proportional implementation of dambreak widening
   subroutine calculate_dambreak_widening_proportional(remainder, left_side, right_side, &
                                                       breach_left_width, breach_right_width)
      real(kind=dp), intent(in) :: remainder !< remaining width
      real(kind=dp), intent(in) :: left_side !< left side of the breach
      real(kind=dp), intent(in) :: right_side !< right side of the breach
      real(kind=dp), intent(inout) :: breach_left_width !< left breach width
      real(kind=dp), intent(inout) :: breach_right_width !< right breach width

      real(kind=dp) :: left_frac !< fraction of structure width on the "left" [-]

      left_frac = left_side / (left_side + right_side)
      breach_left_width = left_frac * remainder
      breach_right_width = (1.0_dp - left_frac) * remainder
   end subroutine

   !> symmetric/asymmetric implementation of dambreak widening
   subroutine calculate_dambreak_widening_symmetric_asymmetric(remainder, left_side, right_side, &
                                                               breach_left_width, breach_right_width)
      real(kind=dp), intent(in) :: remainder !< remaining width
      real(kind=dp), intent(in) :: left_side !< left side of the breach
      real(kind=dp), intent(in) :: right_side !< right side of the breach
      real(kind=dp), intent(inout) :: breach_left_width !< left breach width
      real(kind=dp), intent(inout) :: breach_right_width !< right breach width

      real(kind=dp) :: h_remainder !< half of the remaining breach width [m]

      h_remainder = 0.5_dp * remainder
      if (h_remainder < min(left_side, right_side)) then
         breach_left_width = h_remainder
         breach_right_width = h_remainder
      elseif (left_side <= right_side) then
         breach_left_width = left_side
         breach_right_width = remainder - left_side
      else
         breach_right_width = right_side
         breach_left_width = remainder - right_side
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

      integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
      integer, dimension(:), intent(in) :: lftopol !< the mapping array from flow link to intersecting polyline segment.

      logical, parameter :: NEW_VERSION_OF_DAMBREAK_STRUCTURES = .true.

      call set_dambreaks_configuration(dambridx, lftopol, NEW_VERSION_OF_DAMBREAK_STRUCTURES)

   end subroutine update_dambreak_administration

   !> Update dambreak administration, old version.
   module subroutine update_dambreak_administration_old(dambridx, lftopol)

      integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
      integer, dimension(:), intent(in) :: lftopol !< the mapping array from flow link to intersecting polyline segment.

      logical, parameter :: NEW_VERSION_OF_DAMBREAK_STRUCTURES = .false.

      call set_dambreaks_configuration(dambridx, lftopol, NEW_VERSION_OF_DAMBREAK_STRUCTURES)

   end subroutine update_dambreak_administration_old

   !> set dambreaks configuration.
   subroutine set_dambreaks_configuration(dambridx, lftopol, new_version_of_dambreak_structures)
      use unstruc_channel_flow, only: network
      use m_meteo, only: kedb, dambreakPolygons
      use m_dambreak, only: BREACH_GROWTH_VERHEIJVDKNAAP, BREACH_GROWTH_TIMESERIES

      integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
      integer, dimension(:), intent(in) :: lftopol !< the mapping array from flow link to intersecting polyline segment.
      logical, intent(in) :: new_version_of_dambreak_structures !< whether to use the new version of the dambreak administration

      integer :: n, index_in_structure, index_dambridx, index_in_polygons
      logical :: success !< success flag
      integer :: number_of_points
      real(kind=dp), dimension(:), pointer :: x_points, y_points

      if (n_db_signals <= 0) then
         n_db_links = 0
         return
      end if

      n_db_links = last_link(n_db_signals)
      call allocate_and_initialize_dambreak_data()

      do n = 1, n_db_signals

         index_dambridx = dambridx(n)
         if (index_dambridx == -1) then
            cycle
         end if

         if (new_version_of_dambreak_structures) then
            index_in_structure = index_dambridx
            associate (source => network%sts%struct(index_in_structure))
               call set_links(dambreaks(n), source%linknumbers)
               number_of_points = source%numCoordinates
               x_points => source%xCoordinates
               y_points => source%yCoordinates
            end associate
         else
            call set_links(dambreaks(n), kedb(1 + dambreaks(n)%link_map_offset: &
                                              dambreaks(n)%number_of_links + dambreaks(n)%link_map_offset))
            call read_dambreak_structure_if_it_was_not_done(index_dambridx, dambreaks(n), index_in_structure, &
                                                            index_in_polygons, success)
            if (.not. success) then
               cycle
            end if
            number_of_points = dambreakPolygons(index_in_polygons)%np
            x_points => dambreakPolygons(index_in_polygons)%xp
            y_points => dambreakPolygons(index_in_polygons)%yp
         end if

         associate (source => network%sts%struct(index_in_structure)%dambreak)
            call set_dambreak_data(index_in_structure, network%sts%struct(index_in_structure)%id, &
                                   source, dambreaks(n))

            call set_time_series_if_needed(source%levels_and_widths, dambreaks(n))

            if (dambreaks(n)%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP &
                .or. dambreaks(n)%algorithm == BREACH_GROWTH_TIMESERIES) then
               call add_location_for_mapping_and_averaging(source%water_level_upstream_location_x, &
                                                           source%water_level_upstream_location_y, &
                                                           source%water_level_upstream_node_id, n, UPSTREAM)
               call add_location_for_mapping_and_averaging(source%water_level_downstream_location_x, &
                                                           source%water_level_downstream_location_y, &
                                                           source%water_level_downstream_node_id, n, DOWNSTREAM)
            end if

            call calculate_start_link_and_widths(dambreaks(n), source%start_location_x, source%start_location_y, &
                                                 number_of_points, x_points, y_points, lftopol)
         end associate
         if (.not. new_version_of_dambreak_structures) then
            deallocate (dambreakPolygons(index_in_polygons)%yp)
            deallocate (dambreakPolygons(index_in_polygons)%xp)
         end if
      end do

   end subroutine set_dambreaks_configuration

   !> update array of logicals indicating if the link contains dambreaks
   pure module subroutine indicate_links_that_contain_dambreaks(does_link_contain_structures)

      logical, intent(inout) :: does_link_contain_structures(:) !< array of logicals indicating if the link contains structures

      integer :: n !< loop index
      integer :: k !< loop index

      do n = 1, n_db_signals
         if (dambreaks(n)%index_structure /= 0) then
            do k = 1, dambreaks(n)%number_of_links
               does_link_contain_structures(abs(dambreaks(n)%link_indices(k))) = .true.
            end do
         end if
      end do

   end subroutine indicate_links_that_contain_dambreaks

   !> Check if any dambreaks are active in the current grid
   pure module function should_write_dambreaks() result(res)

      logical :: res

      res = any(dambreaks(1:n_db_signals)%number_of_links > 0)

   end function should_write_dambreaks

   !> set correct flow areas for dambreaks, using the actual flow width
   module subroutine set_flow_areas_for_dambreaks(hu, au)

      real(kind=dp), dimension(:), intent(in) :: hu !< source
      real(kind=dp), dimension(:), intent(inout) :: au !< results

      integer :: n !< loop index
      integer :: k !< loop index
      integer :: link !< link index

      do n = 1, n_db_signals
         do k = 1, dambreaks(n)%number_of_links
            link = abs(dambreaks(n)%link_indices(k))
            au(link) = hu(link) * dambreaks(n)%link_actual_width(k)
         end do
      end do

   end subroutine set_flow_areas_for_dambreaks

   !> Get the index of the active dambreak for a given dambreak name
   !! a dambreak is active in flowgeom when at least 1 flow link associated
   pure module function get_active_dambreak_index(dambreak_name) result(index)
      character(len=*), intent(in) :: dambreak_name !< Id/name of the requested dambreak
      integer :: index !< Returned index of the found dambreak; -1 when not found.

      integer :: i !< loop index

      index = -1
      do i = 1, n_db_signals
         if (trim(dambreaks(i)%name) == trim(dambreak_name) .and. dambreaks(i)%number_of_links > 0) then
            index = i
            exit
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
   module subroutine update_counters_for_dambreaks(id, number_of_links, dambridx, index_structure, kedb, kegen)
      use m_update_counters_for_structures, only: update_counters_for_dambreak_or_pump
      character(len=*), intent(in) :: id !< the id of the structure.
      integer, intent(in) :: number_of_links !< the number of flow links.
      integer, dimension(:), allocatable, intent(inout) :: dambridx !< the index of the structure.
      integer, intent(in) :: index_structure !< the index of the structure.
      integer, dimension(:), allocatable, intent(inout) :: kedb !< edge oriented dambreak
      integer, dimension(:), allocatable, intent(in) :: kegen !< placeholder for the link snapping of all structure types.

      call update_counters_for_dambreak_or_pump(id, number_of_links, n_db_signals, first_link, last_link, dambridx, &
                                                index_structure)
      kedb(first_link(n_db_signals):last_link(n_db_signals)) = kegen(1:number_of_links)

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

   subroutine set_dambreak_data(index_in_structure, name, dambreak_settings, dambreak)
      use m_dambreak, only: t_dambreak_settings
      use m_dambreak, only: BREACH_GROWTH_VDKNAAP, BREACH_GROWTH_VERHEIJVDKNAAP, BREACH_GROWTH_TIMESERIES
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: index_in_structure !< the index of the structure in the structure list.
      character(len=*), intent(in) :: name !< the name of the dambreak
      type(t_dambreak_settings), intent(in) :: dambreak_settings !< the dambreak settings
      type(t_dambreak), intent(inout) :: dambreak !< the dambreak data

      dambreak%name = name
      dambreak%index_structure = index_in_structure
      dambreak%width = 0.0_dp
      dambreak%maximum_width = 0.0_dp
      dambreak%maximum_allowed_width = dambreak_settings%maximum_allowed_width
      dambreak%breach_width_ini = dambreak_settings%breach_width_ini
      dambreak%crest_level = dambreak_settings%crest_level_ini
      dambreak%crest_level_ini = dambreak_settings%crest_level_ini
      dambreak%crest_level_min = dambreak_settings%crest_level_min
      dambreak%algorithm = dambreak_settings%algorithm
      dambreak%t0 = dambreak_settings%t0
      dambreak%time_to_breach_to_maximum_depth = dambreak_settings%time_to_breach_to_maximum_depth
      dambreak%a_coeff = dambreak_settings%a_coeff
      dambreak%b_coeff = dambreak_settings%b_coeff
      dambreak%u_crit = dambreak_settings%u_crit
      dambreak%f1 = dambreak_settings%f1
      dambreak%f2 = dambreak_settings%f2

      select case (dambreak%algorithm)
      case (BREACH_GROWTH_VDKNAAP)
         dambreak%calculate_breach_growth => calculate_breach_growth_using_vdKnaap_model
      case (BREACH_GROWTH_VERHEIJVDKNAAP)
         dambreak%calculate_breach_growth => calculate_breach_growth_using_Verheij_vdKnaap_model
      case (BREACH_GROWTH_TIMESERIES)
         dambreak%calculate_breach_growth => calculate_breach_growth_using_tables
      case default
         write (msgbuf, '(a,a)') 'Unknown dambreak algorithm ', dambreak%algorithm
         call SetMessage(LEVEL_ERROR, msgbuf)
      end select

   end subroutine set_dambreak_data

   subroutine add_location_for_mapping_and_averaging(x_location, y_location, node_id, n, up_down)
      use m_inquire_flowgeom, only: findnode
      use dfm_error, only: DFM_NOERR
      use messagehandling, only: msgbuf, err_flush
      use m_missing, only: dmiss
      use gridoperations, only: incells

      real(kind=dp), intent(in) :: x_location !< x coordinate of the location
      real(kind=dp), intent(in) :: y_location !< y coordinate of the location
      character(len=*), intent(in) :: node_id !< node id
      integer, intent(in) :: n !< index of the dambreak
      integer, intent(in) :: up_down !< UPSTREAM or DOWNSTREAM

      integer :: node, error

      if (node_id /= '') then
         error = findnode(node_id, node)
         if (error /= DFM_NOERR .or. node <= 0) then
            write (msgbuf, '(a,a,a,a,a)') 'Cannot find the node for water_level_upstream_node_id = ''', &
               trim(node_id), ''' in dambreak ''', trim(dambreaks(n)%name), '''.'
            call err_flush()
         else
            call add_location_for_mapping(n, node, up_down)
         end if
      else if (x_location /= dmiss .and. y_location /= dmiss) then
         call incells(x_location, y_location, node)
         if (node > 0) then
            call add_location_for_mapping(n, node, up_down)
         end if
      else
         call add_location_for_averaging(n, up_down)
      end if
   end subroutine add_location_for_mapping_and_averaging

   !> add upstream/downstream dambreak location for mapping
   subroutine add_location_for_mapping(n, node, up_down)
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: n !< number of current dambreak signal
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
         mappings(number_of_mappings)%water_level => dambreaks(n)%upstream_level
      else
         mappings(number_of_mappings)%water_level => dambreaks(n)%downstream_level
      end if

   end subroutine add_location_for_mapping

   !> add upstream/downstream dambreak signal for averagings
   subroutine add_location_for_averaging(n, up_down)
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage

      integer, intent(in) :: n !< number of current dambreak signal
      integer, intent(in) :: up_down !< 1 - upstream, 2 - downstream

      number_of_averagings = number_of_averagings + 1
      if (number_of_averagings > size(averagings)) then
         write (msgbuf, *) 'number of averagings ', number_of_averagings, &
            ' is larger than allocated array size ', size(averagings)
         call SetMessage(LEVEL_ERROR, msgbuf)
      end if
      averagings(number_of_averagings)%dambreak => dambreaks(n)
      if (up_down == UPSTREAM) then
         averagings(number_of_averagings)%link_ids => dambreaks(n)%upstream_link_ids
         averagings(number_of_averagings)%water_level => dambreaks(n)%upstream_level
      else
         averagings(number_of_averagings)%link_ids => dambreaks(n)%downstream_link_ids
         averagings(number_of_averagings)%water_level => dambreaks(n)%downstream_level
      end if

   end subroutine add_location_for_averaging

   !> Set the link numbers for the dambreak
   subroutine set_links(dambreak, links)
      use m_flowgeom, only: ln

      type(t_dambreak), intent(inout) :: dambreak !< the dambreak data
      integer, dimension(:), intent(in) :: links !< the link number to set

      integer :: link ! the dambreak link index
      integer :: flow_link

      do link = 1, dambreak%number_of_links
         dambreak%link_indices(link) = links(link)
         flow_link = abs(dambreak%link_indices(link))
         if (dambreak%link_indices(link) > 0) then
            dambreak%upstream_link_ids(link) = ln(1, flow_link)
            dambreak%downstream_link_ids(link) = ln(2, flow_link)
         else
            dambreak%upstream_link_ids(link) = ln(2, flow_link)
            dambreak%downstream_link_ids(link) = ln(1, flow_link)
         end if
      end do

   end subroutine set_links

   !> Check or read the dambreak structure
   subroutine read_dambreak_structure_if_it_was_not_done(index_structure, dambreak, index_in_structure, &
                                                         index_in_polygons, success)
      use m_1d_structures, only: addstructure, getstructype_from_string
      use m_hash_search, only: hashsearch
      use m_readstructures, only: readdambreak
      use m_structures, only: tree_data, strs_ptr, idlen
      use messagehandling, only: msgbuf, msg_flush, err_flush
      use properties, only: prop_get
      use unstruc_channel_flow, only: network

      integer, intent(in) :: index_structure !< the index of the structure in the structure list.
      type(t_dambreak), intent(inout) :: dambreak !< the dambreak data
      integer, intent(out) :: index_in_structure !< the index of the structure in the structure list.
      integer, intent(out) :: index_in_polygons !< the index of the structure in the dambreakPolygons list.
      logical, intent(out) :: success !< success flag

      integer :: structure_type, upstream_link, downstream_link, flow_link

      type(tree_data), pointer :: structure_ptr
      character(len=IdLen) :: structure_id ! TODO: where to put IdLen (now in MessageHandling)
      character(len=IdLen) :: structure_type_name ! TODO: where to put IdLen (now in MessageHandling)
      integer, parameter :: INDEX_COMPOUND = -1
      character(len=*), parameter :: COMPOUND_NAME = ""

      structure_ptr => strs_ptr%child_nodes(index_structure)%node_ptr
      structure_id = ' '
      call prop_get(structure_ptr, '', 'id', structure_id, success)

      index_in_structure = hashsearch(network%sts%hashlist_structure, structure_id) ! Assumes unique names across all structure types.
      if (index_in_structure /= -1) then
         index_in_polygons = index_in_structure ! dambreakPolygons were already read in network%sts loop.
         success = .true.
      else
         ! Postponed read, because this is with old-style .pli ifile
         index_in_polygons = index_structure ! dambreakPolygons were already read in old style .pli count+selectelset loop above.

         structure_type_name = ' '
         call prop_get(structure_ptr, '', 'type', structure_type_name, success)
         structure_type = getStructype_from_string(structure_type_name)
         ! flow1d_io library: add and read SOBEK dambreak
         if (dambreak%number_of_links > 0) then
            ! structure is active in current grid on one or more flow links: just use the first link of the the structure
            upstream_link = dambreak%upstream_link_ids(1)
            downstream_link = dambreak%downstream_link_ids(1)
            flow_link = abs(dambreak%link_indices(1))
         else
            ! Structure is not active in current grid: use dummy calc points and flow links, not used in computations.
            upstream_link = 0
            downstream_link = 0
            flow_link = 0
         end if
         index_in_structure = addStructure(network%sts, upstream_link, downstream_link, flow_link, &
                                           INDEX_COMPOUND, COMPOUND_NAME, structure_id, structure_type)
         call readDambreak(network%sts%struct(index_in_structure)%dambreak, structure_ptr, structure_id, &
                           network%forcinglist, success)

         if (.not. success) then
            write (msgbuf, '(a,a,a)') 'Dambreak "', trim(structure_id), &
               '" could not be read. Perhaps missing fields in structure file?'
            call err_flush()
         else
            write (msgbuf, '(a,a,a)') 'Dambreak "', trim(structure_id), '" set to new format.'
            call msg_flush()
         end if
      end if

   end subroutine read_dambreak_structure_if_it_was_not_done

   !> Set EC module time series for the dambreak
   subroutine set_time_series_if_needed(filename, dambreak)
      use m_dambreak, only: BREACH_GROWTH_TIMESERIES
      use m_meteo, only: ec_addtimespacerelation
      use messagehandling, only: msgbuf, err_flush
      use timespace_parameters, only: UNIFORM, SPACEANDTIME

      character(len=*), intent(in) :: filename !< the name of the time series file
      type(t_dambreak), intent(inout) :: dambreak !< the dambreak data

      integer, parameter :: KX = 2
      integer, parameter, dimension(1) :: KDUM = 1
      real(kind=dp), parameter, dimension(1) :: XDUM = 1.0_dp, YDUM = 1.0_dp
      character(len=*), parameter :: QID = 'dambreakLevelsAndWidths'

      logical :: success

      if (dambreak%algorithm /= BREACH_GROWTH_TIMESERIES) then
         return
      end if

      if (index(trim(filename)//'|', '.tim|') > 0) then
         success = ec_addtimespacerelation(QID, XDUM, YDUM, KDUM, KX, filename, UNIFORM, &
                                           SPACEANDTIME, 'O', targetIndex=1, tgt_item1=dambreak%ec_item)
         if (.not. success) then
            write (msgbuf, '(5a)') 'Cannot process a tim file for "', QID, '" for the dambreak "', trim(dambreak%name), '".'
            call err_flush()
         end if
      end if

   end subroutine set_time_series_if_needed

   !> Calculate the breach start link and effective/maximum widths of the dambreak
   subroutine calculate_start_link_and_widths(dambreak, start_location_x, start_location_y, number_of_points, &
                                              x_points, y_points, lftopol)
      use geometry_module, only: dbdistance, normalout, comp_breach_point
      use m_cell_geometry, only: xz, yz
      use m_flowgeom, only: ln, kcu, wu, lncn, snu, csu
      use m_missing, only: dmiss, dxymis
      use m_sferic, only: jsferic, jasfer3D
      use network_data, only: xk, yk

      type(t_dambreak), intent(inout) :: dambreak !< the dambreak data
      real(kind=dp), intent(in) :: start_location_x !< x coordinate of the breach start location
      real(kind=dp), intent(in) :: start_location_y !< y coordinate of the breach start location
      integer, intent(in) :: number_of_points !< number of points in the structure
      real(kind=dp), pointer, intent(in) :: x_points(:) !< x points of the structure
      real(kind=dp), pointer, intent(in) :: y_points(:) !< y points of the structure
      integer, dimension(:), intent(in) :: lftopol !< the mapping array from flow link to intersecting polyline segment.

      integer :: k, k1, k2, point, link
      integer :: starting_link
      real(kind=dp) :: x_breach, y_breach ! dummy variables for the breach point
      real(kind=dp) :: xn, yn ! normal vector components
      real(kind=dp), allocatable, dimension(:, :) :: polyline_x, polyline_y

      if (.not. associated(x_points) .or. .not. associated(y_points)) then
         return
      end if

      ! Create the array with the coordinates of the flow links
      allocate (polyline_x(dambreak%number_of_links, 2), polyline_y(dambreak%number_of_links, 2))
      do k = 1, dambreak%number_of_links
         ! compute the mid point
         link = abs(dambreak%link_indices(k))
         k1 = ln(1, link)
         k2 = ln(2, link)
         polyline_x(k, 1) = xz(k1)
         polyline_x(k, 2) = xz(k2)
         polyline_y(k, 1) = yz(k1)
         polyline_y(k, 2) = yz(k2)
      end do

      ! comp_breach_point takes plain arrays to compute the breach point (also used in unstruct_bmi)
      call comp_breach_point(start_location_x, start_location_y, x_points, y_points, number_of_points, &
                             polyline_x, polyline_y, starting_link, x_breach, y_breach, jsferic, jasfer3D, dmiss)

      dambreak%breach_start_link = starting_link

      ! compute the normal projections of the start and endpoints of the flow links
      do k = 1, dambreak%number_of_links
         link = abs(dambreak%link_indices(k))
         if (kcu(link) == 3) then ! 1d2d flow link
            dambreak%link_effective_width(k) = wu(link)
         else
            point = lftopol(k + dambreak%link_map_offset)
            call normalout(x_points(point), y_points(point), x_points(point + 1), y_points(point + 1), &
                           xn, yn, jsferic, jasfer3D, dmiss, dxymis)
            k1 = lncn(1, link)
            k2 = lncn(2, link)
            dambreak%link_effective_width(k) = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss) * &
                                               abs(xn * csu(link) + yn * snu(link))
         end if

         ! Sum the length of the intersected flow links (required to bound maximum breach width)
         dambreak%maximum_width = dambreak%maximum_width + dambreak%link_effective_width(k)
      end do

   end subroutine calculate_start_link_and_widths
end submodule m_dambreak_breach_submodule
