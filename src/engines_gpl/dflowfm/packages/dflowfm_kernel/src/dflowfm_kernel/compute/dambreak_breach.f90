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

!
!

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

   ! time varying, values can be get via BMI interface
   real(kind=dp), dimension(:), allocatable, target, public :: db_breach_depth !< the dambreak breach depth (as a level)
   real(kind=dp), dimension(:), allocatable, target, public :: db_breach_width !< the dambreak breach width (as a level)
   real(kind=dp), dimension(:), allocatable, target, public :: db_upstream_level !< the water levels computed each time step upstream
   real(kind=dp), dimension(:), allocatable, target, public :: db_downstream_level !< the water levels computed each time step downstream

   integer :: n_db_upstream_locations !< nr of dambreak signals with locations upstream
   integer, dimension(:), allocatable :: db_upstream_location_mapping !< mapping of dambreak locations upstream
   integer, dimension(:), allocatable :: db_upstream_location !< store cell ids for water level locations upstream
   integer :: n_db_upstream_averaging !< nr of dambreak signals upstream with averaging
   integer, dimension(:), allocatable :: db_upstream_averaging_mapping !< mapping of dambreak averaging upstream
   integer :: n_db_downstream_locations !< nr of dambreak signals with locations downstream
   integer, dimension(:), allocatable :: db_downstream_location_mapping !< mapping of dambreak locations downstream
   integer, dimension(:), allocatable :: db_downstream_location !< store cell ids for water level locations downstream
   integer :: n_db_downstream_averaging !< nr of dambreak signals downstream with averaging
   integer, dimension(:), allocatable :: db_downstream_averaging_mapping !< mapping of dambreak averaging in the dambreak arrays

   real(kind=dp), dimension(:, :), allocatable :: db_weight_averaged_values !< (1,:) weight averaged values of waterlevel per dambreaklink
                                                                           !! (2,:) weight per dambreaklink

contains

   !> allocate arrays and initialize variables
   subroutine allocate_and_initialize_dambreak_data(n_db_signals)
      use m_alloc, only: realloc

      integer, intent(in) :: n_db_signals !< number of dambreak signals

      call realloc(db_upstream_level, n_db_signals)
      call realloc(db_downstream_level, n_db_signals)
      call realloc(db_weight_averaged_values, [2, n_db_signals])
      call realloc(db_upstream_location_mapping, n_db_signals, fill=0)
      call realloc(db_upstream_location, n_db_signals, fill=0)
      call realloc(db_upstream_averaging_mapping, n_db_signals, fill=0)
      call realloc(db_downstream_location_mapping, n_db_signals, fill=0)
      call realloc(db_downstream_location, n_db_signals, fill=0)
      call realloc(db_downstream_averaging_mapping, n_db_signals, fill=0)
      n_db_upstream_locations = 0
      n_db_upstream_averaging = 0
      n_db_downstream_locations = 0
      n_db_downstream_averaging = 0

   end subroutine allocate_and_initialize_dambreak_data

   !> TODO UNST-8587:: add API documentation
   subroutine update_dambreak_breach(startTime, deltaTime)
      use precision, only: dp
      use m_flowgeom, only: wu
      use m_flow, only: s1, hu, au, u1
      use m_missing, only: dmiss
      use unstruc_channel_flow, only: network
      use m_dambreak, only: prepare_dambreak_calculation, BREACH_GROWTH_VDKNAAP, BREACH_GROWTH_VERHEIJVDKNAAP, BREACH_GROWTH_TIMESERIES
      use m_partitioninfo, only: getAverageQuantityFromLinks
      use m_meteo, only: ec_gettimespacevalue_by_itemID, ecInstancePtr, item_db_levels_widths_table
      use fm_external_forcings_data, only: success, n_db_links, n_db_signals, &
                                           dambreaks, db_levels_widths_table, &
                                           breach_start_link, db_first_link, db_last_link, db_link_ids, db_active_links
      use m_flowtimes, only: irefdate, tunit, tzone

      real(kind=dp), intent(in) :: startTime
      real(kind=dp), intent(in) :: deltaTime

      real(kind=dp) :: s_max, s_min, h_max, h_min
      integer :: n, ierr, istru

      if (n_db_signals > 0) then ! Variable n_db_signals is >0 for all partitions if there is a dambreak, even if it is outside
         ! of a partition. In a parallel simulation, we need to run this subroutine even in a special situation that there is
         ! no dambreak on the current subdomain (i.e. n_db_links == 0), because the following function getAverageQuantityFromLinks
         ! involves mpi communication among all subdomains. However, in this special situation,
         ! all the necessary variables are set to 0 and do not participate the dambreak related computation in this subroutine.

         !
         ! Initialize
         !
         db_weight_averaged_values(:, :) = 0.0d0
         db_upstream_level(:) = 0.0d0
         db_downstream_level(:) = 0.0d0
         do n = 1, n_db_signals
            istru = dambreaks(n)
            if (istru /= 0) then
               network%sts%struct(istru)%dambreak%normal_velocity = 0.0
               network%sts%struct(istru)%dambreak%breach_width_derivative = 0.0
               network%sts%struct(istru)%dambreak%water_level_jump = 0.0
            end if
         end do
         !
         ! Upstream water level
         !
         if (n_db_upstream_locations > 0) then
            db_upstream_level(db_upstream_location_mapping(1:n_db_upstream_locations)) = &
               s1(db_upstream_location(1:n_db_upstream_locations))
         end if

         !call this code only if something has to be averaged
         if (n_db_upstream_averaging > 0) then

            ! Compute sumQuantitiesByWeight upstream
            ierr = getAverageQuantityFromLinks(db_first_link(db_upstream_averaging_mapping(1:n_db_upstream_averaging)), &
                                               db_last_link(db_upstream_averaging_mapping(1:n_db_upstream_averaging)), wu, db_link_ids(3, :), s1, &
                                               db_link_ids(1, :), db_weight_averaged_values, 0, hu, dmiss, db_active_links, 0)

            if (ierr /= 0) then
               success = .false.
               return
            end if

            if (n_db_links > 0) then
               do n = 1, n_db_upstream_averaging
                  if (db_weight_averaged_values(2, n) > 0.0d0) then
                     db_upstream_level(db_upstream_averaging_mapping(n)) = &
                        db_weight_averaged_values(1, n) / db_weight_averaged_values(2, n)
                  else if (abs(startTime - network%sts%struct(dambreaks(db_upstream_averaging_mapping(n)))%dambreak%T0) < 1d-10) then
                     db_upstream_level(db_upstream_averaging_mapping(n)) = &
                        s1(db_link_ids(1, breach_start_link(db_upstream_averaging_mapping(n))))
                  else
                     continue
                  end if
               end do
            end if
         end if

         !
         ! Downstream water level
         !
         if (n_db_downstream_locations > 0) then
            db_downstream_level(db_downstream_location_mapping(1:n_db_downstream_locations)) = &
               s1(db_downstream_location(1:n_db_downstream_locations))
         end if

         !call this code only if something has to be averaged downstream
         if (n_db_downstream_averaging > 0) then

            ! Compute sumQuantitiesByWeight downstream
            ierr = getAverageQuantityFromLinks(db_first_link(db_downstream_averaging_mapping(1:n_db_downstream_averaging)), &
                                               db_last_link(db_downstream_averaging_mapping(1:n_db_downstream_averaging)), wu, db_link_ids(3, :), s1, &
                                               db_link_ids(2, :), db_weight_averaged_values, 0, hu, dmiss, db_active_links, 0)

            if (ierr /= 0) then
               success = .false.
               return
            end if

            if (n_db_links > 0) then
               do n = 1, n_db_downstream_averaging
                  if (db_weight_averaged_values(2, n) > 0.0d0) then
                     db_downstream_level(db_downstream_averaging_mapping(n)) = &
                        db_weight_averaged_values(1, n) / db_weight_averaged_values(2, n)
                  else if (abs(startTime - network%sts%struct(dambreaks(db_downstream_averaging_mapping(n)))%dambreak%T0) < 1d-10) then
                     db_downstream_level(db_downstream_averaging_mapping(n)) = &
                        s1(db_link_ids(2, breach_start_link(db_downstream_averaging_mapping(n))))
                  else
                     continue
                  end if
               end do
            end if
         end if

         !
         ! u0 velocity on the flowlinks (averaged by the wetted area). The mask is the water level itself
         !
         ierr = getAverageQuantityFromLinks(db_first_link, db_last_link, au, db_link_ids(3, :), u1, db_link_ids(3, :), &
                                            db_weight_averaged_values, 1, hu, dmiss, db_active_links, 0)
         if (ierr /= 0) success = .false.

         if (n_db_links > 0) then
            do n = 1, n_db_signals
               istru = dambreaks(n)
               if (istru /= 0 .and. db_weight_averaged_values(2, n) > 0.0d0) then
                  network%sts%struct(istru)%dambreak%normal_velocity = &
                     db_weight_averaged_values(1, n) / db_weight_averaged_values(2, n)
               end if
            end do

            !Compute dambreak widths
            do n = 1, n_db_signals
               istru = dambreaks(n)
               if (istru == 0) continue
               associate (dambreak => network%sts%struct(istru)%dambreak)
                  if (dambreak%algorithm == BREACH_GROWTH_VDKNAAP .or. &
                      dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then
                     ! Compute the breach width
                     call prepare_dambreak_calculation(network%sts%struct(istru)%dambreak, db_upstream_level(n), &
                                                 db_downstream_level(n), startTime, deltaTime)
                  end if
                  if (dambreak%algorithm == BREACH_GROWTH_TIMESERIES .and. &
                      startTime > dambreak%t0) then
                     !Time in the tim file is relative to the start time
                     success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_db_levels_widths_table, &
                                                              irefdate, tzone, tunit, startTime - dambreak%t0)
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
                  ! Store breach width derivative if not already computed in prepare_dambreak_calculation
                  if (dambreak%algorithm /= BREACH_GROWTH_VERHEIJVDKNAAP) then
                     dambreak%breach_width_derivative = &
                        (dambreak%width - db_breach_width(n)) / deltaTime
                  end if

                  ! Store the current dambreak width
                  db_breach_width(n) = dambreak%width
                  ! Store the current dambreak crest level
                  db_breach_depth(n) = dambreak%crest_level

                  ! Possibly overwrite value computed in prepare_dambreak_calculation with value from timeseries
                  if (dambreak%algorithm == BREACH_GROWTH_TIMESERIES) then
                     s_max = max(db_upstream_level(n), db_downstream_level(n))
                     s_min = min(db_upstream_level(n), db_downstream_level(n))
                     h_max = max(0d0, s_max - dambreak%crest_level)
                     h_min = max(0d0, s_min - dambreak%crest_level)
                     dambreak%water_level_jump = h_max - h_min
                  end if
               end associate
            end do
         end if
      end if
   end subroutine update_dambreak_breach

   !> update the crest/bed levels for dambreak breach
   subroutine adjust_bobs_on_dambreak_breach(width, max_width, crest_level, starting_link, left_link, right_link, structure_id)
      use precision, only: dp

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
      if (Lf > 0 .and. width > 0d0) then
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
         db_link_actual_width(starting_link) = max(width, 0d0)
         left_breach_width = 0d0
         right_breach_width = 0d0
      else
         ! breach width larger than width of starting link
         db_link_actual_width(starting_link) = db_link_effective_width(starting_link)
         left_side = sum(db_link_effective_width(left_link:starting_link - 1))
         right_side = sum(db_link_effective_width(starting_link + 1:right_link))
         remainder = width - db_link_effective_width(starting_link)
         if (dambreak_widening == DBW_SYMM) then
            ! original implementation which triggers a breach too wide error be
            h_remainder = 0.5d0 * remainder
            left_breach_width = h_remainder
            right_breach_width = h_remainder
         elseif (dambreak_widening == DBW_PROP) then
            ! proportional
            left_frac = left_side / (left_side + right_side)
            left_breach_width = left_frac * remainder
            right_breach_width = (1.0d0 - left_frac) * remainder
         elseif (dambreak_widening == DBW_SYMM_ASYMM) then
            ! first symmetric, then asymmetric
            h_remainder = 0.5d0 * remainder
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
         end if
      end if

      ! process dam "left" of initial breach segment
      do k = starting_link - 1, left_link, -1
         Lf = abs(db_link_ids(3, k))
         if (left_breach_width > 0d0) then
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
            left_breach_width = 0d0
         end if
      end do

      ! process dam "right" of initial breach segment
      do k = starting_link + 1, right_link
         Lf = abs(db_link_ids(3, k))
         if (right_breach_width > 0d0) then
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
            right_breach_width = 0d0
         end if
      end do

      ! check for any unprocessed breach width
      if (left_breach_width > 1.0d-6 * max_width .or. right_breach_width > 1.0d-6 * max_width) then
         write (msgbuf, '(3a)') 'The breach  of dam ''', trim(structure_id), ''' exceeds the actual dam width on at least one side of the breach point.'
         call SetMessage(LEVEL_WARN, msgbuf)
      end if

   end subroutine adjust_bobs_on_dambreak_breach

   !< store upstream dambreak information
   subroutine add_dambreaklocation_upstream(n_signal, node)

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak

      n_db_upstream_locations = n_db_upstream_locations + 1
      db_upstream_location_mapping(n_db_upstream_locations) = n_signal
      db_upstream_location(n_db_upstream_locations) = node

   end subroutine add_dambreaklocation_upstream

   !> store downstream dambreak information
   subroutine add_dambreaklocation_downstream(n_signal, node)

      integer, intent(in) :: n_signal !< number of current dambreak signal
      integer, intent(in) :: node !< node number for current dambreak

      n_db_downstream_locations = n_db_downstream_locations + 1
      db_downstream_location_mapping(n_db_downstream_locations) = n_signal
      db_downstream_location(n_db_downstream_locations) = node

   end subroutine add_dambreaklocation_downstream

   !> add upstream signal for averaging
   subroutine add_averaging_upstream_signal(n_signal)

      integer, intent(in) :: n_signal !< number of current dambreak signal

      n_db_upstream_averaging = n_db_upstream_averaging + 1
      db_upstream_averaging_mapping(n_db_upstream_averaging) = n_signal

   end subroutine add_averaging_upstream_signal

   !> add downstream signal for averaging
   subroutine add_averaging_downstream_signal(n_signal)

      integer, intent(in) :: n_signal !< number of current dambreak signal

      n_db_downstream_averaging = n_db_downstream_averaging + 1
      db_downstream_averaging_mapping(n_db_downstream_averaging) = n_signal

   end subroutine add_averaging_downstream_signal

end module m_dambreak_breach
