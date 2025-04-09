!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
module m_dambreak

   use m_GlobalParameters, only: Idlen, gravity
   use precision, only: dp

   implicit none

   private

   public prepare_dambreak_calculation
   public set_dambreak_coefficents
   public set_dambreak_widening_method

   integer, parameter, public :: BREACH_GROWTH_VDKNAAP = 1
   integer, parameter, public :: BREACH_GROWTH_VERHEIJVDKNAAP = 2
   integer, parameter, public :: BREACH_GROWTH_TIMESERIES = 3

   integer, parameter, public :: DBW_SYMM = 1 !< symmetrical dambreak widening (limited width in case of asymmetric starting link placement)
   integer, parameter, public :: DBW_PROP = 2 !< dambreak wideining proportional to left/right dam length
   integer, parameter, public :: DBW_SYMM_ASYMM = 3 !< symmetrical dambreak widening until left/right runs out of space then continues one sided
   integer, public :: dambreak_widening = DBW_SYMM_ASYMM !< method for dambreak widening

   type, public :: t_dambreak
      real(kind=dp) :: start_location_x
      real(kind=dp) :: start_location_y
      integer :: algorithm
      real(kind=dp) :: crest_level_ini
      real(kind=dp) :: breach_width_ini
      real(kind=dp) :: crest_level_min
      real(kind=dp) :: time_to_breach_to_maximum_depth
      real(kind=dp) :: discharge_coeff
      real(kind=dp) :: f1
      real(kind=dp) :: f2
      real(kind=dp) :: u_crit
      real(kind=dp) :: t0
      integer :: material_type = 1 !for algorithm BREACH_GROWTH_VDKNAAP, default material type is clay
      real(kind=dp) :: end_time_first_phase
      real(kind=dp) :: breach_width_derivative
      real(kind=dp) :: water_level_jump
      real(kind=dp) :: normal_velocity
      real(kind=dp) :: water_level_upstream_location_x = -999d0
      real(kind=dp) :: water_level_upstream_location_y = -999d0
      real(kind=dp) :: water_level_downstream_location_x = -999d0
      real(kind=dp) :: water_level_downstream_location_y = -999d0
      character(IdLen) :: water_level_upstream_node_id = ''
      character(IdLen) :: water_level_downstream_node_id = ''
      character(IdLen) :: levels_and_widths = ''

      ! State variables
      integer :: phase
      real(kind=dp) :: width
      real(kind=dp) :: maximum_width ! the maximum dambreak width (from pli file)
      real(kind=dp) :: maximum_allowed_width = -1.0d0 ! only relevant for breach growth algorithm BREACH_GROWTH_VDKNAAP
      real(kind=dp) :: crest_level
      real(kind=dp) :: a_coeff
      real(kind=dp) :: b_coeff

   end type

contains
   !> This routine sets dambreak%crest_level and dambreak%width, these varuables are needed
   !! in the actual dambreak computation in dflowfm_kernel
   subroutine prepare_dambreak_calculation(dambreak, upstream_water_level, downstream_water_level, time1, dt)
      use ieee_arithmetic, only: ieee_is_nan

      type(t_dambreak), pointer, intent(inout) :: dambreak !< dambreak settings for a single dambreak
      real(kind=dp), intent(in) :: upstream_water_level !< waterlevel at upstream link from dambreak position
      real(kind=dp), intent(in) :: downstream_water_level !< waterlevel at downstream link from dambreak position
      real(kind=dp), intent(in) :: time1 !< current time
      real(kind=dp), intent(in) :: dt !< timestep

      real(kind=dp), parameter :: HOURS_TO_SECONDS = 3600.0d0
      real(kind=dp) :: s_max
      real(kind=dp) :: s_min
      real(kind=dp) :: h_max
      real(kind=dp) :: h_min
      real(kind=dp) :: delta_level
      real(kind=dp) :: breach_width
      real(kind=dp) :: actual_maximum_width
      real(kind=dp) :: time_from_breaching
      real(kind=dp) :: time_from_first_phase
      real(kind=dp) :: width_increment
      real(kind=dp) :: water_level_jump_dambreak
      real(kind=dp) :: breach_width_derivative

      time_from_breaching = time1 - dambreak%t0

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

         if (time1 <= dambreak%end_time_first_phase) then
            ! phase 1: lowering
            dambreak%crest_level = dambreak%crest_level_ini - &
                time_from_breaching / dambreak%time_to_breach_to_maximum_depth * (dambreak%crest_level_ini - dambreak%crest_level_min)
            dambreak%width = dambreak%breach_width_ini
            dambreak%phase = 1
         else
            ! phase 2: widening
            dambreak%crest_level = dambreak%crest_level_min
            s_max = max(upstream_water_level, downstream_water_level)
            s_min = min(upstream_water_level, downstream_water_level)
            h_max = max(0d0, s_max - dambreak%crest_level)
            h_min = max(0d0, s_min - dambreak%crest_level)
            water_level_jump_dambreak = h_max - h_min
            delta_level = (gravity * water_level_jump_dambreak)**1.5d0
            time_from_first_phase = time1 - dambreak%end_time_first_phase

            if (dambreak%width < dambreak%maximum_width .and. (.not. ieee_is_nan(dambreak%normal_velocity)) &
                .and. dabs(dambreak%normal_velocity) > dambreak%u_crit) then
               breach_width_derivative = (dambreak%f1 * dambreak%f2 / log(10D0)) * &
                                         (delta_level / (dambreak%u_crit * dambreak%u_crit)) * &
                                         (1.0 / (1.0 + (dambreak%f2 * gravity * time_from_first_phase / (dambreak%u_crit * HOURS_TO_SECONDS))))
               width_increment = breach_width_derivative * (dt / HOURS_TO_SECONDS)
               !ensure monotonically increasing dambreak%width
               if (width_increment > 0) then
                  dambreak%width = dambreak%width + width_increment
               end if
            end if
         end if
         dambreak%breach_width_derivative = breach_width_derivative
         dambreak%water_level_jump = water_level_jump_dambreak
      end if

      ! in vdKnaap(2000) the maximum allowed branch width is limited (see sobek manual and set_dambreak_coefficents subroutine below)
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

   subroutine set_dambreak_coefficents(dambreak)

      type(t_dambreak), pointer, intent(inout) :: dambreak

      if (dambreak%algorithm == BREACH_GROWTH_VDKNAAP) then
         ! clay
         if (dambreak%material_type == 1) then
            dambreak%a_coeff = 20
            dambreak%b_coeff = 288
            dambreak%maximum_allowed_width = 75 !meters
            ! sand
         else if (dambreak%material_type == 2) then
            dambreak%a_coeff = 67
            dambreak%b_coeff = 522
            dambreak%maximum_allowed_width = 200 !meters
         end if
      else if (dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then
         dambreak%end_time_first_phase = dambreak%t0 + dambreak%time_to_breach_to_maximum_depth
      end if

   end subroutine set_dambreak_coefficents

   !< set variable dambreak_widening, returns string with the method name
   subroutine set_dambreak_widening_method(method_string)
      use messagehandling, only: mess, LEVEL_ERROR

      character(len=*), intent(inout) :: method_string !< method for dambreak widening

      select case (method_string)
      case ('symmetric')
         dambreak_widening = DBW_SYMM
      case ('proportional')
         dambreak_widening = DBW_PROP
      case ('symmetric-asymmetric')
         dambreak_widening = DBW_SYMM_ASYMM
      case default
         ! default settings if no method is specified
         dambreak_widening = DBW_SYMM_ASYMM
         method_string = 'symmetric-asymmetric'
      end select

   end subroutine set_dambreak_widening_method

end module m_dambreak

