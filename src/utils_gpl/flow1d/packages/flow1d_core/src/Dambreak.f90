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

   use m_GlobalParameters, only: Idlen
   use precision, only: dp

   implicit none

   private

   public set_dambreak_coefficients
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

   subroutine set_dambreak_coefficients(dambreak)

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

   end subroutine set_dambreak_coefficients

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

