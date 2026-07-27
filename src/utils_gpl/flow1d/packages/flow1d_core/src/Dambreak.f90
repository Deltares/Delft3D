!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

   integer, parameter, public :: BREACH_GROWTH_VDKNAAP = 1
   integer, parameter, public :: BREACH_GROWTH_VERHEIJVDKNAAP = 2
   integer, parameter, public :: BREACH_GROWTH_TIMESERIES = 3

   type, public :: t_dambreak_settings
      integer :: algorithm
      integer :: material_type = 1 !for algorithm BREACH_GROWTH_VDKNAAP, default material type is clay
      real(kind=dp) :: start_location_x
      real(kind=dp) :: start_location_y
      real(kind=dp) :: crest_level_ini
      real(kind=dp) :: breach_width_ini
      real(kind=dp) :: crest_level_min
      real(kind=dp) :: time_to_breach_to_maximum_depth
      real(kind=dp) :: f1
      real(kind=dp) :: f2
      real(kind=dp) :: u_crit
      real(kind=dp) :: t0
      real(kind=dp) :: water_level_upstream_location_x = -999d0
      real(kind=dp) :: water_level_upstream_location_y = -999d0
      real(kind=dp) :: water_level_downstream_location_x = -999d0
      real(kind=dp) :: water_level_downstream_location_y = -999d0
      real(kind=dp) :: maximum_allowed_width = -1.0d0 ! only relevant for breach growth algorithm BREACH_GROWTH_VDKNAAP
      real(kind=dp) :: a_coeff
      real(kind=dp) :: b_coeff
      character(IdLen) :: water_level_upstream_node_id = ''
      character(IdLen) :: water_level_downstream_node_id = ''
      character(IdLen) :: levels_and_widths = ''
   end type

contains

   subroutine set_dambreak_coefficients(dambreak)

      type(t_dambreak_settings), pointer, intent(inout) :: dambreak

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
      end if

   end subroutine set_dambreak_coefficients

end module m_dambreak

