!----AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_water_level_boundary

   use m_wind, only: air_pressure_available, air_pressure, pseudo_air_pressure_available, pseudo_air_pressure, &
                     water_level_correction_available, water_level_correction, pavbnd
   use m_fm_icecover, only: ice_apply_pressure, ice_pressure
   use m_physcoef, only: ag, rhomean
   use precision, only: dp
   implicit none

   public :: correct_water_level_boundary

contains

   !> Correct water level boundary due to effect of air pressure, water level correction and/or ice
   pure subroutine correct_water_level_boundary(water_level_boundary, i_boundary)
      real(kind=dp), intent(inout) :: water_level_boundary !< [m] water level boundary
      integer, intent(in) :: i_boundary !< [-] boundary cell index

      if (PavBnd > 0 .and. (air_pressure_available .or. pseudo_air_pressure_available &
                            .or. water_level_correction_available)) then
         water_level_boundary = water_level_boundary + PavBnd / (ag * rhomean)
         if (air_pressure_available) then
            water_level_boundary = water_level_boundary - air_pressure(i_boundary) / (ag * rhomean)
         end if
         if (pseudo_air_pressure_available) then
            water_level_boundary = water_level_boundary - pseudo_air_pressure(i_boundary) / (ag * rhomean)
         end if
         if (water_level_correction_available) then
            water_level_boundary = water_level_boundary - water_level_correction(i_boundary)
         end if
      end if

      if (ice_apply_pressure) then
         water_level_boundary = water_level_boundary - ice_pressure(i_boundary) / (ag * rhomean)
      end if

   end subroutine correct_water_level_boundary

end module m_water_level_boundary
