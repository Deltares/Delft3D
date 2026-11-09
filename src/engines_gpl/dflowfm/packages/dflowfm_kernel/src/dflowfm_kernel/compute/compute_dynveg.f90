!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
module m_update_dynveg
!
!
   implicit none

   private

   public :: update_dynveg

contains

   subroutine update_dynveg()
      use precision, only: dp
      use m_physcoef, only: dynamic_vegetation_roughness_enabled, dynamic_vegetation_stem_height, dynamic_vegetation_root_depth, manning_friction_no_vegetation
      use m_sediment, only: cumes
      use m_flow, only: dynveg, frcu, frcu0

      if (dynamic_vegetation_roughness_enabled > 0) then
         if (.not. allocated(cumes)) then
            ! when sedimentation is not allocated, it's zero and frcu stays equal to frcu0
            return
         end if
         where ((dynveg) .and. (cumes > 0.0_dp)) ! linear function due to deposition ( cumes > 0 )
            frcu = manning_friction_no_vegetation + max((dynamic_vegetation_stem_height - cumes) / dynamic_vegetation_stem_height, 0.0_dp) * (frcu0 - manning_friction_no_vegetation)
         elsewhere((dynveg) .and. (cumes < -dynamic_vegetation_root_depth)) ! erosion larger than root then always minimum ( cumes < -dynamic_vegetation_root_depth )
            frcu = manning_friction_no_vegetation
            dynveg = .false.
         elsewhere(dynveg) ! linear function due to deposition ( -dynamic_vegetation_root_depth < cumes < 0 )
            frcu = manning_friction_no_vegetation + min(max((dynamic_vegetation_root_depth + cumes) / dynamic_vegetation_root_depth, 0.0_dp), 1.0_dp) * (frcu0 - manning_friction_no_vegetation)
         elsewhere ! don't change frcu if dynveg is false
            ! frcu = frcu
         end where
      end if

   end subroutine update_dynveg

end module m_update_dynveg
