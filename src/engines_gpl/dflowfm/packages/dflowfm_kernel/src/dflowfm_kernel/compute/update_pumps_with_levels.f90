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

module m_update_pumps_with_levels

   implicit none

   private

   public :: update_pumps_with_levels

contains

   subroutine update_pumps_with_levels()

      use m_flowgeom
      use m_flow
      use m_missing
      use m_structures
      use unstruc_channel_flow
      use m_pump
      use m_partitioninfo

      integer :: ierr, n

      !Pump with levels, SOBEK style, outside OpenMP region
      ! TODO: merge water level calculations with dambreak
      if (nPumpsWithLevels > 0) then

         ! Initialize
         pumpAveraging = 0.0d0
         waterLevelsPumpLeft = 0.0d0
         waterLevelsPumpRight = 0.0d0

         ! Compute sumQuantitiesByWeight and sumWeights for the suction side
         !LC: TODO, do the average only over open links
         ierr = getAverageQuantityFromLinks(L1pumpsg, L2pumpsg, wu, kpump(3, :), s1, kpump(1, :), pumpAveraging, 0)
         if (ierr /= 0) success = .false.

         do n = 1, npumpsg
            if (pumpAveraging(2, n) > 0.0d0) then
               waterLevelsPumpLeft(n) = pumpAveraging(1, n) / pumpAveraging(2, n)
            end if
         end do

         ! Compute sumQuantitiesByWeight and sumWeights for the delivery side
         ierr = getAverageQuantityFromLinks(L1pumpsg, L2pumpsg, wu, kpump(3, :), s1, kpump(2, :), pumpAveraging, 0)
         if (ierr /= 0) success = .false.

         do n = 1, npumpsg
            if (pumpAveraging(2, n) > 0.0d0) then
               waterLevelsPumpRight(n) = pumpAveraging(1, n) / pumpAveraging(2, n)
            end if
         end do

      end if

   end subroutine update_pumps_with_levels

end module m_update_pumps_with_levels
