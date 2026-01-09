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

module m_reconstruct_hydrodynamics

   use m_get_Lbot_Ltop_max, only: getLbotLtopmax
   use precision, only: dp

   implicit none

   public :: reconstruct_hu_2D_from_3D

contains

   !> Set hu at the 2D indices to the corresponding hu at the top active-layer
   subroutine reconstruct_hu_2D_from_3D(hu, lnx)
      real(kind=dp), dimension(:), intent(inout) :: hu !< [m] Upwind water-height at u-point, i.e. the distance from the top of a layer to the bed
      integer, intent(in) :: lnx !< [-] Number of flow links (internal + boundary)

      integer :: link_index_2d, link_index_3d, link_bottom, link_top_max

      do link_index_2d = 1, lnx
         call getLbotLtopmax(link_index_2d, link_bottom, link_top_max)
         do link_index_3d = link_top_max, link_bottom, -1
            if (hu(link_index_3d) > 0.0_dp) then
               hu(link_index_2d) = hu(link_index_3d)
               exit
            end if
         end do
      end do
   end subroutine reconstruct_hu_2D_from_3D

end module m_reconstruct_hydrodynamics
