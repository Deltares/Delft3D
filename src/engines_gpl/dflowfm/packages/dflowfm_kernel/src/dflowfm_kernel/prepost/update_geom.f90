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

!  update geometry data that may have been incorrectly computed in the ghost area
module m_update_geom

implicit none

private

public :: update_geom

contains

   subroutine update_geom(iphase)
      use m_partitioninfo
      use m_flowgeom
      use unstruc_channel_flow
      use m_crosssections
      use m_cross_helper

      integer, intent(in) :: iphase ! phase, 0 (all), 1 (first) or 2 (second)
      integer :: ierror

      if (iphase == 0 .or. iphase == 1) then
         call update_ghosts(ITYPE_SALL, 1, Ndx, xz, ierror)
         call update_ghostboundvals(ITYPE_SALL, 1, Ndx, xz, 1, ierror) ! safety: check
         call update_ghosts(ITYPE_SALL, 1, Ndx, yz, ierror)
         call update_ghostboundvals(ITYPE_SALL, 1, Ndx, yz, 0, ierror)
      end if

      if (iphase == 0 .or. iphase == 2) then
         call update_ghosts(ITYPE_SALL, 1, Ndx, bl, ierror)
         call update_ghostboundvals(ITYPE_SALL, 1, Ndx, bl, 0, ierror)
      end if

      return
   end subroutine update_geom

end module m_update_geom
