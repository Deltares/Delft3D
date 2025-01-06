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

!> Updates derived bathymetry arrays, after zk values have been changed.
!! This routine updates the entire grid, so call this once after a (series of) zk update(s).
module m_land_change_callback

implicit none

private

public :: land_change_callback

contains

subroutine land_change_callback()
   use m_flowgeom
   use m_flow
   use m_set_kbot_ktop
   use m_volsur
   use m_flow_f0isf1
   use m_set_bobs

   hs = s1 - bl
   call setbobs()
   s1 = bl + hs; s0 = s1; s00 = s1

   call volsur() ! dropland_zk
   call flow_f0isf1() ! dropland_zk
   volerr = 0; volerrcum = 0

   if (kmx > 0) then
      call setkbotktop(1) ! dropland_zk
   end if
end subroutine land_change_callback

end module m_land_change_callback
