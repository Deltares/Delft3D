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

module m_widarhyr

implicit none

private

public :: widarhyr

contains

subroutine widarhyr(hpr, dz, wu2, wid, ar, hyr)
   use m_flow, only: slotw2D
   implicit none
   double precision :: hpr, dz, wu2, wid, ar, hyr
   double precision :: per, hp2
   if (dz / wu2 < 1d-3) then
      wid = wu2; wid = wid + slotw2D
      ar = wid * hpr
      hyr = hpr
   else if (hpr < dz) then
      wid = wu2 * hpr / dz; wid = wid + slotw2D
      ar = 0.5d0 * wid * hpr
      per = sqrt(hpr * hpr + wid * wid)
      hyr = ar / per
   else
      wid = wu2; wid = wid + slotw2D ! wid = max(wid, slotw2d)
      hp2 = hpr - dz
      ar = wid * 0.5d0 * (hpr + hp2)
      per = sqrt(dz * dz + wid * wid)
      hyr = ar / per
   end if
end subroutine widarhyr

end module m_widarhyr
