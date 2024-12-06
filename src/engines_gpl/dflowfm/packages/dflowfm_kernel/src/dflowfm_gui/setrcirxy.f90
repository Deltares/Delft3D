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

module m_setrcirxy

   implicit none

contains

   subroutine setrcirxy(x, y, rcx, rcy) ! determine x and y search tols on the spot where you click
      use precision, only: dp
      use m_wearelt, only: rcir, x1, x2, y1, y2
      use m_sferic
      use m_devices, only: npx, npy
      use m_dproject
      use m_dbdistance_hk
      implicit none
      real(kind=dp) :: x, y, rcx, rcy, xx, yy, xa, ya, rpx, rpy
      rcx = rcir; rcy = rcir
      if (jsfertek >= 1) then
         call dPROJECT(x, y, xa, ya, 2)

         rpy = 28 * (y2 - y1) / npy
         call dPROJECT(xa, ya + rpy, xx, yy, 1) ! you still have to project in
         call dbdistancehk(x, y, xx, yy, rcy)
         rcy = rcy * rd2dg / ra

         rpx = 28 * (x2 - x1) / npx
         call dPROJECT(xa + rpx, ya, xx, yy, 1)
         call dbdistancehk(x, y, xx, yy, rcx)
         rcx = rcx * rd2dg / ra

         rcx = sqrt(rcx * rcx + rcy * rcy)
         rcy = rcx

      end if
   end

end module m_setrcirxy
