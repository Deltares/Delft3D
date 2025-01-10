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

module m_poltoland
   use m_toland, only: toland

   implicit none

   private

   public :: poltoland

contains

   subroutine POLTOLAND(L1, L2) ! SHIFT POLYGON TO LANDBOUNDARY
      use precision, only: dp
      use M_POLYGON
      use M_MISSING
      use M_LANDBOUNDARY

      integer :: l1
      integer :: l2

      integer :: in
      integer :: l, j
      real(kind=dp) :: xp, yp, xpn, ypn, dis, rL

      IN = 1; if (L2 < L1) IN = -1
      do L = L1, L2, IN
         XP = XPL(L)
         if (XP /= XYMIS) then
            YP = YPL(L)
            call TOLAND(XP, YP, 1, MXLAN, 1, xpn, ypn, dis, j, rL)
            XPL(L) = xpn; YPL(L) = ypn
         end if
      end do

   end subroutine POLTOLAND

end module m_poltoland
