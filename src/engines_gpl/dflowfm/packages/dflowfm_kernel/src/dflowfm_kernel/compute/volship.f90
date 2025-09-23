!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_volship

   implicit none

   private

   public :: volship

contains

   subroutine volship() ! compute ship volume relative to fixed level 0d0
      use precision, only: dp
      use m_ship, only: v1ship, zspc
      use m_flowgeom, only: lnx, ln, lncn, wu, dx, acl
      use m_flow, only: slotw2d
      use m_get_link_area_wid2D, only: getlinkareawid2d
      implicit none

      integer :: L, k1, k2, k3, k4
      real(kind=dp) :: BL1, BL2, b21, wu2, ai, wid1, hpr1, dx1, dx2, ar1, slotsav
      slotsav = slotw2D; slotw2D = 0.0_dp
      v1ship = 0.0_dp
      do L = 1, lnx
         k1 = ln(1, L); k2 = ln(2, L)
         k3 = lncn(1, L); k4 = lncn(2, L)
         if (zspc(k3) /= 0.0_dp .or. zspc(k4) /= 0.0_dp) then
            if (zspc(k3) < zspc(k4)) then
               BL1 = zspc(k3); BL2 = zspc(k4)
            else
               BL1 = zspc(k4); BL2 = zspc(k3)
            end if
            wu2 = wu(L); b21 = BL2 - BL1; ai = b21 / wu2

            hpr1 = 0.0_dp - BL1
            if (hpr1 > 0.0_dp) then
               call getlinkareawid2D(wu2, b21, ai, hpr1, ar1, wid1)
               dx1 = 0.5_dp * dx(L) * acl(L)
               dx2 = 0.5_dp * dx(L) * (1.0_dp - acl(L))

               v1ship(k1) = v1ship(k1) + dx1 * ar1
               v1ship(k2) = v1ship(k2) + dx2 * ar1
            end if

         end if
      end do
      slotw2D = slotsav
   end subroutine volship

end module m_volship
