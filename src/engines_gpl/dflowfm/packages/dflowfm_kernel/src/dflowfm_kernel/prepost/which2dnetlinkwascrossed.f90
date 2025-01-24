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

module m_which2dnetlinkwascrossed

   implicit none

   private

   public :: which2dnetlinkwascrossed

contains

   !> Find the 2D netlink (edge of a given 2D net cell) that is intersected
 !! by another given netlink (typically a 1D2D net link).
   subroutine which2Dnetlinkwascrossed(NC1, K1, K2, L) ! find the crossed 2D link
      use precision, only: dp
      use m_flowgeom
      use m_netw
      use geometry_module, only: cross
      use m_missing, only: dmiss
      use m_sferic, only: jsferic

      integer, intent(in) :: NC1 !< Index of 2D netcell in which one end of a 1D2D link lies.
      integer, intent(in) :: K1, K2 !< Start+end index of 1D2D netlink
      integer, intent(out) :: L !< Resulting 2D netlink (edge of 2D grid cell), intersected by input netlink. 0 if not found.

      integer :: nn, kk, jacros, k3, k4, LL
      real(kind=dp) :: SL, SM, XCR, YCR, CRP

      L = 0
      nn = NETCELL(nc1)%N

      do kk = 1, nn
         LL = NETCELL(Nc1)%lin(kk)
         K3 = kn(1, LL)
         K4 = kn(2, LL)

         call CROSS(xk(k1), yk(k1), xk(k2), yk(k2), xk(k3), yk(k3), xk(k4), yk(k4), JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
         if (jacros == 1) then
            L = LL
            return
         end if
      end do

   end subroutine which2Dnetlinkwascrossed ! TEMP STORE CROSSED 2d LINK IN LC

end module m_which2dnetlinkwascrossed
