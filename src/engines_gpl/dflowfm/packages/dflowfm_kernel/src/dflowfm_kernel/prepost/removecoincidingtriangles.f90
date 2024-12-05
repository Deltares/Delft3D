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

   subroutine REMOVECOINCIDINGTRIANGLES()
      use precision, only: dp
      use m_netw ! 2 REMOVES SMALL TRIANGLES NEXT TO
      use M_FLOWGEOM
      use unstruc_messages
      use m_sferic
      use geometry_module, only: getdxdy
      use gridoperations
      use m_mergenodes
      implicit none

      real(kind=dp) :: DX2, DY2, DX3, DY3, DEN
      integer :: K1, K2, K3, KDUM, N, L, LL, JA, IERR

      real(kind=dp), allocatable :: XNW(:), YNW(:)
      integer, allocatable :: NNW(:, :)

      real(kind=dp), external :: getdx, getdy

      call FINDCELLS(3)

      allocate (XNW(NUMP), YNW(NUMP), NNW(3, NUMP), STAT=IERR)
      call AERR('XNW(NUMP),YNW(NUMP),NNW(3,NUMP)', IERR, NUMK * 3)
      NNW = 0
      do N = 1, NUMP ! REMOVE COINCIDING TRIANGLES
         K1 = NETCELL(N)%NOD(1); K2 = NETCELL(N)%NOD(2); K3 = NETCELL(N)%NOD(3)

!     fix for spherical, periodic coordinates
         if (jsferic == 1 .and. abs(abs(yk(k1)) - 90d0) < dtol_pole) then
            kdum = k1
            k1 = k2
            k2 = k3
            k3 = kdum
         end if

         !dx2 = getdx(XK(K1), YK(K1), XK(K2), YK(K2)) ! AvD: TODO: getdx toepassen
         !dy2 = getdy(XK(K1), YK(K1), XK(K2), YK(K2))
         call getdxdy(XK(K1), YK(K1), XK(K2), YK(K2), dx2, dy2, jsferic)
         !dx3 = getdx(XK(K1), YK(K1), XK(K3), YK(K3))
         !dy3 = getdy(XK(K1), YK(K1), XK(K3), YK(K3))
         call getdxdy(XK(K1), YK(K1), XK(K3), YK(K3), dx3, dy3, jsferic)
         den = dy2 * dx3 - dy3 * dx2
         if (DEN == 0d0) then
            do LL = 1, 3
               L = NETCELL(N)%LIN(LL)
               KN(1, L) = 0; KN(2, L) = 0
            end do
            NNW(1, N) = K1; NNW(2, N) = K2; NNW(3, N) = K3
            XNW(N) = (XK(K1) + XK(K2) + XK(K3)) / 3d0
            YNW(N) = (YK(K1) + YK(K2) + YK(K3)) / 3d0
         end if
      end do

      do N = 1, NUMP
         K1 = NNW(1, N); K2 = NNW(2, N); K3 = NNW(3, N)
         if (K1 > 0) then
            XK(K1) = XNW(N)
            YK(K1) = YNW(N)
            call MERGENODES(K2, K1, JA)
            call MERGENODES(K3, K1, JA)
         end if
      end do

      deallocate (XNW, YNW, NNW)
   end subroutine REMOVECOINCIDINGTRIANGLES
