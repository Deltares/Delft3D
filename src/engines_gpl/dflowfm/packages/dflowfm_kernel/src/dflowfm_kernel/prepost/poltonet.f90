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

      subroutine POLTONET(L1, L2) ! PULL POLYGON TO NETWORK, KEEPING SUITABLE TRIANGLES TO OUTSIDE

         use m_closenetbndlink, only: closenetbndlink
         use m_netw
         use m_polygon
         use m_missing
         use m_wearelt
         use m_sferic, only: dtol_pole
         use gridoperations
         use m_qnerror
         use m_makenetnodescoding

         implicit none
         integer :: l1
         integer :: l2

         double precision :: d1, d2, xp1, xp2, yp1, yp2
         integer :: i
         integer :: ja
         integer :: k
         integer :: k1
         integer :: k2
         integer :: kk
         integer :: kl1
         integer :: kl2
         integer :: kn1
         integer :: knaar
         integer :: l
         integer :: ll
         integer :: n
         integer :: n1
         integer :: n2

         double precision :: XR, YR, XN, YN, XR1, YR1, XR2, YR2, AR1, DIS
         call SAVEPOL()

         if (L1 > L2) then
            LL = L1; L1 = L2; L2 = LL
         end if

         XP1 = XPL(L1); YP1 = YPL(L1)
         XP2 = XPL(L2); YP2 = YPL(L2)

         NPL = 4 ! CHANGE POLYGON TO VISIBLE AREA
         XPL(1) = X1; YPL(1) = Y1
         XPL(2) = X2; YPL(2) = Y1
         XPL(3) = X2; YPL(3) = Y2
         XPL(4) = X1; YPL(4) = Y2

         call FINDCELLS(0)

         call MAKENETNODESCODING()

         call CLOSENETBNDLINK(XP1, YP1, N1)
         call CLOSENETBNDLINK(XP2, YP2, N2)

         if (N1 == 0 .or. N2 == 0) then
            call QNERROR('NO CLOSE NET POINT FOUND', ' ', ' ')
         end if

         do N = 1, L1 - 1
            XPL(N) = XPH(N); YPL(N) = YPH(N)
         end do

         L = N2
         K1 = KN(1, L); K2 = KN(2, L)
         XP1 = 0.5d0 * (XK(K1) + YK(K2))
         YP1 = 0.5d0 * (YK(K1) + YK(K2))

!      CALL TEKNODE(K1,221)
!      CALL TEKNODE(K1,31)

         L = N1
         K1 = KN(1, L); K2 = KN(2, L)
         D1 = sqrt((XK(K1) - XP1)**2 + (YK(K1) - YP1)**2)
         D2 = sqrt((XK(K2) - XP1)**2 + (YK(K2) - YP1)**2)

         if (D1 > D2) then
            K = K1; KNAAR = K2
         else
            K = K2; KNAAR = K1
         end if

         N = L1 - 1

         do while (N < MAXPOL)
            JA = 0
            do KK = 1, NMK(K)
               LL = NOD(K)%LIN(KK)
               call OTHERNODE(K, LL, K2)
               if (L == N1 .and. K2 == KNAAR .or. LC(LL) /= -1 .and. LNN(LL) == 1) then
                  JA = 1
                  L = LL
                  call OTHERNODE(K, L, K2)
                  K = K2; LC(L) = -1

                  KL1 = KN(1, L); KL2 = KN(2, L)
                  XR1 = XK(KL1); XR2 = XK(KL2)
                  YR1 = YK(KL1); YR2 = YK(KL2)

                  KN1 = LNE(1, L)
                  call GETCELLSURFACE(KN1, AR1, XR, YR)
                  call MIRRORLINE2(XR, YR, XR1, YR1, XR2, YR2, JA, DIS, XN, YN)

                  N = N + 1
                  XPL(N) = XN
                  YPL(N) = YN

                  ! CALL RCIRC( XPL(N), YPL(N) )
                  ! CALL WAIT ()
                  exit
               end if
            end do
            if (L == N2) exit
         end do

         do I = L2 + 1, NPH
            N = N + 1
            XPL(N) = XPH(I)
            YPL(N) = YPH(I)
         end do

         NPL = N

      end subroutine POLTONET
