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

      ! SPvdP: TIELDB never called
      subroutine TIELDB()
         use precision, only: dp
         use m_addelem, only: addelem
         use m_netw
         use m_missing
         use geometry_module, only: dpinpok, cross
         use m_sferic, only: jsferic
         use m_three_two

         implicit none
         real(kind=dp) :: crp
         integer :: in1, in2, jacros, k, k1, k2, k3, ku, L, Lnu
         real(kind=dp) :: sl, sm, xcr, ycr, z, zcr, x1, x2, y1, y2
         do L = 1, NUML
            K1 = KN(1, L)
            K2 = KN(2, L)
            if (K1 /= 0 .and. K2 /= 0) then
               call DPINPOK(XK(K1), YK(K1), ZK(K1), NPL, XPL, YPL, IN1, jins, dmiss)
               call DPINPOK(XK(K2), YK(K2), ZK(K2), NPL, XPL, YPL, IN2, jins, dmiss)
               if (IN1 == 1 .and. IN2 == 1) then
                  call DRIETWEE(XK(K1), YK(K1), ZK(K1), x1, y1, Z)
                  call DRIETWEE(XK(K2), YK(K2), ZK(K2), x2, y2, Z)
                  K = 0
10                K = K + 1
                  KU = K + 1; if (KU == MXLAN + 1) KU = 1
                  if (XLAN(K) /= XYMIS .and. XLAN(K + 1) /= XYMIS) then
                     call CROSS(x1, y1, x2, y2, XLAN(K), YLAN(K), XLAN(K + 1), YLAN(K + 1), &
                                JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
                     if (JACROS == 1) then
                        LNU = L
                        NUMK = NUMK + 1
                        K3 = NUMK
                        ZCR = SL * ZK(K2) + (1 - SL) * ZK(K1)
                        call SETPOINT(XCR, YCR, ZCR, K3)
                        call ADDELEM(K1, K3)
                        call ADDELEM(K2, K3)
                     end if
                  end if
                  if (K < MXLAN) goto 10
               end if
            end if
         end do
         return
      end subroutine TIELDB
