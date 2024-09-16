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

      subroutine GOLD(AX, BX, CX, TOL, XMIN, P, P2, Q, Q2, XX, YY, N, DIS)
         use m_spldist
         implicit none
         double precision :: c
         double precision :: f0
         double precision :: f1
         double precision :: f2
         double precision :: f3
         integer :: n
         double precision :: r
         double precision :: x0
         double precision :: x3
         parameter(R=.61803399, C=.38196602)

         double precision :: AX, BX, CX, TOL, DIS, XMIN, XX, YY, X1, X2

!     EENDIMENSIONAAL ZOEKEN VAN 'GEBRACKED' MINIMUM
         double precision :: P(N), P2(N), Q(N), Q2(N)
         X0 = AX
         X3 = CX
         if (abs(CX - BX) > abs(BX - AX)) then
            X1 = BX
            X2 = BX + C * (CX - BX)
         else
            X2 = BX
            X1 = BX - C * (BX - AX)
         end if
!     F1=F(X1)
         F1 = SPLDIST(P, P2, Q, Q2, XX, YY, X1, N)
!     F2=F(X2)
         F2 = SPLDIST(P, P2, Q, Q2, XX, YY, X2, N)
1        if (abs(X3 - X0) > TOL * (abs(X1) + abs(X2))) then
!     IF(ABS(X3-X0).GT.TOL) THEN
            if (F2 < F1) then
               X0 = X1
               X1 = X2
               X2 = R * X1 + C * X3
               F0 = F1
               F1 = F2
               F2 = SPLDIST(P, P2, Q, Q2, XX, YY, X2, N)
!         F2=F(X2)
            else
               X3 = X2
               X2 = X1
               X1 = R * X2 + C * X0
               F3 = F2
               F2 = F1
!         F1=F(X1)
               F1 = SPLDIST(P, P2, Q, Q2, XX, YY, X1, N)
            end if
            goto 1
         end if
         if (F1 < F2) then
            DIS = F1
            XMIN = X1
         else
            DIS = F2
            XMIN = X2
         end if
         return
      end subroutine GOLD
