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

      subroutine dLINEDIS3(X3, Y3, X1, Y1, X2, Y2, JA, DIS, XN, YN, RLOUT) ! 3: SORRY
         use geometry_module, only: getdx, getdy, dbdistance, sphertocart3D, Cart3Dtospher
         use m_missing, only: dmiss
         use m_sferic, only: jsferic, jasfer3D

         implicit none
         integer :: ja
         double precision :: X1, Y1, X2, Y2, X3, Y3, DIS, XN, YN
         double precision :: xx1, xx2, xx3, yy1, yy2, yy3, zz1, zz2, zz3, xxn, yyn, zzn
         double precision :: R2, RL, X21, Y21, Z21, X31, Y31, Z31

         double precision :: RLout ! needed in orthogonalisenet/projection of boundary nodes
         ! korste afstand tot lijnelement tussen eindpunten
         JA = 0

         if (jsferic == 0 .or. jasfer3D == 0) then
            X21 = getdx(x1, y1, x2, y2, jsferic)
            Y21 = getdy(x1, y1, x2, y2, jsferic)
            X31 = getdx(x1, y1, x3, y3, jsferic)
            Y31 = getdy(x1, y1, x3, y3, jsferic)
            R2 = dbdistance(x2, y2, x1, y1, jsferic, jasfer3D, dmiss)
            R2 = R2 * R2
            RLout = 0d0
            if (R2 /= 0) then
               RL = (X31 * X21 + Y31 * Y21) / R2
               RLout = RL
               RL = max(min(1d0, RL), 0d0)
               JA = 1
               XN = X1 + RL * (x2 - x1)
               YN = Y1 + RL * (y2 - y1)
               DIS = dbdistance(x3, y3, xn, yn, jsferic, jasfer3D, dmiss)
            end if
         else

            call sphertocart3D(x1, y1, xx1, yy1, zz1)
            call sphertocart3D(x2, y2, xx2, yy2, zz2)
            call sphertocart3D(x3, y3, xx3, yy3, zz3)

            x21 = xx2 - xx1
            y21 = yy2 - yy1
            z21 = zz2 - zz1
            x31 = xx3 - xx1
            y31 = yy3 - yy1
            z31 = zz3 - zz1

            r2 = x21 * x21 + y21 * y21 + z21 * z21
            RLout = 0d0
            if (r2 /= 0d0) then
               RL = (X31 * X21 + Y31 * Y21 + Z31 * Z21) / R2
               RLout = RL
               RL = max(min(1d0, RL), 0d0)
               JA = 1
               XXN = xx1 + RL * x21
               YYN = yy1 + RL * y21
               ZZN = zz1 + RL * z21
               x31 = xxn - xx3
               y31 = yyn - yy3
               z31 = zzn - zz3
               DIS = sqrt(x31 * x31 + y31 * y31 + z31 * z31)

               call Cart3Dtospher(xxn, yyn, zzn, xn, yn, maxval((/x1, x2, x3/)))
            end if

         end if
         return
      end subroutine DLINEDIS3
