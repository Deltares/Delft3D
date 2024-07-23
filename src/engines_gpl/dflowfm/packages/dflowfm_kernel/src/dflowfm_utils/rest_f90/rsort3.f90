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

      subroutine RSORT3(X, Y, Z, N)
         implicit none
         integer :: j
         integer :: j1
         integer :: k0
         integer :: kk
         integer :: lk
         integer :: n
         integer :: nm
         double precision :: temp

         double precision :: X(N), Y(N), Z(N)
         if (N == 0) return
         LK = N / 2
         K0 = LK
         KK = K0

20       J = 2 * KK
         J1 = J + 1

30       if (J1 <= N) then

            if (X(J) < X(J1)) J = J1

         end if

         if (X(KK) < X(J)) then

            TEMP = X(J)
            X(J) = X(KK)
            X(KK) = TEMP

            TEMP = Y(J)
            Y(J) = Y(KK)
            Y(KK) = TEMP

            TEMP = Z(J)
            Z(J) = Z(KK)
            Z(KK) = TEMP

            if (J <= LK) then
               KK = J
               goto 20
            end if

         end if

         K0 = K0 - 1

         if (K0 /= 0) then
            KK = K0
            J = 2 * KK
            J1 = J + 1
            goto 30
         end if

         NM = N

65       TEMP = X(1)
         X(1) = X(NM)
         X(NM) = TEMP

         TEMP = Y(1)
         Y(1) = Y(NM)
         Y(NM) = TEMP

         TEMP = Z(1)
         Z(1) = Z(NM)
         Z(NM) = TEMP

         NM = NM - 1
         if (NM == 1) return

         KK = 1

70       J = 2 * KK
         J1 = J + 1

         if (J > NM) goto 65

         if (J1 <= NM .and. X(J) < X(J1)) J = J1

         if (X(KK) >= X(J)) goto 65

         TEMP = X(J)
         X(J) = X(KK)
         X(KK) = TEMP

         TEMP = Y(J)
         Y(J) = Y(KK)
         Y(KK) = TEMP

         TEMP = Z(J)
         Z(J) = Z(KK)
         Z(KK) = TEMP

         KK = J

         goto 70

      end
