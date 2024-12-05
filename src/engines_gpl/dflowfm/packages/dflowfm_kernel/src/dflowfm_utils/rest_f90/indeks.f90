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

!
      subroutine INDEKS(N, ARRIN, INDX)
         use precision, only: dp
         implicit none
         real(kind=dp) :: arrin
         integer :: i
         integer :: indx
         integer :: indxt
         integer :: ir
         integer :: j
         integer :: l
         integer :: n
         real(kind=dp) :: q
         dimension ARRIN(N), INDX(N)
         do J = 1, N
            INDX(J) = J
         end do
         L = N / 2 + 1
         IR = N
10       continue
         if (L > 1) then
            L = L - 1
            INDXT = INDX(L)
            Q = ARRIN(INDXT)
         else
            INDXT = INDX(IR)
            Q = ARRIN(INDXT)
            INDX(IR) = INDX(1)
            IR = IR - 1
            if (IR == 1) then
               INDX(1) = INDXT
               return
            end if
         end if
         I = L
         J = L + L
20       if (J <= IR) then
            if (J < IR) then
               if (ARRIN(INDX(J)) < ARRIN(INDX(J + 1))) J = J + 1
            end if
            if (Q < ARRIN(INDX(J))) then
               INDX(I) = INDX(J)
               I = J
               J = J + J
            else
               J = IR + 1
            end if
            GO TO 20
         end if
         INDX(I) = INDXT
         GO TO 10
      end
