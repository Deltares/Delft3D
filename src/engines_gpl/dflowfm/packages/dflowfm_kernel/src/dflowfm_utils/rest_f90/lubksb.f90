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

      subroutine LUBKSB(A, N, NP, INDX, B)
         implicit none
         double precision :: a
         double precision :: b
         integer :: i
         integer :: ii
         integer :: indx
         integer :: j
         integer :: ll
         integer :: n
         integer :: np
         double precision :: sum
         dimension A(NP, NP), INDX(N), B(N)
         II = 0
         do I = 1, N
            LL = INDX(I)
            SUM = B(LL)
            B(LL) = B(I)
            if (II /= 0) then
               do J = II, I - 1
                  SUM = SUM - A(I, J) * B(J)
               end do
            else if (SUM /= 0d0) then
               II = I
            end if
            B(I) = SUM
         end do
         do I = N, 1, -1
            SUM = B(I)
            if (I < N) then
               do J = I + 1, N
                  SUM = SUM - A(I, J) * B(J)
               end do
            end if
            B(I) = SUM / A(I, I)
         end do
         return
      end
