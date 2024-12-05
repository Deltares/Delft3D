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

      subroutine LUDCMP(A, N, NP, INDX, D, JAPARALLEL)
         use precision, only: dp
         implicit none
         real(kind=dp) :: a
         real(kind=dp) :: aamax
         real(kind=dp) :: d
         real(kind=dp) :: dum
         integer :: i
         integer :: imax
         integer :: indx
         integer :: j
         integer :: japarallel
         integer :: k
         integer :: n
         integer :: np
         integer :: nx
         real(kind=dp) :: sum
         real(kind=dp) :: tiny
         real(kind=dp) :: vv
         parameter(NX=4, TINY=1d-20)
         dimension A(NP, NP), INDX(N), VV(NX)
         JAPARALLEL = 0
         D = 1.
         do I = 1, N
            AAMAX = 0.
            do J = 1, N
               if (abs(A(I, J)) > AAMAX) AAMAX = abs(A(I, J))
            end do
            if (AAMAX == 0) then
               JAPARALLEL = 1
               return
            end if
            VV(I) = 1./AAMAX
         end do
         do J = 1, N
            if (J > 1) then
               do I = 1, J - 1
                  SUM = A(I, J)
                  if (I > 1) then
                     do K = 1, I - 1
                        SUM = SUM - A(I, K) * A(K, J)
                     end do
                     A(I, J) = SUM
                  end if
               end do
            end if
            AAMAX = 0.
            do I = J, N
               SUM = A(I, J)
               if (J > 1) then
                  do K = 1, J - 1
                     SUM = SUM - A(I, K) * A(K, J)
                  end do
                  A(I, J) = SUM
               end if
               DUM = VV(I) * abs(SUM)
               if (DUM >= AAMAX) then
                  IMAX = I
                  AAMAX = DUM
               end if
            end do
            if (J /= IMAX) then
               do K = 1, N
                  DUM = A(IMAX, K)
                  A(IMAX, K) = A(J, K)
                  A(J, K) = DUM
               end do
               D = -D
               VV(IMAX) = VV(J)
            end if
            INDX(J) = IMAX
            if (J /= N) then
               if (A(J, J) == 0d0) A(J, J) = TINY
               DUM = 1./A(J, J)
               do I = J + 1, N
                  A(I, J) = A(I, J) * DUM
               end do
            end if
         end do
         if (A(N, N) == 0d0) A(N, N) = TINY
         return
      end
