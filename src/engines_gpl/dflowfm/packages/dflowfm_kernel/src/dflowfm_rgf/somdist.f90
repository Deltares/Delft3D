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

      subroutine SOMDIST(A, B, C, D, M1, N1, M2, N2)
         use precision, only: dp
         use m_grid
         use m_missing
         use m_isitu
         implicit none
         integer :: i
         integer :: i2
         integer :: ii
         integer :: j
         integer :: j2
         integer :: jj
         integer :: k
         integer :: l
         integer :: m1
         integer :: m2
         integer :: n1
         integer :: n2
         real(kind=dp) :: A(MMAX, NMAX), B(MMAX, NMAX), &
                          C(MMAX, NMAX), D(MMAX, NMAX)
!
         do I = M1 + 1, M2
            do J = N1 + 1, N2
               if (IJC(I, J) == 11) then
                  II = -1
               else if (IJC(I, J) == 12) then
                  II = 1
               else if (IJC(I, J) == 13) then
                  II = 1
               else if (IJC(I, J) == 14) then
                  II = -1
               end if
               if (IJC(I, J) >= 11 .and. IJC(I, J) <= 14) then
                  K = I
20                continue
                  K = K + II
                  I2 = K
                  if (IJC(K, J) == 10) goto 20
                  do K = I, I2, II
                     IJC(K, J) = 21
                  end do
               end if
            end do
         end do

         call INULARR(IJYES, MMAX, NMAX)
         do I = M1, M2
            do J = N1 + 1, N2
               if (IJC(I, J) /= 0 .and. IJC(I + 1, J) /= 0 .and. &
                   IJC(I, J + 1) /= 0 .and. IJC(I + 1, J + 1) /= 0) then
                  if (B(I, J) /= dmiss .and. B(I, J - 1) /= dmiss .and. &
                      IJC(I, J) /= 21) then
                     B(I, J) = B(I, J) + B(I, J - 1)
                     D(I, J) = D(I, J) + D(I, J - 1)
                     IJYES(I, J) = IJYES(I, J - 1) + 1
                  end if
               end if
            end do
         end do

         do I = M1, M2
            do J = N2 - 1, N1, -1
               if (IJC(I, J) /= 0 .and. IJC(I + 1, J) /= 0 .and. &
                   IJC(I, J + 1) /= 0 .and. IJC(I + 1, J + 1) /= 0) then
                  if (B(I, J) /= dmiss .and. B(I, J + 1) /= dmiss .and. &
                      IJC(I, J + 1) /= 21) then
                     B(I, J) = B(I, J + 1)
                     D(I, J) = D(I, J + 1)
                     IJYES(I, J) = IJYES(I, J + 1)
                  end if
               end if
            end do
         end do

         do I = M1, M2
            do J = N1, N2
               if (IJC(I, J) /= 0 .and. IJC(I + 1, J) /= 0 .and. &
                   IJC(I, J + 1) /= 0 .and. IJC(I + 1, J + 1) /= 0) then
                  B(I, J) = B(I, J) / (IJYES(I, J) + 1)
                  D(I, J) = D(I, J) / (IJYES(I, J) + 1)
               end if
            end do
         end do

         call ISITU()
         call INULARR(IJYES, MMAX, NMAX)

         do I = M1 + 1, M2
            do J = N1 + 1, N2
               if (IJC(I, J) == 11) then
                  JJ = -1
               else if (IJC(I, J) == 12) then
                  JJ = -1
               else if (IJC(I, J) == 13) then
                  JJ = 1
               else if (IJC(I, J) == 14) then
                  JJ = 1
               end if
               if (IJC(I, J) >= 11 .and. IJC(I, J) <= 14) then
                  L = J
120               continue
                  L = L + JJ
                  J2 = L
                  if (IJC(I, L) == 10) goto 120
                  do L = J, J2, JJ
                     IJC(I, L) = 22
                  end do
               end if
            end do
         end do

         do J = N1, N2
            do I = M1 + 1, M2
               if (IJC(I, J) /= 0 .and. IJC(I + 1, J) /= 0 .and. &
                   IJC(I, J + 1) /= 0 .and. IJC(I + 1, J + 1) /= 0) then
                  if (A(I, J) /= dmiss .and. A(I - 1, J) /= dmiss .and. &
                      IJC(I, J) /= 22) then
                     A(I, J) = A(I, J) + A(I - 1, J)
                     C(I, J) = C(I, J) + C(I - 1, J)
                     IJYES(I, J) = IJYES(I - 1, J) + 1
                  end if
               end if
            end do
         end do

         do J = N1, N2
            do I = M2 - 1, M1, -1
               if (IJC(I, J) /= 0 .and. IJC(I + 1, J) /= 0 .and. &
                   IJC(I, J + 1) /= 0 .and. IJC(I + 1, J + 1) /= 0) then
                  if (A(I, J) /= dmiss .and. A(I + 1, J) /= dmiss .and. &
                      IJC(I + 1, J) /= 22) then
                     A(I, J) = A(I + 1, J)
                     C(I, J) = C(I + 1, J)
                     IJYES(I, J) = IJYES(I + 1, J)
                  end if
               end if
            end do
         end do

         do J = N1, N2
            do I = M1, M2
               if (IJC(I, J) /= 0 .and. IJC(I + 1, J) /= 0 .and. &
                   IJC(I, J + 1) /= 0 .and. IJC(I + 1, J + 1) /= 0) then
                  A(I, J) = A(I, J) / (IJYES(I, J) + 1)
                  C(I, J) = C(I, J) / (IJYES(I, J) + 1)
               end if
            end do
         end do

!     Herstellen
         call ISITU()

         return
      end subroutine SOMDIST
