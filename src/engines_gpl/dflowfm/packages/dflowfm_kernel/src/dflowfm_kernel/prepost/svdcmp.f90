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

!>  Singular value Decomposition
!!    from: Numerical Recipes in Fortran 77
module m_svdcmp

implicit none

private

public :: svdcmp

contains

      subroutine SVDCMP(A, M, N, MP, NP, W, V)
         use precision, only: dp

         real(kind=dp) :: A, W, V
         integer, intent(in) :: m, n, mp, np

         real(kind=dp) :: ANORM, C, F, G, H, RV1, S, SCALE, X, Y, Z
         integer :: I, ITS, J, K, L, NM, NMAX

         parameter(NMAX=100)
         dimension A(MP, NP), W(NP), V(NP, NP), RV1(NMAX)
         G = 0.0
         SCALE = 0.0
         ANORM = 0.0
         do I = 1, N
            L = I + 1
            RV1(I) = SCALE * G
            G = 0.0
            S = 0.0
            SCALE = 0.0
            if (I <= M) then
               do K = I, M
                  SCALE = SCALE + abs(A(K, I))
               end do
               if (SCALE /= 0.0) then
                  do K = I, M
                     A(K, I) = A(K, I) / SCALE
                     S = S + A(K, I) * A(K, I)
                  end do
                  F = A(I, I)
                  G = -sign(sqrt(S), F)
                  H = F * G - S
                  A(I, I) = F - G
                  if (I /= N) then
                     do J = L, N
                        S = 0.0
                        do K = I, M
                           S = S + A(K, I) * A(K, J)
                        end do
                        F = S / H
                        do K = I, M
                           A(K, J) = A(K, J) + F * A(K, I)
                        end do
                     end do
                  end if
                  do K = I, M
                     A(K, I) = SCALE * A(K, I)
                  end do
               end if
            end if
            W(I) = SCALE * G
            G = 0.0
            S = 0.0
            SCALE = 0.0
            if ((I <= M) .and. (I /= N)) then
               do K = L, N
                  SCALE = SCALE + abs(A(I, K))
               end do
               if (SCALE /= 0.0) then
                  do K = L, N
                     A(I, K) = A(I, K) / SCALE
                     S = S + A(I, K) * A(I, K)
                  end do
                  F = A(I, L)
                  G = -sign(sqrt(S), F)
                  H = F * G - S
                  A(I, L) = F - G
                  do K = L, N
                     RV1(K) = A(I, K) / H
                  end do
                  if (I /= M) then
                     do J = L, M
                        S = 0.0
                        do K = L, N
                           S = S + A(J, K) * A(I, K)
                        end do
                        do K = L, N
                           A(J, K) = A(J, K) + S * RV1(K)
                        end do
                     end do
                  end if
                  do K = L, N
                     A(I, K) = SCALE * A(I, K)
                  end do
               end if
            end if
            ANORM = max(ANORM, (abs(W(I)) + abs(RV1(I))))
         end do
         do I = N, 1, -1
            if (I < N) then
               if (G /= 0.0) then
                  do J = L, N
                     V(J, I) = (A(I, J) / A(I, L)) / G
                  end do
                  do J = L, N
                     S = 0.0
                     do K = L, N
                        S = S + A(I, K) * V(K, J)
                     end do
                     do K = L, N
                        V(K, J) = V(K, J) + S * V(K, I)
                     end do
                  end do
               end if
               do J = L, N
                  V(I, J) = 0.0
                  V(J, I) = 0.0
               end do
            end if
            V(I, I) = 1.0
            G = RV1(I)
            L = I
         end do
         do I = N, 1, -1
            L = I + 1
            G = W(I)
            if (I < N) then
               do J = L, N
                  A(I, J) = 0.0
               end do
            end if
            if (G /= 0.0) then
               G = 1.0 / G
               if (I /= N) then
                  do J = L, N
                     S = 0.0
                     do K = L, M
                        S = S + A(K, I) * A(K, J)
                     end do
                     F = (S / A(I, I)) * G
                     do K = I, M
                        A(K, J) = A(K, J) + F * A(K, I)
                     end do
                  end do
               end if
               do J = I, M
                  A(J, I) = A(J, I) * G
               end do
            else
               do J = I, M
                  A(J, I) = 0.0
               end do
            end if
            A(I, I) = A(I, I) + 1.0
         end do
         k_loop: do K = N, 1, -1
            do ITS = 1, 30
               l_loop: do L = K, 1, -1
                  NM = L - 1
                  if ((abs(RV1(L)) + ANORM) == ANORM) GO TO 2
                  if ((abs(W(NM)) + ANORM) == ANORM) exit l_loop
               end do l_loop
               C = 0.0
               S = 1.0
               do I = L, K
                  F = S * RV1(I)
                  if ((abs(F) + ANORM) /= ANORM) then
                     G = W(I)
                     H = sqrt(F * F + G * G)
                     W(I) = H
                     H = 1.0 / H
                     C = (G * H)
                     S = -(F * H)
                     do J = 1, M
                        Y = A(J, NM)
                        Z = A(J, I)
                        A(J, NM) = (Y * C) + (Z * S)
                        A(J, I) = -(Y * S) + (Z * C)
                     end do
                  end if
               end do
2              Z = W(K)
               if (L == K) then
                  if (Z < 0.0) then
                     W(K) = -Z
                     do J = 1, N
                        V(J, K) = -V(J, K)
                     end do
                  end if
                  cycle k_loop
               end if
!          IF (ITS.EQ.30) PAUSE 'No convergence in 30 iterations'
               if (ITS == 30) then ! SPvdP: error handling
                  A = 0d0
                  W = 0d0
                  V = 0d0
                  return
               end if
               X = W(L)
               NM = K - 1
               Y = W(NM)
               G = RV1(NM)
               H = RV1(K)
               F = ((Y - Z) * (Y + Z) + (G - H) * (G + H)) / (2.0 * H * Y)
               G = sqrt(F * F + 1.0)
               F = ((X - Z) * (X + Z) + H * ((Y / (F + sign(G, F))) - H)) / X
               C = 1.0
               S = 1.0
               do J = L, NM
                  I = J + 1
                  G = RV1(I)
                  Y = W(I)
                  H = S * G
                  G = C * G
                  Z = sqrt(F * F + H * H)
                  RV1(J) = Z
                  C = F / Z
                  S = H / Z
                  F = (X * C) + (G * S)
                  G = -(X * S) + (G * C)
                  H = Y * S
                  Y = Y * C
                  do NM = 1, N
                     X = V(NM, J)
                     Z = V(NM, I)
                     V(NM, J) = (X * C) + (Z * S)
                     V(NM, I) = -(X * S) + (Z * C)
                  end do
                  Z = sqrt(F * F + H * H)
                  W(J) = Z
                  if (Z /= 0.0) then
                     Z = 1.0 / Z
                     C = F * Z
                     S = H * Z
                  end if
                  F = (C * G) + (S * Y)
                  X = -(S * G) + (C * Y)
                  do NM = 1, M
                     Y = A(NM, J)
                     Z = A(NM, I)
                     A(NM, J) = (Y * C) + (Z * S)
                     A(NM, I) = -(Y * S) + (Z * C)
                  end do
               end do
               RV1(L) = 0.0
               RV1(K) = F
               W(K) = X
            end do
         end do k_loop
         return
      end

end module m_svdcmp
