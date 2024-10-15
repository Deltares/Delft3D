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

      subroutine MODGR2(XH, YH, X, Y, mmax, nmax, MC, NC, NUMP)
         use m_modfld
         use m_grid_block
         implicit none
         integer :: mmax, nmax, mc, nc, nump
         double precision :: X(MMAX, NMAX), Y(MMAX, NMAX), XH(MMAX, NMAX), YH(MMAX, NMAX)
         double precision :: EPS, dx1, dy1, dx2, dy2, fac, efac
         data EPS/0.00001d0/
         integer :: m1, m2, n1, n2, in, jn, i1, j1, klast, num, i, j, i2, j2, ii, jj
!     LINESHIFT

         M1 = MB(1)
         N1 = NB(1)
         M2 = MB(2)
         N2 = NB(2)

         KLAST = 1
         NUM = 0
         IN = min(1, M2 - M1)
         JN = min(1, N2 - N1)
         I1 = M1
         J1 = N1
         DX1 = XH(I1, J1) - X(I1, J1)
         DY1 = YH(I1, J1) - Y(I1, J1)
         do I = M1 + IN, M2
            do J = N1 + JN, N2
               if (abs(XH(I, J) - X(I, J)) > EPS .or. &
                   abs(YH(I, J) - Y(I, J)) > EPS .or. &
                   I == M2 .and. J == N2) then
                  I2 = I
                  J2 = J
                  DX2 = XH(I, J) - X(I, J)
                  DY2 = YH(I, J) - Y(I, J)
                  if (I == M2 .and. J == N2) KLAST = 0
                  do II = I1, I2 - IN * KLAST
                     do JJ = J1, J2 - JN * KLAST
                        if (IN == 1) then
                           FAC = dble(II - I1) / dble(I2 - I1)
                        else
                           FAC = dble(JJ - J1) / dble(J2 - J1)
                        end if
                        EFAC = 1 - FAC
                        XH(II, JJ) = X(II, JJ) + EFAC * DX1 + FAC * DX2
                        YH(II, JJ) = Y(II, JJ) + EFAC * DY1 + FAC * DY2
                        call MODFLD(XH, YH, X, Y, mmax, nmax, &
                                    MC, NC, II, JJ, &
                                    NUMP, 1, JN, IN)
                     end do
                  end do
                  I1 = I2
                  J1 = J2
                  DX1 = DX2
                  DY1 = DY2
               end if
            end do
         end do
         return
      end subroutine modgr2
