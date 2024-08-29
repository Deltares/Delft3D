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

      subroutine MODGR4(NUMP, LANDORSPLINE)
         use m_grid
         use m_landboundary
         use M_SPLINES, only: mcs, splnump => nump
         use m_modfld
         use m_grid_block
         use m_qnerror
         implicit none
         integer :: nump, landorspline
         integer :: m1, m2, n1, n2, i, j, in, jn, ncs, jdum
         double precision :: EPS, X0, Y0, XN, YN, DIS, RL
!     TO LAND
         data EPS/0.00001d0/
         if (LANDORSPLINE == 1) then
            if (MXLAN == 0) then
               call QNERROR('FIRST LOAD A LANDBOUNDARY', ' ', ' ')
               return
            end if
         else
            call splnump(1, ncs)
            if (MCS < 1 .or. NCS < 2) then
               call QNERROR('FIRST DRAW SPLINE NR 1', ' ', ' ')
               return
            end if
         end if
         M1 = MB(1)
         N1 = NB(1)
         M2 = MB(2)
         N2 = NB(2)
         IN = min(1, N2 - N1)
         JN = min(1, M2 - M1)
         do I = M1, M2
            do J = N1, N2
               X0 = Xch(I, J)
               Y0 = Ych(I, J)
               if (LANDORSPLINE == 1) then
                  call TOLAND(X0, Y0, 1, MXLAN, 1, XN, YN, DIS, JDUM, RL)
               else
                  call TOSPLINE(X0, Y0, XN, YN)
               end if
               Xc(I, J) = XN
               Yc(I, J) = YN
               if (abs(Xch(I, J) - Xc(I, J)) > EPS .or. &
                   abs(Ych(I, J) - Yc(I, J)) > EPS) then
                  call MODFLD(Xc, Yc, Xch, Ych, mmax, nmax, &
                              MC, NC, I, J, &
                              NUMP, 1, IN, JN)
               end if
            end do
         end do
         return
      end subroutine modgr4
