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

      subroutine DXYB(X, Y, mmax, nmax, MC, &
                      NC, II, JJ, IN, &
                      JN, DXY0)
         use precision, only: dp
         use m_missing
         use geometry_module, only: dbdistance
         use m_sferic, only: jsferic, jasfer3D

         implicit none
         integer :: mmax, nmax, mc, nc, ii, jj, in, jn
         real(kind=dp) :: dxy0
         real(kind=dp) :: X(MMAX, NMAX), Y(MMAX, NMAX)

         integer :: num
         real(kind=dp) :: XU, YU, XD, YD, dxy1
         NUM = 0
         DXY0 = 0

         if (II + IN <= MC .and. JJ + JN <= NC) then
            XU = X(II + IN, JJ + JN)
            if (XU /= XYMIS) then
               YU = Y(II + IN, JJ + JN)
               dxy0 = dbdistance(X(II, JJ), Y(II, JJ), XU, YU, jsferic, jasfer3D, dmiss)
               NUM = NUM + 1
            end if
         end if

         if (II - IN >= 1 .and. JJ - JN >= 1) then
            XD = X(II - IN, JJ - JN)
            if (XD /= XYMIS) then
               YD = Y(II - IN, JJ - JN)
               dxy1 = dbdistance(X(II, JJ), Y(II, JJ), XD, YD, jsferic, jasfer3D, dmiss)
               NUM = NUM + 1
               DXY0 = (DXY0 + DXY1) / dble(NUM)
            end if
         end if

         return
      end subroutine dxyb
