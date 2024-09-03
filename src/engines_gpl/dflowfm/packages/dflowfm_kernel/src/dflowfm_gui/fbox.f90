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

      subroutine FBOX(X1, Y1, X2, Y2)
         use m_fboxold
         use m_drawthis
         implicit none
         double precision :: x1, x2, y1, y2
         double precision :: xb1, xb2, yb1, yb2

         call DPROJECT(X1, Y1, XB1, YB1, 1)
         call DPROJECT(X2, Y2, XB2, YB2, 1)
         if (ndraw(10) == 0) then
            call RECTANGLE(real(XB1), real(YB1), real(XB2), real(YB2))
         else
            call fboxold(XB1, YB1, XB2, YB2)
         end if
         return
      end
