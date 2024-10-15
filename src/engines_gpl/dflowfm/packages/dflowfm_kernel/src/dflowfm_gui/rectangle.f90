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

      subroutine RECTANGLE(x1, y1, x2, y2)
         use unstruc_opengl
         use m_pfiller_core
         implicit none
         real x1, y1, x2, y2
         real x(4), y(4)

         if (InOpenGLRendering) then
            x(1) = x1
            x(2) = x2
            x(3) = x2
            x(4) = x1
            y(1) = y1
            y(2) = y1
            y(3) = y2
            y(4) = y2
            call PFILLERCORE(x, y, 4)
         else
            call IGRRECTANGLE(x1, y1, x2, y2)
         end if

      end subroutine
