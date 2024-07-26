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

      subroutine CIR(R)
         use unstruc_opengl
         implicit none
         integer :: ncolnow
         double precision :: r, Hr
         common / COLNOW / NCOLNOW

         if (r == 0d0) return
         if (InOpenGLRendering) then
            HR = 0.5d0 * R
            call KREC5(dble(Xlast), dble(Ylast), HR, HR)
            !CALL SetPointSize(real(5))
            !CALL DrawPoint(xlast,ylast)
            !CALL SetPointSize(real(1))
         else
            call IGrCircleRel(real(R))
         end if
      end
