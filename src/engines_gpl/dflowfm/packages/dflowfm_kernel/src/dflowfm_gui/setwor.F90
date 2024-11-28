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

module m_setwor

implicit none

contains

      subroutine SETWOR(XW1, YW1, XW2, YW2)
         use unstruc_opengl
         use precision, only: dp
#ifdef HAVE_OPENGL
         use IFOPNGL, only: GL_PROJECTION, GL_MODELVIEW, fglMatrixMode, fglLoadIdentity, fglOrtho
#endif
         implicit none
         double precision :: XW1, YW1, XW2, YW2
         if (XW1 == XW2 .or. YW1 == YW2) then
            XW2 = XW1 + 1
            YW2 = YW1 + 1
         end if

         if (InOpenGLRendering) then
#ifdef HAVE_OPENGL
            !  CALL fglDisable(GL_DEPTH_TEST) ! no depth
            call fglMatrixMode(GL_PROJECTION)
            call fglLoadIdentity()
            call fglOrtho(real(XW1, dp), real(XW2, dp), real(YW1, dp), real(YW2, dp), real(0, dp), real(1, dp))
            call fglMatrixMode(GL_MODELVIEW)
#endif
         else
            call IGrUnits(real(XW1), real(YW1), real(XW2), real(YW2))
         end if
         return
      end

end module m_setwor
