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

module m_wearel

implicit none

contains

      subroutine WEAREL()
         use m_minmaxworld
         use m_inilca
         use m_wearelt
         implicit none

         call MINMAXWORLD(XMIN, YMIN, XMAX, YMAX)

         !X1   = XMIN
         !Y1   = YMIN
         !X2   = XMAX
         !CALL SETWY(X1,Y1,X2,Y2)
         !IF (INI .EQ. 1) THEN
         call INILCA()
         !ELSE
         !   INI = 1
         !ENDIF
         return
      end

end module m_wearel
