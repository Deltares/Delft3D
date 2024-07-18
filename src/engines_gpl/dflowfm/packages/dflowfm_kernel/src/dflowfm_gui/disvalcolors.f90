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

      subroutine DISVALCOLORS(NUMCOL, N1, N2, N3, IC)
         use M_DEVICES
         implicit none
         integer :: ic
         integer :: n1
         integer :: n2
         integer :: n3
         integer :: numcol
         character TEXT * 47
         if (IC == 1) then
            TEXT = 'COLOR NUMBER:     RED:     g    :     b   :    '
         else if (IC == 2) then
            TEXT = 'COLOR NUMBER:     r  :     GREEN:     b   :    '
         else
            TEXT = 'COLOR NUMBER:     r  :     g    :     BLUE:    '
         end if
         write (TEXT(15:17), '(I3)') NUMCOL
         write (TEXT(23:25), '(I3)') N1
         write (TEXT(34:36), '(I3)') N2
         write (TEXT(44:46), '(I3)') N3
         call KTEXT(TEXT, IWS - 46, 4, 15)
         return
      end
