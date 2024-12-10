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

module m_fieldop

   implicit none

contains

   function FIELDOP(NUM)

      integer :: num
      character(len=40) FIELDOP

      if (NUM == 1) then
         FIELDOP = 'Point Mode                              '
      else if (NUM == 2) then
         FIELDOP = 'Field Mode                              '
      else if (NUM == 3) then
         FIELDOP = '                                        '
      else if (NUM == 4) then
         FIELDOP = 'Line Shift                              '
      else if (NUM == 5) then
         FIELDOP = 'Line Attraction                         '
      else if (NUM == 6) then
         FIELDOP = 'Line Repulsion                          '
      else if (NUM == 7) then
         FIELDOP = 'Line to Land Boundary                   '
      else if (NUM == 8) then
         FIELDOP = 'Line to Spline (only to spline nr 1)    '
      else if (NUM == 9) then
         FIELDOP = 'Line Smooth                             '
      else if (NUM == 10) then
         FIELDOP = 'Line Mirror                             '
      else if (NUM == 11) then
         FIELDOP = 'Refine Grid Locally                     '
      else if (NUM == 12) then
         FIELDOP = 'Derefine Grid Locally                   '
      else if (NUM == 13) then
         FIELDOP = '                                        '
      else if (NUM == 14) then
         FIELDOP = 'Block Delete                            '
      else if (NUM == 15) then
         FIELDOP = 'Block Cut                               '
      else if (NUM == 16) then
         FIELDOP = 'Block Orthogonalise                     '
      else if (NUM == 17) then
         FIELDOP = 'Block Smooth                            '
      else if (NUM == 18) then
         FIELDOP = '                                        '
      else if (NUM == 19) then
         FIELDOP = 'Orthogonise whole grid                  '
      else if (NUM == 20) then
         FIELDOP = 'Refine globally                         '
      else if (NUM == 21) then
         FIELDOP = 'Derefine globally                       '
      else if (NUM == 22) then
         FIELDOP = 'Back to Main Edit Modes                 '
      end if
      return
   end function fieldop

end module m_fieldop
