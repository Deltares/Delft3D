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

      FUNCTION FIELDOP(NUM)
      implicit none
      integer :: num
      CHARACTER*40 FIELDOP
      IF (NUM .EQ. 1) THEN
         FIELDOP = 'Point Mode                              '
      ELSE IF (NUM .EQ. 2) THEN
         FIELDOP = 'Field Mode                              '
      ELSE IF (NUM .EQ. 3) THEN
         FIELDOP = '                                        '
      ELSE IF (NUM .EQ. 4) THEN
         FIELDOP = 'Line Shift                              '
      ELSE IF (NUM .EQ. 5) THEN
         FIELDOP = 'Line Attraction                         '
      ELSE IF (NUM .EQ. 6) THEN
         FIELDOP = 'Line Repulsion                          '
      ELSE IF (NUM .EQ. 7) THEN
         FIELDOP = 'Line to Land Boundary                   '
      ELSE IF (NUM .EQ. 8) THEN
         FIELDOP = 'Line to Spline (only to spline nr 1)    '
      ELSE IF (NUM .EQ. 9) THEN
         FIELDOP = 'Line Smooth                             '
      ELSE IF (NUM .EQ. 10) THEN
         FIELDOP = 'Line Mirror                             '
      ELSE IF (NUM .EQ. 11) THEN
         FIELDOP = 'Refine Grid Locally                     '
      ELSE IF (NUM .EQ. 12) THEN
         FIELDOP = 'Derefine Grid Locally                   '
      ELSE IF (NUM .EQ. 13) THEN
         FIELDOP = '                                        '
      ELSE IF (NUM .EQ. 14) THEN
         FIELDOP = 'Block Delete                            '
      ELSE IF (NUM .EQ. 15) THEN
         FIELDOP = 'Block Cut                               '
      ELSE IF (NUM .EQ. 16) THEN
         FIELDOP = 'Block Orthogonalise                     '
      ELSE IF (NUM .EQ. 17) THEN
         FIELDOP = 'Block Smooth                            '
      ELSE IF (NUM .EQ. 18) THEN
         FIELDOP = '                                        '
      ELSE IF (NUM .EQ. 19) THEN
         FIELDOP = 'Orthogonise whole grid                  '
      ELSE IF (NUM .EQ. 20) THEN
         FIELDOP = 'Refine globally                         '
      ELSE IF (NUM .EQ. 21) THEN
         FIELDOP = 'Derefine globally                       '
      ELSE IF (NUM .EQ. 22) THEN
         FIELDOP = 'Back to Main Edit Modes                 '
      ENDIF
      RETURN
      END function fieldop
