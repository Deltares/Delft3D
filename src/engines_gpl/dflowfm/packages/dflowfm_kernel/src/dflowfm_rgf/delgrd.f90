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

     SUBROUTINE delgrd(KEY,JASAVE,jadelpol)
!    delete grid
     use m_grid
     use m_missing
     use m_polygon, only: NPL, xpl, ypl, zpl
     use geometry_module, only: dbpinpol

     implicit none
     integer                :: inhul, ja, i, j
     integer, intent(in)    :: jasave, jadelpol
     integer, intent(inout) :: key
     double precision       :: xi, yi

     inhul = -1

     IF (JASAVE .EQ. 1) CALL SAVEgrd()
     KEY = 3
     IF (NPL .LE. 2) THEN
        IF (NPL .GE. 1) THEN
           CALL CONFRM('NO POLYON, SO DELETE all GRID POINTS ? ',JA)
           IF (JA .EQ. 0) THEN
              KEY = 0
           ELSE
              XC = 0d0 ; YC = 0d0; MC = 0 ; NC = 0
           ENDIF
        ELSE
           XC = 0d0 ; YC = 0d0; MC = 0 ; NC = 0
        ENDIF
        RETURN
     ENDIF

     do I = 1,MC
        do J = 1,NC
           IF (Xc(I,J) .NE. DXYMIS) THEN
              CALL dbpinpol(Xc(i,j), yc(i,j), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
              IF (INHUL .EQ. 1) Xc(I,J) = XYMIS
           ENDIF

        end do
     end do
     
!      CALL ADJUST(X, Y, MC, NC, WW1, WW2)
     if (jadelpol == 1) call delpol()
     RETURN
     END SUBROUTINE delgrd
