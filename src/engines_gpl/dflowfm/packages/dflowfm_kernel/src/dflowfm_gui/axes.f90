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

      SUBROUTINE AXES()
      use unstruc_colors
      implicit none
      integer :: jaxis
      double precision :: xleft
      double precision :: ybot
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS
      IF (JAXIS .EQ. 1) THEN
         CALL SETCOL(KLAXS)
         CALL viewport(0.0,0.0,1.0,1.0)
         CALL IPGBORDER()
         CALL IPGXTICKPOS(Y1,Y2)
         CALL IPGXSCALE     ('TN')
         CALL IPGXSCALETOP  ('TN')
         CALL IPGYTICKPOS(X1,X2)
         CALL IPGYSCALELEFT ('TN')
         CALL IPGYSCALERIGHT('TN')
         CALL SMALLSCREEN()
      ENDIF
      RETURN
      END
