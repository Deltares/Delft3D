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

      SUBROUTINE HITLIN(P1,P2,X1,Y1,X2,Y2,V,XHIT,YHIT,JA)
      implicit none
      double precision :: dp, dv, dx, dy, frac, p1, p2, v, x1, x2, xhit, y1, y2, yhit
      integer :: ja
!     SNIJDT EEN ISOLIJN EEN LIJNTJE ?
      DX   = X2 - X1
      DY   = Y2 - Y1
      DP   = P2 - P1
      DV   = V  - P1
      IF (DP .NE. 0) THEN
         FRAC = DV/DP
      ELSE IF (V .EQ. P2) THEN
         FRAC = 1d0
      ELSE
         FRAC = 0
      ENDIF
      JA = 0
      IF (0d0 .LT. FRAC .AND. FRAC .LE. 1d0) THEN
         JA   = 1
         XHIT = X1 + FRAC*DX
         YHIT = Y1 + FRAC*DY
      ENDIF
      RETURN
      END
