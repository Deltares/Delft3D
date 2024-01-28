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

      SUBROUTINE DISMIN(X,X2,Y,Y2,XX,YY,N,DIS,TV,XV,YV)
      implicit none
      integer :: n
      double precision :: rn
!     ZOEK MEEST NABIJE PUNT OP SPLINE
!     START ZOEKEN ROND TV, ZOEK MET GULDEN SNEDE ROUTINE
!     N IS MAXIMUM INDEX ZOEKGEBIED
      DOUBLE PRECISION :: X(N), X2(N), Y(N), Y2(N), XV, YV, XX, YY, TV
      DOUBLE PRECISION :: AX, BX, CX, TOL, DIS

!     RLEN = SQRT((X(1)-X(2))**2+(Y(1)-Y(2))**2)
      TOL  = 0.000001d0
!     TOL  = 0.000005*RLEN
      RN  = dble(N)
      AX  = 0d0
      BX  = TV
      CX  = RN
      CALL GOLD(AX,BX,CX,TOL,TV,X,X2,Y,Y2,XX,YY,N,DIS)

      CALL SPLINT(X,X2,N,TV,XV)
      CALL SPLINT(Y,Y2,N,TV,YV)

      RETURN
      END SUBROUTINE DISMIN
