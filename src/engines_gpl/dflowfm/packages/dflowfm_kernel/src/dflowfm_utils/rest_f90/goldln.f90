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

      SUBROUTINE GOLDLN(AX,BX,CX,TOL,XMIN,P,Q,N,XX,YY,DIS)
      implicit none
      double precision :: ax
      double precision :: bx
      double precision :: c
      double precision :: cx
      double precision :: dis
      double precision :: f0
      double precision :: f1
      double precision :: f2
      double precision :: f3
      integer :: n
      double precision :: r
      double precision :: tol
      double precision :: x0
      double precision :: x1
      double precision :: x2
      double precision :: x3
      double precision :: xmin
      double precision :: xx
      double precision :: yy
      PARAMETER (R=.61803399,C=.38196602)
!     EENDIMENSIONAAL ZOEKEN VAN 'GEBRACKED' MINIMUM
      double precision :: P(N), Q(N)
      double precision :: dislin
      X0=AX
      X3=CX
      IF(ABS(CX-BX).GT.ABS(BX-AX))THEN
        X1=BX
        X2=BX+C*(CX-BX)
      ELSE
        X2=BX
        X1=BX-C*(BX-AX)
      ENDIF
!     F1=F(X1)
!     F1=DIST(P,P2,Q,Q2,XX,YY,X1,N)
      F1=DISLIN(P,Q,N,XX,YY,X1)
!     F2=F(X2)
!     F2=DIST(P,P2,Q,Q2,XX,YY,X2,N)
      F2=DISLIN(P,Q,N,XX,YY,X2)
1     IF(ABS(X3-X0).GT.TOL*(ABS(X1)+ABS(X2)))THEN
        IF(F2.LT.F1)THEN
          X0=X1
          X1=X2
          X2=R*X1+C*X3
          F0=F1
          F1=F2
!         F2=DIST(P,P2,Q,Q2,XX,YY,X2,N)
          F2=DISLIN(P,Q,N,XX,YY,X2)
!         F2=F(X2)
        ELSE
          X3=X2
          X2=X1
          X1=R*X2+C*X0
          F3=F2
          F2=F1
!         F1=F(X1)
          F1=DISLIN(P,Q,N,XX,YY,X1)
!         F1=DIST(P,P2,Q,Q2,XX,YY,X1,N)
        ENDIF
      GOTO 1
      ENDIF
      IF(F1.LT.F2)THEN
        DIS =F1
        XMIN=X1
      ELSE
        DIS =F2
        XMIN=X2
      ENDIF
      RETURN
      END
