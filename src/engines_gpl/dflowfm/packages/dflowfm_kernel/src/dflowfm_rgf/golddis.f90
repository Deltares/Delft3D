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

      SUBROUTINE GOLDDIS(AX,BX,CX,TOL,P,P2,Y,Y2,T, N,NT,XMIN,DIS,SSQ,H)
      implicit none
      !USE DIMENS
      integer :: imax, n, nt
      double precision :: P(N), P2(N), Y(N), Y2(N), T(N)
      double precision :: ax, bx, cx, tol, xmin, dis, ssq
      double precision, intent(in) :: H   !< for curvature adapted meshing

      double precision, PARAMETER :: R=.61803399d0,C=.38196602d0
      double precision :: x0, x1, x2, x3, f0, f1, f2, f3, d1, d2

!     Eendimensionaal zoeken van 'gebracked' minimum
      X0=AX
      X3=CX
      IF(ABS(CX-BX).GT.ABS(BX-AX))THEN
        X1=BX
        X2=BX+C*(CX-BX)
      ELSE
        X2=BX
        X1=BX-C*(BX-AX)
      ENDIF
      CALL GETDIS(P,Y,P2,Y2,N,X1,D1,H)
      F1 = ABS(D1 - SSQ)
      CALL GETDIS(P,Y,P2,Y2,N,X2,D2,H)
      F2 = ABS(D2 - SSQ)
1     IF(ABS(X3-X0).GT.TOL*max(ABS(X1)+ABS(X2),1d-8))THEN
!     IF(ABS(X3-X0).GT.TOL) THEN
        IF(F2.LT.F1)THEN
          X0=X1
          X1=X2
          X2=R*X1+C*X3
          F0=F1
          F1=F2
          CALL GETDIS(P,Y,P2,Y2,N,X2,D2,H)
          F2 = ABS(D2 - SSQ)
        ELSE
          X3=X2
          X2=X1
          X1=R*X2+C*X0
          F3=F2
          F2=F1
          CALL GETDIS(P,Y,P2,Y2,N,X1,D1,H)
          F1 = ABS(D1 - SSQ)
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
      END subroutine golddis
