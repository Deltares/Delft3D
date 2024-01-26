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

!>   generate grid between fixed points on a spline that itself is defined by control points
!>     in:  t(Nt) fixed points on spline
!>          x(N)  x-coordinates of spline control points
!>          y(N)  y-coordinates of spline control points
!>          imax  maximum array size (should be .ge. n, nt and kmax)
!>          N     number of spline control points
!>          Nt    number of fixed points
!>          MNfac number of grid intervals between fixed points
!>          H     significant height, where the grid should be equidistant (>0) or disable (<=0)
!>
!>     out: xh(kmax) x-coordinates of grid points
!>          yh(kmax) y-coordinates of grid points
!>          kmax     number of grid points = 1+MNfac*(NT-1)
!>          tt(imax) spline-coordinates of grid points
     SUBROUTINE MAKESPL(T,X,Y,imax, N,NT,MNFAC,XH,YH,KMAX,TT,H)
     use m_gridsettings
     implicit none
     ! USE DIMENS

     integer :: imax, n, nt, kmax, mnfac
      double precision :: X(IMAX), Y(IMAX), X2(IMAX), Y2(IMAX), T(IMAX), S(IMAX),   &
           S2(IMAX), SSQ(IMAX), XH(IMAX), YH(IMAX),                  &
            A(IMAX), SL(IMAX), SR(IMAX)
      double precision, intent(in) :: H   !< for curvature adapted meshing

      double precision, dimension(IMAX), intent(out) :: TT !< spline-coordinates of grid points

      integer :: L, k1, k2, jadip, k
!      COMMON /SPLINEFAC/ SPLFAC, SPLFAC2
!     Maak interpolatie

!     Eerst splines X,Y en S aanmaken
      CALL MAKES(X,Y,X2,Y2,T,S,S2,imax, N,NT,H)

      KMAX = MNFAC*(NT - 1) + 1

      IF (NT .GE. 2) THEN
         CALL MAKESSQ(S,A,SR,SL,SSQ,NT,MNFAC,IMAX)

!         DST  = REAL(NT-1) / REAL(KMAX-1)
!         ST   = 0
!C        Spline interpolatie in afstanden
!         SPLFACORG = SPLFAC
!         SPLFAC    = SPLFAC2
!         DO 10 K = 1,KMAX
!            CALL SPLINT(S,S2,NT,ST,SSQ(K))
!            ST = ST + DST
!    1    CONTINUE
!         SPLFAC = SPLFACORG

!        Check op positief en monotoon
         DO 20 L = 1,NT-1
            K1 = MNFAC*(L - 1) + 1
            K2 = K1 + MNFAC

            JADIP = 0
    23      IF (JADIP .EQ. 1) THEN
               DO 24 K = K1+1,K2-1
                  SSQ(K) = 0.5*( SSQ(K-1) + SSQ(K+1) )
    24         CONTINUE
            ENDIF

            DO 25 K = K1,K2-1
               IF ( SSQ(K+1) .LT. SSQ(K) ) THEN
                  JADIP = 1
                  GOTO 23
               ENDIF
    25      CONTINUE

    20   CONTINUE
      ELSE
         SSQ(1) = T(1)
      ENDIF

!     Punten terug invullen in oorspronkelijke spline
      DO 30 K = 1,KMAX
         CALL GETXY(T,X,X2,Y,Y2,imax,N,NT,SSQ(K),XH(K),YH(K),TT(K),H)
    30 CONTINUE

      RETURN
      END subroutine makespl
