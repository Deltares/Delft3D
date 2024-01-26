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

      SUBROUTINE XYSPLN(      X,      Y,     XR,     YR,              &
                            XI2,    YI2,    XJ2,    YJ2,  XRH,  YRH,  &
                            mmax, nmax, imax, &
                             M1,     N1,     M2,     N2, MC, NC,      &
                           MFAC,   NFAC,   IJYES)
      use m_missing
      implicit none
!     SPLINE INTERPOLATIE BINNEN ALLE GROVE CELLEN
!     LIJN 1,2 ZIJN DE VERTICALE   CELWANDEN
!     LIJN 3,4 ZIJN DE HORIZONTALE CELWANDEN
      integer :: mmax, nmax, imax, m1, n1, m2, n2, mc, nc, mfac, nfac
      double precision :: X(MMAX,NMAX), XR(MMAX,NMAX),               &
              Y(MMAX,NMAX), YR(MMAX,NMAX),                           &
            XI2(MMAX,NMAX),XJ2(MMAX,NMAX),                           &
            YI2(MMAX,NMAX),YJ2(MMAX,NMAX),                           &
                XH1(IMAX),   XH21(IMAX),                             &
                XH2(IMAX),   XH22(IMAX),                             &
                XH3(IMAX),   XH23(IMAX),                             &
                XH4(IMAX),   XH24(IMAX),                             &
                YH1(IMAX),   YH21(IMAX),                             &
                YH2(IMAX),   YH22(IMAX),                             &
                YH3(IMAX),   YH23(IMAX),                             &
                YH4(IMAX),   YH24(IMAX),                             &
                 X1 (IMAX),      Y1(IMAX),                           &
                 X2 (IMAX),      Y2(IMAX),                           &
                 X3 (IMAX),      Y3(IMAX),                           &
                 X4 (IMAX),      Y4(IMAX),                           &
            XRH(MMAX,NMAX), YRH(MMAX,NMAX)
      INTEGER IJYES(MMAX,NMAX)

      double precision :: af, TI, TJ
      integer :: md, nd, mfa, nfa, mfaa, nfaa, ki1, i1, i2, j1, j2, &
                 KI, LJ, LJ1, K, L, dum
      XR = dmiss
      YR = dmiss

      MD   = M2 - M1
      ND   = N2 - N1
      MFAA = MFAC
      NFAA = NFAC
      IF (MD .EQ. 0) MFAA = 1
      IF (ND .EQ. 0) NFAA = 1

      KI1 = 0
      DO 40 I1 = 1,MC-1
         AF = 0.20d0 + 0.70d0*dble(I1-1)/(MC-1)
         CALL READYY(' ',AF)
         IF (I1 .GE. M1 .AND. I1 .LT. M2) THEN
            MFA = MFAA
         ELSE
            MFA = 1
         ENDIF
         I2    = I1 + 1
         CALL GETIJ(X,   XH1,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(XI2,XH21,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(X,   XH2,mmax, nmax, imax, I2,I2,1,NC)
         CALL GETIJ(XI2,XH22,mmax, nmax, imax, I2,I2,1,NC)
         CALL GETIJ(Y,   YH1,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(YI2,YH21,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(Y,   YH2,mmax, nmax, imax, I2,I2,1,NC)
         CALL GETIJ(YI2,YH22,mmax, nmax, imax, I2,I2,1,NC)
         LJ1 = 0
         DO 50 J1 = 1,NC-1
            IF (J1 .GE. N1 .AND. J1 .LT. N2) THEN
               NFA = NFAA
            ELSE
               NFA = 1
            ENDIF
            J2 = J1 + 1
            CALL GETIJ(X,   XH3,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(XJ2,XH23,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(X,   XH4,mmax, nmax, imax, 1,MC,J2,J2)
            CALL GETIJ(XJ2,XH24,mmax, nmax, imax, 1,MC,J2,J2)
            CALL GETIJ(Y,   YH3,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(YJ2,YH23,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(Y,   YH4,mmax, nmax, imax, 1,MC,J2,J2)
            CALL GETIJ(YJ2,YH24,mmax, nmax, imax, 1,MC,J2,J2)
            IF (IJYES(I1,J1) .EQ. 1) THEN

               DO 60 K = 1,MFA+1
                  TI  = (I1-1) + dble(K-1)/dble(MFA)
                  CALL SPLINT(XH3,XH23,MC,TI,X3(K))
                  CALL SPLINT(XH4,XH24,MC,TI,X4(K))
                  CALL SPLINT(YH3,YH23,MC,TI,Y3(K))
                  CALL SPLINT(YH4,YH24,MC,TI,Y4(K))
    60         CONTINUE
               DO 70 L = 1,NFA+1
                  TJ  = (J1-1) + dble(L-1)/dble(NFA)
                  CALL SPLINT(XH1,XH21,NC,TJ,X1(L))
                  CALL SPLINT(XH2,XH22,NC,TJ,X2(L))
                  CALL SPLINT(YH1,YH21,NC,TJ,Y1(L))
                  CALL SPLINT(YH2,YH22,NC,TJ,Y2(L))
!                 als je equidistant wil interpoleren
                  IF (J1 .EQ. -1) THEN
                     CALL EQDINT(XH1,imax,TJ,X1(L))
                     CALL EQDINT(XH2,imax,TJ,X2(L))
                     CALL EQDINT(YH1,imax,TJ,Y1(L))
                     CALL EQDINT(YH2,imax,TJ,Y2(L))
                  ENDIF
    70         CONTINUE
               IF (X1(1) .EQ. 0) THEN
                  DUM = 0
               ENDIF
               CALL TRANFN(     X1,     X2,     X3,     X4,      &
                                Y1,     Y2,     Y3,     Y4,      &
                              mmax,   nmax,   imax,              &
                              MFA ,   NFA ,    XRH,    YRH)
               DO 80 K = 1,MFA+1
                  DO 80 L = 1,NFA+1
                     KI  = KI1 + K
                     LJ  = LJ1 + L
                     XR(KI,LJ) = XRH(K,L)
                     YR(KI,LJ) = YRH(K,L)
    80         CONTINUE
            ENDIF
            LJ1 = LJ1 + NFA
    50   CONTINUE
         KI1 = KI1 + MFA
    40 CONTINUE
      RETURN
      END subroutine XYSPLN
