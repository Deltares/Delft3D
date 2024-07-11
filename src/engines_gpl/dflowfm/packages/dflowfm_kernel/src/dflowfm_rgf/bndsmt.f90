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

!*******************  BOUNDARY TREATMENT *****************************
      SUBROUTINE BNDSMT(XR,YR,XI2,YI2,XJ2,YJ2,ATP,M1,N1,M2,N2)
      use m_grid
      use m_gridsettings
      implicit none
      double precision :: bfe
      integer :: i
      integer :: iff
      integer :: ifr
      integer :: il
      integer :: ilr
      integer :: in
      integer :: int
      integer :: ir
      integer :: irr
      integer :: j
      integer :: jf
      integer :: jfr
      integer :: jl
      integer :: jlr
      integer :: jr
      integer :: jrr
      integer :: kc
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      integer :: num
      double precision :: qb
      double precision :: qbc
      double precision :: qc
      double precision :: rn
      double precision :: x0
      double precision :: x1
      double precision :: x2
      double precision :: x3
      double precision :: y0
      double precision :: y1
      double precision :: y2
      double precision :: y3
!     RANDPUNTEN OF INTERNE PUNTEN
!     TERUGZETTEN OP DE SPLINE TUSSEN OUDE POSITIE OP RAND (BFAC = 0)
!     EN PROJECTIE OP SPLINE VAN NABIJ PUNT (BFAC = 1)
!     BIJ NCODE IS 5, INT(ERNAL) HORIZONTAAL, 6 = VERTICAAL
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX),   &
                          XI2(MMAX,NMAX),XJ2(MMAX,NMAX),ATP(MMAX,NMAX),               &
                          YI2(MMAX,NMAX),YJ2(MMAX,NMAX)

      DOUBLE PRECISION, ALLOCATABLE :: XH(:), YH(:), XH2(:), YH2(:)
      DOUBLE PRECISION              :: XX1, XX2, YY1, YY2, TV, XV, YV, XV2, YV2, DIS

      ALLOCATE ( XH(MNMAX), YH(MNMAX), XH2(MNMAX), YH2(MNMAX) )


      IF (BFAC .EQ. 0) RETURN
      BFE = 1 - BFAC

!     DE HORIZONTALEN
      do JR = 1,NC
         IN  = 0
         INT = 0
         J   = JR
         do IR = 1,MC
            KC = ABS(IJC(IR,JR))
            IF (KC .EQ. 11 .OR. KC .EQ. 14) THEN
               IFR = IR + 1
               IFF = IR
            ELSE IF (KC .EQ. 1) THEN
               IN  = 1
            ELSE IF (KC .EQ. 3) THEN
               IN  = -1
            ELSE IF (KC .EQ. 5) THEN
               IN  = 1
               INT = 1
            ELSE IF (KC .EQ. 12 .OR. KC .EQ. 13 .AND. IN .NE. 0) THEN
               ILR = IR - 1
               IL  = IR
               NUM = IL - IFF + 1
               CALL GETIJ(XC,   XH,MMAX,NMAX,MNMAX,IFF,IL, J, J)
               CALL GETIJ(XJ2,XH2,MMAX,NMAX,MNMAX,IFF,IL, J, J)
               CALL GETIJ(YC,   YH,MMAX,NMAX,MNMAX,IFF,IL, J, J)
               CALL GETIJ(YJ2,YH2,MMAX,NMAX,MNMAX,IFF,IL, J, J)

               DO IRR = IFR,ILR
                  IF (IRR .GE. M1 .AND. IRR .LE. M2 .AND.   &
                       JR .GE. N1 .AND.  JR .LE. N2 .AND.   &
                       IJC(IRR,JR) .GT. 0) THEN
                     XX1 = XR(IRR,JR+IN)
                     YY1 = YR(IRR,JR+IN)
                     X0  = XR(IRR,JR)
                     Y0  = YR(IRR,JR)
                     XX1 = XX1*BFAC + X0*BFE
                     YY1 = YY1*BFAC + Y0*BFE
                     TV  = IRR - IFF
                     IF (IN .EQ. 1) THEN
!                       onder
                        X1  = XR(IRR-1,JR)
                        X3  = XR(IRR+1,JR)
                        X2  = XR(IRR,JR+1)
                        Y1  = YR(IRR-1,JR)
                        Y3  = YR(IRR+1,JR)
                        Y2  = YR(IRR,JR+1)
                        QB  = ATP(IRR-1,JR)
                        QC  = ATP(IRR,JR)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y3 - Y1) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X1 - X3) / RN
                     ELSE IF (IN .EQ. -1) THEN
!                       boven
                        X1  = XR(IRR-1,JR)
                        X3  = XR(IRR+1,JR)
                        X2  = XR(IRR,JR-1)
                        Y1  = YR(IRR-1,JR)
                        Y3  = YR(IRR+1,JR)
                        Y2  = YR(IRR,JR-1)
                        QB  = ATP(IRR-1,JR-1)
                        QC  = ATP(IRR,JR-1)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y1 - Y3) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X3 - X1) / RN
                     ENDIF
!                    CALL RCIRC(XX1,YY1)
                     CALL DISMIN(XH,XH2,YH,YH2,XX1,YY1,NUM,DIS,TV,XV,YV)
                     IF (INT .EQ. 1) THEN
!                       for internal boundary points
                        XX2 = XR(IRR,JR-IN)
                        YY2 = YR(IRR,JR-IN)
                        XX2 = XX2*BFAC + X0*BFE
                        YY2 = YY2*BFAC + Y0*BFE
                        CALL DISMIN(XH,XH2,YH,YH2,XX2,YY2,NUM,DIS,TV,XV2,YV2)
                        XV  = (XV + XV2)/2
                        YV  = (YV + YV2)/2
                     ENDIF
                     XR(IRR,JR) = XV
                     YR(IRR,JR) = YV
                  ENDIF
               end do
               IN  = 0
               INT = 0
            ENDIF
         end do
      end do
      

!     CALL WAITESC()

!     DE VERTICALEN
      DO IR = 1,MC
         IN  = 0
         INT = 0
         I   = IR
         DO JR = 1,NC
            KC = ABS(IJC(IR,JR))
            IF (KC .EQ. 11 .OR. KC .EQ. 12) THEN
               JFR = JR + 1
               JF  = JR
            ELSE IF (KC .EQ. 4) THEN
               IN = 1
            ELSE IF (KC .EQ. 2) THEN
               IN = -1
            ELSE IF (KC .EQ. 6) THEN
               IN  = 1
               INT = 1
            ELSE IF (KC .EQ. 14 .OR. KC .EQ. 13 .AND. IN .NE. 0) THEN
               JLR = JR - 1
               JL  = JR
               NUM = JL - JF + 1
               CALL GETIJ(XC,   XH,MMAX,NMAX,MNMAX, I, I,JF,JL)
               CALL GETIJ(XI2,XH2,MMAX,NMAX,MNMAX, I, I,JF,JL)
               CALL GETIJ(YC,   YH,MMAX,NMAX,MNMAX, I, I,JF,JL)
               CALL GETIJ(YI2,YH2,MMAX,NMAX,MNMAX, I, I,JF,JL)

               DO JRR = JFR,JLR
                  IF (JRR .GE. N1 .AND. JRR .LE. N2 .AND.    &
                       IR .GE. M1 .AND.  IR .LE. M2 .AND.    &
                       IJC(IR,JRR) .GT. 0) THEN
                     XX1 = XR(IR+IN,JRR)
                     YY1 = YR(IR+IN,JRR)
                     X0  = XR(IR,JRR)
                     Y0  = YR(IR,JRR)
                     XX1 = XX1*BFAC + X0*BFE
                     YY1 = YY1*BFAC + Y0*BFE
                     TV  = JRR - JF
                     IF (IN .EQ. 1) THEN
!                       links
                        X1  = XR(IR,JRR-1)
                        X3  = XR(IR,JRR+1)
                        X2  = XR(IR+1,JRR)
                        Y1  = YR(IR,JRR-1)
                        Y3  = YR(IR,JRR+1)
                        Y2  = YR(IR+1,JRR)
                        QC  = 1d0/ATP(IR,JRR)
                        QB  = 1d0/ATP(IR,JRR-1)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y1 - Y3) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X3 - X1) / RN
                     ELSE IF (IN .EQ. -1) THEN
!                       rechts
                        X1  = XR(IR,JRR-1)
                        X3  = XR(IR,JRR+1)
                        X2  = XR(IR-1,JRR)
                        Y1  = YR(IR,JRR-1)
                        Y3  = YR(IR,JRR+1)
                        Y2  = YR(IR-1,JRR)
                        QC  = 1d0/ATP(IR-1,JRR)
                        QB  = 1d0/ATP(IR-1,JRR-1)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y3 - Y1) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X1 - X3) / RN
                     ENDIF
!                    CALL RCIRC(XX1,YY1)
                     CALL DISMIN(XH,XH2,YH,YH2,XX1,YY1,NUM,DIS,TV,XV,YV)
                     IF (INT .EQ. 1) THEN
!                       for internal boundary points
                        XX2 = XR(IR-IN,JRR)
                        YY2 = YR(IR-IN,JRR)
                        XX2 = XX2*BFAC + X0*BFE
                        YY2 = YY2*BFAC + Y0*BFE
                        CALL DISMIN(XH,XH2,YH,YH2,XX2,YY2,NUM,DIS,TV,XV2,YV2)
                        XV = (XV + XV2)/2
                        YV = (YV + YV2)/2
                     ENDIF
                     XR(IR,JRR) = XV
                     YR(IR,JRR) = YV
                  ENDIF
               end do

               IN  = 0
               INT = 0
            ENDIF
         end do
      end do
         
      DEALLOCATE ( XH, YH, XH2, YH2 )


      RETURN
      END SUBROUTINE BNDSMT
