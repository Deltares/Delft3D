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

      SUBROUTINE ISOFILTRI(X,Y,Z,NCOLR)
      implicit none
      double precision :: dv
      double precision :: dzn
      double precision :: frac
      integer :: i
      integer :: ih
      integer :: j
      integer :: j1
      integer :: j2
      integer :: jaauto
      integer :: ncol
      integer :: ncolr
      integer :: ncols
      integer :: nie
      integer :: nis
      integer :: npics
      integer :: num
      integer :: nv
      integer :: nx1
      integer :: nx3
      integer :: ny1
      integer :: ny3
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: zmax
      double precision :: zmin
      double precision :: znex
      double precision :: znow
      double precision :: X(3), Y(3), Z(3), DX(3),DY(3), DZ(3), XH(10),YH(10)
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      DO 10 I = 1,3
         J = I + 1
         IF (I .EQ. 3) J = 1
         DX(I) = X(J) - X(I)
         DY(I) = Y(J) - Y(I)
         DZ(I) = Z(J) - Z(I)
   10 CONTINUE

      ZMAX = Z(1)
      ZMIN = Z(1)
      DO 15 I = 2,3
         ZMAX = MAX(ZMAX,Z(I))
         ZMIN = MIN(ZMIN,Z(I))
   15 CONTINUE

      IF (ZMAX .LE. VAL(1)) THEN
         NCOL = NCOLS(1)
         CALL PFILLER(X,Y,3,NCOL,NCOL)
      ELSE IF (ZMIN .GE. VAL(NV)) THEN
         NCOL = NCOLS(NV+1)
         CALL PFILLER(X,Y,3,NCOL,NCOL)
      ELSE
       DO 20 I = 0,NV
         IF (I .EQ. 0) THEN
            ZNOW = -1E+30
         ELSE
            ZNOW = VAL(I)
         ENDIF
         IF (I .EQ. NV) THEN
            ZNEX = 1E+30
         ELSE
            ZNEX = VAL(I+1)
         ENDIF
         NCOL = NCOLS(I + 1)
         IF (ZMIN .LE. ZNOW .AND. ZMAX .GE. ZNOW .OR.         &
             ZMIN .LE. ZNEX .AND. ZMAX .GE. ZNEX    ) THEN
            IH    = 1
            DO 30 J1 = 1,3
               J2   = J1 + 1
               IF (J1 .EQ. 3) J2 = 1
               IF (Z(J1) .LT. ZNOW) THEN
                  IF (Z(J2) .GT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE IF (Z(J1) .GT. ZNEX) THEN
                  IF (Z(J2) .LT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE
                  XH(IH) = X(J1)
                  YH(IH) = Y(J1)
                  IH     = IH + 1
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ELSE IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ENDIF
   30       CONTINUE

            NUM = IH - 1
            IF (NUM .GE. 3) THEN
               CALL PFILLER(XH,YH,NUM,NCOL,NCOL)
            ELSE IF (NUM .NE. 0) THEN
!              CALL OKAY(1)
            ENDIF
         ELSE IF (ZMIN .GE. ZNOW .AND. ZMAX .LE. ZNEX) THEN
            CALL PFILLER(X,Y,3,NCOL,NCOL)
         ENDIF
   20  CONTINUE
      ENDIF

      CALL TOPIX(X(1),Y(1),NX1,NY1)
      CALL TOPIX(X(3),Y(3),NX3,NY3)
      NPICS = ABS(NX1-NX3) + ABS(NY1-NY3)
      IF (NCOLR .EQ. 0) THEN
         IF (NPICS .GE. 5) THEN
            CALL SETCOL(NCOLR)
            CALL PTABS(X(1),Y(1))
         ENDIF
      ELSE
         IF (NPICS .GE. 5) THEN
            NUM   = 3
            CALL POLYGON(X,Y,NUM,NCOLR)
         ELSE
            CALL SETCOL(NCOLR)
            CALL PTABS(X(1),Y(1))
         ENDIF
      ENDIF
      RETURN
      END
