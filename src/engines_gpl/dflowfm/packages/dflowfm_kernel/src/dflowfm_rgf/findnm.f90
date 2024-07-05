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

      SUBROUTINE FINDNM( XL, YL, X, Y, mmax, nmax, MC, NC, INSIDE, MV, NV, IN, JN, wf )
      use m_missing
      use geometry_module, only: pinpok
      implicit none

      integer :: mmax, nmax, mc, nc, inside, mv, nv, in, jn
      double precision :: X(MMAX,NMAX),Y(MMAX,NMAX),XX(4),YY(4),XK(3),YK(3)
      double precision :: xl, yl, wf(4)

      integer :: ishot, i, j, mz, nz, m1, m2, n1, n2, insidet, mvol, nvol, i1, i2, ier
      double precision :: dx, dy, r, rmin, xxc, yyc

      DATA MVOL /0/, NVOL /0/
      IF (MC .EQ. 0 .OR. NC .EQ. 0) RETURN
      ISHOT = 0
      RMIN  = 99d+20

     5 CONTINUE
      MV = 0
      NV = 0
      IF (MVOL .NE. 0) THEN
         MZ = MVOL
         NZ = NVOL
      ELSE
         do I = 1,MC
            do J = 1,NC
               IF (X(I,J) .NE. XYMIS) THEN
                  DX = XL - X(I,J)
                  DY = YL - Y(I,J)
                  R  = DX*DX + DY*DY
                  IF (R .LT. RMIN) THEN
                     RMIN = R
                     MZ   = I
                     NZ   = J
                  ENDIF
               ENDIF
            end do
         end do
      ENDIF

      M1     = MAX(1,MZ-2)
      N1     = MAX(1,NZ-2)
      M2     = MIN(MC-1 ,MZ+1)
      N2     = MIN(NC-1 ,NZ+1)
      INSIDE = 0
      MVOL   = 0
      NVOL   = 0
      DO I = M1,M2
         DO J = N1,N2
            XX(1) = X(I,J)
            XX(2) = X(I+1,J)
            XX(3) = X(I+1,J+1)
            XX(4) = X(I,J+1)
            YY(1) = Y(I,J)
            YY(2) = Y(I+1,J)
            YY(3) = Y(I+1,J+1)
            YY(4) = Y(I,J+1)
            IF (XX(1) .NE. XYMIS .AND. XX(2) .NE. XYMIS .AND.   &
                XX(3) .NE. XYMIS .AND. XX(4) .NE. XYMIS) THEN
               CALL PINPOK(XL, YL, 4, XX, YY, INSIDE, jins, dmiss)
               IF (INSIDE .EQ. 1) THEN

                  call bilin5( xx, yy, xL, yL ,wf , ier)

                  MVOL = I
                  NVOL = J
                  MV   = I
                  NV   = J
!                 Bepaal kwadrant
                  XXC  = ( XX(1) + XX(2) + XX(3) + XX(4) )/4
                  YYC  = ( YY(1) + YY(2) + YY(3) + YY(4) )/4
                  IN   = 0
                  JN   = 0
                  DO I1 = 1,4
                     I2    = MOD(I1,4) + 1
                     XK(1) = XX(I1)
                     YK(1) = YY(I1)
                     XK(2) = XX(I2)
                     YK(2) = YY(I2)
                     XK(3) = XXC
                     YK(3) = YYC
                     CALL PINPOK(XL, YL, 3, XK, YK, INSIDET, jins, dmiss)
                     IF (INSIDET .EQ. 1) THEN
                        IF ( I1 .EQ. 1) JN = -1
                        IF ( I1 .EQ. 2) IN =  1
                        IF ( I1 .EQ. 3) JN =  1
                        IF ( I1 .EQ. 4) IN = -1
                        RETURN
                     ELSE IF (I1 .EQ. 4) THEN
!                       WRITE(MDIA,*) 'NO KWADRANT'
                        RETURN
                     ENDIF
                  end do
               ENDIF
            ENDIF
         end do
      end do
      
!     WRITE(MDIA,*) 'ISHOT', ISHOT, MVOL, NVOL
      IF (ISHOT .EQ. 1) RETURN
      ISHOT = 1
      GOTO 5

      RETURN
      END subroutine findnm
