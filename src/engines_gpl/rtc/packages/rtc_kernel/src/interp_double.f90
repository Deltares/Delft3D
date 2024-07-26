!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

      SUBROUTINE INTERP_Double (NN,X,Y,XX,YY,INDINT)
!
!  August 2000: In RTC, only Double Precision arrays used
!  Instead of Dimension and real, use Double Precision
!  Do not convert output to single precision
!
!

! *********************************************************************
! *** Program :  RIBASIM
! *** INPUT SHELL FOR RIVER BASIN SIMULATION MODEL
! *********************************************************************
! *** Last update:  March 1991       By : Geert Prinsen
! *********************************************************************
! ***  1986-1988 :  Last modified for BTA-155, by Wil van der Krogt
! ***  March 1991:  Additional comments
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    Interpolation subroutine, for two-dimensional array.
! ***    Same as routine INTERP used in PREPROC and SIMPROC modules.
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  NN     = length of array
! ***  X      = first number in first array
! ***  Y      = first number insecond array
! ***  XX     = specified number, interpolating in first array
! ***  YY     = corresponding number from interpolating in second array
! ***           (YY is output from this routine INTERP)
! ***  INDINT = position in array.
! **********************************************************************
! ***                LINEAR INTERPOLATION ROUTINE
! *********************************************************************

      implicit none
      
      INTEGER          I, NN, INDINT, N, INDX, I1, I2, INTRVL
      Double Precision X(NN), Y(NN), XX, YY
      DOUBLE PRECISION A, B, DYY, XXX, YYY, DY

! *** TEST X - VALUES ON EQUALITY IN ORDER TO REDUCE N AND INDINT

      N = NN
      IF (N .GT. 1) THEN
         DO 300 I=N,2,-1
            IF (X(I) - X(I-1) .GT. .001) GOTO 301
  300    CONTINUE
         I = 1
  301    CONTINUE
         N      = MIN0 (N, I)
         INDINT = MIN0 (N, INDINT)
      ENDIF
!
      XXX = DBLE (XX)
      YYY = 0.D0
      DYY = 0.D0
      IF (N.LE.0) GOTO 900
!
! *** N GREATER THAN 0
!
      IF (N.EQ.1) THEN
         YYY = DBLE(Y(1))
         GOTO 900
      ENDIF
!
      IF (N .EQ. 2) THEN
        INDINT = 1
        GOTO 800
      ENDIF
!
      INDINT = MIN0 (INDINT,N-1)
      INDINT = MAX0 (INDINT,1)
      INDX   = MIN0 (INDINT+1, N-1)
      IF (XXX .GT. DBLE  (X (INDX))) GOTO 200
         INDINT = 1
         IF (INDX .EQ. 2) GOTO 180
            I1 = 1
            I2 = INDX - 2
            DO 110 I=I1,I2
               INTRVL = INDX - I
               INDINT = INTRVL
               IF (XXX .GE.  DBLE (X(INTRVL))) GOTO 180
  110       CONTINUE
            INDINT = 1
  180    CONTINUE
         GOTO 800
  200    CONTINUE
         INDINT = N - 1
         IF (INDX .EQ. N-1) GOTO 280
            I1 = INDX + 1
            I2 = N - 1
            DO 210 INTRVL=I1,I2
               INDINT = INTRVL - 1
               IF (XXX .LT.  DBLE (X(INTRVL))) GOTO 280
  210       CONTINUE
            INDINT = N - 1
  280    CONTINUE
  800    CONTINUE
         A = DBLE (X (INDINT+1))
         B = DBLE (X (INDINT))
         IF (DABS (A - B) .LT. 1.D-3) A = B + 1.D-3
         I1  = INDINT + 1
         DYY = (DBLE(Y(I1)) - DBLE (Y(INDINT))) / (A - B)
         YYY = DBLE (Y (INDINT)) + (XXX - DBLE (X (INDINT))) * DYY
  900 CONTINUE
! August 2000: output Double precision
!     YY = SNGL (YYY)
!     DY = SNGL (DYY)
      YY = YYY
      DY = DYY

      RETURN
      END Subroutine Interp_Double
