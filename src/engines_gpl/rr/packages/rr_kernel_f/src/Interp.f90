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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $
! Added double precision version


      SUBROUTINE RR_INTERP (NN,X,Y,XX,YY,INDINT)
! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RIBASIM version 3.2.                  Date: March 1991
! *** Module  :  SIMPROC
! ***            Simulation module river basin simulation model
! *********************************************************************
! *** Last update:  March 1991       By : Geert Prinsen
! *********************************************************************
! ***       1986: Last modified for BTA-155 project (Wil van der Krogt)
! *** Febr. 1990: Conversion to MS-Fortran 4.10
! *** March 1991: Adding comments etc.
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    Interpolation subroutine, for two-dimensional arrays.
! ***    Frequently called by reservoir routines (CMPRSV etc.)
! ***    power routines reservoirs/run-of-the-rivers (CMPRSV/CMPROR)
! ***    and diversion/bifurcation routines (CMPDIV/CMPBIF).
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  NN     = length of array
! ***  X      = first number in first array
! ***  Y      = first number insecond array
! ***  XX     = specified number, interpolating in first array
! ***  YY     = corresponding number from interpolating in second array
! ***           (YY is output from this routine INTERP)
! ***  INDINT = position in array last used during interpolation.
! **********************************************************************
! ***                LINEAR INTERPOLATION ROUTINE
! *********************************************************************
!
      use Conf_fil, only : Conffil_get_iout1

      implicit none

      Integer NN, N, i, i1, i2, iNdInt, indx, intrvl, iout1
      Real    X, Y, xx, yy
      DIMENSION X(NN),Y(NN)
      DOUBLE PRECISION A, B, DYY, XXX, YYY

      Iout1 = Conffil_get_iout1()
! check on increasing array X, only message if value to be found in critical range
      DO I1=1,NN-1
         IF (X(I1+1) - X(I1) .LE. .0D0 .and. XX .ge. X(I1)) then
             Write(Iout1,*) ' RR_Interp called with non-increasing array X at index i1=',I1
             Write(Iout1,*) ' X(I1+1), X(I1): ', X(i1+1), X(i1)
             Write(Iout1,*) ' look for', XX, ' in array X'
             Write(Iout1,*) ' This may give unexpected results'
             Write(Iout1,*) ' RR_Interp', NN
             Write(Iout1,*) ' nn', NN
!            Write(*,*) ' array X ', (X(i),i=1,min(132,NN))
!            Write(*,*) ' array Y ', (Y(i),i=1,min(132,NN))
             Goto 99
         endif
      Enddo
   99 CONTINUE
! end check

      N = NN
      IF (N .GT. 1) THEN
         DO 300 I=N,2,-1
            IF (X(I) - X(I-1) .GT. .00001) GOTO 301
  300    CONTINUE
         I = 1
  301    CONTINUE
         N      = MIN0 (N, I)
         INDINT = MIN0 (N, INDINT)
      ENDIF
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

      IF (N .EQ. 2) THEN
        INDINT = 1
        GOTO 800
      ENDIF

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
         IF (DABS (A - B) .LT. 1.D-5) A = B + 1.D-5
         I1  = INDINT + 1
         DYY = (DBLE(Y(I1)) - DBLE (Y(INDINT))) / (A - B)
         YYY = DBLE (Y (INDINT)) + (XXX - DBLE (X (INDINT))) * DYY
  900 CONTINUE
      YY = SNGL (YYY)
      RETURN
    END subroutine RR_Interp


      SUBROUTINE RR_D_INTERP (NN,X,Y,XX,YY,INDINT)
! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RIBASIM version 3.2.                  Date: March 1991
! *** Module  :  SIMPROC
! ***            Simulation module river basin simulation model
! *********************************************************************
! *** Last update:  March 1991       By : Geert Prinsen
! *********************************************************************
! ***       1986: Last modified for BTA-155 project (Wil van der Krogt)
! *** Febr. 1990: Conversion to MS-Fortran 4.10
! *** March 1991: Adding comments etc.
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    Interpolation subroutine, for two-dimensional arrays.
! ***    Frequently called by reservoir routines (CMPRSV etc.)
! ***    power routines reservoirs/run-of-the-rivers (CMPRSV/CMPROR)
! ***    and diversion/bifurcation routines (CMPDIV/CMPBIF).
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  NN     = length of array
! ***  X      = first number in first array
! ***  Y      = first number insecond array
! ***  XX     = specified number, interpolating in first array
! ***  YY     = corresponding number from interpolating in second array
! ***           (YY is output from this routine INTERP)
! ***  INDINT = position in array last used during interpolation.
! **********************************************************************
! ***                LINEAR INTERPOLATION ROUTINE
! *********************************************************************
!
      use Conf_fil, only : Conffil_get_iout1
      implicit none

      Integer NN, N, i, i1, i2, iNdInt, indx, intrvl, iout1
      Double precision  X, Y, xx, yy
      DIMENSION X(NN),Y(NN)
      DOUBLE PRECISION A, B, DYY, XXX, YYY

      Iout1 = Conffil_get_iout1()
! check on increasing array X
      DO I1=1,NN-1
         IF (X(I1+1) - X(I1) .LE. .0D0 .and. XX .ge. X(I1)) then
             Write(Iout1,*) ' RR_Interp called with non-increasing array X at index i1=',i1
             Write(Iout1,*) ' X(I1+1), X(I1): ', X(i1+1), X(i1)
             Write(Iout1,*) ' look for', XX, ' in array X'
             Write(Iout1,*) ' This may give unexpected results'
             Write(Iout1,*) ' RR_D_Interp', NN
             Write(Iout1,*) ' nn', NN
!            Write(*,*) ' array X ', (X(i),i=1,min(132,NN))
!            Write(*,*) ' array Y ', (Y(i),i=1,min(132,NN))
             Goto 99
         endif
      Enddo
   99 CONTINUE
! end check

      N = NN
      IF (N .GT. 1) THEN
         DO 300 I=N,2,-1
            IF (X(I) - X(I-1) .GT. .000001D0) GOTO 301
  300    CONTINUE
         I = 1
  301    CONTINUE
         N      = MIN0 (N, I)
         INDINT = MIN0 (N, INDINT)
      ENDIF
      XXX = XX
      YYY = 0.D0
      DYY = 0.D0
      IF (N.LE.0) GOTO 900
!
! *** N GREATER THAN 0
!
      IF (N.EQ.1) THEN
         YYY = Y(1)
         GOTO 900
      ENDIF

      IF (N .EQ. 2) THEN
        INDINT = 1
        GOTO 800
      ENDIF

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
         IF (DABS (A - B) .LT. 1.D-6) A = B + 1.D-6
         I1  = INDINT + 1
         DYY = (DBLE(Y(I1)) - DBLE (Y(INDINT))) / (A - B)
         YYY = DBLE (Y (INDINT)) + (XXX - DBLE (X (INDINT))) * DYY
  900 CONTINUE
      YY = YYY

!     Write(*,*) ' found', YYY, ' from array Y'

      RETURN
    END subroutine RR_D_Interp


      SUBROUTINE NAM_RR_D_INTERP (NN,X,Y,XX,YY,INDINT)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RIBASIM version 3.2.                  Date: March 1991
! *** Module  :  SIMPROC
! ***            Simulation module river basin simulation model
! *********************************************************************
! *** Last update:  March 1991       By : Geert Prinsen
! *********************************************************************
! ***       1986: Last modified for BTA-155 project (Wil van der Krogt)
! *** Febr. 1990: Conversion to MS-Fortran 4.10
! *** March 1991: Adding comments etc.
! ***             NAM interpolation only; no extrapolation
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***    Interpolation subroutine, for two-dimensional arrays.
! ***    Frequently called by reservoir routines (CMPRSV etc.)
! ***    power routines reservoirs/run-of-the-rivers (CMPRSV/CMPROR)
! ***    and diversion/bifurcation routines (CMPDIV/CMPBIF).
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  NN     = length of array
! ***  X      = first number in first array
! ***  Y      = first number insecond array
! ***  XX     = specified number, interpolating in first array
! ***  YY     = corresponding number from interpolating in second array
! ***           (YY is output from this routine INTERP)
! ***  INDINT = position in array last used during interpolation.
! **********************************************************************
! ***                LINEAR INTERPOLATION ROUTINE
! *********************************************************************
!
      use Conf_fil, only : Conffil_get_iout1
      implicit none

      Integer NN, N, i, i1, i2, iNdInt, indx, intrvl, iout1
      Double precision  X, Y, xx, yy
      DIMENSION X(NN),Y(NN)
      DOUBLE PRECISION A, B, DYY, XXX, YYY

      Iout1 = Conffil_get_iout1()
! check on increasing array X
      DO I1=1,NN-1
         IF (X(I1+1) - X(I1) .LE. .0D0 .and. XX .ge. X(I1)) then
             Write(Iout1,*) ' RR_Interp called with non-increasing array X at index i1=',i1
             Write(Iout1,*) ' X(I1+1), X(I1): ', X(i1+1), X(i1)
             Write(Iout1,*) ' look for', XX, ' in array X'
             Write(Iout1,*) ' This may give unexpected results'
             Write(Iout1,*) ' NAM_RR_D_Interp', NN
             Write(Iout1,*) ' nn', NN
!            Write(*,*) ' array X ', (X(i),i=1,min(132,NN))
!            Write(*,*) ' array Y ', (Y(i),i=1,min(132,NN))
             Goto 99
         endif
      Enddo
   99 CONTINUE
! end check

      N = NN
      IF (N .GT. 1) THEN
         DO 300 I=N,2,-1
            IF (X(I) - X(I-1) .GT. .000001D0) GOTO 301
  300    CONTINUE
         I = 1
  301    CONTINUE
         N      = MIN0 (N, I)
         INDINT = MIN0 (N, INDINT)
      ENDIF
      XXX = XX
      YYY = 0.D0
      DYY = 0.D0
      IF (N.LE.0) GOTO 900
!
! *** N GREATER THAN 0
!
      IF (N.EQ.1) THEN
         YYY = Y(1)
         GOTO 900
      ENDIF

      IF (N .EQ. 2) THEN
        INDINT = 1
        GOTO 800
      ENDIF

! extra lines securing no extrapolation
      IF (XXX .ge. X(N)) THEN
        YYY = Y(N)
        GOTO 900
      ENDIF
      IF (XXX .le. X(1)) THEN
        YYY = Y(1)
        GOTO 900
      ENDIF
! end

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
         IF (DABS (A - B) .LT. 1.D-6) A = B + 1.D-6
         I1  = INDINT + 1
         DYY = (DBLE(Y(I1)) - DBLE (Y(INDINT))) / (A - B)
         YYY = DBLE (Y (INDINT)) + (XXX - DBLE (X (INDINT))) * DYY
  900 CONTINUE
      YY = YYY

!     Write(*,*) ' found', YYY, ' from array Y'

      RETURN
    END subroutine NAM_RR_D_Interp

