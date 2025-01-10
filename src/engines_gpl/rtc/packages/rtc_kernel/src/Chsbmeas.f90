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

      Function CHSBMEAS (IDEBUG, IOUT1)  result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                 Date: June 1997
! *********************************************************************
! *** Last update: June 1997         By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Carry out Sobek measures
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! ***  RetVal = Return value; 0=ok, >0 = foutcode
! *********************************************************************

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
      Use Readlib
!
      implicit none

      integer :: RetVal
!
      LOGICAL CHKTRUE, CheckMissingValue
      INTEGER IDEBUG, IOUT1, IPRIOR, IMEAS, IX, IDUMMY, I, IV
      Double Precision RVAL, RVAL2, RVAL3, RHELP
!
      RetVal = 0
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Chsbmeas')
!
! *********************************************************************
! *** Carry out Sobek measures
! *********************************************************************
!
      DO IPRIOR = LOWSPRI, 1, -1
         DO IMEAS=1,NSMEAS
           IF (MEASPR(IMEAS) .EQ. IPRIOR) THEN
             IF (MEASTY(IMEAS) .LE. 4) THEN
               RVAL = DCVVAL(IXMSBP(IMEAS),1)
               IF (MEASTY(IMEAS) .NE. 2) THEN
! type 1: check value and setpoint numerical input
! type 3: check value from other decision parameter, setpoint numeric
! type 4: check value and setpoint from other decision parameters
                  IF (MEASTY(IMEAS) .EQ. 3) THEN
                     RVAL2 = DCVVAL(IXMSCP(IMEAS),1)
                     MEASCV(IMEAS) = RVAL2
                  ELSEIF (MEASTY(IMEAS) .EQ. 4) THEN
                     RVAL2 = DCVVAL(IXMSCP(IMEAS),1)
                     RVAL3 = DCVVAL(IXMSSP(IMEAS),1)
                     MEASCV(IMEAS) = RVAL2
                     MEASSP(IMEAS) = RVAL3
                  ENDIF
                  IF (MEASCH(IMEAS) .EQ. '<') THEN
                     CHKTRUE = (RVAL .LT. MEASCV(IMEAS))
                  ELSEIF (MEASCH(IMEAS) .EQ. '=') THEN
                     CHKTRUE = (ABS (RVAL-MEASCV(IMEAS)) .LT. .0000001)
                  ELSEIF (MEASCH(IMEAS) .EQ. '>') THEN
                     CHKTRUE = (RVAL .GT. MEASCV(IMEAS))
                  ELSE
                     call write_error_message_rtc (920,0, MEASCH(IMEAS), ' Sobek-measure file', IOUT1)
                     RetVal = 920
                     Return
                  ENDIF
                  MSB_ON (IMEAS) = CHKTRUE .and. CheckMissingValue(MeasSp(imeas), MeasMissingValue(imeas))
                  if (idebug .ne. 0) write(idebug,*) ' meassp, measmissingval, Msb_on', &
                                                       MeasSp(imeas), MeasMissingValue(imeas), Msb_On(Imeas)
                  IX = IXMSSB(IMEAS)
                  IF (MSB_ON(imeas)) MSSBST(IX) = MEASSP(IMEAS)
                  if (idebug .ne. 0) write(idebug,*) ' mssbst(ix)',mssbst(ix)
               ELSEIF (MEASTY(IMEAS) .EQ. 2) THEN
! type 2: check value and setpoint arrays; setpoint computed by interpolation
                  MSB_ON (IMEAS) = .TRUE.
                  IX = IXMSSB(IMEAS)
                  IDUMMY = 1
                  CALL INTERP_double (MEASNV(IMEAS), MEASNCV(1,IMEAS),MEASNSP(1,IMEAS), RVAL,RHELP,IDUMMY)
                  MEASSP(IMEAS) = RHELP
                  MSB_ON (IMEAS) =  CheckMissingValue(MeasSp(imeas), MeasMissingValue(imeas))
                  if (MSB_ON (IMEAS)) MSSBST(IX) = RHELP
                  if (idebug .ne. 0) write(idebug,*) ' meassp, measmissingval, Msb_on', &
                                                       MeasSp(imeas), MeasMissingValue(imeas), Msb_On(Imeas)
                  if (idebug .ne. 0) write(idebug,*) ' mssbst(ix)',mssbst(ix)
               ENDIF
             ELSEIF (MEASTY(IMEAS) .LE. 8) THEN
! type 5-8: check values of all decision variables related to this measure
! type 5: check nv decision vars. with nv check values; if all checks ok set to setpoint value
! type 6: check nv decision vars. with nv check values; if all checks ok set to setpoint parameter
! type 7: check nv decision vars. with nv check parameters; if all ok set to setpoint value
! type 8: check nv decision vars. with nv check parameters; if all ok  set to setpoint parameter
               DO IV = 1, MEASNV(IMEAS)
                   RVAL = DCVVAL (IXMSNBP(IV,IMEAS),1)
                   IF (MEASTY(IMEAS) .GE. 7) THEN
                     RVAL2 = DCVVAL(IXMSNCP(IV,IMEAS),1)
                     MEASNCV(IV,IMEAS) = RVAL2
                   ENDIF
                   IF (MEASTY(IMEAS) .EQ. 6 .OR. MEASTY(IMEAS) .EQ. 8) THEN
                     RVAL3 = DCVVAL(IXMSSP(IMEAS),1)
                     MEASSP(IMEAS) = RVAL3
                   ENDIF
                   IF (MEASNCH(IV,IMEAS) .EQ. '<') THEN
                      CHKTRUE = (RVAL .LT. MEASNCV(IV,IMEAS))
                   ELSEIF (MEASNCH(IV,IMEAS) .EQ. '=') THEN
                      CHKTRUE = (ABS(RVAL-MEASNCV(IV,IMEAS)) .LT. .0000001)
                   ELSEIF (MEASNCH(IV,IMEAS) .EQ. '>') THEN
                      CHKTRUE = (RVAL .GT. MEASNCV(IV,IMEAS))
                   ELSE
                      call write_error_message_rtc (920,0, MEASNCH(IV,IMEAS), ' Sobek-measure file', IOUT1)
                      RetVal = 920
                      Return
                   ENDIF
                   IF (.NOT. CHKTRUE) GOTO 501
               ENDDO
  501          MSB_ON (IMEAS) = CHKTRUE .and. CheckMissingValue(MeasSp(imeas), MeasMissingValue(imeas))
               IX = IXMSSB(IMEAS)
               IF (MSB_ON(Imeas)) MSSBST(IX) = MEASSP(IMEAS)
               if (idebug .ne. 0) write(idebug,*) ' meassp, measmissingval, Msb_on', &
                                                    MeasSp(imeas), MeasMissingValue(imeas), Msb_On(Imeas)
               if (idebug .ne. 0) write(idebug,*) ' mssbst(ix)',mssbst(ix)
             ELSEIF (MEASTY(IMEAS) .EQ. 9) THEN
! type 9: Matlab measure is active is decision parameter value not equal to -999.99
!               if active, setpoint is equal to value of decision parameter
!                 CHKTRUE = (RVAL .GT. -999 .or. RVAL .LT. -1000)
! verzoek PJO: als Matlab -999 dan dit ook doorgeven aan Sobek; maatregel dus altijd actief
               RVAL = DCVVAL(IXMSBP(IMEAS),1)
               CHKTRUE = .TRUE.
               MSB_ON (IMEAS) = CHKTRUE .and. CheckMissingValue(RVal, MeasMissingValue(imeas))
               IX = IXMSSB(IMEAS)
               IF (MSB_ON(Imeas)) MSSBST(IX) = RVAL
               if (idebug .ne. 0) write(idebug,*) ' Rval meassp, measmissingval, Msb_on', &
                                                    Rval, MeasMissingValue(imeas), Msb_On(Imeas)
               if (idebug .ne. 0) write(idebug,*) ' mssbst(ix)',mssbst(ix)
             ELSEIF (MEASTY(IMEAS) .EQ. 10) THEN
! type 10: no check parameters, set to setpoint parameter
               CHKTRUE = .TRUE.
               IX = IXMSSB(IMEAS)
               RVAL3 = DCVVAL(IXMSSP(IMEAS),1)
               MEASSP(IMEAS) = RVAL3
               MSB_ON (IMEAS) = CHKTRUE .and. CheckMissingValue(MeasSp(iMeas), MeasMissingValue(imeas))
               IF (MSB_ON(Imeas)) MSSBST(IX) = MEASSP(IMEAS)
               if (idebug .ne. 0) write(idebug,*) ' meassp, measmissingval, Msb_on', &
                                                    MeasSp(imeas), MeasMissingValue(imeas), Msb_On(Imeas)
               if (idebug .ne. 0) write(idebug,*) ' mssbst(ix)',mssbst(ix)
             ELSEIF (MEASTY(IMEAS) .EQ. 11) THEN
! type 11: DLL measure is active is decision parameter value not equal to -999.99
!               if active, setpoint is equal to value of decision parameter
!                 CHKTRUE = (RVAL .GT. -999 .or. RVAL .LT. -1000)
               RVAL = DCVVAL(IXMSBP(IMEAS),1)
               CHKTRUE = .TRUE.
               MSB_ON (IMEAS) = CHKTRUE .and. CheckMissingValue(RVal, MeasMissingValue(imeas))
               IX = IXMSSB(IMEAS)
               IF (MSB_ON(Imeas)) MSSBST(IX) = RVAL
               if (idebug .ne. 0) write(idebug,*) ' Rval meassp, measmissingval, Msb_on', &
                                                    Rval, MeasMissingValue(imeas), Msb_On(Imeas)
               if (idebug .ne. 0) write(idebug,*) ' mssbst(ix)',mssbst(ix)
             ELSEIF (MEASTY(IMEAS) .EQ. 12) THEN
! via Exe (TCN):
! type 12: Exe measure is active is decision parameter value not equal to -999.99
!               if active, setpoint is equal to value of decision parameter
!             CHKTRUE = (RVAL .GT. -999 .or. RVAL .LT. -1000)

               RVAL = DCVVAL(IXMSBP(IMEAS),1)
               if (Rval .le. -998.99) RVAL = InitSp(IMEAS)   ! als geen TCN data doorgegeven, dan default (initial) value
               CHKTRUE = .TRUE.
               MSB_ON (IMEAS) = CHKTRUE .and. CheckMissingValue(RVal, MeasMissingValue(imeas))
               IX = IXMSSB(IMEAS)
               IF (MSB_ON(Imeas)) MSSBST(IX) = RVAL
               if (idebug .ne. 0) write(idebug,*) ' Rval meassp, measmissingval, Msb_on', &
                                                    Rval, MeasMissingValue(imeas), Msb_On(Imeas)
               if (idebug .ne. 0) write(idebug,*) ' mssbst(ix)',mssbst(ix)
             ENDIF

             IF (IDEBUG .GT. 0) THEN
                  WRITE (IDEBUG,*) ' Measure  ',MEASID(IMEAS)(1:len_trim(MeasId(imeas))), IMEAS
                  WRITE (IDEBUG,*) ' Beslispar',MEASBP(IMEAS)(1:len_trim(MeasBp(imeas)))
                  WRITE (IDEBUG,*) '   index  ',IXMSBP(IMEAS)
                  WRITE (IDEBUG,*) '   value  ',DCVVAL(IXMSBP(IMEAS),1)
                  WRITE (IDEBUG,*) ' Priority ',MEASPR(IMEAS)

                IF (MEASTY(IMEAS) .EQ. 2) THEN
                    WRITE (IDEBUG,*) ' Check values', (MEASNCV(I,IMEAS), I=1,MEASNV(IMEAS))
                    WRITE (IDEBUG,*) ' Setpt values', (MEASNSP(I,IMEAS), I=1,MEASNV(IMEAS))
                  ELSEIF (MEASTY(IMEAS) .LE. 4) THEN
                    IF (MEASTY(IMEAS) .EQ. 4) WRITE (IDEBUG,*) ' Check par',MEASCP(IMEAS)
                    WRITE (IDEBUG,*) ' Check val',MEASCV(IMEAS)
                    WRITE (IDEBUG,*) ' Check    ',MEASCH(IMEAS)
                  ELSEIF (MEASTY(IMEAS) .LE. 8) THEN
!                   not all check values may be determined; see label 501
                    WRITE(IDEBUG,*) ' check values used up to index',iv
                    DO IV=1,MEASNV(IMEAS)
                      IF (MEASTY(IMEAS) .GE. 7) &
                           Write (IDEBUG,*) ' Check par',MEASNCP(IV,IMEAS)(1:len_trim(MeasNcp(iv,imeas)))
                      WRITE (IDEBUG,*) ' Check val',MEASNCV(IV,IMEAS)
                      WRITE (IDEBUG,*) ' Check    ',MEASNCH(IV,IMEAS)(1:len_trim(MeasNch(Iv,Imeas)))
                    ENDDO
                  ELSEIF (MEASTY(IMEAS) .EQ. 9) THEN
                    WRITE(IDEBUG,*) ' Matlab measure '
                  ELSEIF (MEASTY(IMEAS) .EQ. 10) THEN
                    WRITE(IDEBUG,*) ' No check, setpoint from parameter measure '
                  ELSEIF (MEASTY(IMEAS) .EQ. 11) THEN
                    WRITE(IDEBUG,*) ' DLL measure '
               ENDIF

               WRITE (IDEBUG,*) ' Set param',MEASCSP(IMEAS)(1:len_trim(MeasCsp(Imeas)))
               WRITE (IDEBUG,*) ' Setpoint ',MEASSP(IMEAS)
               WRITE (IDEBUG,*) ' Set (T/F)',MSB_ON(IMEAS)
               WRITE (IDEBUG,*) '   index2 ',IXMSSB(IMEAS)
               WRITE (IDEBUG,*) '   value2 ',MSSBST(IXMSSB(IMEAS))
             ENDIF
           ENDIF
         ENDDO
      ENDDO

! *********************************************************************
! ***
! *********************************************************************

      RETURN
      END Function ChSbMeas


      Function CheckMissingValue (Value, MissingValue)  result (retval)

!     function returns true if value is not missing value
!                      false if value is missing value

      implicit none

      logical          ::  retval
      double precision ::  Value, MissingValue

      retval = .true.
      if (Value .ge. MissingValue-0.0009 .and. Value .le. MissingValue + 0.0009) retval=.false.

      RETURN
      END Function CheckMissingValue

