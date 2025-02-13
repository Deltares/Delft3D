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

      Function RD3BMS (IDEBUG, IN, IOUT1) result(RetVal)

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
! ***   Read measures file Maalstop/3B
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
      Use OtherData
      Use NewTables_rtc
      Use ParseToken_rtc
      Use ReadLib_rtc

      implicit none

      Integer :: RetVal

      INTEGER    NHLP
      PARAMETER (NHLP = 10)

      LOGICAL        ENDFIL, success
      CHARACTER*9999 STRING
      CHARACTER*999  LocationidRR, LocationnameRR, ErrorString
      INTEGER        IDEBUG, IN, IOUT1,Ilen
      INTEGER        IMEAS, IPARA, IECODE, I3LOC, I3B, ILOC
      Integer        TyValue, NaValue, StartSubRecord, EndSubRecord, NrRRMSSubRecords, iSubRecord
      Logical        MeasureInActive

! Additional variables for NewTables_rtc and ParseToken_rtc
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError
      Type (TokenArray) RecordData

      IStart   = 1     ! Scan from token 1
      ScanToTk = 9999  ! Scan up to Token 60
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken_rtc fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken_rtc
!
      RetVal = 0

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Rd3bms')

! *********************************************************************
! *** skip header of file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL,'RTC')
!c    IF (ENDFIL)
!c   *  call write_error_message_rtc (911, 0, 'Rd3bms', ' 3B-Maatregelfile', IOUT1)

! *********************************************************************
! *** read data
! *** format:
! ***   MLST id 'Measure-id' bp 'Beslispar-id' on onval of ofval cn checkon cf checkof mlst
! ***   3BML id '3B-id' ms 'Measure-id' pr iprior 3bml
! November 2004: New format for 3BML records: group all records working on the same structure together
!RRST id '111' nm '' na 0
!  RRMS ty 10 ms 'Measure1' pr 1 rrms
!  RRMS ty 10 ms 'Measure2' pr 1 rrms
!rrst
!RRST id '2' nm '' na 0
!  RRMS ty 10 ms 'Measure3' pr 1 rrms
!  RRMS ty 9 pr 2 rrms
!rrst
!RRST id '3' nm '' na 0
!  RRMS ty 9 pr 1 rrms
!rrst
! ***   met   iprior (integer), onval (real), ofval (real), checkon (character), checkof (character)
! *********************************************************************

      N3MLOC = 0
      N3MEAS = 0
      N3MatLoc = 0
      LOW3PRI= 1
!     ALLOW  = .FALSE.

      DO While (.not. endfil)
19       Continue
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) STRING(1:len_trim(String))

         IF (STRING(1:4) .EQ. 'MLST') THEN

! *********************************************************************
! ***      MLST records
!RRM2.0
!MLST id 'Measure1' bp 'peil_ow' on -5.95 of -6.05 cn '<' cf '>'  mlst
!MLST id 'Measure2' bp 'trend_ow' on -0.01 of 0.00 cn '<' cf '>'  mlst
!MLST id 'Measure3' bp 'trend2' on 0.01 of 0.00 cn '>' cf '<'  mlst
! *********************************************************************
           BackSpace(IN)
           SearchString = 'MLST'
           ReadError = .false.
           Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
              call write_error_message_rtc (974, 0, 'Rd3bMs', ' Unexpected end of file ',IOUT1)
              RetVal = 974
              Return
           Endif
           String = ' '
           Success = GetStringFromBuffer (String)
           if (.not. Success) ReadError = .true.
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) ReadError = .true.
           IF (ReadError) then
              ErrorString = ' Read error during reading RTC measures with ParseToken_rtc for RR module' // STRING(1:len_trim(String))
              call write_error_message_rtc (974,0,'Rd3BMs',ErrorString, IOUT1)
              RetVal = 920
              Return
           Endif

           N3MEAS = N3MEAS+1
! id
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MsId3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Voor Getvar bp ',STRING(1:len_trim(String))
! bp
           if (Getkey ('bp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MsBp3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! on value
           if (Getkey ('on', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150)   MsOn3B(N3Meas)
           else
               ReadError = .true.
           endif
! off value
           if (Getkey ('of', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150)   MsOff3B(N3Meas)
           else
               ReadError = .true.
           endif
! on check
           if (Getkey ('cn', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               OnCh3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! check On and Off checks '<', '=', '>'
           IF ( ONCH3B(N3MEAS) .NE. '<' .and. ONCH3B(N3MEAS) .NE. '=' .and.  ONCH3B(N3MEAS) .NE. '>' ) THEN
              call write_error_message_rtc (920,N3MEAS,MSID3B(N3MEAS),ONCH3B(N3MEAS),IOUT1)
              RetVal = 920
              Return
           ENDIF
! off check
           if (Getkey ('cf', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               OfCh3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           IF ( OFCH3B(N3MEAS) .NE. '<' .and. OFCH3B(N3MEAS) .NE. '=' .and. OFCH3B(N3MEAS) .NE. '>' ) THEN
              call write_error_message_rtc (920,N3MEAS,MSID3B(N3MEAS),OFCH3B(N3MEAS),IOUT1)
              RetVal = 920
              Return
           ENDIF

           IF (IDEBUG .GT. 0) THEN
             WRITE(IDEBUG,*) ' 3B-meas ID ', MSID3B (N3MEAS)(1:len_trim(MsId3B(N3Meas)))
             WRITE(IDEBUG,*) ' Beslispar  ', MSBP3B (N3MEAS)(1:len_trim(MsBP3B(N3Meas)))
             WRITE(IDEBUG,*) ' Check+set on ', ONCH3B (N3MEAS), MSON3B (N3MEAS)
             WRITE(IDEBUG,*) ' Check+set off', OFCH3B (N3MEAS), MSOFF3B(N3MEAS)
           ENDIF
           IF (ReadError) then
              ErrorString = ' Read error during reading RTC measures MLST records for RR module; MLST record =' // STRING(1:len_trim(String))
              call write_error_message_rtc (974,0,'Rd3BMs',ErrorString, IOUT1)
              RetVal = 920
              Return
           Endif

! *********************************************************************
! *** Check existence beslisparameter id in beslisparameter file (BESLISPA.RTC)
! *********************************************************************

           IXMS3P(N3MEAS) = 0
           DO IPARA=1,NPARA
             IF (PARAID(IPARA) .EQ. MSBP3B(N3MEAS)) THEN
                 IXMS3P(N3MEAS) = IPARA
             ENDIF
           ENDDO
           IF (IXMS3P(N3MEAS) .LE. 0) THEN
              WRITE(IOUT1,*) ' Measure number',N3MEAS
              WRITE(IOUT1,*) ' Number of decision parameters',NPARA
              WRITE(IOUT1,*) ' Measure id ',MSBP3B(N3MEAS)(1:len_trim(MsBp3B(N3Meas)))
              WRITE(IOUT1,*) ' Parameter id', (PARAID(IPARA)(1:len_trim(ParaId(IPara))),IPARA=1,NPARA)
              call write_error_message_rtc (919,0, MSBP3B(N3MEAS), ' 3B-measure file', IOUT1)
              RetVal = 919
              Return
           ENDIF

         ELSEIF (STRING(1:4) .EQ. 'RRST') THEN

! *********************************************************************
! ***      former 3BML records
! ***      Replaced by RRST record with subrecords RRMS, also covering previous MATL record
! November 2004: New format
!RRST id '111' nm '' na 0
!  RRMS ty 10 ms 'Measure1' pr 1 rrms
!  RRMS ty 10 ms 'Measure2' pr 1 rrms
!rrst
!RRST id '2' nm '' na 0
!  RRMS ty 10 ms 'Measure3' pr 1 rrms
!  RRMS ty 9 pr 2 rrms
!rrst
!RRST id '3' nm '' na 0
!  RRMS ty 9 pr 1 rrms
!rrst
! *********************************************************************
           BackSpace(IN)
           SearchString = 'RRST'
           ReadError = .false.
! Get full RRST record, structure id and notactive indication
           success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
              call write_error_message_rtc (974, 0, 'Rd3bMs', ' Unexpected end of file ',IOUT1)
              RetVal = 974
              Return
           ENDIF
           String = ' '
           Success = GetStringFromBuffer (String)
           if (.not. Success) ReadError = .true.
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) ReadError = .true.
           N3MLOC = N3MLOC+1
           IStart   = 1     ! Scan from token 1
           if (Getkey ('RRMS', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              StartSubRecord=ReturnIndx
           endif
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               LcId3B(N3MLoc) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! Optional name
           if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               LcName3B(N3mLoc) = RecordData%Token(ReturnIndx+1)
           endif
!          NotActive option: na 0 = active, na 1 = not active
           if (Getkey ('na', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150) NaValue
               MeasureInActive = (naValue .ne. 0)
               If (MeasureInActive)  Goto 19     !if inactive, skip rest of this record
           else
               ReadError = .true.
           endif
! Get RRMS subrecords
! Find the number of RRMS subrecords within the String
         NrRRMSSubRecords = CntStr (' RRMS ', String)
! Now loop over the possible RRMS subrecords
         Do ISubRecord=1,NrRRMSSubRecords
            if (ISubRecord .gt. 1) then
                N3MLOC = N3MLOC+1
                LcId3B(N3MLoc) = LcId3B(N3MLoc-1)
                LcName3B(N3MLoc) = LcName3B(N3MLoc-1)
                IStart = EndSubRecord+1
            Endif
            if (Getkey ('RRMS', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               StartSubRecord = ReturnIndx
            endif
            if (Getkey ('rrms', StartSubRecord+1, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               EndSubRecord = ReturnIndx
            endif
! measure type/priority
            if (Getkey ('ty', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                Read (RecordData%Token(ReturnIndx+1),*,Err=150)   TyValue
            else
                ReadError = .true.
            endif
            If (TyValue .eq. 10) then
               if (Getkey ('ms', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                   LcMs3B(N3MLoc) = RecordData%Token(ReturnIndx+1)
               else
                   ReadError = .true.
               endif
               if (Getkey ('pr', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                   Read (RecordData%Token(ReturnIndx+1),*,Err=150)   LcPr3B(N3MLoc)
               else
                   ReadError = .true.
               endif
               IF (ReadError) then
                  ErrorString = ' Read error during reading RTC measure RRST record ty 10 for RR module; RRST record=' // STRING(1:len_trim(String))
                  call write_error_message_rtc (974,0,'Rd3BMs',ErrorString, IOUT1)
                  RetVal = 974
                  Return
               ENDIF
            Elseif (TyValue .eq. 9) then
!              matlab measure is read later, so therefore now reduce the counter again
               N3mLoc = N3MLoc -1
            Else
               ErrorString = ' Unsupported type of RTC-measure defined after ty keyword in RRST record for RR module; RRST record=' // STRING(1:len_trim(String))
               call write_error_message_rtc (974,0,'Rd3BMs',ErrorString, IOUT1)
               RetVal = 974
               Return
            Endif
!
! Set lowest priority of 3B-measures;
!
            if (N3mLoc .gt. 0) LOW3PRI = MAX (LOW3PRI, LCPR3B(N3MLOC))

            IF (IDEBUG .GT. 0 .and. N3MLoc .gt. 0) THEN
              WRITE(IDEBUG,*) ' N3MLoc        ', N3MLoc
              WRITE(IDEBUG,*) ' 3B-lokatie ID ', LCID3B (N3MLOC)(1:len_trim(LcId3b(N3mLoc)))
              WRITE(IDEBUG,*) ' Maatregel     ', LCMS3B (N3MLOC)(1:len_trim(LcMS3b(N3mLoc)))
              WRITE(IDEBUG,*) ' Prioriteit    ', LCPR3B (N3MLOC)
            ENDIF

            IF (ReadError) then
               call write_error_message_rtc (974,0,'Rd3BMs',' Read error during reading RTC measures RRST records for RR module',IOUT1)
               RetVal = 920
               Return
            Endif

           Enddo    !end of Subrecord RRMS loop
         ENDIF
   21    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      ENDDO

! *********************************************************************
! ***      Matlab measures for RR: always afterwards
! *********************************************************************

      Rewind (In)
      CALL SKPCOM (IN, ENDFIL,'RTC')
      DO While (.not. endfil)
29       Continue
         READ(IN,'(A)',END=31,ERR=150,IOSTAT=IECODE)  STRING
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) STRING(1:len_trim(String))
         IF (STRING(1:4) .EQ. 'RRST') THEN
           BackSpace(IN)
           SearchString = 'RRST'
           ReadError = .false.
           success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
              call write_error_message_rtc (974, 0, 'Rd3bMs', ' Unexpected end of file ',IOUT1)
              RetVal = 974
              Return
           ENDIF
           String = ' '
           Success = GetStringFromBuffer (String)
           if (.not. Success) ReadError = .true.
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) ReadError = .true.
           IStart   = 1     ! Scan from token 1
           if (Getkey ('RRMS', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              StartSubRecord=ReturnIndx
           endif
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               LocationIdRR = RecordData%Token(ReturnIndx+1)
           endif
! Optional name
           if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               LocationNameRR = RecordData%Token(ReturnIndx+1)
           endif
!          NotActive option: na 0 = active, na 1 = not active
           if (Getkey ('na', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150) NaValue
               MeasureInActive = (naValue .ne. 0)
               If (MeasureInActive)  Goto 29     !if inactive, skip rest of this record
           endif
! Get RRMS subrecords
! Find the number of RRMS subrecords within the String
         NrRRMSSubRecords = CntStr (' RRMS ', String)
! Now loop over the possible RRMS subrecords
           Do ISubRecord=1,NrRRMSSubRecords
             if (ISubRecord .gt. 1) IStart = EndSubRecord+1
             if (Getkey ('RRMS', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               StartSubRecord = ReturnIndx
             endif
             if (Getkey ('rrms', StartSubRecord+1, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               EndSubRecord = ReturnIndx
             endif
! measure type/priority
             if (Getkey ('ty', iStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                Read (RecordData%Token(ReturnIndx+1),*,Err=150)   TyValue
             else
                ReadError = .true.
             endif
             If (TyValue .eq. 9) then
!              Matlab measure
               MatUse = .TRUE.
               N3MatLoc = N3MatLoc+1
               LcId3B(N3MLoc+N3MatLoc) = LocationIdRR
               LcName3B(N3MLoc+N3MatLoc) = LocationNameRR
               if (Getkey ('pr', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                   Read (RecordData%Token(ReturnIndx+1),*,Err=150)   LcPr3B(N3MLoc+N3MatLoc)
               else
                   ReadError = .true.
               endif
               IF (ReadError) then
                  ErrorString = ' Read error during reading RTC measure RRST record ty 9 for RR module; RRST record=' // STRING(1:len_trim(String))
                  call write_error_message_rtc (974,0,'Rd3BMs',ErrorString, IOUT1)
                  RetVal = 974
                  Return
               ENDIF
! Set lowest priority of 3B-measures;
               LOW3PRI = MAX (LOW3PRI, LCPR3B(N3MLOC+N3MatLoc))
! Matlab measure: Add 4 decision variables (on low, off low, on high, off high)
               Do IMeas=1,4
                  NPARA = NPARA + 1
                  PARAID (NPARA) = LCID3B (N3MLOC+N3MatLoc)
                  ILen = len_trim(Paraid(Npara))
                  IF (IMeas .eq. 1)  Then
                     ParaId(NPara) = ParaId(NPara)(1:ilen) // '_lowon'
                  ElseIF (IMeas .eq. 2)  Then
                     ParaId(NPara) = ParaId(NPara)(1:ilen) // '_lowoff'
                  ElseIF (IMeas .eq. 3)  Then
                     ParaId(NPara) = ParaId(NPara)(1:ilen) // '_highon'
                  ElseIF (IMeas .eq. 4)  Then
                     ParaId(NPara) = ParaId(NPara)(1:ilen) // '_highoff'
                  Endif
                  PARDIM (NPARA,1) = 0
                  PARDIM (NPARA,2) = 0
                  PARDIM (NPARA,3) = 0
                  PARDIM (NPARA,4) = 0
                  PARDIM (NPARA,5) = 0
                  PARDIM (NPARA,6) = 0
                  PARDIM (NPARA,7) = 0
! add measure as well (coupling measure-decision parameter)
                  MSBP3B (N3MEAS+(N3MatLoc-1)*4 +IMeas) = ParaId(NPara)
                  IXMS3P (N3MEAS+(N3MatLoc-1)*4 +IMeas) = NPara
                  IF (IDEBUG .GT. 0) THEN
                     WRITE(IDEBUG,*) ' ipara ', npara
                     WRITE(IDEBUG,*) ' paraid', paraid(npara)
                     WRITE(IDEBUG,*) ' n3matloc', n3matloc
                     WRITE(IDEBUG,*) ' meas  ', n3Meas+(N3matloc-1)*4 +Imeas
                     WRITE(IDEBUG,*) ' 3B-lokatie ID  ', LCID3B (N3MLOC+N3MatLoc)(1:len_trim(LCID3B(N3MLOC+N3MatLoc)))
                  endif
               Enddo
! Debug
               IF (IDEBUG .GT. 0) THEN
                 WRITE(IDEBUG,*) ' N3Mloc NcMatLoc', N3MLoc,N3MatLoc
                 WRITE(IDEBUG,*) ' N3Mloc+NcMatLoc', N3MLoc+N3Matloc
                 WRITE(IDEBUG,*) ' 3B-lokatie ID  ', LCID3B (N3MLOC+N3MatLoc)(1:len_trim(LCID3B(N3MLOC+N3MatLoc)))
                 WRITE(IDEBUG,*) ' Maatregel      ', LCMS3B (N3MLOC+N3MatLoc)(1:len_trim(LCMS3B(N3MLOC+N3MatLoc)))
                 WRITE(IDEBUG,*) ' Prioriteit     ', LCPR3B (N3MLOC+N3MatLoc)
                 WRITE(IDEBUG,*) ' Index parameter', IxMs3P (N3MLOC+N3MatLoc)
               ENDIF
             Endif
           Enddo
         Endif
   31    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo


! *********************************************************************
! *** Check dimensies
! *********************************************************************

      IF (N3MEAS .GT. N3MES) THEN
         call write_error_message_rtc (913, 0,'Rd3bms',' N3MES 3B measures',IOUT1)
         RetVal = 913
         Return
      ELSEIF (N3MLOC+N3MatLoc .GT. N3LOC) THEN
         call write_error_message_rtc (913, 0,'Rd3bms',' N3LOC 3B lokatie-measures',IOUT1)
         RetVal = 913
         Return
      ELSEIF (N3MATLOC .GT. N3MAT) THEN
         call write_error_message_rtc (913, 0,'Rd3bms',' N3MATLOC 3B Matlab-measures',IOUT1)
         RetVal = 913
         Return
      ENDIF
      If (N3meas .eq. 0) then
          Write(*,*) ' No RR MLST measures defined'
      Endif
      If (N3MLoc .eq. 0) then
          Write(*,*) ' No RR RRMS ty 10 measures defined'
      Endif
      If (N3MatLoc .eq. 0) then
          Write(*,*) ' No RR RRMS Matlab measures defined'
      Endif


! end check
      GOTO 999

! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150 CONTINUE
      call write_error_message_rtc (902, IECODE, 'Rd3bms', ' 3B-Maatregelfile', IOUT1)
      RetVal = 902
      Return

! *********************************************************************
! *** end of file
! *********************************************************************

  999 CONTINUE

! *********************************************************************
! ***  Check existence maatregel id uit 3BML record in MLST record
! *********************************************************************

      DO I3LOC = 1, N3MLOC+N3MatLoc
         IXMS3B(I3LOC) = 0
         DO IMEAS=1,N3MEAS
           IF (LCMS3B(I3LOC) .EQ. MSID3B(IMEAS))  THEN
               IXMS3B(I3LOC) = IMEAS
           ENDIF
         ENDDO
         IF (IXMS3B(I3LOC) .LE. 0 .AND. I3Loc .le. N3MLoc) THEN
!           WRITE(IOUT1,*) ' RR-location index ', I3LOC
!           WRITE(IOUT1,*) ' Measure',N3MEAS
            WRITE(IOUT1,*) ' RR-location-measure id =',LCMS3B(I3LOC)(1:len_trim(LcMs3b(I3Loc)))
            WRITE(IOUT1,*) ' Available RR-Measure ids:'
            Do Imeas=1,N3Meas
               WRITE(IOUT1,*) MSID3B(IMEAS)(1:len_trim(MsId3B(Imeas)))
            Enddo
            call write_error_message_rtc (922,0, LCMS3B(I3LOC),  ' 3B-measure file', IOUT1)
            RetVal = 922
            Return
         ENDIF
      ENDDO

! *********************************************************************
! *** Determine total different number of 3B-id's involved in measures
! ***   (may be considarably less than number of 3B-location measure-id's,
! ***    because different measures can be applied to the same 3B structure)
! *********************************************************************
! *** Determine total different nr. of id's in 3BML and MATL records
! *********************************************************************

      N3BMS  = 0
      DO ILOC = 1,N3MLOC+N3MatLoc
         DO I3B = 1,N3BMS
            IF (LCID3B(ILOC) .EQ. ID3BML(I3B)) THEN
               IXID3B(ILOC) = I3B
               GOTO 998
            ENDIF
         ENDDO
         N3BMS  = N3BMS + 1
         ID3BML(N3BMS) = LCID3B(ILOC)
         Descr3BML(N3BMS) = LCName3B(ILOC)
         IXID3B(ILOC) = N3BMS
!         MS3BST(N3BMS,1) = 0.
!         MS3BST(N3BMS,2) = 0.
!         MS3BST(N3BMS,3) = 0
!         MS3BST(N3BMS,4) = -999.
!         MS3BST(N3BMS,5) = -999.
!         MS3BST(N3BMS,6) = -999.
!         MS3BST(N3BMS,7) = -999.
  998    CONTINUE
         IF (IDEBUG .GT. 0) THEN
           WRITE(IDEBUG,*) ' Measure-location id ', LCID3B(ILOC), ILOC
           WRITE(IDEBUG,*) ' index ms . ', IXMS3B(ILOC)
           WRITE(IDEBUG,*) ' index id . ', IXID3B(ILOC)
           WRITE(IDEBUG,*) ' Uniek id   ', ID3BML(IXID3B(ILOC))
           WRITE(IDEBUG,*) ' MS-status  ', MS3BST(IXID3B(ILOC),1)
         ENDIF
      ENDDO


      RETURN
      END Function Rd3BMs



      Function RD3BMS_OldFormat (IDEBUG, IN, IOUT1) result(RetVal)

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
! ***   Read measures file Maalstop/3B
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
      Use NewTables_rtc
      Use ParseToken_rtc
      Use ReadLib_rtc

      implicit none

      Integer :: RetVal

      INTEGER    NHLP
      PARAMETER (NHLP = 10)

      LOGICAL      ENDFIL, success
      CHARACTER*999 STRING
      INTEGER       IDEBUG, IN, IOUT1,Ilen
      INTEGER       IMEAS, IPARA, IECODE, I3LOC, I3B, ILOC

! Additional variables for NewTables_rtc and ParseToken_rtc
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError
      Type (TokenArray) RecordData

      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 60
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken_rtc fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken_rtc
!
      RetVal = 0

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Rd3bms_oldformat')

! *********************************************************************
! *** skip header of file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL,'RTC')
!c    IF (ENDFIL)
!c   *  call write_error_message_rtc (911, 0, 'Rd3bms_oldformat', ' 3B-Maatregelfile', IOUT1)

! *********************************************************************
! *** read data
! *** format:
! ***   MLST id 'Measure-id' bp 'Beslispar-id' on onval of ofval cn checkon cf checkof mlst
! ***   3BML id '3B-id' ms 'Measure-id' pr iprior 3bml
! ***   met   iprior (integer), onval (real), ofval (real), checkon (character), checkof (character)
! *********************************************************************

      N3MLOC = 0
      N3MEAS = 0
      N3MatLoc = 0
      LOW3PRI= 1
!     ALLOW  = .FALSE.

      DO While (.not. endfil)
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) STRING(1:len_trim(String))

         IF (STRING(1:4) .EQ. 'MLST') THEN

! *********************************************************************
! ***      MLST records
! *********************************************************************
           BackSpace(IN)
           SearchString = 'MLST'
           ReadError = .false.
           Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
              call write_error_message_rtc (974, 0, 'Rd3bMs_oldformat', ' Unexpected end of file ',IOUT1)
              RetVal = 974
              Return
           Endif
           String = ' '
           Success = GetStringFromBuffer (String)
           if (.not. Success) ReadError = .true.
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) ReadError = .true.

           N3MEAS = N3MEAS+1
! id
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MsId3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Voor Getvar bp ',STRING(1:len_trim(String))
! bp
           if (Getkey ('bp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MsBp3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! on value
           if (Getkey ('on', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150)   MsOn3B(N3Meas)
           else
               ReadError = .true.
           endif
! off value
           if (Getkey ('of', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150)   MsOff3B(N3Meas)
           else
               ReadError = .true.
           endif
! on check
           if (Getkey ('cn', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               OnCh3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! check On and Off checks '<', '=', '>'
           IF ( ONCH3B(N3MEAS) .NE. '<' .and. ONCH3B(N3MEAS) .NE. '=' .and.  ONCH3B(N3MEAS) .NE. '>' ) THEN
              call write_error_message_rtc (920,N3MEAS,MSID3B(N3MEAS),ONCH3B(N3MEAS),IOUT1)
              RetVal = 920
              Return
           ENDIF
! off check
           if (Getkey ('cf', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               OfCh3B(N3Meas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           IF ( OFCH3B(N3MEAS) .NE. '<' .and. OFCH3B(N3MEAS) .NE. '=' .and. OFCH3B(N3MEAS) .NE. '>' ) THEN
              call write_error_message_rtc (920,N3MEAS,MSID3B(N3MEAS),OFCH3B(N3MEAS),IOUT1)
              RetVal = 920
              Return
           ENDIF

           IF (IDEBUG .GT. 0) THEN
             WRITE(IDEBUG,*) ' 3B-meas ID ', MSID3B (N3MEAS)(1:len_trim(MsId3B(N3Meas)))
             WRITE(IDEBUG,*) ' Beslispar  ', MSBP3B (N3MEAS)(1:len_trim(MsBP3B(N3Meas)))
             WRITE(IDEBUG,*) ' Check+set on ', ONCH3B (N3MEAS), MSON3B (N3MEAS)
             WRITE(IDEBUG,*) ' Check+set off', OFCH3B (N3MEAS), MSOFF3B(N3MEAS)
           ENDIF
           IF (ReadError) then
              call write_error_message_rtc (974,0,'Rd3BMs_oldformat',' Read error during reading RTC measures MLST records for RR module',IOUT1)
              RetVal = 920
              Return
           Endif

! *********************************************************************
! *** Check existence beslisparameter id in beslisparameter file (BESLISPA.RTC)
! *********************************************************************

           IXMS3P(N3MEAS) = 0
           DO IPARA=1,NPARA
             IF (PARAID(IPARA) .EQ. MSBP3B(N3MEAS)) THEN
                 IXMS3P(N3MEAS) = IPARA
             ENDIF
           ENDDO
           IF (IXMS3P(N3MEAS) .LE. 0) THEN
              WRITE(IOUT1,*) ' Measure number',N3MEAS
              WRITE(IOUT1,*) ' Number of decision parameters',NPARA
              WRITE(IOUT1,*) ' Measure id ',MSBP3B(N3MEAS)(1:len_trim(MsBp3B(N3Meas)))
              WRITE(IOUT1,*) ' Parameter id', (PARAID(IPARA)(1:len_trim(ParaId(IPara))),IPARA=1,NPARA)
              call write_error_message_rtc (919,0, MSBP3B(N3MEAS), ' 3B-measure file', IOUT1)
              RetVal = 919
              Return
           ENDIF

         ELSEIF (STRING(1:4) .EQ. '3BML') THEN

! *********************************************************************
! ***      3BML records
! *********************************************************************
           BackSpace(IN)
           SearchString = '3BML'
           ReadError = .false.
           success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
              call write_error_message_rtc (974, 0, 'Rd3bMs_oldformat', ' Unexpected end of file ',IOUT1)
              RetVal = 974
              Return
           ENDIF
           String = ' '
           Success = GetStringFromBuffer (String)
           if (.not. Success) ReadError = .true.
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) ReadError = .true.
!           If (Idebug .gt. 0) then
!              Write(Idebug,'(A)') String(1:len_trim(String))
!              Write(Idebug,*) ' Results of ParseToken_rtc in Rd3bMs_oldformat '
!              Write(idebug,*) ' Nr  StartPos  Quotes Token '
!              Do i=1,NumberofTokens
!                 write(idebug,'(I3,I5,L,1X,A)') i, RecordData%StartPositionOfToken(i), &
!                                                RecordData%IsEnclosedByQuotes(i), RecordData%Token(i)
!              Enddo
!           Endif
! total number of gates
           N3MLOC = N3MLOC+1
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               LcId3B(N3MLoc) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('ms', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               LcMs3B(N3MLoc) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('pr', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150)   LcPr3B(N3MLoc)
           else
               ReadError = .true.
           endif
           IF (ReadError) then
              call write_error_message_rtc (974,0,'Rd3bMs_oldformat',' Read error during reading RTC measure 3BML record for RR module',IOUT1)
              RetVal = 974
              Return
           ENDIF
!
! Set lowest priority of 3B-measures;
!
           LOW3PRI = MAX (LOW3PRI, LCPR3B(N3MLOC))

           IF (IDEBUG .GT. 0) THEN
             WRITE(IDEBUG,*) ' 3B-lokatie ID ', LCID3B (N3MLOC)(1:len_trim(LcId3b(N3mLoc)))
             WRITE(IDEBUG,*) ' Maatregel     ', LCMS3B (N3MLOC)(1:len_trim(LcMS3b(N3mLoc)))
             WRITE(IDEBUG,*) ' Prioriteit    ', LCPR3B (N3MLOC)
           ENDIF
         ENDIF
   21    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      ENDDO

! *********************************************************************
! ***      Matlab measures for RR: always afterwards
! *********************************************************************

      Rewind (In)
      CALL SKPCOM (IN, ENDFIL,'RTC')
      DO While (.not. endfil)
         READ(IN,'(A)',END=31,ERR=150,IOSTAT=IECODE)  STRING
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) STRING(1:len_trim(String))
         IF (STRING(1:4) .EQ. 'MATL') THEN
           BackSpace(IN)
           SearchString = 'MATL'
           ReadError = .false.
           success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
              call write_error_message_rtc (974, 0, 'Rd3bMs_oldformat', ' Unexpected end of file ',IOUT1)
              RetVal = 974
              Return
           ENDIF
           String = ' '
           Success = GetStringFromBuffer (String)
           if (.not. Success) ReadError = .true.
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) ReadError = .true.

           N3MatLoc = N3MatLoc+1
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               LcId3B(N3MLoc+N3MatLoc) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('pr', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=150)   LcPr3B(N3MLoc+N3MatLoc)
           else
               ReadError = .true.
           endif
! Set lowest priority of 3B-measures;
           LOW3PRI = MAX (LOW3PRI, LCPR3B(N3MLOC+N3MatLoc))
! Matlab measure: Add 4 decision variables (on low, off low, on high, off high)
           Do IMeas=1,4
              NPARA = NPARA + 1
              PARAID (NPARA) = LCID3B (N3MLOC+N3MatLoc)
              ILen = len_trim(Paraid(Npara))
              IF (IMeas .eq. 1)  Then
                 ParaId(NPara) = ParaId(NPara)(1:ilen) // '_lowon'
              ElseIF (IMeas .eq. 2)  Then
                 ParaId(NPara) = ParaId(NPara)(1:ilen) // '_lowoff'
              ElseIF (IMeas .eq. 3)  Then
                 ParaId(NPara) = ParaId(NPara)(1:ilen) // '_highon'
              ElseIF (IMeas .eq. 4)  Then
                 ParaId(NPara) = ParaId(NPara)(1:ilen) // '_highoff'
              Endif
              PARDIM (NPARA,1) = 0
              PARDIM (NPARA,2) = 0
              PARDIM (NPARA,3) = 0
              PARDIM (NPARA,4) = 0
              PARDIM (NPARA,5) = 0
              PARDIM (NPARA,6) = 0
              PARDIM (NPARA,7) = 0
! add measure as well (coupling measure-decision parameter)
              MSBP3B (N3MEAS+(N3MatLoc-1)*4 +IMeas) = ParaId(NPara)
              IXMS3P (N3MEAS+(N3MatLoc-1)*4 +IMeas) = NPara
           Enddo
! Debug
           IF (IDEBUG .GT. 0) THEN
             WRITE(IDEBUG,*) ' 3B-lokatie ID  ', LCID3B (N3MLOC+N3MatLoc)(1:len_trim(LCID3B(N3MLOC+N3MatLoc)))
             WRITE(IDEBUG,*) ' Maatregel      ', LCMS3B (N3MLOC+N3MatLoc)(1:len_trim(LCMS3B(N3MLOC+N3MatLoc)))
             WRITE(IDEBUG,*) ' Prioriteit     ', LCPR3B (N3MLOC+N3MatLoc)
             WRITE(IDEBUG,*) ' Index parameter', IxMs3P (N3MLOC+N3MatLoc)
           ENDIF
         ENDIF
   31    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo


! *********************************************************************
! *** Check dimensies
! *********************************************************************

      IF (N3MEAS .GT. N3MES) THEN
         call write_error_message_rtc (913, 0,'Rd3bms_oldformat',' N3MES 3B measures',IOUT1)
         RetVal = 913
         Return
      ELSEIF (N3MLOC+N3MatLoc .GT. N3LOC) THEN
         call write_error_message_rtc (913, 0,'Rd3bms_oldformat',' N3LOC 3B lokatie-measures',IOUT1)
         RetVal = 913
         Return
      ELSEIF (N3MATLOC .GT. N3MAT) THEN
         call write_error_message_rtc (913, 0,'Rd3bms_oldformat',' N3MATLOC 3B Matlab-measures',IOUT1)
         RetVal = 913
         Return
      ENDIF
! end check
      GOTO 999

! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150 CONTINUE
      call write_error_message_rtc (902, IECODE, 'Rd3bms_oldformat', ' 3B-Maatregelfile', IOUT1)
      RetVal = 902
      Return

! *********************************************************************
! *** end of file
! *********************************************************************

  999 CONTINUE

! *********************************************************************
! ***  Check existence maatregel id uit 3BML record in MLST record
! *********************************************************************

      DO I3LOC = 1, N3MLOC+N3MatLoc
         IXMS3B(I3LOC) = 0
         DO IMEAS=1,N3MEAS
           IF (LCMS3B(I3LOC) .EQ. MSID3B(IMEAS))  THEN
               IXMS3B(I3LOC) = IMEAS
           ENDIF
         ENDDO
         IF (IXMS3B(I3LOC) .LE. 0 .AND. I3Loc .le. N3MLoc) THEN
            WRITE(IOUT1,*) ' RR-location index ', I3LOC
            WRITE(IOUT1,*) ' Measure',N3MEAS
            WRITE(IOUT1,*) ' RR-location id',LCMS3B (I3LOC)
            WRITE(IOUT1,*) ' RR-Measure id', (MSID3B(IMEAS),IMEAS=1,N3MEAS)
            call write_error_message_rtc (922,0, LCMS3B(I3LOC),  ' 3B-measure file', IOUT1)
            RetVal = 922
            Return
         ENDIF
      ENDDO

! *********************************************************************
! *** Determine total different number of 3B-id's involved in measures
! ***   (may be considarably less than number of 3B-location measure-id's,
! ***    because different measures can be applied to the same 3B structure)
! *********************************************************************
! *** Determine total different nr. of id's in 3BML and MATL records
! *********************************************************************

      N3BMS  = 0
      DO ILOC = 1,N3MLOC+N3MatLoc
         DO I3B = 1,N3BMS
            IF (LCID3B(ILOC) .EQ. ID3BML(I3B)) THEN
               IXID3B(ILOC) = I3B
               GOTO 998
            ENDIF
         ENDDO
         N3BMS  = N3BMS + 1
         ID3BML(N3BMS) = LCID3B(ILOC)
         IXID3B(ILOC) = N3BMS
!         MS3BST(N3BMS,1) = 0.
!         MS3BST(N3BMS,2) = 0.
!         MS3BST(N3BMS,3) = 0
!         MS3BST(N3BMS,4) = -999.
!         MS3BST(N3BMS,5) = -999.
!         MS3BST(N3BMS,6) = -999.
!         MS3BST(N3BMS,7) = -999.
  998    CONTINUE
         IF (IDEBUG .GT. 0) THEN
           WRITE(IDEBUG,*) ' Measure-location id ', LCID3B(ILOC), ILOC
           WRITE(IDEBUG,*) ' index ms . ', IXMS3B(ILOC)
           WRITE(IDEBUG,*) ' index id . ', IXID3B(ILOC)
           WRITE(IDEBUG,*) ' Uniek id   ', ID3BML(IXID3B(ILOC))
           WRITE(IDEBUG,*) ' MS-status  ', MS3BST(IXID3B(ILOC),1)
         ENDIF
      ENDDO


      RETURN
      END Function Rd3BMs_OldFormat
