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

      Function RDSMEAS (IDEBUG, IN  , IOUT1, NSMEASI) result(RetVal)

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
! ***   Read Sobek measures file
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
      Use NewTables
      Use ParseToken
      Use ReadLib
      Use ExternalDLLModule

      implicit none

      Integer :: RetVal

      INTEGER    NHLP, INR
      PARAMETER (NHLP = 150)

      LOGICAL        ENDFIL, Success
      CHARACTER*9999 STRING
      CHARACTER*999  ErrorString
      INTEGER        IDEBUG, IOUT1, IN, IMEAS, IPARA, ISBK, IECODE, I
      INTEGER        NSMEASI
!
! Additional variables for NewTables and ParseToken
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Integer        NaValue, StartSubRecord, EndSubRecord, NrSbmsSubRecords, iSubRecord
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError
      Type (TokenArray) RecordData

      Logical        MeasureInActive

      RetVal = 0

      IStart   = 1     ! Scan from token 1
      ScanToTk = 9999  ! Scan up to Token 9999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Rdsmeas')
!
      IF (NHLP .LT. NSCV) then
         CALL ERRMSG (913, 0,'Rdsmeas',' NHLP local parameter', IOUT1)
         RetVal = 913
         Return
      Endif
!
! *********************************************************************
! *** skip header of file; file may be empty in case of only 3B-measures!
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL,'RTC')
!
! *********************************************************************
! *** read data
! *** old format:
!      SBMS id 'Sobek-id' pr iprior ty 1 bp 'Beslispar-id' cv checkval ch check sp setpoint iv initval sbms
!      SBMS id 'Sobek-id' pr iprior ty 2 bp 'Beslispar-id' nv nval cv N_checkvals sp N_setpoints iv initval sbms
!      SBMS id 'Sobek-id' pr iprior ty 3 bp 'Beslispar-id' cp 'beslispar2' ch check sp setpoint iv initval sbms
!      SBMS id 'Sobek-id' pr iprior ty 4 bp 'Beslispar-id' cp 'beslispar3' ch check psp 'beslispar3' iv initval sbms
!
! Addition June 1998: measures of type 5-8, which were already documented earlier, but not yet implemented
!
!     SBMS id 'Sobek_id5' pr 1 ty 5 nv 3 bp 'Beslispar2' ‘Beslispar5’ 'Beslispar3' cv 3.0 5.0 7.0 ch '<' ‘>‘ '<' sp 5.0 iv  0.0  sbms
!     SBMS id 'Sobek_id6' pr 1 ty 6 nv 2 bp 'Beslispar2' ‘Beslispar5’  cv 3.0 5.0 ch '<' ‘>‘ psp ‘Beslispar1’ iv  0.0 sbms
!     SBMS id 'Sobek_id7' pr 2 ty 7 nv 2 bp 'Beslispar2' ‘Beslispar5’  cp ‘Beslispar4’ ‘Beslispar3’ ch '>' ‘<‘ sp 5.0 iv  0.0  sbms
!     SBMS id 'Sobek_id8' pr 2 ty 8 nv 2 bp 'Beslispar2' ‘Beslispar5’  cp 'Beslispar3' ‘Beslispar4’ ch '<' ‘>‘ psp 'Beslispar1' iv 0.0 sbms
!
! Addition March 1998: MATLAB measure (ty 9)
!      SBMS id 'Sobek-id' pr iprior ty 9 iv initval sbms
!     !!! dus zonder bp beslisparameter !
!
! ***   met   iprior (integer), checkval (real), setpoint (real), check (character)
! ***         initval (real)
!
! Addition           External dll (systeemwerking test)
!      SBMS id 'Sobek-id' nm 'name' pr 1 ty 11 iv initval  sbms
! Addition Oct 2017  External measure (TCN)
!      SBMS id 'Sobek-id' nm 'name' pr 1 ty 12 iv initval ti 'TCN id' sbms
!
! *********************************************************************
! November 2004: Slightly adjusted format: group all measures working on the same controller in 1 FLCM record
!                For each measure an individual subrecords SBMS ... sbms is present.
! *********************************************************************
!FLM2.0
!FLCM id '19' nm '' na 0
! SBMS pr 1 ty 6 nv 1 bp 'Hboezem' cv 0.8 ch '<' psp 'MaxQinlaat'  iv 0 sbms
! SBMS pr 2 ty 6 nv 1 bp 'Hboezem' cv 1.2 ch '>' psp 'MinQinlaat'  iv 0 sbms
! SBMS pr 3 ty 6 nv 1 bp 'CLbuitenwater' cv 200 ch '>' psp 'MinQinlaat'  iv 0 sbms
! SBMS pr 4 ty 6 nv 1 bp 'CLbuitenwater' cv 200 ch '<' psp 'inlaatQ'  iv 0 sbms
!flcm
!FLCM id '12' nm '' na 0
!  SBMS pr 1 ty 6 nv 1 bp 'CLpolder' cv 200 ch '<' psp 'MinQinlaatPolder'  iv 0 sbms
!  SBMS pr 2 ty 6 nv 1 bp 'CLpolder' cv 0 ch '>' psp 'inlaatQPolder'  iv 0 sbms
!flcm
!
!FLM2.1  ! iv moved from SBMS to FLCM record, in Matlab SBMS records additional dv field
!FLCM id '19' nm '' iv 0 na 0
! SBMS pr 1 ty 6 nv 1 bp 'Hboezem' cv 0.8 ch '<' psp 'MaxQinlaat'   sbms
! SBMS pr 2 ty 6 nv 1 bp 'Hboezem' cv 1.2 ch '>' psp 'MinQinlaat'   sbms
! SBMS pr 3 ty 6 nv 1 bp 'CLbuitenwater' cv 200 ch '>' psp 'MinQinlaat'   sbms
! SBMS pr 4 ty 6 nv 1 bp 'CLbuitenwater' cv 200 ch '<' psp 'inlaatQ'   sbms
!flcm
!
! *********************************************************************
!
      IMeas = NSMEAS
      Do While (.not. endfil)
         IMeas = IMeas + 1
         CALL SKPCOM (IN, ENDFIL,'RTC')
         IF (ENDFIL) GOTO 21
   19    CONTINUE
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword
         IF (STRING(1:4) .NE. 'FLCM') GOTO 19
! check dimensies
         IF (IMEAS .GT. NSMES) then
            CALL ERRMSG (913, 0,'Rdsmeas',' NSMES Sobek measures',IOUT1)
            RetVal = 913
            Return
         Endif
! end check
! Get FLCM record, controller id and notactive indication
         BackSpace(IN)
         SearchString = 'FLCM'
         ReadError = .false.
         Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
         If (Endfil .or. .not. success) then
            Call ErrMsg (913, 0, 'RdSMeas', ' Unexpected end of file ',IOUT1)
            RetVal = 913
            Return
         Endif
         String = ' '
         Success = GetStringFromBuffer (String)
         if (.not. Success) ReadError = .true.
         Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
         if (.not. Success) ReadError = .true.
         IF (ReadError) then
             ErrorString = ' Read error during reading RTC measures ParseToken for CF module ' // &
                            String(1:len_trim(String))
             Call ErrMsg (974,0,'RdSBMs',ErrorString,IOUT1)
             RetVal = 974
             Return
         Endif

         IStart   = 1     ! Scan from token 1
         if (Getkey ('SBMS', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
            StartSubRecord=ReturnIndx
         endif
         if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             MeasId(IMeas) = RecordData%Token(ReturnIndx+1)
         else
             ReadError = .true.
         endif
!        Initial value moved from measure to FLCM level
         if (Getkey ('iv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             Read (RecordData%Token(ReturnIndx+1),*,Err=991)   InitSp(IMeas)
         else
!            ReadError = .true.   Sept 2006: keyword optional
             InitSp(IMeas) = 0.
         endif
!        Optional name
         if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             MeasName(IMeas) = RecordData%Token(ReturnIndx+1)
         endif
!        NotActive option: na 0 = active, na 1 = not active
         if (Getkey ('na', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             Read (RecordData%Token(ReturnIndx+1),*,Err=991) NaValue
             MeasureInActive = (naValue .ne. 0)
             If (MeasureInActive)  Goto 19     !if inactive, skip rest of this record
         else
             ReadError = .true.
         endif
         If (ReadError) then
              ErrorString = ' Some error in general part of FLCM record for RTC-measure for CF ' // &
                              String(1:len_trim(String))
              Call ErrMsg (974,0,'RdSMeas',ErrorString,IOUT1)
              RetVal = 974
              Return
         endif
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' meas_id ',MEASID(IMEAS)(1:len_trim(MeasId(Imeas)))
! Find the number of SBMS subrecords within the String
         NrSbmsSubRecords = CntStr (' SBMS ', String)
! Now loop over the possible SBMS subrecords
         Do ISubRecord=1,NrSbmsSubRecords
            if (ISubRecord .gt. 1) then
                Imeas = Imeas + 1
                MeasId(Imeas) = MeasId(Imeas-1)
                MeasName(Imeas) = MeasName(Imeas-1)
                InitSp(Imeas) = InitSp(Imeas-1)
                IStart = EndSubRecord+1
            Endif
            if (Getkey ('SBMS', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               StartSubRecord = ReturnIndx
            endif
            if (Getkey ('sbms', StartSubRecord+1, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               EndSubRecord = ReturnIndx
            endif
! measure type and priority
            if (Getkey ('ty', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasTy(IMeas)
            else
                ReadError = .true.
            endif
            if (Getkey ('pr', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasPr(IMeas)
            else
                ReadError = .true.
            endif
            If (ReadError) then
              ErrorString = ' Some error in type or priority in SBMS record for RTC-measure for CF ' // &
                              String(1:len_trim(String))
              Call ErrMsg (974,0,'RdSMeas',ErrorString,IOUT1)
              RetVal = 974
              Return
            endif
            if (Getkey ('mv', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)  MeasMissingValue(IMeas)
            else
!              mv missing is allowed, then default missing value = -999.999
               MeasMissingValue(imeas) = -999.999
            endif

! read beslisparameter only if not Matlab (9) or DLL (11) or exe (12) measure
            IF (MEASTY(IMEAS) .EQ. 9 .or. MEASTY(Imeas) .eq. 11 .or. MeasTy(Imeas) .eq. 12) THEN
               if (MeasTy(imeas) .eq. 9) then
                  MATUSE = .TRUE.
                  IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Set Matuse=TRUE'
                  MEASBP(IMEAS) = 'Matlab' // INTCH4 (IMEAS)
               elseif (MeasTy(imeas) .eq. 11) then
                  MEASBP(IMEAS) = 'Dll' // INTCH4 (IMEAS)
                  NrSbkDllmeasures = NrSbkDllMeasures + 1
                  if (dll_handle .eq. 0) then
                     CALL ERRMSG (958,IMEAS, ErrorString,' Warning: DLL measure defined, but no DLL shared library defined', IOUT1)
                  endif
               elseif (MeasTy(imeas) .eq. 12) then
                  if (Getkey ('ti', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                     MEASBP(IMEAS) = 'TCN_' // RecordData%Token(ReturnIndx+1)
                  else
                     CALL ERRMSG (958,IMEAS, ErrorString,' Warning: measure type 12, but no measure id with keyword ti defined', IOUT1)
                  endif
! check that csv files are specified if type 12 measure used
                  if (.not. UseTCN) then
                     CALL ERRMSG (958,IMEAS, ErrorString,' Error: measure type 12 used, but UseTCN not switched on in Settings', IOUT1)
                  endif
                  if (WriteCsvFile .eq. '' .or. ReadCsvFile .eq. '' .or. RunCommand .eq. '') then
                     CALL ERRMSG (958,IMEAS, ErrorString,' Error: measure type 12 used, but no external command or csv files specified', IOUT1)
                  endif
               endif
               if (Getkey ('mi', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasIdMatlab(IMeas) = RecordData%Token(ReturnIndx+1)
               else
!                 mi missing is allowed, then Measure Id is used in communicating with Matlab
                  if (MeasTy(imeas) .eq. 9) then
                    MeasIdMatlab(Imeas) = MeasId(Imeas)
                    ErrorString = ' Measure id = '// MeasId(imeas)(1:len_trim(MeasId(imeas))) //  &
                                  ' Measure name = '// MeasName(imeas)(1:len_trim(MeasName(imeas)))
                    CALL ERRMSG (994,IMEAS, ErrorString,' Warning: Matlab communication id is missing for this measure', IOUT1)
                  endif
               endif
!              Matlab/TCN default value
               if (Getkey ('dv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                   Read (RecordData%Token(ReturnIndx+1),*,Err=991)   InitSp(IMeas)
               else
                   ReadError = .true.
               endif
            ELSE
                if (Getkey ('bp', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                    MeasBp(IMeas) = RecordData%Token(ReturnIndx+1)
                elseIf (MeasTy(IMeas) .eq. 10) then
                    MeasBp(IMeas) = 'Year'   ! zet even default op year, dat is de eerste interne beslisparameter
                else
                    ReadError = .true.
                endif
            ENDIF

            IStart = StartSubRecord
            IF (MEASTY(IMEAS) .EQ. 1) THEN
! type 1:  SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 1 cv checkval ch check sp setpoint sbms
              if (Getkey ('cv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasCv(IMeas)
              else
                  ReadError = .true.
              endif
              if (Getkey ('ch', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCh(IMeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
              if (Getkey ('sp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 2) THEN
! type 2: SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 2 nv nval cv N_checkvals sp N_setpoints sbms
              if (Getkey ('nv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
              else
                  ReadError = .true.
              endif
              IF (MEASNV(IMEAS) .GT. NHLP .OR. MEASNV(Imeas) .gt. NSCV)  then
                 CALL ERRMSG (913,0,'Rdsmeas',' NHLP; MEASNV',IOUT1)
                 RetVal = 913
                 Return
              Endif
              if (Getkey ('cv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNCv(i,IMeas)
                 Enddo
              else
                  ReadError = .true.
              endif
              MEASCH(IMEAS) = 'Interpolatie'
              if (Getkey ('sp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNSp(i,IMeas)
                 Enddo
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 3) THEN
! type 3: SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 3 cp 'beslispar2' ch check sp setpoint sbms
              if (Getkey ('cp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCp(IMeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
              if (Getkey ('ch', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCh(IMeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
              if (Getkey ('sp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 4) THEN
! type 4: SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 4 cp 'beslispar3' ch check psp 'beslispar3' sbms
              if (Getkey ('cp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCp(IMeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
              if (Getkey ('ch', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCh(IMeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
              if (Getkey ('psp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCSp(IMeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 5) THEN
! type 5: SBMS id 'Sobek-id' pr iprior ty 5 nv nval bp 'N_Beslisparameters' cv N_checkvals sp 1_setpoint iv initial value sbms
              if (Getkey ('nv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
              else
                  ReadError = .true.
              endif
              IF (MEASNV(IMEAS) .GT. NHLP) then
                 CALL ERRMSG (913,0,'Rdsmeas',' NHLP; MEASNV',IOUT1)
                 RetVal = 913
                 Return
              Endif
              if (Getkey ('bp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('cv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNCv(i,IMeas)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('ch', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('sp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 6) THEN
! type 6: SBMS id 'Sobek-id' pr iprior ty 6 nv nval bp 'N_Beslisparameters' cv N_checkvals psp 'beslispar' iv initial value sbms
              if (Getkey ('nv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
              else
                  ReadError = .true.
              endif
              IF (MEASNV(IMEAS) .GT. NHLP) then
                 CALL ERRMSG (913,0,'Rdsmeas',' NHLP; MEASNV',IOUT1)
                 RetVal = 913
                 Return
              Endif
              if (Getkey ('bp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('cv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNCv(i,IMeas)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('ch', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('psp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCSp(Imeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 7) THEN
! type 7: SBMS id 'Sobek-id' pr iprior ty 7 nv nval bp 'N_Beslisparameters' cp N_checkpars sp 1_setpoint iv initial value sbms
              if (Getkey ('nv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
              else
                  ReadError = .true.
              endif
              IF (MEASNV(IMEAS) .GT. NHLP)  then
                 CALL ERRMSG (913,0,'Rdsmeas',' NHLP; MEASNV',IOUT1)
                 RetVal = 913
                 Return
              Endif
              if (Getkey ('bp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('cp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNCp(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('ch', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('sp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 8) THEN
! type 8: SBMS id 'Sobek-id' pr iprior ty 8 nv nval bp 'N_Beslisparameters' cp N_checkpars psp 'Beslispar' iv initial value sbms
              if (Getkey ('nv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
              else
                  ReadError = .true.
              endif
              IF (MEASNV(IMEAS) .GT. NHLP) then
                 CALL ERRMSG (913,0,'Rdsmeas',' NHLP; MEASNV',IOUT1)
                 RetVal = 913
                 Return
              Endif
              if (Getkey ('bp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('cp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNCp(i,IMeas)  = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('ch', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 Do I=1,MEASNV(IMEAS)
                     MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
                 Enddo
              else
                  ReadError = .true.
              endif
              if (Getkey ('psp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCSp(Imeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
              endif
            ELSEIF (MEASTY(IMEAS) .EQ. 10) THEN
! type 10: SBMS id 'Sobek-id' pr iprior ty 10 psp 'Beslispar' iv initial value sbms
!          is same as type 8, but no check values/parameters
              MEASNV(IMEAS) = 0
              if (Getkey ('psp', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  MeasCSp(Imeas) = RecordData%Token(ReturnIndx+1)
              else
                  ReadError = .true.
                  ErrorString = ' RTC-measure type 10 psp not specified correctly ' // &
                            ' Measure id = '// MeasId(imeas)(1:len_trim(MeasId(imeas))) //  &
                            ' Measure name = '// MeasName(imeas)(1:len_trim(MeasName(imeas)))
                  Call ErrMsg (974,0,'RdSMeas',ErrorString,IOUT1)
                  RetVal = 974
                  Return
              endif

            ELSEIF (MEASTY(IMEAS) .LE. 0 .or. MeasTy(Imeas) .gt. 12) THEN
              ErrorString = ' Unsupported type of RTC-measure after ty keyword in SBMS records for CF module ' // &
                            ' Measure id = '// MeasId(imeas)(1:len_trim(MeasId(imeas))) //  &
                            ' Measure name = '// MeasName(imeas)(1:len_trim(MeasName(imeas)))
              Call ErrMsg (974,0,'RdSMeas',ErrorString,IOUT1)
              RetVal = 974
              Return
            ENDIF
! moved to FLCM level
!           if (Getkey ('iv', IStart, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
!               Read (RecordData%Token(ReturnIndx+1),*)   InitSp(IMeas)
!           else
!               ReadError = .true.
!           endif

            IF (IDEBUG .GT. 0) THEN
              WRITE(IDEBUG,*) ' Measure id ', MEASID(IMEAS)(1:len_trim(MeasId(Imeas)))
              WRITE(IDEBUG,*) ' Pri+type'  ,  MEASPR(IMEAS), MEASTY(IMEAS)

              IF (MEASTY(IMEAS) .LE. 4) THEN
                  WRITE(IDEBUG,*) ' Beslispar',  MEASBP(IMEAS)(1:len_trim(MeasBp(Imeas)))
              ELSEIF (MEASTY(IMEAS) .LE. 8) THEN
                  WRITE(IDEBUG,*) ' Beslispars',(MEASNBP(I,IMEAS)(1:len_trim(MeasNBp(I,Imeas))) ,I=1,MEASNV(IMEAS))
              ENDIF

              IF (MEASTY(IMEAS) .EQ. 1) THEN
                 WRITE(IDEBUG,*) ' Check+set 1', MEASCV(IMEAS), MEASSP(IMEAS)
              ELSEIF (MEASTY(IMEAS) .EQ. 2) THEN
                 WRITE(IDEBUG,*) ' Check+set N', MEASNV(IMEAS)
                 WRITE(IDEBUG,*) ' Check vals ', (MEASNCV(I,IMEAS), I=1,MEASNV(IMEAS))
                 WRITE(IDEBUG,*) ' Setpoints  ', (MEASNSP(I,IMEAS), I=1,MEASNV(IMEAS))
              ELSEIF (MEASTY(IMEAS) .EQ. 3) THEN
                 WRITE(IDEBUG,*) ' Check par', MEASCP(IMEAS)(1:len_trim(MeasCp(Imeas)))
                 WRITE(IDEBUG,*) ' Setpoint ', MEASSP(IMEAS)
              ELSEIF (MEASTY(IMEAS) .EQ. 4) THEN
                 WRITE(IDEBUG,*) ' Check par', MEASCP(IMEAS)(1:len_trim(MeasCp(Imeas)))
                 WRITE(IDEBUG,*) ' Set   par', MEASCSP(IMEAS)

              ELSEIF (MEASTY(IMEAS) .GE. 5 .AND. MEASTY(IMEAS) .LE. 8) THEN
                 DO I=1,MEASNV(IMEAS)
                  WRITE(IDEBUG,*) ' Check par', I, MEASNBP(I,IMEAS)(1:len_trim(MeasNBp(I,Imeas)))
                  WRITE(IDEBUG,*) ' Checktest', I, MEASNCH(I,IMEAS)
                    IF (MEASTY(IMEAS) .LE. 6) THEN
                      WRITE(IDEBUG,*) ' Check val', I, MEASNCV(I,IMEAS)
                    ELSEIF (MEASTY(IMEAS) .LE. 8) THEN
                      WRITE(IDEBUG,*) ' Check par', I, MEASNCP(I,IMEAS)(1:len_trim(MeasNCp(I,Imeas)))
                    ENDIF
                 ENDDO
                 IF (MEASTY(IMEAS) .EQ. 5 .OR. MEASTY(IMEAS) .EQ.  7) THEN
                   WRITE(IDEBUG,*) ' Setpoint ', MEASSP(IMEAS)
                 ELSEIF (MEASTY(IMEAS) .EQ. 6 .OR.  MEASTY(IMEAS) .EQ. 8) THEN
                   WRITE(IDEBUG,*) ' Set par ', MEASCP(IMEAS)(1:len_trim(MeasCp(Imeas)))
                 ENDIF
              ENDIF

              IF (MEASTY(IMEAS) .LE. 4) THEN
                 WRITE(IDEBUG,*) ' Checktest  ', MEASCH(IMEAS)
              ENDIF

              WRITE(IDEBUG,*) ' Init. set  ', INITSP(IMEAS)
            ENDIF
! additional read error label
            Goto 992
  991       Continue
            ReadError = .true.
  992       Continue

            IF (ReadError) then
                ErrorString = ' Read error during reading RTC measures SBMS records for CF module ' // &
                              ' Measure id = '// MeasId(imeas)(1:len_trim(MeasId(imeas))) //  &
                              ' Measure name = '// MeasName(imeas)(1:len_trim(MeasName(imeas)))
                Call ErrMsg (974,0,'RdSBMs',ErrorString,IOUT1)
                RetVal = 974
                Return
            Endif

! *************************************************************************
! Check existence beslisparameter id in beslisparameter file (DECISPAR.RTC)
! *************************************************************************

! all types of measures
            IXMSBP(IMEAS) = 0
            DO IPARA=1,NPARA
              IF (PARAID(IPARA) .EQ. MEASBP(IMEAS)) THEN
                  IXMSBP(IMEAS) = IPARA
              ENDIF
            ENDDO
            IF (IXMSBP(IMEAS) .LE. 0) THEN
              IF (MEASBP(IMEAS)(1:6) .NE. 'Matlab' .and. MEASBP(IMEAS)(1:3) .NE. 'Dll' .and. MeasBP(Imeas)(1:3) .ne. 'TCN') THEN
                 WRITE(IOUT1,*)  ' Error for measure id ', MeasId(IMEAS)(1:len_trim(MeasId(Imeas))), &
                                   ' and name ', MeasName(Imeas)(1:len_trim(MeasName(Imeas)))
                 WRITE(IOUT1,*)  ' Measure-parameter',MEASBP(IMEAS)(1:len_trim(MeasBp(Imeas))), ' not in list:'
                 DO IPARA=1,NPARA
                    WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
                 ENDDO
                 CALL ERRMSG (919,0, MEASBP(IMEAS)(1:len_trim(MeasBP(Imeas))), ' Sobek-measure file', IOUT1)
                 RetVal = 919
                 Return
              ELSE ! Matlab,DLL or TCN maatregel: voeg beslisparameter toe
                 NPARA = NPARA + 1
                 PARAID (NPARA) = MEASBP(IMEAS)
                 PARDIM (NPARA,1) = 0
                 PARDIM (NPARA,2) = 0
                 PARDIM (NPARA,3) = 0
                 PARDIM (NPARA,4) = 0
                 PARDIM (NPARA,5) = 0
                 IXMSBP(IMEAS) = NPARA
              ENDIF
            ENDIF

! check multiple decision parameter measures (type 5-8; not needed for type 9 Matlab,type 10 MeasNV=0, type 11 DLL, type 12 TCN
            IF (MEASTY(IMEAS) .GE. 5 .AND. MEASTY(IMEAS) .LE. 8) THEN
              DO INR=1,MEASNV(IMEAS)
                DO IPARA=1,NPARA
                  IF (PARAID(IPARA) .EQ. MEASNBP(INR,IMEAS)) THEN
                      IXMSNBP(INR,IMEAS) = IPARA
                  ENDIF
                ENDDO
                IF (IXMSNBP(INR,IMEAS) .LE. 0) THEN
                  IF (MEASNBP(INR,IMEAS)(1:6) .NE. 'Matlab' .and. MEASNBP(INR,IMEAS)(1:3) .ne. 'Dll'.and. MEASNBP(INR,IMEAS)(1:3) .ne. 'TCN') THEN
                    WRITE(IOUT1,*)  ' Error for measure id ', MeasId(IMEAS)(1:len_trim(MeasId(Imeas))), &
                                    ' and name ', MeasName(Imeas)(1:len_trim(MeasName(Imeas)))
                    WRITE(IOUT1,*)  ' Measure-parameter',MEASNBP(INR,IMEAS)(1:len_trim(MeasNBP(Inr,Imeas))),' not in list:'
                    DO IPARA=1,NPARA
                      WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
                    ENDDO
                    CALL ERRMSG (919,0, MEASNBP(INR,IMEAS)(1:len_trim(MeasNBP(Inr,Imeas))), ' Sobek-measure file', IOUT1)
                    RetVal = 919
                    Return
                  ELSE ! Matlab maatregel: voeg beslisparameter toe
                    NPARA = NPARA + 1
                    PARAID (NPARA) = MEASNBP(INR,IMEAS)
                    PARDIM (NPARA,1) = 0
                    PARDIM (NPARA,2) = 0
                    PARDIM (NPARA,3) = 0
                    PARDIM (NPARA,4) = 0
                    PARDIM (NPARA,5) = 0
                    IXMSNBP(INR,IMEAS) = NPARA
                  ENDIF
                ENDIF
              ENDDO
            ENDIF

! check check parameters (type 3, 4)
            IF (MEASTY(IMEAS) .EQ. 3 .OR. MEASTY(IMEAS) .EQ. 4) THEN
               IXMSCP(IMEAS) = 0
               DO IPARA=1,NPARA
                 IF (PARAID(IPARA) .EQ. MEASCP(IMEAS)) THEN
                     IXMSCP(IMEAS) = IPARA
                 ENDIF
               ENDDO
               IF (IXMSCP(IMEAS) .LE. 0) THEN
                  WRITE(IOUT1,*)  ' Error for measure id ', MeasId(IMEAS)(1:len_trim(MeasId(Imeas))), &
                                  ' and name ', MeasName(Imeas)(1:len_trim(MeasName(Imeas)))
                  WRITE(IOUT1,*)  ' Measure-checkparameter',MEASCP(IMEAS)(1:len_trim(MeasCP(Imeas))),' not in list '
                  DO IPARA=1,NPARA
                     WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
                  ENDDO
                  CALL ERRMSG (919,0, MEASCP(IMEAS)(1:len_trim(MeasCP(Imeas))), ' Sobek-measure file', IOUT1)
                  RetVal = 919
                  Return
               ENDIF
            ENDIF

! check check parameters (type 7, 8)
            IF (MEASTY(IMEAS) .EQ. 7 .OR. MEASTY(IMEAS) .EQ. 8) THEN
              DO INR=1,MEASNV(IMEAS)
                 IXMSNCP(INR,IMEAS) = 0
                 DO IPARA=1,NPARA
                   IF (PARAID(IPARA) .EQ. MEASNCP(INR,IMEAS)) THEN
                       IXMSNCP(INR,IMEAS) = IPARA
                   ENDIF
                 ENDDO
                 IF (IXMSNCP(INR,IMEAS) .LE. 0) THEN
                    WRITE(IOUT1,*)  ' Error for measure id ', MeasId(IMEAS)(1:len_trim(MeasId(Imeas))), &
                                    ' and name ', MeasName(Imeas)(1:len_trim(MeasName(Imeas)))
                    WRITE(IOUT1,*) ' Measure-parameter',MEASNCP(INR,IMEAS)(1:len_trim(MeasNCP(Inr,Imeas))),' not in list'
                    DO IPARA=1,NPARA
                       WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
                    ENDDO
                    CALL ERRMSG (919,0, MEASNCP(INR,IMEAS)(1:len_trim(MeasNCP(Inr,Imeas))),' Sobek-measure file', IOUT1)
                    RetVal = 919
                    Return
                 ENDIF
              ENDDO
            ENDIF

! check setpoint parameter type 4, 6, 8, 10
            IF (MEASTY(IMEAS) .EQ. 4 .OR. MEASTY(IMEAS) .EQ. 6 .OR.  &
                  MEASTY(IMEAS) .EQ. 8 .OR.  MEASTY(IMEAS) .EQ. 10) THEN
               IXMSSP(IMEAS) = 0
               DO IPARA=1,NPARA
                 IF (PARAID(IPARA) .EQ. MEASCSP(IMEAS)) THEN
                     IXMSSP(IMEAS) = IPARA
                 ENDIF
               ENDDO
               IF (IXMSSP(IMEAS) .LE. 0) THEN
                  WRITE(IOUT1,*) ' Error for measure id ', MeasId(IMEAS)(1:len_trim(MeasId(Imeas))), &
                                 ' and name ', MeasName(Imeas)(1:len_trim(MeasName(Imeas)))
                  WRITE(IOUT1,*) ' Measure-setpoint parameter', MEASCSP(IMEAS)(1:len_trim(MEASCSP(Imeas))),' not in list'
                  DO IPARA=1,NPARA
                     WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
                  ENDDO
                  CALL ERRMSG (919,0, MEASCSP(IMEAS)(1:len_trim(MEASCSP(Imeas))), ' Sobek-measure file', IOUT1)
                  RetVal = 919
                  Return
               ENDIF
            ENDIF

! Check correct type of checks ('<', '=', '>')
            IF (MEASTY(IMEAS) .LE. 4. .and. MEASTY(IMEAS) .ne. 2) Then
               IF ( MEASCH(IMEAS) .NE. '<' .AND. MEASCH(IMEAS) .NE. '=' .AND. &
                                                     MEASCH(IMEAS) .NE. '>')   THEN
                 ErrorString = ' Measure id = '// MeasId(imeas)(1:len_trim(MeasId(imeas))) //  &
                               ' Measure name = '// MeasName(imeas)(1:len_trim(MeasName(imeas)))
                 CALL ERRMSG (920,IMEAS, ErrorString, MEASCH(IMEAS), IOUT1)
                 RetVal = 920
                 Return
               ENDIF
            ELSEIF (MEASTY(IMEAS) .GE. 5. .and. MEASTY(IMEAS) .LE. 8) Then
               DO INR=1,MEASNV(IMEAS)
                 IF ( MEASNCH(INR,IMEAS) .NE. '<' .AND. MEASNCH(INR,IMEAS) .NE. '=' .AND. &
                                                            MEASNCH(INR,IMEAS) .NE. '>')  THEN
                   ErrorString = ' Measure id = '// MeasId(imeas)(1:len_trim(MeasId(imeas))) //  &
                                 ' Measure name = '// MeasName(imeas)(1:len_trim(MeasName(imeas)))
                   CALL ERRMSG (920,IMEAS, ErrorString, MEASNCH(INR,IMEAS), IOUT1)
                   RetVal = 920
                   Return
                 ENDIF
               ENDDO
            ENDIF

! *********************************************************************
! Set lowest priority of Sobek-measures;
! Note: priority 1 = highest (first) priority, priority 2=second priority, etc.
! *********************************************************************

            LOWSPRI = MAX (LOWSPRI, MEASPR(IMEAS))

            IStart   = EndSubRecord
         Enddo

   20 ENDDO
   21 CONTINUE

      NSMEASI = IMEAS - 1 - NSMEAS
      If (NSMEASI .EQ. 0) then
          Write(*,*) ' No Sobek measures defined'
      Endif
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE,'Rdsmeas',' Sbk-measurefile',IOUT1)
      RetVal = 902
      Return

! *********************************************************************
! *** end of file
! *********************************************************************

  999 CONTINUE

! *********************************************************************
! *** Determine total different number of Sobek-id's involved in measures
! ***   (may be considarably less than number of Sobek-measure-id's,
! ***    because different measures can be applied to the same Sobek structure)
! *********************************************************************

      NSMSID = 0
      DO IMEAS=NSMEAS+1,NSMEAS+NSMEASI
         DO ISBK=1,NSMSID
            IF (MEASID(IMEAS) .EQ. MSSBID(ISBK)) THEN
               IXMSSB(IMEAS) = ISBK
               GOTO 998
            ENDIF
         ENDDO
         NSMSID = NSMSID + 1
         MSSBID(NSMSID) = MEASID(IMEAS)
         MSSBDescr(NSMSID) = MEASName(IMEAS)
         IXMSSB(IMEAS) = NSMSID
         MSSBST(NSMSID)= INITSP(IMEAS)

  998    CONTINUE
         IF (IDEBUG .GT. 0) THEN
           WRITE(IDEBUG,*) ' imeas      ', IMEAS
           WRITE(IDEBUG,*) ' Measure id ', MEASID(IMEAS)(1:len_trim(MeasId(imeas))), IMEAS
           WRITE(IDEBUG,*) ' index lok. ', IXMSSB(IMEAS)
           WRITE(IDEBUG,*) ' Uniek id   ', MSSBID(IXMSSB(IMEAS))(1:len_trim(MSSBID(IXMSSB(IMEAS))))
         ENDIF
      ENDDO

      RETURN
      END Function RdSMeas




      Function RDSMEAS_OldFormat (IDEBUG, IN, IOUT1, NSMEASI) result(RetVal)

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
! ***   Read Sobek measures file
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
      Use NewTables
      Use ParseToken
      Use ReadLib

      Integer :: RetVal

      INTEGER    NHLP, INR
      PARAMETER (NHLP = 10)

      LOGICAL      ENDFIL, Success
      CHARACTER*999 STRING
      INTEGER       IDEBUG, IOUT1, IN, IMEAS, IPARA, ISBK, IECODE, I
      INTEGER       NSMEASI
!
! Additional variables for NewTables and ParseToken
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError
      Type (TokenArray) RecordData

      RetVal = 0

      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Rdsmeas_OldFormat')
!
      IF (NHLP .LT. NSCV) then
         CALL ERRMSG (913, 0,'Rdsmeas_OldFormat',' NHLP local parameter', IOUT1)
         RetVal = 913
         Return
      Endif
!
! *********************************************************************
! *** skip header of file; file may be empty in case of only 3B-measures!
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL,'RTC')
!
! *********************************************************************
! *** read data
! *** format:
!      SBMS id 'Sobek-id' pr iprior ty 1 bp 'Beslispar-id' cv checkval ch check sp setpoint iv initval sbms
!      SBMS id 'Sobek-id' pr iprior ty 2 bp 'Beslispar-id' nv nval cv N_checkvals sp N_setpoints iv initval sbms
!      SBMS id 'Sobek-id' pr iprior ty 3 bp 'Beslispar-id' cp 'beslispar2' ch check sp setpoint iv initval sbms
!      SBMS id 'Sobek-id' pr iprior ty 4 bp 'Beslispar-id' cp 'beslispar3' ch check psp 'beslispar3' iv initval sbms
!
! Addition June 1998: measures of type 5-8, which were already documented earlier, but not yet implemented
!
!     SBMS id 'Sobek_id5' pr 1 ty 5 nv 3 bp 'Beslispar2' ‘Beslispar5’ 'Beslispar3' cv 3.0 5.0 7.0 ch '<' ‘>‘ '<' sp 5.0 iv  0.0  sbms
!     SBMS id 'Sobek_id6' pr 1 ty 6 nv 2 bp 'Beslispar2' ‘Beslispar5’  cv 3.0 5.0 ch '<' ‘>‘ psp ‘Beslispar1’ iv  0.0 sbms
!     SBMS id 'Sobek_id7' pr 2 ty 7 nv 2 bp 'Beslispar2' ‘Beslispar5’  cp ‘Beslispar4’ ‘Beslispar3’ ch '>' ‘<‘ sp 5.0 iv  0.0  sbms
!     SBMS id 'Sobek_id8' pr 2 ty 8 nv 2 bp 'Beslispar2' ‘Beslispar5’  cp 'Beslispar3' ‘Beslispar4’ ch '<' ‘>‘ psp 'Beslispar1' iv 0.0 sbms
!
! Addition March 1998: MATLAB measure (ty 9)
!      SBMS id 'Sobek-id' pr iprior ty 9 iv initval sbms
!     !!! dus zonder bp beslisparameter !
!
! ***   met   iprior (integer), checkval (real), setpoint (real), check (character)
! ***         initval (real)
! *********************************************************************
!
      IMeas = NSMEAS
      Do While (.not. endfil)
         IMeas = IMeas + 1
         CALL SKPCOM (IN, ENDFIL,'RTC')
         IF (ENDFIL) GOTO 21
   19    CONTINUE
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword
         IF (STRING(1:4) .NE. 'SBMS') GOTO 19
! check dimensies
         IF (IMEAS .GT. NSMES) then
            CALL ERRMSG (913, 0,'Rdsmeas_OldFormat',' NSMES Sobek measures',IOUT1)
            RetVal = 913
            Return
         Endif
! end check
         BackSpace(IN)
         SearchString = 'SBMS'
         ReadError = .false.
         Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
         If (Endfil .or. .not. success) then
            Call ErrMsg (913, 0, 'Rdpara', ' Unexpected end of file ',IOUT1)
            RetVal = 913
            Return
         Endif
         String = ' '
         Success = GetStringFromBuffer (String)
         if (.not. Success) ReadError = .true.
         Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
         if (.not. Success) ReadError = .true.
         if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             MeasId(IMeas) = RecordData%Token(ReturnIndx+1)
         else
             ReadError = .true.
         endif
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' meas_id ',MEASID(IMEAS)(1:len_trim(MeasId(Imeas)))
         if (Getkey ('ty', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasTy(IMeas)
         else
             ReadError = .true.
         endif
         if (Getkey ('pr', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasPr(IMeas)
         else
             ReadError = .true.
         endif

! read beslisparameter only if not Matlab measure
         IF (MEASTY(IMEAS) .EQ. 9) THEN
            MATUSE = .TRUE.
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Set Matuse=TRUE'
            MEASBP(IMEAS) = 'Matlab' // INTCH4 (IMEAS)
            if (Getkey ('mi', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasIdMatlab(IMeas) = RecordData%Token(ReturnIndx+1)
            else
!              mi missing is allowed (will be missing in 209003), then Measure Id is used in communicating with Matlab
               MeasIdMatlab(Imeas) = MeasId(Imeas)
            endif
         ELSE
             if (Getkey ('bp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                 MeasBp(IMeas) = RecordData%Token(ReturnIndx+1)
             else
                 ReadError = .true.
             endif
         ENDIF

         IF (MEASTY(IMEAS) .EQ. 1) THEN
! type 1:  SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 1 cv checkval ch check sp setpoint sbms
           if (Getkey ('cv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasCv(IMeas)
           else
               ReadError = .true.
           endif
           if (Getkey ('ch', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCh(IMeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('sp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
           else
               ReadError = .true.
           endif
         ELSEIF (MEASTY(IMEAS) .EQ. 2) THEN
! type 2: SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 2 nv nval cv N_checkvals sp N_setpoints sbms
           if (Getkey ('nv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
           else
               ReadError = .true.
           endif
           IF (MEASNV(IMEAS) .GT. NHLP)  then
              CALL ERRMSG (913,0,'Rdsmeas_OldFormat',' NHLP; MEASNV',IOUT1)
              RetVal = 913
              Return
           Endif
           if (Getkey ('cv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNCv(i,IMeas)
              Enddo
           else
               ReadError = .true.
           endif
           MEASCH(IMEAS) = 'Interpolatie'
           if (Getkey ('sp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNSp(i,IMeas)
              Enddo
           else
               ReadError = .true.
           endif
         ELSEIF (MEASTY(IMEAS) .EQ. 3) THEN
! type 3: SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 3 cp 'beslispar2' ch check sp setpoint sbms
           if (Getkey ('cp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCp(IMeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('ch', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCh(IMeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('sp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
           else
               ReadError = .true.
           endif
         ELSEIF (MEASTY(IMEAS) .EQ. 4) THEN
! type 4: SBMS id 'Sobek-id' bp 'Beslispar-id' pr iprior ty 4 cp 'beslispar3' ch check psp 'beslispar3' sbms
           if (Getkey ('cp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCp(IMeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('ch', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCh(IMeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('psp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCSp(IMeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
         ELSEIF (MEASTY(IMEAS) .EQ. 5) THEN
! type 5: SBMS id 'Sobek-id' pr iprior ty 5 nv nval bp 'N_Beslisparameters' cv N_checkvals sp 1_setpoint iv initial value sbms
           if (Getkey ('nv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
           else
               ReadError = .true.
           endif
           IF (MEASNV(IMEAS) .GT. NHLP) then
              CALL ERRMSG (913,0,'Rdsmeas_OldFormat',' NHLP; MEASNV',IOUT1)
              RetVal = 913
              Return
           Endif
           if (Getkey ('bp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('cv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNCv(i,IMeas)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('ch', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('sp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
           else
               ReadError = .true.
           endif
         ELSEIF (MEASTY(IMEAS) .EQ. 6) THEN
! type 6: SBMS id 'Sobek-id' pr iprior ty 6 nv nval bp 'N_Beslisparameters' cv N_checkvals psp 'beslispar' iv initial value sbms
           if (Getkey ('nv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
           else
               ReadError = .true.
           endif
           IF (MEASNV(IMEAS) .GT. NHLP) then
              CALL ERRMSG (913,0,'Rdsmeas_OldFormat',' NHLP; MEASNV',IOUT1)
              RetVal = 913
              Return
           Endif
           if (Getkey ('bp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('cv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  Read (RecordData%Token(ReturnIndx+i),*,Err=991)   MeasNCv(i,IMeas)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('ch', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('psp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCSp(Imeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
         ELSEIF (MEASTY(IMEAS) .EQ. 7) THEN
! type 7: SBMS id 'Sobek-id' pr iprior ty 7 nv nval bp 'N_Beslisparameters' cp N_checkpars sp 1_setpoint iv initial value sbms
           if (Getkey ('nv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
           else
               ReadError = .true.
           endif
           IF (MEASNV(IMEAS) .GT. NHLP)  then
              CALL ERRMSG (913,0,'Rdsmeas_OldFormat',' NHLP; MEASNV',IOUT1)
              RetVal = 913
              Return
           Endif
           if (Getkey ('bp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('cp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNCp(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('ch', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('sp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasSp(IMeas)
           else
               ReadError = .true.
           endif
         ELSEIF (MEASTY(IMEAS) .EQ. 8) THEN
! type 8: SBMS id 'Sobek-id' pr iprior ty 8 nv nval bp 'N_Beslisparameters' cp N_checkpars psp 'Beslispar' iv initial value sbms
           if (Getkey ('nv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Read (RecordData%Token(ReturnIndx+1),*,Err=991)   MeasNv(IMeas)
           else
               ReadError = .true.
           endif
           IF (MEASNV(IMEAS) .GT. NHLP) then
              CALL ERRMSG (913,0,'Rdsmeas_OldFormat',' NHLP; MEASNV',IOUT1)
              RetVal = 913
              Return
           Endif
           if (Getkey ('bp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNBp(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('cp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNCp(i,IMeas)  = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('ch', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,MEASNV(IMEAS)
                  MeasNCh(i,IMeas) = RecordData%Token(ReturnIndx+i)
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('psp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               MeasCSp(Imeas) = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
         ENDIF
! altijd:
         if (Getkey ('iv', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
             Read (RecordData%Token(ReturnIndx+1),*,Err=991)   InitSp(IMeas)
         else
!             ReadError = .true.   ! optional keyword, Sept 2006
             InitSp(Imeas) = 0.0
         endif

         IF (IDEBUG .GT. 0) THEN
           WRITE(IDEBUG,*) ' Measure id ', MEASID(IMEAS)(1:len_trim(MeasId(Imeas)))
           WRITE(IDEBUG,*) ' Pri+type'  ,  MEASPR(IMEAS), MEASTY(IMEAS)

           IF (MEASTY(IMEAS) .LE. 4) THEN
               WRITE(IDEBUG,*) ' Beslispar',  MEASBP(IMEAS)(1:len_trim(MeasBp(Imeas)))
           ELSEIF (MEASTY(IMEAS) .LE. 8) THEN
               WRITE(IDEBUG,*) ' Beslispars',(MEASNBP(I,IMEAS)(1:len_trim(MeasNBp(I,Imeas))) ,I=1,MEASNV(IMEAS))
           ENDIF

           IF (MEASTY(IMEAS) .EQ. 1) THEN
              WRITE(IDEBUG,*) ' Check+set 1', MEASCV(IMEAS), MEASSP(IMEAS)
           ELSEIF (MEASTY(IMEAS) .EQ. 2) THEN
              WRITE(IDEBUG,*) ' Check+set N', MEASNV(IMEAS)
              WRITE(IDEBUG,*) ' Check vals ', (MEASNCV(I,IMEAS), I=1,MEASNV(IMEAS))
              WRITE(IDEBUG,*) ' Setpoints  ', (MEASNSP(I,IMEAS), I=1,MEASNV(IMEAS))
           ELSEIF (MEASTY(IMEAS) .EQ. 3) THEN
              WRITE(IDEBUG,*) ' Check par', MEASCP(IMEAS)(1:len_trim(MeasCp(Imeas)))
              WRITE(IDEBUG,*) ' Setpoint ', MEASSP(IMEAS)
           ELSEIF (MEASTY(IMEAS) .EQ. 4) THEN
              WRITE(IDEBUG,*) ' Check par', MEASCP(IMEAS)(1:len_trim(MeasCp(Imeas)))
              WRITE(IDEBUG,*) ' Set   par', MEASCSP(IMEAS)

           ELSEIF (MEASTY(IMEAS) .GE. 5 .AND. MEASTY(IMEAS) .LE. 8) THEN
              DO I=1,MEASNV(IMEAS)
               WRITE(IDEBUG,*) ' Check par', I, MEASNBP(I,IMEAS)(1:len_trim(MeasNBp(I,Imeas)))
               WRITE(IDEBUG,*) ' Checktest', I, MEASNCH(I,IMEAS)
                 IF (MEASTY(IMEAS) .LE. 6) THEN
                   WRITE(IDEBUG,*) ' Check val', I, MEASNCV(I,IMEAS)
                 ELSEIF (MEASTY(IMEAS) .LE. 8) THEN
                   WRITE(IDEBUG,*) ' Check par', I, MEASNCP(I,IMEAS)(1:len_trim(MeasNCp(I,Imeas)))
                 ENDIF
              ENDDO
              IF (MEASTY(IMEAS) .EQ. 5 .OR. MEASTY(IMEAS) .EQ.  7) THEN
                WRITE(IDEBUG,*) ' Setpoint ', MEASSP(IMEAS)
              ELSEIF (MEASTY(IMEAS) .EQ. 6 .OR.  MEASTY(IMEAS) .EQ. 8) THEN
                WRITE(IDEBUG,*) ' Set par ', MEASCP(IMEAS)(1:len_trim(MeasCp(Imeas)))
              ENDIF
           ENDIF

           IF (MEASTY(IMEAS) .LE. 4) THEN
              WRITE(IDEBUG,*) ' Checktest  ', MEASCH(IMEAS)
           ENDIF

           WRITE(IDEBUG,*) ' Init. set  ', INITSP(IMEAS)
         ENDIF
! additional read error label
         Goto 992
  991    Continue
         ReadError = .true.
  992    Continue

         IF (ReadError) then
             Call ErrMsg (974,0,'RdSBMs',' Read error during reading RTC measures SBMS records for CF module',IOUT1)
             RetVal = 974
             Return
         Endif

! *********************************************************************
! Check existence beslisparameter id in beslisparameter file (BESLISPA.RTC)
! *********************************************************************

! all types of measures
         IXMSBP(IMEAS) = 0
         DO IPARA=1,NPARA
           IF (PARAID(IPARA) .EQ. MEASBP(IMEAS)) THEN
               IXMSBP(IMEAS) = IPARA
           ENDIF
         ENDDO
         IF (IXMSBP(IMEAS) .LE. 0) THEN
           IF (MEASBP(IMEAS)(1:6) .NE. 'Matlab' ) THEN
              WRITE(IOUT1,*)  ' Measure-parameter',MEASBP(IMEAS), ' not in list:'
              DO IPARA=1,NPARA
                 WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
              ENDDO
              CALL ERRMSG (919,0, MEASBP(IMEAS), ' Sobek-measure file', IOUT1)
              RetVal = 919
              Return
           ELSE
!           Matlab maatregel: voeg beslisparameter toe
              NPARA = NPARA + 1
              PARAID (NPARA) = MEASBP(IMEAS)
              PARDIM (NPARA,1) = 0
              PARDIM (NPARA,2) = 0
              PARDIM (NPARA,3) = 0
              PARDIM (NPARA,4) = 0
              PARDIM (NPARA,5) = 0
              IXMSBP(IMEAS) = NPARA
           ENDIF
         ENDIF

! check multiple decision parameter measures (type 5-8)
         IF (MEASTY(IMEAS) .GE. 5 .AND. MEASTY(IMEAS) .LE. 8) THEN
           DO INR=1,MEASNV(IMEAS)
             DO IPARA=1,NPARA
               IF (PARAID(IPARA) .EQ. MEASNBP(INR,IMEAS)) THEN
                   IXMSNBP(INR,IMEAS) = IPARA
               ENDIF
             ENDDO
             IF (IXMSNBP(INR,IMEAS) .LE. 0) THEN
               IF (MEASNBP(INR,IMEAS)(1:6) .NE. 'Matlab' ) THEN
                 WRITE(IOUT1,*)  ' Measure-parameter',MEASNBP(INR,IMEAS),' not in list:'
                 DO IPARA=1,NPARA
                   WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
                 ENDDO
                 CALL ERRMSG (919,0, MEASNBP(INR,IMEAS), ' Sobek-measure file', IOUT1)
                 RetVal = 919
                 Return
               ELSE
!               Matlab maatregel: voeg beslisparameter toe
                 NPARA = NPARA + 1
                 PARAID (NPARA) = MEASNBP(INR,IMEAS)
                 PARDIM (NPARA,1) = 0
                 PARDIM (NPARA,2) = 0
                 PARDIM (NPARA,3) = 0
                 PARDIM (NPARA,4) = 0
                 PARDIM (NPARA,5) = 0
                 IXMSNBP(INR,IMEAS) = NPARA
               ENDIF
             ENDIF
           ENDDO
         ENDIF

! check check parameters (type 3, 4)
         IF (MEASTY(IMEAS) .EQ. 3 .OR. MEASTY(IMEAS) .EQ. 4) THEN
            IXMSCP(IMEAS) = 0
            DO IPARA=1,NPARA
              IF (PARAID(IPARA) .EQ. MEASCP(IMEAS)) THEN
                  IXMSCP(IMEAS) = IPARA
              ENDIF
            ENDDO
            IF (IXMSCP(IMEAS) .LE. 0) THEN
               WRITE(IOUT1,*)  ' Measure-checkparameter',MEASCP(IMEAS),' not in list '
               DO IPARA=1,NPARA
                  WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
               ENDDO
               CALL ERRMSG (919,0, MEASCP(IMEAS), ' Sobek-measure file', IOUT1)
               RetVal = 919
               Return
            ENDIF
         ENDIF

! check check parameters (type 7, 8)
         IF (MEASTY(IMEAS) .EQ. 7 .OR. MEASTY(IMEAS) .EQ. 8) THEN
           DO INR=1,MEASNV(IMEAS)
              IXMSNCP(INR,IMEAS) = 0
              DO IPARA=1,NPARA
                IF (PARAID(IPARA) .EQ. MEASNCP(INR,IMEAS)) THEN
                    IXMSNCP(INR,IMEAS) = IPARA
                ENDIF
              ENDDO
              IF (IXMSNCP(INR,IMEAS) .LE. 0) THEN
                 WRITE(IOUT1,*) ' Measure-parameter',MEASNCP(INR,IMEAS),' not in list'
                 DO IPARA=1,NPARA
                    WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
                 ENDDO
                 CALL ERRMSG (919,0, MEASNCP(INR,IMEAS),' Sobek-measure file', IOUT1)
                 RetVal = 919
                 Return
              ENDIF
           ENDDO
         ENDIF

! check setpoint parameter type 4, 6, 8
         IF (MEASTY(IMEAS) .EQ. 4 .OR. MEASTY(IMEAS) .EQ. 6 .OR.  MEASTY(IMEAS) .EQ. 8) THEN
            IXMSSP(IMEAS) = 0
            DO IPARA=1,NPARA
              IF (PARAID(IPARA) .EQ. MEASCSP(IMEAS)) THEN
                  IXMSSP(IMEAS) = IPARA
              ENDIF
            ENDDO
            IF (IXMSSP(IMEAS) .LE. 0) THEN
               WRITE(IOUT1,*) ' Measure-setpoint parameter', MEASCSP(IMEAS),' not in list'
               DO IPARA=1,NPARA
                  WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))
               ENDDO
               CALL ERRMSG (919,0, MEASCSP(IMEAS), ' Sobek-measure file', IOUT1)
               RetVal = 919
               Return
            ENDIF
         ENDIF

! Check correct type of checks ('<', '=', '>')
         IF (MEASTY(IMEAS) .LE. 4. .and. MEASTY(IMEAS) .ne. 2) Then
            IF ( MEASCH(IMEAS) .NE. '<' .AND. MEASCH(IMEAS) .NE. '=' .AND. &
                                                  MEASCH(IMEAS) .NE. '>')   THEN
              CALL ERRMSG (920,IMEAS, MEASID(IMEAS), MEASCH(IMEAS), IOUT1)
              RetVal = 920
              Return
            ENDIF
         ELSEIF (MEASTY(IMEAS) .GE. 5. .and. MEASTY(IMEAS) .LE. 8) Then
            DO INR=1,MEASNV(IMEAS)
              IF ( MEASNCH(INR,IMEAS) .NE. '<' .AND. MEASNCH(INR,IMEAS) .NE. '=' .AND. &
                                                         MEASNCH(INR,IMEAS) .NE. '>')  THEN
                CALL ERRMSG (920,IMEAS, MEASID(IMEAS), MEASNCH(INR,IMEAS), IOUT1)
                RetVal = 920
                Return
              ENDIF
            ENDDO
         ENDIF

! *********************************************************************
! Set lowest priority of Sobek-measures;
! Note: priority 1 = highest (first) priority, priority 2=second priority, etc.
! *********************************************************************

         LOWSPRI = MAX (LOWSPRI, MEASPR(IMEAS))

   20 ENDDO
   21 CONTINUE

      NSMEASI = IMEAS - 1 - NSMEAS
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE,'Rdsmeas_OldFormat',' Sbk-measurefile',IOUT1)
      RetVal = 902
      Return

! *********************************************************************
! *** end of file
! *********************************************************************

  999 CONTINUE

! *********************************************************************
! *** Determine total different number of Sobek-id's involved in measures
! ***   (may be considarably less than number of Sobek-measure-id's,
! ***    because different measures can be applied to the same Sobek structure)
! *********************************************************************

      NSMSID = 0
      DO IMEAS=NSMEAS+1,NSMEAS+NSMEASI
         DO ISBK=1,NSMSID
            IF (MEASID(IMEAS) .EQ. MSSBID(ISBK)) THEN
               IXMSSB(IMEAS) = ISBK
               GOTO 998
            ENDIF
         ENDDO
         NSMSID = NSMSID + 1
         MSSBID(NSMSID) = MEASID(IMEAS)
         IXMSSB(IMEAS) = NSMSID
         MSSBST(NSMSID)= INITSP(IMEAS)

  998    CONTINUE
         IF (IDEBUG .GT. 0) THEN
           WRITE(IDEBUG,*) ' Measure id ', MEASID(IMEAS)(1:len_trim(MeasId(imeas))), IMEAS
           WRITE(IDEBUG,*) ' index lok. ', IXMSSB(IMEAS)
           WRITE(IDEBUG,*) ' Uniek id   ', MSSBID(IXMSSB(IMEAS))(1:len_trim(MSSBID(IXMSSB(IMEAS))))
         ENDIF
      ENDDO

      RETURN
      END Function RdSMeas_OldFormat
