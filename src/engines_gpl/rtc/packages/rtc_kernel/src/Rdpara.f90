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

      Function RDPARA (IDEBUG, IN, IOUT1, IflRtnRtc) Result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                 Date: June 1997
! *********************************************************************
! *** Last update: November 1998                  By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read decision parameter file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use ParameterModule
      Use ReservoirModule
      Use NewTables
      Use DH_Alloc
      Use ParseToken
      Use LocationDataModule
      Use DecisionModule
      Use OtherData
      Use ReadLib

      Implicit None

! Return Value
      Integer :: RetVal

      INTEGER    NHLP
!      PARAMETER (NHLP = MAX (NSPAR, N3PAR, NPPAR, NEPAR, NQPAR, NDPAR) )
      PARAMETER (NHLP = 50)

      LOGICAL      ENDFIL, ALLOW, FOUND, Par2Use, ParRsvUse, Par3Use
      INTEGER      IDUM(NHLP), IDEBUG, IN, IOUT1, IflRtnRtc, &
                   IPARA, IRecord, IECODE, I, I2, irsv, NrGates  !itype
      INTEGER      JPARA, IPAR, NRsvPar, NSeq, NSeqPrev, Allocation_error, NrInterpTables
      INTEGER      IflowLoc, IRRLoc, IPrecLoc, IExtLoc, IWqLoc, IParLoc, ITabLoc, iD3DLoc
      Integer      NrDataSubRecords,  NrSobekSubRecords, NrRRSubRecords, NrPrecipSubRecords, &
                   NrExtSubRecords,NrWQSubRecords, NrParaSubRecords, NrInterpSubRecords, NrD3DSubRecords
      Integer      IsubRecord, StartSubRecord, EndSubRecord
      Integer      ILeft, iRight, iTable, NrInterpPoints

      Integer, allocatable ::    iHelp(:)
      REAL                       RDUM(NHLP)
      Double Precision           DDUM(NHLP)
      
      CHARACTER(len=CharIdLength) :: ID, CDUM(NHLP), TableName, RecordType, VariableId
      
      CHARACTER*9999 STRING
      CHARACTER*1      KlTeken
      Integer          IVariable, ipoint, idum1
      Double Precision Coef

      Logical        TabYesNo
      Integer        KeyUppUntilColumn, TableNr, NrColumns
      Character*4    SearchString
      Logical        SearchPar2, success

! Additional variables for ParseToken
      Integer           ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical           ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError
      Type (TokenArray) RecordData


! *********************************************************************

      RetVal = 0
      KlTeken = '<'

      IStart   = 1     ! Scan from token 1
      ScanToTk = 9999  ! Scan up to Token 9999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Rdpara')
      ALLOW = .FALSE.
      Par2Use = .False.
      Par3Use = .False.
      ParRsvUse = .False.
!
! *********************************************************************
! *** Initialisations
! *********************************************************************
! init. max. time shifts per type
      Do I=1,9
         MxTmShift(i) = 0
      Enddo
!
! August 2000: ARS 1357
! Predefine Time parameters (11 stuks)
!     1 = Year
!     2 = Month
!     3 = Day
!     4 = Hour
!     5 = Minute
!     6 = Second
!     7 = Date (YYYYMMDD)
!     8 = Time   (HHMMSS)
!     9 = Date_Time (YYYYMMDD.HHMMSS)
!     10= DayofWeek (0=Sunday, 1=Monday etc)
!     11= ComputationTimestepSize

      ParaId(1 ) = 'Year'
      ParaId(2 ) = 'Month'
      ParaId(3 ) = 'Day'
      ParaId(4 ) = 'Hour'
      ParaId(5 ) = 'Minute'
      ParaId(6 ) = 'Second'
      ParaId(7 ) = 'Date'
      ParaId(8 ) = 'Time'
      ParaId(9 ) = 'Date_Time'
      ParaId(10) = 'DayofWeek'
      ParaId(11) = 'CompTimeStep'

      DO IPARA=1,11
         PARDIM (IPARA,1) = 0     ! flow locs
         PARDIM (IPARA,2) = 0     ! RR locs
         PARDIM (IPARA,3) = 0     ! precip
         PARDIM (IPARA,4) = 0     ! ext
         PARDIM (IPARA,5) = 0     ! WQ
         PARDIM (IPARA,6) = 0     ! parameters
         PARDIM (IPARA,7) = 0     ! RSV
         PARDIM (IPARA,8) = 0     ! interp
         PARDIM (IPARA,9) = 0     ! D3D
         PARTYP (IPARA) = 'Internal'
      Enddo

! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      IPARA = NPARI + 1
      NPAR1 = NParI
      NPAR2 = 0
      NPAR3Para = 0
      NPAR3RSVP = 0
      NrInterpTables = 0

      CALL SKPCOM (IN, ENDFIL,'RTC')
      IF (ENDFIL) goto 21
!    *  CALL ERRMSG (911, 0, 'Rdpara', ' Beslisparameterfile', IOUT1)
!
! *********************************************************************
! *** read data
! *** format: PAR2 id 'id' nm 'name' ns i1 nb i2 np i3 ne i4 nd i5
! *** format: PAR2 id 'id' nm 'name' ns i1 nb i2 np i3 ne i4 nd i5
! ***           DATA ty 'Flowloc' lo 'sobekid' va 1 ca 1. cb 0. cn 0 data
! ***           DATA ty '2DFlowloc' lo 'sobekid' va 1 ca 1. cb 0. cn 0 data
! ***           DATA ty 'RRloc' lo 'RRid' va 1 ca 1. cb 0. cn 0 data
! ***           DATA ty 'PrecipLoc' lo 'Precipid' va 1 ca 1. cb 0. cn 0 data
! ***           DATA ty 'ExtLoc' lo 'Extid' va 1 ca 1. cb 0. cn 0 data
! ***           DATA ty 'ParLoc' lo 'paraid' va 1 ca 1. cb 0. cn 0 data
! ***           DATA ty 'DateTimLoc' lo 'Date' va 1 ca 1. cb 0. cn 0 data
! ***           DATA ty 'InterpolationTable' lo 'Table_id' data
! ***           DATA ty 'D3DLoc' lo 'D3DLocId' data
! ***           do 'operation'
! ***         par2
! *** format: PAR3 id 'id' PDIN 1 1 '365:00:00' pdin
! ***         TBLE
! ***          datetime  value <
! ***          datetime  value <
! ***         tble par3
! *** format: RSVP ng 3 nb 1 nt 1 ns 1 id 'id1''id2' id3' rsv 'rsvname'
! ***              id 'id1' 'id ' 'id3'
! ***              no 2 gb 1 gt 2 gs 2 dm 'dmoutlet1' 'dmoutlet2' mf 'mfoutlet1' 'mfoutlet2'
! ***              'il 'initiallevel' ei 'expectedinflow' rsvp
! ***         rsvp
! ***              with il, ei, and dm keyword referring to other decision parameters
! ***              RsvEdit will determine id's automatically from reservoir name, type of gate and gate number.
! *** format: INTP id 'Interpolation table id'
! ***          TBLE
! ***           0.00 0.00 <
! ***           1.00 0.50 <
! ***           2.00 0.70 <
! ***           3.00 0.80 <
! ***           4.00 0.90 <
! ***          tble intp
! *********************************************************************

! Scan file for interpolation tables
!
      NrInterpTables = 0
      DO IRecord =1,99999
         READ(IN,'(A)',END=9,ERR=150,IOSTAT=IECODE)  STRING
         IF (STRING(1:4) .EQ. 'INTP') NrInterpTables = NrInterpTables + 1
      Enddo
    9 Rewind(in)

! Allocate and read interpolation tables
      If (NrInterpTables .gt. 0) then
         Success = DH_AllocInit (999, nrInterpTables, InterpTableInput, InterpTableOutput, 0D0)
         Success = success .and. DH_AllocInit (NrInterpTables, NrInterpolationPoints, 0)
         Success = success .and. DH_AllocInit (NrInterpTables, InterpolationTableId, ' ')
         If (.not. success)  then
            Call ErrMsg (929, 1, ' RdPara-interpolation tables', ' ', IOUT1)
            RetVal = 929
            Return
         Endif
         ITable = 0
         DO 10 IRecord =1,99999
            READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
            IF (STRING(1:4) .NE. 'INTP') goto 10
            Backspace(in)
            ReadError = .false.
            SearchString = 'INTP'
            Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
            If (Endfil .or. .not. success)  then
               Call ErrMsg (974, 0, 'Rdpara', ' Unexpected end of file ',IOUT1)
               RetVal = 974
               Return
            Else
               Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)
               If (.not. success) then
                 Call ErrMsg (913,0,'Rdpara',' Error getting Table Name',IOUT1)
                 RetVal = 913
                 Return
               Endif
               success = GetStringFromBuffer(String)
               IF (.not. success) then
                  Call ErrMsg (913,0,'Rdpara',' Error getting String from Buffer',IOUT1)
                  RetVal = 913
                  Return
               Endif
               ITable = ITable + 1
               InterpolationTableId (ITable) = TableName
               Ileft  = INDEX(String(1:nbuf), 'TBLE') +4
               Iright = INDEX(String(1:nbuf), 'tble')
               NrInterpPoints = Max (1, CntStr (klteken, String(ileft:iright)) )
               If (NrInterpPoints .gt. 999) then
                    write(*,*) ' Reduce input interpolation table to 999 points '
                    stop ' Reduce nr points in interpolation table '
               Endif
               NrInterpolationPoints(ITable) = NrInterpPoints
                ! get table data: eerst < tekens verwijderen, dan free format inlezen
               Do idum1=ileft, iright
                   if (String(idum1:idum1) .eq. klteken) String(idum1:idum1)=' '
               Enddo
               Read (String(ileft:),*,Err=991)  (InterpTableInput(ipoint,ITable), InterpTableOutput (ipoint,iTable), ipoint=1, NrInterpPoints)
               If (idebug .gt. 0) then
                   write(idebug,*) ' Interpolation table nr and name', ITable, TableName
                   do ipoint=1,NrInterpPoints
                      write(idebug,*) ' Interpolation table', InterpTableInput(ipoint,iTable),InterpTableOutput(Ipoint,iTable)
                   enddo
               Endif
            Endif
            If (ITable .eq. NrInterpTables) goto 11
 10      Enddo
 11      Continue
      Endif
      Rewind(in)


! *********************************************************************
! Read parameters
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL,'RTC')
      IF (ENDFIL) goto 21

      iPara = NparI
      DO 20 IRecord =1,9999
         IPara=IPara+1
         CALL SKPCOM (IN, ENDFIL,'RTC')
         IF (ENDFIL) GOTO 21
   19    CONTINUE
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!        WRITE(*,*) STRING
! skip regel als hij niet begint met juist keyword (PARA, PAR2, RSVP, PAR3)
         IF (STRING(1:4) .NE. 'PARA' .AND.  STRING(1:4) .NE. 'PAR2' .AND. &
              STRING(1:4) .ne. 'RSVP' .AND. STRING(1:4) .ne. 'PAR3') GOTO 19

! *********************************************************************
! Parameter id
! *********************************************************************
         RetVal = GETVAR3 (STRING,' id ',1,' Rdpara',' Beslispar.file',IOUT1, &
                       ID, RDUM(1), IDUM(1), Ddum(1), ALLOW, FOUND, IflRtnRtc)
         IF (RetVal .ne. 0) Return
!       check max. number of dec.variabelen
         IF (IPARA .gt. NDECV) then
            CALL ERRMSG (974,0,'Rdpara',' NDECV in decispar.rtc',IOUT1)
            RetVal = 974
            Return
         Endif

! check that id is not already used as decision parameter id
         DO JPARA=1,IPARA
            IF (PARAID(JPARA) .EQ. ID) THEN
               CALL ERRMSG (927,0, ID, ' Decision parameter file', IOUT1)
               RetVal = 927
               Return
            ENDIF
         ENDDO

         PARAID (IPARA) = ID
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' para_id ',PARAID(IPARA)(1:len_trim(ParaId(Ipara)))

         SearchPar2 = .false.
         IF (STRING(1:4) .EQ. 'PARA') THEN
!            If (Par2Use)  then
!               Call Errmsg (926, 0, 'Rdpara', ' ',IOUT1)
!               RetVal = 926
!               Return
!            Endif
             PARTYP (IPARA) = 'PARA'
             NPAR1 = NPAR1 + 1
         ELSEIF (STRING(1:4) .EQ. 'PAR2') THEN
             NPAR2 = NPAR2 + 1
             PARTYP (IPARA) = 'PAR2'
             Par2Use = .True.
             SearchPar2 = .true.
         ELSEIF (STRING(1:4) .EQ. 'RSVP') THEN
             NPAR3Rsvp = NPAR3Rsvp + 1
             PARTYP (IPARA) = 'RSVP'
             RSVIndx(IPARA) = NPAR3Rsvp
             ParRsvUse = .True.
             Irsv = NPar3Rsvp
             GOTO 192
         ELSEIF (STRING(1:4) .EQ. 'PAR3') THEN
             NPAR3Para = NPAR3Para + 1
             PARTYP (IPARA) = 'PAR3'
             Par3TableIndex(IPARA) = NPAR3Para
             Par3Use = .True.
             allow = .true.
             RetVal = GETVAR3 (STRING,' nm ',1,' Rdpara',' Beslispar.file',IOUT1, &
                               ID, RDUM(1), IDUM(1), Ddum(1), ALLOW, FOUND, IflRtnRtc)
             IF (found) ParaDescr(Ipara) = id
             IF (RetVal .ne. 0) Return
             allow = .false.
             GOTO 193
         ENDIF

! *********************************************************************
! ** Eerste type data records met keyword PARA or PAR2
! *** format: PAR2 id 'id' nm 'name' ns i1 nb i2 np i3 ne i4 nd i5
! ***           DATA ty 'Flowloc' lo 'sobekid' va 1 ca 1. cb 0. cn 0
! ***           DATA ty '2DFlowloc' lo 'sobekid' va 1 ca 1. cb 0. cn 0
! ***           DATA ty 'RRloc' lo 'RRid' va 1 ca 1. cb 0. cn 0
! ***           DATA ty 'PrecipLoc' lo 'Precipid' va 1 ca 1. cb 0. cn 0
! ***           DATA ty 'ExtLoc' lo 'Extid' va 1 ca 1. cb 0. cn 0
! ***           DATA ty 'WQLoc' lo 'WQid' va 1 ca 1. cb 0. cn 0
! ***           DATA ty 'ParLoc' lo 'paraid' va 1 ca 1. cb 0. cn 0
! ***           DATA ty 'DateTimLoc' lo 'Date' va 1 ca 1. cb 0. cn 0
! ***           DATA ty 'InterpolationTable' lo 'Table_id' data
! ***           DATA ty 'D3DLoc' lo 'D3DLocId' data
! ***           do 'operation'
! ***
! ***       old:   si 'Sobek_ids' sv var1 sc coef1 sa add1 st int1 (alles i1 keer)
! ***              3b '3B_ids' 3v var2 3c coef2 3a add2 3t int2    (alles i2 keer)
! ***              pi 'Prec_ids' pv var3 pc coef3 pa add3 pt int3  (alles i3 keer)
! ***              ei 'Ext_ids' ev var4 ec coef4 ea add4 et int4   (alles i4 keer)
! ***              di 'Ext_ids'         dc coef5 da add5 dt int5   (alles i5 keer)
! ***         par2
! **
! *********************************************************************
! Aantal Sobek, 3B, precipitation, externe lokaties; beslisparameters
! *********************************************************************
! use Getrecord and ParseToken
         Backspace(in)
         ReadError = .false.
         SearchString = 'PARA'
         If (SearchPar2) SearchString = 'PAR2'
         PARDIM (IPARA,1) = 0
         PARDIM (IPARA,2) = 0
         PARDIM (IPARA,3) = 0
         PARDIM (IPARA,4) = 0
         PARDIM (IPARA,5) = 0
         PARDIM (IPARA,6) = 0
         PARDIM (IPARA,7) = 0
         PARDIM (IPARA,8) = 0
         PARDIM (IPARA,9) = 0
         Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
         If (Endfil .or. .not. success)  then
            Call ErrMsg (974, 0, 'Rdpara', ' Unexpected end of file ',IOUT1)
            RetVal = 974
            Return
         Endif
         String = ' '
         Success = GetStringFromBuffer (String)
         if (.not. Success) ReadError = .true.
         Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
         If (.not. Success) ReadError = .true.
         IF (ReadError) then
            Call ErrMsg (974,0,'Rdpara',' Read error during reading decision parameter file - PARA-PAR2 ParseToken ',IOUT1)
            RetVal = 974
            Return
         Endif
         If (Idebug .gt. 0) then
            Write(Idebug,'(A)') String(1:len_trim(String))
            Write(Idebug,*) ' Results of ParseToken '
            Write(idebug,*) ' Nr  StartPos  Quotes Token '
            Do i=1,NumberofTokens
               write(idebug,'(I3,I5,L,1X,A)') i, RecordData%StartPositionOfToken(i), &
                                              RecordData%IsEnclosedByQuotes(i), RecordData%Token(i)
            Enddo
         Endif

! optional name
         iStart = 1
         success = SetOneVariable('nm',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, ParaDescr(Ipara))
! optional initial value
         if (SetOneVariable('iv',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, DcvVal(Ipara,1))) then
             !
             ! If the user sets initial values then we will need to allow those initial values to be used.
             !
             UseInitValues = 1
         endif
! operation
         CDum(1) = ' '
!         if (GetKey('do', IStart, RecordData,  NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
!            Cdum(1) = RecordData%Token(ReturnIndx+1)
!         else
!            ReadError = .true.
!         endif

         success = SetOneVariable('do',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, Cdum(1))
         CALL UPPERC (CDUM(1))
         IF (CDUM(1) .EQ. 'ADD' .or. CDum(1) .eq. 'NONE') THEN       ! none put by UI in case of 1 DATA subrecord only
           PAROPER(IPARA) = 1
         ELSEIF (CDUM(1) .EQ. 'SUBTRACT' .or. CDUM(1) .EQ. 'SUBSTRACT') THEN
           PAROPER(IPARA) = 2
         ELSEIF (CDUM(1) .EQ. 'MULTIPLY') THEN
           PAROPER(IPARA) = 3
         ELSEIF (CDUM(1) .EQ. 'DIVIDE') THEN
           PAROPER(IPARA) = 4
         ELSEIF (CDUM(1) .EQ. 'MAX') THEN
           PAROPER(IPARA) = 5
         ELSEIF (CDUM(1) .EQ. 'MIN') THEN
           PAROPER(IPARA) = 6
         ELSEIF (CDUM(1) .EQ. 'AVERAGE') THEN
           PAROPER(IPARA) = 7
         ELSEIF (CDUM(1) .EQ. 'POWER') THEN
           PAROPER(IPARA) = 8
         ELSEIF (CDUM(1) .EQ. 'SIN(RADIANS)') THEN
           PAROPER(IPARA) = 9
         ELSEIF (CDUM(1) .EQ. 'SIN(DEGREES)') THEN
           PAROPER(IPARA) = 10
         ELSEIF (CDUM(1) .EQ. 'COS(RADIANS)') THEN
           PAROPER(IPARA) = 11
         ELSEIF (CDUM(1) .EQ. 'COS(DEGREES)') THEN
           PAROPER(IPARA) = 12
         ELSEIF (CDUM(1) .EQ. 'TAN(RADIANS)') THEN
           PAROPER(IPARA) = 13
         ELSEIF (CDUM(1) .EQ. 'TAN(DEGREES)') THEN
           PAROPER(IPARA) = 14
         ELSEIF (CDUM(1) .EQ. 'ARCSIN(RADIANS)') THEN
           PAROPER(IPARA) = 15
         ELSEIF (CDUM(1) .EQ. 'ARCSIN(DEGREES)') THEN
           PAROPER(IPARA) = 16
         ELSEIF (CDUM(1) .EQ. 'ARCCOS(RADIANS)') THEN
           PAROPER(IPARA) = 17
         ELSEIF (CDUM(1) .EQ. 'ARCCOS(DEGREES)') THEN
           PAROPER(IPARA) = 18
         ELSEIF (CDUM(1) .EQ. 'ARCTAN(RADIANS)') THEN
           PAROPER(IPARA) = 19
         ELSEIF (CDUM(1) .EQ. 'ARCTAN(DEGREES)') THEN
           PAROPER(IPARA) = 20
         ELSEIF (CDUM(1) .EQ. 'FLOOR') THEN
           PAROPER(IPARA) = 21
         ELSEIF (CDUM(1) .EQ. 'CEIL') THEN
           PAROPER(IPARA) = 22
         ELSEIF (CDUM(1) .EQ. 'NINT') THEN
           PAROPER(IPARA) = 23
         ELSEIF (CDUM(1) .EQ. 'SQUARE') THEN
           PAROPER(IPARA) = 24
         ELSEIF (CDUM(1) .EQ. 'SQRT') THEN
           PAROPER(IPARA) = 25
         ELSEIF (CDUM(1) .EQ. 'EXP') THEN
           PAROPER(IPARA) = 26
         ELSEIF (CDUM(1) .EQ. 'LOG') THEN
           PAROPER(IPARA) = 27
         ELSEIF (CDUM(1) .EQ. 'LOG10') THEN
           PAROPER(IPARA) = 28
         ELSEIF (CDUM(1) .EQ. 'SINH') THEN
           PAROPER(IPARA) = 29
         ELSEIF (CDUM(1) .EQ. 'COSH') THEN
           PAROPER(IPARA) = 30
         ELSEIF (CDUM(1) .EQ. 'TANH') THEN
           PAROPER(IPARA) = 31
         ELSEIF (CDUM(1) .EQ. 'INTERPOLATE' .OR. CDUM(1) .EQ. 'INTERPOLATION') THEN
           PAROPER(IPARA) = 32
         ELSE
!           no proper value set for do keyword
            ReadError = .true.
         ENDIF
         IF (IDEBUG .GT. 0)  WRITE(IDEBUG,*) ' Paroper', PAROPER(IPARA)
         IF (ReadError) then
            Call ErrMsg (974,0,'Rdpara',' Read error during reading PAR2 decision parameter record',IOUT1)
            RetVal = 974
         Endif
         If (RetVal .ne. 0) Return

! find number of subrecords of each type

! Find the number of DATA subrecords within the String
         NrDataSubRecords = CntStr (' DATA ', String)
         NrSobekSubRecords = CntStr ('FlowLoc', String)     ! this already includes the 2DFlowLoc subrecords!!
         NrRRSubRecords = CntStr ('RRLoc', String) + CntStr('RRloc', String)  ! added because of case sensitivity RRloc written by UI
         NrPrecipSubRecords = CntStr ('PrecipLoc', String)
         NrExtSubRecords = CntStr ('ExtLoc', String)
         NrWQSubRecords = CntStr ('WQLoc', String)
         NrParaSubRecords = CntStr ('ParLoc', String) + CntStr ('DateTimLoc', String)
         NrInterpSubRecords = CntStr ('InterpolationTable', String)
         NrD3DSubRecords = CntStr ('D3DLoc', String)
         Pardim(Ipara,1) = NrSobekSubRecords
         Pardim(Ipara,2) = NrRRSubRecords
         Pardim(Ipara,3) = NrPrecipSubRecords
         Pardim(Ipara,4) = NrExtSubRecords
         Pardim(Ipara,5) = NrWQSubRecords
         Pardim(Ipara,6) = NrParaSubRecords
         Pardim(Ipara,8) = NrInterpSubRecords
         Pardim(Ipara,9) = NrD3DSubRecords
         If (NrDataSubRecords .ne. NrSobekSubRecords + NrRRSubRecords + NrPrecipSubRecords + NrExtSubRecords + &
                                    NrWQSubRecords + NrParaSubRecords + NrInterpSubRecords + NrD3DSubRecords) then
            Call ErrMsg (974,0,'Rdpara',' PAR2 record contains inconsistent subrecords ',IOUT1)
            RetVal = 974
            Return
         Endif
         ALLOW = .FALSE.
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Pardim', (PARDIM(IPARA,I),I=1,9)

! *********************************************************************
! **  check dimensies
! *********************************************************************
      IF (PARDIM(IPARA,1) .GT. NSPAR) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' NSPAR Sobek lokaties', IOUT1)
        RetVal = 913
      ELSEIF (PARDIM(IPARA,2) .GT. N3PAR) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' N3PAR 3B lokaties', IOUT1)
        RetVal = 913
      ELSEIF (PARDIM(IPARA,3) .GT. NPPAR) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' NPPAR neerslag lokaties',IOUT1)
        RetVal = 913
      ELSEIF (PARDIM(IPARA,4) .GT. NEPAR) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' NEPAR externe lokaties',IOUT1)
        RetVal = 913
      ELSEIF (PARDIM(IPARA,5) .GT. NQPAR) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' NQPAR beslisparameters',IOUT1)
        RetVal = 913
      ELSEIF (PARDIM(IPARA,6) .GT. NDPAR) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' NDPAR beslisparameters',IOUT1)
        RetVal = 913
      ELSEIF (PARDIM(IPARA,8) .GT. 1) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' Interpolation references',IOUT1)
        RetVal = 913
      ELSEIF (PARDIM(IPARA,9) .GT. N3DPAR) THEN
        CALL ERRMSG (913, 0, 'Rdpara', ' D3DFlow locations',IOUT1)
        RetVal = 913
      ENDIF
      If (RetVal .ne. 0) Return

! *********************************************************************
! **  check met Settings:  UseSbk, Use3B, UsePre, UseExt, UseWQ, Use3D
! *********************************************************************

      IF (PARDIM(IPARA,3) .GT. 0) Then
         UsePre = .true.
         UseP   = .true.
      Endif
      IF (PARDIM(IPARA,4) .GT. 0) Then
         UseExt = .true.
         UseW   = .true.
      Endif
      IF (PARDIM(IPARA,5) .GT. 0) Then
         UseWQ = .true.
      Endif
      IF (PARDIM(IPARA,9) .GT. 0) Then
         Use3D = .true.
      Endif

      IF (PARDIM(IPARA,1) .GT. 0 .AND. .NOT. USESBK) THEN
        CALL ERRMSG (918, 0, 'Rdpara', ' Sobek ', IOUT1)
        RetVal = 918
      ELSEIF (PARDIM(IPARA,2) .GT. 0 .AND. .NOT. USE3B) THEN
        CALL ERRMSG (918, 0, 'Rdpara', ' 3B    ', IOUT1)
        RetVal = 918
      ELSEIF (PARDIM(IPARA,3) .GT. 0 .AND. .NOT. USEPRE) THEN
        CALL ERRMSG (918, 0, 'Rdpara', ' Precipitation ', IOUT1)
        RetVal = 918
      ELSEIF (PARDIM(IPARA,4) .GT. 0 .AND. .NOT. USEEXT) THEN
        CALL ERRMSG (918, 0, 'Rdpara', ' External data ', IOUT1)
        RetVal = 918
      ELSEIF (PARDIM(IPARA,5) .GT. 0 .AND. .NOT. USEWQ) THEN
        CALL ERRMSG (918, 0, 'Rdpara', ' External data ', IOUT1)
        RetVal = 918
      ELSEIF (PARDIM(IPARA,9) .GT. 0 .AND. .NOT. USE3D) THEN
        CALL ERRMSG (918, 0, 'Rdpara', ' D3DFlow data ', IOUT1)
        RetVal = 918
      ENDIF
      If (RetVal .ne. 0) Return

! *********************************************************************
! Process DATA record
! *********************************************************************
! Loop over the DATA subrecords

         iflowloc = 0
         irrloc   = 0
         iprecloc = 0
         iextloc  = 0
         iwqloc   = 0
         iparloc  = 0
         itabloc  = 0
         id3dloc  = 0
         iStart = 1
         StartSubRecord = 1
         EndSubRecord = NumberOfTokens
         Do iSubRecord = 1, NrDataSubRecords
            if (Getkey ('DATA', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               StartSubRecord = ReturnIndx
            endif
            if (Getkey ('data', StartSubRecord+1, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               EndSubRecord = ReturnIndx
            endif
! parameter type
            if (Getkey ('ty', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                RecordType = RecordData%Token(ReturnIndx+1)
            else
                ReadError = .true.
            endif
! location id's
            if (Getkey ('lo', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                If (RecordType .eq. 'FlowLoc' .or. RecordType .eq. '2DFlowLoc') then
                   iflowloc = iflowloc + 1
                   ParSbk(ipara,iflowloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 1
                   ParOrder(ipara,Isubrecord,2) = iflowloc
                ElseIf (RecordType .eq. 'RRLoc' .or. RecordType .eq. 'RRloc') then
                   irrloc = irrloc + 1
                   Par3B(ipara,irrloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 2
                   ParOrder(ipara,Isubrecord,2) = irrloc
                ElseIf (RecordType .eq. 'PrecipLoc') then
                   iprecloc = iprecloc + 1
                   ParPre(ipara,iprecloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 3
                   ParOrder(ipara,Isubrecord,2) = iprecloc
                ElseIf (RecordType .eq. 'ExtLoc') then
                   iextloc = iextloc + 1
                   ParExt(ipara,iextloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 4
                   ParOrder(ipara,Isubrecord,2) = iextloc
                ElseIf (RecordType .eq. 'WQLoc') then
                   iwqloc = iwqloc + 1
                   ParWQ(ipara,iwqloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 5
                   ParOrder(ipara,Isubrecord,2) = iwqloc
                ElseIf (RecordType .eq. 'ParLoc' .or. RecordType .eq. 'DateTimLoc') then
                   iparloc = iparloc + 1
                   ParPar(ipara,iparloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 6
                   ParOrder(ipara,Isubrecord,2) = iparloc
                ElseIf (RecordType .eq. 'InterpolationTable') then
                   iTabloc = iTabloc + 1
                   ParInterp(ipara,itabloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 8
                   ParOrder(ipara,Isubrecord,2) = itabloc
                ElseIf (RecordType .eq. 'D3DLoc' .or. RecordType .eq. 'D3Dloc') then
                   id3dloc = id3dloc + 1
                   Par3D(ipara,id3dloc) = RecordData%Token(ReturnIndx+1)
                   ParOrder(ipara,Isubrecord,1) = 9
                   ParOrder(ipara,Isubrecord,2) = id3dloc
                endif
            else
                ReadError = .true.
            endif
! variable or variable id
            if (Getkey ('va', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                If (RecordData%IsEnclosedByQuotes(ReturnIndx+1)) then
                   VariableId = RecordData%Token(ReturnIndx+1)
                   ivariable = -1
                else
                   Read (RecordData%Token(ReturnIndx+1),*,Err=991)   ivariable
                   VariableId = ' '
                endif
                If (ivariable .ne. -1) then
                    If (RecordType .eq. 'FlowLoc' .or. RecordType .eq. '2DFlowLoc') then
                        VarSbk(Ipara,iflowloc) = ivariable
                    ElseIf (RecordType .eq. 'RRLoc' .or. RecordType .eq. 'RRloc') then
                        Var3B(Ipara,irrloc) = ivariable
                    ElseIf (RecordType .eq. 'PrecipLoc') then
                        VarPre(Ipara,iprecloc) = ivariable
                    ElseIf (RecordType .eq. 'ExtLoc') then
                        VarExt(Ipara,iextloc) = ivariable
                    ElseIf (RecordType .eq. 'WQLoc') then
                        VarWq(Ipara,iwqloc) = ivariable
                    ElseIf (RecordType .eq. 'ParLoc' .or. RecordType .eq. 'DateTimLoc') then
!                        do nothing
                    ElseIf (RecordType .eq. 'InterpolationTable') then
! NOV 2006               ReadError = .true.
                    ElseIf (RecordType .eq. 'D3DLoc' .or. RecordType .eq. 'D3Dloc') then
                        Var3D(Ipara,id3dloc) = ivariable
                    endif
                ElseIf (VariableId .ne. ' ') then
                    If (RecordType .eq. 'FlowLoc' .or. RecordType .eq. '2DFlowLoc') then
                        VarIdSbk(Ipara,iflowloc) = VariableId
                    ElseIf (RecordType .eq. 'RRLoc' .or. RecordType .eq. 'RRloc') then
                        VarId3B(Ipara,irrloc) = VariableId
                    ElseIf (RecordType .eq. 'WQLoc') then
                        VarIdWq(Ipara,iwqloc) = VariableId
                    ElseIf (RecordType .eq. 'InterpolationTable') then
                        VarIdInterpolationTable(Ipara,itabloc) = VariableId
                    ElseIf (RecordType .eq. 'D3DLoc' .or. RecordType .eq. 'D3Dloc') then
                        VarId3D(Ipara,id3dloc) = VariableId
                    endif
                Endif
            else
                ReadError = .true.
            endif
! multiply coefficient
            if (Getkey ('ca', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                Read (RecordData%Token(ReturnIndx+1),*,Err=991)   coef
                If (RecordType .eq. 'FlowLoc' .or. RecordType .eq. '2DFlowLoc') then
                    CfSbk(Ipara,iflowloc,1) = coef
                ElseIf (RecordType .eq. 'RRLoc' .or. RecordType .eq. 'RRloc') then
                    Cf3B(Ipara,irrloc,1) =  coef
                ElseIf (RecordType .eq. 'PrecipLoc') then
                    CfPre(Ipara,iprecloc,1) = coef
                ElseIf (RecordType .eq. 'ExtLoc') then
                    CfExt(Ipara,iextloc,1) = coef
                ElseIf (RecordType .eq. 'WQLoc') then
                    CfWQ(Ipara,iwqloc,1) =  coef
                ElseIf (RecordType .eq. 'ParLoc' .or. RecordType .eq. 'DateTimLoc') then
                    CfPar(Ipara,iparloc,1) = coef
                ElseIf (RecordType .eq. 'D3DLoc' .or. RecordType .eq. 'D3Dloc') then
                    Cf3D(Ipara,id3dloc,1) =  coef
                endif
            else
                If (RecordType .eq. 'InterpolationTable') then
                   CfPar(Ipara,itabloc,1) = 1.0
                else
                   ReadError = .true.
                endif
            endif
! add coefficient
            if (Getkey ('cb', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                Read (RecordData%Token(ReturnIndx+1),*,Err=991)   coef
                If (RecordType .eq. 'FlowLoc' .or. RecordType .eq. '2DFlowLoc') then
                    CfSbk(Ipara,iflowloc,2) = coef
                ElseIf (RecordType .eq. 'RRLoc' .or. RecordType .eq. 'RRloc') then
                    Cf3B(Ipara,irrloc,2) =  coef
                ElseIf (RecordType .eq. 'PrecipLoc') then
                    CfPre(Ipara,iprecloc,2) = coef
                ElseIf (RecordType .eq. 'ExtLoc') then
                    CfExt(Ipara,iextloc,2) = coef
                ElseIf (RecordType .eq. 'WQLoc') then
                    CfWQ(Ipara,iwqloc,2) =  coef
                ElseIf (RecordType .eq. 'ParLoc' .or. RecordType .eq. 'DateTimLoc') then
                    CfPar(Ipara,iparloc,2) = coef
                ElseIf (RecordType .eq. 'D3DLoc' .or. RecordType .eq. 'D3DRloc') then
                    Cf3D(Ipara,id3dloc,2) =  coef
                endif
            else
                If (RecordType .eq. 'InterpolationTable') then
                    CfPar(Ipara,itabloc,2) = 0.0
                else
                    ReadError = .true.
                endif
            endif
! time shift
            if (Getkey ('cn', StartSubRecord, RecordData, EndSubRecord, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                Read (RecordData%Token(ReturnIndx+1),*,Err=991)   ivariable
                If (RecordType .eq. 'FlowLoc' .or. RecordType .eq. '2DFlowLoc') then
                    TisSbk(Ipara,iflowloc) = ivariable
                ElseIf (RecordType .eq. 'RRLoc' .or. RecordType .eq. 'RRloc') then
                    Tis3B(Ipara,irrloc) = ivariable
                ElseIf (RecordType .eq. 'PrecipLoc') then
                    TisPre(Ipara,iprecloc) = ivariable
                ElseIf (RecordType .eq. 'ExtLoc') then
                    TisExt(Ipara,iextloc) = ivariable
                ElseIf (RecordType .eq. 'WQLoc') then
                    TisWq(Ipara,iwqloc) = ivariable
                ElseIf (RecordType .eq. 'ParLoc' .or. RecordType .eq. 'DateTimLoc') then
                    TisPar(Ipara,iparloc) = ivariable
                ElseIf (RecordType .eq. 'InterpolationTable') then
!                   do nothing
                    TisPar(Ipara,itabloc) = 0
                ElseIf (RecordType .eq. 'D3DLoc' .or. RecordType .eq. 'D3Dloc') then
                    Tis3D(Ipara,id3dloc) = ivariable
                endif
            else
                ReadError = .true.
            endif
            iStart = EndSubRecord+1

         Enddo          ! end of DATA subrecord loop

!        read error handling
         Goto 992
   991   continue
         ReadError = .true.
   992   continue
         IF (ReadError) then
            Call ErrMsg (974,0,'Rdpara',' Read error during reading decision parameter record',IOUT1)
            RetVal = 974
            Return
         Endif

! *********************************************************************
! Check existence Sobek-lokaties in HIS files/invoerfile Sobek;
! Check validiteit his file indices en time shifts
! *********************************************************************

         DO I=1,PARDIM(IPARA,1)
           IXSBHIS(IPARA,I) = 0
           DO I2=1,NSOBEK
              IF (PARSBK(IPARA,I) .EQ. ID_SBK(I2)) THEN
                 IXSBHIS(IPARA,I) = I2
              ENDIF
           ENDDO
           IF (IXSBHIS(IPARA,I) .LE. 0) THEN
               WRITE(IOUT1,*) ' Sbk-Parameter id',PARSBK(IPARA,I)
               WRITE(IOUT1,*) ' Sobek ids',(ID_SBK(I2),I2=1,NSOBEK)
               WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
               CALL ERRMSG (917,0, PARSBK(IPARA,I), ' Beslisparameterfile', IOUT1)
               RetVal = 917
           ENDIF
         ENDDO
         If (RetVal .ne. 0) Return

         DO I=1,PARDIM(IPARA,1)
            IF (TISSBK(IPARA,I) .LT. NTIMSH) THEN
                TISSBK(IPARA,I) = NTIMSH
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (915,NTIMSH, 'Rdpara', ' Beslisparameterfile', IOUT1)  ! is only a warning, so no return value
            ENDIF
            IF (TISSBK(IPARA,I) .GT. 0) THEN
                TISSBK(IPARA,I) = 0
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (923,0, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 923
            ENDIF
            If (RetVal .ne. 0) Return
            MxTmShift(1) = Min (MxTmShift(1), Tissbk(Ipara,I))
         ENDDO

         DO I=1,PARDIM(IPARA,1)
           IF (VARIDSBK(IPARA,I) .eq. '') Then
             IF (VARSBK(IPARA,I) .GT. NPARS .or. VarSbk(Ipara,i) .le. 0) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (916, NPARS, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 916
             ENDIF
             If (RetVal .ne. 0) Return
           ENDIF
         ENDDO

! *********************************************************************
! check existence 3B-lokaties in HIS files/invoerfile 3B
! check validiteit HIS file indices en time shifts
! *********************************************************************

         DO I=1,PARDIM(IPARA,2)
           IX3BHIS(IPARA,I) = 0
           DO I2=1,ND3BID
              IF (PAR3B (IPARA,I) .EQ. ID_D3B (I2))  THEN
                 IX3BHIS(IPARA,I) = I2
              ENDIF
           ENDDO
           IF (IX3BHIS(IPARA,I) .LE. 0) THEN
               WRITE(IOUT1,*) ' RR-parameter id',PAR3B (IPARA,I)
               WRITE(IOUT1,*) ' RR-ids',(ID_D3B(I2),I2=1,ND3BID)
               WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
               CALL ERRMSG (917,0, PAR3B(IPARA,I), ' Beslisparameterfile', IOUT1)
               RetVal = 917
           ENDIF
         ENDDO
         If (RetVal .ne. 0) Return

         DO I=1,PARDIM(IPARA,2)
            IF (TIS3B (IPARA,I) .LT. NTIMSH) THEN
                TIS3B (IPARA,I) = NTIMSH
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (915,NTIMSH, 'Rdpara', ' Beslisparameterfile', IOUT1)
            ENDIF
            IF (TIS3B (IPARA,I) .GT. 0) THEN
                TIS3B (IPARA,I) = 0
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (923,0, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 923
            ENDIF
            If (RetVal .ne. 0) Return
            MxTmShift(2) = Min (MxTmShift(2), Tis3B (Ipara,I))
         ENDDO

         DO I=1,PARDIM(IPARA,2)
           IF (VARID3B(IPARA,I) .eq. '') Then
             IF (VAR3B (IPARA,I) .GT. NPAR3 .or. Var3B(Ipara,i) .le. 0) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (916, NPAR3, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 916
             ENDIF
             If (RetVal .ne. 0) Return
           ENDIF
         ENDDO

! *********************************************************************
! check existence neerslag-lokaties in HIS files/invoerfile neerslag
! check validiteit HIS file indices en time shifts
! *********************************************************************

         DO I=1,PARDIM(IPARA,3)
           IXPRHIS(IPARA,I) = 0
           DO I2=1,NPRECP
              IF (PARPRE(IPARA,I) .EQ. ID_PRE (I2))  THEN
                 IXPRHIS(IPARA,I) = I2
              ENDIF
           ENDDO
           IF (IXPRHIS(IPARA,I) .LE. 0) THEN
               WRITE(IOUT1,*) ' Prediction para.id', PARPRE(IPARA,I)
               WRITE(IOUT1,*) ' Prediction id',(ID_PRE(I2),I2=1,NPRECP)
               WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
               CALL ERRMSG (917,0, PARPRE(IPARA,I), ' Beslisparameterfile', IOUT1)
               RetVal = 917
           ENDIF
         ENDDO
         If (RetVal .ne. 0) Return

         DO I=1,PARDIM(IPARA,3)
            IF (TISPRE(IPARA,I) .LT. NTIMSH) THEN
                TISPRE(IPARA,I) = NTIMSH
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (915,NTIMSH, 'Rdpara',' Beslisparameterfile', IOUT1)
            ENDIF
            IF (TISPRE(IPARA,I) .GT. 0) THEN
                TISPRE(IPARA,I) = 0
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (923,0, 'Rdpara',  ' Beslisparameterfile', IOUT1)
                RetVal = 923
            ENDIF
            If (RetVal .ne. 0) Return
            MxTmShift(3) = Min (MxTmShift(3), TisPre(Ipara,I))
         ENDDO

         DO I=1,PARDIM(IPARA,3)
            IF (VARPRE(IPARA,I) .GT. NPARP .or. VarPre(Ipara,i) .le. 0) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (916, NPARP, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 916
            ENDIF
            IF (VARPRE(IPARA,I) .GT. NTIMHP) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (939, NTIMHP, 'Rdpara',ParaId(ipara), IOUT1)
                RetVal = 939
            ENDIF
            If (RetVal .ne. 0) Return
         ENDDO

! *********************************************************************
! check existence externe-lokaties in HIS files/invoerfile externe data
! check validiteit HIS file indices en time shifts
! *********************************************************************

         DO I=1,PARDIM(IPARA,4)
           IXEXHIS(IPARA,I) = 0
           DO I2=1,NEXT+NEXTHD
              IF (PAREXT(IPARA,I) .EQ. ID_EXT (I2))  THEN
                 IXEXHIS(IPARA,I) = I2
              ENDIF
           ENDDO
           IF (IXEXHIS(IPARA,I) .LE. 0) THEN
               WRITE(IOUT1,*) ' Ext. parameter id',PAREXT(IPARA,I)(1:len_trim(ParExt(Ipara,I)))
               WRITE(IOUT1,*) ' Ext. ids',(ID_EXT(I2)(1:len_trim(ID_Ext(I2))),I2=1,NEXT+NEXTHD)
               WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
               CALL ERRMSG (917,0, PAREXT(IPARA,I), ' Beslisparameterfile', IOUT1)
               RetVal = 917
           ENDIF
         ENDDO
         If (RetVal .ne. 0) Return

         DO I=1,PARDIM(IPARA,4)
            IF (TISEXT(IPARA,I) .LT. NTIMSH) THEN
                TISEXT(IPARA,I) = NTIMSH
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (915,NTIMSH, 'Rdpara', ' Beslisparameterfile', IOUT1)
            ENDIF
            IF (TISEXT(IPARA,I) .GT. 0) THEN
                TISEXT(IPARA,I) = 0
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (923,0, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 923
            ENDIF
            If (RetVal .ne. 0) Return
            MxTmShift(4) = Min (MxTmShift(4), TisExt(Ipara,I))
         ENDDO

         DO I=1,PARDIM(IPARA,4)
            IF (VAREXT(IPARA,I) .GT. NPARE .or. VarExt(Ipara,i) .le. 0) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (916, NPARE, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 916
            ENDIF
            IF ((VAREXT(IPARA,I)+1)/2 .GT. NTIMHW) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (940, NTIMHW, 'Rdpara',ParaId(ipara), IOUT1)
                RetVal = 940
            ENDIF
            If (RetVal .ne. 0) Return
         ENDDO

! *********************************************************************
! Check existence WQ-lokaties in HIS files/invoerfile Sobek;
! Check validiteit his file indices en time shifts
! *********************************************************************

         DO I=1,PARDIM(IPARA,5)
           IXWQHIS(IPARA,I) = 0
           DO I2=1,NSOBWQ
              IF (PARWQ(IPARA,I) .EQ. ID_SWQ(I2)) THEN
                 IXWQHIS(IPARA,I) = I2
              ENDIF
           ENDDO
           IF (IXWQHIS(IPARA,I) .LE. 0) THEN
               WRITE(IOUT1,*) ' WQ-Parameter id',PARWQ(IPARA,I)
               WRITE(IOUT1,*) ' WQ ids',(ID_SWQ(I2),I2=1,NSOBWQ)
               WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
               CALL ERRMSG (917,0, PARWQ(IPARA,I), ' Beslisparameterfile', IOUT1)
               RetVal = 917
           ENDIF
         ENDDO
         If (RetVal .ne. 0) Return

         DO I=1,PARDIM(IPARA,5)
            IF (TISWQ (IPARA,I) .LT. NTIMSH) THEN
                TISWQ (IPARA,I) = NTIMSH
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (915,NTIMSH, 'Rdpara', ' Beslisparameterfile', IOUT1)
            ENDIF
            IF (TISWQ (IPARA,I) .GT. 0) THEN
                TISWQ (IPARA,I) = 0
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (923,0, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 923
            ENDIF
            If (RetVal .ne. 0) Return
            MxTmShift(5) = Min (MxTmShift(5), TisWq (Ipara,I))
         ENDDO

         DO I=1,PARDIM(IPARA,5)
           IF (VARIDWQ(IPARA,I) .eq. '') Then
             IF (VARWQ (IPARA,I) .GT. NPARQ .or. VarWQ (Ipara,i) .le. 0) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (916, NPARQ, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 916
             ENDIF
             If (RetVal .ne. 0) Return
           ENDIF
         ENDDO

! *********************************************************************
! check existence D3D-lokaties in HIS files/invoerfile D3D
! check validiteit HIS file indices en time shifts
! *********************************************************************

         DO I=1,PARDIM(IPARA,9)
           IX3DHIS(IPARA,I) = 0
           DO I2=1,ND3D
              IF (PAR3D (IPARA,I) .EQ. ID_D3D (I2))  THEN
                 IX3DHIS(IPARA,I) = I2
              ENDIF
           ENDDO
           IF (IX3DHIS(IPARA,I) .LE. 0) THEN
               WRITE(IOUT1,*) ' D3DFlow-parameter id',PAR3D (IPARA,I)
               WRITE(IOUT1,*) ' D3D-ids',(ID_D3D(I2),I2=1,ND3D)
               WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
               CALL ERRMSG (917,0, PAR3D(IPARA,I), ' Beslisparameterfile', IOUT1)
               RetVal = 917
           ENDIF
         ENDDO
         If (RetVal .ne. 0) Return

         DO I=1,PARDIM(IPARA,9)
            IF (TIS3D (IPARA,I) .LT. NTIMSH) THEN
                TIS3D (IPARA,I) = NTIMSH
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (915,NTIMSH, 'Rdpara', ' Beslisparameterfile', IOUT1)
            ENDIF
            IF (TIS3D (IPARA,I) .GT. 0) THEN
                TIS3D (IPARA,I) = 0
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (923,0, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 923
            ENDIF
            If (RetVal .ne. 0) Return
            MxTmShift(9) = Min (MxTmShift(9), Tis3D (Ipara,I))
         ENDDO

         DO I=1,PARDIM(IPARA,9)
           IF (VARID3D(IPARA,I) .eq. '') Then
             IF (VAR3D (IPARA,I) .GT. NPAR3D .or. Var3D(Ipara,i) .le. 0) THEN
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (916, NPAR3D, 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 916
             ENDIF
             If (RetVal .ne. 0) Return
           ENDIF
         ENDDO


! *********************************************************************
! check existence referred decision parameters is done later
! check validiteit time shifts PARA record
! *********************************************************************
         If (.not. SearchPar2) Then
           DO I=1,PARDIM(IPARA,6)
              IF (TISPAR(IPARA,I) .LT. NTIMSH) THEN
                  TISPAR(IPARA,I) = NTIMSH
                  WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                  CALL ERRMSG (915,NTIMSH, 'Rdpara',' Beslisparameterfile', IOUT1)
              ENDIF
              IF (TISPAR(IPARA,I) .GE. 0) THEN
                  TISPAR(IPARA,I) = -1
                  WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                  CALL ERRMSG (923, -1, 'Rdpara', ' Beslisparameterfile', IOUT1)
                  RetVal = 923
              ENDIF
              If (RetVal .ne. 0) Return
              MxTmShift(6) = Min (MxTmShift(6), TisPar(Ipara,I))
           ENDDO
         Else
! *********************************************************************
! check existence of decision parameters is done later
! check validiteit time shifts
! *********************************************************************
          DO I=1,PARDIM(IPARA,6)
            IF (TISPAR(IPARA,I) .LT. NTIMSH) THEN
                TISPAR(IPARA,I) = NTIMSH
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (915,NTIMSH, 'Rdpara', ' Beslisparameterfile', IOUT1)
            ENDIF
            IF (TISPAR(IPARA,I) .GT. 0) THEN
                TISPAR(IPARA,I) = 0
                WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
                CALL ERRMSG (923, 0 , 'Rdpara', ' Beslisparameterfile', IOUT1)
                RetVal = 923
            ENDIF
            If (RetVal .ne. 0) Return
            MxTmShift(6) = Min (MxTmShift(6), TisPar(Ipara,I))
          ENDDO
        Endif

! *********************************************************************
! check existence interpolation tables
! *********************************************************************
         DO I=1,PARDIM(IPARA,8)
           IXInterpTable(IPARA,I) = 0
           DO I2=1,NrInterpTables
              IF (PARInterp(IPARA,I) .EQ. InterpolationTableId(I2))  THEN
                 IXInterpTable(IPARA,I) = I2
              ENDIF
           ENDDO
           IF (IXInterpTable(IPARA,I) .LE. 0) THEN
               WRITE(IOUT1,*) ' Interpolation para.id', PARInterp(IPARA,I)
               WRITE(IOUT1,*) ' InterpolationTable id',(InterpolationTableId(I2),I2=1,NrInterpTables)
               WRITE(IOUT1,*) ' Decision parameter ', PARAID(IPARA)
               CALL ERRMSG (917,0, PARInterp (IPARA,I), ' Beslisparameterfile', IOUT1)
               RetVal = 917
           ENDIF
         ENDDO
         If (RetVal .ne. 0) Return


      GOTO 20


! *********************************************************************
! ** Derde type data records met keyword RSVP
! *** format: RSVP ng 3 id 'id1''id2' id3' rsv 'rsvname' il 'initiallevel' ei 'expectedinflow' rsvp
! ***         rsvp
! *** Remarks:
! ***    'initiallevel' and 'expectedinflow' should be earlier defined decision parameters
! ***    'id1' 'id2' id3' can be used in other PARA and PAR2 records afterwards, or with time delays
! *********************************************************************
!
  192 CONTINUE

! *********************************************************************
! ** Aantal verwijzingen naar beslisparameters
! *********************************************************************
         PARDIM (IPARA,1) = 0
         PARDIM (IPARA,2) = 0
         PARDIM (IPARA,3) = 0
         PARDIM (IPARA,4) = 0
         PARDIM (IPARA,5) = 0
         PARDIM (IPARA,6) = 0
         PARDIM (IPARA,8) = 0
! test using GetRecord and ParseToken
         Backspace(in)
         iStart = 1
         ReadError = .false.
         SearchString = 'RSVP'
         Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
         If (Endfil .or. .not. success)  then
            Call ErrMsg (974, 0, 'Rdpara', ' Unexpected end of file ',IOUT1)
            RetVal = 974
            Return
         Endif
         String = ' '
         Success = GetStringFromBuffer (String)
         if (.not. Success) ReadError = .true.
         Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
         If (.not. Success) ReadError = .true.
         IF (ReadError) then
            Call ErrMsg (974,0,'Rdpara',' Read error during reading decision parameter file - RSVP ParseToken ',IOUT1)
            RetVal = 974
            Return
         Endif
         If (Idebug .gt. 0) then
            Write(Idebug,'(A)') String(1:len_trim(String))
            Write(Idebug,*) ' Results of ParseToken '
            Write(idebug,*) ' Nr  StartPos  Quotes Token '
            Do i=1,NumberofTokens
               write(idebug,'(I3,I5,L,1X,A)') i, RecordData%StartPositionOfToken(i), &
                                              RecordData%IsEnclosedByQuotes(i), RecordData%Token(i)
            Enddo
         Endif
! optional name
         success = SetOneVariable('nm',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, ParaDescr(Ipara))
! total number of gates
         ReadError = ReadError .and. .not. &
             SetOneVariable('ng',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, NrRsvOutputPar(Ipara))
         NrsvPar = NrRsvOutputPar(ipara)
         if (idebug .gt. 0) write(idebug,*) ' ng =', NrRsvOutputPar(Ipara), ReadError, ReturnIndx, &
                            RecordData%Token(ReturnIndx+1)(1:len_trim(RecordData%Token(ReturnIndx+1)))
! number of bottom gates
         ReadError = ReadError .and. .not. &
             SetOneVariable('nb',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, NrBottomGates(Irsv))
         if (idebug .gt. 0) write(idebug,*) ' nb =', NrBottomGates(irsv)
! number of turbine gates
         ReadError = ReadError .and. .not. &
             SetOneVariable('nt',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, NrTurbines(Irsv))
         if (idebug .gt. 0) write(idebug,*) ' nt =', NrTurbines(irsv)
! number of spillway gates
         ReadError = ReadError .and. .not. &
             SetOneVariable('ns',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, NrSpillways(Irsv))
         if (idebug .gt. 0) write(idebug,*) ' ns =', NrSpillways(irsv)
! check that nr. bottom gates + turbine gates + spillway gates = total number of gates
         NrGates = NrBottomGates(irsv) + NrTurbines(irsv) + NrSpillways(irsv)
         If (NrGates .ne. NrsvPar) then
             Call ErrMsg (913, 0, 'Rdpara', &
                         ' Inconsistent number of gates (total<> nr. bottom+turbine+spillway gates) ',IOUT1)
             RetVal = 913
             Return
         endif
! number of link outlets
         ReadError = ReadError .and. .not. &
             SetOneVariable('no',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, NrOutletLinks(Irsv))
         if (idebug .gt. 0) write(idebug,*) ' no =', NrOutletLinks(irsv)
! related decision parameters = 3 + nr. link outlets * 2
! viz. max.lvl, initial level, expected inflow; downstream demand per link outlet, maximum flow per link outlet
         PARDIM (IPARA,7) = 3 + 2* NrOutletlinks(irsv)
         IF (PARDIM(IPARA,7) .GT. NRPAR) then
            CALL ERRMSG (913,0,'Rdpara',' NRPAR beslisparameters',IOUT1)
            RetVal = 913
         ElseIF (PARDIM(IPARA,7) .GT. NHLP)  then
            CALL ERRMSG (913,0,'Rdpara',' NHLP; PARDIM7',IOUT1)
            RetVal = 913
         Endif
         If (RetVal .ne. 0) Return
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Pardim', (PARDIM(IPARA,I),I=1,7)

! *********************************************************************
! ** Data
! *********************************************************************
         If (NRSVPAR .GT. 0) Then
! RSVP referentie naar reservoir definitie, initial level, expected inflow
! Get id's of Hav, RuleCurve, BottomGate, Turbine, Spillway definitions
!    Reservoir id = Parameter id (already read)
           RsvId(irsv) = ParaId(ipara)
           ReadError = ReadError .and. .not. &
             SetOneVariable('hav',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, HavId(Irsv))
           ReadError = ReadError .and. .not. &
             SetOneVariable('rule',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, RuleCurveId(Irsv))
           ReadError = ReadError .and. .not. &
             SetOneVariable('hedg',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, HedgingId(Irsv))
           ReadError = ReadError .and. .not. &
             SetOneVariable('bg',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, BottomGateId(Irsv))
           ReadError = ReadError .and. .not. &
             SetOneVariable('tg',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, TurbineId(Irsv))
           ReadError = ReadError .and. .not. &
             SetOneVariable('sg',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, SpillwayId(Irsv))
! initial level decision parameter
           ReadError = ReadError .and. .not. &
             SetOneVariable('il',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, ParRsv(ipara,1))
! expected inflow decision parameter
           ReadError = ReadError .and. .not. &
             SetOneVariable('ei',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, ParRsv(ipara,2))
! maximum allowed level at end of timestep
           ReadError = ReadError .and. .not. &
             SetOneVariable('ml',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, ParRsv(ipara,3))
           If (IDEBUG .GT. 0) Write(IDEBUG,'(A,A,A,A,A)') ' RSVP data ', &
                                       PARRSV(IPARA,1)(1:len_trim(ParRsv(IPara,1))), &
                                         PARRSV(IPARA,2)(1:len_trim(ParRsv(IPara,2))), &
                                           PARRSV(IPARA,3)(1:len_trim(ParRsv(IPara,3)))
! coupling of gates to link outlets
! check that nr. specified outlet link <= total number of outlet links
           if (Getkey ('gb', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,NrBottomGates(Irsv)
                 Read (RecordData%Token(ReturnIndx+i),*,Err=993)   LinkOutlets(irsv,1,i)
                 If (LinkOutlets(irsv,i,1) .gt. NrOutletLinks(irsv)) then
                    Call ErrMsg (913,0,'Rdpara',' Inconsistent outlet link number specified for bottom gate ',IOUT1)
                    RetVal = 913
                    Return
                 Endif
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('gt', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,NrTurbines(Irsv)
                 Read (RecordData%Token(ReturnIndx+i),*,Err=993)   LinkOutlets(irsv,2,i)
                 If (LinkOutlets(irsv,2,i) .gt. NrOutletLinks(irsv)) then
                    Call ErrMsg (913,0,'Rdpara',' Inconsistent outlet link number specified for turbinegate ',IOUT1)
                    RetVal = 913
                    Return
                 Endif
              Enddo
           else
               ReadError = .true.
           endif
           if (Getkey ('gs', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do I=1,NrSpillways(Irsv)
                 Read (RecordData%Token(ReturnIndx+i),*,Err=993)   LinkOutlets(irsv,3,i)
                 If (LinkOutlets(irsv,3,i) .gt. NrOutletLinks(irsv)) then
                    Call ErrMsg (913,0,'Rdpara',' Inconsistent outlet link number specified for spillwaygate ',IOUT1)
                    RetVal = 913
                    Return
                 Endif
              Enddo
           else
               ReadError = .true.
           endif
! demands for each link outlet --> reference to other decision parameters
           if (Getkey ('dm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do i=1,NrOutletLinks(irsv)
                 PARRSV(IPARA,3+i) = RecordData%Token(ReturnIndx+i)
                 If (IDEBUG .GT. 0) Write(IDEBUG,'(A,I3,A)') ' RSVP data link outlet demands link', i, &
                                          PARRSV(IPARA,i+3)(1:len_trim(ParRsv(IPara,i+3)))
              Enddo
           else
               ReadError = .true.
           endif
! maximum flows for each link outlet --> reference to other decision parameters
           if (Getkey ('mf', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Do i=1,NrOutletLinks(irsv)
                 PARRSV(IPARA,3+NrOutletLinks(irsv)+i) = RecordData%Token(ReturnIndx+i)
                 If (IDEBUG .GT. 0) Write(IDEBUG,'(A,I3,A)') ' RSVP data maximum flow link', i, &
                            PARRSV(IPARA,i+3+NrOutletLinks(irsv))(1:len_trim(ParRsv(IPara,i+3+NrOutletLinks(irsv))))
              Enddo
           else
               ReadError = .true.
           endif
! id's decision parameters, stored as separate decision parameters use keyword dp
           ReadError = ReadError .and. .not. &
                SetVariables('dp',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, CDum,NrsvPar)
! NRSVPAR output decision parameters, + 3 for the rule curve values
           Do I=1,NRSVPAR + 3
              PARAID(IPARA) = ' '
              if (i .le. NrsvPar)  then
                 PARAID(IPARA) = CDUM(I)        ! id of individual gate
              elseif (i .eq.  NrsvPar+1) then
                 PARAID(IPARA) = 'FloodCurve ' // RsvId(irsv) (1:len_trim(RsvId(Irsv)))
              elseif (i .eq.  NrsvPar+2) then
                 PARAID(IPARA) = 'TargetCurve ' // RsvId(irsv) (1:len_trim(RsvId(Irsv)))
              elseif (i .eq.  NrsvPar+3) then
                 PARAID(IPARA) = 'FirmCurve ' // RsvId(irsv) (1:len_trim(RsvId(Irsv)))
              endif
              If (i .lt. NRSVPAR+3) IPARA = IPARA + 1
              PARDIM (IPARA,1) = 0
              PARDIM (IPARA,2) = 0
              PARDIM (IPARA,3) = 0
              PARDIM (IPARA,4) = 0
              PARDIM (IPARA,5) = 0
              PARDIM (IPARA,6) = 0
              PARDIM (IPARA,7) = Pardim(Ipara-1,7)
              If (i .le. NrsvPar+3) then
                 Do ipar=1,Pardim(ipara,7)
                    PARRSV(IPARA,ipar) = ParRsv(Ipara-1,ipar)
                 Enddo
              Endif
              If (Idebug .gt. 0) then
                 Write(Idebug,*) ' Reservoir decision parameter', ipara-1
                 Write(Idebug,*) Paraid(ipara-1) (1:len_trim(Paraid(Ipara-1)))
                 Write(Idebug,*) ' depends on ParRsv ', ParDim(Ipara-1,7) ,' other parameters'
                 Do ipar=1,Pardim(ipara,7)
                    Write(Idebug,*) ' parameter ', ipar, PARRSV(IPARA-1,ipar)(1:len_trim(Parrsv(Ipara-1,ipar)))
                 Enddo
              Endif
           Enddo
         Endif
!        read error handling
         Goto 994
   993   continue
         ReadError = .true.
   994   continue
         IF (ReadError) then
            Call ErrMsg (974,0,'Rdpara',' Read error during reading Reservoir decision parameter record',IOUT1)
            RetVal = 974
            Return
         Endif


! *********************************************************************
! check existence of decision parameters is done later
! check validiteit time shifts: NOT applicable for RSV parameters
! *********************************************************************

         GOTO 20


! *********************************************************************
! ** Vierde type data records met keyword PAR3
! *** format: PAR3 id 'id' PDIN ...  pdin
! ***         TBLE
! ***          datetime  value <
! ***          datetime  value <
! ***         tble par3
! *********************************************************************
!
  193 CONTINUE

! *********************************************************************
! ** Aantal verwijzingen naar beslisparameters
! *********************************************************************
         PARDIM (IPARA,1) = 0
         PARDIM (IPARA,2) = 0
         PARDIM (IPARA,3) = 0
         PARDIM (IPARA,4) = 0
         PARDIM (IPARA,5) = 0
         PARDIM (IPARA,6) = 0
         PARDIM (IPARA,7) = 0
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Pardim', (PARDIM(IPARA,I),I=1,7)

! *********************************************************************
! ** Data Read Time table
! *********************************************************************
!        Get table, id=ParaId(ipara)
         Backspace(in)
         KeyUppUntilColumn =5
         SearchString = 'PAR3'
         Success = GetRecord(In, SearchString, Endfil, idebug, Iout1, KeyUppUntilColumn)
         If (Endfil .or. .not. success) then
            Call ErrMsg (913,0,'Rdpara',' Error in PAR3 record: Time Table end not found',IOUT1)
            RetVal = 913
            Return
         else
            Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)
            If (.not. success) then
               Call ErrMsg (913,0,'Rdpara',' Error getting Table Name',IOUT1)
               RetVal = 913
               Return
            Endif
            TableNr = 0
            If (TabYesNo .and. TableName .ne. '') Then
!              Er is een tabel record PAR3 gedefinieerd, met een niet-lege naam
               NrColumns = 1
               Success = GetTable (RTCTableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
               If (.not. success) then
                  Call ErrMsg (913,0,'Rdpara',' Error getting Table Data' ,IOUT1)
                  RetVal = 913
                  Return
               Endif
            Endif
         Endif

   20 ENDDO
   21 CONTINUE

      NPARA = IPARA -1
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdpara', ' Beslispar.file', IOUT1)
      RetVal = 902
      Return

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

! ARS xxxx: let op optie SetSequenceDecisionParameters, if true then set by RTC, else by order in input.
! NSeq = number already labeled in simulation sequence SimSeq
      If (SetSequenceDecisionParameters) then
         SimSeq   = 0
         NSeq     = 0
         NSeqPrev = 0
         ! first, label all internal decision parameters
         Do Ipara=1,NParI
            SimSeq(IPara) = IPara
         Enddo
         NSeq = NParI
         ! second, all decision parameters not depending on other decision parameters (so also not RSVP)
         Do Ipara=1,NPara
            if (Simseq(ipara) .eq. 0 .and. ParDim(Ipara,6) .eq. 0 .and. Pardim(ipara,7) .eq. 0) then
               NSeq = NSeq+1
               SimSeq(IPara) = NSeq
            Endif
         Enddo
         ! then, all decision parameters depending only on already defined decision parameters
         ! (repeat X times until all decision parameters labeled)
  201    Continue
         If (Idebug .gt. 0) then
             Write(Idebug,*) ' after label 201 NSeq, NSeqPrev NPara', NSeq, NSeqPrev, NPara
             Do Ipara=1,NPara
                Write(idebug,*) ipara, SimSeq(Ipara), ParaId(ipara)(1:len_trim(ParaId(ipara)))
             Enddo
         Endif
         If (NSeq .lt. NPara .and. NSeq .gt. NSeqPrev) then
             NSeqPrev = NSeq
             Do Ipara=1,NPara
                if (Simseq(ipara) .eq. 0) then
                ! test dependencies on other decision parameters; corrected/extended Taiwan nov 2003
                   Do ipar=1,Pardim(Ipara,6)
                      Do Jpara=1,NPara
                         if (PARAID(JPARA) .EQ. PARPAR(IPARA,IPAR)) then
                             If (SimSeq(JPara) .eq. 0 .and. ipara .ne. jpara) then
                                 ! referring to other, not yet set, parameter; allowed if value of previous timestep
                                 if (TisPar(Ipara,ipar) .lt. 0) goto 251
                                 goto 253
                             ElseIf (SimSeq(JPara) .eq. 0 .and. ipara .eq. jpara) then
                                 ! ARS 15839: referring to itself is allowed, if it concerns previous timesteps
                                 if (TisPar(Ipara,ipar) .lt. 0) goto 251
                             endif
                             Goto 251
                         Endif
                      Enddo
  251                 Continue
                   Enddo
! test dependencies on other decision parameters; corrected/extended Taiwan nov 2003
                   if (Pardim(Ipara,7) .eq. 0) then
                      NSeq = NSeq+1
                      SimSeq(IPara) = NSeq
                   else   ! check if 3 parameters: max.lvl,initial lvl and expected inflow are already defined
                      Do ipar=1,3
                         Do Jpara=1,NPara
                            if (PARAID(JPARA) .EQ. PARRSV(IPARA,IPAR)) then
                                If (SimSeq(JPara) .eq. 0) goto 253
                                Goto 252
                            Endif
                         Enddo
  252                    Continue
                      Enddo
                      NSeq = NSeq+1
                      SimSeq(IPara) = NSeq
                   endif
  253              Continue
                Endif
             Enddo
             If (NSeq .lt. NPara) goto 201
         Else
            If (NSeq .lt. NPara) then
               Call ErrMsg (951,0,'Rdpara',' No correct simulation order of decision variables can be set',IOUT1)
               RetVal = 951
               Return
            Endif
         Endif
      Else
!        Default, simply use the order in the input file
         Do Ipara=1,NPara
            SimSeq(IPara) = IPara
         Enddo
      Endif

! 14June2002 addition because of possible Matlab measures
      Do Ipara=NPara+1,NDecV
         SimSeq(IPara) = IPara
      Enddo

! *********************************************************************
! *** Check existence beslisparameter id's
! ***   *** ook rekening houden met PAR3 records
! *********************************************************************
      IF (IDEBUG .GT. 0) Then
          Write(idebug,*) ' Voor check existence, voor invert Simseq'
          Do Ipara=1,NPara
             Write(idebug,*) ipara, SimSeq(Ipara), ParaId(ipara)(1:len_trim(ParaId(ipara)))
          Enddo
      Endif

      DO IPARA=1,NPARA
! check existence beslisparameter id's (ParPar)
         DO IPAR=1,PARDIM(IPARA,6)
            IXDPAR(IPARA,IPAR) = 0
            DO JPARA=1,NPARA
               IF (PARAID(JPARA) .EQ. PARPAR(IPARA,IPAR)) THEN
                  IXDPAR(IPARA,IPAR) = JPARA
                  Goto 301
               ENDIF
            ENDDO
  301       Continue
            IF (IXDPAR(IPARA,IPAR) .LE. 0) THEN
! parameter id moet bekend zijn, dus IxdPar > 0
               WRITE(IOUT1,*) ' Parameter nr',IPARA
               WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)
               WRITE(IOUT1,*) ' Number of related decision parameters', PARDIM(IPARA,6)
               WRITE(IOUT1,*) ' Parameter id not found ', PARPAR(IPARA,IPAR)
               CALL ERRMSG (924,0, PARAID(IPARA), ' Check 6 Decision parameter file', IOUT1)
               RetVal = 924
!           ELSEIF (IPARA .GT. NPAR1 .AND.
!    *                IXDPAR(IPARA,IPAR) .GT. IPARA) THEN
! verzoek PJ en Natalie; ARS xxxx
!           ELSEIF (IPARA .GT. NPAR1 .AND. IXDPAR(IPARA,IPAR) .GT. IPARA .AND. Tispar(Ipara,ipar) .eq. 0) Then
! nu via SimSeq
            ELSEIF (IPARA .GT. NPAR1 .AND. SimSeq(IXDPAR(IPARA,IPAR)) .GT. Simseq(IPARA) .AND. Tispar(Ipara,ipar) .eq. 0) Then
! parameter van type 2 moet afhangen van reeds eerder gedefinieerde
! parameters of van zichzelf, dus IxdPar <= IPara
               WRITE(IOUT1,*) ' Parameter nr',IPARA
               WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA)(1:len_trim(Paraid(ipara)))
               WRITE(IOUT1,*) ' Number of related decision parameters', PARDIM(IPARA,6)
               WRITE(IOUT1,*) ' Parameter id to be defined earlier', PARPAR(IPARA,IPAR) (1:len_trim(Parpar(ipara,ipar)))
               CALL ERRMSG (925,0, PARAID(IPARA), ' Decision parameter file', IOUT1)
               RetVal = 925
            ENDIF
         ENDDO
         If (RetVal .ne. 0) Return

! check existence beslisparameter id's (PARRSV: RSVP records: il and ei decision parameters, and others !!
! also IxdPar array can be used to store references to ParRsv, since it does not overlap with ParPar
         DO IPAR=1,PARDIM(IPARA,7)
            IXDPAR(IPARA,IPAR) = 0
            DO JPARA=1,NPARA
               IF (PARAID(JPARA) .EQ. PARRSV(IPARA,IPAR)) THEN
                  IXDPAR(IPARA,IPAR) = JPARA
               ENDIF
            ENDDO
            IF (IXDPAR(IPARA,IPAR) .LE. 0) THEN
! parameter id moet bekend zijn, dus IxdPar > 0
               WRITE(IOUT1,*) ' Parameter nr',IPARA
               WRITE(IOUT1,*) ' Parameter id',PARAID(IPARA) (1:len_trim(Paraid(ipara)))
               WRITE(IOUT1,*) ' Number of related decision parameters RSVP', PARDIM(IPARA,7)
               WRITE(IOUT1,*) ' Parameter id not found ', PARRSV(IPARA,IPAR)(1:len_trim(Parrsv(ipara,ipar)))
               CALL ERRMSG (924,0, PARAID(IPARA), ' Check 7 Decision parameter file', IOUT1)
               RetVal = 924
            ENDIF
         ENDDO
         If (RetVal .ne. 0) Return
      ENDDO
!
!     If simulation sequence is determined by RTC, then invert the SimSeq array here
!     This inverted order is used in sub CmpDecV to compute decision variables in the right order
!     If the order in the input file is used, no inversion of Simseq is needed, since then SimSeq(i)=i for all i
      If (SetSequenceDecisionParameters) then
         ! inverteren array SIMSEQ
         Allocate (ihelp(Npara), Stat=Allocation_Error )
         If (Allocation_Error .ne. 0) then
            Call ERRMSG (913,0,'Rdpara',' Error allocating arrays in subroutine',IOUT1)
            RetVal = 913
            Return
         Endif
         Do IPara=1,NPara
            IHELP(SIMSEQ(IPara)) = IPara
         Enddo
         Do IPara=1,NPara
            SIMSEQ(IPara) = IHELP(IPara)
         Enddo
         deallocate (ihelp)
      Endif
      IF (IDEBUG .GT. 0) Then
          Write(idebug,*) ' list of all decision parameters'
          Do Ipara=1,NPara
             Write(idebug,*) ipara, SimSeq(Ipara), ParaId(ipara)(1:len_trim(ParaId(ipara)))
          Enddo
          WRITE(IDEBUG,*) ' MxTmShift Sbk   ',MxTmShift(1)
          WRITE(IDEBUG,*) ' MxTmShift 3B    ',MxTmShift(2)
          WRITE(IDEBUG,*) ' MxTmShift Rain  ',MxTmShift(3)
          WRITE(IDEBUG,*) ' MxTmShift Wind  ',MxTmShift(4)
          WRITE(IDEBUG,*) ' MxTmShift WQ    ',MxTmShift(5)
          WRITE(IDEBUG,*) ' MxTmShift DecPar',MxTmShift(6)
          WRITE(IDEBUG,*) ' MxTmShift D3DFlw',MxTmShift(9)
      Endif
      Do i=1,9  ! was 6
         If (Abs(NTims) .lt. (2+Abs(MxTmShift(i))) ) then
            Write(Iout1,*) ' NTIMS = ', Ntims
            Write(Iout1,*) ' i, MxTmShift(i) = ', i, 2+Abs(MxTmShift(i))
            Call ERRMSG (913, 0, 'Rdpara', ' Mx. Time shift dimension NTIMS', IOUT1)
            RetVal = 913
         Endif
      Enddo
!     D3DFlow
!     If (Abs(NTims) .lt. (2+Abs(MxTmShift(9))) ) then
!        Write(Iout1,*) ' NTIMS = ', Ntims
!        Write(Iout1,*) ' i, MxTmShift(i) = ', 9, 2+Abs(MxTmShift(9))
!        Call ERRMSG (913, 0, 'Rdpara', ' Mx. Time shift dimension NTIMS', IOUT1)
!        RetVal = 913
!     Endif

! *********************************************************************
! *** end of RdPara function
! *********************************************************************
      RETURN
      END Function RdPara
