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

    Module ReservoirModule

    use NewTables
! nav ARS 11505 (RR)
! Nov 2003 Taiwan
    use Dh_alloc


    Implicit None

    Integer RsvCharIdLength

! Iteration criteria
    Integer MxIter
    Real    EpsVolume, EpsFlow

! Constants

    Parameter (RsvCharIdLength=256)

! *********************************************************************
! Variables
! *********************************************************************
! *** Reservoir data
!
      CHARACTER(len=RsvCharIdLength), pointer, Save :: RsvId (:), HavId(:), RuleCurveId (:), HedgingId(:), &
                                                       BottomGateId(:), TurbineId(:), SpillwayId(:),&
                                                       MaxFlowId(:,:,:), QHId(:,:,:), EnergyId(:,:)

      INTEGER, pointer, Save :: NrBottomGates(:), &
                                NrTurbines(:), &
                                NrSpillways(:), &
                                MaxFlowTable(:), &
                                RuleCurveTable(:), &
                                EnergyTable(:,:), &
                                HavTableLength(:), QHTableLength(:,:,:), &
                                HedgingTableLength(:), &
                                NrOutletLinks(:), LinkOutLets(:,:,:)

      Double Precision, pointer, Save :: HAVRSV(:,:,:), &
                                         HedgingLevelPercentage(:,:), &
                                         HedgingReleasePercentage(:,:), &
                                         QHBottomGate (:,:,:,:), &
                                         QHTurbine(:,:,:,:), &
                                         QHSpillway(:,:,:,:), &
                                         IntakeLvlsBottomGate (:,:), &
                                         IntakeLvlsTurbine (:,:), &
                                         IntakeLvlsSpillway (:,:), &
                                         DeadLevel(:), DeadStorage(:), &
                                         FullRsvLevel(:), FullRsvStorage(:)

! HedgingLevelPercentage and HedgingReleasePercentage are entered as percentages (0-100%),
! but internally the corresponding fractions are used (0-1)

! InterpLength = max. lengte interpolatietabellen
! maxTypeGates=3 nl. 1=bottom gate, 2=turbine, 3=spillway
! maxSameGates=15 aantal gates van zelfde type (in Ribasim=1)
     Integer    InterpLength, MaxTypeGates, MaxSameGates
     Parameter (InterpLength=15, MaxTypeGates=3,MaxSameGates=15)


   Contains

     Subroutine SetDefaultIterationCriteriaValues

     MxIter = 10        ! maximum nr. of iterations
     EpsVolume = 0.1    ! volume criterion convergence
     EpsFlow   = 0.001  ! flow criterion convergence

     Return
     End Subroutine SetDefaultIterationCriteriaValues

     Function AllocReservoirArrays (Iout1, Nrsv) result(RetVal)

! *********************************************************************
! *** Allocate all Reservoir arrays and set default values
! *********************************************************************
      Integer    RetVal

      Integer    Nrsv, Iout1
      Logical    Success

      RetVal = 0

      success = DH_AllocInit(Nrsv, RsvId,'')
      success = success .and. DH_AllocInit(Nrsv, HavId,'')
      success = success .and. DH_AllocInit(Nrsv, RuleCurveId,'')
      success = success .and. DH_AllocInit(Nrsv, HedgingId,'')
      success = success .and. DH_AllocInit(Nrsv, BottomGateId, TurbineId, SpillwayId,'')
      success = success .and. DH_AllocInit(Nrsv, MaxTypeGates, MaxSameGates, MaxFlowId,'')
      success = success .and. DH_AllocInit(Nrsv, MaxTypeGates, MaxSameGates, QHid,'')
      success = success .and. DH_AllocInit(Nrsv, MaxSameGates, EnergyId,'')
!      Allocate   ( RsvId(NRSV), HavId(NRSV), RuleCurveId(NRSV), HedgingId(NRSV), &
!                   BottomGateId(NRSV), TurbineId(NRSV), &
!                   SpillwayId(Nrsv), &
!                   MaxFlowId(NRSV,MaxTypeGates,MaxSameGates), QHId(NRSV,MaxTypeGates,MaxSameGates), &
!                   EnergyId(NRSV,MaxSameGates) , &
!                   STAT=Allocation_Error )
      If (.not. success) then
          RetVal = 929
          Call Rsv_ErrMsg (929, 1, ' AllocReservoirArrays', ' ', IOUT1)
      Endif

      success = DH_AllocInit(Nrsv, NrBottomGates, NrTurbines, NrSpillways, -1)
      success = success .and. DH_AllocInit(Nrsv, RuleCurveTable, 0)
      success = success .and. DH_AllocInit(Nrsv, MaxSameGates, EnergyTable,0)
      success = success .and. DH_AllocInit(Nrsv*MaxTypeGates*MaxSameGates, MaxFlowTable,0)
      success = success .and. DH_AllocInit(Nrsv, HavTableLength, 0)
      success = success .and. DH_AllocInit(Nrsv, MaxTypeGates, MaxSameGates, QHTableLength, 0)
      success = success .and. DH_AllocInit(Nrsv, HedgingTableLength, 0)
      success = success .and. DH_AllocInit(Nrsv, NrOutletLinks, 0)
      success = success .and. DH_AllocInit(Nrsv, MaxTypeGates, MaxSameGates, LinkOutLets,0)
!     Allocate   ( NrBottomGates(NRSV), NrTurbines(NRSV), NrSpillways(NRSV), &
!                  RuleCurveTable(NRSV), &
!                  EnergyTable(NRSV,MaxSameGates), &
!                  MaxFlowTable(NRSV*MaxTypeGates*MaxSameGates), &
!                  HavTableLength(NRSV), QHTableLength(NRSV,MaxTypeGates,MaxSameGates), &
!                  HedgingTableLength(NRSV), &
!                  NrOutletLinks(NRSV), &
!                  LinkOutLets(NRSV,MaxTypeGates,MaxSameGates), STAT=Allocation_Error )
      If (.not. success) then
          RetVal = 929
          Call Rsv_ErrMsg (929, 1, ' AllocReservoirArrays', ' ', IOUT1)
      Endif

      success = DH_AllocInit(InterpLength,3,Nrsv, HavRsv, 0D0)
      success = success .and. DH_AllocInit(InterpLength, Nrsv, HedgingLevelPercentage, 0D0)
      success = success .and. DH_AllocInit(InterpLength, Nrsv, HedgingReleasePercentage, 0D0)
      success = success .and. DH_AllocInit(InterpLength, 2, MaxSameGates, Nrsv, QHBottomGate, 0D0)
      success = success .and. DH_AllocInit(InterpLength, 2, MaxSameGates, Nrsv, QHTurbine   , 0D0)
      success = success .and. DH_AllocInit(InterpLength, 2, MaxSameGates, Nrsv, QHSpillway  , 0D0)
      success = success .and. DH_AllocInit(Nrsv, MaxSameGates, IntakeLvlsBottomGate, 9999D0)
      success = success .and. DH_AllocInit(Nrsv, MaxSameGates, IntakeLvlsTurbine, 9999D0)
      success = success .and. DH_AllocInit(Nrsv, MaxSameGates, IntakeLvlsSpillway, 9999D0)
      success = success .and. DH_AllocInit(Nrsv, DeadLevel, 9999D0)
      success = success .and. DH_AllocInit(Nrsv, DeadStorage, 0D0)
      success = success .and. DH_AllocInit(Nrsv, FullRsvLevel, 9999D0)
      success = success .and. DH_AllocInit(Nrsv, FullRsvStorage, 0D0)
!     Allocate   ( HAVRSV(InterpLength,3,NRSV), &
!                  HedgingLevelPercentage(InterpLength,NRSV),&
!                  HedgingReleasePercentage(InterpLength,NRSV),&
!                  QHBottomGate(InterpLength,2,MaxSameGates,NRSV),&
!                  QHTurbine   (InterpLength,2,MaxSameGates,NRSV),&
!                  QHSpillway  (InterpLength,2,MaxSameGates,NRSV), &
!                  IntakeLvlsBottomGate(NRSV,MaxSameGates), &
!                  IntakeLvlsTurbine(NRSV,MaxSameGates), &
!                  IntakeLvlsSpillway(NRSV,MaxSameGates), &
!                  DeadLevel(NRSV), DeadStorage(NRSV), &
!                  FullRsvLevel(NRSV), FullRsvStorage(NRSV), &
!                  STAT=Allocation_Error )
      If (.not. success) then
          RetVal = 929
          Call Rsv_ErrMsg (929, 1, ' AllocReservoirArrays', ' ', IOUT1)
      Endif

! default initialisations

!     RsvId = ''
!     HavId = ''
!     RuleCurveId  = ''
!     HedgingId    = ''
!     BottomGateId = ''
!     TurbineId    = ''
!     SpillwayId   = ''
!     MaxFlowId    = ''
!     QHId         = ''
!     EnergyId     = ''
!     NrBottomGates = -1  !1
!     NrTurbines    = -1  !1
!     NrSpillways   = -1  !1
!     MaxFlowTable  = 0
!     RuleCurveTable= 0
!     EnergyTable   = 0
      NrOutLetLinks = 1
      LinkOutlets   = 1
!     HavTableLength= 0
!     HavRsv        = 0
!     HedgingTableLength = 0
!     HedgingLevelPercentage = 0
!     HedgingReleasePercentage = 0
!     QHTableLength = 0
!     QHBottomGate  = 0
!     QHTurbine     = 0
!     QHSpillway    = 0
!     IntakeLvlsBottomGate  = 9999
!     IntakeLvlsTurbine     = 9999
!     IntakeLvlsSpillway    = 9999
!     DeadLevel   = 9999
!     DeadStorage = 0
!     FullRsvLevel   = 9999
!     FullRsvStorage = 0

     Return
     End Function AllocReservoirArrays




     Function ReadReservoirInput (RtcTableHandle, Infile, Iout1, Idebug, NRsv)  result (RetVal)

! *********************************************************************
! ***  Read reservoir input data
! *********************************************************************
!  input variables: input file, output file, debug file, return code file
!                   number of decision variables NDecV
!                   number of reservoirs NRsv
! *********************************************************************
    use ParseToken
    use ReadLib

     Integer :: RetVal

     Integer      RtcTableHandle, Infile, Iout1, Idebug, NRsv !, IflRtn

!  local variables
     Integer    i, j, index1, Irsv, ihav, ibotg, iturb, ispil
     Integer    teller, maxGates
     Character(RsvCharIdLength) name
     Integer    NrColumns, TableNr, KeyUppUntilColumn
     Logical    Err969
     Logical    Success

     Character(Len=9999)  String
     Integer          TableLength  ! IDUM(32),
     REAL             HDUM(InterpLength), QDUM(InterpLength), R1, R2 !, RDUM(32),
!    Double Precision DDUM(32)
     Character(RsvCharIdLength)   CDUM(32), TableName
     Character*1      klteken
     Integer          ILeft, idum1

     Logical  Allow, Found, Endfil, Occurs, TabYesNo, CaseSensitive

     Logical, pointer, save :: AlreadyRead(:), AlreadyRead2(:), AlreadyRead3(:)

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


     success = DH_AllocInit(Nrsv*MaxSameGates, AlreadyRead, .false.)
     success = success .and. DH_AllocInit(Nrsv*MaxSameGates, AlreadyRead2, .false.)
     success = success .and. DH_AllocInit(Nrsv*MaxSameGates, AlreadyRead3, .false.)
!    ALLOCATE   (AlreadyRead(NRSV*MaxSameGates), &
!                AlreadyRead2(NRSV*MaxSameGates),&
!                AlreadyRead3(NRSV*MaxSameGates), Stat=Allocation_Error )
     If (.not. success) then
        RetVal = 929
        Call Rsv_errmsg (981, 1, ' Error allocating arrays in subroutine ', ' ReadReservoirInput', Iout1)
        Return
     Endif

     Allow = .false.
     Found = .false.
     CaseSensitive = .false.
     AlreadyRead = .false.
     klteken  = '<'
     KeyUppUnTilColumn = 9999     ! was eerst 5

! *********************************************************************
! ***  RSV definitions now directly from Decision Parameter file
! *********************************************************************
!    Read HAV records: definition of head-area-volume interpolation relation
!    normal: table is filled with increasing heads (but this is not necessary, decreasing should work also)
! *********************************************************************
     Rewind(Infile)
     Endfil = .false.
     teller = 0
     AlreadyRead = .false.
     if (idebug .gt. 0) write(idebug,*) ' Read HAV records'
     Call SKPCOM (INfile, ENDFIL,'ODS')
     Do while (.not. endfil)
!       Read entire HAV record (maybe several lines) and put it into buffer
        SearchString = 'HAVC'
        ReadError    = .false.
        Success = GetRecord (Infile, SearchString, Endfil, Idebug, Iout1, KeyUppUntilColumn)
        If (ENDFIL .or. .not. success) GOTO 211
        String = ' '
        Success = GetStringFromBuffer (String)
        if (.not. Success) ReadError = .true.
        Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
        if (.not. Success) ReadError = .true.
        IF (ReadError) then
           call write_error_message_rtc (974,0,'Rdpara',' Read error during reading reservoir datafile - HAVC ParseToken ',IOUT1)
           RetVal = 974
           Return
        Endif
        Name = ' '
        ReadError = ReadError .and. .not. &
            SetOneVariable('id',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, Name)
        IHav = FindString (Nrsv, HavId, Name, Nrsv, CaseSensitive)
        If (IHav .gt. 0) then
          If (AlreadyRead(ihav)) then
            write(iout1,'(A,A,A)') ' HAV Data ',Cdum(1)(1:len_trim(Cdum(1))),' double in datafile Reservoir.Rtc'
            write(iout1,'(A)') '    second data record is skipped'
          Else
!           Read HAV definition and assign it to the first reservoir using this HAV definition
!           number of interpolation values, levels, areas, volumes
            teller = teller + 1
            AlreadyRead(ihav) = .true.
            ! Find length of table, using routine CntStr from Readlib; routine counts number of times '<' in buffer
            HavTableLength(Ihav) = Max (1, CntStr(klteken, String(1:nbuf)) )
            If (HavTableLength(Ihav) .gt. InterpLength) then
                HavTableLength(Ihav) = InterpLength
                Call Rsv_ErrMsg (972, 0, &
                                ' Length of HAV interpolation table in Reservoi.Rtc file too long; cut-off length=15', &
                                  HavId(ihav)(1:len_trim(HavId(ihav))), Iout1)
            Endif
            ! replace all '<' by ' ' in order to enable free format reading from buffer string
            Ileft = INDEX(String(1:nbuf), 'TBLE') + 4
            Do idum1=ileft,nbuf-8
               if (String(idum1:idum1) .eq. klteken) String(idum1:idum1) = ' '
            Enddo
            Read (String(ileft:),*,Err=991) (HAVRSV(I,1,IHav), &
                                      HAVRSV(I,2,IHav), &
                                       HAVRSV(I,3,IHav),I=1,HavTableLength(ihav))
!           Assign definition to possible other reservoirs using the same HAV definition
            Do Irsv=1,NRsv
               If (StringComp(HavId(irsv), Name, CaseSensitive) .and. irsv .ne. ihav) then
                  teller = teller + 1
                  AlreadyRead(irsv) = .true.
                  HavTableLength(irsv) = HavTableLength(ihav)
                  Do I=1,HavTableLength(ihav)
                    Do j=1,3
                       HavRsv(i,j,IRsv) = HavRsv(i,j,iHav)
                    Enddo
                  Enddo
               Endif
            Enddo
          Endif
        Else
          Write(iout1,'(A,A,A)') ' Data for HAV id ',Cdum(1)(1:len_trim(CDum(1))),' not used'
        Endif
!       read error handling
        Goto 992
   991  Continue
        ReadError = .true.
   992  Continue
        IF (ReadError) call write_error_message_rtc (974,0,'ReadRsv',' Read error during reading RTC Reservoir input HAVC',IOUT1)
     Enddo
 211 CONTINUE
     If (teller .lt. NRsv) then
         RetVal = 930
         Call Rsv_ErrMsg (972, 1, ' Not enough Head-Area-Volume data found', &
                              ' HAV Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
         Do Irsv=1,Nrsv
            If (.not. AlreadyRead(irsv)) then
                Call Rsv_ErrMsg (972, 1, ' Missing HAV data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
            Endif
         Enddo
     Endif


! *********************************************************************
!    Read RULE records: rules curves as Sobek time tables
!     1st column = flood control curve
!     2nd column = target curve
!     3rd column = firm storage curve
! *********************************************************************
     Rewind(Infile)
     if (idebug .gt. 0) write(idebug,*) ' Read RULE records'
     endfil = .false.
     Call SKPCOM (Infile, ENDFIL,'ODS')
     Do while (.not. endfil)
       Success = GetRecord(Infile, 'RULE', Endfil, idebug, Iout1, KeyUppUntilColumn)
       IF (ENDFIL .or. .not. success) GOTO 212
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       If (.not. success) goto 212
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
          Irsv = FindString (Nrsv, RuleCurveId, TableName, Nrsv, CaseSensitive)
          NrColumns = 0
          Occurs = (Irsv .gt. 0)
          if (Irsv .gt. 0) then
             if (RuleCurveTable(irsv) .gt. 0) then
                write(iout1,'(A,A,A)') ' RuleCurve Definition ',&
                                         Tablename(1:len_trim(TableName)),' double in datafile Reservoir.Rtc'
             endif
             NrColumns = 3
          endif
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (RTCTableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            If (.not. success) goto 212
! Set references
            Do irsv = 1, nrsv
               If (StringComp(RuleCurveId(irsv), TableName, CaseSensitive))  RuleCurveTable(irsv) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile, ENDFIL,'ODS')
     Enddo
 212 Continue

! Check of alle referenties naar tabellen opgelost
    Err969 = .false.
    Do irsv  = 1, nrsv
       if (RuleCurveTable(irsv) .eq. 0) then
         Err969 = .true.
         write(*,*) ' For reservoir ', RsvId(irsv)(1:len_trim(RsvId(irsv)))
         write(*,*) ' the rule curve table is not found :',RuleCurveId(irsv)(1:len_trim(RuleCurveId(irsv)))
         Call Rsv_ErrMsg (969, 1, ' Missing RULE data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
       endif
    Enddo
    If (err969) then
        RetVal = 930
        Call Rsv_ErrMsg (969, 1, ' Not enough Rule curve definitions ', &
                             ' RULE Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
    Endif


! *********************************************************************
!    Read HEDG records
!      - percentages to determine levels between firm and dead storage
!      - release percentages for different zones
!      - check: level percentages should be decreasing
! *********************************************************************
!
     Rewind(Infile)
     Endfil = .false.
     teller = 0
     AlreadyRead  = .false.
     if (idebug .gt. 0) write(idebug,*) ' Read HEDG records'
     Call SKPCOM (INfile, ENDFIL,'ODS')
     Do while (.not. endfil)
!       Read entire HEDG record (maybe several lines) and put it into buffer
        SearchString = 'HEDG'
        ReadError    = .false.
        Success = GetRecord (Infile, SearchString, Endfil, Idebug, Iout1, KeyUppUntilColumn)
        If (ENDFIL .or. .not. success) GOTO 2121
        String = ' '
        Success = GetStringFromBuffer (String)
        if (.not. Success) ReadError = .true.
        Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
        if (.not. Success) ReadError = .true.
        IF (ReadError) then
           call write_error_message_rtc (974,0,'Rdpara',' Read error during reading reservoir datafile - HEDG ParseToken ',IOUT1)
           RetVal = 974
           Return
        Endif
        Name = ' '
        ReadError = ReadError .and. .not. &
            SetOneVariable('id',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, Name)
!       Read HEDG definition: determine number of interpolation values, read percentage and release
        ! Find length of table, using routine CntStr from Readlib; routine counts number of times '<' in buffer
        TableLength = Max (1, CntStr(klteken, String(1:nbuf)) )
!       Table is automatically extended with a zero level and release percentage, so check length on InterpLength-1
        If (TableLength .gt. InterpLength-1) then
            TableLength = InterpLength-1
            Call Rsv_ErrMsg (972, 0, ' Length of HEDG interpolation table too long; cut-off length=15', &
                                     ' Check data HEDG records in Reservoir.Rtc file', Iout1)
        Endif
        ! replace all '<' by ' ' in order to enable free format reading from buffer string
        Ileft = INDEX(String(1:nbuf), 'TBLE') + 4
        Do idum1=ileft,nbuf-8
           if (String(idum1:idum1) .eq. klteken) String(idum1:idum1) = ' '
        Enddo
        Read(String(ileft:nbuf),*,Err=993) (HDUM(I), QDUM(I),i=1,TableLength)
!       Table is automatically extended with a zero level and release percentage
        TableLength = TableLength + 1
        HDum(TableLength) = 0.
        QDum(TableLength) = 0.
!       Assign definition to all other reservoirs using the same HEDG definition
!       Write(*,*) ' Read HEDG definition', Name
        Do Irsv=1,NRsv
           If (StringComp(HedgingId(irsv), Name, CaseSensitive)) then
              If (AlreadyRead(irsv)) then
                 write(iout1,'(A,A,A)') ' HEDG data with id ',Name(1:len_trim(Name)),&
                                        ' double in datafile Reservoir.Rtc'
                 write(iout1,'(A,A)') '    second data record is skipped for reservoir', &
                                           RsvId(irsv)(1:len_trim(RsvId(Irsv)))
              Else
!               Check consistency: HedgingLevelPercentage (Hdum) should be decreasing
                Do i=1,TableLength
                   if (Hdum(i) .gt. 100. .or. Hdum(i) .lt. 0.)  then
                       RetVal = 930
                       Call Rsv_ErrMsg (972, 0, ' Reservoir Hedging level percentages should be between 0 and 100', &
                                                ' Check data HEDG records in Reservoir.Rtc file', Iout1)
                   endif
                Enddo
                Do i=1,TableLength
                   if (Qdum(i) .gt. 100. .or. Qdum(i) .lt. 0.)  then
                       RetVal = 930
                       Call Rsv_ErrMsg (972, 0, ' Reservoir Hedging release percentages should be between 0 and 100', &
                                                ' Check data HEDG records in Reservoir.Rtc file', Iout1)
                   endif
                Enddo
                Do i=2,TableLength
                   if (Hdum(i) .gt. Hdum(i-1)) then
                       RetVal = 930
                       Call Rsv_ErrMsg (972, 0, ' Reservoir Hedging level percentages should be non-increasing', &
                                                ' Check data HEDG records in Reservoir.Rtc file', Iout1)
                   endif
                Enddo
!               Write (*,*) ' Assigned to reservoir ', irsv
                teller = teller + 1
                AlreadyRead(irsv) = .true.
                HedgingTableLength(irsv) = TableLength
                Do i=1,HedgingTableLength(irsv)
                   HedgingLevelPercentage(i,IRsv)   = Hdum(i) / 100.
                   HedgingReleasePercentage(i,IRsv) = Qdum(i) / 100.
                Enddo
              Endif
           Endif
        Enddo
!       read error handling
        Goto 994
   993  Continue
        ReadError = .true.
   994  Continue
        IF (ReadError) call write_error_message_rtc (974,0,'ReadRsv',' Read error during reading RTC Reservoir input HEDG',IOUT1)
     Enddo
2121 CONTINUE
     If (teller .lt. Nrsv) then
         RetVal = 930
         write(*,*) ' assigned', teller, ' definitions instead of ',Nrsv
         Call Rsv_ErrMsg (972, 1, ' Not enough Hedging-relation data found', &
                              ' HEDG Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
         Do Irsv=1,Nrsv
            If (.not. AlreadyRead(irsv)) then
               Call Rsv_ErrMsg (972, 1, ' Missing HEDGing rule data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
            Endif
         Enddo
     Endif
     If (Idebug .gt. 0) then
        write(Idebug,*) ' Hedging rules'
        Do irsv=1,nrsv
           write(Idebug,'(A,A)') ' reservoir ', RsvId(irsv)
           write(Idebug,'(A,I3)')      ' Hedging Table length ', HedgingTableLength(irsv)
           write(Idebug,'(A,999F9.3)') ' Hedging Levels (percentage between firm and dead storage) ', &
                                         (HedgingLevelPercentage(i,Irsv),i=1,HedgingTableLength(irsv))
           write(Idebug,'(A,999F9.3)') ' Hedging Release percentages ', &
                                         (HedgingReleasePercentage(i,Irsv),i=1,HedgingTableLength(irsv))
        Enddo
     Endif

! *********************************************************************
!    Read BOTG records: bottom gate characteristics
!      - nr. of bottom gates from Decispar.Rtc input file (RSVP record)
!      - intake levels
!      - references to q-h relation and maximum flow table
! *********************************************************************
     Rewind(Infile)
     Endfil = .false.
     teller = 0
     AlreadyRead = .false.
     Call SKPCOM (INfile, ENDFIL,'ODS')
     if (idebug .gt. 0) write(idebug,*) ' Read BOTG records'
     Do while (.not. endfil)
!       Read record using ParseToken
        SearchString = 'BOTG'
        ReadError    = .false.
        Success = GetRecord (Infile, SearchString, Endfil, Idebug, Iout1, KeyUppUntilColumn)
        If (ENDFIL .or. .not. success) GOTO 2111
        String = ' '
        Success = GetStringFromBuffer (String)
        if (.not. Success) ReadError = .true.
        Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
        if (.not. Success) ReadError = .true.
        IF (ReadError) then
           call write_error_message_rtc (974,0,'Rdpara',' Read error during reading reservoir datafile - BOTG ParseToken ',IOUT1)
           RetVal = 974
           Return
        Endif
        Name = ' '
        ReadError = ReadError .and. .not. &
            SetOneVariable('id',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, Name)
        IBotg = FindString (Nrsv, BottomGateId, Name, Nrsv, CaseSensitive)
        If (IBotg .gt. 0) then
          If (AlreadyRead(ibotg)) then
            write(iout1,'(A,A,A)') ' BOTG Data ',Cdum(1)(1:len_trim(Cdum(1))),' double in datafile Reservoir.Rtc'
            write(iout1,'(A)') '    second data record is skipped'
          Else
!           Read BOTG definition and assign it to the first reservoir using this BOTG definition
            AlreadyRead(ibotg) = .true.
            teller = teller + 1
!     NrBottomGates already read from decision parameter file
            If (NrBottomGates(IBotg) .gt. MaxSameGates) then
                RetVal = 930
                Call Rsv_ErrMsg (972, 1, &
                                   ' Dimension problem: Number of bottom gates in present version should be <=15', &
                                     ' Check data BOTG records in Reservoir.Rtc file', Iout1)
                Return
            Endif
            ReadError = ReadError .and. .not. &
                SetVariables('lv',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              IntakeLvlsBottomGate,NRsv,MaxSameGates,ibotg, NrBottomGates(IBotg))
            ReadError = ReadError .and. .not. &
                SetVariables('qh',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              QhId,NRsv,MaxTypeGates,MaxSameGates,ibotg,1,NrBottomGates(IBotg))
            ReadError = ReadError .and. .not. &
                SetVariables('mf',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              MaxFlowId,NRsv,MaxTypeGates,MaxSameGates,ibotg,1,NrBottomGates(IBotg))
!           Assign definition to possible other reservoirs using the same BOTG definition
            Do Irsv=1,NRsv
               If (StringComp(BottomGateId(irsv), Name, CaseSensitive) .and. irsv .ne. ibotg) then
                  teller = teller + 1
                  NrBottomGates(irsv) = NrBottomGates(ibotg)
                  Do i=1,NrBottomGates(IBotG)
                     IntakeLvlsBottomGate(Irsv,i) = IntakeLvlsBottomGate(IBotg,i)
                     QHId     (IRsv,1,i) = QHId     (IBotg,1,i)
                     MaxFlowId(IRsv,1,i) = MaxFlowId(IBotg,1,i)
                  Enddo
               Endif
            Enddo
          Endif
        Endif
        IF (ReadError) call write_error_message_rtc (974,0,'ReadRsv',' Read error during reading RTC Reservoir input BOTG',IOUT1)
     Enddo
2111 CONTINUE
     If (teller .lt. NRsv) then
         RetVal = 930
         Call Rsv_ErrMsg (972, 1, ' Not enough Bottom gate definitions found', &
                              ' BOTG Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
         Do Irsv=1,Nrsv
            If (NrBottomGates(irsv) .eq. -1)  then
               Call Rsv_ErrMsg (972, 1, ' Missing BOTG record data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
            Endif
         Enddo
     Endif



! *********************************************************************
!    Read TURB records
!      - nr. of turbine gates from Decispar.Rtc input file (RSVP record)
!      - intake levels
!      - references to q-h relation, maximum flow table, energy demand tables
! *********************************************************************
     Rewind(Infile)
     Endfil = .false.
     teller = 0
     AlreadyRead = .false.
     if (idebug .gt. 0) write(idebug,*) ' Read TURB records'
     Call SKPCOM (INfile, ENDFIL,'ODS')
     Do while (.not. endfil)
!       Read using ParseToken
        SearchString = 'TURB'
        ReadError    = .false.
        Success = GetRecord (Infile, SearchString, Endfil, Idebug, Iout1, KeyUppUntilColumn)
        If (ENDFIL .or. .not. success) GOTO 2112
        String = ' '
        Success = GetStringFromBuffer (String)
        if (.not. Success) ReadError = .true.
        Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
        if (.not. Success) ReadError = .true.
        IF (ReadError) then
           call write_error_message_rtc (974,0,'Rdpara',' Read error during reading reservoir datafile - TURB ParseToken ',IOUT1)
           RetVal = 974
           Return
        Endif
        Name = ' '
        ReadError = ReadError .and. .not. &
            SetOneVariable('id',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, Name)
        Iturb = FindString (Nrsv, TurbineId, Name, Nrsv, CaseSensitive)
        If (Iturb .gt. 0) then
          If (AlreadyRead(iturb)) then
            write(iout1,'(A,A,A)') ' TURB data ',Cdum(1)(1:len_trim(Cdum(1))),' double in datafile Reservoir.Rtc'
            write(iout1,'(A)') '    second data record is skipped'
          Else
!           Read TURB definition and assign it to the first reservoir using this TURB definition
            AlreadyRead(iturb) = .true.
            teller = teller + 1
!           NrTurbines already read from decision parameter file
            If (NrTurbines(Iturb) .gt. MaxSameGates) then
                RetVal = 930
                Call Rsv_ErrMsg (972, 1, &
                                   ' Dimension problem: Number of turbines in present version should be <=15', &
                                   ' Check data TURB records in Reservoir.Rtc file', Iout1)
                Return
            Endif
            ReadError = ReadError .and. .not. &
                SetVariables('lv',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              IntakeLvlsTurbine,NRsv,MaxSameGates,iturb, NrTurbines(Iturb))
            ReadError = ReadError .and. .not. &
                SetVariables('qh',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              QhId,NRsv,MaxTypeGates,MaxSameGates,iturb,2,NrTurbines(Iturb))
            ReadError = ReadError .and. .not. &
                SetVariables('mf',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              MaxFlowId,NRsv,MaxTypeGates,MaxSameGates,iturb,2,NrTurbines(Iturb))
            ReadError = ReadError .and. .not. &
                SetVariables('ed',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              EnergyId,NRsv,MaxSameGates,iturb, NrTurbines(Iturb))
!           Assign definition to possible other reservoirs using the same TURB definition
            Do Irsv=1,NRsv
               If (StringComp(TurbineId(irsv), Name, CaseSensitive) .and. irsv .ne. iturb) then
                  teller = teller + 1
                  NrTurbines(irsv) = NrTurbines(iturb)
                  Do i=1,NrTurbines(Iturb)
                     EnergyId (irsv,i)  = EnergyId(iturb,i)
                     IntakeLvlsTurbine(Irsv,i) = IntakeLvlsTurbine(Iturb,i)
                     QHId     (IRsv,2,i) = QHId     (Iturb,2,i)
                     MaxFlowId(IRsv,2,i) = MaxFlowId(Iturb,2,i)
                  Enddo
               Endif
            Enddo
          Endif
        Endif
        IF (ReadError) call write_error_message_rtc (974,0,'ReadRsv',' Read error during reading RTC Reservoir input TURB',IOUT1)
     Enddo
2112 CONTINUE
     If (teller .lt. NRsv) then
         RetVal = 930
         Call Rsv_ErrMsg (972, 1, ' Not enough Turbine definitions found', &
                              ' TURB Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
         Do Irsv=1,Nrsv
            If (NrTurbines(irsv) .eq. -1)  then
               Call Rsv_ErrMsg (972, 1, ' Missing TURB record data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
            Endif
         Enddo
     Endif

! *********************************************************************
!    Read SPIL records
!      - nr. of spillways from Decispar.Rtc input file (RSVP record)
!      - intake levels
!      - references to q-h relation, maximum flow table
! *********************************************************************
     Rewind(Infile)
     Endfil = .false.
     teller = 0
     AlreadyRead = .false.
     Call SKPCOM (INfile, ENDFIL,'ODS')
     if (idebug .gt. 0) write(idebug,*) ' Read SPIL records'
     Do while (.not. endfil)
!       Read input using ParseToken
        SearchString = 'SPIL'
        ReadError    = .false.
        Success = GetRecord (Infile, SearchString, Endfil, Idebug, Iout1, KeyUppUntilColumn)
        If (ENDFIL .or. .not. success) GOTO 2113
        String = ' '
        Success = GetStringFromBuffer (String)
        if (.not. Success) ReadError = .true.
        Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
        if (.not. Success) ReadError = .true.
        IF (ReadError) then
           call write_error_message_rtc (974,0,'Rdpara',' Read error during reading reservoir datafile - SPIL ParseToken ',IOUT1)
           RetVal = 974
           Return
        Endif
        Name = ' '
        ReadError = ReadError .and. .not. &
            SetOneVariable('id',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, Name)
        Ispil = FindString (Nrsv, SpillwayId, Name, Nrsv, CaseSensitive)
        If (Ispil .gt. 0) then
          If (AlreadyRead(Ispil)) then
            write(iout1,'(A,A,A)') ' SPIL Data ',Cdum(1)(1:len_trim(CDUm(1))),' double in datafile Reservoir.Rtc'
            write(iout1,'(A)') '    second data record is skipped'
          Else
!           Read SPIL definition and assign it to the first reservoir using this BOTG definition
            AlreadyRead(ispil) = .true.
            teller = teller + 1
!          NrSpillways already read from decision parameter file
            If (NrSpillways(Ispil) .gt. MaxSameGates) then
                RetVal = 930
                Call Rsv_ErrMsg (972, 1, &
                                     ' Dimension problem: Number of spillways in present version should be <=15', &
                                     ' Check data SPIL records in Reservoir.Rtc file', Iout1)
                Return
            Endif
            ReadError = ReadError .and. .not. &
                SetVariables('lv',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              IntakeLvlsSpillway,NRsv,MaxSameGates,ispil, NrSpillways(Ispil))
            ReadError = ReadError .and. .not. &
                SetVariables('qh',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              QhId,NRsv,MaxTypeGates,MaxSameGates,ispil,3,NrSpillways(Ispil))
            ReadError = ReadError .and. .not. &
                SetVariables('mf',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, &
                              MaxFlowId,NRsv,MaxTypeGates,MaxSameGates,ispil,3,NrSpillways(Ispil))
!           Assign definition to possible other reservoirs using the same SPIL definition
            Do Irsv=1,NRsv
               If (StringComp(SpillwayId(irsv), Name, CaseSensitive) .and. irsv .ne. ispil) then
                  teller = teller + 1
                  NrSpillways(irsv) = NrSpillways(ispil)
                  Do i=1,NrSpillways(Ispil)
                     IntakeLvlsSpillway(Irsv,i) = IntakeLvlsSpillway(Ispil,i)
                     QHId     (IRsv,3,i) = QHId     (Ispil,3,i)
                     MaxFlowId(IRsv,3,i) = MaxFlowId(Ispil,3,i)
                  Enddo
               Endif
            Enddo
          Endif
        Endif
        IF (ReadError) call write_error_message_rtc (974,0,'ReadRsv',' Read error during reading RTC Reservoir input SPIL',IOUT1)
     Enddo
2113 CONTINUE
     If (teller .lt. NRsv) then
         RetVal = 930
         Call Rsv_ErrMsg (972, 1, ' Not enough Spillway definitions found', &
                              ' SPIL Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
         Do Irsv=1,Nrsv
            If (NrSpillways(irsv) .eq. -1)  then
               Call Rsv_ErrMsg (972, 1, ' Missing SPIL record data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
            Endif
         Enddo
     Endif


! *********************************************************************
!    Read MAXF records
!      - maximum flow specification as Sobek time series
! *********************************************************************
     Rewind(Infile)
     if (idebug .gt. 0) write(idebug,*) ' Read MAXF records'
     endfil = .false.
     Call SKPCOM (Infile, ENDFIL,'ODS')
     Do while (.not. endfil)
       Success = GetRecord(Infile, 'MAXF', Endfil, idebug, Iout1, KeyUppUntilColumn)     ! get record van keyword MAXF tot maxf, zet in buffer
       IF (ENDFIL .or. .not. success) GOTO 3111
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       If (.not. success) goto 3111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken

! since MaxFlowId is a 3d array, and not 1d array, not using FindString routine but otherwise
! not through FindString;  Irsv = FindString (Nrsv*MaxTypeGates*MaxSameGates, MaxFlowId, TableName, Nrsv*MaxTypeGates*MaxSameGates, CaseSensitive)
          Do irsv = 1, nrsv
            Do i = 1,MaxTypeGates
              MaxGates = 1
              if (i .eq. 1) MaxGates = NrBottomGates(irsv)
              if (i .eq. 2) MaxGates = NrTurbines(irsv)
              if (i .eq. 3) MaxGates = NrSpillways(irsv)
              Do j = 1,MaxGates
                if (StringComp(MaxFlowId(irsv,i,j), TableName, CaseSensitive) )   goto 2114
              Enddo
            Enddo
          Enddo
          irsv = 0
2114      Continue
          NrColumns = 0
          Occurs = (Irsv .gt. 0)
          if (Irsv .gt. 0) then
             index1 = (irsv-1) * MaxTypeGates * MaxSameGates + (i-1) * MaxSameGates + j
             if (MaxFlowTable(index1) .gt. 0) then
                write(*,*) ' TableName', ' found index', irsv, i, j
                write(*,*) ' MaxFlowTable', (MaxFlowTable(i),i=1,9)
                write(iout1,'(A,A,A)') ' MaxFlow Definition ',&
                                         Tablename(1:len_trim(TableName)),' double in datafile Reservoir.Rtc'
             endif
             NrColumns = 1
          else
             write(*,*) ' MAXF definition ', TableName, ' skipped'
          endif
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (RTCTableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            If (.not. success) goto 3111
! Set references
            Do irsv = 1, nrsv
              Do i = 1,MaxTypeGates
                MaxGates = 1
                if (i .eq. 1) MaxGates = NrBottomGates(irsv)
                if (i .eq. 2) MaxGates = NrTurbines(irsv)
                if (i .eq. 3) MaxGates = NrSpillways(irsv)
                Do j = 1,MaxGates
                  if (StringComp(MaxFlowId(irsv,i,j), TableName, CaseSensitive) )  then
                      index1 = (irsv-1) * MaxTypeGates * MaxSameGates + (i-1) * MaxSameGates + j
                      MaxFlowTable(index1) = TableNr
                  Endif
                Enddo
              Enddo
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile, ENDFIL,'ODS')
     Enddo
3111 Continue

! Check of alle referenties naar tabellen opgelost
    Err969 = .false.
    Do irsv  = 1, nrsv
       Do i = 1,MaxTypeGates
          MaxGates = 1
          if (i .eq. 1) MaxGates = NrBottomGates(irsv)
          if (i .eq. 2) MaxGates = NrTurbines(irsv)
          if (i .eq. 3) MaxGates = NrSpillways(irsv)
          Do j = 1,MaxGates
              index1 = (irsv-1) * MaxTypeGates * MaxSameGates + (i-1) * MaxSameGates + j
              if (MaxFlowTable(index1) .eq. 0) then
                Err969 = .true.
                write(*,*) ' For reservoir ', RsvId(irsv)(1:len_trim(RsvId(Irsv))),&
                           ' maximum flow table for 1 of the gates is not found'
                write(*,*) ' Maximum flow table =', MaxFlowId(irsv,i,j)(1:len_trim(MaxFlowId(irsv,i,j)))
                Call Rsv_ErrMsg (969, 1, ' Missing MAXF data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
              endif
          Enddo
       Enddo
    Enddo
    If (err969) then
        RetVal = 930
        Call Rsv_ErrMsg (969, 1, ' Not enough Maximum Flow definitions ', &
                             ' MAXF Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
    Endif

! *********************************************************************
!    Read QHRE records
!      - Q-H interpolation relations
!      - check: the Q-H relation is monotonic in H (either increasing or decreasing)
! *********************************************************************
     Rewind(Infile)
     Endfil = .false.
     teller = 0
     AlreadyRead  = .false.
     AlreadyRead2 = .false.
     AlreadyRead3 = .false.
     if (idebug .gt. 0) write(idebug,*) ' Read QHRE records'
     Call SKPCOM (INfile, ENDFIL,'ODS')
     Do while (.not. endfil)
!       Read entire QHRE record (maybe several lines) and put it into buffer
        SearchString = 'QHRE'
        ReadError    = .false.
        Success = GetRecord (Infile, SearchString, Endfil, Idebug, Iout1, KeyUppUntilColumn)
        If (ENDFIL .or. .not. success) GOTO 2115
        String = ' '
        Success = GetStringFromBuffer (String)
        if (.not. Success) ReadError = .true.
        Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
        if (.not. Success) ReadError = .true.
        IF (ReadError) then
           call write_error_message_rtc (974,0,'Rdpara',' Read error during reading reservoir datafile - QHRE ParseToken ',IOUT1)
           RetVal = 974
           Return
        Endif
        Name = ' '
        ReadError = ReadError .and. .not. &
            SetOneVariable('id',IStart, RecordData, NumberOfTokens, ParseTokenSearchCaseSensitive, Name)
!       Read QHRE definition: number of interpolation values, levels, maximum flows
        ! Find length of table, using routine CntStr from Readlib; routine counts number of times '<' in buffer
        TableLength = Max (1, CntStr(klteken, String(1:nbuf)) )
        If (TableLength .gt. InterpLength) then
            TableLength = InterpLength
            Call Rsv_ErrMsg (972, 0, ' Length of QHRE interpolation table too long; cut-off length=15', &
                                    ' Check data QHRE records in Reservoir.Rtc file', Iout1)
        Endif
        ! replace all '<' by ' ' in order to enable free format reading from buffer string
        Ileft = INDEX(String(1:nbuf), 'TBLE') + 4
        Do idum1=ileft,nbuf-8
           if (String(idum1:idum1) .eq. klteken) String(idum1:idum1) = ' '
        Enddo
        Read(String(ileft:nbuf),*,Err=995) (HDUM(I), QDUM(I),i=1,TableLength)
!       Check consistency: H (Hdum) of the Q-H relation should be monotonically increasing or decreasing
!       (so the product of 2 consecutive H-differences should be positive (>= 0))
        Do i=2,TableLength-1
           R1 = Hdum(i) - Hdum(i-1)
           R2 = Hdum(i+1) - Hdum(i)
           if (R1*R2 .lt. 0.) then
               RetVal = 930
               Call Rsv_ErrMsg (972, 0, ' Q-H relation should be specified with H monotonic (either increasing or decreasing)', &
                                        ' Check data QHRE records in Reservoir.Rtc file', Iout1)
           endif
        Enddo
!       Assign definition to all other reservoirs using the same QHRE definition
!       Write(*,*) ' Read QHRE definition', Name
        Do Irsv=1,NRsv
          Do j=1,MaxSameGates
            index1 = (irsv-1) * MaxSameGates + j
           ! type 1 = bottom gate
!           If (QHId(irsv,1,j) .eq. Name) then
            If (StringComp(QHId(irsv,1,j), Name, CaseSensitive)) then
!             Write (*,*) ' Test reservoir - bottom gate', irsv, j, QHID(irsv,1,j)
              If (AlreadyRead(index1)) then
                 write(iout1,'(A,A,A)') ' QHRE data Bottom gate ',Name(1:len_trim(Name)),&
                                                                ' double in datafile Reservoir.Rtc'
                 write(iout1,'(A,A)') '    second data record is skipped for reservoir', &
                                           RsvId(irsv)(1:len_trim(RsvId(Irsv)))
              Else
!               Write (*,*) ' Assigned to reservoir - bottom gate', irsv, j
                teller = teller + 1
                AlreadyRead(index1) = .true.
                QHTableLength(irsv,1,j) = TableLength
                Do i=1,QHTableLength(irsv,1,j)
                   QHBottomGate(i,1,j,IRsv) = Hdum(i)
                   QHBottomGate(i,2,j,IRsv) = Qdum(i)
                Enddo
              Endif
            Endif
            ! type 2 = turbine
!           If (QHId(irsv,2,j) .eq. Name) then
            If (StringComp(QHId(irsv,2,j), Name, CaseSensitive)) then
!             Write (*,*) ' Test reservoir - turbine gate', irsv, j, QHID(irsv,2,j)
              If (AlreadyRead2(index1)) then
                 write(iout1,'(A,A,A)') ' QHRE data turbines ',Name(1:len_trim(Name)),&
                                                             ' double in datafile Reservoir.Rtc'
                 write(iout1,'(A,A)') '    second data record is skipped for reservoir', &
                                           RsvId(irsv)(1:len_trim(RsvId(Irsv)))
              Else
!               Write (*,*) ' Assigned to reservoir - turbine gate', irsv, j
                teller = teller + 1
                AlreadyRead2(index1) = .true.
                QHTableLength(irsv,2,j) = TableLength
                Do i=1,QHTableLength(irsv,2,j)
                   QHTurbine(i,1,j,IRsv) = Hdum(i)
                   QHTurbine(i,2,j,IRsv) = Qdum(i)
                Enddo
              Endif
            Endif
            ! type 3 = spillway
!           If (QHId(irsv,3,j) .eq. Name) then
            If (StringComp(QHId(irsv,3,j), Name, CaseSensitive)) then
!             Write (*,*) ' Test reservoir - spillway gate', irsv, j, QHID(irsv,3,j)
              If (AlreadyRead3(index1)) then
                 write(iout1,'(A,A,A)') ' QHRE data spillway ',Name(1:len_trim(Name)),&
                                                               ' double in datafile Reservoir.Rtc'
                 write(iout1,'(A,A)') '    second data record is skipped for reservoir', &
                                           RsvId(irsv)(1:len_trim(RsvId(Irsv)))
              Else
!               Write (*,*) ' Assigned to reservoir - spillway gate', irsv, j
                teller = teller + 1
                AlreadyRead3(index1) = .true.
                QHTableLength(irsv,3,j) = TableLength
                Do i=1,QHTableLength(irsv,3,j)
                   QHSpillway(i,1,j,IRsv) = Hdum(i)
                   QHSpillway(i,2,j,IRsv) = Qdum(i)
                Enddo
              Endif
            Endif
          Enddo
        Enddo
     Enddo
2115 CONTINUE
     MaxGates = 0
     Do irsv=1,Nrsv
        MaxGates = MaxGates + NrBottomGates(irsv) + NrTurbines(irsv) + NrSpillways(irsv)
     Enddo
     If (teller .lt. MaxGates) then
         RetVal = 930
         write(*,*) ' assigned', teller, ' definitions instead of ',MaxGates
         Call Rsv_ErrMsg (972, 1, ' Not enough QH-relation data found', &
                              ' QHRE Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
     Endif
     If (Idebug .gt. 0) then
        write(Idebug,*) ' QH relations'
        Do irsv=1,nrsv
           write(Idebug,'(A,A)') ' reservoir ', RsvId(irsv)
           write(Idebug,'(A,999F9.3)') ' Intake level bottomgate ',IntakeLvlsBottomGate(Irsv,1)
           do j=1,NrBottomGates(irsv)
             write(Idebug,'(A,999F9.3)') ' QHBottomGate - H values ',(QHBottomGate(i,1,j,Irsv),i=1,QhTableLength(irsv,1,j))
             write(Idebug,'(A,999F9.3)') ' QHBottomGate - Q values ',(QHBottomGate(i,2,j,Irsv),i=1,QhTableLength(irsv,1,j))
           enddo
           write(Idebug,'(A,999F9.3)') ' Intake level turbine ',IntakeLvlsTurbine(Irsv,1)
           do j=1,NrTurbines(irsv)
             write(Idebug,'(A,999F9.3)') ' QHTurbine    - H values ',(QHTurbine(i,1,j,Irsv),i=1,QhTableLength(irsv,2,j))
             write(Idebug,'(A,999F9.3)') ' QHTurbine    - Q values ',(QHTurbine(i,2,j,Irsv),i=1,QhTableLength(irsv,2,j))
           enddo
           write(Idebug,'(A,999F9.3)') ' Intake level spillway ',IntakeLvlsSpillway(Irsv,1)
           do j=1,NrSpillways(irsv)
             write(Idebug,'(A,999F9.3)') ' QHSpillway   - H values ',(QHSpillway(i,1,j,Irsv),i=1,QhTableLength(irsv,3,j))
             write(Idebug,'(A,999F9.3)') ' QHSpillway   - Q values ',(QHSpillway(i,2,j,Irsv),i=1,QhTableLength(irsv,3,j))
           enddo
        Enddo
     Endif
!       read error handling
        Goto 996
   995  Continue
        ReadError = .true.
   996  Continue
        IF (ReadError) call write_error_message_rtc (974,0,'ReadRsv',' Read error during reading RTC Reservoir input QHRE',IOUT1)


! *********************************************************************
!    Read ENGD records
!      - Sobek time series of energy demands
! *********************************************************************
     Rewind(Infile)
     if (idebug .gt. 0) write(idebug,*) ' Read ENGD records'
     endfil = .false.
     AlreadyRead  = .false.
     Call SKPCOM (Infile, ENDFIL,'ODS')
     Do while (.not. endfil)
       Success = GetRecord(Infile, 'ENGD', Endfil, idebug, Iout1, KeyUppUntilColumn)     ! get record van keyword ENGD tot engd, zet in buffer
       IF (ENDFIL .or. .not. success) GOTO 312
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       If (.not. success) goto 312
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
          NrColumns = 1
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
          Success = GetTable (RTCTableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
          If (.not. success) goto 312
! Set references
          Do Irsv=1,NRsv
            Do j=1,MaxSameGates
              index1 = (irsv-1) * MaxSameGates + j
              If (StringComp(EnergyId(irsv,j), TableName, CaseSensitive)) then
!               Write (*,*) ' Test reservoir - Energy data', irsv, j, EnergyId(irsv,j)
                If (AlreadyRead(index1)) then
                   write(iout1,'(A,A,A)') ' ENGD data  ',TableName(1:len_trim(TableName)),&
                                          ' double in datafile Reservoir.Rtc'
                   write(iout1,'(A,A)') '    second data record is skipped for reservoir', &
                                             RsvId(irsv)(1:len_trim(RsvId(Irsv)))
                Else
!                 Write (*,*) ' Assigned to reservoir - turbine gate', irsv, j
                  teller = teller + 1
                  AlreadyRead(index1) = .true.
                  EnergyTable(irsv,j) = TableNr
                Endif
              Endif
            Enddo
          Enddo
       Endif
       Call SKPCOM (Infile, ENDFIL,'ODS')
     Enddo
 312 Continue

! Check of alle referenties naar tabellen opgelost
    Err969 = .false.
    Do irsv  = 1, nrsv
       do j=1,NrTurbines(irsv)
          if (EnergyTable(irsv,j) .eq. 0) then
             Err969 = .true.
             write(*,*) ' For reservoir ', RsvId(irsv)(1:len_trim(RsvId(Irsv))), ' turbine gate ',j
             write(*,*) ' the table with flow for energy is not found :',EnergyId(irsv,j)(1:len_trim(EnergyId(irsv,j)))
             Call Rsv_ErrMsg (969, 1, ' Missing ENGD data for reservoir ',RsvId(Irsv)(1:len_trim(RsvId(Irsv))),Iout1)
          endif
       Enddo
    Enddo
    If (err969) then
        RetVal = 930
        Call Rsv_ErrMsg (969, 1, ' Not enough flow requirements for energy generation definitions ', &
                                 ' ENGD Data for some reservoirs not present in Reservoir.Rtc file', Iout1)
    Endif

! *********************************************************************
!    Read DEMD records
!      - Sobek time series of consumptive downstream demands for all outlet links
!      - nr. columns in tables depends on outlet link definition
! *********************************************************************
     Rewind(Infile)
     if (idebug .gt. 0) write(idebug,*) ' Read DEMD records'
     endfil = .false.
     Call SKPCOM (Infile, ENDFIL,'ODS')
     Do while (.not. endfil)
       Success = GetRecord(Infile, 'DEMD', Endfil, idebug, Iout1, KeyUppUntilColumn)     ! get record van keyword DEMD tot demd, zet in buffer
       IF (ENDFIL .or. .not. success) GOTO 412
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       If (.not. success) goto 412
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
          NrColumns = 1
          if (occurs) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (RTCTableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            If (.not. success) goto 412
          Endif
       Endif
       Call SKPCOM (Infile, ENDFIL,'ODS')
     Enddo
 412 Continue


     Deallocate (AlreadyRead, AlreadyRead2, AlreadyRead3)
     Deallocate (HavId, RuleCurveId, HedgingId, &
                 BottomGateId, TurbineId, SpillwayId, &
                 MaxFlowId, QHId, EnergyId)

! Determine Dead level and dead storage = level and storage of lowest outlet
! Determine Full rsv level and storage = level and storage of lowest spillway outlet
! Taiwan Nov2003: FullRsvLevel = MAX of spillway outlet
     Do Irsv=1,Nrsv
        Do ibotg=1,NrBottomGates(irsv)
           DeadLevel(irsv) = min (DeadLevel(irsv), IntakeLvlsBottomGate(irsv,ibotg))
        Enddo
        Do iturb=1,NrTurbines(irsv)
           DeadLevel(irsv) = min (DeadLevel(irsv), IntakeLvlsTurbine (irsv,iturb))
        Enddo
        Do ispil=1,NrSpillways(irsv)
           DeadLevel(irsv) = min (DeadLevel(irsv), IntakeLvlsSpillway(irsv,ispil))
           FullRsvLevel(irsv) = max (FullRsvLevel(irsv), IntakeLvlsSpillway(irsv,ispil))
        Enddo
        Call LevelToVolume (DeadLevel(irsv), DeadStorage(irsv), irsv)
        Call LevelToVolume (FullRsvLevel(irsv), FullRsvStorage(irsv), irsv)
     Enddo

     Return


     150 CONTINUE
         call write_error_message_rtc (974, 1, ' Read error in ReadReservoirInput',' ',Iout1)
         RetVal = 974

     Return
     End Function ReadReservoirInput


!!   Extra toegevoegen aan parameterlijst
!!     - desired demand per link outlet
!!     - max. flow met link outlet

     Function RibasimReservoir (Irsv, Nrsv, MaxAllowedLvl, InitLevel, ExpInflow, &
                                DesiredQCons, MaxFlowOutletLinks, &
                                RtcTableHandle, Idebug, Iout1, TimestepSize, &
                                IYear, IMonth, IDay, IHour, IMinute, ISec , &
                                Results, HFlood, HTarget, HFirm) result (RetVal)

! *********************************************************************
!    Simulate reservoir in a Ribasim look-alike way
!    Output to decision parameters: the computed flows QBottomGate, Turbine, Spillway
!    Output to decision parameters: the actual rule curve levels
! *********************************************************************

     Integer          RetVal

     Integer          RtcTableHandle
     Integer          irsv, nrsv, idebug, iout1, TimestepSize
     Integer          IYear, IMonth, IDay, IHour, IMinute, ISec
     Double precision MaxAllowedLvl, InitLevel, ExpInflow
     Double precision Results(MaxTypeGates*MaxSameGates), HFirm, HTarget, HFlood
     Double precision HedgingLevelsNow(InterpLength), HedgingReleasesNow(InterpLength)
     Double precision HedgingVolumesNow(InterpLength)

     Integer          i, j, index, ilink, Iter, iturb
     Double precision QBottomGate(MaxSameGates), &
                      QTurbine(MaxSameGates), &
                      QSpillway(MaxSameGates)
     Double precision Sinit, Sprov, SInitAndInflow, HinitAndInflow, Hend, Hprov, Hav, Qextra, &
                      SFirm, STarget, SFlood, &
                      DesiredRelease(MaxTypeGates*MaxSameGates), &
                      DesiredQCons(MaxTypeGates*MaxSameGates), DesiredQEnergy(MaxSameGates), &
                      DesiredReleaseForEnergy(MaxTypeGates*MaxSameGates), &
                      ActualRelease(MaxTypeGates*MaxSameGates), QMax(MaxTypeGates*MaxSameGates), &
                      ReduceRelease(MaxTypeGates*MaxSameGates), &
                      TempRelease(MaxTypeGates*MaxSameGates), &
                      Eps1, Eps2, Sprov1, HProv1
     Double Precision QMxGate(MaxSameGates), QMxTurb(MaxSameGates), QMxSpil(MaxSameGates), &
                      MaxFlowOutletLinks(MaxTypeGates*MaxSameGates)
     Double Precision QExtraTurbine, QExtraBottomGate, QExtraSpillway, QTemp, QAllocThroughTurbines, QReduce
     Logical          Converged, CheckNextHedgingLevel
     Double Precision ReductionAvailable, QReduction, Reduction, VolumeInZone, TotalRelease, Ratio

     Integer Rownr, TabelNr
     logical DateTimeOutsideTable

     type (Date) currentDate
     type (Time) currentTime

! *********************************************************************
!    Initialisation
!    Set convergence criteria
! *********************************************************************

     QMxGate = 0.0
     QMxTurb = 0.0
     QMxSpil = 0.0

     currentDate%year   = iYear
     currentDate%month  = iMonth
     currentDate%day    = iDay
     currentTime%hour   = iHour
     currentTime%minute = iMinute
     currentTime%second = iSec

     if (Idebug .gt. 0) then
         write(idebug,*) ' RibasimReservoir ', irsv
         write(idebug,*) ' MaxAllowedLvl', MaxAllowedLvl
         write(idebug,*) ' InitLevel  ', InitLevel
         write(idebug,*) ' ExpInflow  ', ExpInflow
         write(idebug,*) ' DesiredQCons ', (DesiredQCons(ilink),ilink=1,NrOutletLinks(irsv))
         write(idebug,*) ' MaxFlowOutletLinks ', (MaxFlowOutletLinks(ilink),ilink=1,NrOutletLinks(irsv))
         write(idebug,*) ' Timestepsize in seconds ', TimestepSize
         write(idebug,*) ' Current date : ', IYear, iMonth, IDay
         write(idebug,*) ' Current time : ', IHour, IMinute, ISec
     endif

     RetVal = 0
     Hend = InitLevel
     Hav  = InitLevel
     Converged = .false.
     Eps1 = EpsVolume ! 0.1    volume criterion
     Eps2 = EpsFlow   ! 0.001  flow criterion
     iter   = 0

!    Find Initial volume corresponding with InitLevel using HavRsv
     Call LevelToVolume (InitLevel, Sinit, irsv)
     SInitAndInflow = Sinit + ExpInflow * TimestepSize
     Call VolumeToLevel (SInitAndInflow, HInitAndInflow, irsv)

! *********************************************************************
! Determine desired releases on various downstream links
! taking into account consumptive and energy demands
! *********************************************************************
! DesiredRelease, DesiredReleaseForEnergy, ActualRelease: on NrOutletLinks,
!  at most x=MaxTypeGates=3 different types of gates  (in case of bottom gate, turbine and spillway on different links)
!  at most y=MaxSameGates   of the same kind
!  Determine desired release for downstream consumptive demand, all downstream links from a Sobek TimeTable
!!! Now through decision parameter
! *********************************************************************
!    Find desired release for energy generation; from Sobek TimeTable
     RowNr = -1
     Do iturb =1,NrTurbines(irsv)
        TabelNr = EnergyTable (irsv,iturb)
        DesiredQEnergy(iturb) = GetNewValue(RTCTableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime,&
                                            Idebug, iout1, DateTimeOutsideTable, .true.)
     Enddo

!    Determine desired downstream demands on all downstream links
!    (take into account LinkOutlets definitions, consumptive and energy demands
     DesiredRelease = 0.0
     Do ilink=1,NrOutletLinks(irsv)
        DesiredRelease(ilink) = DesiredQCons(ilink)
     Enddo
!    Find turbine outlet link and set DesiredReleaseLinkEnergy on that link
     DesiredReleaseForEnergy = 0.0
     Do iturb=1,NrTurbines(irsv)
        Ilink = LinkOutlets(irsv,2,iturb)
        DesiredReleaseForEnergy(Ilink) = DesiredReleaseForEnergy(Ilink) + DesiredQenergy(iturb)
     Enddo
!    Set DesiredRelease on link >= DesiredRelease for downstream demand and >= DesiredReleaseLinkEnergy
     Do ilink=1,MaxTypeGates*MaxSameGates
        DesiredRelease(Ilink) = Max (DesiredRelease(ilink), DesiredReleaseForEnergy(ilink))
     Enddo

! *********************************************************************
!    find operation rules: Hflood, Htarget, Hfirm from time tables
! *********************************************************************
     HFlood  = 0.0
     HTarget = 0.0
     HFirm   = 0.0
     TabelNr = RuleCurveTable (irsv)
     RowNr  = -1
     HFlood = GetNewValue(RtcTableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                          Idebug, iout1, DateTimeOutsideTable, .true.)
     RowNr  = -1
     HTarget= GetNewValue(RtcTableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                          Idebug, iout1, DateTimeOutsideTable, .true.)
     RowNr  = -1
     HFirm  = GetNewValue(RtcTableHandle, TabelNr, 3, RowNr, CurrentDate, CurrentTime, &
                          Idebug, iout1, DateTimeOutsideTable, .true.)
!    add hedging levels
     Do i=1,HedgingTableLength(irsv)
        HedgingLevelsNow(i)   = DeadLevel(irsv) +  HedgingLevelPercentage(i,IRsv) * (Hfirm - DeadLevel(irsv))
        HedgingReleasesNow(i) = HedgingReleasePercentage(i,IRsv)
     Enddo
! Taiwan nov 2003
!    Restrict flood levels to MaxAllowedLvl (which is >= firm level for consistency reasons)
     MaxAllowedLvl = max (MaxAllowedLvl,HFirm)
     HFlood  = min (HFlood, MaxAllowedLvl)
! end Taiwan nov 2003

!    Consistency checks (Flood >= Target >=Firm) and check with full reservoir level and dead storage level
     HFlood  = min (HFlood, FullRsvLevel(irsv))
     HTarget = min (HTarget, HFlood)
     HFirm   = min (HFirm, HTarget)
     HFlood  = max (HFlood, DeadLevel(irsv))
     HTarget = max (HTarget, DeadLevel(irsv))
     HFirm   = max (HFirm, DeadLevel(irsv))

!    convert to Sflood, Starget, Sfirm
     Call LevelToVolume (HFlood , SFlood , irsv)
     Call LevelToVolume (HTarget, STarget, irsv)
     Call LevelToVolume (HFirm  , SFirm  , irsv)
!    convert hedging levels to storages
     Do i=1,HedgingTableLength(irsv)
        Call LevelToVolume (HedgingLevelsNow(i), HedgingVolumesNow(i), irsv)
     Enddo

! *********************************************************************
!    Find maximum flows from Sobek time table
! *********************************************************************
!    bottom gate i=1
     i = 1
     Do j = 1, NrBottomGates(irsv)
       index = (irsv-1) * MaxTypeGates * MaxSameGates + (i-1) * MaxSameGates + j
       TabelNr = MaxFlowTable(index)
       RowNr  = -1
       QMax(j) = GetNewValue(RtcTableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                             Idebug, iout1, DateTimeOutsideTable, .true.)
     Enddo
!    turbine gate i=2
     i = 2
     Do j = 1, NrTurbines(irsv)
        index = (irsv-1) * MaxTypeGates * MaxSameGates + (i-1) * MaxSameGates + j
        TabelNr = MaxFlowTable(index)
        RowNr  = -1
        Qmax(MaxSameGates+j) = GetNewValue(RtcTableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime,&
                                           Idebug, iout1, DateTimeOutsideTable, .true.)
     Enddo
!    spillway gate i=3
     i = 3
     Do j = 1, NrSpillways(irsv)
        index = (irsv-1) * MaxTypeGates * MaxSameGates + (i-1) * MaxSameGates + j
        TabelNr = MaxFlowTable(index)
        RowNr  = -1
        Qmax(2*MaxSameGates+j) = GetNewValue(RtcTableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime,&
                                             Idebug, iout1, DateTimeOutsideTable, .true.)
     Enddo

! *********************************************************************
!    First provisional water balance
!    Compute provisional end storage, using desired release
!      (release may be reduced if Sprov would become negative!)
!    Find corresponding level at the end of the timestep
! *********************************************************************
     Call SetSprov (Sinit, ExpInflow, DesiredRelease, Sprov, TimestepSize, Idebug)

     Call VolumeToLevel (Sprov, HProv, irsv)

     if (Idebug .gt. 0) then
         write(idebug,*) ' RibasimReservoir ', irsv
         write(idebug,*) ' Maximum releases ', QMax
         write(idebug,*) ' H and S - flood control curve ', HFlood, SFlood
         write(idebug,*) ' H and S - target rule curve   ', HTarget, STarget
         write(idebug,*) ' H and S - firm storage curve  ', HFirm, SFirm
         write(idebug,*) ' H and S - hedging storages    '
         Do i=1,HedgingTableLength(irsv)
            write(Idebug,*) ' i - level - volume ',i,HedgingLevelsNow(i), HedgingVolumesNow(i)
         Enddo
         write(idebug,*) ' H and S - dead storage curve  ', DeadLevel(irsv), DeadStorage(irsv)
         write(idebug,*) ' Desired release           ', DesiredRelease
         write(idebug,*) ' Desired releaseForEnergy  ', DesiredReleaseForEnergy
         write(idebug,*) ' ActualRelease             ', ActualRelease
         write(idebug,*) ' Expected inflow  ', ExpInflow
         write(idebug,*) ' Timestepsize in seconds ', TimestepSize
         write(idebug,*) ' Initial water level and volume', InitLevel, SInit
         write(idebug,*) ' First provisional end H and S ', HProv, Sprov
     endif

! *********************************************************************
!    First step: Iterate on discharge capacities until converged
! *********************************************************************
 101 Continue
     iter = iter + 1
     Sprov1 = Sprov
     Hprov1 = Hprov

!    set average water level
     Hav = 0.5 * (InitLevel + HProv)

!    Check with discharge capacities for desired release;
     Call SetReleaseCapacities (DesiredRelease, ActualRelease, Qmax, Hav, Irsv,  &
                                QMxGate, QMxTurb, QMxSpil, Idebug, SInitAndInflow, TimestepSize, 1)
     Do ilink = 1, NrOutletLinks(Irsv)
        ActualRelease(Ilink) = min (ActualRelease(ilink), MaxFlowOutletLinks(ilink))
     Enddo

     if (Idebug .gt. 0) then
         write(idebug,*) ' RibasimReservoir and iteration', irsv, iter
         write(idebug,*) ' used average level' , Hav
         write(idebug,*) ' QmxGate ', QmxGate
         write(idebug,*) ' QmxTurb ', QmxTurb
         write(idebug,*) ' QmxSpil ', QmxSpil
         write(idebug,*) ' Actual Release after SetReleaseCapacities ', ActualRelease
     endif

! *********************************************************************
!    Compute provisional end storage, using actual release
!    (release may be reduced if Sprov would become negative!)
!    And find corresponding level at the end of the timestep
! *********************************************************************
     Call SetSprov (Sinit, ExpInflow, ActualRelease, Sprov, TimestepSize, Idebug)
     Call VolumeToLevel (Sprov, HProv, irsv)
     if (Idebug .gt. 0) then
         write(idebug,*) ' Iterate - provisional end H and S ', HProv, Sprov
     endif
     Converged = (Abs(Sprov-Sprov1) .lt. Eps1 .and. Abs(Hprov-Hprov1) .lt. Eps2)
     If (.not. Converged .and. iter .lt. MxIter) goto 101

! *********************************************************************
!    Iteration on discharge capacities has converged
! *********************************************************************
!    Now, Iterate on operation rules until converged;
!      - extra releases may be forced through any gate (if above flood control level)
!      - extra releases may be forced through turbine gate only (if above target level)
!      - reduction of releases may be forced (if below firm level, due to hedging rules)
! *********************************************************************
     iter   = 0
     Converged = .false.
 102 Continue
     iter = iter + 1
     Sprov1 = Sprov
     Hprov1 = Hprov

!    set average water level
     Hav = 0.5 * (InitLevel + HProv)

!    Check with discharge capacities after actual releases / operation rules
     Call SetReleaseCapacities (ActualRelease, ActualRelease, Qmax, Hav, Irsv, &
                                QMxGate, QMxTurb, QMxSpil, Idebug, SinitAndInflow, TimestepSize, 2)
     Do ilink = 1, NrOutletLinks(Irsv)
        ActualRelease(Ilink) = min (ActualRelease(ilink), MaxFlowOutletLinks(ilink))
     Enddo
     if (Idebug .gt. 0) then
         write(idebug,*) ' After 2nd call SetReleaseCapacities'
         write(idebug,*) ' used average level' , Hav
         write(idebug,*) ' QmxGate ', QmxGate
         write(idebug,*) ' QmxTurb ', QmxTurb
         write(idebug,*) ' QmxSpil ', QmxSpil
         write(idebug,*) ' Actual Release after SetReleaseCapacities ', ActualRelease
     endif


! *********************************************************************
!    Distribute releases over different outlet gates
! *********************************************************************
     QTurbine = 0
     QBottomGate = 0
     QSpillway = 0
     Do ilink = 1, NrOutletLinks(Irsv)
        if (idebug .gt. 0) write(idebug,*) ' Link and ActualRelease to be distributed',ilink, ActualRelease(ilink)
        Qtemp  = ActualRelease(ilink)
        QallocThroughTurbines = 0.
        ! turbines
        Call SetFlows (irsv, nrsv, 2, ilink, NrTurbines, QTurbine, QMxTurb, Qtemp)
        QExtra = max (0.0, ActualRelease(ilink)-DesiredReleaseForEnergy(ilink), &
                              ActualRelease(ilink)-DesiredReleaseForEnergy(ilink) )
        if (QExtra .gt. 0.0) then
           !  actual release exceeds maximum of the demands, so extra release computed because of rule curves
           !  These ExtraReleases are allowed through any of the gates
           !  no adjustment of ActualRelease here
        else
           ! ActualRelease equals maximum of the demands or lower (because of limiting discharge capacities)
           ! No extra releases due to rule curves
           ! Make sure bottom gate and spillway are only used for remaining downstream consumptive demands
           ! ActualRelease also includes demands for energy, which only may be released through the turbines
            QallocThroughTurbines = ActualRelease(ilink) - Qtemp
! Mar2003 the following was not working correctly
!           if (QAllocThroughTurbines .lt. DesiredQCons(ilink)) then
!             Qreduce = Qtemp
!             Qtemp   = min (Qtemp, DesiredQCons(ilink) - QAllocThroughTurbines)
!             Qreduce = max (0.0, Qreduce - Qtemp)
!             ActualRelease(ilink) = ActualRelease(ilink) - Qreduce
!           endif
! It is replaced by:
            if (QAllocThroughTurbines .lt. DesiredReleaseForEnergy(ilink)) then
              Qreduce = min (Qtemp, DesiredReleaseForEnergy(ilink) - QAllocThroughTurbines)
              Qreduce = max (0.0, Qreduce)
              QTemp = max (0.0, QTemp - Qreduce)
              ActualRelease(ilink) = ActualRelease(ilink) - Qreduce
            endif
! end Mar2003
            if (idebug .gt. 0) then
               write(idebug,*) ' Allocated through turbines', QAllocThroughTurbines
               write(idebug,*) ' Total Energydemand        ', DesiredReleaseForEnergy(ilink)
               write(idebug,*) ' Total DesiredQCons        ', DesiredQCons(ilink)
               write(idebug,*) ' Shortage on energy demand ', QReduce
               write(idebug,*) ' Remaining ActualRelease to be distributed',QTemp
            endif
        endif
        ! bottom gates
        Call SetFlows (irsv, nrsv, 1, ilink, NrBottomGates, QBottomGate, QMxGate, Qtemp)
        ! spillways
        Call SetFlows (irsv, nrsv, 3, ilink, NrSpillways, QSpillway, QMxSpil, Qtemp)
     Enddo

     If (Idebug .gt. 0) then
         write(idebug,*) ' Before check with operation rules '
         write(idebug,*) ' Actual Release ', ActualRelease
         write(idebug,*) ' QBottomgate', QBottomGate
         write(idebug,*) ' QTurbine   ', QTurbine
         write(idebug,*) ' QSpillway  ', QSpillway
         write(idebug,*) ' Hend       ', Hend
         write(idebug,*) ' Send       ', Sprov
     Endif

! *********************************************************************
!    Check with operation rules
! *********************************************************************

     ReduceRelease = 0.0
!    First flood control curve
     If (Hprov .gt. Hflood) then
!       provisional end level above flood control curve
!       try to release by any means (turbine, bottom gate, spillway)
        Qextra = (Sprov - Sflood)/ TimestepSize
        If (NrOutletLinks(Irsv) .eq. 1) then
           ! bottom gate, turbine and spillway all on same link = outlet link 1
           ! limit QExtra by physical and managerial discharge capacities
           QExtraTurbine    = 0.0
           QExtraBottomGate = 0.0
           QExtraSpillway   = 0.0
           Do j=1,NrTurbines(irsv)
               QExtraTurbine = QextraTurbine + (QmxTurb(j) - QTurbine(j))
           Enddo
           Do j=1,NrBottomGates(irsv)
               QExtraBottomGate = QextraBottomGate + (QmxGate(j) - QBottomGate(j))
           Enddo
           Do j=1,NrSpillways(irsv)
               QextraSpillway = QextraSpillway + (QmxSpil(j) - QSpillway(j))
           Enddo
           QExtra = min (QExtra, QExtraTurbine + QExtraSpillway + QExtraBottomGate)
           ActualRelease(1) = ActualRelease(1) + QExtra
           ! but still satisfy the specified maximum flows on the outlet link !!
           ActualRelease(1) = min (ActualRelease(1), MaxFlowOutletLinks(1))
        Else
           ! outlets on different links; order of spilling is: turbine, bottom gate, spillway
           ! first turbines
           Call TryExtraRelease (NrTurbines, QMxTurb, QTurbine, QExtra, &
                                 ActualRelease, irsv, nrsv, 2, idebug, ' Extra release turbines')
           ! then bottom gates
           Call TryExtraRelease (NrBottomGates, QMxGate, QBottomGate, QExtra, &
                                 ActualRelease, irsv, nrsv, 1, idebug, ' Extra release bottomgate')
           ! then spillways
           Call TryExtraRelease (NrSpillways, QMxSpil, QSpillway, QExtra, &
                                 ActualRelease, irsv, nrsv, 3, idebug, ' Extra release spillway')
           ! but still satisfy the specified maximum flows on the outlet links !!
           Do ilink = 1, NrOutletLinks(Irsv)
              ActualRelease(Ilink) = min (ActualRelease(ilink), MaxFlowOutletLinks(ilink))
           Enddo
        Endif
        Call SetSprov (Sinit, ExpInflow, ActualRelease, Sprov, TimestepSize, Idebug)
        Call VolumeToLevel (Sprov, HProv, irsv)
     Endif

! Then check target or firm curve
     If (Hprov .gt. HTarget) then
!       Provisional end level above target curve
!       Try to release more through the turbines only
        Qextra = (Sprov - STarget)/ TimestepSize
        Call TryExtraRelease (NrTurbines, QMxTurb, QTurbine, QExtra, &
                              ActualRelease, irsv, nrsv, 2, idebug, ' Extra release turbines')
       ! but still satisfy the specified maximum flows on the outlet links !!
        Do ilink = 1, NrOutletLinks(Irsv)
           ActualRelease(Ilink) = min (ActualRelease(ilink), MaxFlowOutletLinks(ilink))
        Enddo
        Call SetSprov (Sinit, ExpInflow, ActualRelease, Sprov, TimestepSize, Idebug)
        Call VolumeToLevel (Sprov, HProv, irsv)
     ElseIf (Hprov .lt. HFirm) then
!       provisional end level below firm storage; try to reduce releases using hedging rules
!       TotalRelease = current sum of actual releases (checked with capacities, but not yet with rule curves)
!       check each zone, find volume to be kept in that zone, increase Sprov with that volume
!       reduce releases with the corresponding flow (sum over all zones) Qreduction
        TotalRelease = 0.0
        Do i=1,MaxTypeGates*MaxSameGates
           TotalRelease = TotalRelease + ActualRelease(i)
        Enddo
        ReductionAvailable = TotalRelease * TimestepSize
        QReduction = 0.0
        i = 0
        CheckNextHedgingLevel = (ReductionAvailable .gt. 0.0)
        Do while (i .lt. HedgingTableLength(irsv) .and. CheckNextHedgingLevel)
           i = i+1
!       Compatible with Ribasim is the following principle:
!         from Hedging level 1   ]
!                                ]  apply release perc. 1
!         to   Hedging level 2   ]
!         from Hedging level 2   ]]
!                                ]] apply release perc. 2
!         to   Hedging level 3   ]]
!         from Hedging level 3   ]
!                                ]  apply release perc. 3
!         to   Hedging level 4   ]
!         from Hedging level 4   ]
!                                ]] apply release perc. 4
!         to   Hedging level 5   ]]
!         etc.
!         Furthermore, apply hedging rules only from the maximum level HInitAndInflow to the provisional end level HProv.
           if (Idebug .gt. 0) then
              write(Idebug,*) ' HProv           ',   Hprov
              write(Idebug,*) ' HedgingLevels(i)',   i, HedgingLevelsNow(i)
              write(Idebug,*) ' HInitAndInflow', HInitAndInflow
              write(Idebug,*) ' HedgingLevels(i+1)', i+1, HedgingLevelsNow(i+1)
           endif
           if (Hprov .lt. HedgingLevelsNow(i) .and. HinitAndInflow .ge. HedgingLevelsNow(i+1)) then
               if (Idebug .gt. 0) write(Idebug,*) ' Check Hedging level i with Hprov ', i,HedgingLevelsNow(i),Hprov
               VolumeinZone = min (SinitAndInflow,HedgingVolumesNow(i)) - HedgingVolumesNow(i+1)
               VolumeInZone = min (VolumeInZone, ReductionAvailable)
               Reduction = (1.0 - HedgingReleasesNow(i)) * VolumeinZone
               if (Idebug .gt. 0) then
                  write(Idebug,*) ' VolumeInZone', VolumeInZone
                  write(Idebug,*) ' ReductionAvailable looking at TotalRelease', ReductionAvailable
                  write(Idebug,*) ' HedgingReleasesNow', HedgingReleasesNow(i)
                  write(Idebug,*) ' computed Reduction ', Reduction
               endif
               ReductionAvailable = ReductionAvailable - VolumeInZone
               QReduction = QReduction + Reduction
               Sprov = Sprov + Reduction
               Call VolumeToLevel (Sprov, HProv, irsv)
               CheckNextHedgingLevel = (ReductionAvailable .gt. 0.0)
               if (Idebug .gt. 0) then
                  write(Idebug,*) ' Total QReduction as volume', QReduction
                  write(Idebug,*) ' New Sprov', Sprov
                  write(Idebug,*) ' New Hprov', Hprov
               endif
           elseif (Hprov .gt. HedgingLevelsNow(i)) then
               CheckNextHedgingLevel = .false.
           endif
        Enddo
        QReduction = QReduction / TimestepSize
        if (Idebug .gt. 0) write(Idebug,*) ' hedging rules : reduce flow with ', QReduction
!       Reductions will be simply divided proportionally over all link outlets
!       In case of several gates on the same link outlet,
!          water is first allocated to the turbines, than bottom gate, than the spillway
        Ratio = 0.0
        if (TotalRelease .gt. 0) Ratio = (QReduction / TotalRelease)
        Do i=1,MaxTypeGates*MaxSameGates
           ReduceRelease(i) = Ratio * ActualRelease(i)
        Enddo
        TempRelease = ActualRelease - ReduceRelease
        Call SetSprov (Sinit, ExpInflow, TempRelease, Sprov, TimestepSize, Idebug)
        Call VolumeToLevel (Sprov, HProv, irsv)
     Endif

     If (Idebug .gt. 0) then
         write(idebug,*) ' Iterate - check with rule curves - end H and S ', HProv, Sprov
     Endif

! *********************************************************************
!   Check if convergence achieved
!   if so, distribute actual releases (which are possible according to discharge capacities)
!          over bottom gate, spillway, turbine
!          first turbine up to capacity, then bottom gate, then spillway
!   save final results
! *********************************************************************
     Converged = (Abs(Sprov-Sprov1) .lt. Eps1 .and. Abs(Hprov-Hprov1) .lt. Eps2)
     If (.not. Converged .and. iter .lt. MxIter) goto 102

     HEnd = Hprov
     if (idebug .gt. 0) then
        Write(Idebug,*) ' Distributing release over all outlets'
        Write(Idebug,*) ' ActualRelease ', ActualRelease
        Write(Idebug,*) ' ReduceRelease ', ReduceRelease
     endif
     Do ilink = 1, NrOutletLinks(Irsv)
        ! turbine
        ActualRelease(ilink) = ActualRelease(ilink) - ReduceRelease(ilink)
        Call SetFlows (irsv, nrsv, 2, ilink, NrTurbines, QTurbine, QMxTurb, ActualRelease(ilink))
        ! bottom gate
        Call SetFlows (irsv, nrsv, 1, ilink, NrBottomGates, QBottomGate, QMxGate, ActualRelease(ilink))
        ! spillway
        Call SetFlows (irsv, nrsv, 3, ilink, NrSpillways, QSpillway, QMxSpil, ActualRelease(ilink))
        ! remaining flow to be distributed should be zero
        If (abs(ActualRelease(ilink)) .gt. eps2) then
           Write(Iout1,*) ' Ribasim reservoir - date', irsv, iyear, imonth, iday, ihour
           Write(Iout1,*) ' ActualRelease not zero - link', ActualRelease, ilink
           if (idebug .gt. 0) then
              Write(Idebug,*) ' Ribasim reservoir - date', irsv, iyear, imonth, iday, ihour
              Write(Idebug,*) ' ActualRelease not zero - link', ActualRelease, ilink
           endif
           if (iter .lt. MxIter)  RetVal = 930
           Call Rsv_ErrMsg (930, 1, ' RibasimReservoir', &
                                ' Error distributing releases over gates', IOUT1)
        endif
     Enddo

     If (Idebug .gt. 0) then
         write(idebug,*) ' QBottomgate', QBottomGate
         write(idebug,*) ' QTurbine   ', QTurbine
         write(idebug,*) ' QSpillway  ', QSpillway
         write(idebug,*) ' Hend       ', Hend
         write(idebug,*) ' Send       ', Sprov
     Endif

! Assign computation results to output array to RTC
     i = 0
     Do j=1,NrBottomGates(irsv)
        i = i+1
        Results(i) = QBottomGate(j)
     Enddo

     Do j=1,NrTurbines(irsv)
        i = i+1
        Results(i) = QTurbine(j)
     Enddo

     Do j=1,NrSpillways(irsv)
        i = i+1
        Results(i) = QSpillway(j)
     Enddo


     Return
     End Function RibasimReservoir



     Subroutine TryExtraRelease (NrGates, MxFlowGates, QGate, QExtra, &
                                 ActualRelease, irsv, nrsv, TypeOfGate, idebug, String)
! *********************************************************************
!    Try to release more water through type of gate TypeOfGate, (1=bottomgate, 2=turbine, 3=spillway)
!    There are NrGates(irsv) number of gates of this type for this reservoir
!    The computed maximum flow through the gates is MxFlowGates
!    The computed flow through the gates is QGate
!    The desired amount to release extra is QExtra
!    The actual releases are set in the array ActualRelease
! *********************************************************************

     Integer          nrsv
     Integer          NrGates(nrsv), Irsv, TypeOfGate, Idebug
     Double precision MxFlowGates(MaxSameGates), ActualRelease(MaxTypeGates*MaxSameGates)
     Double precision QGate (MaxSameGates), Qextra
     Character(*)     String

     Integer          j, ilink
     Double precision QExtraThroughGate

     Do j=1,NrGates(irsv)
        ilink = LinkOutlets(irsv,TypeOfGate,j)
        if (ilink .gt. 0) then
           QextraThroughGate = min (Qextra, MxFlowGates(j) - QGate(j))
           QextraThroughGate = max (0.0, QExtraThroughGate)
           Qextra = Qextra - QextraThroughGate
           ActualRelease(ilink) = ActualRelease(ilink) + QextraThroughGate
           If (Idebug .gt. 0) write(Idebug,*)  String, QextraThroughGate, ' via link', ilink
        endif
     Enddo

     Return
     End Subroutine TryExtraRelease


     Subroutine SetFlows (irsv, nrsv, TypeOfGate, ilink, NrGates, QGate, MxFlowGates, Qtemp)

! *********************************************************************
!    Set flows through type of gate TypeOfGate, (1=bottomgate, 2=turbine, 3=spillway)
!    There are NrGates(irsv) number of gates of this type for this reservoir
!    The computed maximum flow through the gates is MxFlowGates
!    The computed flow through the gates is QGate
!    The desired amount to release in total is QTemp
! *********************************************************************

     Integer          nrsv
     Integer          NrGates(nrsv), Irsv, TypeOfGate
     Double precision MxFlowGates(MaxSameGates)
     Double precision QGate (MaxSameGates), Qtemp

     Integer          j, ilink

     Do j=1,NrGates(irsv)
        if (LinkOutlets(irsv,TypeOfGate,j) .eq. ilink) then
           QGate(j) = min (Qtemp, MxFlowGates(j))
           Qtemp = Qtemp - QGate(j)
        endif
     Enddo

     Return
     End Subroutine SetFlows


     Subroutine SetReleaseCapacities (DesiredRelease, ActualRelease, QMax, Hav, Irsv, &
                        QMxGate, QMxTurb, QMxSpil, Idebug, SInitAndInflow, TimestepSize, iMode)
! *********************************************************************
!    Compute actual release, and maximum flows through bottom gate, turbine and spillway
!    looking at desired release
!    taking care of discharge capacities at average level Hav
! *********************************************************************
!
      Double Precision DesiredRelease(MaxTypeGates*MaxSameGates), &
                       ActualRelease(MaxTypeGates*MaxSameGates), &
                       QMax(MaxTypeGates*MaxSameGates)
      Double Precision Hav, SInitAndInflow, SIntakeLvl
      Double Precision QMxGate(MaxSameGates), QMxTurb(MaxSameGates), QMxSpil(MaxSameGates)

      Integer          idebug, irsv, IMode, index, TimestepSize
      Integer          i, j,ilink, nval, idum
      Double Precision NtHead, QDMax(MaxTypeGates*MaxSameGates)

!     bottom gates
      Do j=1, NrBottomGates(irsv)
         NVal = QHTableLength(irsv,1,j)
         NtHead = Hav - IntakeLvlsBottomGate(Irsv,j)
         if (NtHead .lt. 0.) then
            QMxGate(j) = 0.0
         else
            Call INTERP_Double (nVal, QHBottomGate(1,1,j,IRsv), QHBottomGate(1,2,j,IRsv), NtHead, QmxGate(j), idum)
         endif
         if (Idebug .gt. 0)  write(idebug,*) ' NtHead Bottom gate j - QmxGate physical ', j, NtHead, QMxGate(j)
         if (imode .eq. 2) then
           Call LevelToVolume (IntakeLvlsBottomGate(irsv,j), SIntakeLvl, irsv)
           QMxGate(j) = min (QmxGate(j), (SInitAndInflow-SIntakeLvl)/TimestepSize)
           if (Idebug .gt. 0)  write(idebug,*) ' QmxGate volumecheck', QMxGate(j)
         endif
      Enddo
!     turbines
      Do j=1, NrTurbines(irsv)
         NVal = QHTableLength(irsv,2,j)
         NtHead = Hav - IntakeLvlsTurbine(Irsv,j)
         if (NtHead .lt. 0.) then
            QMxTurb(j) = 0.0
         else
            Call INTERP_Double (nVal, QHTurbine   (1,1,j,IRsv), QHTurbine (1,2,j,IRsv), NtHead, QmxTurb(j), idum)
         endif
         if (Idebug .gt. 0) write(idebug,*) ' NtHead Turbine j - QmxTurb physical', j,NtHead, QMxTurb(j)
         if (imode .eq. 2) then
           Call LevelToVolume (IntakeLvlsTurbine(irsv,j), SIntakeLvl, irsv)
           QMxTurb(j) = min (QmxTurb(j), (SInitAndInflow-SIntakeLvl)/TimestepSize)
           if (Idebug .gt. 0)  write(idebug,*) ' QmxTurb volumecheck', QMxTurb(j)
         endif
      Enddo
!     spillway
      Do j=1, NrSpillways(irsv)
         NVal = QHTableLength(irsv,3,j)
         NtHead = Hav - IntakeLvlsSpillway(Irsv,j)
         if (NtHead .lt. 0.) then
            QMxSpil(j) = 0.0
         else
            Call INTERP_Double (nVal, QHSpillway  (1,1,j,IRsv), QHSpillway  (1,2,j,IRsv), NtHead, QmxSpil(j), idum)
         endif
         if (Idebug .gt. 0)  write(idebug,*) ' NtHead Spillway j - QMxSpil physical',j, NtHead, QMxSpil(j)
         if (imode .eq. 2) then
           Call LevelToVolume (IntakeLvlsSpillway(irsv,j), SIntakeLvl, irsv)
           QMxSpil(j) = min (QmxSpil(j), (SInitAndInflow-SIntakeLvl)/TimestepSize)
           if (Idebug .gt. 0)  write(idebug,*) ' QmxSpil volumecheck', QMxSpil(j)
         endif
      Enddo

! Maximum flow per gate
      Do i=1,MaxTypeGates
         Do j=1,MaxSameGates
! fout     index = (irsv-1) * MaxTypeGates * MaxSameGates + (i-1) * MaxSameGates + j
           index = (i-1) * MaxSameGates + j
           if (i .eq. 1) then
              QMxGate(j) = min (QMxGate(j), QMax(index))
              QMxGate(j) = max (QMxGate(j), 0.0)
           elseif (i .eq. 2) then
              QMxTurb(j) = min (QMxTurb(j), QMax(index))
              QMxTurb(j) = max (QMxTurb(j), 0.0)
           elseif (i .eq. 3) then
              QMxSpil(j) = min (QMxSpil(j), QMax(index))
              QMxSpil(j) = max (QMxSpil(j), 0.0)
           endif
         Enddo
      Enddo

!  Set Maximum flow per outlet link, and actual release
      QDMax = 0.0
      Do ilink = 1, NrOutletLinks(Irsv)
         Do i=1,MaxTypeGates
           Do j=1,MaxSameGates
              if (LinkOutlets(irsv,i,j) .eq. ilink) then
                 if (i .eq. 1)  QDMax(ilink) = QDMax(ilink) + QMxGate(j)
                 if (i .eq. 2)  QDMax(ilink) = QDMax(ilink) + QMxTurb(j)
                 if (i .eq. 3)  QDMax(ilink) = QDMax(ilink) + QMxSpil(j)
              Endif
           Enddo
         Enddo
         ActualRelease(ilink) = Min (DesiredRelease(ilink), QDMax(ilink))
         ActualRelease(ilink) = Max (0.0, ActualRelease(ilink))
      Enddo

      Return
      End Subroutine SetReleaseCapacities


      Subroutine SetSprov (Sinit, ExpInflow, Release, Sprov, TimestepSize, Idebug)
! *********************************************************************
!     Compute provisional End storage Sprov
! *********************************************************************

      Integer          TimestepSize, Idebug
      Double Precision Sinit, ExpInflow, Release(MaxTypeGates*MaxSameGates), SProv
      Double Precision TotalRelease, TotalReduction, Ratio
      Integer          i

!     Set total release
      TotalRelease = 0.
      Do i=1,MaxTypeGates*MaxSameGates
         TotalRelease = TotalRelease + Release(i)
      Enddo

!     Set provisional end storage = initial storage + expected inflow - DesiredRelease
      SProv = SInit + (ExpInflow - TotalRelease) * TimestepSize

!     if provisional end storage negative: reduce TotalRelease accordingly
!     reduce over all gates in same ratio
      if (Sprov .lt. 0) then
         TotalReduction = Sprov / Timestepsize
         Ratio = 1.
         if (TotalRelease .gt. 0) Ratio = (TotalRelease - TotalReduction) / TotalRelease
         if (Idebug .gt. 0) then
             Write(Idebug,*) ' Reduction of releases in SetSprov with factor', ratio
             Write(Idebug,*) ' TotalRelease, TotalReduction ', TotalRelease, TotalReduction
         endif
         Do i=1,MaxTypeGates*MaxSameGates
            Release (i) =  Release(i) * Ratio
         Enddo
      endif

      Return
      End Subroutine SetSprov



      Subroutine LevelToVolume (Level, Volume, irsv)
!  *********************************************************************
!     Given reservoir level, compute reservoir volume
!  *********************************************************************

      Double Precision Level, Volume
      Integer Irsv

      Integer NVal, idum

!  use standard interpolation routine to convert level to volume

      nVal = HavTableLength (Irsv)
      CALL INTERP_Double (NVAL, HAVRSV(1,1,Irsv), HAVRSV(1,3,Irsv), Level, Volume, IDUM)

      Return
      End Subroutine LevelToVolume



      Subroutine VolumeToLevel (Volume, Level, irsv)
!  *********************************************************************
!     Given reservoir volume, compute reservoir level
!  *********************************************************************

      Double Precision Level, Volume
      Integer Irsv

      Integer NVal, idum

!  use standard interpolation routine to convert volume to level

      nVal = HavTableLength (Irsv)
      CALL INTERP_Double (NVAL, HAVRSV(1,3,Irsv), HAVRSV(1,1,Irsv), Volume, Level, IDUM)

      Return
      End Subroutine VolumeToLevel



      Subroutine Rsv_ErrMsg (IeCode, IeCod, Str1, Str2, Iout1)
! *********************************************************************
!    Error messages
! *********************************************************************

      Integer      IOut1, IEcode, IeCod
      Character(*) Str1, Str2

      if (IECod .ne. 0) then
          Write(Iout1,'(A)') ' Error from Ribasim reservoir module'
      else
          Write(Iout1,'(A)') ' Warning from Ribasim reservoir module'
      endif

      Write(Iout1,*)  ' Error code ', IECode
      Write(Iout1,'(A)') Str1
      Write(Iout1,'(A)') Str2

      Return
      End Subroutine Rsv_Errmsg


    End Module ReservoirModule
