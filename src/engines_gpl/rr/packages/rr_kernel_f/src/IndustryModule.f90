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
! at:               $Modtime:: 18-08-97 2:07p   $
!
! current revision: $Revision:: 8               $


module Industry

  use Conf_Fil
  use Conf_Arr
  use NewTables
  use Network
  use NWRW
  use RRConnectionBifurcationNodes
  use Openwater
  use Boundary
  use RWZI
  use DH_Alloc
  use ReadLib

  ! use Messages

  ! variables
   implicit none

  ! Data industrie: demand, discharge, return flow, salt conc. discharge, computation option.
  ! demand and discharge may be in tables

  type Industrydata
    Real RetFlowPerc
    Real FixedSaltConc
    Integer CompOption
! opties voor hoe om te gaan met kortingen
    Integer ReductOpenWaterOption
    Integer ReductReturnFlowOption
    Real    ReductOpenWaterLevel
    Real    ReductBoundaryLevel
  end type Industrydata

  type (Industrydata), allocatable :: Industrydat(:)


  ! *** Results Industry
  ! *** QIndDis= Industry discharge 1 tijdstap
  ! *** QIndDem= Industry demand 1 tijdstap
  ! *** SltIndDis   = zoutconcentratie discharge


  REAL, Pointer, SAVE ::     QIndDis(:), QIndDem(:), SltIndDis(:)
  Integer, Pointer, SAVE ::     IndDemTable(:), IndDisTable(:)
! Industrial allocation, shortage, actual return flow
  REAL, Pointer, SAVE ::     QIndAll(:), QIndShortage(:), QIndReturnFlow(:)
! Industrial allocation, actual return flow of previous iteration
  REAL, Pointer, SAVE ::     QIndAll0(:), QIndReturnFlow0(:)

!Industry Output
  ! *** QDisMx= maximum discharge for each event
  ! *** QDemMx= maximum demand for each event
  ! *** SltIndMx= maximum salt conc.discharge for each event
  ! *** QAllMx= maximum allocation for each event
  ! *** QShortMx= maximum shortage for each event
   REAL, Pointer, SAVE ::   QDisMx(:,:),  QDemMx(:,:), SltIndMx(:,:)
   REAL, Pointer, SAVE ::   QAllMx(:,:),  QShortMx(:,:)


contains

  Subroutine Industry_confAr1

    implicit none

    Integer iOut1, Allocation_Error
    Logical success

    iOut1 = ConfFil_get_iOut1()

    NIndus = MAX (1, NcIndus ) !Industry
    IF ((NcIndus .GT. 0) .and. (ConfFil_get_iOut1() > 0)) then
      WRITE(IOUT1,*) ' Industry nodes        =',NIndus
    end if

   !*** Data industry
    ALLOCATE ( Industrydat(NIndus), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', ' Industry_ConfAr1' )
   ! Initialise
    IndustryDat%RetFlowPerc   = 0.0
    IndustryDat%FixedSaltConc = 0.0
    IndustryDat%ReductOpenWaterOption = 0
    IndustryDat%ReductReturnFlowOption = 0.0
    IndustryDat%ReductOpenWaterLevel = 0.0
    IndustryDat%ReductBoundaryLevel = 0.0

   !*** Results industry
    Success = Dh_AllocInit (NIndus, QIndDis, QIndDem, SltIndDis, 0E0)
    Success = success .and. Dh_AllocInit (NIndus, QIndAll, QIndShortage, QIndReturnFlow, 0E0)
    Success = success .and. Dh_AllocInit (NIndus, QIndAll0, QIndReturnFlow0, 0E0)
    Success = success .and. Dh_AllocInit (NIndus, IndDemTable, IndDisTable, 0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Industry_ConfAr1' )
!    ALLOCATE ( QIndDis(NIndus), QIndDem(NIndus), SltIndDis (NIndus), Stat=Allocation_Error )
!    ALLOCATE ( QIndAll(NIndus), QIndShortage(NIndus), QindReturnFlow(Nindus), Stat=Allocation_Error )
!    ALLOCATE ( QIndAll0(NIndus), QindReturnFlow0(Nindus), Stat=Allocation_Error )
!    ALLOCATE (IndDemTable(NIndus), IndDisTable(NIndus), Stat=Allocation_Error )

  Return
  end subroutine Industry_confAr1


  Subroutine IndustryOutput_Confar (NEvnt)

    implicit none

    Integer Nevnt
    Logical success

 !Industry
    Success = Dh_AllocInit (NIndus, Nevnt, QAllMx, QShortMx, 0E0)
    Success = success .and. Dh_AllocInit (NIndus, Nevnt, QDisMx, QDemMx, SltIndMx, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Industry_OutputConfAr' )
!    ALLOCATE ( QDisMx(NIndus,Nevnt), QDemMx(NIndus,Nevnt), SltIndMx (NIndus,Nevnt), Stat=Allocation_Error)
!    ALLOCATE ( QAllMx(NIndus,Nevnt), QShortMx(NIndus,Nevnt), Stat=Allocation_Error)
  Return
  End subroutine IndustryOutput_Confar





  subroutine Industry_readAsciiInput(infile1,infile2)

    implicit none

    integer :: RetVal

    Integer(4) infile1, infile2
    Integer    teller, index, iNod
    Integer    ncIndus, ncNode

    Integer         iecode, iout1,idebug, TableNr, NrColumns, Iindus
    Character(1000) string
    Logical         allow, found, endfil, TabYesNo, Occurs, Err969
    Integer         NHLP
    Parameter       (NHLP=32)
    Integer         IDUM(NHLP)
    REAL            RDUM(NHLP)
    Character(CharIdLength)   CDUM(NHLP), id, TableName, NodeId
    Character(CharIdLength), Pointer :: DEMDEF(:),DISDEF(:)
    Logical       , Pointer :: AlreadyRead(:)
    Logical success
    Character(Len=CharIdLength)  FileName
    Character(Len=1000000)       KeepBufString
    Integer                      IoUnit, LenString, ipos


    iOut1 = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()

    allow = .false.
    found = .false.
    ncIndus = Network_get_nrIndus()
    ncNode  = Network_get_nrNodes()
!   write(*,*) ' NcIndus =', NcIndus

    Success = Dh_AllocInit (NCIndus, DemDef, DisDef, '')
    Success = success .and. Dh_AllocInit (NIndus, AlreadyRead, .false.)
    if (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Industry_ReadAscii' )
!    ALLOCATE  (DEMDEF(NCIndus),DISDEF(NCIndus), Stat=Allocation_Error )
!    ALLOCATE  (AlreadyRead(NCIndus), Stat=Allocation_Error )

!   Vector/Array initialisation
    DemDef = ''
    DisDef = ''
    AlreadyRead = .false.

!   TableType = 8

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input Industry
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(61)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !wwtp.3b
        Write(*,*) ' Cleaning industry.3b to file:', FileName
        Write(iout1,*) ' Cleaning industry.3b to file:', FileName
   endif
! *********************************************************************
! read file industry.3b
! *********************************************************************
    call SetMessage(LEVEL_DEBUG, 'Read Industry.3b file')
    Endfil = .false.
    teller = 0
    RetVal = 0
    CALL SKPCOM (Infile1, ENDFIL,'ODS')
    do while (.not. endfil)
      READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword (INDU)
      If (STRING(1:4) .eq. 'INDU') Then
! industry node id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Industry_readAscii',' Industry.3b file',IOUT1, &
                     ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       index = 0
       call fndNd2(index, id)
       if (index .gt. 0) then
        inod = index
        index = EiNode(inod,2)
        if (EiNode(inod,3) .eq. 15) then  ! knoop is een industry node
         if (AlreadyRead(index)) then
           call SetMessage(LEVEL_ERROR, 'Data for Industry node '//id(1:Len_trim(id))//' double in datafile Industry.3B')
         else
! cleaning RR files
           If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
           AlreadyRead(index) = .true.
           teller = teller + 1
! demand table id
           RetVal = RetVal + GetVAR2 (STRING,' dm ',1,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DEMDEF (index) = CDUM(1)
! discharge table id
           RetVal = RetVal + GetVAR2 (STRING,' ds ',1,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           DISDEF (index) = CDUM(1)
! return flow percentage
           RetVal = RetVal + GetVAR2 (STRING,' rf ',2,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Industrydat(index)%retflowperc = RDUM(1)
! salt concentration of discharge
           RetVal = RetVal + GetVAR2 (STRING,' sc ',2,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Industrydat(index)%FixedsaltConc = RDUM(1)
! computation option
           RetVal = RetVal + GetVAR2 (STRING,' co ',2,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           Industrydat(index)%CompOption = RDUM(1)
           If (IndustryDat(index)%CompOption .ne. 1) DisDef(Index) = ''
! additions Taiwan Nov 2001, allowing reduction of demand
! first set default values, then read input data if present (missing is allowed!)
           Industrydat(index)%ReductOpenWaterOption = 0
           Industrydat(index)%ReductReturnFlowOption = 0
           Industrydat(index)%ReductOpenWaterLevel = -999.99
           Industrydat(index)%ReductBoundaryLevel = -999.99
           allow = .true.
           RetVal = RetVal + GetVAR2 (STRING,' redow ',3,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) Industrydat(index)%ReductOpenWaterOption = Idum(1)
           RetVal = RetVal + GetVAR2 (STRING,' redlv ',2,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) Industrydat(index)%ReductOpenWaterLevel = Rdum(1)
           RetVal = RetVal + GetVAR2 (STRING,' redbnlv ',2,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) Industrydat(index)%ReductBoundaryLevel = Rdum(1)
           RetVal = RetVal + GetVAR2 (STRING,' redrf ',3,' Industry_readAscii',' Industry.3b file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) Industrydat(index)%ReductReturnFlowOption = Idum(1)
! Reduction based on boundary level added July 2002
! End additions Taiwan Nov 2001
         Endif
        Endif
       Endif
      Endif
      CALL SKPCOM (Infile1, ENDFIL,'ODS')
    Enddo
 21 continue
    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading Industry.3B file', ' Error getting INDU records')
    If (teller .lt. NcIndus)  Then
        Do inod=1,NcNode
          iindus = EiNode(inod,2)
          if (EiNode(inod,3) .eq. 15) then   ! en is open water knoop
            if (.not. AlReadyRead(iindus)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for industry node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
        call ErrMsgStandard (972, 0, ' Not enough data for all industry nodes in schematisation found', &
                             ' Some industry nodes in schematisation not found in Industry.3B file')
    Endif

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for industry.tbl
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(77)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !industry.tbl
        Write(*,*) ' Cleaning industry.tbl to file:', FileName
        Write(iout1,*) ' Cleaning industry.tbl:', FileName
   endif
! *********************************************************************
! read Industry.tbl; industrial demands
! *********************************************************************
    call SetMessage(LEVEL_DEBUG, 'Read Industry.tbl file: Demands')
    if (idebug .ne. 0) write(idebug,*) ' Read Industry.Tbl file'
!   Vector/Array initialisation
!    IndDemTable = 0
!    IndDisTable = 0

    Endfil = .false.
    Call SKPCOM (Infile2, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile2, 'DEMD', Endfil, idebug, Iout1)  ! get record van keyword DEMD tot demd, zet in buffer
       IF (ENDFIL) GOTO 5111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer IndustryModule too small, DEMD record'
           Write(iout1,*) 'local buffer IndustryModule too small, DEMD record'
           GOTO 5111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       If (.not. Success) Goto 5111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt
          IIndus = FindString (NcIndus, Demdef, TableName, NcIndus, CaseSensitive)
          Occurs = (IIndus .gt. 0)
          NrColumns = 1
          if (IIndus .gt. 0) then
             if ( IndDemTable(iIndus) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'Industry Demand table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Industry.Tbl')
               NrColumns = 0     ! om verdere verwerking uit te zetten
             endif
          endif
!         Verwerken tabel definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
             Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
             If (.not. Success) Goto 5111
! clean RR files
             If (CleanRRFiles) then
               ! use KeepBufString to write to file
               ! first till TBLE
               ! then till < characters
               ! then till the end of the buffer string
               lenstring = len_trim(KeepBufString)
               ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
               if (ipos .gt. 0) then
                  write(Iounit,'(A)') KeepBufString (1:ipos+4)
                  KeepBufString(1:) = KeepBufString(ipos+5:)
               else
                  ! error: no TBLE found
                    call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
               endif
 1041          continue
               lenstring = len_trim(KeepBufString)
               ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
               if (ipos .gt. 0) then
                  write(Iounit,'(A)') KeepBufString (1:ipos+2)
                  KeepBufString(1:) = KeepBufString(ipos+3:)
                  goto 1041
               else
                  ! write remaining part
                  write(Iounit,'(A)') KeepBufString (1:lenstring)
               endif
             Endif
! Set references
             Do iIndus=1, NcIndus
                if (StringComp (DemDef(IIndus), TableName, CaseSensitive) )  IndDemTable(iIndus) = TableNr
             Enddo
          Endif
       Endif
       Call SKPCOM (Infile2, ENDFIL,'ODS')
     Enddo
5111 Continue


! *********************************************************************
! read Industry.tbl; industrial discharges
! *********************************************************************
    Rewind(inFile2)
    call SetMessage(LEVEL_DEBUG, 'Read Industry.tbl file: Discharges')
    Endfil = .false.
    Call SKPCOM (Infile2, ENDFIL,'ODS')
    Do while (.not. endfil)
       Success = GetRecord(Infile2, 'DISC', Endfil, idebug, Iout1)     ! get record van keyword DISC tot disc, zet in buffer
       IF (ENDFIL) GOTO 6111
       Success = GetStringFromBuffer (KeepBufString)
       IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer IndustryModule too small, DISC record'
           Write(iOut1,*) 'local buffer IndustryModule too small, DISC record'
           GOTO 6111
       Endif
       Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
       If (.not. Success) Goto 6111
       If (TabYesNo .and. TableName .ne. '') Then
!         Er is een tabel gedefinieerd, met een niet-lege naam
!         Eerst testen of tabel definition wel gebruikt wordt
          IIndus = FindString (NcIndus, Disdef, TableName, NcIndus, CaseSensitive)
          Occurs = (IIndus .gt. 0)
          NrColumns = 2
          if (IIndus .gt. 0) then
             if ( IndDisTable(iIndus) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'Industry Discharge table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Industry.Tbl')
               NrColumns = 0     ! om verdere verwerking uit te zetten
             endif
          endif
!         Verwerken tabel definitie
          if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
            Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
            If (.not. Success) Goto 6111
! clean RR files
            If (CleanRRFiles) then
              ! use KeepBufString to write to file
              ! first till TBLE
              ! then till < characters
              ! then till the end of the buffer string
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+4)
                 KeepBufString(1:) = KeepBufString(ipos+5:)
              else
                 ! error: no TBLE found
                   call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
              endif
 1051         continue
              lenstring = len_trim(KeepBufString)
              ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
              if (ipos .gt. 0) then
                 write(Iounit,'(A)') KeepBufString (1:ipos+2)
                 KeepBufString(1:) = KeepBufString(ipos+3:)
                 goto 1051
              else
                 ! write remaining part
                 write(Iounit,'(A)') KeepBufString (1:lenstring)
              endif
            Endif
! Set references
            Do iIndus=1, NcIndus
               if (StringComp (DisDef(IIndus), TableName, CaseSensitive) )  IndDisTable(iIndus) = TableNr
            Enddo
          Endif
       Endif
       Call SKPCOM (Infile2, ENDFIL,'ODS')
     Enddo
6111 Continue

! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! check if all table references resolved
    Err969 = .false.
    Do iIndus=1,NcIndus
       If (IndDemTable(Iindus) .eq. 0 .and. DemDef(iindus) .ne. '')  then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Industrial demand tables not present in Industry.Tbl file.',DemDef(iIndus))
       Endif
       If (IndDisTable(Iindus) .eq. 0 .and. DisDef(iindus) .ne. '')  then
           Err969 = .true.
           call ErrMsgStandard (969, 0, ' Industrial discharge tables not present in Industry.Tbl file.',DisDef(iIndus))
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough Industry data found',&
                                     ' Some industrial tables not present in Industry.Tbl file')

    DeALLOCATE  (DEMDEF,DISDEF)
    Deallocate (AlreadyRead)

    Return

150 CONTINUE
     call SetMessage(LEVEL_FATAL, 'Read error in Industry ASCII')


  end subroutine Industry_readAsciiInput







  SUBROUTINE CmpInd (ITMSTP, IIndus, INODE)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for Industries
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IIndus = intern industry nr
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor Industry: zie INIT2 voor bepalen demand/discharges
    ! *********************************************************************

    implicit none

    Integer iIndus, iTmStp, iNode, iDebug, ibnd, irwzi, ipluv, iow, iowup, ibndup, nodeup, ikindup, Iconn, IBifur, i
    Real    UpLvl, UpTargetLevel, UpVol, MinReqLvl, MinReqVol, AvailableVolume
    Real    AllocationRatio, LvlDiff
    Real    PeilArray(6), VolumeArray(6)

    iDebug = ConfFil_get_iDebug()

    IF (IDEBUG /= 0)  WRITE(IDEBUG,*) 'CmpInd iIndus=',IIndus

    ! *********************************************************************
    ! *** Industry
    ! *********************************************************************

    IOW = EIOW(INODE)    ! benedenstrooms open water
    IBND = EIBND(INODE)  ! of benedenstrooms een rand
    IPluv = EIPluv(INODE)  ! of benedenstrooms een NWRW node
    IRWZI = EIRWZI(INODE)  ! of benedenstrooms een RWZI

! Industry Demand via upstream open water or (added July 2002) via a boudnary
    NodeUp = UPNODE(INODE)
!   initialisation defaults
    AllocationRatio = 1.0    ! 29 April 2002; initialise to 1 instead of zero
    QindAll(Iindus) = 0.0
    QindShortage(Iindus) = 0.0

    if (QIndDem (Iindus) .gt. 0) then
!      July 2002: add optional demand extraction from boundary
       ikindup = EiNode(nodeup,3)
       If (Ikindup .eq. 4) then
          IowUp  = EiNode (Nodeup,2)
          IBndUp = 0
       ElseIf (Ikindup .eq. 6) then
          IOwUp  = 0
          IBndUp = EiNode (Nodeup,2)
       Endif
       if (idebug .ne. 0) then
           write(idebug,*) ' Nodeup  iowup  qinow(iowup,6) ibndup'
           if (iowup .gt. 0) then
              write(idebug,*) Nodeup,  iowup,  qinow(iowup,6), ibndup
           else
              write(idebug,*) Nodeup,  iowup,  ' qinow(0,6)', ibndup
           endif
       endif
       QIndAll(Iindus)  = QindDem(Iindus)
!      check possible reduction of demand, given upstream open water level or boundary level
       If (IowUp .gt. 0) then
         UpLvl = (1.0 - timesettings%timeWeightFactor) * LVLOW0(IowUp) + timesettings%timeWeightFactor * LVLOW(IowUp)
         UpVol = (1.0 - timesettings%timeWeightFactor) * VolOW0(IowUp) + timesettings%timeWeightFactor * VolOW(IowUp)
         if (idebug .ne. 0) then
            Write(Idebug,*) ' upstream open water level  av.', Uplvl
            Write(Idebug,*) ' upstream open water volume av.', UpVol
            Write(Idebug,*) ' upstream bottom level    ', BottomLevel(IowUp)
         Endif
       Elseif (IbndUp .gt. 0) then
          UpVol = BndPar(IbndUp,5) * BndPar(IbndUp,6)      ! area * depth
       Endif
!      determine upstream open water level, excluding net abstraction from current industry node in previous iteration
       UpVol = UpVol + QIndAll0(IIndus) * TimeSettings%TimestepSize
       If (IowUp .gt. 0) then
           Do i=1,NVal
              PeilArray (i)   = PeilOw(i,iowUp)
              VolumeArray (i) = VOLUOw(i,iowUp)
           Enddo
           UpVol = UpVol - QIndReturnFlow0(IIndus) * TimeSettings%TimestepSize
!          if industry discharges to same open water, also exclude return flow previous iteration
           if (Iow .eq. IowUp) UpVol = UpVol - QIndReturnFlow0(IIndus) * TimeSettings%TimestepSize
           CALL RR_INTERP (NVAL, VolumeArray, PeilArray, UpVol, UpLvl, OwLastInterpIndex(IowUp))
           If (UpLvl .le. Bottomlevel(iowUp) .or. UpVol .le. 0) then
              QIndAll(IIndus) = 0.0
           Else
              If (IndustryDat(Iindus)%ReductOpenWaterOption .eq. 1) then
!                 reduction level specified w.r.t. upstream target level
                  UpTargetLevel = CurrentTargetLevel(IowUp) !Call DttLvl (UpTargetLevel, IowUp)
                  MinReqLvl = UpTargetLevel + IndustryDat(Iindus)%ReductOpenWaterLevel
              Else
                  MinReqLvl = IndustryDat(Iindus)%ReductOpenWaterLevel
              Endif
              CALL RR_INTERP (NVAL, PeilArray, VolumeArray, MinReqLvl, MinReqVol, OwLastInterpIndex(IowUp))
              AvailableVolume = UpVol - MinReqVol
              if (idebug .ne. 0) then
                Write(Idebug,*) ' MinReqLvl MinReqVol', MinReqLvl,  MinReqVol
                Write(Idebug,*) ' Uplvl     UpVol    ', Uplvl, UpVol
              Endif
              QIndAll(IIndus) = Min (AvailableVolume/Timesettings%TimestepSize, QindDem(IIndus))
              QIndAll(IIndus) = Max (0.0, QIndAll(IIndus))
           Endif
           QIndShortage(Iindus)  = max (0.0, QindDem(Iindus) - QindAll(Iindus))
           AllocationRatio = QindAll(IIndus) / QindDem(Iindus)
           QINOW(IOWup,6) = QINOW(IOWup,6) - QIndAll(IIndus)
       Elseif (IBndUp .gt. 0) then
           UpVol = UpVol - QIndReturnFlow0(IIndus) * TimeSettings%TimestepSize
!          if industry discharges to same boundary, also exclude return flow previous iteration
           if (Ibnd .eq. IbndUp) UpVol = UpVol - QIndReturnFlow0(IIndus) * TimeSettings%TimestepSize
           If (UpVol .le. 0) then
              QIndAll(IIndus) = 0.0
           Else
!             reduction level boundary is always specified as absolute level
              MinReqLvl = IndustryDat(Iindus)%ReductBoundaryLevel
              LvlDiff   = BndPar(Ibndup,1) - MinReqLvl
              MinReqVol = max (0.0, (BndPar(IbndUp,6)-LvlDiff) * BndPar(Ibndup,5))
              AvailableVolume = UpVol - MinReqVol
              if (idebug .ne. 0) then
                Write(Idebug,*) ' ActLvl Area depth  ', BndPar(IbndUp,1),BndPar(IbndUp,5), BndPar(ibndUp,6)
                Write(Idebug,*) ' LvlDiff            ', LvlDiff
                Write(Idebug,*) ' MinReqLvl MinReqVol', MinReqLvl,  MinReqVol
                Write(Idebug,*) ' Uplvl     UpVol    ', Uplvl, UpVol
              Endif
              QIndAll(IIndus) = Min (AvailableVolume/Timesettings%TimestepSize, QindDem(IIndus))
              QIndAll(IIndus) = Max (0.0, QIndAll(IIndus))
           Endif
           QIndShortage(Iindus)  = max (0.0, QindDem(Iindus) - QindAll(Iindus))
           AllocationRatio = QindAll(IIndus) / QindDem(Iindus)
           QINBND(IbndUp)%totalIndustry = QINBND(IBndUp)%totalIndustry - QIndAll(IIndus)
           QBND(IBndUp) = QBND(IBndUp) - QIndAll(IIndus)
       Endif
    Endif

    IOW = EIOW(INODE)    ! benedenstrooms open water
    IBND = EIBND(INODE)  ! of benedenstrooms een rand
    IPluv = EIPluv(INODE)  ! of benedenstrooms een NWRW node
    IRWZI = EIRWZI(INODE)  ! of benedenstrooms een RWZI
    IConn = EIConn(INODE)  ! benedenstrooms een RRConnection
    IBifur = EIBifur(INODE)  ! benedenstrooms een RRBifurcation


! Industry Discharge
    QIndReturnFlow(IIndus) = QindDis(IIndus)
!   return also reduced if allocation less then demand?
    If (IndustryDat(Iindus)%ReductReturnFlowOption .eq. 0) QIndReturnFlow(IIndus) = QindDis(IIndus) * AllocationRatio
    if (IOW .gt. 0) then
       QINOW(IOW,6) = QINOW(IOW,6) + QIndReturnFlow(IIndus)
    elseif (IBND .GT. 0) THEN
       QBND(IBND) = QBND(IBND) + QIndReturnFlow(IIndus)
       QINBND(IBND)%totalIndustry = QINBND(IBND)%totalIndustry + QIndReturnFlow(IIndus)
    elseif (IPluv .GT. 0) THEN
       QInPluv(IPluv) = QInPluv(IPluv) + QIndReturnFlow(IIndus)
       QPluv(IPluv)%totalIndustry = QPluv(IPluv)%totalIndustry + QIndReturnFlow(IIndus)
    elseif (IRWZI .GT. 0) THEN
       QRWZI(IRWZI) = QRWZI(IRWZI) + QIndReturnFlow(IIndus)
! downstream also a RR-Connection node is allowed
    elseif (IBifur .GT. 0) THEN
       QBifur(Ibifur) = QBifur(IBifur) + QindReturnFlow(IIndus)
    elseif (IConn .GT. 0) THEN
       Call SetQConn(IConn, QindReturnFlow(IIndus))
    endif


    ! *********************************************************************
    ! *** DEBUG
    ! *********************************************************************

    IF (IDEBUG /= 0) THEN
       WRITE(IDEBUG,*) ' Industry ', NamNod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Demand                     :',QIndDem(IIndus), &
                                                       QIndDem(IIndus) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Allocation prev. iter.     :',QIndAll0(IIndus), &
                                                       QIndAll0(IIndus) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Allocation                 :',QIndAll(IIndus), &
                                                       QIndAll(IIndus) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Shortage                   :',QIndShortage(IIndus), &
                                                       QIndShortage(IIndus) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Discharge in case no short.:',QIndDis(IIndus), &
                                                       QIndDis(IIndus) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Actual Return flow previter:',QIndReturnFlow0(IIndus), &
                                                       QIndReturnFlow0(IIndus) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Actual Return flow         :',QIndReturnFlow(IIndus), &
                                                       QIndReturnFlow(IIndus) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Salt conc. Discharge       :',SltIndDis(IIndus)
       WRITE(IDEBUG,*) ' Connected Ow,Bnd,Rwzi,ipluv:',Iow, Ibnd, Irwzi, ipluv
       WRITE(IDEBUG,*) ' Connected Owup,BndUp       :',IowUp, IBndUp
    ENDIF

    RETURN
  END subroutine CmpInd




  Subroutine Init2Industry (Idebug, IOut1, IHour)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van industry per tijdstap
    ! *********************************************************************

      Implicit none

      Integer Idebug, Iout1, iHour
      Integer INode, ikind, IIndus
      Integer NodeUp, NodeDown

      Integer RowNr, TabelNr
      Logical DateTimeOutsideTable
      Character(Len=40) String
      Character(Len=30) NodeName

      type (Date) currentDate
      type (Time) currentTime


      IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' NCNode NcIndus', NCNode, NcIndus

!   Vector/Array initialisation
!   Default zero
      QIndAll0 = 0.
      QIndReturnFlow0  = 0.
      QIndDem  = 0.
      QIndDis  = 0.
      SltIndDis  = 0.
      String = ' '

      DO Inode=1,NcNode
        ikind = EiNode(Inode,3)
        IF (iDebug .ne. 0) WRITE(IDEBUG,*) ' Inode, ikind ', Inode, Ikind

        if (ikind .eq. 15) then
          IIndus = EiNode(inode,2)
          NodeName = Id_Nod(Inode)

          currentDate%year = ConfArr_get_IYear()
          currentDate%month = ConfArr_get_iMonth()
          currentDate%day = ConfArr_get_iDay()
          currentTime%hour = Ihour
          currentTime%minute = ConfArr_get_iMinute()
          currentTime%second = 0
!         Industrial demand from NewTable
          RowNr = -1
          TabelNr = IndDemTable (iIndus)
          If (TabelNr .gt. 0) then
              QindDem (Iindus) = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                 Idebug, Iout1, DateTimeOutsideTable, .true.)
          Endif
          QIndDem (Iindus) = Max (0.0, QIndDem(Iindus))
          NodeUp = UPNODE(INODE)
          if (QIndDem (Iindus) .gt. 0 .and. NodeUp .le. 0) then
             Write(STRING,'(1X,A,1X,F10.3)')  NodeName(1:Len_trim(NodeName)), QindDem(Iindus)
             call ErrMsgStandard (921, 0, ' Industry node with name and demand ',  STRING)
             QindDem (Iindus) = 0.0
          endif

          currentDate%year = ConfArr_get_IYear()
          currentDate%month = ConfArr_get_iMonth()
          currentDate%day = ConfArr_get_iDay()
          currentTime%hour = Ihour
          currentTime%minute = ConfArr_get_iMinute()
          currentTime%second = 0
!         Industrial discharge from NewTable
          RowNr = -1
          TabelNr = IndDisTable (iIndus)
          If (TabelNr .gt. 0) then
             QindDis (Iindus) = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime,  &
                                            Idebug, Iout1, DateTimeOutsideTable, .true.)
             SltindDis (Iindus) = GetNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime,  &
                                            Idebug, Iout1, DateTimeOutsideTable, .true.)
          Endif
!
          NodeDown = DONODE(INODE)
          if (QIndDis (Iindus) .gt. 0 .and. NodeDown .le. 0) then
             Write(STRING,'(1X,A,F10.3)')  NodeName(1:Len_trim(NodeName)), QindDis(Iindus)
             call ErrMsgStandard (922, 0, ' Industry node with name and discharge ',  STRING)
             QindDis (Iindus) = 0.0
          endif

          if (IndustryDat(Iindus)%CompOption .eq. 2) then
             QindDis(IIndus) = IndustryDat(IIndus)%RetFlowPerc / 100. * QindDem(IIndus)
             SltIndDis (Iindus) = IndustryDat(IIndus)%FixedSaltConc
             if (NodeDown .le. 0 .and. QindDis(Iindus) .gt. 0.) then
                Write(STRING,'(1X,A,F10.3)')  NodeName(1:Len_trim(NodeName)), QindDis(Iindus)
                call ErrMsgStandard (922, 0, ' Industry node with name and return flow ',  STRING)
                QindDis (Iindus) = 0.0
             endif
          endif

          if (idebug .ne. 0) then
             write(idebug,*) ' Industry', Inode, Iindus
             write(idebug,*) ' upstream and downstream', NodeUp, NodeDown
             write(idebug,*) ' demand  discharge  salt', QindDem(Iindus), QindDis(iindus), SltIndDis(IIndus)
          endif

        endif

      ENDDO

  END subroutine Init2Industry


  Subroutine Industry_DeAllocateArrays

    if (Allocated(IndustryDat)) DeAllocate(IndustryDat)

  Return
  End subroutine Industry_DeallocateArrays


end module Industry
