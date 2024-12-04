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



module Boundary

  use Conf_fil
  use Conf_Arr
  use NewTables
  use Network
  use Messages
  use DH_alloc
  use ReadLib
  use m_ec_module
  use globals

  ! variables

  ! *** BNDPAR (, 1) = actual level
  ! *** BNDPAR (, 2) = optie fixed level (default) or variable level
  ! *** BNDPAR (, 3) = variable level column number from file
  ! *** BNDPAR (, 4) = initial level (input data)
  ! *** BNDPAR (, 5) = areaal (m2)
  ! *** BNDPAR (, 6) = diepte (m2)
  ! *** BNDNAM       = geeft bij index IBND de knoop index INODE
  ! *** SBKLVL       = Sobek water level boezem (per tijdstap ingelezen)
  ! *** INSHIS       = 0            variabele peilen volgens ASCII
  ! ***                1            variabele peilen volgens HIS-on line
  ! *** INSBK        = 0            als geen variabele peilen
  !                    1            als variabele peilen via tabel
  !                    2            als variabele peilen via HIS-file (online gegenereerd)
  ! ***                file unit nr als wel variabele peilen
  !  SLTBND: (initiele) zoutconcentratie inlaten op boundaries

  ! uiteenrafelen in de volgende structuur (tesamen met output)
    ! type Boundary
    !     inputdata hier
    !   real actualLevel
    !   logical optionFixedLevel
    !   integer columnVariableLevel
    !   real initialLevel
    !   integer internalNodeNr
    !   real  levelBoezem   ! tbv sobek
    !   logical optionASCII   ! 0 -> ASCII input, 1 -> HIS-input
    !   integer variableLvlFileHandle   ! 0 -> no variable levels
    !     results hierna
    !   real flow
    !   real saltConcentration
    !   real flowAvgDay
    ! end type Boundary

   implicit none

   type(tEcInstance), pointer :: ec => NULL()

   ! needed to communicate with ec
   integer, pointer, dimension(:)                   :: ec_target_items_ids
   character(len=maxNameLen), pointer, dimension(:) :: ec_loc_names
   character(len=maxNameLen), pointer, dimension(:) :: ec_quant_names

   logical, pointer, dimension(:)                   :: ec_item_has_been_set_externally
   integer, pointer, dimension(:,:)                 :: ec_bnd_2_ec_index

   public ec_target_items_ids, ec_loc_names, ec_quant_names, ec_item_has_been_set_externally
   public readBoundaryConditionsInto_ec, getBoundaryValue, closeBoundaryConditionFiles

  INTEGER, Pointer, SAVE :: BNDNAM (:)
! logical om aan te geven of knoop RR-CF connection is
  Logical, Pointer, SAVE :: RRCFConnect (:)

  INTEGER INSBK, INSHIS, typeBoundaryLevel

! OnLineSobekLevelused=True als er minstens 1 RR boundary is met bndpar(.,2)=2 dwz peilen on-line uit Sobek
  Logical OnLineSobekLevelUsed

  REAL, Pointer, SAVE ::  BNDPAR (:,:)
  Integer, Pointer, SAVE ::  SobekHisLoc(:)
  Integer, Pointer, SAVE ::  SobekSaltHisLoc(:)
  REAL, Pointer, SAVE ::  SBKLVL(:)

! BndRefRable geeft referentie naar tabel
  Integer, Pointer, SAVE ::  BndRefTable(:)



  Character(Len=CharIdLength), Pointer :: sobekNodeID(:)
  ! *** Results boundaries
  ! *** QBND   = debiet voor 1 tijdstap
  ! *** CBND   = zoutconcentratie
  ! *** QDYBND = daggemiddelde debiet

  ! *** Type QtoBoundary defines an array QINBND of this type,
  ! with same functionality as array QINOW elsewhere.

  type QtoBoundary
    Real totalPaved
    Real totalUnpaved
    Real totalGreenhouse
    Real totalStructure
    Real totalRwzi
    Real totalIndustry
    Real totalSacramento
  end type QtoBoundary

  type (QtoBoundary), allocatable :: QINBND(:)

  REAL, Pointer, SAVE ::     QBND(:), CBND(:), QDYBND(:), sltBnd(:)

! Boundaries
   ! *** QBNDMX = maximum inflow to boundary for each event
   REAL, Pointer, SAVE ::     QBNDMX(:,:,:)


contains


  Subroutine Boundary_confAr1

    Integer iOut1, Allocation_Error
    Logical Success

    iOut1 = ConfFil_get_iOut1()
    OnLineSobekLevelUsed = .false.

    NBND = MAX (1, NCBOUN ) !boundary
    If ( (NBND .GT. 0) .and. (iOut1 .ne. 0) ) then
      WRITE(IOUT1,*) ' Boundary nodes        =',NBND
    endif

   !*** Data boundaries
    Success = Dh_AllocInit (Nbnd, NVal, BndPar, 0E0)
    Success = success .and. Dh_AllocInit (Nbnd, SobekHisLoc, 0)
    Success = success .and. Dh_AllocInit (Nbnd, SobekSaltHisLoc, 0)
    Success = success .and. Dh_AllocInit (Nbnd, SobekNodeId, ' ')
    Success = success .and. Dh_AllocInit (Nbnd, BndNam, 0)
    Success = success .and. Dh_AllocInit (Nbnd, SltBnd, 0E0)
    Success = success .and. Dh_AllocInit (NNod, RRCFConnect, .false.)
    Success = success .and. Dh_AllocInit (NBnd, BndRefTable, 0)
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' Boundary_ConfAr1')

!   ALLOCATE ( BNDPAR (NBND, NVAL), SobekHisLoc(Nbnd), SobekNodeID(nBnd), Stat=Allocation_Error )
!   ALLOCATE ( BNDNAM (NBND), SLTBND(NBND), Stat=Allocation_Error)
!   ALLOCATE ( RRCFConnect(NNOD), Stat=Allocation_Error)
!   ALLOCATE ( BndRefTable(NBND), Stat=Allocation_Error)

   !*** Results boundaries
    Success = success .and. Dh_AllocInit (NBnd, QBnd, Cbnd, QDyBnd, 0E0)
!    ALLOCATE ( QBND (NBND), CBND(NBND), QDYBND (NBND), Stat=Allocation_Error )
    ALLOCATE ( QINBND (NBND), Stat=Allocation_Error )
    If (Allocation_Error .ne. 0) &
       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
                                           ' Boundary_ConfAr1')
!   Initialisatie
    QINBND%totalPaved      = 0.0
    QINBND%totalUnpaved    = 0.0
    QINBND%totalGreenhouse = 0.0
    QINBND%totalStructure  = 0.0
    QINBND%totalRwzi       = 0.0
    QINBND%totalIndustry   = 0.0
    QINBND%totalSacramento = 0.0

  Return
  End subroutine Boundary_confAr1


  Subroutine BoundaryOutput_confAr (Nevnt)
    Integer Nevnt
	Logical Success

! Boundary output
    Success = Dh_AllocInit (Nbnd, Nevnt, 2, QbndMx, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' Boundary_OutputConfAr')
!   ALLOCATE   ( QBNDMX(NcBoun,NEvnt,2), Stat=Allocation_Error )

  Return
  End subroutine BoundaryOutput_confAr


  SUBROUTINE boundary_CONFAR4
    ! set array size various arrays: NSOBK
    Logical Success

    NcSobk = 0   ! toegevoegd, want nog niet geinitialiseerd !
    NSOBK  = MAX (NCSOBK, NCBOUN)
    ncSobk = nSobk
    Success = Dh_AllocInit (NSobk, SbkLvl, 0E0)
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' Boundary_ConfAr4')
!    ALLOCATE ( SBKLVL (NSOBK), Stat=Allocation_Error )
!    If (Allocation_Error .ne. 0) &
!       call ErrMsgStandard (981, Allocation_Error, ' Error allocating arrays in subroutine ', &
!                                           ' Boundary_ConfAr4')

    RETURN
  END subroutine boundary_confar4

  subroutine Boundary_readAsciiInput(Infile1,Infile2)

    Integer :: RetVal

    Integer(4)      Infile1, Infile2
    Integer         iecode, iout1,idebug
    Character(1000) string
    Integer         Nhlp
    Parameter      (NHLP=32)
    Integer         IDUM(NHLP)
    REAL            RDUM(NHLP)
    Character(CharIdLength)   CDUM(NHLP), TableName, NodeId

    Logical         Allow, Found, Endfil, BndTable, TabYesNo, Occurs
    Logical         Err969
    Integer         teller, index,  iNod
    Integer         ncBoun, ncNode, NrColumns, TableNr  !, FindString
    Real            iniSalt
    integer         bltyp, iboun, lenstring, ipos
    real            blval
    Character(len=CharIdLength), Pointer :: TBLDEF(:)
    Character(len=1000000) KeepBufString   ! buffer 1 million char

    Logical       , Pointer :: AlreadyRead(:)
    Logical Success

    Character(Len=CharIdLength)  FileName
    Integer                      IoUnit

    ncBoun = Network_get_nrBoun()
    ncNode = Network_get_nrNodes()

    Success = Dh_AllocInit (NCBoun, TblDef, ' ')
    Success = success .and. Dh_AllocInit (NCBoun, AlreadyRead, .false.)
    if (.not. success) call ErrMsgStandard (981, 0,' Error allocating arrays in subroutine ', ' Boundary_ReadAscii')
!   ALLOCATE  (TBLDEF(NCBOUN), Stat=Allocation_Error )
!   ALLOCATE  (AlreadyRead(NCBOUN), Stat=Allocation_Error )

!   Vector/Array initialisations
!    Bndpar = 0
!    Sltbnd  = 0.0
!    TblDef  = ''
!    BndRefTable = 0
!    AlreadyRead = .false.

    iOut1 = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()

    BndTable = .false.
    allow = .false.
    found = .false.

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input RR-boundaries
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(56)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !bound3b.3b_cleaned
        Write(*,*) ' Cleaning bound3b.3b to file:', FileName
        Write(iout1,*) ' Cleaning bound3b.3b to file:', FileName
   endif

! *********************************************************************
! read bound3b.3b
! *********************************************************************

   call SetMessage(LEVEL_DEBUG, 'Read Bound3B.3b file')
   Endfil = .false.
   teller = 0
   RetVal = 0
   CALL SKPCOM (Infile1, ENDFIL,'ODS')
   do while (.not. endfil)
     READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword (BOUN)
     IF (STRING(1:4) .eq. 'BOUN') Then
! boundary node id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                     CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       index = 0
       call fndNd2(index, CDUM(1))
       if (index .gt. 0) then     ! knoop bestaat
        inod = index
        index = EiNode(inod,2)
        if (EiNode(inod,3) .eq.  6) then   ! en is een boundary node
         if (alreadyRead(index)) then
           call SetMessage(LEVEL_ERROR, 'Data for Boundary node '//cdum(1)(1:Len_trim(cdum(1)))//' double in datafile Bound3B.3B')
         else
! cleaning RR files
          If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

          AlreadyRead(index) = .true.
          teller = teller + 1
          RetVal = RetVal + GetVAR2 (STRING,' bl ',3,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          bltyp = IDUM(1)
          BndPar(index,2) = bltyp
          BndPar(Index,5) = 1000000000.
          BndPar(Index,6) = +999.         ! default: zet diepte op +999, dus niet gebruikt
          BndNam(Index) = Inod
          id_nod2bndnam(inod) = index
          select case (bltyp)
          case (0) !fixed boundary level
               RetVal = RetVal + GetVAR2 (STRING,' bl 0 ',2,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               blval = RDUM(1)
               BndPar(index,1) = blval
               BndPar(index,4) = blval
               RetVal = RetVal + GetVAR2 (STRING,' is ',2,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               IniSalt = RDUM(1)
               SltBnd(index) = IniSalt
          case (1) !Boundary levels en salt concentrations uit TBL file
               RetVal = RetVal + GetVAR2 (STRING,' bl 1 ',1,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               TblDef(index) = cdum(1)
               BndTable = .true.
               RetVal = RetVal + GetVAR2 (STRING,' is ',2,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               IniSalt = RDUM(1)
               SltBnd(index) = IniSalt
          case (2) !CF online connectie; level uit CF
! Waarom stond Call Getvar en SobekNodeId= uitgecommentarieerd?
               RetVal = RetVal + GetVAR2 (STRING,' bl 2 ',1,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               ! added June 2008: do not read the Sobek id after bl 2, since Modelpaste does this wrong
               ! instead, just take the id from the id field
               RetVal = RetVal + GetVAR2 (STRING,' id ',1,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                     CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               ! end addition June 2008
               SobekNodeID (index) = CDUM(1)
               OnLineSobekLevelUsed = .true.
               RetVal = RetVal + GetVAR2 (STRING,' is ',2,' Boundary_readAscii',' bound3b.3b file',IOUT1, &
                             CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               IniSalt = RDUM(1)
               SltBnd(index) = IniSalt
          end select
         Endif
        Endif
       Endif
     Endif
     CALL SKPCOM (Infile1, ENDFIL,'ODS')
   Enddo
21 continue
   If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading Bound3b.3B file ', ' Error getting BOUN records')
   If (teller .lt. NcBoun)  then
        Do inod=1,NcNode
          iboun = EiNode(inod,2)
          if (EiNode(inod,3) .eq. 6) then   ! en is boundary knoop
            if (.not. AlReadyRead(iboun)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for boundary node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
      call ErrMsgStandard (972, 0, ' Not enough data for all boundaries in schematisation found', &
                           ' Some boundaries from schematisation not present in Bound3B.3B file')
   Endif

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for bound3b.tbl
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(57)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !unpaved.sto_cleaned
        Write(*,*) ' Cleaning bound3b.tbl to file:', FileName
        Write(iout1,*) ' Cleaning bound3b.tbl to file:', FileName
   endif
! *********************************************************************
! read Bound3b.tbl
! *********************************************************************
! de tabellen met boundary levels en salt concentrations
! BN_T records, alleen als BndTable = .true.
     endfil = .not. BndTable
     if (.not. endfil) call SetMessage(LEVEL_DEBUG, 'Read Bound3B.Tbl file')
     Call SKPCOM (Infile2, ENDFIL,'ODS')
     Do while (.not. endfil)
        Success = GetRecord(Infile2, 'BN_T', Endfil, idebug, Iout1)  ! get record van keyword BN_T tot bn_t, zet in buffer
        IF (ENDFIL .or. .not. Success) GOTO 3111
        Success = GetStringFromBuffer (KeepBufString)
        IF (.not. Success .and. CleanRRFiles)   then
           Write(*,*) 'local buffer BoundaryModule to small'
           Write(iout1,*) 'local buffer BoundaryModule to small'
           GOTO 3111
        Endif
        Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
        IF (.not. Success) GOTO 3111
        If (TabYesNo .and. TableName .ne. '') Then
!          Er is een tabel gedefinieerd, met een niet-lege naam
!          Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
           NrColumns = 2
           IBoun = FindString (NcBoun, Tbldef, TableName, NcBoun, CaseSensitive)
           Occurs = (IBoun .gt. 0)
           if (Iboun .gt. 0) then
              if ( BndRefTable(iboun) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'Boundary level Table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Bound3B.Tbl')
                 NrColumns = 0  ! om verdere verwerking te stoppen
              endif
           endif
!          Verwerken boundary definitie
           if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
              Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
              IF (.not. Success) GOTO 3111
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
                      call SetMessage(LEVEL_ERROR, 'Boundary level Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
                 endif
 1041            continue
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
              Do iboun = 1, ncboun
                if (StringComp (TblDef(Iboun), TableName, CaseSensitive) )  BndRefTable(iboun) = TableNr
              Enddo
           Endif
        Endif
        Call SKPCOM (Infile2, ENDFIL,'ODS')
     Enddo
3111 Continue
! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! Check of alle referenties naar tabellen opgelost
! *********************************************************************
    Err969 = .false.
    Do iboun = 1, ncboun
       if (BndRefTable(iboun) .eq. 0 .and. TblDEF (iboun) .ne. '')  then
         Err969 = .true.
         call ErrMsgStandard (969, 0, ' Boundary table not found in .Tbl file.', TblDef(iboun))
       Endif
    Enddo
    If (Err969) call ErrMsgStandard (972, 0, ' Not enough Boundary data found', &
                                     ' Some BN_T Table Definitions not present in Bound3B.Tbl file')

    Deallocate (TblDef)
    Deallocate (AlreadyRead)




! ascbin.bas bevat een check op on-line connectie met CF, OPLETTEN DUS
!               if control.istuurm
!                else
!                      errormessage
!               endif
!               nu volgen er nog meer checks in ascbin!!




     Return

150 CONTINUE

   call SetMessage(LEVEL_FATAL, 'Read error in Boundary Read ASCII')

  Return
  end subroutine Boundary_readAsciiInput





  subroutine CleanBoundaryConditionsbcFile(Infile1)

    Integer :: RetVal

    Integer(4)      Infile1
    Integer         iecode, iout1,idebug
    Character(len=1000) string
    Integer         Nhlp
    Parameter      (NHLP=32)
    Integer         IDUM(NHLP)
    REAL            RDUM(NHLP)
    Character(CharIdLength)   CDUM(NHLP), TableName, NodeId

    Logical         Endfil
    Integer         teller, teller2, index
    Integer         ncBoun, ncNode, NrTables
    integer         iboun, lenstring, ipos

    Character(len=CharIdLength)  Key, id
    Character(len=CharIdLength), Pointer :: Names(:),Quantities(:)
    Character(len=1000000) KeepBufString   ! buffer 1 million char

    Logical       , Pointer :: AlreadyRead(:)
    Logical Success

    Character(Len=CharIdLength)  FileName
    Integer                      IoUnit

    iOut1 = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input RR-boundaries
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(122)
        if (FileName == ' ') FileName = 'BoundaryConditions.bc'
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !BoundaryConditions.bc_cleaned
        Write(*,*) ' Cleaning BoundaryConditions.bc to file:', FileName
        Write(iout1,*) ' Cleaning BoundaryConditions.bc to file:', FileName
   endif

! *********************************************************************
! Scan boundary_conditions bc file, how many sections [Boundary]
! *********************************************************************
    call SetMessage(LEVEL_DEBUG, 'Read BoundaryConditions.bc file')
    Endfil = .false.
    teller = 0
    RetVal = 0
    CALL SKPCOM (Infile1, ENDFIL,'ODS')
    do while (.not. endfil)
       READ(infile1,'(A1000)',END=21,ERR=21,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword ([Boundary]
       IF (STRING(1:10) .eq. '[Boundary]')  teller = teller +1
    Enddo
21  continue

    NrTables = teller

! allocate arrays

    Success = Dh_AllocInit (NrTables, Names, ' ')
    Success = success .and. Dh_AllocInit (NrTables, Quantities, ' ')
    Success = success .and. Dh_AllocInit (NrTables, AlreadyRead, .false.)

! *********************************************************************
! Read bc file, name (location) and quantity and determine if unique
! *********************************************************************

    Rewind(infile1)

    Endfil = .false.
    teller = 0
    CALL SKPCOM (Infile1, ENDFIL,'ODS')

    do while (.not. endfil)
30     continue
       READ(infile1,'(A1000)',END=31,ERR=31,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword ([Boundary]
       IF (STRING(1:10) .eq. '[Boundary]') then
          teller = teller + 1
301    continue
          READ(infile1,'(A1000)',END=31,ERR=31,IOSTAT=IECODE) STRING
          Call ProcessString (String, Key, id)
          if (Key(1:4) == 'name') then
             Names (teller) = id
          elseif (Key(1:8) == 'quantity') then
             Quantities (teller) = id
          elseif (STRING(1:10) .eq. '[Boundary]') then
             Backspace(infile1)
             goto 30
          endif
          goto 301
       Endif
    Enddo
31  continue

    Do teller = 1, NrTables
       Do teller2 = 1, teller-1
          if (Names(teller) .eq. Names(teller2) .and. Quantities(teller) .eq. Quantities(teller2)) AlreadyRead(teller) = .true.
       Enddo
    Enddo

! *********************************************************************
! Read bc file again and write cleaned file
! *********************************************************************

    Rewind(infile1)

    Endfil = .false.
    teller = 0
    CALL SKPCOM (Infile1, ENDFIL,'ODS')

    do while (.not. endfil)
40     continue
       READ(infile1,'(A1000)',END=41,ERR=41,IOSTAT=IECODE) STRING
       if (teller .eq. 0 .and. String(1:10) .ne. '[Boundary]')  then
           ! zorg dat [General] sectie helemaal gekopieerd wordt
           write(Iounit,'(A)') String (1:len_trim(String))
           goto 40
       endif
! Skip regel als hij niet begin met juist keyword ([Boundary]
       IF (STRING(1:10) .eq. '[Boundary]') then
          teller = teller + 1
          if (AlreadyRead(teller) .eq. .false.) write(Iounit,'(A)') String (1:len_trim(String))
401    continue
          READ(infile1,'(A1000)',END=41,ERR=41,IOSTAT=IECODE) STRING
          if (STRING(1:10) .eq. '[Boundary]') then
             Backspace(infile1)
             goto 40
          endif
          if (AlreadyRead(teller) .eq. .false.) write(Iounit,'(A)') String (1:len_trim(String))
          goto 401
       Endif
    Enddo
41  continue

    Call CloseGP(iounit)

return
end subroutine CleanBoundaryConditionsbcFile


subroutine ProcessString (String, Key, id)

    character (len=1000)  String
    character (len=CharIdLength)  Key, id

    character (len=1)  IsTeken
    integer    ipos, lenstring

    IsTeken = '='
    Key = ''
    id = ''

    lenstring = len_trim(String)
    ipos  = FndFrst (IsTeken,String(1:lenstring),.false.)

    if (ipos .gt. 2) then
       Key = String (1:ipos-1)
       id  = String (ipos+1:)
       Call LowerC (Key)
       Call Ltrim (Key)
       Call Ltrim (id)
    endif

return
end subroutine ProcessString


subroutine readBoundaryConditionsInto_ec(boundaryConditionsFile)

   use m_ec_bccollect
   use precision_basics

   implicit none

   character(len=*), intent(in)    :: boundaryConditionsFile  ! file containing the lateral conditions
   logical                         :: success

   integer              :: istat          ! status after function call
   integer              :: i  ! loop counter
   integer              :: total_items    ! total number of items expected in bc-file (boundaries, laterals)

   character(len=40)    :: locationID     ! boundary location identifier (node name)
   character(len=40)    :: quantityID     ! water level or discharge

   integer              :: ncNode         ! #nodes in model
   integer              :: ncboun         ! #RR-boundary nodes in model
   integer              :: bc_count       ! #boundary conditions read from bc file
   integer              :: ec_bc_item     ! boundary condition item id in ec- module

   integer     :: index

   inquire(file=boundaryConditionsFile, exist=success)

   if (.not. success) then
      call ErrMsgStandard(995, 995, 'RR :'//trim(boundaryConditionsFile) // ' not found.', ' ')
      return
   endif

   if (associated(ec)) then
      success = ecInstanceFree(ec)
      ec => null()
      if (.not. success) then
         call ErrMsgStandard(995, 995, 'Reading RR Boundaries: Could not free/deallocate ec-instance', ' ')
      endif
   endif

   if (.not. ecInstanceCreate(ec))then
      call ErrMsgStandard(995, 995, 'Reading RR-Boundaries: Could not create ec-instance', ' ')
   endif

   bc_count = ecCollectBCBlocks(ec, boundaryConditionsFile, istat)

   if (bc_count /= 0 .and. istat /= 0)then ! boundaries/laterals found, but error in reading them
      call ErrMsgStandard(995, 995, 'Reading RR-Boundaries: could not read boundary blocks from file '// trim(boundaryConditionsFile), ' ')
      return
   endif

   istat = 0
   total_items = Network_get_nrBoun()

   if (bc_count < total_items) then
      call ErrMsgStandard(995, 995, 'Reading RR-Boundaries: Not enough boundaries in file '// trim(boundaryConditionsFile), ' ')
   else
!SOFTSUP-152     total_items = bc_count
   endif

   if (islcmp .eq. 0) then   ! only RR-boundary water level
      success = Dh_AllocInit(total_items, ec_target_items_ids, -1)
      success = success .and. Dh_AllocInit(total_items, ec_loc_names, ' ')
      success = success .and. Dh_AllocInit(total_items, ec_quant_names, ' ')
      success = success .and. Dh_AllocInit(total_items, ec_item_has_been_set_externally, .false.)
      success = success .and. Dh_AllocInit(1, total_items, ec_bnd_2_ec_index, -1)
      if (.not. success) then
         call ErrMsgStandard(996, 996, 'Reading RR-Boundaries: Error Allocating Arrays', ' ')
      endif
   else
      total_items = total_items * 2   ! water_level and chloride
      success = Dh_AllocInit(total_items, ec_target_items_ids, -1)
      success = success .and. Dh_AllocInit(total_items, ec_loc_names, ' ')
      success = success .and. Dh_AllocInit(total_items, ec_quant_names, ' ')
      success = success .and. Dh_AllocInit(total_items, ec_item_has_been_set_externally, .false.)
      success = success .and. Dh_AllocInit(1, total_items, ec_bnd_2_ec_index, -1)
      if (.not. success) then
         call ErrMsgStandard(996, 996, 'Reading RR-Boundaries: Error Allocating Arrays', ' ')
      endif
      total_items = total_items / 2  ! back to ncboun
   endif
   bc_count = 0

! initialisation required for BNDNAM
   ncBoun = Network_get_nrBoun()
   ncNode = Network_get_nrNodes()

   do i = 1, ncNode
      If (Einode(i,3) .eq. 6) then
         index = Einode(i,2)
         BndNam(index) = i
         BndPar(index,2) = 0
         BndPar(Index,5) = 1000000000.
         BndPar(Index,6) = +999.         ! default: zet diepte op +999, dus niet gebruikt
         id_nod2bndnam(i) = index
! temporary:  set initial level at boundary (will be updated from EC module or on-line from FM)
         BndPar(index,1) = 0.
         BndPar(index,4) = 0.
! temporary: initial salt (default value)
         SltBnd(index) = 10. ! some default value, was present in bound_3b.3b but not yet in bc file or on-line
      endif
   enddo


   ! check boundaries using EC module; water levels
   do i = 1, total_items
      locationID = Id_Nod(BNDNAM(i))
      quantityID = 'water_level'
      ec_bc_item = ecFindItemByQuantityLocation(ec, locationID, quantityID)
      if (ec_bc_item < 0) then
         call ErrMsgStandard(995, 995, 'Reading RR-Boundaries: '//'Could not find '//trim(locationID)//'.'//trim(quantityID)//' in file '//trim(boundaryConditionsFile), ' ')
      else
         bc_count = bc_count + 1
         ec_target_items_ids(bc_count) = ecCreateTimeInterpolatedItem(ec, ec_bc_item)
         ec_loc_names(bc_count) = locationID
         ec_quant_names(bc_count) = quantityID
         ec_bnd_2_ec_index(1, i) = bc_count
      endif
   enddo
   ! check boundaries using EC module; chloride
   if (Islcmp .ne. 0) then
      do i = 1, total_items
         locationID = Id_Nod(BNDNAM(i))
         quantityID = 'chloride_concentration'
         ec_bc_item = ecFindItemByQuantityLocation(ec, locationID, quantityID)
         if (ec_bc_item < 0) then
            call ErrMsgStandard(995, 995, 'Reading RR-Boundaries: '//'Could not find '//trim(locationID)//'.'//trim(quantityID)//' in file '//trim(boundaryConditionsFile), ' ')
         else
            bc_count = bc_count + 1
            ec_target_items_ids(bc_count) = ecCreateTimeInterpolatedItem(ec, ec_bc_item)
            ec_loc_names(bc_count) = locationID
            ec_quant_names(bc_count) = quantityID
            ec_bnd_2_ec_index(1, total_items+i) = bc_count
         endif
      enddo
   endif

end subroutine readBoundaryConditionsInto_ec

function getBoundaryValue(ec_target_item, timeAsMJD) result(value_from_ec)
   double precision               :: value_from_ec
   integer         , intent(in)   :: ec_target_item
   double precision, intent(in)   :: timeAsMJD
   double precision, dimension(1) :: array_values_from_ec

   value_from_ec = 0.0
   if (.not. ecGetValues(ec, ec_target_item, timeAsMJD, array_values_from_ec) ) then
      ! call SetMessage(LEVEL_FATAL, 'Error ec_target_item value from EC file')
   else
      value_from_ec = array_values_from_ec(1)
   endif

end function getBoundaryValue

subroutine closeBoundaryConditionFiles()
   logical :: success
   if (associated(ec)) then
      success = ecInstanceFree(ec)
      ec => null()
   endif
end subroutine closeBoundaryConditionFiles


  SUBROUTINE CMPBND (ITMSTP, IBND, INODE)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for boundaries
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IBND   = intern boundary nr
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor boundaries
    ! *********************************************************************


    Integer iBnd, iTmStp, iNode, iDebug

    iDebug = ConfFil_get_iDebug()

    IF (IDEBUG .ne. 0)  WRITE(IDEBUG,*) 'CMPBND ibnd=',IBND

    ! *********************************************************************
    ! *** Boundaries
    ! *********************************************************************

    ! *********************************************************************
    ! *** DEBUG
    ! *********************************************************************

    IF (IDEBUG .ne. 0) THEN
       WRITE(IDEBUG,*) ' Boundary ', NamNod(INODE)
       WRITE(idebug,*) ' level on boundary (m)      :',BNDPAR(IBND,1)
       Write(idebug,*) ' Total surface boundary (m2):',BNDPAR(IBND,5)
       Write(idebug,*) ' Depth at boundary (m)      :',BNDPAR(IBND,6)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Total inflow to boundary   :',QBND(IBND), &
                                         QBND(IBND) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Total inflow paved area    :',QINBND(IBND)%totalPaved
       WRITE(IDEBUG,*) ' Total inflow unpaved area  :',QINBND(IBND)%totalUnPaved
       WRITE(IDEBUG,*) ' Total inflow greenhouses   :',QINBND(IBND)%totalGreenhouse
       WRITE(IDEBUG,*) ' Total inflow structures    :',QINBND(IBND)%totalStructure
       WRITE(IDEBUG,*) ' Total inflow RWZI          :',QINBND(IBND)%totalRWZI
       WRITE(IDEBUG,*) ' Total inflow Industry      :',QINBND(IBND)%totalIndustry
       WRITE(IDEBUG,*) ' Total inflow Sacramento    :',QINBND(IBND)%totalSacramento
    ENDIF
!   IDEBUG = 0

    RETURN
  END subroutine CMPBND



  SUBROUTINE RDSBK

  ! *********************************************************************
  ! ***                D E L F T         H Y D R A U L I C S
  ! ***
  ! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
  ! *********************************************************************
  ! *** Program :  DELFT_3B version 1.3.                 Date: March 1995
  ! *********************************************************************
  ! *** Last update: Jan 1996          By : Geert Prinsen
  ! *********************************************************************
  ! *** Brief description:
  ! *** ------------------
  ! ***   Read boezempeil file
  ! *********************************************************************
  ! *** Input/output parameters:
  ! *** ------------------------
  ! ***  IDEBUG = file unit number of debug file
  ! ***  INSBK  = file unit number of input file
  ! ***  IOUT1  = file unit number of output file with messages
  ! *********************************************************************

    use ParallelData, only: JulianTimestep
    use timers

    Integer                    :: teller, teller1, rowNr, iDebug, IOut1, TabelNr
    type (Time)                :: currentTime
    type (Date)                :: currentDate
    logical                    :: DateTimeOutsideTable
    double precision           :: value_from_ec
    double precision           :: current_time
    double Precision, external :: modified_julian_fromJulian

    integer, save :: timerRRRdSobek    = 0

    iDebug = ConfFil_get_iDebug()
    iOut1  = ConfFil_get_iOut1()

!   Write(*,1) NBND, NCBOUN, NCSOBK
    IF (IDEBUG .ne. 0) WRITE (IDEBUG,1) NBND, NCBOUN, NCSOBK
  1 FORMAT (' RDSBK',3I10)

  ! *********************************************************************
  ! *** read water levels in m tov NAP uit tabel
  ! *** als ook zoutberekeningen: dan ook zoutconcentraties
  ! *********************************************************************

  !      CALL SKPCOM (INSBK, ENDFIL, 'ODS ')
  !      STRING = ' Boezempeil file'
  !      IF (ENDFIL) call ErrMsgStandard (911, 0, '  Rdsbk', STRING)


    if (idebug .ne. 0) write(IdebugLunRR,*) ' RDSBK : dll_mode', dll_mode
    if (.not. dll_mode) then

       do teller = 1, ncBoun

         IF (BNDPAR(teller,2) .eq. 2) then
           ! initialiseer op nul, want niets beters bekend.
           !  sbklvl (teller)   = 0.0
   !       write(*,*) ' Boundary ',teller,' init. op nul'
           if (idebug .ne. 0) write(idebug,*) ' Boundary ',teller,' init. op nul'
           bndpar (teller,1) = 0.0
           bndpar (teller,4) = 0.0
           sbklvl (teller)   = 0.0
           sltbnd (teller)   = 0.0
         else
           currentDate%year = 0 ! not necessary
           currentDate%month = ConfArr_get_iMonth()
           currentDate%day = ConfArr_get_iDay()
           currentTime%hour = ConfArr_get_iHour()
           currentTime%minute = ConfArr_get_iMinute()
           currentTime%second = 0

   ! nieuwe methode NewTables; alleen als echt een tabel is opgegeven; anders is BndPar en SltBnd al correct geinitialiseerd
           if (BNDPAR(teller,2) .eq. 1) then
              RowNr = -1
              TabelNr = BndRefTable (teller)
              currentDate%year = ConfArr_get_IYear()  ! wel nodig
              SbkLvl(teller)   = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                             Idebug, iout1, DateTimeOutsideTable, .true. )
              BndPar(teller,1) = SbkLvl(teller)
   ! ARS 11551
              BndPar (Teller,4) = SbkLvl(teller)
   ! end
              SltBnd(teller)   = GetNewValue(TableHandle, TabelNr, 2, RowNr, CurrentDate, CurrentTime, &
                                             Idebug, iout1, DateTimeOutsideTable, .true. )
           else   ! zet SbkLevel volgens BndPar1
             SbkLvl(teller) = BndPar(teller,1)
           endif
           if (idebug .ne. 0) then
              write(idebug,*) ' nieuwe methode', teller, BndPar(teller,1), SltBnd(teller)
           endif
         endif

      enddo

   elseif (dll_mode .and. .not. RunSimultaneous) then
      ! extra check, in dll-mode alleen EC module lezen als vlag RunSimultaneous niet aanstaat (in dat geval wordt nl. in rr_dll_bmi direct ingeprikt)

      call timstrt('RdSobek', timerRRRdSobek)

      ! Data from EC-Module
      current_time = julStart + JulianTimestep * dble(timeSettings%CurrentTimeStep - 1)

      ! water level
      do teller = 1, ncBoun
         value_from_ec = getBoundaryValue(ec_target_items_ids(teller), current_time)

         BndPar(teller,1) = value_from_ec
         if (timeSettings%CurrentTimeStep <= 1) then
            BndPar(teller,4) = BndPar(teller,1)
         endif

         SbkLvl(teller) = BndPar(teller,1)
         SltBnd(teller) = 0.0

      enddo

      ! chloride concentration
      if (IslCmp .ne. 0) then
         do teller = NcBoun+1, ncBoun+ncBoun
            value_from_ec = getBoundaryValue(ec_target_items_ids(teller), current_time)
            teller1 = teller - NcBoun
            SltBnd(teller1) = value_from_ec
         enddo
      endif

      call timstop(timerRRRdSobek)

   else
      if (idebug .ne. 0) write(IdebugLunRR,*) ' RDSBK : Error status, dll_mode and RunSimultaneous (T, T) ', dll_mode, RunSimultaneous
   endif

    IF (IDEBUG .ne. 0) WRITE(IDEBUG,*) ' Boundary-levels in m NAP', (SBKLVL(teller),teller = 1,NCSOBK)
    IF (IDEBUG .ne. 0) WRITE(IDEBUG,*) ' Boundary-concentrations',  (SLTBND(teller),teller = 1,NCSOBK)

    RETURN

  END subroutine rdsbk





  Subroutine Init1Boundary
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van boundary data per tijdstap
    ! *********************************************************************

      Implicit none

      Integer Ibnd

      DO ibnd = 1,NCBOUN
         BNDPAR(ibnd,1) = BNDPAR(ibnd,4)
      ENDDO

!     Vector/Array initialisation
      QBND  = 0

  Return
  END subroutine Init1Boundary


  Subroutine Init2Bound
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van boundary data per tijdstap
    ! *********************************************************************

      Implicit none

!      Integer Ibnd
!
!      DO Ibnd = 1,NCBOUN
!         QINBND(ibnd)%totalPaved      = 0.0
!         QINBND(ibnd)%totalUnpaved    = 0.0
!         QINBND(ibnd)%totalGreenhouse = 0.0
!         QINBND(ibnd)%totalStructure  = 0.0
!         QINBND(ibnd)%totalRwzi       = 0.0
!         QINBND(ibnd)%totalIndustry   = 0.0
!         QINBND(ibnd)%totalSacramento = 0.0
!        QBND (ibnd) = 0.0
!        CBND (ibnd) = 0.0
!      Enddo
!     Vector/Array initialisations
      QBND  = 0.0
      CBND  = 0.0
      QINBND%totalPaved      = 0.0
      QINBND%totalUnpaved    = 0.0
      QINBND%totalGreenhouse = 0.0
      QINBND%totalStructure  = 0.0
      QINBND%totalRwzi       = 0.0
      QINBND%totalIndustry   = 0.0
      QINBND%totalSacramento = 0.0

  Return
  END subroutine Init2Bound



      Subroutine WrInputDataBoundaries (Iout9, Iout7, RnDate, RnTime)
        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Stuk uit sub WrData: uitvoer van Boundaries in *.Out files
        ! *********************************************************************

        Implicit none

        Integer      INODE, IKIND, INR, i
        Integer      IOUT9, IOUT7
        Integer*2    RNDATE(3), RNTIME(4)

! Boundaries
      IF (NCBOUN .GT. 0) THEN
         WRITE(IOUT9,16)
   16    FORMAT (//,' Summary input data boundaries      ',//, &
              ' Node identification   Node    Init.level variable  serie',/,&
              '                       name      (m NAP)   (1=yes)   number',/,64('='))
         DO INODE =1,NCNODE
          IKIND = EiNode(INODE,3)
          INR   = EiNode(INODE,2)
          IF (IKIND .EQ. 6) THEN
            WRITE(IOUT9,26) Id_Nod(INODE), &
                            NamNod(INODE), &
                            BNDPAR(INR,1), INT(BNDPAR(INR,2)),INT(BNDPAR(INR,3))
   26       FORMAT (A20,1X,A12,1X,F8.3,2(4X,I5))
          ENDIF
         ENDDO
      ENDIF

! RR boundaries
      If ( ncboun .gt. 0 .and.  OutputDesired(6) ) then
        WRITE(IOUT7,51) RNDATE(3),RNDATE(2),RNDATE(1),(RNTIME(I),I=1,2), CASENM
   51   FORMAT (' Run datum (DD-MM-YY) and time:',I2,'-',I2,'-',I4,2X,I2,':',I2,//,&
                ' Case name                 ',A20,/)
        WRITE(IOUT7,1011) '[m3/s]'
 1011   FORMAT(//,' Maxima per event',//,&     ! boundary
                  ' Event   Start     Node identification   Node ',10X,'Maximum_flow  ',/,&
                  '  nr  year-mon-day', 23X, 'name ',14X,A6,/,75('='))
      Endif


      Return
      END subroutine WrInputDataBoundaries




  Subroutine Wr1OutBoundary (Iout7, Ievent, Month, INode, Ibnd)
    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Wr1Out: uitvoer van Greenhouse node: maxima per event in OUT file
    ! *********************************************************************

      Implicit none

    ! variables

    Integer      INODE, Ibnd, Iout7, Ievent
    Real         QFlw
    CHARACTER(len=3)  MONTH(12)

       if (.not. associated(QBNDMX)) return  ! If there is nothing, do nothing

       QFLW = QBNDMX(IBND,IEVENT,1)
       WRITE(IOUT7,1012) IEVENT, EventStartDateTime(IEVENT,1),MONTH(EventStartDateTime(IEVENT,2)), &
               EventStartDateTime(IEVENT,3),&
               Id_Nod(INODE), NamNod(INODE),QFLW
 1012  FORMAT (I4,1X,I4,1X,A3,1X,I3,3X,A20,1X,A15,1X,F12.3)

  Return
  END subroutine Wr1OutBoundary


  Subroutine Boundary_DeAllocateArrays

    if (Allocated(QinBnd)) DeAllocate(QinBnd)

  Return
  End subroutine Boundary_DeallocateArrays


end module Boundary
