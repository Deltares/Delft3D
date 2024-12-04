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


module Rwzi

  use Conf_Fil
  use Conf_Arr
  use NewTables
  use Network
  use Openwater
  use Boundary
  use DH_Alloc
  use ReadLib

  ! use Messages

  ! variables
  implicit none

  ! *** Results boundaries
  ! *** QRWZI = inkomend debiet voor 1 tijdstap
  ! *** Qeffluent = uitgaand debiet (=inkomend debiet of =gemeten debiet)

  REAL, Pointer, SAVE ::  QRWZI(:), QEFFLUENT(:)


! RwziRefRable geeft referentie naar tabel
  Integer, Pointer, SAVE ::  RwziRefTable(:)

!RWZI output
  ! *** QRWZIMX = maximum inflow to RWZI for each event
  ! *** QEffRWZIMX = maximum outflow to RWZI for each event
  REAL, Pointer, SAVE ::  QRWZIMX(:,:), QEffRwziMx(:,:)


 contains






  Subroutine RWZI_confAr1

    implicit none

    Integer iOut1
    Logical Success

    iOut1 = ConfFil_get_iOut1()

    NRWZI = MAX (1, NCRWZI ) !aantal RWZI's
    IF ((NCRWZI .GT. 0) .and. (Iout1 .ne. 0))  WRITE(IOUT1,*) ' WWTP or RWZI nodes    =',NRWZI

    !*** Results RWZI QRWZI = influent
    !*** Results RWZI Qeffluent = gemeten effluent
    Success = Dh_AllocInit (NRWZI, QRWZI, 0E0)
    Success = success .and. Dh_AllocInit (NRWZI, QEffluent, 0E0)
    Success = success .and. Dh_AllocInit (NRWZI, RwziRefTable, 0)
    if (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RWZI_ConfAr1'  )
!    ALLOCATE ( QRWZI(NRWZI), Stat=Allocation_Error )
!    ALLOCATE ( QEFFLUENT(NRWZI), Stat=Allocation_Error )
!    ALLOCATE ( RwziRefTable(NRWZI), Stat=Allocation_Error )

  Return
  End subroutine RWZI_confAr1


  Subroutine RWZIOutput_confAr (Nevnt)
    Integer Nevnt
    Logical Success

 !RWZI
    Success = Dh_AllocInit (NRWZI, Nevnt, QRWZIMx, 0E0)
    Success = success .and. Dh_AllocInit (NRWZI, NEvnt, QEffRWZIMx, 0E0)
    if (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RWZI_OutputConfAr1'  )
!    ALLOCATE ( QRWZIMX(NRWZI,NEvnt), Stat=Allocation_Error )
!    ALLOCATE ( QEffRWZIMX(NRWZI,NEvnt), Stat=Allocation_Error )
  Return
  End subroutine RWZIOutput_confAr



  subroutine RWZI_readAscii (Infile1,Infile2)

    implicit none

    Integer :: RetVal

    Integer(4)      Infile1, Infile2
    Integer         iecode, iout1,idebug
    Character(1000) string
    Integer         Nhlp
    Parameter      (NHLP=32)
    Integer         IDUM(NHLP)
    REAL            RDUM(NHLP)
    Character(CharIdLength)   CDUM(NHLP), TableName, NodeId

    Logical         Allow, Found, Endfil, WWTPTable, TabYesNo, Occurs, Err969
    Integer         teller, index, iNod
    Integer         ncRwzi, ncNode, NrColumns, TableNr
    integer         bltyp, irwzi
    Character(CharIdLength), Pointer :: TBLDEF(:)
    Logical      , Pointer :: AlreadyRead(:)
    Logical Success
    Character(Len=CharIdLength)  FileName
    Character(Len=1000000)       KeepBufString
    Integer                      IoUnit, LenString, ipos

    ncRwzi = Network_get_nrRwzi()
    ncNode = Network_get_nrNodes()

    Success = Dh_AllocInit (NcRWZI, TblDef, ' ')
    Success = success .and. Dh_AllocInit (NcRWZI, AlreadyRead, .false. )
    if (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RWZI_ReadAscii'  )
!   ALLOCATE  (TBLDEF(NCRwzi), Stat=Allocation_Error )
!   ALLOCATE  (AlreadyRead(NCRwzi), Stat=Allocation_Error )

! initialisatie
!   TblDef = ''
!   RwziRefTable = 0
!   AlreadyRead  = .false.

    iOut1 = ConfFil_get_iOut1()
    iDebug = ConfFil_get_iDebug()

    WWTPTable = .false.
    allow = .false.
    found = .false.

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(59)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !wwtp.3b_cleaned
        Write(*,*) ' Cleaning wwtp.3b to file:', FileName
        Write(iout1,*) ' Cleaning wwtp.3b to file:', FileName
   endif
! *********************************************************************
! read wwtp.3b
! *********************************************************************
    call SetMessage(LEVEL_INFO, 'Read WWTP.3B file')
    Endfil = .false.
    teller = 0
    RetVal = 0
    CALL SKPCOM (Infile1, ENDFIL,'ODS')
    do while (.not. endfil)
      READ(infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE) STRING
! Skip regel als hij niet begin met juist keyword (WWTP)
      IF (STRING(1:4) .eq. 'WWTP') Then
! WWTP node id
       RetVal = RetVal + GetVAR2 (STRING,' id ',1,' WWTP_readAscii',' bound3b.3b file',IOUT1, &
                     CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
       index = 0
       call fndNd2(index, CDUM(1))
       if (index .gt. 0) then
        inod = index
        index = EINode(inod,2)
        if (EINode(inod,3) .eq. 14) then   ! knoop is een RWZI
         if (alreadyRead(index)) then
           call SetMessage(LEVEL_ERROR, 'Data for WWTP node '//cdum(1)(1:Len_trim(Cdum(1)))//' double in datafile WWTP.3B')
         else
! cleaning RR files
          If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

          AlreadyRead(index) = .true.
          teller = teller + 1
          RetVal = RetVal + GetVAR2 (STRING,' tb ',3,' WWTP_readAscii',' bound3b.3b file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
          bltyp = IDUM(1)
          if (bltyp .eq. 1) then  ! table defined
              RetVal = RetVal + GetVAR2 (STRING,' tb 1 ',1,' WWTP_readAscii',' bound3b.3b file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              TblDef(index) = cdum(1)
              WWTPTable = .true.
            endif
          endif
        Endif
       Endif
      Endif
      CALL SKPCOM (Infile1, ENDFIL,'ODS')
    Enddo
21  continue
    If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading WWTP.3B file ',' Error getting WWTP records')
    If (teller .lt. NcRwzi)  then
        Do inod=1,NcNode
          irwzi = EINode(inod,2)
          if (EINode(inod,3) .eq. 14) then   ! en is WWTP (RWZI) node
            if (.not. AlReadyRead(irwzi)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_trim(NodeId)) // ' is missing'
              call ErrMsgStandard (977, 0, ' Data for WWTP node ',String(1:Len_trim(String)) )
            endif
          endif
        Enddo
       call ErrMsgStandard (972, 0, ' Not enough data for all WWTPs in schematisation found', &
                            ' Some WWTPs from schematisation not present in WWTP.3B file')
    Endif

! cleaning RR files
   If (CleanRRFiles) Call closeGP (Iounit)

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input for wwtp.tbl
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(60)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,2)  !wwtp.tbl_cleaned
        Write(*,*) ' Cleaning wwtp.tbl to file:', FileName
        Write(iout1,*) ' Cleaning wwtp.tbl:', FileName
   endif
! *********************************************************************
! read wwtp.tbl
! *********************************************************************
! de tabellen met wwtp measured data
! MEAS records, alleen als WwtpTable = .true.
     call SetMessage(LEVEL_INFO, 'Read WWTP.Tbl file')
     endfil = .not. WwtpTable
     Call SKPCOM (Infile2, ENDFIL,'ODS')
     Do while (.not. endfil)
        Success = GetRecord(Infile2, 'MEAS', Endfil, idebug, Iout1)  ! get record van keyword MEAS tot meas, zet in buffer
        IF (.not. success) GOTO 3111
        IF (ENDFIL) GOTO 3111
        Success = GetStringFromBuffer (KeepBufString)
        IF (.not. Success .and. CleanRRFiles)   then
            Write(*,*) 'local buffer RWZIModule too small'
            Write(iout1,*) 'local buffer RWZIModule too small'
            GOTO 3111
        Endif
        Success = GetTableName (TabYesNo, TableName, ' id ', Iout1) ! get table name via keyword ' id ', TabYesNo=TBLE found
        IF (.not. success) GOTO 3111
        If (TabYesNo .and. TableName .ne. '') Then
!          Er is een tabel gedefinieerd, met een niet-lege naam
!          Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
           NrColumns = 1
           IRwzi = FindString (Ncrwzi, Tbldef, TableName, NcRwzi, CaseSensitive)
           Occurs = (Irwzi .gt. 0)
           if (Irwzi .gt. 0) then
              if ( RwziRefTable(irwzi) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'WWTP table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile WWTP.Tbl')
                 NrColumns = 0     ! om verdere verwerking uit te zetten
              endif
           endif
!          Verwerk definitie
           if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
              Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
              IF (.not. success) GOTO 3111
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
 1041           continue
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
              Do irwzi = 1, ncrwzi
                if (StringComp(TblDef(Irwzi), TableName, CaseSensitive) )  RwziRefTable(irwzi) = TableNr
              Enddo
           Endif
        Endif
        Call SKPCOM (Infile2, ENDFIL,'ODS')
     Enddo
3111 Continue

! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

! Check of alle referenties naar tabellen opgelost
    Err969 = .false.
    Do irwzi = 1, ncrwzi
       if (RwziRefTable(irwzi) .eq. 0 .and. TblDEF (irwzi) .ne. '')  Then
          Err969 = .true.
          call ErrMsgStandard (969, 0, ' WWTP Table Definitions not present in WWTP.Tbl file. ',TblDef(IRwzi))
       Endif
    Enddo
    if (Err969) call ErrMsgStandard (972, 0, ' Not enough WWTP data found',  &
                                     ' Some WWTP Table Definitions not present in WWTP.Tbl file')

    Deallocate (TblDef)
    Deallocate (AlreadyRead)

    Return

150 CONTINUE
    call SetMessage(LEVEL_FATAL, 'Read error in WWTP Read ASCII')

    Return
  end subroutine RWZI_readAscii





  SUBROUTINE CmpRWZI (Itmstp, IRWZI, Inode)
    ! *********************************************************************
    ! *** Last update:  March 1995
    ! *********************************************************************
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for RWZI's
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IBND   = intern WWTP nr
    ! ***  INODE  = knoopnr
    ! *********************************************************************
    ! *** Berekeningen voor RWZI'
    ! *********************************************************************

    implicit none

    Integer       iRWZI, iTmStp, iNode, iDebug, Iout1, Iow, IBnd
    Character(Len=CharIdLength) String
    Real          MeasuredValue

    Integer     rowNr, TabelNr
    type (Time) currentTime
    type (Date) currentDate
    logical     DateTimeOutsideTable



    iDebug = ConfFil_get_iDebug()
    iOut1  = ConfFil_get_iOut1()

    IF (IDEBUG /= 0)  WRITE(IDEBUG,*) 'CmpRWZI irwzi=',IRWZI

    ! *********************************************************************
    ! *** RWZI's
    ! *********************************************************************


    IOW = EIOW(INODE)
    IBND = EIBND(INODE)

! bepaal effluent volgens RR of volgens meetwaarde
    QEffluent(irwzi) = Qrwzi(irwzi)

    if (RwziRefTable(irwzi) .ne. 0) then
!      bepaal meetwaarde uit tabel; als missing value (-999) dan gewoon de RR-waarde gebruiken
        currentDate%year = ConfArr_get_IYear()  ! wel nodig
        currentDate%month = ConfArr_get_iMonth()
        currentDate%day = ConfArr_get_iDay()
        currentTime%hour = ConfArr_get_iHour()
        currentTime%minute = ConfArr_get_iMinute()
        currentTime%second = 0
        RowNr = -1
        TabelNr = RwziRefTable (irwzi)
        MeasuredValue = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                                    Idebug, iout1, DateTimeOutsideTable, .true.)
        if (MeasuredValue .gt. -999.) QEffluent(irwzi) = MeasuredValue
    endif

! Loos effluent op open water of rand
    if (IOW .gt. 0) then
       QINOW(IOW,5) = QINOW(IOW,5) + Qeffluent(IRWZI)
    elseif (IBND .GT. 0) THEN
       QBND(IBND) = QBND(IBND) + Qeffluent(IRWZI)
       QINBND(IBND)%totalRWZI = QINBND(IBND)%totalRWZI + Qeffluent(IRWZI)
    else
       String = Id_Nod(Inode)
       call ErrMsgStandard (976, 0, ' CmpRwzi ', STRING)
    endif




    ! *********************************************************************
    ! *** DEBUG
    ! *********************************************************************

    IF (IDEBUG /= 0) THEN
       WRITE(IDEBUG,*) ' RWZI ', NamNod(INODE)
       WRITE(IDEBUG,*) ' Timestep nr                :',ITMSTP
       WRITE(IDEBUG,*) ' Timestepsize in s          :',timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Total inflow to WWTP       :',QRWZI(IRWZI), &
                                                       QRWZI(IRWZI) * timeSettings%timestepSize
       WRITE(IDEBUG,*) ' Total outflow of WWTP      :',QEffluent(IRWZI), &
                                                       Qeffluent(IRWZI) * timeSettings%timestepSize
    ENDIF

    RETURN
  END subroutine CmpRWZI




  Subroutine Init2RWZI

    ! *********************************************************************
    ! *** Last update:
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Stuk uit sub Init2: initialisie van RWZI data per tijdstap
    ! *********************************************************************

      Implicit none

!      Integer Irwzi
!
!      DO IRwzi = 1,NCRwzi
!         QRWZI (Irwzi) = 0.
!         QEffluent(Irwzi) = 0.
!      ENDDO
!   Vectorised initialisation
      QRWZI  = 0.
      QEffluent = 0.

  END subroutine Init2Rwzi

  !> If success, function returns Values array of length ElementCount
  !! for waste water treatment plant (WWTP) elementset on specific quantity handle
  function RR_GetWWTPDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in WWTP elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in WWTP elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiFlowIn)
    !RR WWTP Inflow in m3/s
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRWZI > 0) then
                Values(1:NRWZI) = RSLMAP14_RWZI(1, 1:NRWZI, 1)
            else
                success = .false.
            endif
    case (RRiFlow)
    !RR WWTP Outflow in m3/s
            if (NRWZI > 0) then
                Values(1:NRWZI) = RSLMAP14_RWZI(2, 1:NRWZI, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

  end function

end module RWZI
