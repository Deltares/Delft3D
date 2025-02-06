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
! ***   Inlezen resultaten uit HIS file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  ITMSTP = tijdstap nummer
! ***  IOUT1  = file unit number of output file with messages
! ***  NAME   = name/indicatie soort variabelen (Sobek/3B/etc)
! ***  IDREAD = id's of locations, read from HIS file
! ***  IDPARA = id's of parameters, read from HIS file
! ***  ID     = id's specified in Locations file, expected in HIS file, but not necessarily in the same order anymore
! ***  ASCLOC = Conversion index location HIS array to LOC array
! ***  RESNOW = results current timestep
! ***  NLOC   = max. number of locations
! ***  NLOCHIS= max. number of locations in HIS file
! ***  NHIS   = max. number of series
! ***  NLOCA  = actual nr. of locations deeired volgens ASCII input file
! ***  IDUM   = tijdstap nr. Als tijdstap=1 dan afleiden volgorde arrays
! *********************************************************************
!


Subroutine InitDioPlt (DataSet)

! *********************************************************************
! *** Initialise Dio Dataset (Create empty dataset)
! *********************************************************************
    use dio_plt_rw

    implicit none
    
    ! arguments
    type(DioPltType), intent(inout) :: DataSet   ! plt dataset to be read or written

    ! body: create empty dataset (no name, var. type unknown)

    DataSet = DioPltCreate("NoName", Dio_PLT_Unknown)
    return
End Subroutine InitDioPlt


Function ReadDioPlt (inDataSet, inName, IDEBUG, ITMSTP, IOUT1, &
                      NLOC, NLOCHIS, NHIS, NLOCA, NAME, IDREAD, IDPARA, &
                      ID, ASCLOC, RESNOW, DoHeader)  result(RetVal)
! *********************************************************************
! *** Read Dio dataset with RR data
! *********************************************************************

    Use ReadLib_rtc
    Use Dh_Alloc
    use Dio_Plt_Rw

    implicit none

    Integer :: RetVal

    LOGICAL      Found
    CHARACTER*6  NAME
    Character*(*)  inName       ! name of in dataset

    INTEGER      NLOC, NLOCHIS, NHIS, IDEBUG, ITMSTP, IOUT1, NLOCA
    INTEGER      NLOCF, NHISP, I, ILOC, ILOC2, IPAR, ILEN, ILEN2
!    CHARACTER*32 ID (NLOC), IDREAD (NLOCHIS)
!    CHARACTER*20 IDPARA (NHIS)
    CHARACTER*(*) :: ID (NLOC), IDREAD (NLOCHIS)
    CHARACTER*(*) :: IDPARA (NHIS)
    Double Precision :: RESNOW (NLOC, NHIS)

    INTEGER      AscLoc (NLOCHIS)

    character(Len=132)  MsgString

! DIO var.s
    Character(Len=DioMaxStreamLen)    :: locInName    ! name of in dataset
    type(DioPltType), intent(inout)   :: inDataSet    ! plt dataset to be read
    character(Len=DioMaxParLen), pointer, dimension(:) :: parNames     ! par. names
    character(Len=DioMaxLocLen), pointer, dimension(:) :: locNames     ! loc. names
    Logical                           :: doHeader                      ! Process Header next time?
    real*4, dimension(:,:), pointer :: readValues
!
    logical                    , pointer, dimension(:) :: FoundLoc     ! found locations yes/no
!
! body

      RetVal = 0

      found = Dh_AllocInit(NLoc, FoundLoc, .false.)

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadDioPlt')
!
! *********************************************************************
! *** read header
! ***   NLOCF = aantal lokaties in His file
! ***   NHISP = aantal parameters in His file
! *********************************************************************
!
    if ( doHeader ) then
        ! Get dataset header and dimensions
        ! check dimensions
        locInName = inName
!        write(*,*) ' ReadDioPlt before DioPltGetDataset Name=',locInName
!        write(idebug,*) ' ReadDioPlt before DioPltGetDataset Name=',locInName
        inDataSet = DioPltGetDataset(locInName)
        NHISP =  DioPltGetNPar(inDataSet)
        NLOCF =  DioPltGetNLoc(inDataSet)
!        write(Idebug,*) ' NHISP, NLOCF = ', NHISP, NLOCF
!        write(Idebug,*) ' NHIS, NLOCHIS ', NHIS, NLOCHIS

        IF (NLOCF .GT. NLOCHIS) THEN
            call write_error_message_rtc (913, 0, NAME,' NLOCaties in HIS file',IOUT1)
            RetVal = 913
        ELSEIF (NHISP .GT. NHIS) THEN
            call write_error_message_rtc (913, 0, NAME,' NPARameters in HIS file',IOUT1)
            RetVal = 913
        ENDIF
        if (RetVal .ne. 0) Return

        ! Get parameter and location names
        parNames => DioPltGetPars(inDataSet)
        locNames => DioPltGetLocs(inDataSet)
        IDPARA(1:NHISP) = parNames(1:NHISP)
        IDREAD(1:NLOCF) = locNames(1:NLOCF)

        IF (IDEBUG .GT. 0) THEN
            WRITE(IDEBUG,*)  NLOCF
            WRITE(IDEBUG,*)  (IDREAD(I)(1:len_trim(IdRead(i))),I=1,NLOCF)
            WRITE(IDEBUG,*)  NHISP
            WRITE(IDEBUG,*)  (IDPARA(I)(1:len_trim(IdPara(i))),I=1,NHISP)
        ENDIF

        ! *********************************************************************
        ! *** Check dimensions
        ! *********************************************************************
        ! Already done above
        ! IF (NHISP .GT. NHIS)  call write_error_message_rtc (913, 0, NAME,' NPARameters in HIS file',IOUT1)

        ! *********************************************************************
        ! *** In First timestep: determine conversion array mapping id's
        ! *** from HIS file to RTC.Loc file
        ! *********************************************************************

        ! WRITE(*,*) ' RdHis itmstp= ', Itmstp
        If (ITmstp .eq. 0) then
            ! Determine AscLoc
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Determine AscLoc'
            Do ILOC=1,NLOCF
              AscLoc(ILoc) = 0
              ILEN  = len_trim(IDREAD(ILOC))
              ILoc2 = 1
              Found = .false.
              Do While (ILoc2 .le. NLocA .and. .not. found)
                ILEN2 = len_trim(ID(ILOC2))
                IF (ILEN .EQ. ILEN2 .AND. IDREAD(ILOC)(1:ILEN) .EQ. ID(ILOC2)(1:ILEN2)) THEN
                   AscLoc(Iloc) = Iloc2
                   FoundLoc(iloc2) = .true.
                   Found = .true.
                Endif
                Iloc2 = Iloc2 + 1
              Enddo
              IF (IDEBUG .GT. 0)  WRITE(IDEBUG,*)  ILoc, IdRead(Iloc)(1:len_trim(IdRead(Iloc))), AscLoc(Iloc)
            Enddo
            Do iloc=1,Nloc
               If (.not. FoundLoc(iloc)) then
                  MsgString = ' Desired measurement location is not available from RR/CF/WQ/EXT-results:' // Id(iloc)(1:len_trim(id(iloc)))
                  call write_error_message_rtc (973, 0, ' ReadDioPlt ', MsgString, IOUT1)
               endif
            Enddo
        Endif
    Else
        NHISP  =  DioPltGetNPar(inDataSet)
        NLOCF  =  DioPltGetNLoc(inDataSet)
    Endif  !!!(doHeader)

! *********************************************************************
! *** read data, store in resnow
! *********************************************************************
!    write(*,*) ' ReadDioPlt before DioPltGet'
    if (.not. DioPltGet(inDataSet, readValues)) then
       if (NLocf * NHisp .gt. 0) then
          write(*,*) 'Could not read values in RdHis'
          MsgString = DioGetLastErrorMsg()
          write(*,*) MsgString
          if (len_trim(MsgString) .gt. 0) call write_error_message_rtc (9131, 0, ' ReadDioPlt ', MsgString(1:len_trim(MsgString)), IOUT1)
          call write_error_message_rtc (9131, 0, ' ReadDioPlt ',' DIO Could not get the values from RR/Flow/3DFlow ',IOUT1)
          RetVal = 913
       endif
    endif
    if (RetVal .ne. 0) Return
!   write(*,*) ' ReadDioPlt after DioPltGet'
!
!   Put the data in RTC arrays
    Do Iloc=1,NLocf
     ILoc2 = AscLoc(Iloc)
     If (Iloc2 .gt. 0) then
       Do Ipar=1,NHisp
          RESNOW(ILOC2, IPAR) = readValues(IPar,Iloc)
       Enddo
     Endif
    Enddo

! If this is a shared mem strea:
! - keep it open (and don't send the header next timen)
! - close it (next step will exactly behave as present one)
    if ( DioPltGetStreamType(inDataSet) .eq. Dio_SharedMem_stream ) then
!        write(*,*) ' ReadDioPlt voor doHeader =false'
        doHeader = .false.
    else
!        write(*,*) ' ReadDioPlt voor destroy'
        call DioPltDestroy (inDataSet)
    endif

! *********************************************************************
! *** Debug
! *********************************************************************

    IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,'(15X,A2,15X,A20)')  'id','parameter1   2   etc'
        DO ILOC=1,NLOCF
            ILoc2 = AscLoc(Iloc)
            ! If (Iloc2 .gt. 0) WRITE(IDEBUG,*)  IDREAD(ILOC2), (RESNOW(ILOC2,IPAR),IPAR=1,NHISP)
            If (Iloc2 .gt. 0) WRITE(IDEBUG,*)  ID(ILOC2)(1:len_trim(Id(Iloc2))), (RESNOW(ILOC2,IPAR),IPAR=1,NHISP)
        Enddo
    Endif

! *********************************************************************
! *** Error
! *********************************************************************
!
  999 Continue
!      write(*,*) ' ReadDioPlt is ready'

      Return
End Function ReadDioPlt



Function ReadDioPltExt (inDataSet, inName, IExtDataSet, &
                        IDEBUG, ITMSTP, IOUT1, RESEXT, DoHeader, &
                        HisDataSet, HisParExt, HisLocExt, &
                        HisDataSetTimes, HisDataSetNParLocTimes, &
                        NExtH, NExtHD, NExt, NExtD, NParE, NTimHis) result(RetVal)

! *********************************************************************
! *** Read Dio dataset with Ext data
! *********************************************************************

    use Dio_Plt_Rw
    use ReadLib_rtc

    implicit none

    Character*(*)  inName       ! name of in dataset

    Integer :: RetVal

    Integer          NExtH, NExtHD, NExt, NExtD, NTimHis
    INTEGER          NParE, IDEBUG, ITMSTP, IOUT1, IExtDataSet
    CHARACTER*(*)    :: HisParExt(NExt+NExtH), HisLocExt(Next+NextH)
    Double Precision :: RESEXT (NExt+NExtH, NParE), HisDataSetTimes(NextH,NTimHis)
    Integer          :: HisDataSet(NextH,3), HisDataSetNParLocTimes(NExtH,3)

    INTEGER          NLOCF, NHISP, NTimes, I, IExt, ILOC, ILOC2, IPAR, ILEN, ILEN2, IFile
    INTEGER          Allocation_error

    CHARACTER*256, pointer, Save :: IDPara (:), IDRead(:)


! DIO var.s
    Character(Len=DioMaxStreamLen)    :: locInName    ! name of in dataset
    type(DioPltType), intent(inout)   :: inDataSet    ! plt dataset to be read
    character(Len=DioMaxParLen), pointer, dimension(:) :: parNames     ! par. names
    character(Len=DioMaxLocLen), pointer, dimension(:) :: locNames     ! loc. names
    double precision, pointer, dimension(:)            :: JulianTimes  ! times
    Logical                           :: doHeader                      ! Process Header next time?
    real*4, dimension(:,:), pointer :: readValues
!
! body

      RetVal = 0

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadDioPltExt')
      IF (IDEBUG .GT. 0) THEN
         WRITE(IDEBUG,*)  ' InName', InName
         WRITE(IDEBUG,*)  ' IextDataSet', IextDataSet
         iext = iextDataSet
         WRITE(IDEBUG,*)  ' HisDataSet', HisDataSet(iext,1), HisDataSet(iext,2), HisDataSet(iext,3)
      !  iext = iextDataSet + NExt
      !  WRITE(IDEBUG,*)  ' Iext', Iext
      !  WRITE(IDEBUG,*)  ' HisDataSet iext+next',HisDataSet(iext,1), HisDataSet(iext,2), HisDataSet(iext,3)
      endif
!
!
! *********************************************************************
! *** read header
! ***   NLOCF = aantal lokaties in His file
! ***   NHISP = aantal parameters in His file
! *********************************************************************
!
    if ( doHeader ) then
        ! Get dataset header and dimensions
        ! check dimensions
        locInName = inName
        inDataSet = DioPltGetDataset(locInName)
        NHISP  =  DioPltGetNPar(inDataSet)
        NLOCF  =  DioPltGetNLoc(inDataSet)
        NTimes =  DioPltGetNTimes(inDataSet)
        If (NTimes .gt. NTimHis) then
           call write_error_message_rtc (913, 0, 'Rdpara', ' NTimHis parameter', IOUT1)
           RetVal = 913
        endif

        Allocate  (IDPara (NHisP), IdRead(NLocF), STAT=Allocation_Error )
        If (Allocation_Error .ne. 0) then
            call write_error_message_rtc (929, Allocation_Error, ' Alloc ExtLoc Arrays', ' ', IOUT1)
            RetVal = 929
        endif
        if (RetVal .ne. 0) Return

        ! Get parameter and location names
        parNames => DioPltGetPars(inDataSet)
        locNames => DioPltGetLocs(inDataSet)
        JulianTimes => DioPltGetTimes(inDataSet)
        IDPARA(1:NHISP) = parNames(1:NHISP)
        IDREAD(1:NLOCF) = locNames(1:NLOCF)
        HisDataSetNParLocTimes(IExtDataSet,1) = NHisp
        HisDataSetNParLocTimes(IExtDataSet,2) = NLocf
        HisDataSetNParLocTimes(IExtDataSet,3) = NTimes
        HisDataSetTimes(IExtDataSet,1:NTimes) = JulianTimes(1:NTimes)

        IF (IDEBUG .GT. 0) THEN
            WRITE(IDEBUG,*)  ' locaties ', NLocF
            WRITE(IDEBUG,*)  (IDREAD(I)(1:len_trim(IdRead(i))),I=1,NLOCF)
            WRITE(IDEBUG,*)  ' Parameters ', NHISP
            WRITE(IDEBUG,*)  (IDPARA(I)(1:len_trim(IdPara(i))),I=1,NHISP)
            WRITE(IDEBUG,*)  ' Tijdstappen ', NTimes
            WRITE(IDEBUG,*)  (HisDataSetTimes(IextDataSet,i),I=1,NTimes)
        ENDIF

        ! *********************************************************************
        ! *** In First timestep: determine conversion array mapping id's
        ! *** from HIS file to RTC.Loc file
        ! *********************************************************************
        ! WRITE(*,*) ' RdHis itmstp= ', Itmstp
        If (ITmstp .eq. 0) then
            ! Determine HisDataSet par en loc index; parameter and location should be from the same file!!
            IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Determine HisLocId coupling '
            Do ILOC=1,NLOCF    ! nr. locations from the HIS file
              ILEN  = len_trim(IDREAD(ILOC))   ! id of location from HIS file
              Do ILoc2=1,NExtHd   ! nr. locations in HEXT locations
                If (HisDataSet(Iloc2,1) .eq. IExtDataSet) then
                  ILEN2 = len_trim(HisLocExt(ILOC2))
                  IF (ILEN .EQ. ILEN2 .AND. IDREAD(ILOC)(1:ILEN) .EQ. HisLocExt(ILOC2)(1:ILEN2)) THEN
                     HisDataSet(Iloc2,3) = Iloc  ! id's of location match
                  Endif
                Endif
              Enddo
            Enddo
            Do IPar=1,NHISP    ! nr. parameters from the HIS file
              ILEN  = len_trim(IDPARA(IPAR))   ! id of parameter from HIS file
              Do ILoc2=1,NExtHd   ! nr. locations in HEXT locations
                if (idebug .gt. 0) then
                  write(idebug,*) ' parid from HIS file ', IdPara(ipar)
                  write(idebug,*) ' ilen                ', ilen
                  write(idebug,*) ' parid wanted        ', HisParExt(iloc2)
                endif
                If (HisDataSet(Iloc2,1) .eq. IExtDataSet .and. HisDataSet(Iloc2,3) .gt. 0) then
                  ILEN2 = len_trim(HisParExt(ILOC2))
                  if (idebug .gt. 0) write(idebug,*) ' ilen2               ', ilen2
                  IF (ILEN .EQ. ILEN2 .AND. IDPara(IPar)(1:ILEN) .EQ. HisParExt(ILOC2)(1:ILEN2)) THEN
                     HisDataSet(Iloc2,2) = Ipar  ! id's of parameter match
                  Endif
                Endif
              Enddo
            Enddo
            IF (IDEBUG .GT. 0)  Then
               WRITE(IDEBUG,*)  ' HEXT locnr    FileNr  ParameterNr LocationNr '
               Do ILoc2=1,NExtHd
                  WRITE(IDEBUG,'(I4,3I7)')  ILoc2, &
                                   HisDataSet(Iloc2,1),HisDataSet(Iloc2,2), HisDataSet(Iloc2,3)
               Enddo
            Endif
        Endif
    Endif  !!!(doHeader)

! *********************************************************************
! *** read data, store in resnow
! *********************************************************************
    if (.not. DioPltGet(inDataSet, readValues)) then
       if (NLocf * NHisp .gt. 0) then
          write(*,*) 'Could not read values in RdHis'
          call write_error_message_rtc (974, 0, ' ReadDioPltExt ',' DIO Could not get the values from RR ',IOUT1)
          RetVal = 974
          Return
       endif
    endif
!
!   Put the data in RTC arrays
    Do IExt=1,NExtHd
       IFile= HisDataSet(IExt,1)
       IPar = HisDataSet(IExt,2)
       ILoc = HisDataSet(IExt,3)
       If (IFile .eq. IExtDataSet .and. IPar .gt. 0 .and. ILoc .gt. 0) then
          RESEXT(NExt+IExt,1) = readValues(IPar,Iloc)
       Endif
    Enddo

! This is always an off-line stream, next time do not read header again
    doHeader = .false.

! *********************************************************************
! *** Debug
! *********************************************************************

    IF (IDEBUG .GT. 0) THEN
        DO IExt=1,NExtHd
           WRITE(IDEBUG,*) ' iext ', iext
           Write(Idebug,*) ' HisDataSet(iext,i) i=1..3 ', (HisDataSet(Iext,i),i=1,3)
           WRITE(IDEBUG,*) ' locationid  ', HisLocExt(IExt)(1:32)
           write(idebug,*) ' parameterid ', HisParExt(IExt)(1:32)
           write(idebug,*) ' value',RESExt(IExt,1), ResExt(Next+Iext,1)
        Enddo
    Endif

! *********************************************************************
! *** Error
! *********************************************************************
!
  999 Continue
!      write(*,*) ' ReadDioPltExt is ready'

      Return
End Function ReadDioPltExt




Function ReadDioPltExtSelection (Ievent, Itmstp, Idebug, Iout1, &
                                   InDataSet, IExtDataSet) result(RetVal)
! *********************************************************************
! *** Read Dio dataset, current timestep, selection of parameters and location
! *********************************************************************

    use Dio_Plt_Rw

    Use ParameterModule
    Use LocationDataModule
    Use DecisionModule
    Use MeasureModule
    Use OtherData
    Use NewTables_rtc
    Use ReadLib_rtc

    implicit none

    Integer :: RetVal

    Integer Ievent, Itmstp, Idebug, Iout1, IextDataSet

    Integer          Iloc, Ipar,  IExt, NHisTim, i, iloc2, ipar2, ifile, NDumPar
    INTEGER          SYEAR, SMO, SDAY, SHOUR, SMIN, SSEC, NTimes
    Integer          IDate1,  ITime1, FirstIndex, SecondIndex, Index1, Index2
    Double precision JulianDate1, JulianDate2, Julian, &
                     JulianDateStart, JulianDateEnd, JulianDateCurrent, &
                     RSSEC
    Real             Ratio, Rdum, Rdum1, Rdum2, rmissingvalue
    INTEGER          Allocation_error, Itim

! DIO variables
    type(DioPltType), intent(inout)   :: inDataSet
    Real, Allocatable, Save           :: Values (:,:,:)
    Integer, Allocatable, Save        :: HisParIndex(:), HisLocIndex(:)
    Integer, Allocatable, Save        :: HisParIndx(:), HisLocIndx(:), HisTimIndx(:)


    RetVal = 0

    IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
  1 FORMAT (' ReadDioPltExtSelection')
!
!   Find Dates
!
    SYEAR = IfYEAR
    SMO   = IfMO
    SDAY  = IfDAY
    SHOUR = IfHOUR
    SMIN  = IfMIN
    SSEC  = IfSEC
    RSSEC = SSEC
    IDate1 = SetDate (SYear, SMo, SDay)
    ITime1 = SetTime (SHour, SMin, SSec)
    JulianDate1 = Julian (IDate1,ITime1)
    Do IPAR=1, NTIMHW
      If (IPAR .GT. 1) Then
         Call NXTSTP (IDEBUG, SYEAR,SMO,SDAY,SHOUR, SMIN,SSEC, RSSEC, IDHR, IDMIN, RDSEC)
      EndIf
    EndDo
    IDate1 = SetDate (SYear, SMo, SDay)
    ITime1 = SetTime (SHour, SMin, SSec)
    JulianDate2 = Julian (IDate1,ITime1)

!   Now find time indices for JulianDate1 and JulianDate2; nog even simple linear search vanaf laatst gevonden index
    NTimes = HisDataSetNParLocTimes(IExtDataSet,3)
    Call FindIndex (FirstTimeDataSet(IextDataSet), NTimes, FirstIndex, JulianDate1, IExtDataSet, 1)
    FirstTimeDataSet(IextDataSet) = FirstIndex
    Call FindIndex (FirstIndex, NTimes, SecondIndex, JulianDate2, IExtDataSet, 2)
    NHisTim = SecondIndex - FirstIndex + 1

!   Start, End and Current Julian date
    JulianDateStart = HisDataSetTimes(IextDataSet,FirstIndex)
    JulianDateEnd   = HisDataSetTimes(IextDataSet,SecondIndex)
    JulianDateCurrent = JulianDate1

!   Find number of actual locations and parameters to get from the dataset
    Allocate  ( HisParIndex(NExtH), HisLocIndex(NExtH),STAT=Allocation_Error )
    If (Allocation_Error .ne. 0) Then
        call write_error_message_rtc (929, Allocation_Error, ' AllocLocationArrays', ' ', IOUT1)
        RetVal = 929
        Return
    Endif
    HisParIndex = 0
    HisLocIndex = 0
    IPar = 0
    Do Iext=1,NExtHd
       If (HisDataSet(IExt,1) .eq. IExtDataSet) then
          IPar = IPar + 1
          HisParIndex(ipar) = HisDataSet(IExt,2)
          HisLocIndex(ipar) = HisDataSet(IExt,3)
       Endif
    Enddo

    NDumPar = Ipar
    Allocate  ( HisParIndx(NDumPar), HisLocIndx(NDumPar), HisTimIndx(NHisTim), STAT=Allocation_Error )
    If (Allocation_Error .ne. 0) Then
        call write_error_message_rtc (929, Allocation_Error, ' AllocLocationArrays', ' ', IOUT1)
        RetVal = 929
        Return
    Endif
    HisParIndx(1:NDumpar) = HisParIndex(1:NDumpar)
    HisLocIndx(1:NDumpar) = HisLocIndex(1:NDumpar)
    HisTimIndx(1) = FirstIndex
    Do I=2,NHisTim
       HisTimIndx(i) = HisTimIndx(i-1) + 1
    Enddo

!   Invert array HisParIndex, HisLocIndx
    Deallocate(HisParIndex,HisLocIndex)
    IPar = HisDataSetNParLocTimes(IExtDataSet,1)
    Allocate  ( HisParIndex(IPar), STAT=Allocation_Error )
    If (Allocation_Error .ne. 0) Then
        call write_error_message_rtc (929, Allocation_Error, ' AllocLocationArrays', ' ', IOUT1)
        RetVal = 929
        Return
    Endif
    ILoc = HisDataSetNParLocTimes(IExtDataSet,2)
    Allocate  ( HisLocIndex(ILoc), STAT=Allocation_Error )
    If (Allocation_Error .ne. 0) Then
        call write_error_message_rtc (929, Allocation_Error, ' AllocLocationArrays', ' ', IOUT1)
        RetVal = 929
        Return
    Endif
    HisParIndex = 0
    HisLocIndex = 0

    Do IExt=1,NExtHD
       If (HisDataSet(IExt,1) .eq. IExtDataSet) then
          Do I=1,NDumPar
             If (HisDataSet(IExt,2) .eq. HisParIndx(i) .and. HisDataSet(Iext,2) .gt. 0) HisParIndex(HisDataSet(Iext,2)) = i
             If (HisDataSet(IExt,3) .eq. HisLocIndx(i) .and. HisDataSet(Iext,2) .gt. 0) HisLocIndex(HisDataSet(Iext,3)) = i
          Enddo
          if (HisDataSet(Iext,2) .eq. 0 .or. HisDataSet(Iext,3) .eq. 0) then
                Write(Iout1,*) ' External location nr', iExt, 'id ', Id_Ext(Next+iext)
                Write(Iout1,*) ' Desired Hisfile-Parameter id=', HisParExt(iext)(1:len_trim(HisParExt(iext)))
                Write(Iout1,*) ' Desired Hisfile- Location id=', HisLocExt(iext)(1:len_trim(HisLocExt(iext)))
             if (HisDataSet(Iext,2) .eq. 0) then
                call write_error_message_rtc (959, Allocation_Error, ' ReadDioPLtExtSelection ',  HisParExt(iext) , IOUT1)
                RetVal = 959
                Return
             endif
             if (HisDataSet(Iext,3) .eq. 0) then
                call write_error_message_rtc (959, Allocation_Error, ' ReadDioPltExtSelection ',  HisLocExt(iext) , IOUT1)
                RetVal = 959
                Return
             endif
          endif
       Endif
    Enddo

    Allocate  (Values(NDumPar,NDumPar,NHisTim), STAT=Allocation_Error )
    If (Allocation_Error .ne. 0) Then
        call write_error_message_rtc (929, Allocation_Error, ' Alloc ExtLoc Arrays', ' ', IOUT1)
        RetVal = 929
        Return
    Endif
    Values = -999.

!   Read data, store is in ResExt
    If (DioPltGetSelectionReals(InDataSet, NDumPar, HisParIndx, NDumPar, HisLocIndx, nHisTim, HisTimIndx, Values)) then
!      Put the data in RTC arrays
       Do IExt=1,NExtHd
          IFile= HisDataSet(IExt,1)
          IPar = HisDataSet(IExt,2)
          ILoc = HisDataSet(IExt,3)
          If (IFile .eq. IExtDataSet .and. IPar .gt. 0 .and. ILoc .gt. 0) then
             IPar2 = HisParIndex(Ipar)
             ILoc2 = HisLocIndex(Iloc)
!            Use last known value (at timestep FirstIndex)
!!!             RESEXT(NExt+IExt,1) = Values (IPar2,Iloc2,1)
             SYEAR = IfYEAR
             SMO   = IfMO
             SDAY  = IfDAY
             SHOUR = IfHOUR
             SMIN  = IfMIN
             SSEC  = IfSEC
             RSSEC = SSEC
             IDate1 = SetDate (SYear, SMo, SDay)
             ITime1 = SetTime (SHour, SMin, SSec)
             JulianDateCurrent = Julian (IDate1,ITime1)
             Do Itim = 1, NtimHw
                If (ITim .GT. 1) Then
                    Call NXTSTP (IDEBUG, SYEAR,SMO,SDAY,SHOUR, SMIN, SSEC, RSSEC, IDHR, IDMIN, RDSEC)
                    IDate1 = SetDate (SYear, SMo, SDay)
                    ITime1 = SetTime (SHour, SMin, SSec)
                    JulianDateCurrent = Julian (IDate1,ITime1)
                EndIf
!   Find indices in array
                Call FindIndex (FirstIndex, NTimes, Index1, JulianDateCurrent, IExtDataSet, 1)
                Index2 = min (Index1+1, NTimes)
!   Interpolate
                Rdum1 = Values (IPar2,Iloc2,  Index1-FirstIndex+1)
                Rdum2 = Values (IPar2,Iloc2,  Index2-FirstIndex+1)
                JulianDateStart = HisDataSetTimes(IextDataSet,Index1)
                JulianDateEnd   = HisDataSetTimes(IextDataSet,Index2)
                Ratio = 0.0
                If (Index2 .gt. Index1) Ratio = (JulianDateCurrent-JulianDateStart) / (JulianDateEnd-JulianDateStart)
                if (JulianDateCurrent .lt. 0 .or. Ratio .gt. 1 .or. Ratio .lt. 0) then
                   if (idebug .gt. 0)  write(idebug,*) ' Error: Julian Date negative or Ratio invalid; put ratio=0'
                   Ratio = 0.0
                endif
                If (Idebug .gt. 0) then
                   write(Idebug,*) ' JulianDateCurrent             ',  JulianDateCurrent
                   write(Idebug,*) ' JulianDateStart, JulianDateEnd',  JulianDateStart, JulianDateEnd
                   write(Idebug,*) ' Rdum1   Rdum2 ', Rdum1, Rdum2
                   write(Idebug,*) ' Ratio ', Ratio
                   write(Idebug,*) ' FirstIndex, Index1, Index2, SecondIndex',FirstIndex,Index1,Index2,SecondIndex
                Endif
                Rdum = (1-Ratio) * Rdum1 + Ratio * Rdum2
! check missing values!! juli 2011
                rmissingvalue = -999.999
                if (Rdum1 .gt. rmissingvalue .and. Rdum2 .gt. rmissingvalue) then
                   ! both real values, use interpolated value
                   RESEXT(NExt+IExt,Itim) = RDum
                elseif (Rdum2 .le. rmissingvalue .and. Ratio .gt. 0.0) then
                   ! new value is missing, use missing value (=new value)
                   RESEXT(NExt+IExt,Itim) = RDum2
                elseif (Rdum1 .le. rmissingvalue .and. Ratio .lt. 1.0) then
                   ! old value is missing, use missing value (=old value)
                   RESEXT(NExt+IExt,Itim) = RDum1
                endif
! end check missing values!! juli 2011
             EndDo
          Endif
       Enddo
    Elseif (IPar .gt. 0) then
       write(*,*) 'Could not read values in ReadPltDioExtSelection'
       call write_error_message_rtc (974, 0, ' ReadDioPltExtSelection ',' DIO Could not get the values from Ext HIS file',IOUT1)
       RetVal = 974
       Return
    Endif
    DeAllocate  (Values)
    Deallocate  (HisParIndex,HisLocIndex, HisTimIndx, HisParIndx, HisLocIndx)

! *********************************************************************
! *** Debug
! *********************************************************************

    IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,'(A)')  'locationid   parameterid   value'
        DO IExt=1,NExtHd
           WRITE(IDEBUG,*)  IExt, (HisDataSet(Iext,i),i=1,3)
           WRITE(IDEBUG,*)  HisLocExt(IExt)(1:32), HisParExt(IExt)(1:32), RESExt(IExt,1)
        Enddo
    Endif

! *********************************************************************
! *** Error
! *********************************************************************
!
  999 Continue

      Return
End Function ReadDioPltExtSelection


    Subroutine FindIndex (StartIndex, EndIndex, ReturnIndex, JulianDate1, IExtDataSet, Imode)

! *********************************************************************
! *** Find timestep index in external HIS file
! ***   mode 1 = lower timestep
! ***   mode 2 = upper timestep
! *********************************************************************

    Use ParameterModule
    Use LocationDataModule

    implicit none
    
    Integer          StartIndex, EndIndex, ReturnIndex, Imode, IExtDataSet
    Double Precision JulianDate1

    Logical Found
    Integer i

    Found = .false.
    i = StartIndex
    Do while (.not. found .and. i .lt. EndIndex)
       i = i+1
       If (HisDataSetTimes(IExtDataSet,i) .gt. JulianDate1) then
          found = .true.
          ReturnIndex = max (i-1,1)
          if (Imode .eq. 2) ReturnIndex = max (i,1)
       Endif
    Enddo
    If (.not. found) ReturnIndex = EndIndex

    Return
    End Subroutine FindIndex







Function WriteDioPlt3B (outDataSet, Itmstp, NameFile, Iout1, Idebug) Result(RetVal)

! *********************************************************************
! *** Write Dio dataset to RR
! *********************************************************************
    use ParameterModule
    use LocationDataModule
    use MeasureModule

    Use ReadLib_rtc
    Use Dh_Alloc

    use dio_plt_rw

    IMPLICIT NONE

    ! arguments

    Integer :: RetVal

    type(DioPltType), intent(inout) :: outDataSet           ! output dataset
    Integer, intent(in)             :: ItmStp               ! current timestep
    Character(*)                    :: NameFile             ! filename
    Integer  iout1, idebug

    ! locals

    Character(Len=DioMaxStreamLen)                      :: outName              ! name of out dataset
    Character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId                ! (HIS) runid
    Character(Len=DioMaxParLen), pointer, dimension(:)  :: parNames             ! variable(s) in dataset
    Character(Len=DioMaxLocLen), pointer, dimension(:)  :: locNames, locDescr   ! locations(s) in dataset
    Logical, save                                       :: doHeaderRR =  .True.   ! Process Header next time?

    Double precision, pointer :: Dio3BResult (:,:)
    Integer iloc, ipar
    Logical success

    RetVal = 0

    outName = NameFile
!    write(*,*) ' WriteDio Outname=',OutName

    IF (outName .NE. ' ' .AND. N3bMs .GT. 0) THEN
        if ( doHeaderRR ) then
            success = Dh_AllocInit(N3BMs, LocNames, ' ')
            success = success .and. Dh_AllocInit(N3BMs, LocDescr, ' ')
            success = success .and. Dh_AllocInit(NParaHis, ParNames, ' ')
            If (.not. success) then
                call write_error_message_rtc (981, 0, ' RTC', ' Error allocating arrays ', Iout1)
                RetVal = 981
                Return
            Endif
            ! header
            runId  = '3B HIS file output 1 timestep'
            LocNames = Id3BML
            LocDescr = Descr3BML
            ParNames = D3bPara
            outDataSet = DioPltDefine(outName, runId, Dio_Plt_Double, parNames, locNames)
            Call DioPltAddDescriptions (outDataSet, dio_plt_locs, locDescr)
            deallocate(LocNames, ParNames, LocDescr)
        endif
        !
        ! Send the data
        !
!       write(*,*) ' WriteDio voor DioPltPut=',OutName
        success = Dh_AllocInit(7, N3BMs, Dio3BResult, 0D0)
!       Allocate  ( Dio3BResult(7,N3BMs), Stat=Allocation_Error )
        If (.not. success) then
            call write_error_message_rtc (981, 0, ' RTC', ' Error allocating arrays ', Iout1)
            RetVal = 981
            Return
        Endif
        Do ipar=1,7
           Do iloc=1,N3BMS
              Dio3BResult(ipar,iloc) = Ms3bSt(iloc,ipar)
           Enddo
        Enddo
        if (Idebug .gt. 0) then
           write(Idebug,*) ' Put 7 parameters and ', N3BMs, ' Locations'
           Do iloc=1,N3BMS
              Write (Idebug,*) ' loc ', iloc, ' values ', (Dio3BResult(ipar,iloc),ipar=1,7)
           Enddo
        Endif
        Call DioPltPut (OutDataSet,Itmstp, Dio3BResult )
        Deallocate (Dio3BResult)

        ! If this is a shared mem strea:
        ! - keep it open (and don't send the header next timen)
        ! - close it (next step will exactly behave as present one)
        !
        if ( DioPltGetStreamType(outDataSet) .eq. Dio_SharedMem_stream ) then
!            write(*,*) ' WriteDio voor doHeaderRR=',OutName
            doHeaderRR = .false.
        else
            call DioPltDestroy (outDataSet)
        endif
    Endif

    Return
End Function WriteDioPlt3B



Function WriteDioPltSbk (outDataSet, Itmstp, NameFile, Iout1, Idebug)  Result(RetVal)

! *********************************************************************
! *** Write Dio dataset to Sobek
! *********************************************************************
    use ParameterModule
    use LocationDataModule
    use MeasureModule

    Use ReadLib_rtc
    Use Dh_Alloc
    use dio_plt_rw

    IMPLICIT NONE

    ! arguments

    Integer :: RetVal
    type(DioPltType), intent(inout) :: outDataSet           ! output dataset
    Integer, intent(in)             :: ItmStp               ! current timestep
    Character(*)                    :: NameFile             ! filename
    Integer                         :: Iout1, Idebug

    ! locals

    Character(Len=DioMaxStreamLen)                      :: outName              ! name of out dataset
    character(len=HisRunIdSize), dimension(HisRunIdDim) :: runId                ! (HIS) runid
    Character(Len=DioMaxParLen), pointer, dimension(:)  :: parNames             ! variable(s) in dataset
    Character(Len=DioMaxLocLen), pointer, dimension(:)  :: locNames, LocDescr   ! locations(s) in dataset
    Logical, save                                       :: doHeaderSbk =  .True.   ! Process Header next time?

    Double precision, pointer :: DioSbkResult (:,:)
    Integer  iloc
    Logical success

    RetVal = 0

    outName = NameFile
!    write(*,*) ' WriteDio Outname=',OutName

    IF (outName .NE. ' ' .AND. NsMsId_SBK .GT. 0) THEN
        if ( doHeaderSbk ) then
            success = Dh_AllocInit(NsMsId_SBK, LocNames, ' ')
            success = Success .and. Dh_AllocInit(NsMsId_SBK, LocDescr, ' ')
            success = Success .and. Dh_AllocInit(1, ParNames, ' ')
            If (.not. success) Then
                call write_error_message_rtc (981, 0, ' RTC', ' Error allocating arrays ', Iout1)
                RetVal = 981
                Return
            Endif
            ! header
            runId  = 'Sobek HIS file output 1 timestep'
            LocNames = MsSbId(1:NsMsId_SBK)
!            LocDescr = MsSbId
            ParNames = SbkPara(1)
            outDataSet = DioPltDefine(outName, runId, Dio_Plt_Double, parNames, locNames)
            Call DioPltAddDescriptions(OutDataSet, dio_plt_locs, LocDescr)
            deallocate(LocNames, ParNames, LocDescr)
        endif
        !
        ! Send the data
        !
!        Allocate  ( DioSbkResult(1,NsMsId_SBK), Stat=Allocation_Error )
        success = Dh_AllocInit(1,NsMsId_SBK, DioSbkResult,0D0)
        If (.not. success) Then
            call write_error_message_rtc (981, 0, ' RTC', ' Error allocating arrays ', Iout1)
            RetVal = 981
            Return
        Endif
        Do iloc=1,NsMsId_SBK
           DioSbkResult(1,iloc) = MsSbSt(iloc)
        Enddo
        if (Idebug .gt. 0) then
           write(Idebug,*) ' Put 1 parameter and ', NsMsId_SBK, ' Locations'
           Do iloc=1,NsMsId_SBK
              Write (Idebug,*) ' loc ', iloc, ' values ', DioSbkResult(1,iloc)
           Enddo
        Endif
        Call DioPltPut (OutDataSet,Itmstp, DioSbkResult )
        Deallocate (DioSbkResult)

        ! If this is a shared mem strea:
        ! - keep it open (and don't send the header next timen)
        ! - close it (next step will exactly behave as present one)
        !
        if ( DioPltGetStreamType(outDataSet) .eq. Dio_SharedMem_stream ) then
!            write(*,*) ' WriteDio voor doHeaderSbk=',OutName
            doHeaderSbk = .false.
        else
            call DioPltDestroy (outDataSet)
        endif
    Endif
!    write(*,*) ' WriteDio is ready=',OutName

    Return
End Function WriteDioPltSbk




Subroutine CloseDioPlt (DataSet)
! *********************************************************************
! *** Close Dio dataset
! *** => destroy it, if it is a sharedmem stream (otherwise it was already destroyed)
! *********************************************************************
    use dio_plt_rw

    implicit none
    
    ! arguments
    type(DioPltType), intent(inout) :: DataSet   ! dataset to be read or written using DIO

    ! body: if shared mem, close now

    if ( DioPltGetStreamType(DataSet) .eq. Dio_SharedMem_stream ) then
        call DioPltDestroy (DataSet)
    endif

    return
End Subroutine CloseDioPlt

