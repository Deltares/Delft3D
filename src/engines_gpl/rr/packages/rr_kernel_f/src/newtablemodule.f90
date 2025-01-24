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
!
! originally developed GPrinsen September 2002
! integration of RR and RTC version of NewTableModule
!   - unification of NewTableConfAr
!   - messages without using Messagemodule
!   - try speed up cyclic tables
!   - Functions SetDate and SetTime added, to prevent having 10000*year + 100*month + day everywhere in the code.
!
! July 2004
!   - mechanism for multiple SobekTimeTableStore

module NewTables

use ParseToken
use Dh_Alloc
use MessageHandling

implicit none
!*** type declarations

  type TTime
    Real hour
    Real minute
    Real second
  end type TTime

  type TDate
    Real year
    Real month
    Real day
  end type TDate

  type Time
    Integer hour
    Integer minute
    Integer second
  end type Time

  type Date
    Integer year
    Integer month
    Integer day
  end type Date

  type SobekTimeTableStore
      integer ModelHandle
      ! Estimated dimensions
      integer MaxData
      integer MaxTabNr
      integer ActMaxData
      integer ActMaxTabNr
      ! First free positions
      integer FirstFreeTable
      integer FirstFreeDateTime
      integer FirstFreeData
      ! Table data
      integer, dimension(:,:), pointer ::    Ntab
      Integer, dimension(:,:), pointer ::    TableDateTime
      real, dimension(:), pointer ::         TableData
      logical oldmethod
  end type SobekTimeTableStore


  interface GetRecord
     module procedure GetRecordKeyUppAnywhere
  end interface


! constants
! define constants for array indices
! indices in TableDateTime
  integer, parameter, private :: IndexDay = 1
  integer, parameter, private :: IndexMonth = 2
  integer, parameter, private :: IndexYear = 3
  integer, parameter, private :: IndexHour = 4
  integer, parameter, private :: IndexMinute = 5
  integer, parameter, private :: IndexSecond = 6
! indices in NTab
  integer, parameter, private :: IndexNTabElements  = 1
  integer, parameter, private :: IndexFirstDateTime  = 2
  integer, parameter, private :: IndexFirstDataValue = 3
  integer, parameter, private :: IndexInterpolationCode = 4
  integer, parameter, private :: IndexPeriodic = 5
  integer, parameter, private :: IndexLastRowUsed = 6

  logical, parameter, private :: FixARS14800 =  .true. ! .false.
! constants
  integer, parameter, private :: MaxKeyLength = 4
  integer, parameter, private :: MaxKeyLengthPlus1 = 5



!variables

      integer            lutemp, rerror, bufmax, ibuf, nbuf, colmax, last_position

! Buffer size has dramatic impact on performance!! to be checked with Stef
      parameter         (bufmax=10000000, colmax=10)    ! bufmax = 10 million characters
!     parameter         (bufmax=1000000 , colmax=10)    ! bufmax =  1 million characters   !! Taiwan oct 2007, NewFormat*
      logical            eof
      character(Len=bufmax) buffer
      character(Len=MaxKeyLength)   keyact
      character(len=40) column_names(colmax), column_ids(colmax)

  type T_SobekTimeTableStore
     type (SobekTimeTableStore), pointer, dimension(:) :: SbkTables
     integer  :: numModels
     integer  :: initialized = -999
  end type T_SobekTimeTableStore

  type (T_SobekTimeTableStore), private, save ::  SobekTimeTables

!
! PRIVATE FUNCTIONS
!
integer, parameter, private  :: MaxNumModels = 5
integer, parameter, private  :: NewTablesInitializedValue = 9876
integer, parameter, private  :: NewTables_Undefined       = -999

integer, private :: currentModelHandle = 0

!     integer, pointer, save ::    Ntab (:,:)
!     Integer, pointer, save ::    TableDateTime (:,:)
!     real, pointer, save ::       TableData (:)
      real, pointer, save ::       RLocal(:,:)


!       Ntab (tabelnr, 1) = aantal elementen in de tabel (lengte)
!            (tabelnr, 2) = eerste element in Array TableDateTime
!            (tabelnr, 3) = eerste element in array TableData
!            (tabelnr, 4) = Interpolation Code   1=geen (=blok), 0=lineair (continuous)
!            (tabelnr, 5) = Period               periodiek met periode xx seconden
!            (tabelnr, 6) = Last rownr used for table
!       TableDateTime(Idata,1) = dag
!                    (Idata,2) = maand
!                    (Idata,3) = jaar
!                    (Idata,4) = uur
!                    (Idata,5) = minuut
!                    (Idata,6) = seconde
!       TableData    (Idata) = data value
!
!       Rlocal        real local array dat allocatable moet zijn


private NewTablesModelFind_ByHandle
private NewTablesModelAdd
private NewTablesCleanup
private GetRecordGeneral
private SetRowNr
private SetCycRowNr
private GenerateMessage
private Uppcas
private Lowcas
private NCalcDate
private NCalcTime
private LeapYear


contains

! Create / Destroy SobekTimeTableDataStore

function NewTablesInitialized() result(retVal)

    implicit none

    logical :: retVal

    retVal = .false.
    if ( SobekTimeTables % initialized == NewTablesInitializedValue ) retVal = .true.

end function NewTablesInitialized


subroutine NewTablesCreate

   implicit none

    if ( NewTablesInitialized() ) return

    allocate(SobekTimeTables % SbkTables(MaxNumModels) )

    SobekTimeTables % SbkTables(:) % modelHandle = NewTables_UNDEFINED
    SobekTimeTables % numModels   = 0
    SobekTimeTables % initialized = NewTablesInitializedValue

end subroutine NewTablesCreate


subroutine NewTablesDestroy

    implicit none

    integer :: i

    if ( .not. NewTablesInitialized() ) return

    Do i=1, SobekTimeTables % NumModels
       Call NewTablesCleanup (SobekTimeTables%SbkTables(i))
    Enddo
    deallocate(SobekTimeTables % SbkTables)

    SobekTimeTables % numModels    = 0
    SobekTimeTables % initialized  = NewTables_Undefined

end subroutine NewTablesDestroy


subroutine NewTablesCleanup (SobekTables)

    implicit none

    type (SobekTimeTableStore) SobekTables

    if ( .not. NewTablesInitialized() ) return

    SobekTables % ModelHandle  = NewTables_Undefined
    SobekTables % MaxData      = 0
    SobekTables % MaxTabnr     = 0
    SobekTables % ActMaxData   = 0
    SobekTables % ActMaxTabnr  = 0
    SobekTables%FirstFreeTable    = 0
    SobekTables%FirstFreeDateTime = 0
    SobekTables%FirstFreeData     = 0
    deallocate(SobekTables%NTab)
    deallocate(SobekTables % TableDateTime)
    deallocate(SobekTables % TableData)

end subroutine NewTablesCleanup


!-----------------------------------------------------------------
! CREATE / DESTROY / GET HANDLE TO MODEL-(=COMP./SCHEM.COMBI
!-----------------------------------------------------------------


function NewTablesModelFindOrCreate (inputHandle)  result(modelHandle)

    implicit none

    integer                      :: inputHandle, modelHandle

    type(SobekTimeTableStore), pointer     :: model

    ! body
    ! provide valid handle by default
    modelHandle = 1

    if ( .not. NewTablesInitialized() ) return

    ! find or create model handle
    modelHandle = NewTables_UNDEFINED
    nullify(model)
    model => NewTablesModelFind_ByHandle (inputHandle)

    if ( associated(model) ) then
        modelHandle = model % modelHandle
    else
        currentModelHandle = currentModelHandle + 1
        model => NewTablesModelAdd(currentModelHandle)
    endif

    if ( associated(model) ) modelHandle = model % modelHandle

end function NewTablesModelFindOrCreate


!
! Find / add models
!

function NewTablesModelAdd(modelHandle) result(model)

    ! return value
    type (SobekTimeTableStore) , pointer    :: model

    ! arguments
    integer         , intent(in) :: modelHandle

    nullify(model) ; if ( .not. NewTablesInitialized() ) return

    if ( SobekTimeTables % numModels >= MaxNumModels ) then
        call SetMessage(LEVEL_FATAL, 'modelCreate: Maximum number of models exceeded')
    else
        SobekTimeTables % numModels = SobekTimeTables % numModels + 1
        model => SobekTimeTables % SbkTables (SobekTimeTables%numModels)
        model % modelHandle = modelHandle
        model % MaxData     = 0
        model % MaxTabNr    = 0
        model % ActMaxData  = 0
        model % ActMaxTabNr = 0
        model%FirstFreeTable    = 0
        model%FirstFreeDateTime = 0
        model%FirstFreeData     = 0

    endif

end function NewTablesModelAdd


function NewTablesModelFind_ByHandle(modelHandle)  result(model)

    ! return value
    type(SobekTimeTableStore) , pointer  :: model

    ! arguments
    integer, intent(in)        :: modelHandle

    ! locals
    integer :: m

    ! body

    nullify(model) ; if ( .not. NewTablesInitialized() ) return

    do m = 1 , SobekTimeTables % numModels
        if ( SobekTimeTables % SbkTables(m) % modelHandle /= NewTables_UNDEFINED) then
            if ( SobekTimeTables% SbkTables(m) % modelHandle ==  modelHandle ) then
                model => SobekTimeTables % SbkTables(m)
                exit
            endif
        endif
    enddo

end function newTablesModelFind_ByHandle




      Logical Function NewTableConfar (ModelHandle, SetMaxTabNr, SetMaxDataNr)

! Dimension tables

         type (SobekTimeTableStore) , pointer    :: model

         Integer, intent(in)   :: modelHandle, SetMaxTabNr, SetMaxDataNr
         Integer Allocation_Error
         Logical success

         model => NewTablesModelFind_ByHandle (modelHandle)

         model % MaxTabNr = SetMaxTabNr
         model % MaxData  = SetMaxDataNr

         Allocation_Error = 999

         success = Dh_AllocInit (SetMaxTabnr, 6, model%NTab, 0)
!         Allocate     ( Ntab(MaxTabnr, 6), Stat=Allocation_Error )

         success = success .and. Dh_AllocInit (SetMaxDataNr, 6, model % TableDateTime, 0)
!         Allocate     ( TableDateTime (MaxData,6), Stat=Allocation_Error )

         success = success .and. Dh_AllocInit (SetMaxDataNr, model % TableData, 0E0)
!         Allocate     ( TableData(MaxData), Stat=Allocation_Error )

         If (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error allocating arrays in subroutine NewTable_ConfAr')
         Endif
         model%FirstFreeTable    = 1
         model%FirstFreeDateTime = 1
         model%FirstFreeData     = 1

!        write(*,*) ' MaxTabNr = ', MaxTabNr
!        write(*,*) ' MaxData  = ', MaxData

         model % oldmethod = .false.

         NewTableConfAr = .true.

      Return
      End Function NewTableConfar


      Logical Function GetStringFromBuffer(String)
      ! put buffer in String for external use
      character(len=*) String

      String = ' '
      String = Buffer(1:nbuf)

      If (Len(String) .lt. nbuf) then
        call SetMessage(LEVEL_FATAL, 'Stringsize too small to copy buffer contents NewTable_GetStringFromBuffer')
        GetStringFromBuffer = .false.
        Return
      Endif

      GetStringFromBuffer = .true.
      Return
      End Function GetStringFromBuffer



      Logical Function GetRecordKeyUppAnyWhere  ( lu , keywrd, Endfil, Idebug, Iout1)
!     KeyUpp (upper case version of Keywrd) anywhere in the line
      logical endfil
      integer lu, idebug, Iout1
      character(Len=MaxKeyLength)   keywrd
      integer     KeyUppOption

      KeyUppOption = 9999     ! just put KeyUppOption on a value higher than the line length of lines in the input file
      GetRecordKeyUppAnyWhere =  GetRecordGeneral ( lu , keywrd, Endfil, Idebug, Iout1, KeyUppOption)

      Return
      End Function GetRecordKeyUppAnyWhere


      Logical Function GetRecordGeneral ( lu , keywrd, Endfil, Idebug, Iout1, KeyUppOption)
!***********************************************************************
! Routine voor lezen van 1 record van unit lu met keyword Keywrd
! the uppercase version of the keyword should occur before position KeyUppOption in the input file lines
!***********************************************************************

      logical endfil
      integer lu, idebug, Iout1
      character(Len=MaxKeyLength)   keywrd
      integer     KeyUppOption

      character(Len=MaxKeyLength)       keyupp, keylow
      character(Len=MaxKeyLengthPlus1)  space_keylow
      integer            linlen, iposl, iposr, toklen, ios, nrecr, actlen, StartPos
      parameter         (linlen=2048)
      character(len=linlen) line
      logical            start , stopr

! Additional variables for ParseToken
      Integer ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical ParseTokenCaseSensitive
      Type (TokenArray) RecordData
!     Logical ReadError

      GetRecordGeneral = .false.
      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenCaseSensitive = .true.   ! handhaaf de originale upper/lower case
! end of additional variables

      nbuf = 0
      last_position = 1
      buffer = ' '
      start  = .false.
      stopr  = .false.
      keyupp = ''
      keylow = ''
      space_keylow = ''
      rerror = 0
      eof = .false.
      nrecr = 0

      if ( keywrd .eq. ' ' ) then
          rerror = 1
          return
      else
!         Construct UPPERCASE en lowercase version of keyword
          call uppcas ( keywrd , keyupp , MaxKeyLength )
          call lowcas ( keywrd , keylow , MaxKeyLength )
          space_keylow = ' '//keylow
      endif

!     Read new line as long as it is necessary
   10 read (lu,'(a)',iostat=ios) line
      actlen = Len_Trim(line)
      if (idebug .ne. 0) write(idebug,'(A,A)') ' Getrecord line =',line(1:actlen)
      nrecr = nrecr + 1
      eof = ios.ne.0
      if ( eof ) goto 20

!     write(*,*) ' start buffer=', buffer(1:nbuf)
!     write(*,*) ' Actlen Line=', actlen
!     Look for keyupp: to start recording
!     Nov 2002: added check on KeyUppOption:
!               -KEYUPP may occur anywhere in the line (KeyUppOption is any number > LinLen)
!               -KEYUPP may occur only at the beginning of the line before column KeyUppOption
      if ( .not.start ) then
          iposl = index(line,keyupp)
          if ( iposl .gt. 0) then
              ! check if position is valid for KeyUpp (it should be a separate token!)
              If (.not. ParseTokenArrayWithKeywords (Line(1:), ScanToTk, RecordData, &
                                                     NumberOfTokens, ParseTokenCaseSensitive) ) Return
                   ! always returns true!
              if (Getkey (KeyUpp, IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenCaseSensitive)) then
                 StartPos = RecordData%StartPositionOfToken(ReturnIndx)
                 if (KeyUppOption .ge. LinLen) then
                    start = .true.
                 else
                    start = (StartPos .le. KeyUppOption)
                 endif
                 if (idebug .ne. 0) write(idebug,*) ' Start recording; Found keyword ',Keyupp, &
                                                    ' at position', iposl, ' token nr ', ReturnIndx, &
                                                    ' StartPos   ', StartPos
                 iposl = StartPos
              endif
          endif
      else
          iposl = 1
      endif

!     Look for keylow: to stop recording
      if ( start .and. .not.stopr ) then
!         Check for space // keylow
          iposr = index(line(iposl:),space_keylow)
          if (Idebug .ne. 0) write(idebug,*) ' check space_keylow found position', iposr
          if (Idebug .ne. 0) write(idebug,'(A,A)') ' space_keylow =', keylow
          if ( iposr .gt. 0 ) then
              ! check if position is valid for KeyLow (it should be a separate token!)
              if (.not. ParseTokenArrayWithKeywords (Line(iposl:ActLen), ScanToTk, RecordData, &
                                                     NumberOfTokens, ParseTokenCaseSensitive) ) return
                 ! always returns true
              if (Getkey (KeyLow, IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenCaseSensitive)) then
                 StartPos = RecordData%StartPositionOfToken(ReturnIndx)
                 if (Idebug .ne. 0) write(idebug,*) ' GetKey keylow found position', StartPos
                 if (Idebug .ne. 0) write(idebug,'(A,1X,A)') ' Line(Startpos:)', Line(StartPos:)
                 if (idebug .ne. 0) write(idebug,*) ' Stop recording; Found keyword ',Keylow, &
                                                    ' at position', iposr, ' token nr ', ReturnIndx, &
                                                    ' StartPos   ', StartPos
                 iposr = StartPos
!                Position IPOSR at end of keylow
                 stopr = .true.
                 iposr = iposr + (iposl -1) + Len_Trim(Keylow)   !was +4
              else
                 iposr = actlen
               endif
          elseif ( iposl .eq. 1 .and. line(1:4) .eq. keylow ) then
!             Keylow is in first 4 positions!
!             iposr = 4  ! first an extra check is needed !!!
              ! check if position is valid for KeyLow (it should be a separate token!)
              if (.not. ParseTokenArrayWithKeywords (Line(1:), ScanToTk, RecordData, &
                                                NumberOfTokens, ParseTokenCaseSensitive) ) Return
              if (Getkey (KeyLow, IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenCaseSensitive)) then
                 StartPos = RecordData%StartPositionOfToken(ReturnIndx)
                 if (idebug .ne. 0) write(idebug,*) ' Stop recording; Found keyword ',Keylow, &
                                                    ' at position', iposr, ' token nr ', ReturnIndx, &
                                                    ' StartPos   ', StartPos
                 iposr = StartPos + Len_Trim(KeyLow)  ! was + 4
                 stopr = .true.
              else
!                 Apparently not found, so copy whole line
                  iposr = actlen     ! was linlen
              endif
          else
!             Find end of line
                  iposr = actlen     ! was linlen
!              endif
          endif
      endif

      if ( start ) then
!         Store valid part of line in buffer
          toklen = iposr-iposl+1
          if ( nbuf+toklen+1 .gt. bufmax ) then
!             write(*,'(i5,a)') nrecr,buffer(nbuf-20:nbuf)
             GetRecordGeneral = .not. GenerateMessage ( 3 , 'record does not fit in buffer' , keywrd , ' ' , Iout1)
             Return
          endif
          if ( toklen .gt. 0 ) then
              if (idebug .ne. 0) write(idebug,*) ' store line from ',iposl,' to ',iposr, Line(iposl:iposr)
              buffer(nbuf+1:nbuf+toklen) = line(iposl:iposr)
              nbuf = nbuf + toklen + 1
          endif
      endif

!     Stop reading??
      if ( .not.stopr ) goto 10
   20 continue
      endfil = eof
      if ( start .and. .not.stopr ) then
         GetRecordGeneral = .not. GenerateMessage ( 3 , 'Incomplete record',keyupp, ' ', Iout1)
         Return
      endif
      keyact = keyupp

      if (idebug .ne. 0) write(idebug,*) ' Get Record to Buffer ', buffer(1:500)
      GetRecordGeneral = .true.

      return
 9000 GetRecordGeneral = .not. GenerateMessage ( 3 , 'Unknown read error' ,' ', ' ' , Iout1)

      return
      end Function GetRecordGeneral



      Logical Function GetTableName (TabYesNo, TableName, Keywrd, Iout1)

! Get table name achter keyword Keywrd
! En geef aan of keyword TBLE en tble gevonden
! Routine supports table names up to 128/256 characters  =CharIdLength !!!!; Check Taiwan Oct 2007
!   if the calling program only supports table names up to 32 characters, the first 32 will be used

      Logical   TabYesNo
      Character(len=*) TableName
      character(Len=MaxKeyLength) keywrd

      Integer       idum, iout1
      Real          rdum
      Character(len=128) cdum            ! was 256, Taiwan Oct 2007
      Logical       allow, found

      Allow = .false.
      Found = .false.

      TableName = ''
      GetTableName = ( GETVAR (Buffer(1:nbuf), Keywrd, 1,' Read Table',' TBL file',IOUT1, &
                               CDUM, RDUM, IDUM, ALLOW, FOUND)  .eq. 0)
      if (found) TableName = CDUM

! find keyword TBLE and tble in buffer; gebeurt in GetTab
!     Idum  = INDEX(Buffer, 'TBLE')
!     Idum2 = INDEX(Buffer, 'tble')
!     TabYesNo = (idum .gt. 0 .and. idum2 .gt. idum)
      TabYesNo = .true.

      Return
      End Function GetTableName



      Logical Function GetTable (ModelHandle, TableName, NrColumns, TableNr, Idebug, Iout1)

! Get table with name TableName, NrColumns data fields
! Use full Buffer
! Put result in Table arrays; table number = TableNr

      Character(len=*) TableName
      Integer, intent(in)  :: ModelHandle, idebug, Iout1
      Integer      TableNr
      Integer      NrColumns
      Integer      IleftBuf, IRightBuf

      ILeftBuf = 1
      IRightBuf = NBuf
      GetTable = GetTableFromBuffer (ModelHandle, TableName, NrColumns, TableNr, Idebug, Iout1, ILeftBuf, IRightBuf)

      End Function GetTable


      Logical Function GetTableFromBuffer (ModelHandle, TableName, NrColumns, TableNr, Idebug, Iout1, ILeftBuf, IRightBuf)

! Get table with name TableName, NrColumns data fields
! From Buffer part (ILeftBuf:IRightBuf)
! Put result in Table arrays; table number = TableNr

      type (SobekTimeTableStore) , pointer    :: model

      Logical      TabYesNo
      Character(len=*) TableName
      Integer, intent(in)  :: ModelHandle, idebug, Iout1
      Integer      TableNr
      Integer      NrColumns
      Integer      LocalNrRows, &
                   InterpolationCode, PeriodCode, ILeftBuf, IRightBuf
      Real         CheckPeriod

      Integer      iyear, imonth, iday, ihour, iminute, isecond
      Integer      i,j,k, idum, idum1, idum2, ileft, iright, ipos, NrData, indexj, NrDays, NrHours, NrMinutes
      Integer      itest, itest2
      Character(len=1) quote, klteken, slash, puntkomma, dubbelpunt, teken
      Logical      lstop, rderror, PeriodError

      Integer IDate1, IDate2, ITime1, ITime2, icount, Allocation_Error
      Double precision JulianDate1, JulianDate2, Julian

! May 2002; Additional variables for ParseToken
      Integer           NumberOfTokens
      Type (TokenArray) TempData
      Character(len=1000) String


      GetTableFromBuffer = .false.
      klteken   = '<'
      slash     = '/'
      puntkomma = ';'
      dubbelpunt= ':'
      quote     = ''''

      String    = ''


      model => NewTablesModelFind_ByHandle (modelHandle)

! find Period and Interpolation: PDIN keyword
      Ileft  =  INDEX(Buffer(ILeftBuf:IRightBuf), 'PDIN')
      Iright =  INDEX(Buffer(ILeftBuf:IRightBuf), 'pdin')
      InterpolationCode = 1
      PeriodCode        = 0
      if (ileft .gt. 0 .and. iright .gt. ileft) then
         ileft = ILeftBuf-1 + ILeft
         iright= ILeftBuf-1 + IRight
         Read (Buffer(ileft+4:),*) InterpolationCode, PeriodCode
         If (PeriodCode .gt. 0) then
!           get period; eerst eventuele quotes verwijderen tussen PDIN en pdin
            Do idum=ileft+4, iright
               if (buffer(idum:idum) .eq. quote) buffer(idum:idum) = ' '
            enddo
            Itest  =  INDEX(Buffer(ileft:iright), puntkomma)
            Itest2 =  INDEX(Buffer(ileft:iright), dubbelpunt)
! format van periode is of 'ddd;hh:mm:ss' (ARS 4778) of 'sssssss' (als voorheen)
            if (itest .gt. 0 .or. itest2 .gt. 0) then
!              Eerst de ; en : verwijderen om dan free format te kunnen lezen en de periode in seconden te bepalen
               Do idum=ileft, iright
                  teken = buffer(idum:idum)
                  if (teken .eq. dubbelpunt .or. teken .eq. puntkomma .or. teken .eq. quote) buffer(idum:idum) = ' '
               Enddo
               Read (Buffer(ileft+4:),*) idum1, idum2, iday, ihour, iminute, isecond
               PeriodCode = 86400 * Iday + 3600 * IHour + 60 * iMinute + Isecond
            else
               Read (Buffer(ileft+4:),*) idum1, idum2, PeriodCode
            endif
            if (PeriodCode .ne. 31536000 .and. PeriodCode .ne. 30*86400 .and. PeriodCode .ne. 86400) then
               call SetMessage(LEVEL_WARN, 'Check Input Table '//trim(TableName(1:32))//' Specified value for Period is unlikely')
               call SetMessage(LEVEL_WARN, 'Likely values are 31536000 (year), 30*86400 (month) and 86400 (daily period)')
            endif
         endif
      Endif

! find keyword TBLE and tble in buffer; nog eens verificatie dat het voorkomt
      if (idebug .ne. 0) write(idebug,*) ' Get Table with ', NrColumns, ' columns'
      if (idebug .ne. 0) write(idebug,*) ' from Buffer ', buffer(1:500)
      Ileft  =  INDEX(Buffer(ILeftBuf:IRightBuf), 'TBLE')
      Iright =  INDEX(Buffer(iLeftBuf:IRightBuf), 'tble')
      TabYesNo = (ileft .gt. 0 .and. iright .gt. ileft)
!     CntStr = aantal keren '<' in de ingelezen tabel
!     difference between CntStr and MyOwnCntStr, both from ReadLib, is the use of intrinsic Len_trim or not
!     the intrinsic Len_trim routine of Compaq 6.1 Visual Fortran fails for very long strings with a stack overflow
!     MyOwnLenTrim still works fine, also for long strings, but is fo course somewhat slower
!     the test value of 5.0E5 works ok for 1 GlobalWind definition on my NT PC 512 Mb Ram
      If (IRightBuf-ILeftBuf .lt. 5.0E5) Then
         LocalNrRows = Max (1, CntStr (klteken, buffer(ILeftBuf:IRightBuf)) ) + 1
      Else
         LocalNrRows = Max (1, MyOwnCntStr (klteken, buffer(ILeftBuf:IRightBuf)) ) + 1
      Endif
!     NB LocalRows = aantal < tekens + 1, ivm mogelijke extra rij bij cyclische tabel
      if (idebug .ne. 0) write(idebug,*) ' GetTable LocalNrRows=', LocalNrRows
      Allocate   ( RLocal (LocalNrRows, NrColumns), Stat=Allocation_Error )
      If (Allocation_Error .ne. 0) then
        call SetMessage(LEVEL_FATAL, 'Error allocating arrays in subroutine GetTableFRomBuffer')
      Endif

      if (TabYesNo) then
         ileft = ILeftBuf-1 + ILeft
         iright= ILeftBuf-1 + IRight
! 17 May 2002: check on max. table number should be .le. instead of .lt.
         if (model%FirstFreeTable .le. model%MaxTabnr) then
            TableNr = model%FirstFreeTable
            model%FirstFreeTable = model % FirstFreeTable + 1
            model%NTab (TableNr,IndexNTabElements)      = 0
            model%NTab (TableNr,IndexFirstDateTime)     = model%FirstFreeDateTime
            model%NTab (TableNr,IndexFirstDataValue)    = model%FirstFreeData
            model%NTab (TableNr,IndexInterpolationCode) = InterpolationCode   ! Interpolatie volgens PDIN
            model%NTab (TableNr,IndexPeriodic)          = PeriodCode          ! Periode volgens PDIN
            model%NTab (TableNr,IndexLastRowUsed)       = 0                   ! last index used
            NrData = 0
            lstop  = .false.
            icount = 0
            if (model%oldmethod) then
!            Oldmethod nog niet aangepast voor cyclische tabellen
              rderror = (model%FirstFreeDateTime .ge. model%MaxData .or. model%FirstFreeData .ge. model%MaxData)
              Do While (.not. lstop .and. .not. rderror)
!              ! Get next Date and time
                 Ipos  = INDEX(Buffer(ileft:iright), klteken)
                 icount = icount + 1
                 Idum  = ileft
                 Idum1 = ileft + ipos - 1
                 Idum2 = Index (buffer(idum:idum1), puntkomma)
                 Idum2 = ileft + idum2 - 1
                 if (idebug .ne. 0) then
                   write(idebug,*) ' Table read string positions ', ileft, idum1, idum2, iright
                   Write(idebug,*) buffer(idum2-10:idum2+9)
                 Endif
                 read (buffer(idum2-10:idum2+9),'(I4,5(1X,I2))' )  iyear, imonth, iday, ihour, iminute, isecond
                 if (idebug .ne. 0) write(idebug,*) iyear, imonth, iday, ihour, iminute, isecond
! ARS XXXX May 21, 2002: check correctness date/time; IHour =0..23, IMinute = 0..59
                If (IMinute .ge. 60) then
                    IMinute = 0
                    IHour   = IHour + 1
                Endif
                If (IHour .ge. 24) then
                   IHour = 0
                   Call NxtDay (Idebug, IYear, IMonth, IDay)
                Endif
! End ARS
                model%TableDateTime (model%FirstFreeDateTime,IndexDay)    = iday
                model%TableDateTime (model%FirstFreeDateTime,IndexMonth)  = imonth
                model%TableDateTime (model%FirstFreeDateTime,IndexYear)   = iyear
                model%TableDateTime (model%FirstFreeDateTime,IndexHour)   = ihour
                model%TableDateTime (model%FirstFreeDateTime,IndexMinute) = iminute
                model%TableDateTime (model%FirstFreeDateTime,IndexSecond) = isecond
                model%FirstFreeDateTime = model%FirstFreeDateTime + 1
               ! Get data values for Nrcolumns
                 NrData = NrData + 1
                 if (NrData .gt. LocalNrRows) then
                    Return
                 endif
                 ileft  =  idum2+10
                 if (idebug .ne. 0) write(idebug,*) ' Get data positions ', ileft, idum1-1, NrColumns
                 if (idebug .ne. 0) write (idebug,'(A)') buffer(ileft:idum1-1)
                 read (buffer(ileft:),*) (Rlocal(NrData,i),i=1,NrColumns)
                 ileft = idum1 + 1
                model%FirstFreeData  = model%FirstFreeData + NrColumns
                 lstop = (ileft .ge. iright-2 .OR. Icount .eq. localNrRows-1)
                 rderror = (model%FirstFreeDateTime .ge. model%MaxData .or. model%FirstFreeData .ge. model%MaxData)
              Enddo
            Else   ! NEW METHOD
              rderror = (model%FirstFreeDateTime .ge. model%MaxData-LocalNrRows .or. &
                          model%FirstFreeData .ge. model%MaxData-LocalNrRows*NrColumns)
              if (.not. Rderror) then
! May 2002: check NrColumns
                 Ipos  = INDEX(Buffer(ileft:iright), klteken)
                 String = Buffer(ileft:ileft+ipos-1)
                 if (.not. ParseTokenArrayWithKeywords (String, NrColumns+3, TempData, NumberOfTokens, .false.) ) Return
                 If (TempData%Token(NumberOfTokens) .ne. '<') then
                    NumberOfTokens = NumberOfTokens-1
                 Endif
                 If (NumberOfTokens .lt. NrColumns+3) then
                    call SetMessage(LEVEL_WARN, 'Table contains less columns then expected; will read less columns for table: '//trim(TableName))
                    NrColumns = NumberOfTokens-3
                    If (NrColumns .le. 0) then
                       call SetMessage(LEVEL_FATAL, 'Error getting Table '//trim(TableName))
                       GetTableFromBuffer = .false.
                       Return
                    Endif
                 Endif
! End Check
                 ileft  = ileft + 4     ! ileft was begin van TBLE; kan dus 4 naar rechts
                 iright = iright- 1     ! iright was begin van tble; kan dus 1 naar links
!                verwijder alle overtollige karakters ;:'/< en lees dan free format in
                 Do idum=ileft, iright
                    teken = buffer(idum:idum)
                    if (teken .eq. klteken .or. teken .eq. slash .or.&
                         teken .eq. dubbelpunt .or. teken .eq. puntkomma .or. &
                                                       teken .eq. quote) buffer(idum:idum) = ' '
                 Enddo
                 If (Idebug .ne. 0) Write(idebug,*) Buffer(ileft:iright)
                 Read (buffer(ileft:),*) ( model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexYear),&
                                            model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexMonth),&
                                             model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexDay),&
                                               model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexHour), &
                                               model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexMinute), &
                                               model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexSecond), &
                                               (Rlocal(NrData,i),I=1,NrColumns), NrData=1,LocalNrRows-1 )
! ARS XXXX May 21, 2002: check correctness date/time; IHour =0..23, IMinute = 0..59
                 Do NrData=1,LocalNrRows-1
                   If (model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexMinute) .ge. 60) then
                       model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexMinute) = 0
                       model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexHour) = &
                                  model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexHour) + 1
                   Endif
                   If (model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexHour) .ge. 24) then
                      model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexHour) = 0
                      Call NxtDay (Idebug, model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexYear), &
                                             model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexMonth), &
                                               model%TableDateTime(model%FirstFreeDateTime+NrData-1,IndexDay))
                   Endif
                 Enddo
! End ARS
                 model%FirstFreeDateTime  = model%FirstFreeDateTime + LocalNrRows - 1
                 model%FirstFreeData      = model%FirstFreeData + NrColumns * (LocalNrRows-1)
              Else
                 call SetMessage(LEVEL_ERROR, 'Tables too long. Error NewTable dimensions')
                 Return
              Endif
            Endif
         Else
            call SetMessage(LEVEL_ERROR, 'Too many tables. Error NewTable dimensions')
            Return
         Endif
      Endif

! zet data van array RLocal in global array, ook de eventuele extra rij
      model%Ntab(TableNr,IndexNTabElements) = LocalNrRows
      Idum = model%Ntab(TableNr,IndexFirstDataValue) -1
      Do i=1,NrColumns
         Do j=1,LocalNrRows
            idum  = idum+1
            if (.not. FixArs14800) then
               indexj = min (j, LocalNrRows-1)
            else
              indexj = j
              if (j .eq. LocalNrRows) indexj = 1
            endif
            model%TableData (idum) = Rlocal(indexj,i)
         Enddo
      Enddo

! extra rij voor 1 extra datum/tijd
      model%FirstFreeDateTime  = model%FirstFreeDateTime + 1
      model%FirstFreeData      = model%FirstFreeData + NrColumns
      RdError = (model%FirstFreeDateTime .ge. model%MaxData .or. model%FirstFreeData .ge. model%MaxData)
      if (RdError) then
         call SetMessage(LEVEL_ERROR, 'Error NewTable dimensions')
         Return
      Endif
! zet default waarde voor extra datum/tijd = oude waarde + 1 minuut
      j = model%Ntab(TableNr,IndexFirstDateTime) + model%NTab(TableNr,IndexNTabElements)-1
      model%TableDateTime(j,IndexDay)    = model%TableDateTime(j-1,IndexDay)
      model%TableDateTime(j,IndexMonth)  = model%TableDateTime(j-1,IndexMonth)
      model%TableDateTime(j,IndexYear)   = model%TableDateTime(j-1,IndexYear)
      model%TableDateTime(j,IndexHour)   = model%TableDateTime(j-1,IndexHour)
      model%TableDateTime(j,IndexMinute) = model%TableDateTime(j-1,IndexMinute)
      model%TableDateTime(j,IndexSecond) = model%TableDateTime(j-1,IndexSecond)
      if (model%TableDateTime(j,IndexMinute) .ne. 59) then
         model%TableDateTime(j,IndexMinute) = model%TableDateTime(j,IndexMinute) + 1
      elseif (model%TableDateTime(j,IndexHour) .ne. 23) then
         model%TableDateTime(j,IndexHour) = model%TableDateTime(j,IndexHour) + 1
         model%TableDateTime(j,IndexMinute) = 0.0
      else
!       laat laatste tijd maar hetzelfde als de voorlaatste
      endif


      if (idebug .ne. 0) then
         write(idebug,*) ' TableNr ', TableNr
         write(idebug,*) ' length of table ', model%Ntab(TableNr,IndexNTabElements)
         write(idebug,*) ' first position datetime ', model%Ntab(TableNr,IndexFirstDateTime)
         write(idebug,*) ' first position data     ', model%Ntab(TableNr,IndexFirstDataValue)
         write(idebug,*) ' Day Month Year Hour Min Sec DataValue'
         Do i = 1,model%NTab(TableNr,IndexNTabElements)
            j = model%Ntab(TableNr,IndexFirstDateTime)  + i-1
            k = model%Ntab(TableNr,IndexFirstDataValue) + i-1
            write(idebug,'(6I5,999F10.3)') &
               (model%TableDateTime(j,idum),idum=1,6), &
               (model%TableData (k+idum2*model%NTab(TableNr,IndexNTabElements)),idum2=0,NrColumns-1)
         Enddo

         write(idebug,*) ' All Table characteristics'
         write(idebug,*) ' Tablength FirstDatePos FirstDataPos Interpolation Period LastIndex'
         Do i = 1,model%FirstFreeTable-1
            write(idebug,'(6I9)') (model%NTab(i,idum),idum=1,6)
         Enddo
         write(idebug,*) ' All Table Date/Times '
         Do i = 1,model%FirstFreeDateTime-1
            write(idebug,'(6I5)') (model%TableDateTime(i,idum),idum=1,6)
         Enddo
         write(idebug,*) ' Data array '
         write(idebug,'(10F8.3)') (model%TableData (i),i=1, model%FirstFreeData-1)
      endif

      Deallocate   ( RLocal )

! check de opgegeven periode van een cyclische tabel met het verschil van begin- en eindtijd
      if (model%NTab (TableNr,IndexPeriodic) .gt. 0) then
         j  = model%Ntab(TableNr,IndexFirstDateTime)
         IDate1 = SetDate (model%TableDateTime(j,IndexYear), model%TableDateTime(j,IndexMonth), model%TableDateTime(j,IndexDay))
         ITime1 = SetTime (model%TableDateTime(j,IndexHour), model%TableDateTime(j,IndexMinute), model%TableDateTime(j,IndexSecond))
         JulianDate1= Julian (idate1,itime1)
         j = model%Ntab(TableNr,IndexFirstDateTime) + model%NTab(TableNr,IndexNTabElements)-1
         IDate2 = SetDate (model%TableDateTime(j,IndexYear), model%TableDateTime(j,IndexMonth), model%TableDateTime(j,IndexDay))
         ITime2 = SetTime (model%TableDateTime(j,IndexHour), model%TableDateTime(j,IndexMinute), model%TableDateTime(j,IndexSecond))
         JulianDate2 = Julian (idate2,itime2)
         If (idebug .ne. 0) write(idebug,*) ' Check Periodicity Juliandates ', JulianDate1, JulianDate2
         CheckPeriod = ( 86400. * (JulianDate2-JulianDate1) )
         If (idebug .ne. 0) write(idebug,*) ' CheckPeriod ', CheckPeriod
         PeriodError = .false.
         ! periode >= 1 maand; afwijking tot 1 dag toegestaan
         ! periode >= 1 dag; afwijking tot 1 uur
         ! periode < 1 dag; afwijking tot 1 minuut toegestaan
!        Bij cyclische tabel: geef waarschuwing als periode niet klopt met lengte tabel
         If (model%Ntab(TableNr,IndexPeriodic) .ge. 30*86400 .and.  &
               Abs (CheckPeriod-model%NTab(TableNr,IndexPeriodic)) .gt. 86400) PeriodError = .true.
         If (model%Ntab(TableNr,IndexPeriodic) .ge. 86400 .and. &
                Abs (CheckPeriod-model%NTab(TableNr,IndexPeriodic)) .gt. 3600) PeriodError = .true.
         If (Abs (CheckPeriod-model%NTab(TableNr,IndexPeriodic)) .gt. 60) PeriodError = .true.
!        If (model%Ntab(TableNr,IndexInterpolationCode) .eq. 1 .and. PeriodError) then
! Bij cyclische tabel, blok: als periode niet klopt met lengte tabel voeg einddatum toe met identieke waarde
         If (PeriodError) then
! Bij cyclische tabel: als periode niet klopt met lengte tabel voeg einddatum toe met identieke waarde
! Compute and set date in array; from starttime, determine how many days in the period (no check on hours!!)
            if (idebug .ne. 0) then
               write(idebug,*) ' Table will be extended'
               write(idebug,*) ' TableNr ', TableNr
            endif
            j = model%Ntab(TableNr,IndexFirstDateTime)
            IDay   = model%TableDateTime(j,IndexDay)
            IMonth = model%TableDateTime(j,IndexMonth)
            IYear  = model%TableDateTime(j,IndexYear)
            IHour  = model%TableDateTime(j,IndexHour)
            IMinute= model%TableDateTime(j,IndexMinute)
            ISecond= model%TableDateTime(j,IndexSecond)
            j = model%NTab(TableNr,IndexPeriodic) / 86400
            if (idebug .ne. 0) then
               write(idebug,*) ' Initial IYear, IMonth, IDay, IHour, IMinute, ISecond'
               write(idebug,'(6I5)')  IYear, IMonth, IDay, IHour, IMinute, ISecond
            endif
            Do idum=1,j
               Call NxtDay (Idebug,IYear,IMonth,IDay)
            Enddo
            Idate2 = SetDate (IYear, IMonth, IDay)
! ARS 14799 also allow tables with periodicity less than one day
            NrDays = model%NTab(TableNr,IndexPeriodic) / 86400
            j = model%NTab(TableNr,IndexPeriodic) - (NrDays * 86400)
            NrHours = j / 3600
            IHour = Ihour + NrHours
            If (Ihour .ge. 24) then
               Call NxtDay (Idebug,IYear,IMonth,IDay)
               Ihour = Ihour - 24
            Endif
            j = model%NTab(TableNr,IndexPeriodic) - (NrDays * 86400) - (NrHours * 3600)
            NrMinutes = j / 60
            IMinute = IMinute + NrMinutes
            If (IMinute .ge. 60) then
               IMinute = Iminute - 60
               IHour = IHour + 1
               If (Ihour .ge. 24) then
                  Call NxtDay (Idebug,IYear,IMonth,IDay)
                  Ihour = Ihour - 24
               Endif
            Endif
            IDate2 = SetDate (IYear, IMonth , IDay)
            ITime1 = SetTime (Ihour, IMinute, ISecond)
!           Before used uncorrected IDate2 and Itime1, so assuming only periods of 1 day or a multiple of 1 day
!           Now, using corrected date/time
            JulianDate2 = Julian (idate2,itime1)
            Call GregorSbk (JulianDate2,  IYear, IMonth, IDay, IHour, IMinute, ISecond)
            j = model%Ntab(TableNr,IndexFirstDateTime) + model%NTab(TableNr,IndexNTabElements)-1
            if (idebug .ne. 0) then
               write(idebug,*) ' Final IYear, IMonth, IDay, IHour, IMinute, ISecond'
               write(idebug,'(6I5)')  IYear, IMonth, IDay, IHour, IMinute, ISecond
            endif
            model%TableDateTime(j,IndexDay)   = IDay
            model%TableDateTime(j,IndexMonth) = IMonth
            model%TableDateTime(j,IndexYear)  = IYear
            model%TableDateTime(j,IndexHour)  = Ihour
            model%TableDateTime(j,IndexMinute)= IMinute
            model%TableDateTime(j,IndexSecond)= ISecond
! Copy last data already done
! Extra debug
            if (idebug .ne. 0) then
               write(idebug,*) ' Table is extended'
               write(idebug,*) ' TableNr ', TableNr
               write(idebug,*) ' length of table ', model%Ntab(TableNr,IndexNTabElements)
               write(idebug,*) ' first position datetime ', model%Ntab(TableNr,IndexFirstDateTime)
               write(idebug,*) ' first position data     ', model%Ntab(TableNr,IndexFirstDataValue)
               write(idebug,*) ' Day Month Year Hour Min Sec DataValue'
               Do i = 1,model%NTab(TableNr,IndexNTabElements)
                  j = model%Ntab(TableNr,IndexFirstDateTime) + i-1
                  k = model%Ntab(TableNr,IndexFirstDataValue)+ i-1
                  write(idebug,'(6I5,999F10.3)') &
                     (model%TableDateTime(j,idum),idum=1,6), &
                     (model%TableData (k+idum2*model%NTab(TableNr,IndexNTabElements)),idum2=0,NrColumns-1)
               Enddo

               write(idebug,*) ' All Table characteristics'
               write(idebug,*) ' Tablength FirstDatePos FirstDataPos Interpolation Period LastIndex'
               Do i = 1,model%FirstFreeTable-1
                  write(idebug,'(6I9)') (model%NTab(i,idum),idum=1,6)
               Enddo
               write(idebug,*) ' All Table Date/Times '
               Do i = 1,model%FirstFreeDateTime-1
                  write(idebug,'(6I5)') (model%TableDateTime(i,idum),idum=1,6)
               Enddo
               write(idebug,*) ' Data array '
               write(idebug,'(10F8.3)') (model%TableData (i),i=1, model%FirstFreeData-1)
            endif
         Endif

!        Bij cyclische tabel, interpolatie: geef waarschuwing
         If (model%Ntab(TableNr,IndexInterpolationCode) .eq. 0 .and. PeriodError) then
            call SetMessage(LEVEL_WARN, 'Period specified for table not consistent with length of table'//trim(TableName(1:32)))
            Write(iout1,*) ' Period specified:', model%NTab(TableNr,IndexPeriodic)
            Write(iout1,*) ' Length of table :', CheckPeriod
         Endif
      Endif


      GetTableFromBuffer = .true.

      Return
      End Function GetTableFromBuffer


      Logical Function ReduceTable(ModelHandle, TabelNr, NrColumns, StartDate, StartTime, &
                                   EndDate, EndTime, Idebug, Iout1)

!  Try to reduce the used table length(useful if table is much longer than relevant simulation period)
!  whole table is read, but variables indicating start and length are changed

      type (SobekTimeTableStore) , pointer    :: model

      Integer, intent(in)  :: ModelHandle, idebug, Iout1
      Integer TabelNr, NrColumns
      type (Date)  StartDate, EndDate
      type (Time)  StartTime, EndTime

      Integer i, j, k, idum, idum2
      Integer IDate1, IDate2
      Integer ITime1, ITime2
      Double precision JulianDate1, JulianDate2, Julian
      logical Again

      ReduceTable = .false.

      model => NewTablesModelFind_ByHandle (modelHandle)

! No action if table nr too large or table periodic,
      If (TabelNr .gt. model%MaxTabNr) then
         ReduceTable = .not. GenerateMessage ( 3 , 'Table Number too large',' ', ' ' , Iout1)
         Return
      Endif
      If (model%NTab(TabelNr,IndexPeriodic) .gt. 0)  goto 999

!  Julian date of StartDate
      Idate2 = SetDate(StartDate%Year, StartDate%Month, StartDate%Day)
      Itime2 = SetTime(StartTime%Hour, StartTime%Minute, StartTime%Second)
      JulianDate2= Julian (idate2,itime2)
!  Check with Julian date of tabel dates; dates before start date are not used
      Again = .true.
      Do while (Again)
  101    j = model%Ntab(TabelNr,IndexFirstDateTime)       ! j = first position in date-time array
         Idate1 = SetDate(model%TableDateTime(j,IndexYear), model%TableDateTime(j,IndexMonth), model%TableDateTime(j,IndexDay))
         Itime1 = SetTime(model%TableDateTime(j,IndexHour), model%TableDateTime(j,IndexMinute), model%TableDateTime(j,IndexSecond))
         JulianDate1= Julian (idate1,itime1)
         Again = .false.
         If (JulianDate1 .lt. JulianDate2) then
            ! length of table reduced by 1
            model%NTab(TabelNr,IndexNTabElements  ) = model%NTab(TabelNr,IndexNTabElements  ) - 1
            ! start position in date increased by 1
            model%NTab(TabelNr,IndexFirstDateTime ) = model%NTab(TabelNr,IndexFirstDateTime ) + 1
            ! start position in data increased by NrColumns
            model%NTab(TabelNr,IndexFirstDataValue) = model%NTab(TabelNr,IndexFirstDataValue) + NrColumns
            Again = (model%NTab(TabelNr,IndexNTabElements) .gt. 0)
         Else
            Again = .false.
         Endif
      Enddo

!  Julian date of EndDate
      Idate2 = SetDate(EndDate%Year, EndDate%Month, EndDate%Day)
      Itime2 = SetTime(EndTime%Hour, EndTime%Minute, EndTime%Second)
      JulianDate2= Julian (idate2,itime2)
!  Check with Julian date of tabel dates; dates after end date are not used
      Again = .true.
      Do while (Again)
         j = model%Ntab(TabelNr,IndexFirstDateTime) +model%Ntab(TabelNr,IndexNTabElements)-1  ! j = last position in date-time array
         Idate1 = SetDate(model%TableDateTime(j,IndexYear), model%TableDateTime(j,IndexMonth), model%TableDateTime(j,IndexDay))
         Itime1 = SetTime(model%TableDateTime(j,IndexHour), model%TableDateTime(j,IndexMinute), model%TableDateTime(j,IndexSecond))
         JulianDate1= Julian (idate1,itime1)
         Again = .false.
         If (JulianDate1 .gt. JulianDate2) then
            model%NTab(TabelNr,IndexNTabElements) = model%NTab(TabelNr,IndexNTabElements) - 1
            Again = (model%NTab(TabelNr,IndexNTabElements) .gt. 0)
         Else
            Again = .false.
         Endif
      Enddo


! Extra debug
      if (idebug .ne. 0) then
         write(idebug,*) ' Table is reduced'
         write(idebug,*) ' TableNr ', TabelNr
         write(idebug,*) ' length of table ', model%Ntab(TabelNr,IndexNTabElements)
         write(idebug,*) ' first position datetime ', model%Ntab(TabelNr,IndexFirstDateTime)
         write(idebug,*) ' first position data     ', model%Ntab(TabelNr,IndexFirstDataValue)
         write(idebug,*) ' Day Month Year Hour Min Sec DataValue'
         Do i = 1,model%NTab(TabelNr,IndexNTabElements)
            j = model%Ntab(TabelNr,IndexFirstDateTime)  + i-1
            k = model%Ntab(TabelNr,IndexFirstDataValue) + i-1
            write(idebug,'(6I5,999F10.3)') &
               (model%TableDateTime(j,idum),idum=1,6), &
               (model%TableData (k+idum2*model%NTab(TabelNr,IndexNTabElements)),idum2=0,NrColumns-1)
         Enddo

         write(idebug,*) ' All Table characteristics'
         write(idebug,*) ' Tablength FirstDatePos FirstDataPos Interpolation Period LastIndex'
         Do i = 1,model%FirstFreeTable-1
            write(idebug,'(6I9)') (model%NTab(i,idum),idum=1,6)
         Enddo
         write(idebug,*) ' All Table Date/Times '
         Do i = 1,model%FirstFreeDateTime-1
            write(idebug,'(6I5)') (model%TableDateTime(i,idum),idum=1,6)
         Enddo
         write(idebug,*) ' Data array '
         write(idebug,'(10F8.3)') (model%TableData (i),i=1, model%FirstFreeData-1)
      endif

      ReduceTable = .true.
  999 Continue
      Return
      End Function ReduceTable


      Real Function GetFirstTableValue(ModelHandle, TableNr)

!     Return first value from table TableNr

      integer ModelHandle, TableNr
      type (SobekTimeTableStore) , pointer    :: model

      model => NewTablesModelFind_ByHandle (modelHandle)
      GetFirstTableValue = model%TableData(model%NTab(TableNr,IndexFirstDataValue))

      Return
      End function GetFirstTableValue



      Real Function GetLastValue(ModelHandle)

!     Return last value from the table

      integer ModelHandle
      type (SobekTimeTableStore) , pointer    :: model

      model => NewTablesModelFind_ByHandle (modelHandle)
      GetLastValue = model%TableData (model%FirstFreeData-1)

      Return
      End function GetLastValue


      Real Function GetNewValue(ModelHandle, TabelNr, ColumnNr, RowNr, CurrentDate, CurrentTime, &
                                Idebug, Iout1, DateTimeOutsideTable, UseLastIndexFound)

!  Zoekt waarde uit tabel Tabelnr, kolom ColumnNr, voor date/time CurrentDate and CurrentTime
!  Als RowNr<0: eerst bepalen welke index (rijnummer) gebruikt moet worden uit de tabel (whichrow)
!  Als RowNr>0: dan is rijnummer voor kolom 1 gegeven; bij columnr <> 1 alleen index ophogen

      type (SobekTimeTableStore) , pointer    :: model

      Integer, intent(in)  :: ModelHandle, idebug, Iout1
      Integer TabelNr, ColumnNr, RowNr
      type (Date)  CurrentDate
      type (Time)  CurrentTime
      logical DateTimeOutsideTable, UseLastIndexFound, ProgramError

      Integer i, j, k, i1
      Real    Ratio


      model => NewTablesModelFind_ByHandle (modelHandle)

      If (TabelNr .gt. model%MaxTabNr) then
          call SetMessage(LEVEL_FATAL, 'GetNewValue called with table nr which is too high')
      Endif
      If (RowNr .lt. 0) Call SetRownr (ModelHandle, TabelNr, Rownr, CurrentDate, CurrentTime, &
                                       DateTimeOutsideTable, Idebug, UseLastIndexFound)
!     RowNr geeft nu het rijnummer in het Date/Time array

      if (model%NTab(TabelNr,IndexPeriodic) .gt. 0 .and. DateTimeOutsideTable .and. &
          (RowNr .eq. 1 .or. RowNr .eq. model%NTab(TabelNr,IndexNTabElements)) ) then
            Call SetCycRownr (ModelHandle, TabelNr, Rownr, CurrentDate, CurrentTime, Idebug, UseLastIndexFound)
            DateTimeOutsideTable = (RowNr .eq. model%NTab(TabelNr,IndexNTabElements) .or. RowNr .eq. 1)
      endif

      if (idebug .ne. 0) then
         write(idebug,*) ' TabelNr ', TabelNr
         write(idebug,*) ' Column  ', ColumnNr
         write(idebug,*) ' length of table ', model%Ntab(TabelNr,IndexNTabElements)
         write(idebug,*) ' first position datetime ', model%Ntab(TabelNr,IndexFirstDateTime)
         write(idebug,*) ' first position data     ', model%Ntab(TabelNr,IndexFirstDataValue)
         write(idebug,*) '    i    j    k   Day Month Year Hour Min Sec DataValue'
         Do i = 1,model%NTab(TabelNr,IndexNTabElements)
            j = model%Ntab(TabelNr,IndexFirstDateTime ) + i-1
            k = model%Ntab(TabelNr,IndexFirstDataValue) + i-1 + model%Ntab(TabelNr,IndexNTabElements)* (ColumnNr-1)
            write(idebug,'(2I5,I7,6I5,F10.3)') i,j,k, (model%TableDateTime(j,i1),i1=1,6), model%TableData(k)
         Enddo
      Endif

      i = model%Ntab(TabelNr,IndexFirstDataValue) + RowNr-1 + (ColumnNr-1) * model%NTab(TabelNr,IndexNTabElements)
      If (i .gt. model%MaxData) then
         ProgramError = GenerateMessage ( 3 , 'Table Data index too large' , '   ', ' ' , Iout1)
         call SetMessage(LEVEL_FATAL, 'GetNewValue called with table index too high')
      Endif
      if (idebug .ne. 0)  write(idebug,*) ' Index in DataArray ', i, model%TableData(i)

      if (DateTimeOutsideTable) then
         if (model%NTab(TabelNr,IndexPeriodic) .eq. 0) then
!           geen interpolatie mogelijk, want datum/tijd buiten tabel en tabel is niet periodiek
            GetNewValue = model%TableData (i)
         else
            if (model%NTab(TabelNr,IndexInterpolationCode) .eq. 1) then
      !        periodieke tabel, maar blok interpolatie = geen interpolatie
               GetNewValue = model%TableData (i)
            else
      !        periodieke tabel, linear interpolation
!              lineaire interpolatie tussen data waarde i en i+1, op basis van CurrentDate/Time en Julian dates
               Call SetRatio (ModelHandle, TabelNr, RowNr, CurrentDate, CurrentTime, &
                              Idebug, Iout1, i, j, Ratio)
               i1 = i + 1
               GetNewValue = (1-Ratio) * model%TableData (i) + Ratio * model%TableData(i1)
            endif
         endif
      elseif (model%NTab(TabelNr,IndexInterpolationCode) .eq. 1) then
!        blok interpolatie = geen interpolatie
         GetNewValue = model%TableData (i)
      elseif (model%NTab(TabelNr,IndexInterpolationCode) .eq. 0) then
!        lineaire interpolatie tussen data waarde i en i+1, op basis van CurrentDate/Time en Julian dates
         Call SetRatio (ModelHandle, TabelNr, RowNr, CurrentDate, CurrentTime, &
                        Idebug, Iout1, i, j, Ratio)
         i1 = i + 1
         GetNewValue = (1-Ratio) * model%TableData (i) + Ratio * model%TableData(i1)
      endif

      Return
      End Function GetNewValue



      Subroutine SetRatio(ModelHandle, TabelNr, RowNr, CurrentDate, CurrentTime, &
                          Idebug, Iout1, i, j, Ratio)

!  Zoekt waarde uit tabel Tabelnr, kolom ColumnNr, voor date/time CurrentDate and CurrentTime
!  Als RowNr<0: eerst bepalen welke index (rijnummer) gebruikt moet worden uit de tabel (whichrow)
!  Als RowNr>0: dan is rijnummer voor kolom 1 gegeven; bij columnr <> 1 alleen index ophogen

      type (SobekTimeTableStore) , pointer    :: model

      Integer, intent(in)  :: ModelHandle, Idebug, Iout1
      Integer TabelNr, RowNr
      type (Date)  CurrentDate
      type (Time)  CurrentTime
      logical  ProgramError

      Integer i, j, i1, j1
      Integer IDate1, IDate2, IDate3
      Integer ITime1, ITime2, Itime3
      Double precision JulianDate1, JulianDate2, JulianDate3, Julian
      Real    Ratio

      model => NewTablesModelFind_ByHandle (modelHandle)

!     lineaire interpolatie tussen data waarde i en i+1, op basis van CurrentDate/Time en Julian dates
      i1 = i + 1
      If (i1 .gt. model%MaxData) then
         ProgramError = GenerateMessage ( 3 , 'Table Data index too large' , '   ', ' ' , Iout1)
         call SetMessage(LEVEL_FATAL, 'GetNewValue called with table data index which is too high')
      Endif
      j = model%Ntab(TabelNr,IndexFirstDateTime) + RowNr-1
      j1 = j+1
      Idate1 = SetDate(model%TableDateTime(j,IndexYear), model%TableDateTime(j,IndexMonth), model%TableDateTime(j,IndexDay))
      Itime1 = SetTime(model%TableDateTime(j,IndexHour), model%TableDateTime(j,IndexMinute), model%TableDateTime(j,IndexSecond))
      JulianDate1= Julian (idate1,itime1)
      Idate2 = SetDate(model%TableDateTime(j1,IndexYear), model%TableDateTime(j1,IndexMonth), model%TableDateTime(j1,IndexDay))
      Itime2 = SetTime(model%TableDateTime(j1,IndexHour), model%TableDateTime(j1,IndexMinute), model%TableDateTime(j1,IndexSecond))
      JulianDate2 = Julian (idate2,itime2)
      Idate3 = SetDate(CurrentDate%Year, CurrentDate%Month, CurrentDate%Day)
      Itime3 = SetTime(CurrentTime%Hour, CurrentTime%Minute, CurrentTime%Second)
      JulianDate3 = Julian (idate3,itime3)
      Ratio = (JulianDate3-JulianDate1) / (JulianDate2-JulianDate1)
      if (JulianDate3 .lt. 0 .or. Ratio .gt. 1 .or. Ratio .lt. 0) then
          if (idebug .ne. 0)  write(idebug,*) ' Error: Julian Date negative or Ratio invalid; put ratio=0'
          Ratio = 0.0
      endif
      if (idebug .ne. 0) then
         write(idebug,*) ' Interpolatie ', i, model%TableData(i), i1, model%TableData(i1)
         write(idebug,*) ' JulianDates  ', JulianDate1, JulianDate2, JulianDate3
         write(idebug,*) ' Ratio        ', Ratio
      endif


      Return
      End Subroutine SetRatio



      Subroutine  SetRowNr (ModelHandle, TabelNr, RowNr, CurrentDate, &
                            CurrentTime, DateTimeOutsideTable, Idebug, UseLastIndexFound)

!     Find RowNumber RowNr for CurrentDate and CurrentTime in tabel TabelNr
!     Uitgangspunt: tabel is netjes ingevoerd, dwz op toenemende volgorde van datum/tijd

      type (SobekTimeTableStore) , pointer    :: model

      Integer, intent(in)  :: ModelHandle, idebug
      Integer TabelNr, RowNr
      type (Date)  CurrentDate, RowDate, LowerDate, UpperDate
      type (Time)  CurrentTime, RowTime, LowerTime, UpperTime
      logical DateTimeOutsideTable, UseLastIndexFound

      Integer LengthTable, FirstPos, Ipos, LastPos
      Logical Ready


      model => NewTablesModelFind_ByHandle (modelHandle)

      DateTimeOutsideTable = .false.
      LengthTable = model%NTab(TabelNr, IndexNTabElements)
      FirstPos    = model%NTab(TabelNr, IndexFirstDateTime)
      IPos        = FirstPos
      LastPos     = FirstPos + LengthTable -1
      Ready       = .false.
! even hard aanzetten
!     Idebug=42

      LowerDate%year   = model%TableDateTime (Firstpos,IndexYear)
      LowerDate%month  = model%TableDateTime (Firstpos,IndexMonth)
      LowerDate%day    = model%TableDateTime (Firstpos,IndexDay )
      LowerTime%hour   = model%TableDateTime (Firstpos,IndexHour)
      LowerTime%minute = model%TableDateTime (Firstpos,IndexMinute)
      LowerTime%second = model%TableDateTime (Firstpos,IndexSecond)

      UpperDate%year   = model%TableDateTime (Lastpos,IndexYear)
      UpperDate%month  = model%TableDateTime (Lastpos,IndexMonth)
      UpperDate%day    = model%TableDateTime (Lastpos,IndexDay )
      UpperTime%hour   = model%TableDateTime (Lastpos,IndexHour)
      UpperTime%minute = model%TableDateTime (Lastpos,IndexMinute)
      UpperTime%second = model%TableDateTime (Lastpos,IndexSecond)

! First test if currentDate, CurrentTime within range of table; if not, Rownr will be either first or last row.
      if ((NcalcDate(currentDate) .lt. NcalcDate(lowerDate)) .or. &
           ((NcalcDate(currentDate) .eq. NcalcDate(lowerDate)) .and. &
            (NcalcTime(CurrentTime) .lt. NCalcTime(LowerTime)) ) ) then
         RowNr = 1
         Ready = .true.
      end if

      if ((NcalcDate(CurrentDate) .gt. NcalcDate(UpperDate)) .or. &
           ((NcalcDate(CurrentDate) .eq. NcalcDate(UpperDate)) .and. &
            (NcalcTime(CurrentTime) .ge. NCalcTime(UpperTime)) ) ) then
          RowNr = LengthTable
          Ready = .true.
      end if
      DateTimeOutsideTable = Ready
      if (idebug .ne. 0)  then
        write(idebug,*) ' SetRowNr called with RowNr=', RowNr
        write(idebug,*) ' DateTimeOutsideTable=',DateTimeOutsideTable
      endif

      If (.not. Ready) then

         if (idebug .ne. 0)  write(idebug,*) ' Vooraf: FirstPos LastPos LastIndexFound', &
                                              FirstPos, LastPos, Ipos, model%Ntab(TabelNr,IndexLastRowUsed)

! Search only if CurrentDate and Time within table range; At the moment always simple linear search
!!       If (NTab(TabelNr,6) .le. 0 .or. NTab(TabelNr,5) .gt. 0 .or. .not. UseLastIndexFound) then
!!          not yet a search done, or table is cyclic
         If (model%NTab(TabelNr,IndexLastRowUsed) .le. 0 .or. .not. UseLastIndexFound) then
!           not yet a search done
            model%NTab (TabelNr,IndexLastRowUsed) = 0
         Else
!           already a search done; resulting row index was nTabNr(TableNr,6)
            FirstPos         = FirstPos + model%NTab(TabelNr,IndexLastRowUsed) -1
            LowerDate%year   = model%TableDateTime (Firstpos,IndexYear)
            LowerDate%month  = model%TableDateTime (Firstpos,IndexMonth)
            LowerDate%day    = model%TableDateTime (Firstpos,IndexDay )
            LowerTime%hour   = model%TableDateTime (Firstpos,IndexHour)
            LowerTime%minute = model%TableDateTime (Firstpos,IndexMinute)
            LowerTime%second = model%TableDateTime (Firstpos,IndexSecond)
            ! check for cyclic table
            ! if lastindexfound >= lengthtable: start search at beginning of table
            ! if firstpos is not after current date; if so, start at beginning of table
            if (model%NTab(TabelNr,IndexPeriodic) .gt. 0) then
                if (model%NTab(TabelNr,IndexLastRowUsed) .ge. LengthTable .or. &
                     ((NcalcDate(currentDate) .lt. NcalcDate(lowerDate)) .or. &
                      ((NcalcDate(currentDate) .eq. NcalcDate(lowerDate)) .and. &
                       (NcalcTime(CurrentTime) .lt. NCalcTime(LowerTime)) ) ) ) then
                   FirstPos    = model%NTab(TabelNr, IndexFirstDateTime)
                   LowerDate%year   = model%TableDateTime (Firstpos,IndexYear)
                   LowerDate%month  = model%TableDateTime (Firstpos,IndexMonth)
                   LowerDate%day    = model%TableDateTime (Firstpos,IndexDay )
                   LowerTime%hour   = model%TableDateTime (Firstpos,IndexHour)
                   LowerTime%minute = model%TableDateTime (Firstpos,IndexMinute)
                   LowerTime%second = model%TableDateTime (Firstpos,IndexSecond)
                   model%NTab(TabelNr,IndexLastRowUsed) = 0
                endif
            endif

         Endif
         Ipos        = FirstPos
         UpperDate   = LowerDate
         UpperTime   = LowerTime
         RowNr = max (model%Ntab(TabelNr,IndexLastRowUsed)-1, 0)
         Do while (Ipos .lt. Lastpos .and. .not. Ready)
            RowNr = RowNr + 1
            LowerDate = UpperDate
            LowerTime = UpperTime
            RowDate = LowerDate
            RowTime = LowerTime
            UpperDate%year   = model%TableDateTime (Ipos+1,IndexYear)
            UpperDate%month  = model%TableDateTime (Ipos+1,IndexMonth)
            UpperDate%day    = model%TableDateTime (Ipos+1,IndexDay )
            UpperTime%hour   = model%TableDateTime (Ipos+1,IndexHour)
            UpperTime%minute = model%TableDateTime (Ipos+1,IndexMinute)
            UpperTime%second = model%TableDateTime (Ipos+1,IndexSecond)
            If ( NcalcDate(currentDate) .gt. NcalcDate(UpperDate) ) then
!               nog minstens 1 element verder
            Elseif (  NcalcDate(currentDate) .eq. NcalcDate(UpperDate)  .and. &
                      NcalcTime(CurrentTime) .ge. NCalcTime(UpperTime) ) then
!               nog minstens 1 element verder
            Else
                Ready = .true.
            Endif
            Ipos = Ipos + 1
         Enddo
      Endif

! Zet de laatst gevonden index, voor het geval van een niet periodieke tabel of als de datum in het tabelbereik valt
!     if (NTab(TabelNr,5) .eq. 0 .or. .not. DateTimeOutsideTable) NTab(TabelNr,6) = RowNr
      if (.not. DateTimeOutsideTable) model%NTab(TabelNr,IndexLastRowUsed) = RowNr

      if (idebug .ne. 0) then
         Ipos  = max (model%NTab(TabelNr,IndexFirstDateTime), model%NTab(TabelNr,IndexFirstDateTime) + model%NTab(TabelNr,IndexLastRowUsed) - 1)
         write(idebug,*) ' End of SetRowNr'
         write(idebug,*) ' FirstPos LastPos IPos LastIndexFound', FirstPos, LastPos, Ipos, model%Ntab(TabelNr,IndexLastRowUsed)
         write(idebug,*) ' CurrentDate ', CurrentDate%day, CurrentDate%month, CurrentDate%year
         write(idebug,*) ' CurrentTime ', CurrentTime%hour, CurrentTime%minute, CurrentTime%second
         write(idebug,*) ' FoundDate ', model%TableDateTime(ipos,1), model%TableDateTime(ipos,2), model%TableDateTime(ipos,3)
         write(idebug,*) ' FoundTime ', model%TableDateTime(ipos,4), model%TableDateTime(ipos,5), model%TableDateTime(ipos,6)
      Endif


      Return
      End Subroutine SetRowNr




      Subroutine  SetCycRowNr (ModelHandle, TabelNr, RowNr, CurrentDate, CurrentTime, Idebug, UseLastIndexFound)

!     Find RowNumber RowNr voor een Cyclische tabel, met Date buiten de tabel
!     Bij aanroep geldt RowNr=1 of RowNr=laatste rij, zodat we al weten aan welke kant de datum erbuiten valt

      type (SobekTimeTableStore) , pointer    :: model

      Integer, intent(in)  :: ModelHandle, idebug
      Integer TabelNr, RowNr
      type (Date)  CurrentDate
      type (Time)  CurrentTime
      Logical UseLastIndexFound

      Integer IDate1, IDate2, IDate3, iYear, IMonth
      Integer ITime1, ITime2, Itime3
      Double precision JulianDate1, JulianDate2, JulianDate3, Julian, DCurrent, DEnd, DPeriod

      Integer Ipos, j, j1
      Logical Doorgaan, OutSide, Annual


      model => NewTablesModelFind_ByHandle (modelHandle)

! even hard aanzetten
!     Idebug=42

!     Check nog eens, want nu zoeken in een periodieke/cyclische tabel !!!!
!     bepaal Julian dates van 1.Begindatum tabel, 2. Einddatum tabel, 3. Huidige datum/tijd
      j  = model%Ntab(TabelNr,IndexFirstDateTime)
      Idate1 = SetDate(model%TableDateTime(j,IndexYear), model%TableDateTime(j,IndexMonth), model%TableDateTime(j,IndexDay))
      Itime1 = SetTime(model%TableDateTime(j,IndexHour), model%TableDateTime(j,IndexMinute), model%TableDateTime(j,IndexSecond))
      JulianDate1= Julian (idate1,itime1)

      j1 = model%Ntab(TabelNr,IndexFirstDateTime) + model%NTab(TabelNr,IndexNTabElements)-1
      Idate2 = SetDate(model%TableDateTime(j1,IndexYear), model%TableDateTime(j1,IndexMonth), model%TableDateTime(j1,IndexDay))
      Itime2 = SetTime(model%TableDateTime(j1,IndexHour), model%TableDateTime(j1,IndexMinute), model%TableDateTime(j1,IndexSecond))
      JulianDate2 = Julian (idate2,itime2)

      Idate3 = SetDate(CurrentDate%Year, CurrentDate%Month, CurrentDate%Day)
      Itime3 = SetTime(CurrentTime%Hour, CurrentTime%Minute, CurrentTime%Second)
      JulianDate3 = Julian (idate3,itime3)
      IYear  = CurrentDate%Year
      IMonth = CurrentDate%Month

      if (idebug .ne. 0) then
         write(idebug,*) ' SetCycRowNr called with RowNr=', RowNr
         write(idebug,*) ' CurrentDate ', CurrentDate%day, CurrentDate%month, CurrentDate%year
         write(idebug,*) ' CurrentTime ', CurrentTime%hour, CurrentTime%minute, CurrentTime%second
         write(idebug,'(A)')        ' Julian Dates   StartTable   EndTable     CurrentDate'
         write(idebug,'(A,3F15.6)') ' Julian Dates', JulianDate1, JulianDate2, JulianDate3
      Endif

      doorgaan = .true.
      DEnd     =  Juliandate2-JulianDate1
      DCurrent =  Juliandate3-JulianDate1
      Annual   =  .false.
      if (model%Ntab(TabelNr,IndexPeriodic) .eq.  31536000)  Annual = .true.
      DPeriod = model%NTab(TabelNr,IndexPeriodic) /86400.0D0
      if (idebug .ne. 0) then
         write (idebug,*) ' Set Dperiod =', Dperiod, model%NTab(TabelNr,IndexPeriodic), 86400
         write (idebug,*) ' Dcurrent  DPeriod DEnd =', DCurrent, DPeriod, DEnd
      endif

!     bepaal datum in tabel door verschuiven;
!     Let op dat bij periode van 1 jaar voor schrikkeljaar aparte acties plaatsvinden (extra dag!)
      if (RowNr .eq. 1) then
         ! datum valt voor begin tabel;
         Do while (doorgaan)
            DCurrent = DCurrent + DPeriod
            If (annual) then
                if (idebug .ne. 0) write(Idebug,*) ' check annual'
                If (LeapYear(Iyear) .and. IMonth .le. 2) then
                   Dcurrent = Dcurrent + 1.0D0     ! extra dag erbij tellen
                elseIf (LeapYear(Iyear+1) .and. IMonth .ge. 3) then
                   Dcurrent = Dcurrent + 1.0D0     ! extra dag erbij tellen
                endif
                IYear = IYear + 1
            Endif
            Doorgaan = (DCurrent .lt. 0)
            If (Idebug .ne. 0) write(Idebug,*) ' Doorgaan1?=',Doorgaan, DCurrent, DEnd, DPeriod
         Enddo
      else
         ! datum valt na einde tabel
         Do while (doorgaan)
            DCurrent = DCurrent - DPeriod
            If (annual) then
                if (idebug .ne. 0) write(Idebug,*) ' check annual'
                If (LeapYear(Iyear) .and. IMonth .ge. 3) then
                   Dcurrent = Dcurrent - 1.0D0     ! extra dag eraf halen
                elseIf (LeapYear(Iyear-1) .and. IMonth .le. 2) then
                   Dcurrent = Dcurrent - 1.0D0     ! extra dag eraf halen
                endif
                IYear = IYear - 1
            Endif
            Doorgaan = (DCurrent .gt. DEnd)
            If (Idebug .gt. 0) write(Idebug,*) ' Doorgaan2?=',Doorgaan, DCurrent, DEnd, DPeriod
         Enddo
      Endif

! Nogmaals Checken of Icurrent nu in de tabel valt;
! Check uitgezet, want we laten foutjes in de tabel toe!! zie GetTable Check periode
!     Doorgaan = (DCurrent .ge. 0 .and. DCurrent .le. DEnd)
!     if (doorgaan) then
!     else
!        call SetMessage(LEVEL_FATAL, 'Internal Search Error Periodic Table')
!     Endif

      if (idebug .ne. 0) then
         write (idebug,*) ' After Check annual: Dcurrent  DPeriod =', DCurrent, DPeriod
      endif

      JulianDate3 = JulianDate1 + DCurrent
      Call GregorSbk (JulianDate3, CurrentDate%year, CurrentDate%month , CurrentDate%day, &
                                 CurrentTime%hour  , CurrentTime%minute, CurrentTime%second)
      if (idebug .ne. 0) then
         Write(idebug,*) ' JulianDate3', JulianDate3, ' Using Gregor found CurrentDate/Time: '
         write(idebug,*) ' CurrentDate ', CurrentDate%day, CurrentDate%month, CurrentDate%year
         write(idebug,*) ' CurrentTime ', CurrentTime%hour, CurrentTime%minute, CurrentTime%second
      endif
      Call SetRownr (modelHandle, TabelNr, Rownr, CurrentDate, CurrentTime, Outside, Idebug, UseLastIndexFound)

      if (idebug .ne. 0) then
         IPos         = model%NTab(TabelNr,IndexFirstDateTime) + model%NTab(TabelNr,IndexLastRowUsed) - 1
         if (IPos .le. 0) ipos = 1
         write(idebug,*) ' End of SetCycRowNr'
         write(idebug,*) ' FoundDate ', model%TableDateTime(ipos,IndexDay), &
                                          model%TableDateTime(ipos,IndexMonth), model%TableDateTime(ipos,IndexYear)
         write(idebug,*) ' FoundTime ', model%TableDateTime(ipos,IndexHour), &
                                          model%TableDateTime(ipos,IndexMinute), model%TableDateTime(ipos,IndexSecond)
      Endif


      Return
      End Subroutine SetCycRowNr


      Logical Function MakeConstTable (ModelHandle, NrColumns, TableNr, cnst1, cnst2, Idebug)

! Eelco Verschelling: routine om tabel met constante te vullen, 2 rijen.
! MAKE table with id TableNr, NrColumns data fields (1 or 2) filled with c1 (and c2)
! Put result in Table arrays; table number = TableNr


      type (SobekTimeTableStore) , pointer    :: model

      Integer, intent(in)  :: ModelHandle, idebug
      Integer      TableNr
      Integer      NrColumns
      Integer      InterpolationCode, PeriodCode
      Real         cnst1, cnst2
      Integer      i,j,k, idum, idum2, Allocation_Error


      MakeConstTable = .false.
      InterpolationCode = 1
      PeriodCode        = 31536000

      model => NewTablesModelFind_ByHandle (modelHandle)

      Allocate   ( RLocal (2, NrColumns), Stat=Allocation_Error )
      If (Allocation_Error .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Error allocating arrays in subroutine MakeConstTable')
      Endif
      if (model%FirstFreeTable .lt. model%MaxTabnr) then
          TableNr = model%FirstFreeTable
          model%FirstFreeTable = model%FirstFreeTable + 1
          model%NTab (TableNr,IndexNTabElements  )    = 0
          model%NTab (TableNr,IndexFirstDateTime )    = model%FirstFreeDateTime
          model%NTab (TableNr,IndexFirstDataValue)    = model%FirstFreeData
          model%NTab (TableNr,IndexInterpolationCode) = InterpolationCode   ! Interpolatie volgens PDIN
          model%NTab (TableNr,IndexPeriodic)          = PeriodCode          ! Periode volgens PDIN
          model%NTab (TableNr,IndexLastRowUsed)       = 0                   ! last index used
          model%TableDateTime (model%FirstFreeDateTime,IndexDay)    = 1
          model%TableDateTime (model%FirstFreeDateTime,IndexMonth)  = 1
          model%TableDateTime (model%FirstFreeDateTime,IndexYear)   = 1990
          model%TableDateTime (model%FirstFreeDateTime,IndexHour)   = 0
          model%TableDateTime (model%FirstFreeDateTime,IndexMinute) = 0
          model%TableDateTime (model%FirstFreeDateTime,IndexSecond) = 0
          model%FirstFreeDateTime = model%FirstFreeDateTime + 1
          Rlocal(1,1) = cnst1
          if (NrColumns .eq. 2) Rlocal(1,2) = cnst2
          model%FirstFreeData  = model%FirstFreeData + NrColumns
          model%TableDateTime (model%FirstFreeDateTime,IndexDay)    = 31
          model%TableDateTime (model%FirstFreeDateTime,IndexMonth)  = 12
          model%TableDateTime (model%FirstFreeDateTime,IndexYear)   = 1990
          model%TableDateTime (model%FirstFreeDateTime,IndexHour)   = 23
          model%TableDateTime (model%FirstFreeDateTime,IndexMinute) = 59
          model%TableDateTime (model%FirstFreeDateTime,IndexSecond) = 59
          model%FirstFreeDateTime = model%FirstFreeDateTime + 1
          Rlocal(2,1) = cnst1
          if (NrColumns .eq. 2) Rlocal(2,2) = cnst2
          model%FirstFreeData  = model%FirstFreeData + NrColumns
      Endif

! zet data van array RLocal in global array
      model%Ntab(TableNr,IndexNTabElements) = 2
      Idum = model%Ntab(TableNr,IndexFirstDataValue) -1
      Do i=1,NrColumns
         Do j=1,2
            idum  = idum+1
            model%TableData (idum) = Rlocal(j,i)
         Enddo
      Enddo
      if (idebug .ne. 0) then
         write(idebug,*) ' TableNr ', TableNr
         write(idebug,*) ' length of table ', model%Ntab(TableNr,IndexNTabElements)
         write(idebug,*) ' first position datetime ', model%Ntab(TableNr,IndexFirstDateTime)
         write(idebug,*) ' first position data     ', model%Ntab(TableNr,IndexFirstDataValue)
         write(idebug,*) ' Day Month Year Hour Min Sec DataValue'
         Do i = 1,model%NTab(TableNr,IndexNTabElements)
            j = model%Ntab(TableNr,IndexFirstDateTime) + i-1
            k = model%Ntab(TableNr,IndexFirstDataValue) + i-1
            write(idebug,'(6I5,999F10.3)') &
                 (model%TableDateTime(j,idum),idum=1,6), &
                 (model%TableData (k+idum2*model%NTab(TableNr,1)),idum2=0,NrColumns-1)
         Enddo

         write(idebug,*) ' All Table characteristics'
         write(idebug,*) ' Tablength FirstDatePos FirstDataPos Interpolation Period LastIndex'
         Do i = 1,model%FirstFreeTable-1
            write(idebug,'(6I9)') (model%NTab(i,idum),idum=1,6)
         Enddo
         write(idebug,*) ' All Table Date/Times '
         Do i = 1,model%FirstFreeDateTime-1
            write(idebug,'(6I5)') (model%TableDateTime(i,idum),idum=1,6)
         Enddo
         write(idebug,*) ' Data array '
         write(idebug,'(10F8.3)') (model%TableData (i),i=1, model%FirstFreeData-1)
      endif

      Deallocate   ( RLocal )

      MakeConstTable = .true.

      Return
      End Function MakeConstTable


  ! private operations


      Integer Function SetDate(Year, Month, Day)
!     Sets output integer equal to yearmoday,
!     e.g. for September 19 in the year 2002 this function returns the value: 20020919
      Integer Year, Month, Day

      SetDate = 10000*Year + 100*Month + Day

      Return
      End Function SetDate


      Integer Function SetTime(Hour, Minute, Second)
      Integer Hour, Minute, Second

!     Sets output integer equal to hrmnsc
!     e.g. for 13 hours 59 minutes and 3 seconds is returns the value 135903
      SetTime = 10000*Hour + 100*Minute + Second

      Return
      End Function SetTime


      Logical Function GenerateMessage ( errtyp, messag, id, nm, iout1)
!=======================================================================
!  Generate message in case of errors
!-----------------------------------------------------------------------
! Variable Type Length I/O Description
! errtyp   i    1      i   Type of message
!                          1 = information
!                          2 = warning
!                          3 = error
! messag   c    1      i   Text of message
! id       c    1      i   Id of object related to message
! nm       c    1      i   Name of object related to message
! indeks   i    1      i   Nr of object related to message
!***********************************************************************
! CVS log information:
!
!
!***********************************************************************

      character(len=*) messag, id    , nm
      integer       errtyp, iout1

      logical stopflag

      stopflag = .false.

      if (errtyp .eq. 1) then
        call setMessage(LEVEL_INFO, messag)
      elseif (errtyp .eq. 2) then
        call setMessage(LEVEL_WARN, messag)
      elseif (errtyp .eq. 3) then
        stopflag = .true.
        call setMessage(LEVEL_ERROR, messag)
      else
      endif

      if (id  .ne. ' ') then
         call SetMessage(LEVEL_INFO, 'Object-id  : '//trim(id))
      endif

      if (nm  .ne. ' ') then
         call SetMessage(LEVEL_INFO, 'Object-name: '//trim(nm))
      endif

      GenerateMessage = stopflag

      End Function GenerateMessage



      Subroutine uppcas ( keywrd , keyupp , len )

!     Convert string into upper case

!     Old method: hard coded ASCII codes of a,z,A
!     character*(*) keywrd, keyupp
!     integer len, i, i1
!     do 10 i = 1,len
!         i1 = ichar(keywrd(i:i))
!         if ( i1 .ge. 97 .and. i1 .le. 122 ) then
!             keyupp(i:i) = char(i1-32)
!         else
!             keyupp(i:i) = keywrd(i:i)
!         endif
!  10 continue

!     new method: not hard-coded ASCII codes, but using relative position of a, z,A,Z

      character(len=*) keywrd, keyupp
      integer len, i, j, smalla, smallz, biga, offset

      smalla = ICHAR('a')
      smallz = ICHAR('z')
      biga   = ICHAR('A')

      offset = biga - smalla

      do i=1,len
         j = ichar (keywrd(i:i))
         if ( (j .ge. smalla) .and. (j .le. smallz) ) then
            j = j + offset
            keyupp(i:i) = char (j)
         else
            keyupp(i:i) = keywrd(i:i)
         endif
      enddo

      Return
      End Subroutine UppCas



      Subroutine lowcas ( keywrd , keylow , len )

!     Convert string into lower case

!      character*(*) keywrd, keylow
!      integer len, i, i1
!      do 10 i = 1,len
!          i1 = ichar(keywrd(i:i))
!          if ( i1 .ge. 65 .and. i1 .le. 90 ) then
!              keylow(i:i) = char(i1+32)
!          else
!              keylow(i:i) = keywrd(i:i)
!          endif
!   10 continue

!     new method: not hard-coded ASCII codes, but using relative position of a, z,A,Z

      character(len=*) keywrd, keylow
      integer len, i, j, biga, bigz, smalla, offset

      biga   = ICHAR('A')
      bigz   = ICHAR('Z')
      smalla = ICHAR('a')

      offset = smalla - biga

      do i=1,len
         j = ichar (keywrd(i:i))
         if ( (j .ge. biga) .and. (j .le. bigz) ) then
            j = j + offset
            keylow(i:i) = char (j)
         else
            keylow(i:i) = keywrd(i:i)
         endif
      enddo

      Return
      End Subroutine LowCas



  integer function NcalcDate(CurrentDate)

    ! variables
    type (Date) CurrentDate

    Integer IDate, Itime
    Double precision JulianDate, Julian

    Idate = 10000*CurrentDate%Year + 100*CurrentDate%Month + CurrentDate%Day
    Itime = 0
    JulianDate = Julian (idate, Itime)

    NCalcDate = INT( JulianDate )

  return
  end function NCalcDate




  Integer function NCalcTime(cTime)
    ! calculates # seconds sice 00:00:00

    ! constants
    Integer, parameter :: secPerHour = 3600
    Integer, parameter :: secPerMinute = 60

    !variables
    type (Time) cTime

    ! conversion characterstring to integer
    NCalcTime = (cTime%hour * secPerHour) + (cTime%minute * secPerMinute) + (cTime%second)
  return
  end function NcalcTime


  Logical function LeapYear(IYear)
! Functie bepaalt of jaar IYEAR een schrikkeljaar is

    Integer IYear
    Logical Leap

    Leap = .false.
    if ( modulo(Iyear, 4) .eq. 0) then
         Leap = .true.   ! als jaar deelbaar door 4 ==> schrikkeljaar
         if (modulo(Iyear, 100) .eq. 0) then
            Leap = .false. ! als ook deelbaar door 100, dan juist geen schrikkeljaar
            if (modulo(Iyear, 400) .eq. 0)  Leap = .true. ! maar als ook deelbaar door 400, dan juist wel
         end if
    end if

    LeapYear = leap

  return
  end function LeapYear





  !  Subroutine WriteMessage (Iout1, Messg)
  !
  !  INTEGER         iOut1
  !  CHARACTER(999)  MESSG
  !
  !  if (iOut1 .ne. 0)  then
  !     WRITE(IOUT1,'(A)') MESSG(1:Len_Trim(Messg))
  !  endif
  !  WRITE(*,'(A)') MESSG(1:Len_Trim(Messg))
  !
  !Return
  !End Subroutine WriteMessage
  !

end module NewTables

