!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! DIO-streams: f90-version of streams for Delft-IO
!!!
!!! (c) Deltares, apr 2001
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module dio_streams

use dio_ds_config
use dio_shm


implicit none


!!
!! Types/definitions
!!

integer, parameter :: DIO_HANDLE_INVALID = -1

!
! Time conversion constant
!
integer, parameter :: DioDayToSec = 86400


integer            :: maxStreamUnit = -214748300
integer            :: minStreamUnit = 214748300

!!
!! Definition of external data types
!!

!
! Name / unit for each stream
!
type DioStreamType

    character(DioMaxStreamLen) :: name   ! stream name
    integer           :: id               ! stream id

    character(5)      :: mode             ! stream mode 'r' | 'w'
    logical           :: synched          ! stream synchronized?
    integer           :: streamType       ! type of stream

    integer           :: lun              ! file unit (ASCII/Binary/HIS)
    character(20)     :: form             ! stream format (ASCII/Binary/HIS)

    integer           :: nefFileHandle    ! Nefis file handle

    logical           :: opened           ! stream opened succesfully?
    logical           :: connected        ! stream connected?


    !
    ! streamtype for dataset automatically detected
    !
    logical           :: autoStream    ! stream Created automatically

    !
    ! shared mem comm.
    !
    type(DsShmHandle), pointer    :: shmHandle

end type DioStreamType


!!
!! Declaration of internal data
!!

!!
!! Error Message Storage
!!
integer, parameter                     :: DioMaxErrMsgLen = 256
integer, private                       :: lastErrorNr  = 0
character(Len=DioMaxErrMsgLen), private:: lastErrorMsg = ''
logical, private                       :: logMsgToFile = .true.


!!
!! Declaration of interface functions
!!

interface DioStreamCreate
    module procedure DioStreamCreate
    module procedure DioStreamCreateSynched
    module procedure DioStreamCreateAuto
end interface
     
interface DioStreamError
    module procedure DioStreamError
    module procedure DioStreamError1Arg
    module procedure DioStreamError2Args
    module procedure DioStreamError3Args
end interface


!******************************************************************************
!* STREAM COLLECTION FOR F77 INTERFACE (integer handles = indices)
!******************************************************************************

integer, parameter, private :: dioMaxStreams  = 50  ! max #streams
integer, private            :: nStreams = 0         ! actual #streams

type(DioStreamType), dimension(dioMaxStreams), target :: StreamData


contains


!******************************************************************************
!* PUBLIC FUNCTIONS on STREAM COLLECTION FOR F77 INTERFACE 
!******************************************************************************

function DioStreamNewF77Handle() result(retVal)

    ! return value
    integer  :: retVal  ! >0 : success

    ! locals
    integer  :: p ! loop counter

    retVal = 0
    if (nStreams < dioMaxStreams ) then
        nStreams = nStreams + 1
        StreamData(nStreams) % id = nStreams
        retVal = nStreams
    else
        do p = 1 , nStreams
            if ( StreamData(p) % id == -1 ) then
                StreamData(p) % id = p
                retVal = p
                exit
            endif 
        enddo
    endif

    if ( retVal == 0 ) then
        call DioStreamError(405, 'DioStreamNewF77Handle: Out of PLT-handles')
    endif

end function DioStreamNewF77Handle


function DioStreamF77HandleIsValid(handle) result(retVal)

    ! return value
    logical :: retVal   ! .true. : success

    ! arguments
    integer  :: handle  ! stream-handle in F77 array

    ! locals
    character(Len=DioMaxStreamLen) :: errorString

    ! body

    retVal = .false.
    if ( handle > 0 .and. handle <= nStreams .and. &
                StreamData(handle) % id == handle ) then
        retVal = .true.
    else
        write(errorString, '(A,A,I4)' ) 'Invalid Stream Handle', ': ', handle
        call DioStreamError(406, errorString)
    endif

end function DioStreamF77HandleIsValid


subroutine DioStreamReleaseF77Handle(handle)

    ! arguments
    integer  :: handle  ! stream-handle in F77 array

    if (DioStreamF77HandleIsValid(handle)) then
        StreamData(handle) % id = -1
    endif

end subroutine DioStreamReleaseF77Handle


!
! provide pointer to stream to F77-interface functions
!

function DioStreamGetF77Data(streamID) result(stream)

    ! return value

    type(DioStreamType), pointer :: stream      ! pointer to stream

    ! arguments

    integer                      :: streamID    ! stream id (= index) 

    ! body

    nullify(stream)
    
    if ( streamId .gt. 0 .and. streamId .le. nStreams ) then
        stream => StreamData(streamId)
    endif

end function DioStreamGetF77Data



!!
!! Error functions
!!

subroutine DioUnsetLogErrors()
    logMsgToFile = .false.
end subroutine DioUnsetLogErrors


function DioGetLastError() result(nr)
    integer :: nr
    nr = lastErrorNr
    lastErrorNr = 0
end function DioGetLastError


function DioGetLastErrorMsg() result(msg)
    character(Len=DioMaxErrMsgLen) :: msg
    msg = lastErrorMsg
    lastErrorMsg = ''
end function DioGetLastErrorMsg


!!
!! Implementation of internal functions
!!

function DioStreamUsesLun(stream) result(retVal)

    ! return value

    logical :: retVal                   ! .true. for Ascii/Bin/His
                                        ! .false. otherwise

    ! arguments

    type(DioStreamType)  :: stream      ! handle to stream

    ! body

    retVal = .false.
    if (      stream % streamType .eq. dio_ASCII_stream  &
         .or. stream % streamType .eq. dio_Binary_stream &
         .or. stream % streamType .eq. dio_His_stream    &
         .or. stream % streamType .eq. dio_WQMap_stream) then
        retVal = .true.
    endif

end function DioStreamUsesLun


!
! ErrorMessages
!
! TODO, extend eror message with process name
! 
subroutine DioStreamError(nr, text)
    ! arguments
    integer                        :: nr       ! message nr
    character(Len=*)               :: text     ! message text

#if (defined(WIN32))
    ! locals
    integer                        :: lun      ! file handle
    character(Len=DioMaxStreamLen) :: exePath  ! name and path of executable
    character(Len=DioMaxStreamLen) :: errFile  ! name of errorFile
    integer                        :: ierr     ! open status
    if (logMsgToFile) then
        call GetArg(0, exePath)
        call DioConfGetExeName(exePath, errFile)
        errFile = 'dio-' // trim(errFile) // '-errors.txt'
        open(newunit=lun, file=errFile, position = 'append', iostat=ierr)
        if ( ierr == 0 ) then
            write(lun, '(A,I5,A,A)') 'DioError ', nr, ': ', trim(text)
            close(lun)
        endif
    endif
#else
    write(*, *) 'DioError ', nr, ' : ', trim(text)
#endif
    lastErrorNr  = nr
    lastErrorMsg = text

end subroutine DioStreamError


subroutine DioStreamError1Arg(nr, text, arg1)
    ! arguments
    integer          :: nr    ! message nr
    character(Len=*) :: text  ! message text
    character(Len=*) :: arg1  ! first argument

    call DioStreamError(nr, trim(text) // ' ' // trim(arg1))
    return
end subroutine DioStreamError1Arg


subroutine DioStreamError2Args(nr, text, arg1, arg2)
    ! arguments
    integer          :: nr    ! message nr
    character(Len=*) :: text  ! message text
    character(Len=*) :: arg1  ! first argument
    character(Len=*) :: arg2  ! second argument

    call DioStreamError(nr, trim(text) //' '// trim(arg1) //' '// trim(arg2))
    return
end subroutine DioStreamError2Args


subroutine DioStreamError3Args(nr, text, arg1, arg2, arg3)
    ! arguments
    integer          :: nr    ! message nr
    character(Len=*) :: text  ! message text
    character(Len=*) :: arg1  ! first argument
    character(Len=*) :: arg2  ! second argument
    character(Len=*) :: arg3  ! third argument

    call DioStreamError(nr, trim(text) //' '// trim(arg1) //' '// &
                            trim(arg2) //' '// trim(arg3) )
    return
end subroutine DioStreamError3Args


!
! Determine stream name
!

function DioStreamGetExtension(inName, ext) result(extStart)

    ! return value
    integer                       :: extStart   ! 0: no extension found
                                                ! > 0: start of ext (at '.')

    ! arguments
    character(Len=*), intent(IN)  :: inName     ! incoming stream/dataset name
    character(Len=*), intent(OUT) :: ext        ! extension found

    ! locals
    integer                       :: slashPos   ! pos. of last slash
    character(Len=1)              :: slash      ! slash or backslash
    integer                       :: baseStart  ! start of basename
    integer                       :: dotPos     ! Position of '.<ext>'

#if (defined(WIN32))
    slash = '\'
#else
    slash = '/'
#endif
    
    ! body:
    ! - check if stream has slash, if so, set start of base name
    ! - find '.' in base name
    ! - if found, return extension start and content

    ext = ' '
    extStart = 0

    baseStart = 0
    slashPos = index(inName, slash, .true.)
    if ( slashPos > 0 ) baseStart = slashPos

    dotPos = index( inName(baseStart+1:), '.', .true.)
    if (dotPos > 0 ) then
        extStart = baseStart+dotPos
        ext = inName(extStart:)
    endif

end function DioStreamGetExtension


function DioStreamNameWithExtension(inName, streamType) result(outName)

    ! return value
    character(Len=DioMaxStreamLen) :: outName    ! stream name with extension

    ! arguments
    character(Len=*)               :: inName     ! incoming stream name
    integer                        :: streamType ! type of stream
    integer                        :: extStart   ! Start of extension
    character(Len=DioMaxStreamLen) :: ext        ! stream name extension
    
    ! body:
    ! - check if stream has extension.
    ! - if not, add it, based on stream type
    outName = inName
    extStart = DioStreamGetExtension(outName, ext)
    if (extStart .eq. 0 ) then
        ext = ' '
        select case ( streamType )
            case (dio_ASCII_stream)
                ext = ".txt"
            case (dio_Binary_stream)
                ext = ".dat"
            case (dio_His_stream)
                ext = ".his"
            case (dio_WQMap_stream)
                ext = ".map"
        end select
        if ( ext .ne. ' ' ) then
            outName(len_trim(outName)+1:) = ext
        endif
    endif
end function DioStreamNameWithExtension


!
! Determine stream type from name
!

function DioDetermineStreamTypeFromName(inName) result(streamType)

    ! return value
    integer                        :: streamType ! type of stream

    ! arguments
    character(Len=*)               :: inName     ! stream name with extension
    integer                        :: extStart   ! start of '.<ext>'
    character(Len=DioMaxStreamLen) :: ext        ! stream name extension
    
    ! body:
    ! - check if stream has extension.
    ! - if so, determine type, based on extension

    streamType = Dio_Unknown_stream

    extStart = DioStreamGetExtension(inName, ext)
    if (extStart > 0 ) then
        if ( StringsEqual( CaseInsens, ext, '.txt' ) ) then
            streamType = Dio_ASCII_Stream
        else if ( StringsEqual( CaseInsens, ext, '.dat' ) ) then
            streamType = Dio_Binary_Stream
        else if ( StringsEqual( CaseInsens, ext, '.his' ) ) then
            streamType = Dio_HIS_Stream
        else if ( StringsEqual( CaseInsens, ext, '.map' ) ) then
            streamType = Dio_WQMAP_Stream
        else if ( StringsEqual( CaseInsens, ext, '.shm' ) ) then
            streamType = Dio_SharedMem_Stream
        else if ( StringsEqual( CaseInsens, ext, '.mem' ) ) then
            streamType = Dio_InMem_Stream
        endif
    endif

end function DioDetermineStreamTypeFromName


!
! Connect / Disconnect
!

function DioStreamConnect(stream, alsoForAsync) result(retVal)

    ! return value

    logical              :: retVal        ! .true. succes

    ! arguments

    type(DioStreamType)  :: stream        ! handle to stream
    logical              :: alsoForAsync  ! force connection,
                                          ! even if not synched

#if (defined(DIO_NEFIS_INCLUDED))
    ! externals
    integer, external :: crenef
    ! local variables
    logical :: defExists, datExists
    character(LEN=DioMaxStreamLen) :: defFileName
    character(LEN=DioMaxStreamLen) :: datFileName
    character*1     :: coding, accessType
#endif

    ! local variables

    integer              :: ierr          ! error flag for open statement
    logical              :: openNefis     ! nefis stream
            
    ! body

    retVal = .true.

    if ( stream % synched .or. alsoForAsync ) then
        ierr = -1
        retVal = .false.
        openNefis = .false.
        if ( stream % mode .eq. 'r') then
            if ( stream % streamType .eq. dio_Nefis_Stream ) then
                openNefis = .true.
            else if ( DioStreamUsesLun(stream)) then
#if (defined(WIN32))
                 open (newunit=stream%lun, file=stream % name, action='read', &
                       shared, status='old', form= stream % form, iostat=ierr)
#elif (defined(salford32))
                 open (newunit=stream%lun, file=stream % name, action='read', &
                       access='transparent', share='DENYWR', status='old', form= stream % form, iostat=ierr)
#else
                 open (newunit=stream%lun, file=stream % name, action='read', &
                       status='old', form= stream % form, iostat=ierr)
#endif
            endif
        else
            if ( stream % streamType .eq. dio_Nefis_Stream ) then
                openNefis = .true.
            else if ( DioStreamUsesLun(stream)) then
#if (defined(WIN32))
                 open (newunit=stream%lun, file=stream % name, action='write', &
                       shared, form= stream % form, iostat=ierr)
#elif (defined(salford32))
                 open (newunit=stream%lun, file=stream % name, action='write', &
                       access='transparent', share='DENYWR', form= stream % form, iostat=ierr)
#else
                 open (newunit=stream%lun, file=stream % name, action='write', &
                       form= stream % form, iostat=ierr)
#endif
            endif
        endif
        
        maxStreamUnit = max(stream%lun, maxStreamUnit)
        minStreamUnit = min(stream%lun, minStreamUnit)
        
        if ( openNefis ) then

#if (defined(DIO_NEFIS_INCLUDED))

            coding        = 'N'

            if ( stream % mode == 'r' ) then
                !
                ! Check existence in case of read
                !
                defExists = .true.
                datExists = .true.
                ! check if there are two nefis files
                defFileName = trim(stream % name) // '.def'
                datFileName = trim(stream % name) // '.dat'
                inquire(file=defFileName, exist=defExists)
                inquire(file=datFileName, exist=datExists)

                if ( .not. defExists .or. .not. datExists ) then
                    ! most probably 1 nefis file (nefis 4)
                    defFileName = trim(stream % name) // '.daf'
                    datFileName = defFileName
                endif

                accessType = 'r'

            else

                ! TODO (write file in nefis 4 way, i.e. create 1 file)
                ! Activate after introducing an option in dio-config
                !
                ! defFileName = trim(stream % name) // '.daf'
                ! datFileName = defFileName
                !
                ! For now: create 2 files

                defFileName = trim(stream % name) // '.def'
                datFileName = trim(stream % name) // '.dat'

                accessType = 'c'

            endif

            ierr  = CRENEF (stream % nefFileHandle, datFileName, defFileName, coding, accessType)

#else
            call DioStreamError(191, 'Nefis not supported (stream: ', stream % name, ')')
#endif
        endif

        if ( ierr .eq. 0 ) then
            stream % connected = .true.
            retVal = .true.
        else
            call DioStreamError(101, 'Could not connect to stream', stream % name)
            stream % connected = .false.
        endif
    endif

end function DioStreamConnect


subroutine DioStreamDisconnect(stream, alsoForAsync)

    ! arguments

    type(DioStreamType)  :: stream        ! handle to stream
    logical              :: alsoForAsync  ! force connection,
                                          ! even if not synched
#if (defined(DIO_NEFIS_INCLUDED))
    ! externals
    integer, external :: clsnef
    ! local variables
    integer :: ierr
#endif

    ! body

    if ( stream % synched .or. alsoForAsync ) then
        if ( stream % connected ) then
            if ( DioStreamUsesLun(stream) ) then
                if ( stream % lun .ne. 0 ) then
                    ! call flush(stream % lun)   ! Flush before close seems senseless
                                                 ! but avoids the nfs problem: file is there,
                                                 ! content not yet.
                    close( stream%lun )
                    stream%lun = 0
                endif
            else if(stream % streamType .eq. dio_Nefis_stream) then
#if (defined(DIO_NEFIS_INCLUDED))
                ierr = CLSNEF (stream % nefFileHandle)
                if ( ierr .ne. 0 ) then
                    call DioStreamError(104, 'Could not disconnect from stream', stream % name)
                endif
#else
                call DioStreamError(103, 'Nefis not supported (stream: ', stream % name, ')')
#endif
            endif
            stream % connected = .false.
        endif
    endif

end subroutine DioStreamDisconnect


!!
!! Implementation of external functions
!!

!
! Create a new stream, option to create synchronized
!

function DioStreamCreateSynched(streamType, name, mode, sync) result(resultStream)

    include 'dio-sync.inc'                  ! for synchronization functions

    ! return value

    type(DioStreamType)         :: resultStream  ! handle to DIO stream
    type(DioStreamType)         :: stream      ! handle to DIO stream

    ! arguments

    integer                     :: streamType  ! type of stream
    character(Len=*),intent(in) :: name        ! stream name
    character(Len=*),intent(in) :: mode        ! open mode ('r' | 'w' | 'a'),
                                            ! ( read | write | append )
    logical, intent(in)         :: sync        ! sync stream or not


    logical                     :: success     ! boolean for open call

    !
    ! initialisation
    !

    stream % name = DioStreamNameWithExtension(name, streamType)
    stream % streamType = streamType
    stream % lun = 0
    stream % id = -1
    stream % synched = sync
    stream % mode = mode
    stream % opened = .false.
    stream % connected = .false.
    stream % autoStream = .false.

    stream % nefFileHandle = DIO_HANDLE_INVALID

    nullify(stream % shmHandle)

    if ( streamType .eq. dio_ASCII_stream) then
        stream % form = 'formatted'
    else if ( streamType .eq. dio_Binary_stream) then
        stream % form = 'unformatted'
    else if ( streamType .eq. dio_His_stream   .or. &
              streamType .eq. dio_WQMap_stream        ) then
#if (defined(WIN32)||defined(HAVE_CONFIG_H))
        stream % form = 'binary'
#else
        stream % form = 'unformatted'
#endif
    else
        stream % form = 'unknown'
    endif

    ! wait until stream is available

    success = .true.
    if ( mode .eq. 'r' ) then
        success = DioSyncGetStreamAvailable(stream)
    endif

    ! Connect to stream (for synch and a-synch streams)
    if ( success ) then
        success = DioStreamConnect(stream, .true.)
        if ( success ) then
            call DioStreamDisconnect(stream, .false.)
            if ( mode .eq. 'w' ) then
                call DioSyncSetStreamAvailable(stream)
            else
                call DioSyncSetStreamReceived(stream)
            endif
            stream % opened = .true.
        endif
    endif

    resultStream = stream

end function DioStreamCreateSynched


!
! Create a new stream
!

function DioStreamCreate(streamType, name, mode) result(stream)
    ! return value

    type(DioStreamType)         :: stream      ! handle to DIO stream

    ! arguments

    integer                     :: streamType  ! type of stream
    character(Len=*),intent(in) :: name        ! stream name
    character(Len=*),intent(in) :: mode        ! open mode ('r' | 'w' | 'a'),
                                            ! ( read | write | append )

    stream = DioStreamCreateSynched(streamType, name, mode, .false.)

end function DioStreamCreate


function DioStreamCreateAuto(name, mode) result(resultStream)

    ! return value

    type(DioStreamType)         :: resultStream  ! stream

    ! arguments

    character(Len=*),intent(in) :: name       ! stream name (=dataset name)
    character(Len=*),intent(in) :: mode       ! 'r' | 'w'

    ! locals

    type(DioStreamType)         :: stream     ! stream
    integer                     :: streamType ! type of stream
    logical                     :: onLine     ! stream synchronized or not
    logical                     :: active     ! ds active or not
    integer                     :: retVal     ! result of shm create
    logical                     :: memBased   ! exchanged is memory based
    integer                     :: memType    ! in Mem or Share

    stream % nefFileHandle = DIO_HANDLE_INVALID
    nullify (stream % shmHandle)
    stream % opened    = .false.
    stream % connected = .false.

    call DioConfGetDsProp(name, dio_ds_Active, active)
    if ( active ) then

        memBased = .false.
        memType  = DioShmUnknown

        streamType = DioDetermineStreamTypeFromName(name)

        if ( streamType == Dio_Unknown_stream ) then
            call DioConfGetDsProp(name, dio_ds_StreamType, streamType)
        endif

        if (streamType .eq. Dio_SharedMem_stream ) then
            memBased = .true.
            memType  = DioShmSharedMem
        endif
        if (streamType .eq. Dio_InMem_stream ) then
            memBased = .true.
            memType  = DioShmInMem
        endif

        if ( memBased ) then
            stream % streamType = streamType
            allocate(stream % shmHandle)
            if ( mode .eq. 'w' ) then
                retval = DioShmDsCreate(stream % shmHandle, 0, 0, memType, name)
            else
                retval = DioShmDsCreate(stream % shmHandle, memType, name)
            endif
            if (retVal .ne. 0) then
                deallocate(stream % shmHandle); nullify(stream % shmHandle)
            else
                stream % autoStream = .true.
                stream % opened     = .true.
                stream % mode       = mode
            endif
        else
            call DioConfGetDsProp(name, dio_ds_OnLine, onLine)
            stream = DioStreamCreateSynched(streamType, name, mode, onLine)
            if ( stream % opened ) then
                stream % autoStream = .true.
            endif
        endif
    endif

    resultStream = stream

end function DioStreamCreateAuto


function DioStreamOpenedOK(stream) result(retVal)

    ! return value
    logical retVal

    ! arguments
    type(DioStreamType)      :: stream      ! handle to DIO stream

    ! body
    retVal = stream % opened

end function DioStreamOpenedOK

!
! Close a stream
!

subroutine DioStreamClose(stream)
    ! arguments

    type(DioStreamType)      :: stream      ! handle to DIO stream

    ! body

    if (associated(stream % shmHandle)) then
        call DioShmDsDestroy(stream % shmHandle)
        nullify(stream % shmHandle)
    else
        call DioStreamDisconnect(stream, .true.)
    endif

    stream % nefFileHandle = DIO_HANDLE_INVALID

end subroutine DioStreamClose


subroutine DioStreamCloseAllUnits()

   ! Used to free all connected files during finalize

   integer             :: iunit
   logical             :: isOpened

   if (maxStreamUnit >= minStreamUnit) then
   
      do iunit = minStreamUnit, maxStreamUnit

         inquire(iunit, opened=isOpened)
         if (isOpened) then
            close(iunit)
         endif
      
      enddo
      
   endif
   
   maxStreamUnit = -214748300
   minStreamUnit = 214748300

end subroutine DioStreamCloseAllUnits


!
! Sleep for a while
!
subroutine DioStreamDelay
    integer     ::  delay !< sleep period in milliseconds

    call DioConfGetStProp(dio_st_Delay, delay)

    call CUTIL_SLEEP(delay)

end subroutine DioStreamDelay


function DioStreamSleep(timeWaited) result(retVal)

    include 'dio-time-support.inc'

    logical                  :: retVal
    integer, intent(INOUT)   :: timeWaited           ! time waited in milliseconds
    character(DioMaxPropLen) :: timeOutString        ! open mode ('r' | 'w' | 'a'),
    real                     :: timeOutJulian
    integer                  :: timeOut              ! time out in milliseconds
    integer                  :: sleepTime            ! sleepTime for timeout in milliseconds

    retVal = .true.

    call DioConfGetStProp(dio_st_SleepTime, sleepTime)
    call DioConfGetStProp(dio_st_TimeOut, timeOutString)
    timeOutJulian = DioDeltaTimeString2Julian(timeOutString)
    timeOut = timeOutJulian*DioDayToSec*1000

    call CUTIL_SLEEP(sleepTime)
    timeWaited = timeWaited + sleepTime                

    if ( timeWaited .gt. (timeOut) ) then
#if (defined(USE_DLL))
        call DioStreamError(105, 'DelftIO TIME OUT')
#else
        write(*,*) 'DelftIO TIME OUT'
#endif
        retVal = .false.
    endif

end function DioStreamSleep


end module dio_streams
