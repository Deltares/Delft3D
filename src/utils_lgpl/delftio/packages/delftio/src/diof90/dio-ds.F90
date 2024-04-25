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
!!! Dio-DS: Dio Datasets
!!!
!!! (c) Deltares, dec 2000
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module Dio_ds

use dio_streams
use dio_shm

implicit none

!*
!* prive constants
!*

!
! int steps in HIS File
!
double precision, parameter :: maxIntSteps = 2147483600.0D+00

!*
!* public constants
!*

!!
!! Parameters
!!

integer, parameter :: DioMaxDsNameLen  = 132   ! max len of ds name
integer, parameter :: DioMaxDsDescLen  = 256   ! max len of ds description
integer, parameter :: DioMaxTimLen     = 25    ! max len of time string

integer, parameter :: DioMaxNrTimeSteps =  256 ! max #timesteps in a dataset

character(len = DioMaxDsNameLen), parameter :: DsNoName = '-'
character(len = DioMaxDsDescLen), parameter :: DsNoDesc = '-'


!!
!! Write to/from Nefis files
!! - Identification-extionsions (e.g. for storage in Nefis files)
!! - Max. elem. name len
!! - Max. nefis error len
!!

character(len = 5), parameter :: DSHeaderExtension = '_INFO'
character(len = 5), parameter :: DSDataExtension   = '_DATA'
integer, parameter  :: DioMaxElmNameLen = 16
integer, parameter  :: DioNefErrMsgLen = 1024


!!
!! Definition of data types
!!

!
! dataset
!
!
! dataset info
!
type DioDsType
    character(len=DioMaxDsNameLen):: name           ! name
    character(len=DioMaxDsDescLen):: descript       ! description

    integer                       :: id             ! dataset ID (F77 interface)

    !
    ! general time info
    !
    logical                       :: timeGrows      ! does time-info grow per timestep?
    logical                       :: dataGrows      ! does stored date grow per timestep?

    integer                       :: nTimes         ! # timesteps (? = curTimeIndex?)
    integer                       :: curDataIndex   ! current index in dataset
    integer                       :: curTimeIndex   ! current timestep counter
    integer                       :: curPutGetCount ! counts current PutGet action

    !
    ! time info as strings
    !
    character(len=DioMaxTimLen):: startTimeStr      ! start Time
    character(len=DioMaxTimLen):: deltaT            ! Timestep (for equidist. series)

    !
    ! time info julian date values
    !
    double precision              :: startTimeVal    ! 
    double precision              :: DeltaTVal       ! 
    double precision, &
             pointer, dimension(:):: timeStep        ! timestep values
    double precision, &
             pointer, dimension(:):: preReadTims     ! pre-read julian dates
    integer, &
             pointer, dimension(:):: preReadHisSteps ! pre-read HisSteps
    integer                       :: nPreReadTims    ! # preread time steps
    logical                       :: readingJuls     ! pre-reading time steps?

    !
    ! time info for 'HIS file' data
    !
    integer                       :: hisTimeUnit     ! time unit for HIS files
    integer                       :: hisTimeMult     ! time interval in HIS files
    integer, pointer, dimension(:):: hisStep         ! current his step

    !
    ! input and output streams
    !
    type(DioStreamType)           :: inStream        ! stream for dataset
    type(DioStreamType)           :: outStream       ! stream for dataset

end type DioDsType



!!
!! Interface for overloaded functions
!!

interface DioDsCreate
    module procedure DioDsCreate
    module procedure DioDsCreateWithDescription
end interface


contains

!!
!! functions / subroutines
!!

!
! Create dataset for empty set
!


function DioDsCreate(name) result(ds)

    include 'dio-time-support.inc' ! for time support functions

    ! return value

    type(DioDsType)              :: ds    ! dataset

    ! arguments

    character(Len=*), intent(in) :: name  ! dataset name

    ! intialize ds fields

    ds % name           = name
    ds % descript       = DsNoDesc
    ds % id             = -1

    ds % timeGrows      = .false.
    ds % dataGrows      = .false.

    ds % nTimes         = 0
    ds % curDataIndex   = 0
    ds % curTimeIndex   = 0
    ds % curPutGetCount = 0

    call DioConfGetDsProp(name, dio_ds_StartTime   , ds % startTimeStr)
    call DioConfGetDsProp(name, dio_ds_DeltaTime   , ds % deltaT)

    ds % startTimeVal  = DioTimeString2Julian(ds % startTimeStr)
    ds % deltaTVal     = DioDeltaTimeString2Julian(ds % deltaT)

    allocate(ds % timeStep(DioMaxNrTimeSteps))

    ds % timeStep   = ds % startTimeVal

    call DioConfGetDsProp(name, dio_ds_HisTimeUnit, ds % hisTimeUnit)
    call DioConfGetDsProp(name, dio_ds_hisTimeMult, ds % hisTimeMult)
    allocate(ds % hisStep(DioMaxNrTimeSteps))
    ds % hisStep = 0

    ds % inStream  % opened = .false.
    ds % outStream % opened = .false.

    nullify(ds % preReadTims)
    nullify(ds % preReadHisSteps)
    ds % nPreReadTims = 0
    ds % ReadingJuls  = .false.

end function DioDsCreate


function DioDsCreateWithDescription(name, desc) result(ds)

    ! return value

    type(DioDsType)             :: ds    ! dataset

    ! arguments

    character(Len=*), intent(in):: name  ! dataset name
    character(Len=*), intent(in):: desc  ! dataset description

    ! intialize

    ds = DioDsCreate(name)
    ds % descript = desc

end function DioDsCreateWithDescription


function DioDsDataSize(ds) result(dataSize)

    ! return value

    integer            :: dataSize    ! size of time dim in mem.

    ! arguments

    type(DioDsType)    :: ds        ! dataset

    ! body

    if (ds % dataGrows) then
        dataSize = ds % curDataIndex
    else
        dataSize = 1
    endif

end function DioDsDataSize


subroutine DioDsDestroy(ds)

    ! arguments

    type(DioDsType)            :: ds    ! dataset

    ! body

    ds % name          = DsNoName
    ds % descript      = DsNoDesc

    if (associated(ds % timeStep)) then
       deallocate(ds % timeStep)
       nullify(ds % timeStep)
    endif
    
    if (associated(ds % hisStep)) then
       deallocate(ds % hisStep)
       nullify(ds % hisStep)
    endif
    
    if (associated(ds % preReadTims)) then
        deallocate(ds % preReadTims)
        nullify(ds % preReadTims)
    endif
    
    if (associated(ds % preReadHisSteps)) then
        deallocate(ds % preReadHisSteps)
        nullify(ds % preReadHisSteps)
    endif

    if (ds % inStream % opened) then
        if ( ds % inStream % autoStream ) then
            call DioStreamClose(ds % inStream)
            ds % inStream % opened = .false.
        endif
    endif
    
    if (ds % outStream % opened) then
        if ( ds % outStream % autoStream ) then
            call DioStreamClose(ds % outStream)
            ds % outStream % opened = .false.
        endif
    endif

end subroutine DioDsDestroy


subroutine DioDsIncreaseTimestep(ds)

    ! arguments

    type(DioDsType)  :: ds            ! dataset

    ! locals
    integer          :: prevHisStep   ! previous hisStep
    double precision :: prevJulian    ! previous julian time
    logical          :: firstTimeStep ! first time step?
    ! body

    firstTimeStep = .true.
    if ( ds % curTimeIndex > 0 ) then
        firstTimeStep = .false.
        prevJulian  = ds % timeStep(ds % curTimeIndex)
        prevHisStep = ds % hisStep (ds % curTimeIndex)
    endif

    ds % curPutGetCount = ds % curPutGetCount + 1

    if ( ds % dataGrows ) then
        ds % curDataIndex = ds % curDataIndex + 1
    else
        ds % curDataIndex = 1
    endif

    if ( ds % timeGrows ) then
        ds % curTimeIndex = ds % curTimeIndex + 1
    else
        ds % curTimeIndex = 1
    endif

    if ( ds % curTimeIndex > size ( ds % timeStep) ) call DioDsTimeRealloc(ds)

    if ( .not. firstTimeStep ) then
        ds % timeStep(ds % curTimeIndex) = &
                        prevJulian  + ds % deltaTVal
        ds % hisStep(ds % curTimeIndex) = &
                        prevHisStep + 1
    endif
    
end subroutine DioDsIncreaseTimestep


subroutine DioDsSetHisTimestep(ds, intTime)

    ! arguments

    type(DioDsType)  :: ds           ! dataset
    integer          :: intTime      ! HIS timestep
    double precision :: deltaTime    ! time increase due to His step

    ! body
    if ( ds % outStream % streamType .eq. Dio_HIS_Stream ) then
        ds % hisStep(ds % curTimeIndex) = intTime
        deltaTime =  dble(ds % hisTimeUnit) * dble(ds % hisTimeMult) * dble(intTime) / 86400.0D+00
        ds % timeStep(ds % curTimeIndex) = (ds % startTimeVal) + deltaTime
    endif

end subroutine DioDsSetHisTimestep


subroutine DioDsSetJulTimestep(ds, julTime)

    ! arguments

    type(DioDsType)  :: ds           ! dataset
    double precision :: julTime      ! time step as julian date/time

    ! body
    ds % timeStep(ds % curTimeIndex) = julTime
    ds % hisStep (ds % curTimeIndex) = nint(( julTime - ds % startTimeVal ) &
                   * 86400.0D+00 / ( dble(ds % hisTimeUnit) * dble(ds % hisTimeMult) ))

end subroutine DioDsSetJulTimestep


subroutine DioDsDecreaseTimestep(ds)

    include 'dio-time-support.inc' ! for time support functions
    ! arguments

    type(DioDsType)            :: ds    ! dataset

    ! body

    if ( ds % dataGrows ) then
        ds % curDataIndex = ds % curDataIndex - 1
        if ( ds % curDataIndex .lt. 1 ) then
            ds % curDataIndex = 1
        endif
    else
        ds % curDataIndex = 1
    endif

    if ( ds % timeGrows ) then

        ! undo setting of timeStep value for previous time index
        ds % timeStep(ds % curTimeIndex) = DioTimeString2Julian(ds % startTimeStr)
        ds % hisStep(ds % curTimeIndex) = 0

        ds % curTimeIndex = ds % curTimeIndex - 1
        if ( ds % curTimeIndex .lt. 1 ) then
            ds % curTimeIndex = 1
        endif
    else
        ds % curTimeIndex = 1
    endif

    ds % curPutGetCount = ds % curPutGetCount - 1

end subroutine DioDsDecreaseTimestep


subroutine DioDsTimeRealloc(ds)

    ! arguments

    type(DioDsType)            :: ds    ! dataset

    ! locals

    integer                        :: oldSize    ! size of old arrays
    integer                        :: newSize    ! size of new arrays
    double precision, &
             pointer, dimension(:) :: timeStep   ! new timestep values
    integer, &
             pointer, dimension(:) :: hisStep    ! new his steps

    ! body

    oldSize = size(ds % timeStep)
    newSize = oldSize + DioMaxNrTimeSteps

    allocate(timeStep(newSize))
    allocate(hisStep(newSize))

    timeStep   = ds % startTimeVal
    hisStep    = 0

    timeStep  (1:oldSize) = ds % timeStep
    hisStep   (1:oldSize) = ds % hisStep

    deallocate( ds % timeStep) ; nullify( ds % timeStep)
    deallocate( ds % hisStep ) ; nullify( ds % hisStep )

    ds % timeStep => timeStep
    ds % hisStep => hisStep

end subroutine DioDsTimeRealloc


function DioDsGetCurrentTimeString(ds) result(timeString)

    include 'dio-time-support.inc' ! for time support functions
    ! return value

    character(len=DioMaxTimLen)  :: timeString ! time string for
                                               ! current time step
    ! arguments

    type(DioDsType)              :: ds         ! dataset

    ! body

    timeString = DioJulian2DioTime(ds % timeStep ( ds % curTimeIndex )) 

end function DioDsGetCurrentTimeString


function DioDsGetCurrentTimeStep(ds) result(timeStep)

    ! return value

    double precision :: timeStep

    ! arguments

    type(DioDsType)  :: ds    ! dataset

    ! body

    timeStep = ds % timeStep ( ds % curTimeIndex )

end function DioDsGetCurrentTimeStep


subroutine DioDsSetStartTime(ds, startTime)

    include 'dio-time-support.inc'

    ! arguments

    type(DioDsType)              :: ds        ! dataset
    double precision, intent(in) :: startTime ! startTime as julian

    ! body

    ds % startTimeVal = startTime
    ds % startTimeStr = DioJulian2DioTime(startTime)

    ds % timeStep     = startTime

end subroutine DioDsSetStartTime


subroutine DioDsSetEndTime(ds, endTime)

    include 'dio-time-support.inc'

    ! arguments

    type(DioDsType)              :: ds      ! dataset
    double precision, intent(in) :: endTime ! endTime as julian
    double precision             :: nSteps  ! expected #steps in HIS

    ! body

    nSteps = ( endTime - ds % startTimeVal ) * 86400.D+0
    if ( nSteps > maxIntSteps ) then
        ds % hisTimeMult = 60
        nSteps = (endTime - ds % startTimeVal) * 86400.D+0 / 60.0D+0
        if ( nSteps > maxIntSteps ) then
            ds % hisTimeMult = 3600
            nSteps = (endTime - ds % startTimeVal) * 86400.D+0 / 3600.0D+0
            if ( nSteps > maxIntSteps ) then
                ds % hisTimeMult = 86400
            endif
        endif
    endif

end subroutine DioDsSetEndTime


subroutine DioDsMakeT0String(ds, T0String)

    include 'dio-time-support.inc' ! for time support functions

    ! arguments

    type(DioDsType)              :: ds       ! dataset
    character(len=*)             :: T0String ! Generated T0-string

    ! locals
    character(DioMaxTimLen)      :: hisTime  ! temp HIS time format

    ! body

    hisTime = DioJulian2HisTime(ds % startTimeVal)
    T0String = ' '
    write(T0String, '(A4, A19, ''  (scu='', I8, ''s)'')' ) &
                'T0: ', hisTime, ds % hisTimeMult

end subroutine DioDsMakeT0String


function DioDsSetTimeFromT0String(ds, T0String) result(retVal)

    include 'dio-time-support.inc' ! for time support functions

    ! return value

    logical                     :: retVal          ! .true.: T0 string was OK

    ! arguments
    
    type(DioDsType), target     :: ds              ! dataset
    character(Len=*),intent(IN) :: T0String        ! HIS T0 String

    ! locals

    integer, parameter          :: scu=4           ! lenght of 'scu='
    integer                     :: timeUnit        ! time unit in seconds
    integer                     :: timeMult        ! time multiplier
    character(len=1)            :: unitType        ! s/m/d etc.
    integer                     :: scuPos, unitPos ! help vars.
    character(len=20)           :: multFormat      ! format for read mult.
    
    ! body
    
    retVal = .false.
    timeUnit = 0 
    timeMult = 0 

    scuPos=index(T0String,'scu=')+scu  !! "...(scu=_ <- start, unit -> <u>)"
    if ( scuPos .gt. scu ) then
        unitPos =index(T0String(scuPos:),'d')
        if ( unitPos .eq. 0) unitPos =index(T0String(scuPos:),'h')
        if ( unitPos .eq. 0) unitPos =index(T0String(scuPos:),'m')
        if ( unitPos .eq. 0) unitPos =index(T0String(scuPos:),'s')

        if ( unitPos .ne. 0 ) then
            unitPos = unitPos + scuPos-1 !! index from start of hisTime
            if ( unitPos - scuPos - 1 < 10 ) then
                write(multFormat, '(A2, I1, A1)') '(I', unitPos - scuPos - 1, ')'
            else
                write(multFormat, '(A2, I2, A1)') '(I', unitPos - scuPos - 1, ')'
            endif
            read(T0String(scuPos+1:unitPos-1), multFormat, err=999) timeMult

            unitType = T0String(unitPos:unitPos)
            if ( unitType .eq. 's' ) then
                timeUnit = 1
            else if ( unitType .eq. 'm' ) then
                timeUnit = 60
            else if ( unitType .eq. 'h' ) then
                timeUnit = 3600
            else if ( unitType .eq. 'd' ) then
                timeUnit = 86400
            endif

        999 continue

        endif
    endif

    if ( timeUnit .gt. 0 .and. timeMult .gt. 0 ) then
        call DioHisTime2DioTime(T0String(5:), ds % startTimeStr)
        ds % startTimeVal = DioTimeString2Julian(ds % startTimeStr)
        ds % hisTimeUnit = timeUnit
        ds % hisTimeMult = timeMult
        retVal = .true.
    endif

end function DioDsSetTimeFromT0String


!!
!! General Time Support Functions
!!
function DioDsTimeString2Julian(dioTime) result(julian)

    include 'dio-time-support.inc' ! for time support functions

    ! return value
    double precision            :: julian   ! julian date

    ! arguments
    character(Len=*),intent(IN) :: dioTime  ! DIO time format

    julian = DioTimeString2Julian(dioTime)

end function DioDsTimeString2Julian


function DioDsJulian2DioTime(julian) result(dioTime)

    include 'dio-time-support.inc' ! for time support functions

    ! return value
    character(len=DioMaxTimLen)  :: dioTime

    ! arguments
    double precision, intent(IN) :: julian

    dioTime = DioJulian2DioTime(julian)

end function DioDsJulian2DioTime


function DioDsJulian2HisTime(julian) result(hisTime)

    include 'dio-time-support.inc' ! for time support functions

    ! return value
    character(len=DioMaxTimLen)  :: hisTime

    ! arguments
    double precision, intent(IN) :: julian

    hisTime = DioJulian2HisTime(julian)

end function DioDsJulian2HisTime


end module Dio_ds
