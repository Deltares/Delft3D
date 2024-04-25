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
!!! Dio-setting: Dio Config
!!!
!!! (c) Deltares, dec 2000
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module Dio_ds_config


use dio_prop
use delftio_version_module

implicit none

!*
!* EXTERNAL constants
!* (unless declared private)
!*

!
! File length
!

integer, parameter :: DioMaxStreamLen = 256 ! Max length of stream name
integer, parameter :: DioMaxPropLen   = 128 ! Max length of ini-file property

!
! Enumeration for Dio Stream types
!

integer, parameter :: dio_Unknown_Stream   = 0

integer, parameter :: dio_ASCII_stream     = 1
integer, parameter :: dio_Binary_stream    = 2
integer, parameter :: dio_His_stream       = 3
integer, parameter :: dio_Nefis_stream     = 4
integer, parameter :: dio_SharedMem_stream = 5
integer, parameter :: Dio_InMem_stream     = 6
integer, parameter :: Dio_WQMap_stream     = 7

 

!
! Enumeration for types of var.s that can be stored in a dataset
!

integer, parameter :: Dio_Var_Unknown  = 0
integer, parameter :: Dio_Var_Integer  = 1
integer, parameter :: Dio_Var_Real     = 2
integer, parameter :: Dio_Var_Double   = 3
integer, parameter :: Dio_Var_Logical  = 4


!
! Enumerations for Settings properties
! (integer, logical and string properties)
!

integer, private, parameter :: dio_St_NR_IntProps       = 2
integer, parameter          :: dio_st_Delay       = 1
integer, parameter          :: dio_st_SleepTime   = 2

integer, private, parameter :: dio_St_NR_LogProps       = 1
integer, parameter          :: dio_st_AllowJumpBack = 1

integer, private, parameter :: dio_St_NR_StrProps       = 1
integer, parameter          :: dio_st_Timeout     = 1


!
! Enumerations for Dataset properties
! (integer, logical and string properties)
!

integer, private, parameter :: dio_Ds_NR_IntProps       = 4
integer, parameter :: dio_ds_StreamType           = 1
integer, parameter :: dio_ds_VarType              = 2
integer, parameter :: dio_ds_HisTimeUnit          = 3
integer, parameter :: dio_ds_HisTimeMult          = 4

integer, private, parameter :: dio_Ds_NR_LogProps       = 2
integer, parameter :: dio_ds_OnLine               = 1
integer, parameter :: dio_ds_Active               = 2

integer, private, parameter :: dio_Ds_NR_StrProps       = 2
integer, parameter :: dio_ds_StartTime            = 1
integer, parameter :: dio_ds_DeltaTime            = 2



!*
!* DEFAULTS (internal constants)
!*

character(len=DioMaxStreamLen), private, &   ! default config file
          parameter :: DefaultConfigFile = 'dioconfig.ini'

character(len = 22), private, parameter :: DsDefaultStartTime   = '1900/01/01;00:00:00.00'
character(len = 22), private, parameter :: DsDefaultDeltaT      =         '00;00:00:01.00'
integer,             private, parameter :: DsDefaultHisTimeUnit =   1
integer,             private, parameter :: DsDefaultHisTimeMult =   1

character(len = 22), private, parameter :: DsDefaultTimeOut     =         '01;00:00:00.00'


!*
!* INTERNAL constants
!*

logical, private            :: initialized    = .false.   ! intialized or not?

integer, private, parameter :: MaxNrDsConfigs = 50        ! man # dataset configs

integer, private, parameter :: MaxKeyTextLen  = 64        ! prop. keyword len



!*
!* INTERNAL data types and data storage
!*


type DioStConfigType
    character(len=DioMaxPropLen)           :: name
    integer, dimension(dio_St_NR_IntProps) :: intProp
    logical, dimension(dio_St_NR_LogProps) :: logProp
    character(len=DioMaxPropLen), dimension(dio_St_NR_StrProps) :: strProp
end type DioStConfigType


type DioDsConfigType
    character(len=DioMaxPropLen)           :: name
    integer, dimension(dio_Ds_NR_IntProps) :: intProp
    logical, dimension(dio_Ds_NR_LogProps) :: logProp
    character(len=DioMaxPropLen), dimension(dio_Ds_NR_StrProps) :: strProp
end type DioDsConfigType


type DioConfigType
    character(len=DioMaxStreamLen):: iniFile
    type(DioStConfigType)         :: settings
    type(DioDsConfigType)         :: dsDefaults
    type(DioDsConfigType),&
        dimension(MaxNrDsConfigs) :: dsConfig
    integer                       :: nDs            ! # dataset configs
end type DioConfigType


type(DioConfigType), private, target :: dioConfig


!*
!* EXTERNAL interface
!*

!
! Initialize DIO
!

interface DioInit
    module procedure DioInit
    module procedure DioInitByFile
end interface


!
! Get a Settings property
!

interface DioConfGetStProp
    module procedure DioConfGetStIntProp
    module procedure DioConfGetStLogProp
    module procedure DioConfGetStStrProp
end interface

!
! Get a Dataset property
!

interface DioConfGetDsProp
    module procedure DioConfGetDsIntProp
    module procedure DioConfGetDsLogProp
    module procedure DioConfGetDsStrProp
end interface


!
! Set Dataset property for all datasets
!

interface DioSetDefaultDsProp
    module procedure DioSetDefaultDsIntProp
    module procedure DioSetDefaultDsLogProp
    module procedure DioSetDefaultDsStrProp
end interface




contains


!*
!* Internal functions
!*


subroutine DioConfigDump(lun)

    integer                        :: lun
    integer                        :: d
    type(DioDsConfigType), pointer :: ds

    write(lun,*) 'Dio ini-File: ', trim(dioConfig % iniFile)

    write(lun,*) 'Settings Dly: ', dioConfig % settings % intProp(dio_st_Delay)
    write(lun,*) 'Settings Slp: ', dioConfig % settings % intProp(dio_st_SleepTime)
    write(lun,*) 'Settings Tmo: ', dioConfig % settings % strProp(dio_st_Timeout)
    write(lun,*) 'Settings AJB: ', dioConfig % settings % strProp(dio_st_AllowJumpBack)


    ds => dioConfig % dsDefaults
    write(lun,*)
    write(lun,*) 'Ds Def Nam: ', trim(ds % name)
    write(lun,*) 'Ds Def OnL: ', ds % logProp(dio_ds_OnLine)
    write(lun,*) 'Ds Def Str: ', ds % intProp(dio_ds_StreamType)
    write(lun,*) 'Ds Def Var: ', ds % intProp(dio_ds_VarType)
    write(lun,*) 'Ds Def Act: ', ds % logProp(dio_ds_Active)
    write(lun,*) 'Ds Def STi: ', ds % strProp(dio_ds_StartTime)
    write(lun,*) 'Ds Def DTi: ', ds % strProp(dio_ds_DeltaTime)
    write(lun,*) 'Ds Def DHS: ', ds % intProp(dio_ds_HisTimeMult)
    write(lun,*) 'Ds Def HTU: ', ds % intProp(dio_ds_HisTimeUnit)

    write(lun,*)
    write(lun,*) '#datasets: ', dioConfig % nDs

    do d = 1, dioConfig % nDs
        ds => dioConfig % dsConfig(d)
        write(lun,*)
        write(lun,*) 'Ds Nam: ', trim(ds % name)
        write(lun,*) 'Ds OnL: ', ds % logProp(dio_ds_OnLine)
        write(lun,*) 'Ds Str: ', ds % intProp(dio_ds_StreamType)
        write(lun,*) 'Ds Var: ', ds % intProp(dio_ds_VarType)
        write(lun,*) 'Ds Act: ', ds % logProp(dio_ds_Active)
        write(lun,*) 'Ds STi: ', ds % strProp(dio_ds_StartTime)
        write(lun,*) 'Ds DTi: ', ds % strProp(dio_ds_DeltaTime)
        write(lun,*) 'Ds DHS: ', ds % intProp(dio_ds_HisTimeMult)
        write(lun,*) 'Ds HTU: ', ds % intProp(dio_ds_HisTimeUnit)
    enddo
    write(lun,*)

end subroutine DioConfigDump


function DetermStreamType(valString) result(retVal)


    character(Len=*):: valString
    integer :: retVal


    retVal = dioConfig % dsDefaults % intProp(dio_ds_StreamType)


    if ( StringsEqual( CaseInsens, valString, 'Bin' )   .or. &
         StringsEqual( CaseInsens, valString, 'Bin' )   .or. &
         StringsEqual( CaseInsens, valString, 'Binary' )     ) then
        retVal = Dio_Binary_Stream
    else if ( StringsEqual( CaseInsens, valString, 'ASCII' ) ) then
        retVal = Dio_ASCII_Stream
    else if ( StringsEqual( CaseInsens, valString, 'HIS' ) ) then
        retVal = Dio_His_Stream
    else if ( StringsEqual( CaseInsens, valString, 'SharedMem'   ) .or. &
              StringsEqual( CaseInsens, valString, 'SharedMemory') ) then
        retVal = Dio_SharedMem_Stream
    else if ( StringsEqual( CaseInsens, valString, 'InMem'   ).or. &
              StringsEqual( CaseInsens, valString, 'InMemory') ) then
        retVal = Dio_InMem_Stream
    else if ( StringsEqual( CaseInsens, valString, 'Map'  ) .or. &
              StringsEqual( CaseInsens, valString, 'WQMap') ) then
        retVal = Dio_WQMap_Stream
    endif

end function DetermStreamType


function DetermVarType(valString) result(retVal)
    character(Len=*):: valString
    integer :: retVal
    retVal = dioConfig % dsDefaults % intProp(dio_ds_VarType)
    if ( StringsEqual( CaseInsens, valString, 'Real' ) ) then
        retVal = Dio_Var_Real
    else if ( StringsEqual( CaseInsens, valString, 'Double' ) ) then
        retVal = Dio_Var_Double
    else if ( valString .eq. 'Integer' ) then
        retVal = Dio_Var_Integer
    else if ( valString .eq. 'Logical' ) then
        retVal = Dio_Var_Logical
    endif
end function DetermVarType


subroutine DioInitConfig

    ! locals
    double precision               :: timeOutJulian   ! deltaT expressed modified julian
    integer                        :: timeOutTime     ! time out in milliseconds

    ! externals (time support)
    double precision, external     :: DioDeltaTimeString2Julian

    ! body

    dioConfig % iniFile = DefaultConfigFile
#if(defined(WIN32))
    dioConfig % settings % intProp(dio_st_Delay)      = 1
#else
    dioConfig % settings % intProp(dio_st_Delay)      = 500
#endif

    dioConfig % settings % intProp(dio_st_SleepTime)  = 5
    dioConfig % settings % strProp(dio_st_TimeOut)    = DsDefaultTimeOut

    call DIOSETSYNCSLEEPTIME_C(dioConfig % settings % intProp(dio_st_SleepTime));
    timeOutJulian = DioDeltaTimeString2Julian(dioConfig % settings % strProp(dio_st_TimeOut))
    timeOutTime   = timeOutJulian*86400*1000
    call DIOSETSYNCTIMEOUT_C  (timeOutTime);

    dioConfig % dsDefaults % name = '-'

    dioConfig % dsDefaults % intProp(dio_ds_StreamType)   = Dio_HIS_Stream
    dioConfig % dsDefaults % intProp(dio_ds_VarType)      = Dio_Var_Real

    dioConfig % dsDefaults % logProp(dio_ds_OnLine)       = .false.
    dioConfig % dsDefaults % logProp(dio_ds_Active)       = .true.

    dioConfig % dsDefaults % strProp(dio_ds_StartTime)    = DsDefaultStartTime
    dioConfig % dsDefaults % strProp(dio_ds_DeltaTime)    = DsDefaultDeltaT
    dioConfig % dsDefaults % intProp(dio_ds_HisTimeUnit)  = DsDefaultHisTimeUnit
    dioConfig % dsDefaults % intProp(dio_ds_HisTimeMult)  = DsDefaultHisTimeMult

    dioConfig % nDs = 0

end subroutine DioInitConfig


subroutine DioUpdateConfig

    integer :: i

    do i = 1, MaxNrDsConfigs
        dioConfig % dsConfig(i) = dioConfig % dsDefaults
    enddo

end subroutine DioUpdateConfig


subroutine DioReadDsConfig(ds, group)
    type(DioDsConfigType)          :: ds
    character(Len=*)                  :: group

    character(len=DioMaxPropLen)   :: valString
    logical                        :: getRes    

    getRes = prop_get( group, 'Name'        , ds % name )
    getRes = prop_get( group, 'OnLine'      , ds % logProp(dio_ds_OnLine) )
    getRes = prop_get( group, 'Active'      , ds % logProp(dio_ds_Active) )
    getRes = prop_get( group, 'StartTime'   , ds % strProp(dio_ds_StartTime) )
    getRes = prop_get( group, 'DeltaTime'   , ds % strProp(dio_ds_DeltaTime) )
    getRes = prop_get( group, 'HisTimeUnit' , ds % intProp(dio_ds_HisTimeUnit) )

    if ( prop_get( group, 'StreamType', valString ) ) then
        ds % intProp(dio_ds_StreamType) = DetermStreamType(valString)
    endif

    if ( prop_get( group, 'VarType', valString ) ) then
        ds % intProp(dio_ds_VarType) = DetermVarType(valString)
    endif

end subroutine DioReadDsConfig


subroutine DioReadStConfig


    ! locals

    type(DioStConfigType), pointer :: settings        ! pointer to settings part of config
    logical                        :: getRes          ! result of get call

    double precision               :: timeOutJulian   ! deltaT expressed modified julian
    integer                        :: timeOutTime     ! time out in milliseconds

    ! externals (time support)
    double precision, external     :: DioDeltaTimeString2Julian

    settings => dioConfig % settings
    getRes = prop_get( 'Settings', 'Delay'    , settings % intProp(dio_st_Delay) )
    getRes = prop_get( 'Settings', 'SleepTime', settings % intProp(dio_st_SleepTime) )
    getRes = prop_get( 'Settings', 'TimeOut'  , settings % strProp(dio_st_TimeOut) )

    call DIOSETSYNCSLEEPTIME_C(settings % intProp(dio_st_SleepTime));
    timeOutJulian = DioDeltaTimeString2Julian(settings % strProp(dio_st_TimeOut))
    timeOutTime   = timeOutJulian*86400*1000
    call DIOSETSYNCTIMEOUT_C  (timeOutTime);

end subroutine DioReadStConfig


subroutine DioReadIniFile(fileName)
    character(Len=*), intent(IN)      :: fileName
    character(len=MaxKeyTextLen)   :: group
    logical                        :: getRes    
    integer                        :: d
    type(DioDsConfigType), pointer :: ds
    character(len=DioMaxStreamLen) :: pathFileName

    if ( .NOT. prop_file_by_name(fileName) ) then
        pathFileName = fileName
        call DioConfAddExecutablePath(pathFileName)
        if ( .NOT. prop_file_by_name(pathFileName) ) then
            return
        endif
    endif

    dioConfig % iniFile = fileName

    call DioReadStConfig

    group = 'Defaults'
    ds => dioConfig % dsDefaults
    call DioReadDsConfig(ds, group)

    ! set all dataset according to just read values
    call DioUpdateConfig

    getRes = prop_get( group, 'NumberOfDatasets', dioConfig % nDs )
    do d = 1, dioConfig % nDs
        if ( d .lt. 10 ) then
            write(group, '(A,I1)') 'Dataset', d
        else
            write(group, '(A,I2)') 'Dataset', d
        endif
        ds => dioConfig % dsConfig(d)
        call DioReadDsConfig(ds, group)
    enddo
end subroutine DioReadIniFile


!!
!! 
!!

subroutine DioConfGetExeName(inName, exeName)
    character(Len=*), intent(IN) :: inName  ! ds name
    character(Len=*), intent(OUT):: exeName   ! ds name without path
    integer                      :: subPos    ! pos. substring
    character(Len=1)             :: slash     ! slash or backslash

#if (defined(WIN32))
    slash = '\'
#else
    slash = '/'
#endif
    ! value in case of no slash
    exeName = inName

    ! find slash pos. from end. If found, skip path.
    subPos = index(inName, slash, .true.)
    if ( subPos .ne. 0 ) then
        exeName = inName(subPos+1:)
    endif

    ! find exeextension from end
    subPos = index(exeName, '.exe', .true.)
    if ( subPos .eq. 0 ) subPos = index(exeName, '.EXE', .true.)
    if ( subPos .eq. 0 ) subPos = index(exeName, '.Exe', .true.)

    ! If extension found, replace by spaces
    if ( subPos .ne. 0 .and. subpos .eq. (len_trim(exeName)-3) ) then
        exeName(subPos:) = '    '
    endif

end subroutine DioConfGetExeName


subroutine DioConfAddExecutablePath(fileName)
    character(Len=*),intent(INOUT) :: fileName
    integer(2)                  :: index
    character(DioMaxStreamLen)  :: buf
    logical                     :: ready


    call GetArg(0, buf)
    index = DioMaxStreamLen
    ready = .FALSE.
    do while ((index.gt.0).AND..NOT.ready)
      if((buf(index:index) .eq. char(92)).OR.(buf(index:index) .eq. '/')) then
        ready = .TRUE.
      else
        buf(index:index) = ' ' 
        index = index-1
      endif    
    enddo
    buf(index + 1:) = fileName
    ! return new filename
    fileName = buf
     
end subroutine DioConfAddExecutablePath


function DioConfGetDsConfig(dsName) result(ds)

    type(DioDsConfigType), pointer :: ds

    character(Len=*),intent(IN)   :: dsName

    integer                    :: i

    ds => dioConfig % dsDefaults
    do i = 1, dioConfig % nDs
        if ( StringsEqual(CaseInsens, dioConfig % dsConfig(i) % name, dsName ) ) then
            ds => dioConfig % dsConfig(i)
        endif
    enddo

end function DioConfGetDsConfig


!!
!! EXTERNAL FUNCTIONS
!!


subroutine DioCheckInit
    if (.not. initialized) then
        call DioInit
    endif
end subroutine DioCheckInit


!
! Initialisation
!

subroutine DioInit

    ! no file specified, call init function with default filename

    call DioInitByFile(dioConfig % iniFile)

end subroutine DioInit


subroutine DioInitByFile(fileName)

    ! arguments
    character(Len=*),intent(IN) :: fileName

    ! body
    initialized = .false.
    call DioInitConfig
    call DioUpdateConfig
    call DioReadIniFile(fileName)
    initialized = .true.
end subroutine DioInitByFile


!
! Get Version / Ident. String
!

subroutine DioGetVersion(retVal)

    ! arguments
    character(Len=*) :: retval

    ! body
    retVal = ' '
    call getfullversionstring_delftio(retval)

end subroutine DioGetVersion


subroutine DioGetIdent(retVal)

    ! arguments
    character(Len=*) :: retval

    ! locals
    integer       :: identLen ! len of id.string minus null-char

    ! body
    retVal = ' '
    call getfullversionstring_delftio(retval)

end subroutine DioGetIdent


!
! TODO
! Get process name from argument list
!

!*
!* EXTERNAL Set Functions
!*

!
! Set a Settings property (integer, logical, string)
!

subroutine DioSetStIntProp(stProp, value)

    ! arguments
    integer, intent(IN) :: stProp  ! property to be set
    integer, intent(IN) :: value   ! value for property


    ! body
    call DioCheckInit
    dioConfig % settings % intProp(stProp) = value

    ! Store sleep-time in C++ shared mem communication
    if ( stProp == dio_st_SleepTime ) then
        call DIOSETSYNCSLEEPTIME_C(dioConfig % settings % intProp(dio_st_SleepTime));
    endif

end subroutine DioSetStIntProp


subroutine DioSetStLogProp(stProp, value)

    ! arguments
    integer, intent(IN) :: stProp  ! property to be set
    logical, intent(IN) :: value   ! value for property

    ! body
    call DioCheckInit
    dioConfig % settings % logProp(stProp) = value

end subroutine DioSetStLogProp


subroutine DioSetStStrProp(stProp, value)

    ! arguments
    integer, intent(IN)      :: stProp  ! property to be set
    character(Len=*),intent(IN) :: value   ! value for property

    ! locals
    double precision             :: timeOutJulian   ! deltaT expressed modified julian
    integer                      :: timeOutTime     ! time out in milliseconds

    ! externals (time support)
    double precision, external   :: DioDeltaTimeString2Julian

    ! body

    call DioCheckInit
    dioConfig % settings % strProp(stProp) = value

    ! Store time-out in C++ shared mem communication
    if ( stProp == dio_st_TimeOut ) then
        timeOutJulian = DioDeltaTimeString2Julian(dioConfig % settings % strProp(dio_st_TimeOut))
        timeOutTime   = timeOutJulian*86400*1000
        call DIOSETSYNCTIMEOUT_C(timeOutTime);
    endif

end subroutine DioSetStStrProp


!
! Set a dataset property (integer, logical, string) for all datasets
!

subroutine DioSetDefaultDsIntProp(dsProp, value)

    ! arguments
    integer, intent(IN) :: dsProp  ! property to be set
    integer, intent(IN) :: value   ! value for property

    ! locals
    integer                  :: d       ! counter


    ! body
    call DioCheckInit
    dioConfig % dsDefaults % intProp(dsProp) = value
    do d = 1, MaxNrDsConfigs
        dioConfig % dsConfig(d) % intProp(dsProp) = value
    enddo

end subroutine DioSetDefaultDsIntProp


subroutine DioSetDefaultDsLogProp(dsProp, value)

    ! arguments
    integer, intent(IN) :: dsProp  ! property to be set
    logical, intent(IN) :: value   ! value for property

    ! locals
    integer                  :: d       ! counter

    ! body
    call DioCheckInit
    dioConfig % dsDefaults % logProp(dsProp) = value
    do d = 1, MaxNrDsConfigs
        dioConfig % dsConfig(d) % logProp(dsProp) = value
    enddo

end subroutine DioSetDefaultDsLogProp


subroutine DioSetDefaultDsStrProp(dsProp, value)

    ! arguments
    integer, intent(IN)         :: dsProp  ! property to be set
    character(Len=*),intent(IN) :: value   ! value for property

    ! locals
    integer                  :: d       ! counter

    ! body
    call DioCheckInit
    dioConfig % dsDefaults % strProp(dsProp) = value
    do d = 1, MaxNrDsConfigs
        dioConfig % dsConfig(d) % strProp(dsProp) = value
    enddo

end subroutine DioSetDefaultDsStrProp


!
! Set Time Information for all datasets
!

subroutine DioSetStartTime(startTime)

    ! arguments
    character(Len=*),intent(IN) :: startTime
    
    ! body
    call DioCheckInit
    call DioSetDefaultDsStrProp(dio_ds_StartTime, startTime)

end subroutine DioSetStartTime


subroutine DioSetDeltaTime(deltaTime)

    ! arguments
    character(Len=*),intent(IN) :: deltaTime
    
    ! body
    call DioCheckInit
    call DioSetDefaultDsStrProp(dio_ds_DeltaTime, deltaTime)

end subroutine DioSetDeltaTime


!*
!* EXTERNAL Get Functions
!*

!
! Get a Settings property (integer, logical, string)
!

subroutine DioConfGetStIntProp(stProp, value)

    ! arguments
    integer, intent(IN)  :: stProp
    integer, intent(OUT) :: value

    ! body
    call DioCheckInit
    value = dioConfig % settings % intProp(stProp)

end subroutine DioConfGetStIntProp


subroutine DioConfGetStLogProp(stProp, value)

    integer, intent(IN)  :: stProp
    logical, intent(OUT) :: value

    ! body
    call DioCheckInit
    value = dioConfig % settings % logProp(stProp)

end subroutine DioConfGetStLogProp


subroutine DioConfGetStStrProp(stProp, value)

    ! arguments
    integer, intent(IN)          :: stProp
    character(Len=*),intent(OUT) :: value

    ! body
    value = dioConfig % settings % strProp(stProp)

end subroutine DioConfGetStStrProp


!
! Get Property for a dataset (integer, logical, string)
!

subroutine DioConfGetDsIntProp(dsName, dsProp, value)

    ! arguments
    character(Len=*)     :: dsName  ! dataset name
    integer, intent(IN)  :: dsProp  ! property to be retreived
    integer, intent(OUT) :: value   ! value for property

    ! locals
    type(DioDsConfigType), pointer :: ds

    ! body: get pointer to dataset <dsName> or to default,
    !       return property.
    call DioCheckInit
    ds => DioConfGetDsConfig(dsName)
    value = ds % intProp(dsProp)

end subroutine DioConfGetDsIntProp


subroutine DioConfGetDsLogProp(dsName, dsProp, value)

    ! arguments
    character(Len=*)     :: dsName  ! dataset name
    integer, intent(IN)  :: dsProp  ! property to be retreived
    logical, intent(OUT) :: value   ! value for property

    ! locals
    type(DioDsConfigType), pointer :: ds

    ! body: get pointer to dataset <dsName> or to default,
    !       return property.
    call DioCheckInit
    ds => DioConfGetDsConfig(dsName)
    value = ds  % logProp(dsProp)

end subroutine DioConfGetDsLogProp


subroutine DioConfGetDsStrProp(dsName, dsProp, value)

    ! arguments
    character(Len=*)     :: dsName     ! dataset name
    integer, intent(IN)  :: dsProp     ! property to be retreived
    character(Len=*), intent(OUT) :: value ! value for property

    ! locals
    type(DioDsConfigType), pointer :: ds

    ! body: get pointer to dataset <dsName> or to default,
    !       return property.
    call DioCheckInit
    ds => DioConfGetDsConfig(dsName)
    value = ds  % strProp(dsProp)

end subroutine DioConfGetDsStrProp


!
! Stop Dio (no real need to call it)
!

subroutine DioStop

    initialized = .false.
    call DioInitConfig
    call DioUpdateConfig
    initialized = .true.

end subroutine DioStop


end module Dio_ds_config


