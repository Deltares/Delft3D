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
!!! Dio-2d-field-rw: 2d field datasets on stream functions
!!!
!!! (c) Deltares, aug 2002
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module Dio_2dfield_rw

use Dio_ds
use Dio_3d_block

implicit none


!!
!! Types for checking dataset sizes (only used in this module)
!!
integer, parameter, private :: Dio_Ds2DFLarger  = 1
integer, parameter, private :: Dio_Ds2DFSmaller = 2


!!
!! Definition of data types
!!

!
! 2DF dataset header
!
type Dio2DFHeaderType

    integer                       :: varType     ! int, real, double, logical

    integer                       :: numM        ! first dimension
    integer                       :: numN        ! second dimension

end type Dio2DFHeaderType


!
! 2D Dataset
!
type Dio2DFType
    type(DioDsType)               :: ds          ! dataset
    type(Dio2DFHeaderType)        :: header      ! dataset header
    type(Dio3DType)               :: values      ! dataset values
end type Dio2DFType


!!
!! Module functions / subroutines
!!

!
! private procedures
!

!! private :: Dio2DFHeaderCreate
!! private :: Dio2DFCheckSizes

!!
!! Interfaces (overloaded procedures)
!!

interface Dio2DFCreate
    module procedure Dio2DFCreate
    module procedure Dio2DFCreateEmpty
end interface


interface Dio2DFSetValues
    module procedure Dio2DFSetValuesReal
    module procedure Dio2DFSetValuesDouble
    module procedure Dio2DFSetValuesInteger
end interface

interface Dio2DFGetValues
    module procedure Dio2DFGetValuesReal
    module procedure Dio2DFGetValuesDouble
    module procedure Dio2DFGetValuesInteger
end interface


interface Dio2DFDefine
    module procedure Dio2DFDefine
    module procedure Dio2DFDefineAutoStream
end interface

interface Dio2DFPut
    module procedure Dio2DFPutReals
    module procedure Dio2DFPutDoubles
    module procedure Dio2DFPutIntegers
end interface

interface Dio2DFGetDataset
    module procedure Dio2DFGetDataset
    module procedure Dio2DFGetDatasetAutoStream
end interface

interface Dio2DFGet
    module procedure Dio2DFGetReals
    module procedure Dio2DFGetDoubles
    module procedure Dio2DFGetIntegers
end interface

interface Dio2DFGetAll
    module procedure Dio2DFGetAllReals
end interface

contains


!!
!! PRIVATE Functions
!!

!
! Create header for an empty f2D-dataset
!
function Dio2DFHeaderCreate(varType) result(header)

    ! return value

    type(Dio2DFHeaderType)            :: header  ! dataset header

    ! arguments

    integer                           :: varType ! type of variable

    ! intialize header fielf2D to 'empty dataset'

    header % varType = varType
    header % numM = 0
    header % numN = 0

end function Dio2DFHeaderCreate
 

!
! check sizes of dataset
!
function Dio2DFCheckSizes(f2D, nP, nL, modus) result(ok)

    logical                       :: ok               ! sizes ok

    ! arguments

    type(Dio2DFType), intent(in)  :: f2D              ! dataset
    integer         , intent(in)  :: nP, nL           ! #pars, #locs
    integer         , intent(in)  :: modus            ! bigger or smaller

    ! body

    ok = .false.
    if ( modus .eq. Dio_Ds2DFLarger ) then
        !
        ! check if 2DF is large enough to store nP*nL values
        !
        ok = (f2D % header % numM .ge. nP .and. f2D % header % numN .ge. nL)
    else if ( modus .eq. Dio_Ds2DFSmaller ) then
        !
        ! check if 2DF's values can be stored in an nP*nL array
        !
        ok = (f2D % header % numM .le. nP .and. f2D % header % numN .le. nL)
    endif
    end function Dio2DFCheckSizes


!!
!! PUBLIC Functions Data Manipulation
!!
!!
!! Create/Destroy 2DF datasets
!!

!
! Create empty 2DF dataset
!
function Dio2DFCreateEmpty(name, varType) result(f2D)

    ! return value

    type(Dio2DFType)             :: f2D      ! dataset

    ! arguments

    character(Len=*), intent(in) :: name    ! dataset name
    integer                      :: varType ! type of variabe

    ! create the ds and the header
    f2D % ds     = DioDsCreate(name)
    f2D % header = Dio2DFHeaderCreate(varType)
    f2D % values = Dio3DCreate(varType)

end function Dio2DFCreateEmpty


!
! Create filled 2DF dataset
!

function Dio2DFCreate(name, varType, numM, numN) result(f2D)

    ! return value

    type(Dio2DFType)             :: f2D     ! dataset

    ! arguments

    character(Len=*), intent(in) :: name    ! dataset name
    integer         , intent(in) :: varType ! type of variabe
    integer         , intent(in) :: numM    ! first dim
    integer         , intent(in) :: numN    ! second dim

    ! body

    f2D = Dio2DFCreateEmpty(name, varType)
    f2D % header = Dio2DFHeaderCreate(varType)
    f2D % header % numM = numM
    f2D % header % numN = numN
    f2D % values = Dio3DCreate(varType, numM, numN)
 
end function Dio2DFCreate


!
! Destroy 2DF dataset
!

subroutine Dio2DFDestroy(f2D)

    ! arguments

    type(Dio2DFType)                :: f2D      ! dataset

    ! body

    call DioDsDestroy(f2D % ds)
    call Dio3DDestroy(f2D % values)
    
end subroutine Dio2DFDestroy


!!
!! Put/Get values to/from 2DF datasets
!!

!
! Put real values in dataset
!
subroutine Dio2DFSetValuesReal(f2D, values)

    ! arguments

    type(Dio2DFType)                 :: f2D       ! dataset
    real, dimension(:,:), intent(in) :: values    ! values for this step

    ! locals

    integer                          :: nP, nL    ! sizes

    ! get / check sizes; allocate and store value

    nP = size(values, 1)
    nL = size(values, 2)
    if ( Dio2DFCheckSizes(f2D, nP, nL, Dio_Ds2DFLarger ) ) then
        call Dio3DAllocateValues(f2D % values, DioDsDataSize(f2D % ds))
        f2D % values % reals(1:nP, 1:nL, f2D % ds % curDataIndex) = values(1:nP, 1:nL)
    endif

    return

end subroutine Dio2DFSetValuesReal


!
! Put double values in dataset
!
subroutine Dio2DFSetValuesDouble(f2D, values)

    ! arguments

    type(Dio2DFType)                 :: f2D       ! dataset
    double precision, dimension(:,:),&
                          intent(in) :: values    ! values for this step

    ! locals

    integer                          :: nP, nL    ! sizes

    ! get / check sizes; allocate and store value

    nP = size(values, 1)
    nL = size(values, 2)
    if ( Dio2DFCheckSizes(f2D, nP, nL, Dio_Ds2DFLarger ) ) then
        call Dio3DAllocateValues(f2D % values, DioDsDataSize(f2D % ds))
        f2D % values % doubles(1:nP, 1:nL, f2D % ds % curDataIndex)  = values(1:nP, 1:nL)
    endif

    return

end subroutine Dio2DFSetValuesDouble


!
! Put integer values in dataset
!
subroutine Dio2DFSetValuesInteger(f2D, values)

    ! arguments

    type(Dio2DFType)                   :: f2D    ! dataset
    integer, dimension(:,:), intent(in):: values ! values for this step

    ! locals

    integer                          :: nP, nL    ! sizes

    ! get / check sizes; allocate and store value

    nP = size(values, 1)
    nL = size(values, 2)
    if ( Dio2DFCheckSizes(f2D, nP, nL, Dio_Ds2DFLarger ) ) then
        call Dio3DAllocateValues(f2D % values, DioDsDataSize(f2D % ds))
        f2D % values % ints(1:nP, 1:nL, f2D % ds % curDataIndex) = values(1:nP, 1:nL)
    endif

    return

end subroutine Dio2DFSetValuesInteger


!
! Get a copy of the real values in dataset
!
function Dio2DFGetValuesReal(f2D, values) result(retVal)

    ! return value
    logical                          :: retVal   ! .true.: success

    ! arguments

    type(Dio2DFType)    , intent(in) :: f2D      ! dataset
    real, dimension(:,:), intent(out):: values   ! space for values, must be made
                                                 ! available by caller.
    ! body

    retVal = .false.

    if ( Dio2DFCheckSizes(f2D, &
            size(values, 1), size(values, 2), Dio_Ds2DFSmaller ) ) then
        values(1:f2D%header%numM, 1: f2D%header%numN) = f2D % values % reals(:,:,f2D % ds % curDataIndex)
        retVal = .true.
    endif

end function Dio2DFGetValuesReal


!
! Get a copy of the double values in dataset
!
function Dio2DFGetValuesDouble(f2D, values) result(retVal)

    ! return value
    logical                          :: retVal   ! .true.: success

    ! arguments

    type(Dio2DFType)    , intent(in) :: f2D      ! dataset
    double precision, dimension(:,:), &
                          intent(out):: values   ! space for values, must be made
                                                 ! available by caller.
    ! body

    retVal = .false.

    if ( Dio2DFCheckSizes(f2D, &
            size(values, 1), size(values, 2), Dio_Ds2DFSmaller ) ) then
        values(1:f2D%header%numM, 1: f2D%header%numN) = f2D % values % doubles(:,:,f2D % ds % curDataIndex)
        retVal = .true.
    endif

end function Dio2DFGetValuesDouble


!
! Get a copy of the integer values in dataset
!
function Dio2DFGetValuesInteger(f2D, values) result(retVal)

    ! return value
    logical                          :: retVal   ! .true.: success

    ! arguments

    type(Dio2DFType)    , intent(in)    :: f2D   ! dataset
    integer, dimension(:,:), intent(out):: values! space for values, must be made
                                                 ! available by caller.
    ! body

    retVal = .false.

    if ( Dio2DFCheckSizes(f2D, &
            size(values, 1), size(values, 2), Dio_Ds2DFSmaller ) ) then
        values(1:f2D%header%numM, 1: f2D%header%numN) = f2D % values % ints(:,:,f2D % ds % curDataIndex)
        retVal = .true.
    endif

end function Dio2DFGetValuesInteger


!!
!! Get information of a 2DF dataset
!!

!
! Get #Parameters
!
function Dio2DFGetNumM(f2D) result(dimSize)
    ! return value
    integer                          :: dimSize  ! dim.size
    ! arguments
    type(Dio2DFType)    , intent(in) :: f2D      ! dataset
    ! body
    dimSize = f2D % header % numM
end function Dio2DFGetNumM


!
! Get #Locations
!
function Dio2DFGetNumN(f2D) result(dimSize)
    ! return value
    integer                          :: dimSize  ! dim.size
    ! arguments
    type(Dio2DFType)    , intent(in) :: f2D      ! dataset
    ! body
    dimSize = f2D % header % numN
end function Dio2DFGetNumN


!
! Get #Timesteps
!
function Dio2DFGetNTimes(f2D) result(dimSize)

    ! return value
    integer                          :: dimSize  ! dim.size

    ! arguments
    type(Dio2DFType)                 :: f2D      ! dataset

    ! body: if #readTimes is 0, read times
    if ( f2D % ds % nPreReadTims .eq. 0 ) then
        call Dio2DFReadAllTimes(f2D)
    endif
    dimSize = f2D % ds % nPreReadTims

end function Dio2DFGetNTimes


!
! Get pointer to the timestep strings
!
function Dio2DFGetTimes(f2D) result(times)

    ! return value
    double precision, &
                  pointer, dimension(:):: times  ! pointer to julTimes
    ! arguments
    type(Dio2DFType)                   :: f2D    ! dataset

    ! body
    nullify(times)
    if ( f2D % ds % nPreReadTims .eq. 0 ) call Dio2DFReadAllTimes(f2D)
    if ( f2D % ds % nPreReadTims .gt. 0 ) times => f2D % ds % preReadTims

end function Dio2DFGetTimes



!!
!! PUBLIC Functions Read/Write
!!

!
! Write header to stream
!

subroutine Dio2DFWriteHeader(f2D)

    include 'dio-sync.inc'                  ! for synchronization functions

    ! arguments

    type(Dio2DFType), target        :: f2D                    ! dataset

    ! locals

    type(Dio2DFHeaderType), pointer :: header                 ! dataset header
    integer                         :: lun, sType             ! counter, stream lun/type

#if (defined(DIO_NEFIS_INCLUDED))
    integer                         :: retVal                 ! return value of Nefis call
                                                              ! (TODO: make current subr. a function)
    ! externals
    integer, external               :: Dio2DFWriteHeaderNefis 
#endif

    ! body

    header => f2D % header

    if ( f2D % ds % outStream % opened ) then

        sType = f2D % ds % outStream % streamType
        if ( associated(f2D % ds % outStream % shmHandle) ) then
            call Dio2DFWriteHeaderShm(f2D)
        else if ( DioSyncDsItemCanBeSent(f2D % ds) ) then
            if ( DioStreamConnect(f2D % ds % outStream, .false. ) ) then

                lun   = f2D % ds % outStream % lun

                if ( sType .eq. dio_Nefis_Stream ) then
#if (defined(DIO_NEFIS_INCLUDED))
                    retVal = Dio2DFWriteHeaderNefis(f2D)
#else
                    call DioStreamError(391, 'Nefis not supported (ds: ', f2D % ds % name, ')')
#endif

                else if ( sType .eq. Dio_ASCII_Stream) then

                    write(lun,*) 'Parameters ',header % numM
                    write(lun,*) 'Locations ', header % numN
                    write(lun,*) 'VarType ',   header % varType

                else if ( sType .eq. Dio_Binary_Stream) then

                    write(lun)                 header % numM
                    write(lun)                 header % numN
                    write(lun)                 header % varType

                endif

                call DioStreamDisconnect(f2D % ds % outStream, .false.)
            endif
            call DioSyncDsItemSent(f2D % ds)
        endif
    endif

end subroutine Dio2DFWriteHeader


!
! Write values to stream
!

subroutine Dio2DFWriteValues(f2D)

    include 'dio-sync.inc'            ! for synchronization functions

    ! arguments

    type(Dio2DFType)  :: f2D                   ! dataset

    ! locals

    integer           :: sType,lun             ! stream type and lun
#if (defined(DIO_NEFIS_INCLUDED))
    integer           :: retVal                ! return value of Nefis call
                                               ! (TODO: make current subr. a function)
    ! externals
    integer, external :: Dio3DWriteValuesNefis
#endif

    ! body

    if ( f2D % ds % outStream % opened ) then

        sType = f2D % ds % outStream % streamType
        if ( associated(f2D % ds % outStream % shmHandle) ) then
            if ( .not. Dio3DWriteValuesShm( f2D % values, f2D % ds % outStream % shmHandle, f2D % ds % curDataIndex ) ) then
                call DioStreamError(204, 'Could not write to shared memory block ', f2D % ds % name, ')')
            endif
        else
            if ( DioSyncDsItemCanBeSent(f2D % ds) ) then
                if ( DioStreamConnect(f2D % ds % outStream, .false. ) ) then

                    lun   = f2D % ds % outStream % lun

                    if ( sType .eq. dio_Nefis_Stream ) then
#if (defined(DIO_NEFIS_INCLUDED))
                        retVal = Dio3DWriteValuesNefis(f2D % ds, f2D % header % numM, f2D % header % numN, f2D % values)
#else
                        call DioStreamError(391, 'Nefis not supported (ds: ', f2D % ds % name, ')')
#endif
                    else if ( sType .eq. Dio_ASCII_Stream) then
                        call Dio3DWriteValuesASCII( f2D % values, lun, f2D % ds % curDataIndex )

                    else if ( sType .eq. Dio_Binary_Stream) then
                        call Dio3DWriteValuesBinary( f2D % values, lun, f2D % ds % curDataIndex )

                    endif

                    call DioStreamDisconnect(f2D % ds % outStream, .false.)
                endif
                call DioSyncDsItemSent(f2D % ds)
            endif
        endif
    endif

end subroutine Dio2DFWriteValues


subroutine Dio2DFReadHeader(f2D)

    include 'dio-sync.inc'                    ! for synchronization functions

    ! return value
    logical                         :: retVal ! .true. : success

    ! arguments
    type(Dio2DFType)      , target  :: f2D    ! dataset

    ! locals

    type(Dio2DFHeaderType), pointer :: header ! dataset header
    integer                 :: lun, sType     ! counter, stream lun/type
    character(DioMaxTimLen) :: dummy          ! dummy for char string
#if (defined(DIO_NEFIS_INCLUDED))
    logical, external       :: Dio2DFReadHeaderNefis ! read-funct.
#endif

    ! body

    retVal = .false.

    header => f2D % header

    if ( f2D % ds % inStream % opened ) then

        sType = f2D % ds % inStream % streamType

        if ( associated(f2D % ds % inStream % shmHandle) ) then
            call Dio2DFReadHeaderShm(f2D)
        else
            if (DioSyncDsItemAvailable(f2D % ds)) then
                if ( DioStreamConnect(f2D % ds % inStream, .false. ) ) then

                    lun   = f2D % ds % inStream % lun

                    if ( sType .eq. dio_Nefis_Stream ) then
#if (defined(DIO_NEFIS_INCLUDED))
                        retVal = Dio2DFReadHeaderNefis(f2D)
#else
                        call DioStreamError(391, 'Nefis not supported (ds: ', f2D % ds % name, ')')
#endif
                    else
                        retVal = .false.
                        if ( sType .eq. Dio_ASCII_Stream) then
                            read (lun,*, err=991, end=991) dummy,  header % numM
                            read (lun,*, err=991, end=991) dummy,  header % numN
                            read (lun,*, err=991, end=991) dummy,  header % varType
                        else if ( sType .eq. Dio_Binary_Stream) then
                            read (lun, err=991, end=991) header % numM
                            read (lun, err=991, end=991) header % numN
                            read (lun, err=991, end=991) header % varType
                        endif
                        retVal = .true.
                        goto 992
                    991 call DioStreamError(301, 'Error reading ASCII/BIN Header:', f2D % ds % name)
                    992 continue

                    endif
                    call DioStreamDisconnect(f2D % ds % inStream, .false.)
                endif
                call DioSyncDsItemReceived(f2D % ds)
            endif
        endif
        !
        ! Store type/size admin in values block
        !
        f2D % values % varType = f2D % header % varType
        f2D % values % numM    = f2D % header % numM
        f2D % values % numN    = f2D % header % numN

    endif

end subroutine Dio2DFReadHeader


function Dio2DFReadValues(varType, f2D) result(retVal)

    include 'dio-sync.inc'             ! for synchronization functions

    ! result value
    logical               :: retVal    ! .true. : success

    ! arguments
    type(Dio2DFType)      :: f2D       ! dataset
    integer ,  intent(in) :: varType   ! vartype to be read

    ! locals
    integer               :: sType,lun ! stream type and lun

    ! externals
#if (defined(DIO_NEFIS_INCLUDED))
    logical, external    :: Dio3DReadValuesNefis
#endif

    ! body

    retVal = .false.

    if ( f2D % ds % inStream % opened ) then

        sType = f2D % ds % inStream % streamType

        if ( associated(f2D % ds % inStream % shmHandle) ) then
            call Dio3DAllocateValues(f2D % values, DioDsDataSize(f2D % ds))
            retVal = Dio3DReadValuesShm( f2D % values, f2D % ds % inStream % shmHandle, f2D % ds % curDataIndex )
        else
            if (DioSyncDsItemAvailable(f2D % ds)) then
                if ( DioStreamConnect(f2D % ds % inStream, .false. ) ) then

                    call Dio3DAllocateValues(f2D % values, DioDsDataSize(f2D % ds))

                    lun   = f2D % ds % inStream % lun

                    if ( f2D % header % varType .eq. varType) then

                        if ( sType .eq. dio_Nefis_Stream ) then
#if (defined(DIO_NEFIS_INCLUDED))
                            retVal = Dio3DReadValuesNefis(f2D % ds, f2D % header % numM, &
                                                   f2D % header % numN, f2D % values)
#else
                            call DioStreamError(391, 'Nefis not supported (ds: ', f2D % ds % name, ')')
#endif

                        else if ( sType .eq. Dio_ASCII_Stream) then
                            retVal = Dio3DReadValuesASCII( f2D % values, lun, f2D % ds % curDataIndex )

                        else if ( sType .eq. Dio_Binary_Stream) then
                            retVal = Dio3DReadValuesBinary( f2D % values, lun, f2D % ds % curDataIndex )

                        endif

                    endif

                    call DioStreamDisconnect(f2D % ds % inStream, .false.)
                endif
                call DioSyncDsItemReceived(f2D % ds)
            endif
        endif
    endif

end function Dio2DFReadValues


function Dio2DFReadAllValues(varType, f2D) result(retVal)

    ! result value
   
    logical               :: retVal         ! .true. : success

    ! arguments
   
    type(Dio2DFType)      :: f2D            ! dataset
    integer ,  intent(in) :: varType        ! vartype to be read

    ! locals
   
    logical               :: readRes        ! result of read action

    ! body

    retVal = .false.

    if ( f2D % ds % inStream % opened ) then
        if ( f2D % header % varType .eq. varType) then
            f2d % ds % dataGrows = .true.
            f2d % ds % timeGrows = .true.
            readRes = .true.
            do while ( readRes )
                call DioDsIncreaseTimestep(f2D % ds)
                readRes = Dio2DFReadValues(varType, f2D)
                if ( readRes ) then
                    retVal = .true.
                else
                    call DioDsDecreaseTimestep(f2D % ds)
                endif
            enddo
        endif
    endif

end function Dio2DFReadAllValues


subroutine Dio2DFReadAllTimes(f2D)

    ! arguments
   
    type(Dio2DFType)  :: f2D     ! dataset

    ! body

    if ( f2D % ds % inStream % opened ) then
        if ( .not. f2D % ds % inStream % synched ) then
            select case (f2D % ds % inStream % streamType)

                case ( Dio_Nefis_Stream)
#if (defined(DIO_NEFIS_INCLUDED))
                    call Dio2DFNefisReadAllTimes(f2D)
#else
                    call DioStreamError(391, 'Nefis not supported (ds: ', f2D % ds % name, ')')
#endif
            end select
        endif
    endif

end subroutine Dio2DFReadAllTimes


!!
!! PUBLIC Interface functions
!!

!
! Define dataset
!

function Dio2DFDefine(stream, name, varType, numM, numN) result(f2D)

    ! return value
    
    type(Dio2DFType)                        :: f2D            ! dataset

    ! arguments
    
    type(DioStreamType), target, intent(in) :: stream         ! stream
    character(Len=*)           , intent(in) :: name           ! f2D name
    integer                    , intent(in) :: varType        ! type of var to be stored
    integer                    , intent(in) :: numM,numN      ! size

    ! body
    
    f2D = Dio2DFCreate(name, varType, numM, numN)

    f2D % ds % outStream = stream 

    call Dio2DFWriteHeader(f2D)

end function Dio2DFDefine


function Dio2DFDefineAutoStream(name, varType, numM, numN) result(f2D)

    ! return value
    
    type(Dio2DFType)              :: f2D             ! dataset

    ! arguments
    
    character(Len=*) , intent(in) :: name           ! f2D name
    integer          , intent(in) :: varType        ! type of var to be stored
    integer          , intent(in) :: numM,numN      ! size

    ! locals
    type(DioStreamType) :: stream         ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    f2D = Dio2DFDefine(stream, name, varType, numM, numN)

end function Dio2DFDefineAutoStream


subroutine Dio2DFPutOnStream(f2D, stream)

    ! arguments
    
    type(Dio2DFType)                        :: f2D     ! dataset
    type(DioStreamType), target, intent(in) :: stream  ! stream

    ! body

    f2D % ds % outStream = stream 
    call Dio2DFWriteHeader(f2D)

end subroutine Dio2DFPutOnStream


!
! Put functions
!

subroutine Dio2DFPutReals(f2D, values)

    ! arguments

    type(Dio2DFType)                                :: f2D      ! dataset
    real              , dimension(:,:), intent(in)  :: values

    ! body

    call DioDsIncreaseTimestep(f2D % ds)
    call Dio2DFSetValues(f2D, values)
    call Dio2DFWriteValues(f2D)

    return

end subroutine Dio2DFPutReals


subroutine Dio2DFPutDoubles(f2D, values)

    ! arguments

    type(Dio2DFType)                              :: f2D      ! dataset
    double precision, dimension(:,:), intent(in)  :: values

    ! body

    call DioDsIncreaseTimestep(f2D % ds)
    call Dio2DFSetValues(f2D, values)
    call Dio2DFWriteValues(f2D)

    return

end subroutine Dio2DFPutDoubles


subroutine Dio2DFPutIntegers(f2D, values)

    ! arguments

    type(Dio2DFType)                                :: f2D      ! dataset
    integer           , dimension(:,:), intent(in)  :: values

    ! body

    call DioDsIncreaseTimestep(f2D % ds)
    call Dio2DFSetValues(f2D, values)
    call Dio2DFWriteValues(f2D)

    return

end subroutine Dio2DFPutIntegers


!
! Get functions
!

function Dio2DFGetDataset(stream, name) result(f2D)

    ! return value

    type(Dio2DFType)                        :: f2D     ! dataset

    ! arguments

    type(DioStreamType), target, intent(in) :: stream  ! stream
    character(Len=*)           , intent(in) :: name    ! dataset name

    ! body

    f2D = Dio2DFCreate(name, Dio_Plt_Unknown)
    f2D % ds % inStream = stream

    call Dio2DFReadHeader(f2D)

end function Dio2DFGetDataset


function Dio2DFGetDatasetAutoStream(name) result(f2D)

    ! return value

    type(Dio2DFType)             :: f2D       ! dataset

    ! arguments

    character(Len=*), intent(in) :: name    ! dataset name

    ! locals

    type(DioStreamType)     :: stream         ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'r')
    f2D = Dio2DFGetDataset(stream, name)

end function Dio2DFGetDatasetAutoStream


function Dio2DFGetReals(f2D, values) result(retVal)

    ! result value
   
    logical                        :: retVal        ! .true. : success

    ! arguments

    type(Dio2DFType)               :: f2D           ! dataset
    real, dimension(:,:), pointer  :: values        ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(f2D % ds)
    if (Dio2DFReadValues(Dio_Plt_Real, f2D)) then
        values => f2D % values % reals(:,:, f2D % ds % curDataIndex )
    else
        call DioDsDecreaseTimestep(f2D % ds)
    endif

    retVal = associated(values)

end function Dio2DFGetReals


function Dio2DFGetDoubles(f2D, values) result(retVal)

    ! return value

    logical                        :: retVal        ! .true. : success

    ! arguments

    type(Dio2DFType)                :: f2D         ! dataset
    double precision, dimension(:,:), pointer     :: values     ! pointer to values

    nullify(values)
    
    call DioDsIncreaseTimestep(f2D % ds)
    if (Dio2DFReadValues(Dio_Plt_Double, f2D)) then
        values => f2D % values % doubles(:,:, f2D % ds % curDataIndex ) ! , f2D % ds % curDataIndex )
    else
        call DioDsDecreaseTimestep(f2D % ds)
    endif

    retVal = associated(values)

end function Dio2DFGetDoubles


function Dio2DFGetIntegers(f2D, values) result(retVal)

    ! result value
   
    logical                         :: retVal     ! .true. : success

    ! arguments

    type(Dio2DFType)                :: f2D        ! dataset
    integer, dimension(:,:), pointer:: values     ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(f2D % ds)
    if (Dio2DFReadValues(Dio_Plt_Integer, f2D)) then
        values => f2D % values % ints(:,:, f2D % ds % curDataIndex )
    else
        call DioDsDecreaseTimestep(f2D % ds)
    endif

    retVal = associated(values)

end function Dio2DFGetIntegers


function Dio2DFGetAllReals(f2D) result(values)

    ! return value

    real, dimension(:,:,:), pointer :: values     ! pointer to values

    ! arguments

    type(Dio2DFType)                :: f2D         ! dataset

    nullify(values)
    
    if (Dio2DFReadAllValues(Dio_Plt_Real, f2D)) then
        values => f2D % values % reals
    endif

end function Dio2DFGetAllReals


end module Dio_2dfield_rw
