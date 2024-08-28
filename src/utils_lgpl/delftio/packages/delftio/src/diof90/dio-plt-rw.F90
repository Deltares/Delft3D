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
!!! Dio-DS-PLT-RW: Param./Loc./Time datasets
!!!
!!! (c) Deltares, dec 2000
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!! File organization:
!!!
!!!    Declarations
!!!    - decl. of public constants, enumerations, datatypes and interfaces
!!!    - decl. of private module functions/subroutines and constants
!!!
!!!    Implementation of private general functions
!!!    - data manipulation functions (create/destroy plt, set header parts)
!!!    - read/write PLT
!!!
!!!    Implementation of public functions
!!!    - public functions for defining a PLT header information
!!!    - public functions for getting PLT header information
!!!    - public functions to set/get values in PLT
!!!    - public functions to read/write PLT
!!!
!!!    Implementation of private File Type Specific functions
!!!    - writing/reading HIS file
!!!    - writing/reading HIA file
!!!    - writing/reading ASCII/BIN format
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module Dio_plt_rw

use Dio_ini
use Dio_ds
use Dio_3d_block

implicit none

!******************************************************************************
!* PUBLIC CONSTANTS, ENUMERATIONS, DATATYPES and INTERFACES
!******************************************************************************

!*
!* Constants/enumerations
!*

!
! PLT String sizes
!
integer, parameter :: DioMaxParLen    = 256   ! max len of parName string
integer, parameter :: DioMaxLocLen    = 256  ! max len of locName string
integer, parameter :: DioMaxDescrLen  = 256  ! max len of descr. string

!
! PLT String sizes for HIS Files
!
integer, parameter :: HisStringSize   = 20 ! len of par|loc string in HIS files
integer, parameter :: HisRunIdSize    = 40 ! len of runid string in HIS files
integer, parameter :: HisRunIdDim     = 4  ! #runid strings in HIS files

!
! Description types:
!
integer, parameter, public :: dio_plt_pars = 1
integer, parameter, public :: dio_plt_locs = 2


!*
!* Definition of data types
!*

!
! PLT dataset header
!
type DioPltHeaderType

    integer                       :: varType      ! int, real, double, logical

    integer                       :: nPar         ! #parameters
    integer                       :: nLoc         ! #locations

    character(len=DioMaxParLen), pointer, &
                     dimension(:) :: pars         ! parameter names
    character(len=DioMaxLocLen), pointer, &
                     dimension(:) :: locs         ! location names

    character(len=DioMaxDescrLen), pointer, &
                     dimension(:) :: parDescripts ! parameter descriptions
    character(len=DioMaxDescrLen), pointer, &
                     dimension(:) :: locDescripts ! location descriptions

    !
    ! Specific data for HIS files
    !
    character(len=HisRunIdSize), &
             pointer, dimension(:):: hisRunId       ! runid for HIS files
    integer, pointer, dimension(:):: hisIntIds      ! location number for HIS files
    logical                       :: hasRunid       ! Plt contains HIS information
    logical                       :: allowJumpBack  ! while writing, his file can jump back in time

end type DioPltHeaderType


!
! PLT Dataset
!
type DioPltType
    type(DioDsType)               :: ds          ! dataset
    type(DioPltHeaderType)        :: header      ! dataset header
    type(Dio3DType)               :: values      ! dataset values
end type DioPltType


!*
!* PUBLIC INTERFACES (overloaded functions)
!*

interface DioPltCreate
    module procedure DioPltCreate
    module procedure DioPltCreateEmpty
    module procedure DioPltCreateMap
end interface

interface DioPltSetValues
    module procedure DioPltSetValuesReal
    module procedure DioPltSetValuesDouble
    module procedure DioPltSetValuesInteger
end interface

interface DioPltSetValues1D
    module procedure DioPltSetValues1DReal
    module procedure DioPltSetValues1DDouble
end interface

interface DioPltGetValues
    module procedure DioPltGetRealValues
    module procedure DioPltGetDoubleValues
    module procedure DioPltGetIntegerValues
end interface


interface DioPltGetCurHisStep
    module procedure DioPltGetCurHisStep
    module procedure DioPltGetCurHisStepAndUnit
end interface


interface DioPltDefine 
    module procedure DioPltDefine
    module procedure DioPltDefineMap
    module procedure DioPltDefineWithJulian
    module procedure DioPltDefineWithJulianMap
    module procedure DioPltDefineWithRunid
    module procedure DioPltDefineWithRunidMap
    module procedure DioPltDefineWithRIDAndJul
    module procedure DioPltDefineWithRIDAndJulMap
    module procedure DioPltDefineWithIntIds
    module procedure DioPltDefineWithRunidAndIntIds
    module procedure DioPltDefineWithRunidIntsJul
    module procedure DioPltDefAutoStream
    module procedure DioPltDefAutoStreamMap
    module procedure DioPltDefAutoWithJulian
    module procedure DioPltDefAutoWithJulianMap
    module procedure DioPltDefAutoWithRunid
    module procedure DioPltDefAutoWithRunidMap
    module procedure DioPltDefAutoWithRIDAndJul
    module procedure DioPltDefAutoWithRIDAndJulMap
    module procedure DioPltDefAutoWithIntIds
    module procedure DioPltDefAutoWithRIDAndInts
    module procedure DioPltDefAutoWithRunidIntsJul1
    module procedure DioPltDefAutoWithRunidIntsJul2
end interface


interface DioPltPut
    module procedure DioPltPutReals
    module procedure DioPltPutDoubles
    module procedure DioPltPutIntegers
    module procedure DioPltPutHisStepReals
    module procedure DioPltPutHisStepDoubles
    module procedure DioPltPutHisStepIntegers
    module procedure DioPltPutJulTimeReals
    module procedure DioPltPutJulTimeDoubles
    module procedure DioPltPutJulTimeIntegers
    module procedure DioPltPutHisStepReals1D
    module procedure DioPltPutHisStepDoubles1D
end interface


interface DioPltGetDataset
    module procedure DioPltGetDataset
    module procedure DioPltGetDatasetAutoStream
end interface


interface DioPltGet
    module procedure DioPltGetReals
    module procedure DioPltGetDoubles
    module procedure DioPltGetIntegers
    module procedure DioPltGetHisStepReals
    module procedure DioPltGetHisStepDoubles
    module procedure DioPltGetHisStepIntegers
    module procedure DioPltGetJulTimeReals
    module procedure DioPltGetJulTimeDoubles
    module procedure DioPltGetJulTimeIntegers
end interface


interface DioPltGetSelection
    module procedure DioPltGetSelectionReals
    module procedure DioPltGetSelectionDoubles
    module procedure DioPltGetSelOneTimeStepReals
    module procedure DioPltGetSelOneTimeStepDoubles
end interface


interface DioPltGetAll
    module procedure DioPltGetAllReals
end interface


!******************************************************************************
!* DECL. OF PRIVATE FUNCTIONS/SUBROUTINES AND CONSTANTS
!******************************************************************************


!*
!* prive functions/subroutines
!*

private :: DioPltHeaderCreateEmpty
private :: DioPltHeaderCreate
private :: DioPltHeaderCreateMap
private :: DioPltHeaderDestroy
private :: DioPltCheckSizes


!*
!* prive constants
!*

!
! Sizes for jumping in HIS File
!
integer, parameter :: integerSize  = 4       ! #bytes in a long
integer, parameter :: realSize     = 4       ! #bytes in a real

!
! Types for checking dataset sizes
!
integer, private, parameter :: Dio_DsPltLarger  = 1 ! Larger then data to be stored?
integer, private, parameter :: Dio_DsPltSmaller = 2 ! Smaller then data to be retrieved?


!
! String lengths for HIA file
!
integer, private, parameter :: DioHiaLineLen = 256 ! HIA line len
integer, private, parameter :: HiaStrLen     = 4   ! hia ext./vers. strings

!
! Format for Long Par/Loc Items in HIA file
!
character(len=20), &
        private, parameter :: ItemIndexFormat = '(I6.6,'' '',A,A,I8)', &
                              ItemIdFormat    = '(I6.6,'' '',A,A,A)'
integer, private, parameter:: ItemNrLen  = 7   ! Must be equal to n+1 in 'In.n'
                                               ! (index + space)

!
! HIA file extension/version
!
character(len=HiaStrLen), &
        private, parameter :: HiaVersion = '1.00' ! version string
character(Len=HiaStrLen), &
        private, parameter :: HisLongExt = '.hia' ! HIS->HIA extension
character(Len=HiaStrLen), &
        private, parameter :: MapLongExt = '.maa' ! MAP->MAA extension


!
! Groups and Keywords in HIA file
!
character(len=DioHiaLineLen), &
        private, parameter :: GeneralGroup     = 'General'               , &
                              DioCheckGroup    = 'DioCheck'              , &
                              LongLocGroup     = 'Long Locations'        , &
                              LongParGroup     = 'Long Parameters'       , &
                              ParDescriptGroup  = 'Parameter Descriptions', &
                              LocDescriptGroup  = 'Location Descriptions'

character(len=DioHiaLineLen), &
        private, parameter :: FileTypeKey   = 'FileType'           , &
                              DioVersKey    = 'DioVersion'         , &
                              HiaVersKey    = 'HiaVersion'         , &
                              HiaStatusKey  = 'HiaStatus'          , &
                              NumParsKey    = 'NumberOfParameters' , &
                              NumLocsKey    = 'NumberOfLocations'  , &
                              T0Key         = 'T0'                 , &
                              TSUnitKey     = 'TimeStepUnit'       , &
                              TSMultKey     = 'TimeStepMultiplyer'


!******************************************************************************
!* PLT DATASETSET COLLECTION FOR F77 INTERFACE (integer handles = indices)
!******************************************************************************

integer, parameter, private           :: dioMaxPLTs  = 50 ! max #datasets
integer, private                      :: nPlts       = 0  ! actual #datasets

type(DioPltType), dimension(dioMaxPLTs), target :: PltDataset  ! datasets


contains


!******************************************************************************
!* PUBLIC FUNCTIONS on DATASET COLLECTION FOR F77 INTERFACE 
!******************************************************************************


function DioPltNewF77Handle() result(retVal)

    ! return value
    integer  :: retVal  ! >0 : success

    ! locals
    integer  :: p ! loop counter

    retVal = 0
    if (nPlts < dioMaxPLTs ) then
        nPlts = nPlts + 1
        PltDataset(nPlts) % ds % id = nPlts
        retVal = nPlts
    else
        do p = 1 , nPlts
            if ( PltDataset(p) % ds % id == -1 ) then
                PltDataset(p) % ds % id = p
                retVal = p
                exit
            endif 
        enddo
    endif

    if ( retVal == 0 ) then
        call DioStreamError(405, 'DioPltNewF77Handle: Out of PLT-handles')
    endif

end function DioPltNewF77Handle


function DioPltF77HandleIsValid(handle) result(retVal)

    ! return value
    logical :: retVal   ! .true. : success

    ! arguments
    integer  :: handle  ! plt-handle in F77 array

    ! locals
    character(Len=DioMaxStreamLen) :: errorString

    ! body

    retVal = .false.
    if ( handle > 0 .and. handle <= nPlts .and. &
                PltDataset(handle) % ds % id == handle ) then
        retVal = .true.
    else
        write(errorString, '(A,A,I4)' ) 'Invalid PLT Handle', ': ', handle
        call DioStreamError(406, errorString)
    endif

end function DioPltF77HandleIsValid


subroutine DioPltReleaseF77Handle(handle)

    ! arguments
    integer  :: handle  ! plt-handle in F77 array

    if (DioPltF77HandleIsValid(handle)) then
        PltDataset(handle) % ds % id = -1
    endif

end subroutine DioPltReleaseF77Handle


!******************************************************************************
!******************************************************************************
!* PRIVATE PLT FUNCTIONS
!******************************************************************************
!******************************************************************************


!******************************************************************************
!* PRIVATE PLT DATA MANIPULATION FUNCTIONS
!******************************************************************************

!*
!* Create/Destroy LPT Header
!*

!
! Create header for an empty plt-dataset
!
function DioPltHeaderCreateEmpty(varType) result(header)

    ! return value

    type(DioPltHeaderType)            :: header  ! dataset header

    ! arguments

    integer                           :: varType ! type of variable

    ! intialize header fielplt to 'empty dataset'

    header % varType = varType

    header % nPar = 0
    header % nLoc = 0
    nullify(header % pars)
    nullify(header % locs)

    nullify(header % parDescripts)
    nullify(header % locDescripts)

    ! specific for His files
    nullify(header % hisRunId)
    nullify(header % hisIntIds)
    header % hasRunid = .false.
    header % allowJumpBack = .false.

end function DioPltHeaderCreateEmpty


!
! Create filled header for a plt-dataset
!
function DioPltHeaderCreate(varType, pars, locs) result(header)

    ! return value

    type(DioPltHeaderType)                  :: header    ! dataset header

    ! arguments

    integer                       , intent(in) :: varType ! type of variabe
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! locals

    integer                                 :: nP, nL  ! #pars, #locs

    header = DioPltHeaderCreateEmpty(varType)

    nP = size(pars)
    nL = size(locs)

    ! allocate space for par./loc. names,
    ! store the par./loc. names

    if ( nP > 0 ) then
        allocate(header % pars(nP))
        header % pars = pars
        header % nPar = nP
    endif
    if ( nL > 0 ) then
        allocate(header % locs(nL))
        header % locs = locs
        header % nLoc = nL
    endif

end function DioPltHeaderCreate


function DioPltHeaderCreateMap(varType, pars, nLoc) result(header)

    ! return value

    type(DioPltHeaderType)                 :: header  ! dataset header

    ! arguments

    integer                      , intent(in) :: varType ! type of variabe
    character(Len=*),dimension(:), intent(in) :: pars    ! parameter names
    integer                      , intent(in) :: nLoc    ! #locations

    ! locals

    integer                                 :: nP      ! #pars

    header = DioPltHeaderCreateEmpty(varType)

    nP = size(pars)

    ! allocate space for par. names,
    ! store the par.names and sizes

    header % nPar = nP
    if ( nP > 0 ) then
        allocate(header % pars(nP))
        header % pars = pars
    endif
    header % nLoc = nLoc

end function DioPltHeaderCreateMap


!
! destroy PLT dataset header
!
subroutine DioPltHeaderDestroy(header)

    ! arguments

    type(DioPltHeaderType)  :: header    ! dataset header

    ! body

    if (associated(header % pars)) then
        deallocate(header % pars) ; nullify(header % pars)
    endif
    if (associated(header % locs)) then
        deallocate(header % locs) ; nullify(header % locs)
    endif

    if (associated(header % hisRunID)) then
        deallocate(header % hisRunID) ; nullify(header % hisRunID)
    endif
    if (associated(header % hisIntIds)) then
        deallocate(header % hisIntIds) ; nullify(header % hisIntIds)
    endif

end subroutine DioPltHeaderDestroy
 

!*
!* Check sizes of dataset versus values to be restored/retrieved
!*

function DioPltCheckSizes(plt, nP, nL, modus) result(ok)

    logical                       :: ok               ! sizes ok

    ! arguments

    type(DioPltType), intent(in)  :: plt              ! dataset
    integer         , intent(in)  :: nP, nL           ! #pars, #locs
    integer         , intent(in)  :: modus            ! bigger or smaller

    ! body

    ok = .false.
    if ( modus .eq. Dio_DsPltLarger ) then
        !
        ! check if PLT is large enough to store nP*nL values
        !
        ok = (plt % header % nPar .ge. nP .and. plt % header % nLoc .ge. nL)
        if ( .not. ok ) then
            call DioStreamError(201, 'Size of PLT Array in PUT larger then NPAR/NLOC for ', plt % ds % name)
        endif
    else if ( modus .eq. Dio_DsPltSmaller ) then
        !
        ! check if PLT's values can be stored in an nP*nL array
        !
        ok = (plt % header % nPar .le. nP .and. plt % header % nLoc .le. nL)
        if ( .not. ok ) then
            call DioStreamError(202, 'Size of PLT Array in GET smaller then NPAR/NLOC for ', plt % ds % name)
        endif
    endif
    end function DioPltCheckSizes


!******************************************************************************
!* PRIVATE FUNCTIONS TO READ/WRITE PLT
!******************************************************************************

!
! Write PLT header.
!
subroutine DioPltWriteHeader(plt)

    include 'dio-sync.inc'   ! for synchronization functions

    ! arguments

    type(DioPltType), target        :: plt         ! dataset

    ! locals

    type(DioPltHeaderType), pointer :: header      ! dataset header
    integer                         :: sType       ! stream type

    ! body: switch on stream type

    header => plt % header

    if ( plt % ds % outStream % opened ) then

        sType = plt % ds % outStream % streamType

        if ( associated(plt % ds % outStream % shmHandle) ) then
            call DioPltWriteHeaderShm(plt)
        else if ( DioSyncDsItemCanBeSent(plt % ds) ) then
            if ( DioStreamConnect(plt % ds % outStream, .false. ) ) then

                !
                ! Perform StreamType specific actions
                !
                select case(sType)

                    case (dio_His_Stream, dio_WQMap_Stream)
                        call DioPltHisWriteHeader(plt)

                    case (Dio_ASCII_Stream, Dio_Binary_Stream)
                        call DioPltWriteHeaderASCIIOrBIN(plt)

                    case (dio_Nefis_Stream)
#if (defined(DIO_NEFIS_INCLUDED))
                        call DioPltWriteHeaderNefis(plt)
#else
                        call DioStreamError(291, 'Nefis not supported (ds: ', &
                                                plt % ds % name, ')')
#endif
                end select

                call DioStreamDisconnect(plt % ds % outStream, .false.)
            endif
            call DioSyncDsItemSent(plt % ds)
        endif
        call DioPltFreeHeader(plt)
    endif

end subroutine DioPltWriteHeader


!
! Write PLT values
!
subroutine DioPltWriteValues(plt)

    include 'dio-sync.inc'            ! for synchronization functions

    ! arguments

    type(DioPltType)  :: plt          ! dataset

    ! locals

    integer           :: sType,lun    ! stream type and lun

    ! body: switch on stream type

    if ( plt % ds % outStream % opened ) then

        sType = plt % ds % outStream % streamType

        if ( associated(plt % ds % outStream % shmHandle) ) then
            if ( .not. Dio3DWriteValuesShm( plt % values, plt % ds % outStream % shmHandle, plt % ds % curDataIndex ) ) then
                call DioStreamError(204, 'Could not write to shared memory block ', plt % ds % name, ')')
            endif
        else if ( DioSyncDsItemCanBeSent(plt % ds) ) then
            if ( DioStreamConnect(plt % ds % outStream, .false. ) ) then

                !
                ! Perform StreamType specific actions
                !
                lun   = plt % ds % outStream % lun

                select case(sType)

                    case (dio_His_Stream, dio_WQMap_Stream)
                        call DioPltHisWriteValues(plt, lun)

                    case (Dio_ASCII_Stream)
                        call Dio3DWriteValuesASCII ( plt % values, lun, &
                                                    plt % ds % curDataIndex )

                    case (Dio_Binary_Stream)
                        call Dio3DWriteValuesBinary( plt % values, lun, &
                                                    plt % ds % curDataIndex )

                    case (dio_Nefis_Stream)
#if (defined(DIO_NEFIS_INCLUDED))
                        call Dio3DWriteValuesNefis(plt % ds, plt % header % nPar, &
                                                plt % header % nLoc, plt % values)
#else
                        call DioStreamError(291, 'Nefis not supported (ds: ', &
                                                plt % ds % name, ')')
#endif
                end select

                call DioStreamDisconnect(plt % ds % outStream, .false.)
            endif
            call DioSyncDsItemSent(plt % ds)
        endif
    endif

end subroutine DioPltWriteValues


!
! Read PLT header
!
subroutine DioPltReadHeader(plt)

    include 'dio-sync.inc'         ! for synchronization functions
    include 'dio-time-support.inc' ! for time support functions

    ! return value (TODO)

    logical                         :: retVal
    ! arguments

    type(DioPltType)      , target  :: plt ! dataset

    ! locals

    type(DioPltHeaderType), pointer :: header      ! dataset header
    integer                         :: sType       ! stream type

    ! body: switch on stream type

    header => plt % header

    retVal = .false.

    if ( plt % ds % inStream % opened ) then

        sType = plt % ds % inStream % streamType

        if ( associated(plt % ds % inStream % shmHandle) ) then
            call DioPltReadHeaderShm(plt)
        else if (DioSyncDsItemAvailable(plt % ds)) then
            if ( DioStreamConnect(plt % ds % inStream, .false. ) ) then

                !
                ! Free current name strings, if filled
                !
                if ( associated(header % pars) ) then
                    deallocate(header % pars) ; nullify(header % pars)
                endif
                if ( associated(header % locs) ) then
                    deallocate(header % locs) ; nullify(header % locs)
                endif

                select case(sType)

                    case (dio_His_Stream, dio_WQMap_Stream)
                        retVal = DioPltHisReadHeader(plt)

                    case (Dio_ASCII_Stream, Dio_Binary_Stream)
                        call DioPltReadHeaderASCIIorBIN(plt)
                        retVal = .true.

                    case (dio_Nefis_Stream)

#if (defined(DIO_NEFIS_INCLUDED))
                        call DioPltReadHeaderNefis(plt)
                        retVal = .true.
#else
                        call DioStreamError(291, 'Nefis not supported (ds: ', &
                                                plt % ds % name, ')')
#endif
                end select

                call DioStreamDisconnect(plt % ds % inStream, .false.)
            endif
            call DioSyncDsItemReceived(plt % ds)
        endif
        !
        ! Store type/size admin in values block
        !
        plt % values % varType = plt % header % varType
        plt % values % numM    = plt % header % nPar
        plt % values % numN    = plt % header % nLoc

    endif

end subroutine DioPltReadHeader


!
! Read PLT Values
!
function DioPltReadValues(varType, plt) result(retVal)

    include 'dio-sync.inc'            ! for synchronization functions

    ! result value
    logical              :: retVal    ! .true. : success

    ! arguments
    type(DioPltType)     :: plt       ! dataset
    integer,  intent(in) :: varType   ! vartype to be read

    ! locals
    integer              :: sType,lun ! stream type and lun

    ! externals
#if (defined(DIO_NEFIS_INCLUDED))
    logical, external    :: Dio3DReadValuesNefis
#endif

    ! body: switch on stream type
    retVal = .false.

    if ( plt % ds % inStream % opened ) then

        sType = plt % ds % inStream % streamType

        call DioPltFreeHeader(plt)

        if ( associated(plt % ds % inStream % shmHandle) ) then
            call Dio3DAllocateValues(plt % values, DioDsDataSize(plt % ds))
            retVal = Dio3DReadValuesShm( plt % values, plt % ds % inStream % shmHandle, plt % ds % curDataIndex )
        else if (DioSyncDsItemAvailable(plt % ds)) then
            if ( DioStreamConnect(plt % ds % inStream, .false. ) ) then

                call Dio3DAllocateValues(plt % values, DioDsDataSize(plt % ds))

                lun   = plt % ds % inStream % lun

                if ( plt % header % varType .eq. varType) then

                    select case(sType)

                        case (dio_His_Stream, dio_WQMap_Stream)
                            retVal = DioPltHisReadValues(plt, lun)

                        case (Dio_ASCII_Stream)
                            retVal = Dio3DReadValuesASCII ( plt % values, lun, &
                                                        plt % ds % curDataIndex )

                        case (Dio_Binary_Stream)
                            retVal = Dio3DReadValuesBinary( plt % values, lun, &
                                                        plt % ds % curDataIndex )

                        case (dio_Nefis_Stream)
#if (defined(DIO_NEFIS_INCLUDED))
                            retVal = Dio3DReadValuesNefis(plt % ds, plt % header % nPar, &
                                                       plt % header % nLoc, plt % values)
#else
                            call DioStreamError(291, 'Nefis not supported (ds: ', &
                                                    plt % ds % name, ')')
#endif
                    end select

                    if ( (.not. retVal) ) then
                         ! synched streams may serve for synchronization only
                         if ( (plt % ds % inStream % synched ) .and. &
                              (plt % header % nPar == 0)       .and. &
                              (plt % header % nLoc == 0)               ) retVal = .true.
                    endif
                endif

                call DioStreamDisconnect(plt % ds % inStream, .false.)
            endif
            call DioSyncDsItemReceived(plt % ds)
        endif

    endif

end function DioPltReadValues


!
! Read ALL PLT Values
!
function DioPltReadAllValues(varType, plt) result(retVal)

    ! result value
   
    logical                          :: retVal         ! .true. : success

    ! arguments
   
    type(DioPltType)               :: plt             ! dataset
    integer            ,  intent(in) :: varType        ! vartype to be read

    ! locals
   
    logical                          :: readRes         ! result of read action

    ! body

    retVal = .false.

    if ( plt % ds % inStream % opened ) then
        if ( plt % header % varType .eq. varType) then
            plt % ds % dataGrows = .true.
            plt % ds % timeGrows = .true.
            readRes = .true.
            do while ( readRes )
                call DioDsIncreaseTimestep(plt % ds)
                readRes = DioPltReadValues(varType, plt)
                if ( readRes ) then
                    retVal = .true.
                else
                    call DioDsDecreaseTimestep(plt % ds)
                endif
            enddo
        endif
    endif

end function DioPltReadAllValues


!
! Function to read all timesteps in file
!

subroutine DioPltReadAllTimes(plt)

    ! arguments
   
    type(DioPltType) :: plt        ! dataset

    ! body

    if ( plt % ds % inStream % opened ) then
        if ( .not. plt % ds % inStream % synched ) then
            select case (plt % ds % inStream % streamType)

                case ( Dio_HIS_Stream, Dio_WQMap_Stream)
                    if ( plt % header % varType .eq. Dio_PLT_Real ) then
                        call DioPltHisReadAllTimes(plt)
                    endif

            end select
        endif
    endif

end subroutine DioPltReadAllTimes


!******************************************************************************
!******************************************************************************
!* PUBLIC FUNCTIONS
!******************************************************************************
!******************************************************************************


!******************************************************************************
!* PUBLIC FUNCTIONS TO CREATE/DESTROY PLT AND ITS HEADER INFO
!* - Create / Destroy PLT
!* - Free part of header
!* - Set several parts of header
!******************************************************************************

!*
!* Create/Destroy LPT dataset
!*

!
! Create empty PLT dataset
!
function DioPltCreateEmpty(name, varType) result(plt)

    ! return value

    type(DioPltType)              :: plt     ! dataset

    ! arguments

    character(Len=*) , intent(in) :: name    ! dataset name
    integer                       :: varType ! type of variabe

    ! create the ds and the header
    plt % ds     = DioDsCreate(name)
    plt % header = DioPltHeaderCreateEmpty(varType)
    plt % values = Dio3DCreate(varType)

end function DioPltCreateEmpty


!
! Create PLT dataset with parameter and location names
!
function DioPltCreate(name, varType, pars, locs) result(plt)

    ! return value

    type(DioPltType)                           :: plt      ! dataset

    ! arguments

    character(Len=*)              , intent(in) :: name    ! dataset name
    integer                       , intent(in) :: varType ! type of variable
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! body

    plt = DioPltCreateEmpty(name, varType)
    plt % header = DioPltHeaderCreate(varType, pars, locs)
    plt % values = Dio3DCreate(varType, size(pars), size(locs))
 
end function DioPltCreate


!
! Create PLT dataset with parameter and location names
!
function DioPltCreateMap(name, varType, pars, nLoc) result(plt)

    ! return value

    type(DioPltType)                           :: plt     ! dataset

    ! arguments

    character(Len=*)              , intent(in) :: name    ! dataset name
    integer                       , intent(in) :: varType ! type of variabe
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer                       , intent(in) :: nLoc    ! #locations

    ! body

    plt = DioPltCreateEmpty(name, varType)
    plt % header = DioPltHeaderCreateMap(varType, pars, nLoc)
    plt % values = Dio3DCreate(varType, size(pars), nLoc)
 
end function DioPltCreateMap


!
! Destroy PLT dataset
!
subroutine DioPltDestroy(plt)

    ! arguments

    type(DioPltType)                :: plt      ! dataset

    ! body

    call DioDsDestroy(plt % ds)
    call DioPltHeaderDestroy(plt % header)
    call Dio3DDestroy(plt % values)
    
end subroutine DioPltDestroy


!
! Free PLT Header data that is not used anymore
!
subroutine DioPltFreeHeader(plt)

    ! arguments

    type(DioPltType), target        :: plt      ! dataset

    ! body

    if ( plt % ds % inStream % opened ) then
        if ( ( plt % ds % inStream % streamType .eq. dio_HIS_Stream .or.     &
               plt % ds % inStream % streamType .eq. dio_WQMap_Stream  ).and.&
             .not. plt % ds % inStream % synched                          )then
            if ( plt % ds % ReadingJuls ) then
                ! Free-header is called from ReadValues while scanning
                ! num. time steps, so don't free yet (TODO remove)
                return
            endif
        endif
    endif

    if ( associated(plt % header % hisIntIds) ) then
        deallocate(plt % header % hisIntIds) ; nullify(plt % header % hisIntIds)
    endif

    if ( associated(plt % header % pars) ) then
        deallocate(plt % header % pars) ; nullify(plt % header % pars)
    endif

    if ( associated(plt % header % locs) ) then
        deallocate(plt % header % locs) ; nullify(plt % header % locs)
    endif

    if ( associated(plt % header % parDescripts) ) then
        deallocate(plt % header % parDescripts) ; nullify(plt % header % parDescripts)
    endif

    if ( associated(plt % header % locDescripts) ) then
        deallocate(plt % header % locDescripts) ; nullify(plt % header % locDescripts)
    endif

end subroutine DioPltFreeHeader


!*
!* Set several parts of PLT header:
!* - set parameter/location descriptions
!* - add parameters  / locations
!* - add HIS runid / add HIS integer location Indices
!*


!
! Add Parameters or Location Descriptions PLT (i.e. to HIA-file)
!
subroutine DioPltAddDescriptions(plt, descr_type, descriptions) 

    ! arguments

    type(DioPltType), target        :: plt          ! dataset
    integer         , intent(in)    :: descr_type   ! type: parameters or locations
    character(Len=*), dimension(:), &
        intent(in), target          :: descriptions ! parameter or location descriptions

    ! locals
    logical                         :: statusOK     ! PLT opened, descr.size OK?
    integer                         :: numDesc      ! # descriptions
    integer                         :: i            ! # counter (to avoid stack overflow in array-assignment)

    statusOK = .false.

    numDesc = size(descriptions)

    if ( DioPltOpenedOK(plt) ) then

        if ( plt % ds % outStream % streamType == Dio_WQMap_stream .or. &
             plt % ds % outStream % streamType == Dio_His_stream        ) then
            if ( ( descr_type == dio_plt_pars                     )  .and. &
                 ( numDesc  == plt % header % nPar  )        ) then
                allocate(plt % header % parDescripts(numDesc))
                plt % header % parDescripts = descriptions
                statusOK = .true.
            else
                ! TODO: Store Error
            endif

            if ( ( descr_type == dio_plt_locs               )  .and. &
                 ( numDesc == plt % header % nLoc  )        ) then
                allocate(plt % header % locDescripts(numDesc))
                ! note: array-assignment may cause stack overflow in case of many locations
                do i = 1, numDesc
                    plt % header % locDescripts(i) = descriptions(i)
                enddo
                statusOK = .true.
            else
                ! TODO: Store Error
            endif
        else
            ! TODO: Store Error
        endif
    else
        ! TODO: Store Error
    endif

    if ( statusOK ) then

        call HiaWriteDescriptions(plt, descr_type)

    endif

    if ( associated(plt % header % parDescripts) ) then
        deallocate(plt % header % parDescripts) ; nullify   (plt % header % parDescripts)
    endif

    if ( associated(plt % header % locDescripts) ) then
        deallocate(plt % header % locDescripts) ; nullify   (plt % header % locDescripts)
    endif

end subroutine DioPltAddDescriptions


!
! Add Parameters to Header
!
subroutine DioPltHeaderAddPars(header, pars) 

    ! arguments

    type(DioPltHeaderType)                     :: header   ! dataset header
    character(Len=*), dimension(:), intent(in) :: pars     ! parameter names

    ! locals

    integer :: nParNew, nParOld                         ! #par, #loc
    character(len=DioMaxParLen), pointer, &
                     dimension(:) :: parsTemp        ! parameter names
    
    nParNew = size(pars)
    nParOld = 0

    if (associated(header % pars)) then
        nParOld = size(header % pars)
        allocate(parsTemp(nParOld + nParNew))
        parsTemp(1 : nParOld) = header % pars
        deallocate(header % pars) ; nullify(header % pars)
    else
        allocate(parsTemp(nParOld + nParNew))
    endif

    parsTemp(nParOld + 1 : nParOld + nParNew) = pars
    ! allocate space for par./loc./names,
    ! store the par./loc./names,

    allocate(header % pars(nParOld + nParNew))
    ! write(*,*) 'ALLOC pars', nP
    header % pars = parsTemp
    header % nPar = nParOld + nParNew
 

end subroutine DioPltHeaderAddPars


!
! add Locs to Header
!
subroutine DioPltHeaderAddLocs(header, locs) 

    ! arguments

    type(DioPltHeaderType)                     :: header   ! dataset header
    character(Len=*), dimension(:), intent(in) :: locs     ! parameter names

    ! locals

    integer :: nLocNew, nLocOld                            ! #par, #loc
    character(len=DioMaxLocLen), pointer, &
                                  dimension(:) :: locsTemp ! parameter names
    
    nLocNew = size(locs)
    nLocOld = 0

    if (associated(header % locs)) then
        nLocOld = size(header % locs)
        allocate(locsTemp(nLocOld + nLocNew))
        locsTemp(1 : nLocOld) = header % locs
        deallocate(header % locs) ; nullify(header % locs)
    else
        allocate(locsTemp(nLocOld + nLocNew))
    endif

    locsTemp(nLocOld + 1 : nLocOld + nLocNew) = locs
    ! allocate space for par./loc./names,
    ! store the par./loc./names,

    allocate(header % locs(nLocOld + nLocNew))
    ! write(*,*) 'ALLOC locs', nP
    header % locs = locsTemp
    header % nLoc = nLocOld + nLocNew
 
end subroutine DioPltHeaderAddLocs


!
! add HIS RunId to Header
!
subroutine DioPltSetHisRunId(plt, runId, startTime, endTime)

    ! arguments

    type(DioPltType), target               :: plt       ! plt dataset
    character(HisRunIdSize),&
                dimension(HisRunIdDim)     :: runId     ! HIS runid strings
    double precision, optional, intent(in) :: startTime ! endTime as julian
    double precision, optional, intent(in) :: endTime   ! endTime as julian

    ! locals
    type(DioPltHeaderType), pointer    :: header   ! dataset header

    ! body

    header => plt % header

    if ( .not. associated(header % hisRunId) ) &
                allocate(header % hisRunId(HisRunIdDim))

    header % hisRunId = runId

    ! If hisRunId(4) contains valid T0-string:
    !    Derive Start Time en HIS time Unit/Multiplier from T0String
    ! Otherwise:
    !    if EndTime is given: determine HIS time Multiplier
    !    write T0-string in hisRunId(4)

    if ( .not. DioDsSetTimeFromT0String(plt % ds, runId(4)) ) then
        if ( present(startTime) ) then
            call DioDsSetStartTime(plt % ds, startTime)
            if ( present(endTime) ) then
                call DioDsSetEndTime(plt % ds, endTime)
            endif
        endif
        call DioDsMakeT0String( plt % ds, header % hisRunId(4))
    endif

end subroutine DioPltSetHisRunId


!
! add HIS Integer Location IDS to Header
!
subroutine DioPltSetIntIds(plt, intIds)

    ! arguments

    type(DioPltType), target          :: plt      ! plt dataset
    integer, dimension(:), intent(in) :: intIds   ! Integer Ids

    ! body

    if ( associated(plt % header % hisIntIds) ) then
        deallocate(plt % header % hisIntIds) ; nullify(plt % header % hisIntIds)
    endif
    allocate(plt % header % hisIntIds(size(intIds)))
    plt % header % hisIntIds = intIds

end subroutine DioPltSetIntIds


!******************************************************************************
!* PUBLIC FUNCTIONS FOR GETTING PLT HEADER INFORMATION
!* - #pars/#locs/#times
!* - pars/locs/times
!* - miscellaneous
!* - time (step) information
!******************************************************************************


function DioPltOpenedOK(plt) result(retVal)

    ! return value
    logical                          :: retVal ! stream opened OK?
    ! arguments
    type(DioPltType)    , intent(in) :: plt    ! dataset

    retVal = .false.
    if ( plt % ds % inStream % mode .eq. 'r' .and. &
         plt % ds % inStream % opened              ) then
        retVal = .true.
    else
        if ( plt % ds % outStream % mode .eq. 'w' .and. &
             plt % ds % outStream % opened              ) then
            retVal = .true.
        endif
    endif

end function DioPltOpenedOK


!*
!* Functions for getting #pars/#locs/#times, pars/locs/times
!*

!
! Get #Parameters
!
function DioPltGetNPar(plt) result(dimSize)
    ! return value
    integer                          :: dimSize  ! dim.size
    ! arguments
    type(DioPltType)    , intent(in) :: plt      ! dataset
    ! body
    dimSize = plt % header % nPar
end function DioPltGetNPar


!
! Get pointer to the parameter names
!
function DioPltGetPars(plt) result(names)
    ! return value
    character(len=DioMaxParLen), &
                  pointer, dimension(:):: names  ! pointer to names
    ! arguments
    type(DioPltType)                   :: plt    ! dataset
    ! body
    nullify(names)
    if ( plt % header % nPar .gt. 0 ) then
        if ( .not. associated(plt % header % pars) ) then
            if ( plt % ds % inStream % streamType .ne. Dio_SharedMem_stream ) then
                call DioPltReadHeader(plt)
            endif
        endif
        names => plt % header % pars
    endif
end function DioPltGetPars


!
! Get #Locations
!
function DioPltGetNLoc(plt) result(dimSize)

    ! return value
    integer                          :: dimSize  ! dim.size

    ! arguments
    type(DioPltType)    , intent(in) :: plt      ! dataset

    ! body
    dimSize = plt % header % nLoc
end function DioPltGetNLoc


!
! Get pointer to the location names
!
function DioPltGetLocs(plt) result(names)

    ! return value
    character(len=DioMaxLocLen), &
                  pointer, dimension(:):: names  ! pointer to names
    ! arguments
    type(DioPltType)                   :: plt    ! dataset
    ! body
    nullify(names)
    if ( plt % header % nLoc .gt. 0 ) then
        if ( .not. associated(plt % header % locs) ) then
            if ( plt % ds % inStream % streamType .ne. Dio_SharedMem_stream ) then
                call DioPltReadHeader(plt)
            endif
        endif
        names => plt % header % locs
    endif
end function DioPltGetLocs


!
! Get pointer to the parameter names
!
function DioPltGetDescriptions(plt, descr_type) result(descriptions)

    ! return value
    character(len=DioMaxDescrLen), &
                  pointer, dimension(:):: descriptions ! pointer to descriptions
    ! arguments
    type(DioPltType)    , intent(in)   :: plt          ! dataset
    integer             , intent(in)   :: descr_type   ! type: parameters or locations

    ! body

    nullify(descriptions)

    if ( ( descr_type == dio_plt_pars  )  .and. &
         ( plt % header % nPar > 0 )        ) then
        descriptions => plt % header % parDescripts
    endif

    if ( ( descr_type == dio_plt_locs  )  .and. &
         ( plt % header % nLoc > 0 )        ) then
        descriptions => plt % header % locDescripts
    endif

end function DioPltGetDescriptions


!
! Get pointer to the integer location ids
!
function DioPltGetIntIds(plt) result(intIds)

    ! return value
    integer, pointer, dimension(:) :: intIds ! pointer int ids
    ! arguments
    type(DioPltType)               :: plt    ! dataset
    ! body
    nullify(intIds)
    if ( plt % header % nLoc .gt. 0 ) then
        if ( .not. associated(plt % header % hisIntIds) ) then
            if ( plt % ds % inStream % streamType .ne. Dio_SharedMem_stream ) then
                call DioPltReadHeader(plt)
            endif
        endif
        intIds => plt % header % hisIntIds
    endif
end function DioPltGetIntIds


!
! Get #Timesteps
!
function DioPltGetNTimes(plt) result(dimSize)

    ! return value
    integer                          :: dimSize  ! dim.size

    ! arguments
    type(DioPltType)                 :: plt      ! dataset

    ! body: if #readTimes is 0, read times
    if ( plt % ds % nPreReadTims .eq. 0 ) then
        call DioPltReadAllTimes(plt)
    endif
    dimSize = plt % ds % nPreReadTims

end function DioPltGetNTimes


!
! Get pointer to the pre-read timesteps
!
function DioPltGetTimes(plt) result(times)

    ! return value
    double precision, &
                  pointer, dimension(:):: times  ! pointer to julTimes
    ! arguments
    type(DioPltType)                   :: plt    ! dataset

    ! body: if #readTimes is 0, read times. If timesteps available, provide.
    nullify(times)
    if ( plt % ds % nPreReadTims .eq. 0 ) call DioPltReadAllTimes(plt)
    if ( plt % ds % nPreReadTims .gt. 0 ) times => plt % ds % preReadTims
end function DioPltGetTimes


!
! Get pointer to HIS timesteps
!
function DioPltGetHisSteps(plt) result(hisSteps)

    ! return value
    integer, pointer, dimension(:) :: hisSteps  ! pointer to HIS steps
    ! arguments
    type(DioPltType)               :: plt       ! dataset

    ! body: if #readTimes is 0, read times. If timesteps available, 
    ! convert to HIS steps and provide hisSteps
    nullify(hisSteps)
    if ( plt % ds % nPreReadTims .eq. 0 ) call DioPltReadAllTimes(plt)
    if ( plt % ds % nPreReadTims .gt. 0 ) then
        allocate(plt % ds % preReadHisSteps(plt % ds % nPreReadTims))
        plt % ds % preReadHisSteps = &
            nint(( plt % ds % preReadTims - plt % ds % startTimeVal ) &
            * 86400.0D+00 / ( dble(plt % ds % hisTimeUnit) * dble(plt % ds % hisTimeMult) ))
        hisSteps => plt % ds % preReadHisSteps
    endif
end function DioPltGetHisSteps


!
! Get one of the header lines (from HIS file, or generated)
!
function DioPltGetHeaderLine(plt, lineNr, line) result(retVal)

    ! return value
    logical              :: retVal           ! .true.: success

    ! arguments
    type(DioPltType), intent(in) :: plt      ! plt dataset
    integer, intent(in)          :: lineNr   ! 1 <= linenr <= 4
    character(Len=*), intent(out):: line     ! resulting line

    ! body
    retVal = .false.
    line = ' '
    if ( associated(plt % header % hisRunId) ) then
        if ( lineNr >= 1 .and. lineNr <= HisRunIdDim  ) then
            line = plt % header % hisRunId(lineNr)
            retVal = .true.
        endif
    endif

end function DioPltGetHeaderLine


!*
!* Get functions for retreiving time step information
!*

!
! Get current time step
!
function DioPltGetCurrentTimeStep(plt) result(timeStep)

    ! return value
    double precision              :: timeStep ! current time step (Julian)

    ! arguments
    type(DioPltType)              :: plt      ! plt dataset

    ! body: provide dsInfo's current timestep
    timeStep = DioDsGetCurrentTimeStep(plt % ds)

end function DioPltGetCurrentTimeStep


!
! Get the current HIS time step (timestep only)
!
function DioPltGetCurHisStep(plt) result(hisStep)

    ! return value
    integer                          :: hisStep    ! HIS file time step

    ! arguments
    type(DioPltType)    , intent(in) :: plt        ! plt dataset

    ! body: provide HISTEP
    hisStep = plt % ds % hisStep(plt % ds % curTimeIndex)

end function DioPltGetCurHisStep


!
! Get the current HIS time unit
!
function DioPltGetHisTimeUnit(plt) result(hisTimeUnit)

    ! return value
    integer                          :: hisTimeUnit    ! HIS file time step

    ! arguments
    type(DioPltType)    , intent(in) :: plt        ! plt dataset

    ! body: provide.
    hisTimeUnit = plt % ds % hisTimeUnit

end function DioPltGetHisTimeUnit


!
! Get the current HIS time unit
!
function DioPltGetHisTimeMult(plt) result(hisTimeMult)

    ! return value
    integer                          :: hisTimeMult    ! HIS file time step

    ! arguments
    type(DioPltType)    , intent(in) :: plt        ! plt dataset

    ! body: provide.
    hisTimeMult = plt % ds % hisTimeMult

end function DioPltGetHisTimeMult


!
! Get the current his time step and the his unit/multiplier
!
function DioPltGetCurHisStepAndUnit(plt, hisTimeMultiplier, hisTimeUnit) result(hisStep)

    ! return value
    integer                          :: hisStep       ! HIS file time step

    ! arguments
    type(DioPltType), intent(in) :: plt               ! plt dataset
    integer         , intent(out):: hisTimeMultiplier ! HIS file time unit
    integer         , intent(out):: hisTimeUnit       ! HIS file time unit

    ! body: provide.
    hisTimeUnit       = plt % ds % hisTimeUnit
    hisTimeMultiplier = plt % ds % hisTimeMult
    hisStep = plt % ds % hisStep( plt % ds % curTimeIndex)

end function DioPltGetCurHisStepAndUnit


!*
!* Miscellaneous Get functions
!*

!
! Get the current Stream Type (For Shared Mem / HIS-file switch in Sobek-RR)
!
function DioPltGetStreamType(plt) result(streamType)

    ! return value
    integer      :: streamType   ! Unknown if not connected; otherwise:
                                 ! plt % ds % (in|out)stream % streamType

    ! arguments
    type(DioPltType)    , intent(in) :: plt        ! plt dataset

    ! body

    streamType = dio_Unknown_Stream
    if ( plt % ds % inStream % opened ) then
        streamType = plt % ds % inStream % streamType
    endif
    if ( plt % ds % outStream % opened ) then
        streamType = plt % ds % outStream % streamType
    endif

end function DioPltGetStreamType


!******************************************************************************
!* PUBLIC FUNCTIONS TO SET/GET VALUES IN PLT
!******************************************************************************

!*
!* Functions for storing values in PLT
!*

!
! Set real values in dataset
!
subroutine DioPltSetValuesReal(plt, values)

    ! arguments
    type(DioPltType)                 :: plt       ! dataset
    real, dimension(:,:), intent(in) :: values    ! values for this step

    ! locals
    integer                          :: nP, nL    ! sizes

    ! body: get / check sizes; allocate and store value
    nP = size(values, 1)
    nL = size(values, 2)
    if ( DioPltCheckSizes(plt, nP, nL, Dio_DsPltLarger ) ) then
        call Dio3DAllocateValues(plt % values, DioDsDataSize(plt % ds))
        plt % values % reals(1:nP, 1:nL, plt % ds % curDataIndex) = values(1:nP, 1:nL)
    endif

end subroutine DioPltSetValuesReal


!
! Set double values in dataset
!
subroutine DioPltSetValuesDouble(plt, values)

    ! arguments
    type(DioPltType)                 :: plt       ! dataset
    double precision, dimension(:,:),&
                          intent(in) :: values    ! values for this step
    ! locals
    integer                          :: nP, nL    ! sizes

    ! body: get / check sizes; allocate and store value
    nP = size(values, 1)
    nL = size(values, 2)
    if ( DioPltCheckSizes(plt, nP, nL, Dio_DsPltLarger ) ) then
        call Dio3DAllocateValues(plt % values, DioDsDataSize(plt % ds))
        plt % values % doubles(1:nP, 1:nL, plt % ds % curDataIndex)  = values(1:nP, 1:nL)
    endif

end subroutine DioPltSetValuesDouble


!
! Set integer values in dataset
!
subroutine DioPltSetValuesInteger(plt, values)

    ! arguments
    type(DioPltType)                   :: plt    ! dataset
    integer, dimension(:,:), intent(in):: values ! values for this step

    ! locals
    integer                          :: nP, nL    ! sizes

    ! body: get / check sizes; allocate and store value
    nP = size(values, 1)
    nL = size(values, 2)
    if ( DioPltCheckSizes(plt, nP, nL, Dio_DsPltLarger ) ) then
        call Dio3DAllocateValues(plt % values, DioDsDataSize(plt % ds))
        plt % values % ints(1:nP, 1:nL, plt % ds % curDataIndex)  = values(1:nP, 1:nL)
    endif

end subroutine DioPltSetValuesInteger


!
! Set real values in dataset for 1D-represented array
!
function DioPltSetValues1DReal(plt, values, allowLarger) result(retVal)

    ! return value
    logical                        :: retVal     ! .true.: success

    ! arguments
    type(DioPltType)               :: plt         ! dataset
    real, dimension(:), intent(in) :: values      ! values for this step
    logical, optional , intent(in) :: allowLarger ! size > nPar*nLoc allowed

    ! locals
    integer                        :: size1D     ! sizes of values-array
    integer                        :: par,loc    ! loop counters
    logical                        :: OK         ! size > or >= nPar*nLoc ?

    ! body: get / check sizes; allocate and store value
    retVal = .false.
    size1D = size(values)

    OK = .false.
    if ( size1D .eq. (plt % header % nPar * plt % header % nLoc) ) OK = .true.
    if ( .not. OK ) then
        if ( present(allowLarger) ) then
            if ( allowLarger ) then
                if ( size1D .gt. (plt % header % nPar * plt % header % nLoc) ) then
                    OK = .true.
                endif
            endif
        endif
    endif

    if ( OK ) then
        call Dio3DAllocateValues(plt % values, DioDsDataSize(plt % ds))
        do par = 1, plt % header % nPar
            do loc = 1, plt % header % nLoc
                plt % values % reals(par, loc, plt % ds % curDataIndex) = &
                        values( par + (loc - 1 )*plt % header % nPar)
            enddo
        enddo
        retVal = .true.
    endif

end function DioPltSetValues1DReal


!
! Set double values in dataset for 1D-represented array
!
function DioPltSetValues1DDouble(plt, values, allowLarger) result(retVal)

    ! return value
    logical                        :: retVal     ! .true.: success

    ! arguments
    type(DioPltType)               :: plt        ! dataset
    double precision, &
          dimension(:), intent(in) :: values     ! values for this step
    logical, optional , intent(in) :: allowLarger ! size > nPar*nLoc allowed

    ! locals
    integer                        :: size1D     ! sizes of values-array
    integer                        :: par,loc    ! loop counters
    logical                        :: OK         ! size > or >= nPar*nLoc ?

    ! body: get / check sizes; allocate and store value

    retVal = .false.
    size1D = size(values)

    OK = .false.
    if ( size1D .eq. (plt % header % nPar * plt % header % nLoc) ) OK = .true.
    if ( .not. OK ) then
        if ( present(allowLarger) ) then
            if ( allowLarger ) then
                if ( size1D .gt. (plt % header % nPar * plt % header % nLoc) ) then
                    OK = .true.
                endif
            endif
        endif
    endif

    if ( OK ) then
        call Dio3DAllocateValues(plt % values, DioDsDataSize(plt % ds))
        do par = 1, plt % header % nPar
            do loc = 1, plt % header % nLoc
                plt % values % doubles(par, loc, plt % ds % curDataIndex) = &
                        values( par + (loc - 1 )*plt % header % nPar)
            enddo
        enddo
        retVal = .true.
    endif

end function DioPltSetValues1DDouble


!*
!* Functions for retreiving a copy of the PLT's values
!*

!
! Get a copy of the real values in dataset
!
function DioPltGetRealValues(plt, values) result(retVal)

    ! return value
    logical                          :: retVal   ! .true.: success

    ! arguments
    type(DioPltType)    , intent(in) :: plt      ! dataset
    real, dimension(:,:), intent(out):: values   ! space for values, must be made
                                                 ! available by caller.

    ! body: check target size, deliver values
    retVal = .false.
    if ( DioPltCheckSizes(plt, &
            size(values, 1), size(values, 2), Dio_DsPltSmaller ) ) then
        values(1:plt%header%nPar, 1: plt%header%nLoc) = plt % values % reals(:,:,plt % ds % curDataIndex)
        retVal = .true.
    endif

end function DioPltGetRealValues


!
! Get a copy of the double values in dataset
!
function DioPltGetDoubleValues(plt, values) result(retVal)

    ! return value
    logical                          :: retVal   ! .true.: success

    ! arguments
    type(DioPltType)    , intent(in) :: plt      ! dataset
    double precision, dimension(:,:), &
                          intent(out):: values   ! space for values, must be made
                                                 ! available by caller.

    ! body: check target size, deliver values
    retVal = .false.
    if ( DioPltCheckSizes(plt, &
            size(values, 1), size(values, 2), Dio_DsPltSmaller ) ) then
        values(1:plt%header%nPar, 1: plt%header%nLoc) = plt % values % doubles(:,:,plt % ds % curDataIndex)
        retVal = .true.
    endif

end function DioPltGetDoubleValues


!
! Get a copy of the integer values in dataset
!
function DioPltGetIntegerValues(plt, values) result(retVal)

    ! return value
    logical                          :: retVal   ! .true.: success

    ! arguments
    type(DioPltType)    , intent(in)    :: plt   ! dataset
    integer, dimension(:,:), intent(out):: values! space for values, must be made
                                                 ! available by caller.
    ! body: check target size, deliver values

    retVal = .false.
    if ( DioPltCheckSizes(plt, &
            size(values, 1), size(values, 2), Dio_DsPltSmaller ) ) then
        values(1:plt%header%nPar, 1: plt%header%nLoc) = plt % values % ints(:,:,plt % ds % curDataIndex)
        retVal = .true.
    endif

end function DioPltGetIntegerValues


!******************************************************************************
!* PUBLIC functions to define a PLT on a stream
!* For specified streamType, with:
!* - No additional info
!* - Additional Julian Start Time
!* - Additional HIS runId
!* - Additional HIS integerIds
!* - Additional HIS runId and integerIds
!* For automatically determined streamType, with:
!* - No additional info
!* - Additional Julian Start Time
!* - Additional HIS runId
!* - Additional HIS integerIds
!* - Additional HIS runId and integerIds
!******************************************************************************

!
! Define on specified streamType, no additional info
!
function DioPltDefine(stream, name, varType, pars, locs) result(plt)

    ! return value
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    type(DioStreamType), target, intent(in)    :: stream  ! stream
    integer                    , intent(in)    :: varType ! type of var to be stored
    character(Len=*)           , intent(in)    :: name    ! plt name
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! body
    plt = DioPltCreate(name, varType, pars, locs)
    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefine


function DioPltDefineMap(stream, name, varType, pars, nLoc) result(plt)

    ! return value
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    type(DioStreamType), target   , intent(in) :: stream  ! stream
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*)              , intent(in) :: name    ! plt name
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer                       , intent(in) :: nLoc    ! #locations

    ! body
    plt = DioPltCreate(name, varType, pars, nLoc)
    plt % ds % outStream = stream
    call DioPltWriteHeader(plt)

end function DioPltDefineMap


!
! Define on specified streamType, additional Julian Start Time
!
function DioPltDefineWithJulian(stream, name, varType, pars, locs, startTime, endTime) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    
    type(DioStreamType), target   , intent(in) :: stream    ! stream
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! body
    plt = DioPltCreate(name, varType, pars, locs)
    call DioDsSetStartTime(plt % ds, startTime)
    if ( present(endTime) ) call DioDsSetEndTime(plt % ds, endTime)
    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithJulian


function DioPltDefineWithJulianMap(stream, name, varType, pars, nLoc, startTime, endTime) result(plt)

    ! return value
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    type(DioStreamType), target   , intent(in) :: stream    ! stream
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! body
    plt = DioPltCreate(name, varType, pars, nLoc)
    call DioDsSetStartTime(plt % ds, startTime)
    if ( present(endTime) ) call DioDsSetEndTime(plt % ds, endTime)
    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithJulianMap


!
! Define on specified streamType, additional HIS RunID
!
function DioPltDefineWithRunid(stream, name, runId, varType, pars, locs) result(plt)

    ! return value
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    type(DioStreamType), target   , intent(in) :: stream  ! stream
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId   ! HIS runid for this dataset
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*)              , intent(in) :: name    ! plt name
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! body
    plt = DioPltCreate(name, varType, pars, locs)
    call DioPltSetHisRunId(plt, runId)
    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithRunid


function DioPltDefineWithRunidMap(stream, name, runId, varType, pars, nLoc) result(plt)

    ! return value
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    type(DioStreamType), target   , intent(in) :: stream  ! stream
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId   ! HIS runid for this dataset
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*)              , intent(in) :: name    ! plt name
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer                       , intent(in) :: nLoc    ! #locations

    ! body
    plt = DioPltCreate(name, varType, pars, nLoc)
    call DioPltSetHisRunId(plt, runId)
    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithRunidMap


function DioPltDefineWithRIDAndJul(stream, name, runId, varType, pars, locs, startTime, endTime) result(plt)
    
    include 'dio-time-support.inc'

    ! return value
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    type(DioStreamType), target   , intent(in) :: stream    ! stream
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId     ! HIS runid for this dataset
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! locals
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: locRunId  ! loc HIS runid

    ! body

    plt = DioPltCreate(name, varType, pars, locs)

    call DioDsSetStartTime(plt % ds, startTime)
    if ( present(endTime) ) call DioDsSetEndTime(plt % ds, endTime)

    locRunId = runID
    call DioDsMakeT0String(plt % ds, locRunId(4))
    call DioPltSetHisRunId(plt, locRunId)

    ! Write to file

    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithRIDAndJul


function DioPltDefineWithRIDAndJulMap(stream, name, runId, varType, pars, nLoc, startTime, endTime) result(plt)
    
    include 'dio-time-support.inc'

    ! return value
    type(DioPltType)                        :: plt          ! dataset

    ! arguments
    type(DioStreamType), target   , intent(in) :: stream    ! stream
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId     ! HIS runid for this dataset
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! locals
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: locRunId  ! loc HIS runid

    ! body

    plt = DioPltCreate(name, varType, pars, nLoc)

    call DioDsSetStartTime(plt % ds, startTime)
    if ( present(endTime) ) call DioDsSetEndTime(plt % ds, endTime)

    locRunId = runID
    call DioDsMakeT0String(plt % ds, locRunId(4))
    call DioPltSetHisRunId(plt, locRunId)

    ! Write to file

    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithRIDAndJulMap


!
! Define on specified streamType, additional HIS RunID and Integer Ids
!
function DioPltDefineWithIntIds(stream, name, varType, pars, intIds, locs) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    
    type(DioStreamType), target   , intent(in) :: stream  ! stream
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*)              , intent(in) :: name    ! plt name
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer         , dimension(:), intent(in) :: intIds  ! Integer Ids
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! body

    plt = DioPltCreate(name, varType, pars, locs)

    call DioPltSetIntIds(plt, intIds)

    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithIntIds


function DioPltDefineWithRunidAndIntIds(stream, name, runId, varType, pars, intIds, locs) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt      ! dataset

    ! arguments
    
    type(DioStreamType), target   , intent(in) :: stream   ! stream
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId    ! HIS runid for this dataset
    integer                       , intent(in) :: varType  ! type of var to be stored
    character(Len=*)              , intent(in) :: name     ! plt name
    character(Len=*), dimension(:), intent(in) :: pars     ! parameter names
    integer         , dimension(:), intent(in) :: intIds   ! Integer Ids
    character(Len=*), dimension(:), intent(in) :: locs     ! location names

    ! body

    plt = DioPltCreate(name, varType, pars, locs)

    call DioPltSetHisRunId(plt, runId)
    call DioPltSetIntIds(plt, intIds)

    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithRunidAndIntIds


function DioPltDefineWithRunidIntsJul(stream, name, runId, varType, pars, intIds, locs, startTime, endTime) result(plt)

    include 'dio-time-support.inc'

    ! return value
    
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    
    type(DioStreamType), target   , intent(in) :: stream    ! stream
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId     ! HIS runid for this dataset
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    integer         , dimension(:), intent(in) :: intIds    ! Integer Ids
    character(Len=*), dimension(:), intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! locals
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: locRunId  ! loc HIS runid

    ! body

    plt = DioPltCreate(name, varType, pars, locs)

    ! Set t0 string in hisRunId(4), set starttime, set ints

    call DioDsSetStartTime(plt % ds, startTime)
    if ( present(endTime) ) call DioDsSetEndTime(plt % ds, endTime)

    locRunId = runID
    call DioDsMakeT0String(plt % ds, locRunId(4))
    call DioPltSetHisRunId(plt, locRunId)

    call DioPltSetIntIds(plt, intIds)

    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end function DioPltDefineWithRunidIntsJul


!
! Define on automatic streamType, no additional info
!
function DioPltDefAutoStream(name, varType, pars, locs) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*)              , intent(in) :: name    ! plt name
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! locals
    type(DioStreamType)                        :: stream  ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    plt = DioPltDefine(stream, name, varType, pars, locs)

end function DioPltDefAutoStream


function DioPltDefAutoStreamMap(name, varType, pars, nLoc) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*)              , intent(in) :: name    ! plt name
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer                       , intent(in) :: nLoc    ! #locations

    ! locals
    type(DioStreamType)                        :: stream  ! stream

    ! body

    stream = DioStreamCreate(Dio_WQMap_Stream, name, 'w')
    stream % autoStream = .true.
    plt = DioPltDefine(stream, name, varType, pars, nLoc)

end function DioPltDefAutoStreamMap


!
! Define on automatic streamType, additional Julian Start Time
!
function DioPltDefAutoWithJulian(name, varType, pars, locs, startTime, endTime) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! locals
    type(DioStreamType)                        :: stream    ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    if ( present(endTime) ) then
        plt = DioPltDefine(stream, name, varType, pars, locs, startTime, endTime)
    else
        plt = DioPltDefine(stream, name, varType, pars, locs, startTime)
    endif

end function DioPltDefAutoWithJulian


function DioPltDefAutoWithJulianMap(name, varType, pars, nLoc, startTime, endTime) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! locals
    type(DioStreamType)                        :: stream    ! stream

    ! body

    stream = DioStreamCreate(Dio_WQMap_Stream, name, 'w')
    stream % autoStream = .true.
    if ( present(endTime) ) then
        plt = DioPltDefine(stream, name, varType, pars, nLoc, startTime, endTime)
    else
        plt = DioPltDefine(stream, name, varType, pars, nLoc, startTime)
    endif

end function DioPltDefAutoWithJulianMap


!
! Define on automatic streamType, additional HIS RunID
!
function DioPltDefAutoWithRunid(name, runId, varType, pars, locs) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt      ! dataset

    ! arguments
    
    character(Len=*)              , intent(in) :: name     ! plt name
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId    ! HIS runid for this dataset
    integer                       , intent(in) :: varType  ! type of var to be stored
    character(Len=*), dimension(:), intent(in) :: pars     ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs     ! location names

    ! body

    ! locals
    type(DioStreamType)                        :: stream   ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    plt = DioPltDefine(stream, name, runId, varType, pars, locs)

end function DioPltDefAutoWithRunid


function DioPltDefAutoWithRunidMap(name, runId, varType, pars, nLoc) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    
    character(Len=*)              , intent(in) :: name    ! plt name
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId   ! HIS runid for this dataset
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer                       , intent(in) :: nLoc    ! #locations

    ! locals
    type(DioStreamType)                        :: stream  ! stream

    ! body

    stream = DioStreamCreate(Dio_WQMap_Stream, name, 'w')
    stream % autoStream = .true.
    plt = DioPltDefine(stream, name, runId, varType, pars, nLoc)

end function DioPltDefAutoWithRunidMap


function DioPltDefAutoWithRIDAndJul(name, runId, varType, pars, locs, startTime, endTime) result(plt)

    ! return value
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId     ! HIS runid for this dataset
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    character(Len=*), dimension(:), intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! locals
    type(DioStreamType)                        :: stream    ! stream

    ! body
    stream = DioStreamCreateAuto(name, 'w')
    if ( present(endTime) ) then
        plt = DioPltDefine(stream, name, runId, varType, pars, locs, startTime, endTime)
    else
        plt = DioPltDefine(stream, name, runId, varType, pars, locs, startTime)
    endif

end function DioPltDefAutoWithRIDAndJul



function DioPltDefAutoWithRIDAndJulMap(name, runId, varType, pars, nLoc, startTime, endTime) result(plt)

    ! return value
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId     ! HIS runid for this dataset
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*)              , intent(in) :: name      ! plt name
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    integer                       , intent(in) :: nLoc      ! #locations
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision, optional    , intent(in) :: endTime   ! endTime as julian

    ! locals
    type(DioStreamType)                        :: stream    ! stream

    ! body
    stream = DioStreamCreate(Dio_WQMap_Stream, name, 'w')
    stream % autoStream = .true.
    if ( present(endTime) ) then
        plt = DioPltDefine(stream, name, runId, varType, pars, nLoc, startTime, endTime)
    else
        plt = DioPltDefine(stream, name, runId, varType, pars, nLoc, startTime)
    endif

end function DioPltDefAutoWithRIDAndJulMap



!
! Define on automatic streamType, additional HIS Integer Ids
!
function DioPltDefAutoWithIntIds(name, varType, pars, intIds, locs) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    
    character(Len=*)              , intent(in) :: name    ! plt name
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer         , dimension(:), intent(in) :: intIds  ! Integer Ids
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! locals
    type(DioStreamType)                        :: stream  ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    plt = DioPltDefine(stream, name, varType, pars, intIds, locs)

end function DioPltDefAutoWithIntIds


!
! Define on automatic streamType, additional HIS RunID and Integer Ids
!
function DioPltDefAutoWithRIDAndInts(name, runId, varType, pars, intIds, locs) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt     ! dataset

    ! arguments
    
    character(Len=*)              , intent(in) :: name    ! plt name
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId   ! HIS runid for this dataset
    integer                       , intent(in) :: varType ! type of var to be stored
    character(Len=*), dimension(:), intent(in) :: pars    ! parameter names
    integer         , dimension(:), intent(in) :: intIds  ! Integer Ids
    character(Len=*), dimension(:), intent(in) :: locs    ! location names

    ! locals
    type(DioStreamType)                        :: stream  ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    plt = DioPltDefine(stream, name, runId, varType, pars, intIds, locs)

end function DioPltDefAutoWithRIDAndInts


!
! Define on automatic streamType, additional HIS RunID, Integer Ids and jul StartTime
!
function DioPltDefAutoWithRunidIntsJul1(name, runId, varType, pars, intIds, locs, startTime) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    
    character(Len=*)              , intent(in) :: name      ! plt name
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId     ! HIS runid for this dataset
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    integer         , dimension(:), intent(in) :: intIds    ! Integer Ids
    character(Len=*), dimension(:), intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian

    ! locals
    type(DioStreamType)                        :: stream    ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    plt = DioPltDefine(stream, name, runId, varType, pars, intIds, locs, startTime)

end function DioPltDefAutoWithRunidIntsJul1


!
! Define on automatic streamType, additional HIS RunID, Integer Ids and jul StartTime
!
function DioPltDefAutoWithRunidIntsJul2(name, runId, varType, pars, intIds, locs, startTime, endTime) result(plt)

    ! return value
    
    type(DioPltType)                           :: plt       ! dataset

    ! arguments
    
    character(Len=*)              , intent(in) :: name      ! plt name
    character(HisRunIdSize),&
                dimension(HisRunIdDim)         :: runId     ! HIS runid for this dataset
    integer                       , intent(in) :: varType   ! type of var to be stored
    character(Len=*), dimension(:), intent(in) :: pars      ! parameter names
    integer         , dimension(:), intent(in) :: intIds    ! Integer Ids
    character(Len=*), dimension(:), intent(in) :: locs      ! location names
    double precision              , intent(in) :: startTime ! startTime as julian
    double precision              , intent(in) :: endTime   ! endTime as julian

    ! locals
    type(DioStreamType)                        :: stream    ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'w')
    plt = DioPltDefine(stream, name, runId, varType, pars, intIds, locs, startTime, endTime)

end function DioPltDefAutoWithRunidIntsJul2


!*
!* Function to rewind PLT
!*
subroutine DioPltRewind(plt)

    ! arguments
    
    type(DioPltType)                        :: plt     ! dataset

    ! body

    if ( plt % ds % outStream % opened ) then
        if ( DioStreamUsesLun(plt % ds % outStream) ) then
#if (defined(WIN32))
            rewind( plt % ds % outStream % lun )
#else
            ! Unix keeps original content, so remove file and reopen
            close (plt % ds % outStream % lun, status='delete')
            open (newunit=plt % ds % outStream % lun, form = plt % ds % outStream % form, &
                     file = plt % ds % outStream % name, action='write')
            ! write(*,*) 'Deleted/Opened ', plt % ds % outStream % name
#endif
        endif
    endif

    if ( plt % ds % inStream % opened ) then
        if ( DioStreamUsesLun(plt % ds % inStream) ) then
            rewind( plt % ds % inStream % lun )
        endif
    endif

end subroutine DioPltRewind


!*
!* Function to assign a stream to an already created PLT
!*
subroutine DioPltPutOnStream(plt, stream)

    ! arguments
    
    type(DioPltType)                :: plt     ! dataset
    type(DioStreamType), intent(in) :: stream  ! stream

    ! body

    plt % ds % outStream = stream 
    call DioPltWriteHeader(plt)

end subroutine DioPltPutOnStream


!******************************************************************************
!* PUBLIC functions to PUT (=write) values
!* - floats    for next timestep (no timeStamp specified, auto increase)
!* - doubles   "
!* - integers  "
!* - floats    for a timestep as integer value (HIS timestep)
!* - doubles   "
!* - integers  "
!******************************************************************************

!
! PUT next floats
!
subroutine DioPltPutReals(plt, values)

    ! arguments

    type(DioPltType)                                :: plt      ! dataset
    real              , dimension(:,:), intent(in)  :: values   ! reals

    ! body

    call DioDsIncreaseTimestep(plt % ds)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutReals


!
! PUT next doubles
!
subroutine DioPltPutDoubles(plt, values)

    ! arguments

    type(DioPltType)                              :: plt      ! dataset
    double precision, dimension(:,:), intent(in)  :: values   ! doubles

    ! body

    call DioDsIncreaseTimestep(plt % ds)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutDoubles


!
! PUT next integers
!
subroutine DioPltPutIntegers(plt, values)

    ! arguments

    type(DioPltType)                                :: plt      ! dataset
    integer           , dimension(:,:), intent(in)  :: values   ! integers

    ! body

    call DioDsIncreaseTimestep(plt % ds)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutIntegers


!
! PUT floats for a timestep as integer value (HIS timestep)
!
subroutine DioPltPutHisStepReals(plt, intTime, values)

    ! arguments

    type(DioPltType)                                :: plt      ! dataset
    integer,                            intent(in)  :: intTime  ! HIS timestep
    real,               dimension(:,:), intent(in)  :: values   ! reals

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, integerHisTime=intTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetHisTimestep(plt % ds, intTime)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutHisStepReals


!
! PUT doubles for a timestep as integer value (HIS timestep)
!
subroutine DioPltPutHisStepDoubles(plt, intTime, values)

    ! arguments

    type(DioPltType)                              :: plt      ! dataset
    integer,                            intent(in):: intTime  ! HIS timestep
    double precision, dimension(:,:), intent(in)  :: values   ! doubles

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, integerHisTime=intTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetHisTimestep(plt % ds, intTime)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutHisStepDoubles


!
! PUT integers for a timestep as integer value (HIS timestep)
!
subroutine DioPltPutHisStepIntegers(plt, intTime, values)

    ! arguments

    type(DioPltType)                                :: plt      ! dataset
    integer,                            intent(in)  :: intTime  ! HIS timestep
    integer           , dimension(:,:), intent(in)  :: values   ! integers

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, integerHisTime=intTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetHisTimestep(plt % ds, intTime)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutHisStepIntegers


!
! PUT floats for a HIS timestep for a 1D-represented array
!
subroutine DioPltPutHisStepReals1D(plt, intTime, values, allowLarger)

    ! arguments

    type(DioPltType)                           :: plt         ! dataset
    integer,                        intent(in) :: intTime     ! HIS timestep
    real,             dimension(:), intent(in) :: values      ! reals
    logical, optional , intent(in)             :: allowLarger ! size > nPar*nLoc allowed

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, integerHisTime=intTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetHisTimestep(plt % ds, intTime)

    if ( present(allowLarger) )then
        if (DioPltSetValues1D(plt, values, allowLarger)) call DioPltWriteValues(plt)
    else
        if (DioPltSetValues1D(plt, values)) call DioPltWriteValues(plt)
    endif

    return

end subroutine DioPltPutHisStepReals1D


!
! PUT doubles for a HIS timestep for a 1D-represented array
!
subroutine DioPltPutHisStepDoubles1D(plt, intTime, values, allowLarger)

    ! arguments

    type(DioPltType)                           :: plt         ! dataset
    integer,                        intent(in) :: intTime     ! HIS timestep
    double precision, dimension(:), intent(in) :: values      ! doubles
    logical, optional , intent(in)             :: allowLarger ! size > nPar*nLoc allowed

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, integerHisTime=intTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetHisTimestep(plt % ds, intTime)

    if ( present(allowLarger) )then
        if (DioPltSetValues1D(plt, values, allowLarger)) call DioPltWriteValues(plt)
    else
        if (DioPltSetValues1D(plt, values)) call DioPltWriteValues(plt)
    endif

    return

end subroutine DioPltPutHisStepDoubles1D


!
! PUT floats for a timestep as julian date
!
subroutine DioPltPutJulTimeReals(plt, julTime, values)

    ! arguments

    type(DioPltType)                                :: plt      ! dataset
    double precision, intent(in)                    :: julTime  ! Julian timestep
    real,               dimension(:,:), intent(in)  :: values

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, julTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetJulTimestep(plt % ds, julTime)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutJulTimeReals


!
! PUT doubles for a timestep as integer value (HIS timestep)
!
subroutine DioPltPutJulTimeDoubles(plt, julTime, values)

    ! arguments

    type(DioPltType)                                :: plt      ! dataset
    double precision, intent(in)                    :: julTime  ! Julian timestep
    double precision, dimension(:,:), intent(in)    :: values

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, julTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetJulTimestep(plt % ds, julTime)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutJulTimeDoubles


!
! PUT integers for a timestep as integer value (HIS timestep)
!
subroutine DioPltPutJulTimeIntegers(plt, julTime, values)

    ! arguments

    type(DioPltType)                     :: plt      ! dataset
    double precision,        intent(in)  :: julTime  ! Julian timestep
    integer, dimension(:,:), intent(in)  :: values

    ! body

    if (plt % header % allowJumpBack) then
        call DioPltCheckJumpBack(plt, julTime)
    endif

    call DioDsIncreaseTimestep(plt % ds)
    call DioDsSetJulTimestep(plt % ds, julTime)
    call DioPltSetValues(plt, values)
    call DioPltWriteValues(plt)

    return

end subroutine DioPltPutJulTimeIntegers


subroutine DioPltCheckJumpBack(plt, julianTime, integerHisTime)

#if (defined(HAVE_CONFIG_H)||defined(salford32))
    type(DioPltType)                            :: plt
    double precision, intent(in), optional      :: julianTime
    integer         , intent(in), optional      :: integerHisTime
    call DioStreamError(229, &
        'DioPltCheckJumpBack not supported for LINUX (ds: ', &
                            plt % ds % name, ')')
#else

#if (defined(WIN32))
    use dfport          ! for fseek
#else
    integer, external   :: fseek
    integer,  parameter :: SEEK_SET = 0, SEEK_CUR = 1
#endif

    ! arguments   
    type(DioPltType)                            :: plt
    double precision, intent(in), optional      :: julianTime
    integer         , intent(in), optional      :: integerHisTime

    ! locals
   
    integer         :: headerSize,& ! size of HIS header 
                       blockSize ,& ! size of HIS time step block
                       jumpSize     ! total jump size
    integer         :: tIndex       ! time index to write to
    integer         :: fseekResult  ! result of fseek call

    ! body

    if (plt % ds % curTimeIndex > 0) then

        tIndex = plt % ds % curTimeIndex
        if (present(integerHisTime)) then
            do while (integerHisTime <= plt % ds % hisStep(tIndex) .and. tIndex > 1)
                tIndex = tIndex - 1
            enddo
        endif
        if (present(julianTime)) then
            do while (julianTime <= plt % ds % timeStep(tIndex) .and. tIndex > 1)
                tIndex = tIndex - 1
            enddo
        endif

        if (tIndex < plt % ds % curTimeIndex) then
        
            headerSize = DioPltHisDetermineHeaderSize(plt)  ! todo: for performance: store sizes
            blockSize  = DioPltHisDetermineBlockSize(plt)
            
            jumpSize = headerSize + blockSize * (tIndex - 1)

            fseekResult = fseek(plt % ds % outStream % lun, jumpSize, SEEK_SET)
            if ( fseekResult .ne. 0 ) then
                call DioStreamError(230, 'Could not jump back in his file')
            endif

            plt % ds % curPutGetCount = tIndex
            if ( plt % ds % dataGrows ) then
                plt % ds % curDataIndex = tIndex
            endif
            if ( plt % ds % timeGrows ) then
                plt % ds % curTimeIndex = tIndex
            endif

        endif
   endif

#endif

end subroutine DioPltCheckJumpBack


!******************************************************************************
!* Functions to GET a PLT from a stream
!******************************************************************************

!
! Get PLT Dataset from a speficied stream
!
function DioPltGetDataset(stream, name) result(plt)

    ! return value

    type(DioPltType)                       :: plt     ! dataset

    ! arguments

    type(DioStreamType), target, intent(in) :: stream ! stream
    character(Len=*)           , intent(in) :: name   ! dataset name

    ! body

    plt = DioPltCreate(name, Dio_Plt_Unknown)
    plt % ds % inStream = stream

    call DioPltReadHeader(plt)

end function DioPltGetDataset


!
! Get Dataset from an automatic stream
!
function DioPltGetDatasetAutoStream(name) result(plt)

    ! return value

    type(DioPltType)             :: plt    ! dataset

    ! arguments

    character(Len=*), intent(in) :: name   ! dataset name

    ! locals

    type(DioStreamType)          :: stream ! stream

    ! body

    stream = DioStreamCreateAuto(name, 'r')
    if (stream % opened) then
        plt = DioPltGetDataset(stream, name)
    else
        call DioStreamError(203, 'Could not open ', trim(name), ' for reading')
        plt = DioPltCreateEmpty(name, Dio_PLT_Unknown)
    endif

end function DioPltGetDatasetAutoStream


!******************************************************************************
!* Functions to GET (=read) values
!******************************************************************************

!*
!* Functions to GET next values from file
!*

!
! GET next floats
!
function DioPltGetReals(plt, values) result(retVal)

    ! result value
   
    logical                        :: retVal        ! .true. : success

    ! arguments

    type(DioPltType)               :: plt           ! dataset
    real, dimension(:,:), pointer  :: values        ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Real, plt)) then
        values => plt % values % reals(:,:, plt % ds % curDataIndex )
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetReals


!
! GET next doubles
!
function DioPltGetDoubles(plt, values) result(retVal)

    ! return value

    logical                        :: retVal        ! .true. : success

    ! arguments

    type(DioPltType)                :: plt         ! dataset
    double precision, dimension(:,:), pointer     :: values     ! pointer to values

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Double, plt)) then
        values => plt % values % doubles(:,:, plt % ds % curDataIndex ) ! , plt % ds % curDataIndex )
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetDoubles


!
! GET next integers
!
function DioPltGetIntegers(plt, values) result(retVal)

    ! result value
   
    logical                         :: retVal     ! .true. : success

    ! arguments

    type(DioPltType)                :: plt        ! dataset
    integer, dimension(:,:), pointer:: values     ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Integer, plt)) then
        values => plt % values % ints(:,:, plt % ds % curDataIndex )
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetIntegers


!*
!* Functions to GET next values and current julian timestep from file
!*

!
! GET floats and current timestep
!
function DioPltGetHisStepReals(plt, hisStep, values) result(retVal)

    ! result value
   
    logical                       :: retVal  ! .true. : success

    ! arguments

    type(DioPltType)              :: plt     ! dataset
    integer, intent(out)          :: hisStep ! HIS  timestep
    real, dimension(:,:), pointer :: values  ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Real, plt)) then
        values => plt % values % reals(:,:, plt % ds % curDataIndex )
        hisStep = DioPltGetCurHisStep(plt)
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetHisStepReals


!
! GET doubles and current timestep
!
function DioPltGetHisStepDoubles(plt, hisStep, values) result(retVal)

    ! return value

    logical                          :: retVal  ! .true. : success

    ! arguments

    type(DioPltType)                 :: plt     ! dataset
    integer, intent(out)             :: hisStep ! HIS  timestep
    double precision, dimension(:,:)&
                           , pointer :: values  ! pointer to values

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Double, plt)) then
        values => plt % values % doubles(:,:, plt % ds % curDataIndex ) ! , plt % ds % curDataIndex )
        hisStep = DioPltGetCurHisStep(plt)
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetHisStepDoubles


!
! GET integers and current timestep
!
function DioPltGetHisStepIntegers(plt, hisStep, values) result(retVal)

    ! result value
   
    logical                         :: retVal  ! .true. : success

    ! arguments

    type(DioPltType)                :: plt     ! dataset
    integer, intent(out)            :: hisStep ! HIS  timestep
    integer, dimension(:,:), pointer:: values  ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Integer, plt)) then
        values => plt % values % ints(:,:, plt % ds % curDataIndex )
        hisStep = DioPltGetCurHisStep(plt)
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetHisStepIntegers


!*
!* Functions to GET next values and current julian timestep from file
!*

!
! GET floats and current timestep
!
function DioPltGetJulTimeReals(plt, julTime, values) result(retVal)

    ! result value
   
    logical                        :: retVal        ! .true. : success

    ! arguments

    type(DioPltType)               :: plt           ! dataset
    double precision, intent(out)  :: julTime       ! Julian timestep
    real, dimension(:,:), pointer  :: values        ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Real, plt)) then
        values => plt % values % reals(:,:, plt % ds % curDataIndex )
        julTime = DioPltGetCurrentTimeStep(plt)
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetJulTimeReals


!
! GET doubles and current timestep
!
function DioPltGetJulTimeDoubles(plt, julTime, values) result(retVal)

    ! return value

    logical                        :: retVal        ! .true. : success

    ! arguments

    type(DioPltType)                :: plt         ! dataset
    double precision, intent(out)  :: julTime       ! Julian timestep
    double precision, dimension(:,:), pointer     :: values     ! pointer to values

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Double, plt)) then
        values => plt % values % doubles(:,:, plt % ds % curDataIndex ) ! , plt % ds % curDataIndex )
        julTime = DioPltGetCurrentTimeStep(plt)
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetJulTimeDoubles


!
! GET integers and current timestep
!
function DioPltGetJulTimeIntegers(plt, julTime, values) result(retVal)

    ! result value
   
    logical                         :: retVal     ! .true. : success

    ! arguments

    type(DioPltType)                :: plt        ! dataset
    double precision, intent(out)  :: julTime       ! Julian timestep
    integer, dimension(:,:), pointer:: values     ! pointer to values

    ! body

    nullify(values)
    
    call DioDsIncreaseTimestep(plt % ds)
    if (DioPltReadValues(Dio_Plt_Integer, plt)) then
        values => plt % values % ints(:,:, plt % ds % curDataIndex )
        julTime = DioPltGetCurrentTimeStep(plt)
    else
        call DioDsDecreaseTimestep(plt % ds)
    endif

    retVal = associated(values)

end function DioPltGetJulTimeIntegers


!*
!* Functions to GET all values from File
!*

!
! Get all reals
!
function DioPltGetAllReals(plt) result(values)

    ! return value

    real, dimension(:,:,:), pointer :: values     ! pointer to values

    ! arguments

    type(DioPltType)                :: plt         ! dataset

    ! body

    nullify(values)
    
    if (DioPltReadAllValues(Dio_Plt_Real, plt)) then
        values => plt % values % reals
    endif

end function DioPltGetAllReals


!
! Function to read a selection of values from a file
!

function DioPltGetSelectionReals(plt, nPar, parIndx, nLoc, locIndx,&
                                     nTim, timIndx, values) result(retVal)

    ! return value
    logical                               :: retVal  ! .true: succes

    ! arguments   
    type(DioPltType)                      :: plt     ! dataset
    integer         , intent(in)          :: nPar, & ! #indices per dimension
                                             nLoc, &
                                             nTim
    integer, dimension(nPar) , intent(in) :: parIndx ! indices per dimension
    integer, dimension(nLoc) , intent(in) :: locIndx ! indices per dimension
    integer, dimension(nTim) , intent(in) :: timIndx ! indices per dimension
    real, dimension(nPar,nLoc,nTim), &
                               intent(out):: values  ! selected values

    ! body

    retVal = .false.

    if ( plt % ds % inStream % opened ) then
        if (.not. plt % ds % inStream % synched) then

            call DioPltFreeHeader(plt)

            select case ( plt % ds % inStream % streamType )
                case (Dio_HIS_Stream, Dio_WQMap_Stream)
                    retVal = DioPltHisGetSelectionReals( plt,      &
                                    nPar, parIndx, nLoc, locIndx, &
                                    nTim, timIndx, values )
            end select
        endif
    endif

end function DioPltGetSelectionReals


!
! Function to read a selection of values from a file
!

function DioPltGetSelectionDoubles(plt, nPar, parIndx, nLoc, locIndx,&
                                     nTim, timIndx, values) result(retVal)

    ! return value
    logical                               :: retVal   ! .true: succes

    ! arguments   
    type(DioPltType)                      :: plt      ! dataset
    integer         , intent(in)          :: nPar, &  ! #indices per dimension
                                             nLoc, &
                                             nTim
    integer, dimension(nPar) , intent(in) :: parIndx  ! indices per dimension
    integer, dimension(nLoc) , intent(in) :: locIndx  ! indices per dimension
    integer, dimension(nTim) , intent(in) :: timIndx  ! indices per dimension
    double precision, dimension(nPar,nLoc,nTim), &
                               intent(out):: values   ! selected values

    ! locals
    real, dimension(:,:,:), allocatable   :: realvals ! temp real Values
    ! body

    retVal = .false.

    if ( plt % ds % inStream % opened ) then
        if (.not. plt % ds % inStream % synched) then

            call DioPltFreeHeader(plt)

            select case ( plt % ds % inStream % streamType )

                case (Dio_HIS_Stream, Dio_WQMap_Stream)

                    allocate(realvals(nPar,nLoc,nTim))
                    retVal = DioPltHisGetSelectionReals( plt,      &
                                    nPar, parIndx, nLoc, locIndx, &
                                    nTim, timIndx, realVals)
                    if (retVal) values = realVals
                    deallocate(realvals)

            end select

        endif
    endif

end function DioPltGetSelectionDoubles


function DioPltGetSelOneTimeStepReals(plt, timeIndex, values) result(retVal)

    ! return value
    logical                          :: retVal    ! .true: succes

    ! arguments   
    type(DioPltType)                 :: plt       ! dataset
    integer         , intent(in)     :: timeIndex ! required time index
    real, dimension(:,:), intent(out):: values    ! selected values

    ! locals
    integer                          :: nPar      ! #pars
    integer                          :: nLoc      ! #locs
    integer                          :: nTim      ! #times
    integer, dimension(:),allocatable:: parIndx   ! par indices
    integer, dimension(:),allocatable:: locIndx   ! loc indices
    integer, dimension(1)            :: timIndx   ! tim indices
    integer                          :: i         ! loop counter

    ! body

    retVal = .false.

    nPar = plt % header % nPar
    nLoc = plt % header % nLoc

    if ( nPar > 0 .and. nLoc > 0 ) then
        allocate(parIndx(nPar))
        do i = 1, nPar ; parIndx(i) = i ; enddo
        allocate(locIndx(nLoc))
        do i = 1, nLoc ; locIndx(i) = i ; enddo
        nTim = 1 ; timIndx(1) = timeIndex
        retVal = DioPltGetSelectionReals(plt, nPar, parIndx, nLoc, locIndx,&
                                     nTim, timIndx, values)
        deallocate(parIndx)
        deallocate(locIndx)
    endif

end function DioPltGetSelOneTimeStepReals


function DioPltGetSelOneTimeStepDoubles(plt, timeIndex, values) result(retVal)

    ! return value
    logical                          :: retVal    ! .true: succes

    ! arguments   
    type(DioPltType)                 :: plt       ! dataset
    integer         , intent(in)     :: timeIndex ! required time index
    double precision, &
          dimension(:,:), intent(out):: values    ! selected values

    ! locals
    integer                          :: nPar      ! #pars
    integer                          :: nLoc      ! #locs
    integer                          :: nTim      ! #times
    integer, dimension(:),allocatable:: parIndx   ! par indices
    integer, dimension(:),allocatable:: locIndx   ! loc indices
    integer, dimension(1)            :: timIndx   ! tim indices
    integer                          :: i         ! loop counter

    ! body

    retVal = .false.

    nPar = plt % header % nPar
    nLoc = plt % header % nLoc

    if ( nPar > 0 .and. nLoc > 0 ) then
        allocate(parIndx(nPar))
        do i = 1, nPar ; parIndx(i) = i ; enddo
        allocate(locIndx(nLoc))
        do i = 1, nLoc ; locIndx(i) = i ; enddo
        nTim = 1 ; timIndx(1) = timeIndex
        retVal = DioPltGetSelectionDoubles(plt, nPar, parIndx, nLoc, locIndx,&
                                     nTim, timIndx, values)
        deallocate(parIndx)
        deallocate(locIndx)
    endif

end function DioPltGetSelOneTimeStepDoubles


!******************************************************************************
!******************************************************************************
!* PRIVATE FILE TYPE SPECIFIC FUNCTIONS
!******************************************************************************
!******************************************************************************


!******************************************************************************
!* PRIVATE FUNCTIONS FOR WRITING/READING HIS FORMAT
!******************************************************************************

!
! Write PLT Header to HIS file
!
subroutine DioPltHisWriteHeader(plt)

    ! arguments

    type(DioPltType), target        :: plt        ! dataset

    ! locals

    integer                         :: p, l       ! par/loc counters
    integer                         :: lun        ! stream lun
    type(DioPltHeaderType), pointer :: header     ! dataset header
    character(Len=100)              :: dioVersion ! dioVersion

    ! body

    lun = plt % ds % outStream % lun
    header => plt % header
    call DioConfGetStProp(dio_st_AllowJumpBack, plt % header % allowJumpBack)
    if (plt % header % allowJumpBack) then
        plt % ds % timeGrows = .true.
    endif

    ! TODO CHECK REMOVAL header % varType = Dio_Plt_Real

    if ( .not. associated(header % hisRunId) ) then
        allocate(header % hisRunId(HisRunIdDim))
        call DioGetVersion(dioVersion)
        write(header % hisRunId(1),'(A)'  ) 'Deltares HIS file'
        write(header % hisRunId(2),'(A)'  ) 'Generated by DelftIO'
        write(header % hisRunId(3),'(A,A)') dioVersion(1:HisRunIdSize)
        ! Fill hisRunId(4) with T0-string
        call DioDsMakeT0String(plt % ds, header % hisRunId(4))
    endif

    if ( .not. plt % ds % outStream % streamType == Dio_WQMap_stream ) then
        if ( header % nLoc .gt. 0 ) then
            if ( .not. associated(header % hisIntIds) ) then
                allocate(header % hisIntIds(header % nLoc))
                do l = 1, header % nloc
                    header % hisIntIds(l) = l
                enddo
            endif
        endif
    endif

    write ( lun) header % hisRunId
    write ( lun) header % npar, header % nloc
    if ( header % nPar .gt. 0 ) then
        write ( lun) ( header % pars ( p)(1:HisStringSize), p = 1, header % npar)
    endif
    if ( .not. plt % ds % outStream % streamType == Dio_WQMap_stream ) then
        if ( header % nLoc .gt. 0 ) then
            write ( lun) ( header % hisIntIds(l), &
                    header % locs ( l)(1:HisStringSize), l = 1, header % nloc)
        endif
    endif

    call HiaWriteFile(plt)

end subroutine DioPltHisWriteHeader


!
! Write PLT Values to HIS file
!
subroutine DioPltHisWriteValues(plt, lun)

    ! arguments

    type(DioPltType)        :: plt           ! dataset
    integer                 :: lun           ! stream lun

    ! locals

    integer                 :: hisTimeStep   ! timestep in hisFile
    real, dimension(:,:), &
                allocatable :: tempReals     ! tempReals to avoid stack overflow

    ! body

    hisTimeStep = plt % ds % hisStep(plt % ds % curTimeIndex)

    if ( .not. associated(plt % values % reals) ) then
        if ( .not. associated(plt % values % doubles) ) then
            call dioStreamError(223, 'ERROR in DioPltWriteValuesHis:', &
                    'Dataset is not of type REAL or DOUBLES')
        else
            ! convert to Reals
            allocate(tempReals(plt%header%nPar, plt%header%nLoc))
            tempReals = plt % values % doubles(:, :, plt % ds % curDataIndex)
            write ( lun) hisTimeStep, tempReals
            deallocate(tempReals)
        endif
    else
        ! store in tempReals to avoid stack overflow
        allocate(tempReals(plt%header%nPar, plt%header%nLoc))
        tempReals = plt % values % reals(:, :, plt % ds % curDataIndex)
        write ( lun) hisTimeStep, tempReals
        deallocate(tempReals)
    endif

end subroutine DioPltHisWriteValues


!
! Read PLT header from HIS file
!
function DioPltHisReadHeader(plt) result(retVal)

    include 'dio-time-support.inc'              ! for time support functions

    ! return value

    logical                         :: retVal   ! .true.: success

    ! arguments

    type(DioPltType), target        :: plt      ! dataset

    ! locals

    integer                         :: p, l     ! par/loc counters
    integer                         :: lun      ! stream lun
    type(DioPltHeaderType), pointer :: header   ! dataset header
    type(DioDsType), pointer        :: ds       ! dataset info
    logical                         :: timeSet  ! timeSet from T0 String?

    ! body

    lun = plt % ds % inStream % lun
    header => plt % header
    ds     => plt % ds

    rewind(lun)
    retVal = .false.
    if ( .not. associated(header % hisRunId) ) then
        allocate(header % hisRunId(HisRunIdDim))
        header % hisRunId = ''
    endif
    read ( lun, err=999, end=999) header % hisRunId
    read ( lun, err=999, end=999) header % npar, header % nloc
    if ( header % nPar .gt. 0 ) then
        allocate(header % pars(header % nPar))
        header % pars = ''
        read ( lun, err=999, end=999) ( header % pars ( p)(1:HisStringSize), p = 1, header % npar)
    endif
    if ( .not. plt % ds % inStream % streamType == Dio_WQMap_stream ) then
        if ( header % nLoc .gt. 0 ) then
            allocate(header % locs(header % nLoc))
            header % locs = ''
            allocate(header % hisIntIds(header % nLoc))
            header % hisIntIds = 0
            read ( lun, err=999, end=999) ( header % hisIntIds(l), &
                        header % locs ( l)(1:HisStringSize), l = 1, header % nloc)
            do l = 1, header % nloc
            enddo
        endif
    endif

    ! read succesful
    retVal = .true.

999 continue

    if ( retVal ) then

        ! his delwaq contains Dio_Plt_Real values
        header % varType = Dio_Plt_Real
        
        ! Set HIS time Unit, read/check HIA file
        timeSet = DioDsSetTimeFromT0String(ds, header % hisRunId(4))

        call HiaReadFile(plt)

    else

        if (associated(header % hisRunId    )) deallocate(header % hisRunId    )
        if (associated(header % pars        )) deallocate(header % pars        )
        if (associated(header % locs        )) deallocate(header % locs        )
        if (associated(header % parDescripts)) deallocate(header % parDescripts)
        if (associated(header % locDescripts)) deallocate(header % locDescripts)


        nullify(header % hisRunId    )
        nullify(header % pars        )
        nullify(header % locs        )
        nullify(header % parDescripts)
        nullify(header % locDescripts)

        header % nPar = 0
        header % nLoc = 0

    endif

end function DioPltHisReadHeader


!
! Read PLT Values from HIS file
!
function DioPltHisReadValues(plt, lun) result(retVal)

    ! return value

    logical                   :: retVal    ! .true.: success

    ! arguments

    type(DioPltType)          :: plt       ! dataset
    integer                   :: lun       ! stream lun

    ! locals

    integer                 :: hisTimeStep ! timestep in hisFile
    real, dimension(:,:), &
                allocatable :: tempReals     ! tempReals to avoid stack overflow

    ! body

    retVal = .false.

    if ( plt % header % varType .eq. Dio_Plt_Real) then

        allocate(tempReals(plt%header%nPar, plt%header%nLoc))
        read ( lun, err=999, end=999) hisTimeStep, tempReals
        plt % values % reals ( 1:plt%header%nPar, &
                               1:plt%header%nLoc, &
                               plt % ds % curDataIndex) = tempReals
        deallocate(tempReals)
        plt % ds % hisStep(plt % ds % curTimeIndex) = hisTimeStep
        plt % ds % timeStep(plt % ds % curTimeIndex) = & 
                          plt % ds % startTimeVal + &
                           (  dble(hisTimeStep)            * &
                              dble(plt % ds % hisTimeUnit) * &
                              dble(plt % ds % hisTimeMult) / &
                              86400.0D+00                      ) 
        retVal = .true.
        return

999     continue
        deallocate(tempReals)

        if ( plt % ds % inStream % synched ) then
            call dioStreamError(224, 'ERROR in DioPltHisReadValues:', &
                        'Unexpected end of file')
        endif
    else
        call dioStreamError(225, 'ERROR in DioPltHisReadValues:', &
                    'Dataset is not of type REAL')
    endif

end function DioPltHisReadValues


!
! Function to read all timesteps in file
!

subroutine DioPltHisReadAllTimes(plt)

#if (defined(HAVE_CONFIG_H)||defined(salford32))
    type(DioPltType)     :: plt      ! dataset
    call DioStreamError(228, &
        'DioPltHisReadAllTimes not supported for LINUX (ds: ', &
                            plt % ds % name, ')')
#else

#if (defined(WIN32))
    use dfport          ! for fseek
#else
    integer,  external  :: fseek
    integer,  parameter :: SEEK_SET = 0, SEEK_CUR = 1
#endif

    ! arguments
   
    type(DioPltType)     :: plt      ! dataset

    ! locals
    logical                              :: goOn     ! continue reading time steps
    double precision, &
            dimension(DioMaxNrTimeSteps) :: tempStep ! temp stor. of ds timeSteps
    integer, dimension(DioMaxNrTimeSteps):: tempHis  ! temp stor. of ds timeSteps
    real                                 :: lastBlockVal ! last par/loc val in time block

    ! body

    if (associated(plt % ds % preReadTims)) deallocate(plt % ds % preReadTims)
    plt % ds % nPreReadTims = 0

    ! jump to start of values block
    if ( fseek(plt % ds % inStream % lun,      &
               DioPltHisDetermineHeaderSize(plt), &
               SEEK_SET                        ) .ne. 0 ) then
        call DioStreamError(221, 'Could not Skip HIS header')
    else
        ! store initial time information

        tempStep   = plt % ds % timeStep
        tempHis    = plt % ds % hisStep

        ! read all time steps

        plt % ds % timeGrows = .true.

        goOn = .true.
        do while ( goOn )
            call DioDsIncreaseTimestep(plt % ds)
            read(plt % ds % inStream % lun, err=999, end=999) &
                    plt % ds % hisStep (plt % ds % curTimeIndex)
            plt % ds % timeStep(plt % ds % curTimeIndex) = & 
                  plt % ds % startTimeVal + &
                   (  dble(plt % ds % hisStep(plt % ds % curTimeIndex)) * &
                      dble(plt % ds % hisTimeUnit) * &
                      dble(plt % ds % hisTimeMult) / &
                      86400.0D+00                      ) 
            if ( fseek(plt % ds % inStream % lun,                   &
                       DioPltHisDetermineBlockSize(plt) - integerSize - realSize, &
                       SEEK_CUR                    ) .ne. 0 ) then
                goOn = .false.
            else
                read(plt % ds % inStream % lun, err=999, end=999) lastBlockVal
            endif
            cycle

    999     continue    ! error when reading time step
            call DioDsDecreaseTimestep(plt % ds)
            goOn = .false.

        enddo

        plt % ds % timeGrows   = .false.

        ! store all read time steps (by copying the read timestep array)

        plt % ds % nPreReadTims = plt % ds % curTimeIndex
        allocate(plt % ds % preReadTims(plt % ds % nPreReadTims))
        plt % ds % preReadTims = plt % ds % timestep
        deallocate(plt % ds % timestep)

        ! restore initial time information

        allocate(plt % ds % timeStep(DioMaxNrTimeSteps)) 
        plt % ds % timeStep   = tempStep   

        deallocate(plt % ds % hisStep)
        allocate(plt % ds % hisStep(DioMaxNrTimeSteps))
        plt % ds % hisStep    = tempHis    

        ! reset to 'nothing read yet'

        plt % ds % curDataIndex   = 0
        plt % ds % curTimeIndex   = 0
        plt % ds % curPutGetCount = 0

        if ( fseek(plt % ds % inStream % lun,      &
                   DioPltHisDetermineHeaderSize(plt), &
                   SEEK_SET                        ) .ne. 0 ) then
            call DioStreamError(222, 'Could not ReSkip HIS header')
        endif

    endif             
   
#endif

end subroutine DioPltHisReadAllTimes


!
! Function to read a selection of values from a file
!

function DioPltHisGetSelectionReals(plt, nPar, parIndx, nLoc, locIndx,&
                                     nTim, timIndx, values) result(retVal)

#if (defined(HAVE_CONFIG_H)||defined(salford32))
    logical                                     :: retVal 
    type(DioPltType)                            :: plt
    integer         , intent(in)                :: nPar, nLoc, nTim
    integer, dimension(nPar) , intent(in)       :: parIndx
    integer, dimension(nLoc) , intent(in)       :: locIndx
    integer, dimension(nTim) , intent(in)       :: timIndx
    real, dimension(nPar,nLoc,nTim), intent(out):: values
    call DioStreamError(228, &
        'DioPltHisGetSelectionReals not supported for LINUX (ds: ', &
                            plt % ds % name, ')')
    values = 0
    retVal = .false.
#else

#if (defined(WIN32))
    use dfport          ! for fseek
#else
    integer, external   :: fseek
    integer,  parameter :: SEEK_SET = 0, SEEK_CUR = 1
#endif

    ! return value
    logical                               :: retVal  ! .true: succes

    ! arguments   
    type(DioPltType)                      :: plt     ! dataset
    integer         , intent(in)          :: nPar, & ! #indices per dimension
                                             nLoc, &
                                             nTim
    integer, dimension(nPar) , intent(in) :: parIndx ! indices per dimension
    integer, dimension(nLoc) , intent(in) :: locIndx ! indices per dimension
    integer, dimension(nTim) , intent(in) :: timIndx ! indices per dimension
    real, dimension(nPar,nLoc,nTim), &
                               intent(out):: values  ! selected values

    ! locals
   
    integer         :: lun          ! lun to read from
    integer         :: par, loc, t  ! loop counters
    integer         :: tIndxAt      ! Time-index at file pointer
    integer         :: headerSize,& ! size of HIS header 
                       blockSize ,& ! size of HIS time step block
                       nJump     ,& ! #time blocks to skip
                       jumpSize     ! size of block to skip in file
    integer         :: dummy        ! dummy for HIS step
    real, dimension(:,:), &
          allocatable :: reals      ! temp storage for values

    ! body

    retVal = .false.

    headerSize = DioPltHisDetermineHeaderSize(plt)
    blockSize  = DioPltHisDetermineBlockSize(plt)

    ! jump to start of values block
    lun = plt % ds % inStream % lun
    if ( fseek(lun, headerSize, SEEK_SET) .ne. 0 ) then
        call DioStreamError(226, 'Could not Skip HIS header')
    else
        allocate(reals(plt % header % nPar, plt % header % nLoc))

        tIndxAt = 1
        do t = 1, nTim
            if ( timIndx(t) .gt. 0 ) then
                nJump    = (timIndx(t) - tIndxAt)
                jumpSize = nJump * blockSize
                if ( fseek(lun, jumpSize, SEEK_CUR) .ne. 0 ) then
                    exit
                else
                    read(lun, err=999, end=999) dummy, reals
                    do par = 1, nPar
                        do loc = 1, nLoc
                            if ( (parIndx(par) .gt. 0)                   .and. &
                                 (locIndx(loc) .gt. 0)                   .and. &
                                 (parIndx(par) .le. plt % header % nPar) .and. &
                                 (locIndx(loc) .le. plt % header % nLoc)      ) then
                                values(par,loc,t) = reals(parIndx(par), locIndx(loc))
                            endif
                        enddo
                    enddo
                    retVal = .true.
                endif

                tIndxAt = timIndx(t)+1

                cycle

            999 continue    ! error when reading time step
                exit

            endif

        enddo

        deallocate(reals)

        if ( fseek(lun, headerSize, SEEK_SET ) .ne. 0 ) then
            call DioStreamError(227, 'Could not ReSkip HIS header')
        endif

    endif             

#endif

end function DioPltHisGetSelectionReals


!
! Determine size of HIS header (for jumping through file)
!
function DioPltHisDetermineHeaderSize(plt) result(retVal)

    ! return value
    integer          :: retVal  ! headersize

    ! arguments
    type(DioPltType) :: plt     ! dataset

    ! locals
    logical          :: isWQMap ! WQMap file?

    ! body: calc. size
    isWQMap = .false.
    if ( plt % ds % inStream % streamType == Dio_WQMap_stream ) then
        isWQMap = .true.
    else
        if ( plt % ds % outStream % streamType == Dio_WQMap_stream ) then
            isWQMap = .true.
        else
        endif
    endif
    
    retVal = HisRunIdSize * HisRunIdDim            + & !Runid String
             integerSize  + integerSize            + & !#pars/#locs
             plt % header % nPar * HisStringSize       !par strings

    if ( .not. isWQMap ) then
        retVal = retVal              + &
                 plt % header % nLoc * &
                 ( integerSize + HisStringSize )       ! loc indices/strings
    endif

end function DioPltHisDetermineHeaderSize


!
! Determine size of HIS values block (for jumping through file)
!
function DioPltHisDetermineBlockSize(plt) result(retVal)

    ! return value
    integer          :: retVal ! timestep block size

    ! arguments
    type(DioPltType) :: plt    ! dataset

    ! body: calc. size (his-timestep + (#values in block) )

    retVal = integerSize + ( realSize            * &
                             plt % header % nPar * &
                             plt % header % nLoc )

end function DioPltHisDetermineBlockSize


!******************************************************************************
!* PRIVATE FUNCTIONS FOR WRITING/READING HIA FILE
!******************************************************************************

!*
!* Functions for setting/getting long Pars/Locs in PLT
!*

!
! Get long Parameters
!
function GetLongPars(plt, indices, longNames) result(retVal)
    ! result
    integer                         :: retVal    ! #long par's present
    ! arguments
    type(DioPltType), intent(in)    :: plt       ! PLT dataset
    integer, dimension(:), pointer  :: indices   ! indices of long par. names
    character(Len=DioMaxParLen), &
        dimension(:), pointer       :: longNames ! long par. names
    ! locals
    integer                         :: i         ! loop counter

    retVal = 0
    nullify(longNames)
    nullify(indices)
    do i = 1, plt % header % nPar
        if ( len_trim(plt % header % pars(i)) .gt. HisStringSize) then
            retVal = retVal + 1
            if ( .not. associated(longNames) ) then
                allocate(longNames(plt % header % nPar))
                allocate(indices(plt % header % nPar))
            endif
            longNames(retVal) = plt % header % pars(i)
            indices(retVal)   = i
        endif
    enddo

end function GetLongPars


!
! Get long locations
!
function GetLongLocs(plt, indices, longNames) result(retVal)
    ! result
    integer                         :: retVal    ! #long loc's present
    ! arguments
    type(DioPltType), intent(in)    :: plt       ! PLT dataset
    integer, dimension(:), pointer  :: indices   ! indices of long loc. names
    character(Len=DioMaxLocLen), &
        dimension(:), pointer       :: longNames ! long loc. names
    ! locals
    integer                         :: i         ! loop counter

    retVal = 0
    nullify(longNames)
    nullify(indices)
    do i = 1, plt % header % nLoc
        if ( len_trim(plt % header % locs(i)) .gt. HisStringSize) then
            retVal = retVal + 1
            if ( .not. associated(longNames) ) then
                allocate(longNames(plt % header % nLoc))
                allocate(indices(plt % header % nLoc))
            endif
            longNames(retVal) = plt % header % locs(i)
            indices(retVal)   = i
        endif
    enddo

end function GetLongLocs


!*
!* Functions for setting/getting descriptions in PLT
!*


!
! Get Parameters
!
function AnalyzeParDescriptions(plt, indices, descriptions) result(retVal)

    ! result
    integer                         :: retVal       ! #par's present

    ! arguments
    type(DioPltType), intent(in)    :: plt          ! PLT dataset
    integer, dimension(:), pointer  :: indices      ! indices of par. descriptions
    character(Len=DioMaxDescrLen), &
        dimension(:), pointer       :: descriptions ! par. descriptions

    ! locals
    integer                         :: i            ! loop counter

    retVal = 0
    nullify(descriptions)
    nullify(indices)
    if ( associated(plt % header % parDescripts) ) then
        do i = 1, plt % header % nPar
            if ( plt % header % parDescripts(i) /= '' ) then
                retVal = retVal + 1
                if ( .not. associated(descriptions) ) then
                    allocate(descriptions(plt % header % nPar))
                    allocate(indices(plt % header % nPar))
                endif
                descriptions(retVal) = plt % header % parDescripts(i)
                indices(retVal)   = i
            endif
        enddo
    endif

end function AnalyzeParDescriptions


!
! Get locations
!
function AnalyzeLocDescriptions(plt, indices, descriptions) result(retVal)

    ! result
    integer                         :: retVal       ! #loc's present

    ! arguments
    type(DioPltType), intent(in)    :: plt          ! PLT dataset
    integer, dimension(:), pointer  :: indices      ! indices of loc. descriptions
    character(Len=DioMaxDescrLen), &
        dimension(:), pointer       :: descriptions ! loc. descriptions

    ! locals
    integer                         :: i            ! loop counter

    retVal = 0
    nullify(descriptions)
    nullify(indices)
    if ( associated(plt % header % locDescripts) ) then
        do i = 1, plt % header % nLoc
            if ( plt % header % locDescripts(i) /= '' ) then
                retVal = retVal + 1
                if ( .not. associated(descriptions) ) then
                    allocate(descriptions(plt % header % nLoc))
                    allocate(indices(plt % header % nLoc))
                endif
                descriptions(retVal) = plt % header % locDescripts(i)
                indices(retVal)   = i
            endif
        enddo
    endif

end function AnalyzeLocDescriptions


!*
!* Private functions for Writing the file
!*


!
! Determine HIA file name
!
subroutine HiaDetermineFileName(name, hiaName, streamType)

    ! arguments
    character(Len=*), intent(IN)   :: name       ! his stream name
    character(Len=*), intent(OUT)  :: hiaName    ! hia file name
    integer                        :: streamType ! map or his

    ! locals
    integer                        :: extStart   ! ext. start
    character(Len=DioMaxStreamLen) :: fileExt    ! fileExtension
    character(Len=DioMaxStreamLen) :: newExt     ! Hia or Maa ext.

    ! body:
    ! - determ. new ext. name (hia or maa)
    ! - find extension: replace by or add new ext.

    newExt = HisLongExt
    if ( streamType .eq. Dio_WQMap_Stream ) newExt = MapLongExt

    hiaName = name
    extStart = DioStreamGetExtension(hiaName, fileExt)
    if ( extStart == 0 ) then
        ! add extension
        hiaName = trim(hiaName) // newExt
    else
        ! replace extension
        hiaName(extStart:) = newExt
    endif

end subroutine HiaDetermineFileName


!
! Write General Part of HIA file
!
subroutine HiaWriteGeneralPart(lun, plt)

    ! arguments
    integer                      :: lun          ! lun to write to
    type(DioPltType), intent(IN) :: plt          ! PLT dataset

    ! locals
    character(Len=DioHiaLineLen) :: dioVersion   ! DelftIO Version
    character(Len=DioHiaLineLen) :: timeStepUnit ! HIS time step unit string

    ! body

    call DioGetVersion(dioVersion)

    timeStepUnit=' '
    select case (plt % ds % hisTimeUnit)
        case (1)
            timeStepUnit='Second'
        case (60)
            timeStepUnit='Minute'
        case (3600)
            timeStepUnit='Hour'  ! TODO extend
    end select


    call DioIniWriteGroupLine(lun, GeneralGroup)
    if ( plt % ds % outStream % streamType .eq. Dio_WQMap_Stream ) then
        call DioIniWriteStrIsStrLine(lun, FileTypeKey, 'DelftIO MAA file')
    else
        call DioIniWriteStrIsStrLine(lun, FileTypeKey, 'DelftIO HIA file')
    endif
    call DioIniWriteStrIsStrLine(lun, DioVersKey,  dioVersion)
    call DioIniWriteStrIsStrLine(lun, HiaVersKey,  hiaVersion)
    write(lun, *)                                ! empty line

    call DioIniWriteGroupLine(lun, DioCheckGroup)
    call DioIniWriteStrIsIntLine(lun, NumParsKey, plt % header % nPar)
    call DioIniWriteStrIsIntLine(lun, NumLocsKey, plt % header % nLoc)
    call DioIniWriteStrIsStrLine(lun, T0Key,     plt % header % hisRunId(4)(5:23))
    call DioIniWriteStrIsStrLine(lun, TSUnitKey, timeStepUnit)
    call DioIniWriteStrIsIntLine(lun, TSMultKey, plt % ds % hisTimeMult)

    write(lun, *)                                ! empty line

end subroutine HiaWriteGeneralPart


!*
!* Private functions for Reading the file
!*


!
! Read General Part of HIA file
!
function HiaReadGeneralPart_V100(hiaFile, plt, dioVersionInFile) result(retVal)

    ! result
    logical                      :: retVal           ! success

    ! arguments
    type(TDioIniFile)            :: hiaFile          ! HIA ini file
    type(DioPltType), intent(IN) :: plt              ! PLT dataset
    character(Len=*),intent(OUT) :: dioVersionInFile ! dioVersion in File

    ! locals
    character(Len=DioMaxIniLineLen):: hiaVersionInFile ! hiaVersion in File
    integer                        :: nPar, nLoc       ! #pars/#locs in HIA
    character(Len=DioMaxIniLineLen):: T0               ! T0
    character(Len=DioMaxIniLineLen):: timeStepUnitStr  ! timestep unit in HIA
    integer                        :: timeStepUnit     ! timestep unit in HIA
    integer                        :: timeStepMult     ! timestep mult. in HIA
    logical                        :: findRes          ! result of find call
    character(Len=DioMaxTimLen)    :: DioT0inHIS       ! T0 in His as Dio str.
    character(Len=DioMaxTimLen)    :: DioT0inHIA       ! T0 in His as Dio str.

    ! body
    
    !
    ! Read General Group
    !
    retVal = .true.

    call DioIniSetStopAtGroup(hiaFile, LongParGroup)

    dioVersionInFile = ' '
    hiaVersionInFile = ' '
    findRes = DioIniFindItem(hiaFile, GeneralGroup, DioVerskey, dioVersionInFile)
    findRes = DioIniFindItem(hiaFile, GeneralGroup, HiaVerskey, hiaVersionInFile)

    if ( DioIniFindGroup(hiaFile, DioCheckGroup ) ) then
        findRes = DioIniFindIntItem(hiaFile, DioCheckGroup, NumParsKey, nPar)
        if ( findRes ) then
            if ( nPar .ne. plt % header % nPar ) then
                call DioStreamError(241, 'Incompatible #pars in HIA file for', &
                                                trim(plt % ds % name) )
                retVal = .false.
            endif
        endif

        findRes = DioIniFindIntItem(hiaFile, DioCheckGroup, NumLocsKey, nLoc)
        if ( findRes ) then
            if ( nLoc .ne. plt % header % nLoc ) then
                call DioStreamError(243, 'Incompatible #locs in HIA file for', &
                                                trim(plt % ds % name) )
                retVal = .false.
            endif
        endif

        findRes = DioIniFindItem(hiaFile, DioCheckGroup, T0Key, T0)
        if ( findRes ) then
            call DioHisTime2DioTime(plt % header % hisRunId(4)(5:23), DioT0inHIS)
            call DioHisTime2DioTime(T0                              , DioT0inHIA)
            if ( DioT0inHIS .ne. DioT0inHIA ) then
                call DioStreamError(245, 'Incompatible T0 in HIA file for', &
                                                trim(plt % ds % name) )
                retVal = .false.
            endif
        endif

        findRes = DioIniFindItem(hiaFile, DioCheckGroup, TSUnitKey, timeStepUnitStr)
        if ( findRes ) then
            select case (timeStepUnitStr)
                case ('Second')
                    timeStepUnit=1
                case ('Minute')
                    timeStepUnit=60
                case ('Hour')
                    timeStepUnit=3600  ! TODO extend
            end select
            if ( timeStepUnit .ne. plt % ds % hisTimeUnit ) then
                call DioStreamError(247, 'Incompatible Time Step Unit in HIA file for', &
                                                trim(plt % ds % name) )
                retVal = .false.
            endif
        endif

        findRes = DioIniFindIntItem(hiaFile, DioCheckGroup, TSMultKey, timeStepMult)
        if ( findRes ) then
            if ( timeStepMult .ne. plt % ds % hisTimeMult) then
                call DioStreamError(249, 'Incompatible Time Step Multiplier in HIA file for', &
                                                trim(plt % ds % name) )
                retVal = .false.
            endif
        endif

    endif

    call DioIniUnsetStopAtGroup(hiaFile)

end function HiaReadGeneralPart_V100


!
! Check if the HIA file has to be overwritten. Delete it, if so
!
function HiaOverwriteFile(hiaName, doDelete) result(overWrite)

    ! return value
    logical                        :: overWrite ! overwrite HIA file?

    ! arguments
    character(Len=*)               :: hiaName   ! HIA File Name
    logical                        :: doDelete  ! do delete indeeed?

    ! locals
    type(TDioIniFile)              :: hiaFile   ! HIA IniFile Handle
    character(Len=DioMaxIniLineLen):: hiaStatus ! value of hiaStatus key

    overWrite = .true.
    if ( DioIniFileOpen(hiaFile, hiaName, 'r') ) then
        hiaStatus = ' '
        if ( DioIniFindItem(hiaFile, GeneralGroup, HiaStatusKey, hiaStatus) ) then
            if ( StringsEqual(CaseInsens, hiaStatus, 'Keep' ) ) then
                overWrite = .false.
            endif
        endif
        if ( overWrite .and. doDelete ) then
            call DioIniFileCloseAndDelete(hiaFile)
        else
            call DioIniFileClose(hiaFile)
        endif
    endif

end function HiaOverwriteFile

!
! Write the HIA file, if applicable
!
subroutine HiaWriteFile(plt)

    ! arguments
    type(DioPltType), intent(IN)   :: plt         ! PLT dataset

    ! locals
    character(Len=DioMaxStreamLen) :: hiaName     ! HIA File Name
    type(TDioIniFile)              :: hiaFile     ! HIA IniFile Handle
    character(Len=DioMaxParLen),&
        dimension(:), pointer      :: longPars    ! long parameter names
    integer, dimension(:), pointer :: parIndices  ! long parameter indices
    integer                        :: nLongPars   ! #long parameter names

    character(Len=DioMaxLocLen),&
        dimension(:), pointer      :: longLocs    ! long location names
    integer, dimension(:), pointer :: locIndices  ! long location indices
    integer                        :: nLongLocs   ! #long location names

    integer                        :: lun         ! file handle
    integer                        :: i           ! counter

    call HiaDetermineFileName(plt % ds % outStream % name, hiaName, &
                              plt % ds % outStream % streamType )

    if ( .not. HiaOverwriteFile(hiaName, .true.) ) then
        return
    endif

    nLongLocs = 0
    nLongPars = GetLongPars(plt, parIndices, longPars)
    if ( .not. plt % ds % outStream % streamType == Dio_WQMap_stream ) then
        nLongLocs = GetLongLocs(plt, locIndices, longLocs)
    endif

    if ( nLongPars .gt. 0 .or. nLongLocs .gt. 0 ) then
        if ( DioIniFileOpen(hiaFile, hiaName, 'w') ) then

            lun = hiaFile % lun

            call HiaWriteGeneralPart(lun, plt) 

            ! write long parameters
            call DioIniWritegroupLine(lun, LongParGroup) 
            do i = 1, nLongPars
                call DioIniWriteIntIsStrLine(lun, parIndices(i), longPars(i)) 
            enddo    
            write(lun, *)                                 ! empty line

            ! write long locations (not for WQMap file)
            if ( .not. plt % ds % outStream % streamType == Dio_WQMap_stream ) then
                call DioIniWritegroupLine(lun, LongLocGroup) 
                do i = 1, nLongLocs
                    call DioIniWriteIntIsStrLine(lun, locIndices(i), longLocs(i)) 
                enddo    
                write(lun, *)                                 ! empty line
            endif

            call DioIniFileClose(hiaFile)
        endif
    endif

    if ( nLongPars .gt. 0 ) then
        deallocate(parIndices) ; deallocate(longPars)
    endif
    if ( nLongLocs .gt. 0 ) then
        deallocate(locIndices) ; deallocate(longLocs)
    endif

end subroutine HiaWriteFile


!
! Write Descriptions to the the HIA file
!
subroutine HiaWriteDescriptions(plt, descr_type)

    ! arguments
    type(DioPltType), intent(in)   :: plt              ! PLT dataset
    integer         , intent(in)   :: descr_type       ! type: parameters or locations

    ! locals
    character(Len=DioMaxStreamLen) :: hiaName          ! HIA File Name
    type(TDioIniFile)              :: hiaFile          ! HIA IniFile Handle
    character(Len=DioMaxDescrLen),&
        dimension(:), pointer      :: parDiscriptions  ! parameter descriptions
    integer, dimension(:), pointer :: parIndices       ! parameter indices
    integer                        :: nParDiscriptions ! #parameter descriptions

    character(Len=DioMaxDescrLen),&
        dimension(:), pointer      :: locDiscriptions  ! location descriptions
    integer, dimension(:), pointer :: locIndices       ! location indices
    integer                        :: nLocDiscriptions ! #location descriptions

    logical                        :: hiaExists        ! hia already there?
    logical                        :: hiaOpened        ! result of ini open call

    integer                        :: lun              ! file handle
    integer                        :: i                ! counter

    call HiaDetermineFileName(plt % ds % outStream % name, hiaName, &
                              plt % ds % outStream % streamType )

    if ( .not. HiaOverwriteFile(hiaName, .false. ) ) then
        return
    endif

    nLocDiscriptions = 0
    nParDiscriptions = 0

    if ( descr_type == dio_plt_pars ) then
        nParDiscriptions = AnalyzeParDescriptions(plt, parIndices, parDiscriptions)
    endif

    if ( descr_type == dio_plt_locs ) then
        nLocDiscriptions = AnalyzeLocDescriptions(plt, locIndices, locDiscriptions)
    endif

    if ( nParDiscriptions .gt. 0 .or. nLocDiscriptions .gt. 0 ) then

        ! Open hia file for append, or write header if the file is not there yet

        hiaExists = .false.

        if ( DioIniFileOpen(hiaFile, hiaName, 'r') ) then
            call DioIniFileClose(hiaFile)
            hiaExists = .true.
        endif

        if ( hiaExists ) then
            hiaOpened = DioIniFileOpen(hiaFile, hiaName, 'a')
        else
            hiaOpened = DioIniFileOpen(hiaFile, hiaName, 'w')
            if ( hiaOpened ) then
                call HiaWriteGeneralPart(hiaFile % lun, plt) 
            endif
        endif

        if ( hiaOpened ) then

            lun = hiaFile % lun

            ! write parameters
            if ( nParDiscriptions > 0 ) then
                call DioIniWritegroupLine(lun, ParDescriptGroup) 
                do i = 1, nParDiscriptions
                    call DioIniWriteIntIsStrLine(lun, parIndices(i), parDiscriptions(i)) 
                enddo    
                write(lun, *)                                 ! empty line
            endif

            ! write parameters
            if ( nLocDiscriptions > 0 ) then
                call DioIniWritegroupLine(lun, LocDescriptGroup) 
                do i = 1, nLocDiscriptions
                    call DioIniWriteIntIsStrLine(lun, locIndices(i), locDiscriptions(i)) 
                enddo    
                write(lun, *)                                 ! empty line
            endif

            call DioIniFileClose(hiaFile)
        endif
    endif

    if ( nParDiscriptions .gt. 0 ) then
        deallocate(parIndices) ; deallocate(parDiscriptions)
    endif
    if ( nLocDiscriptions .gt. 0 ) then
        deallocate(locIndices) ; deallocate(locDiscriptions)
    endif

end subroutine HiaWriteDescriptions


!
! Read the HIA file, if applicable
!
subroutine HiaReadFile(plt)

    ! arguments
    type(DioPltType) :: plt    ! PLT dataset

    ! locals
    character(Len=DioMaxStreamLen) :: hiaName       ! HIA File Name
    type(TDioIniFile)              :: hiaFile       ! HIA IniFile Handle
    logical                        :: readRes       ! local return value
    character(Len=DioHiaLineLen)   :: dioVersInFile ! DelftIO Vers. in HIA
    character(Len=DioMaxParLen)    :: parName       ! HIA long par name
    character(Len=DioMaxLocLen)    :: locName       ! HIA long loc name
    integer                        :: indx          ! HIA index for long name

    call HiaDetermineFileName(plt % ds % inStream % name, hiaName, &
                              plt % ds % inStream % streamType  )

    if ( DioIniFileOpen(hiaFile, hiaName, 'r') ) then

        readRes = HiaReadGeneralPart_V100(hiaFile, plt, dioVersInFile)
        if (readRes) then

            ! Read Long Parameters

            if (DioIniFindGroup(hiaFile, LongParGroup ) ) then
                do while ( DioIniNextIntKeyLine(hiaFile, indx, parName) )
                    if ( indx > 0 .and. indx <= plt % header % nPar ) then
                        plt % header % pars(indx) = parName
                    endif
                enddo
            endif

            ! Read Long Locations (not for Map type)

            if ( .not. plt % ds % inStream % streamType == Dio_WQMap_stream ) then
                if (DioIniFindGroup(hiaFile, LongLocGroup ) ) then
                    do while ( DioIniNextIntKeyLine(hiaFile, indx, locName) )
                        if ( indx > 0 .and. indx <= plt % header % nLoc ) then
                            plt % header % locs(indx) = locName
                        endif
                    enddo
                endif
            endif

            ! Read Location Descriptions

            if ( .not. plt % ds % inStream % streamType == Dio_WQMap_stream ) then
                if (DioIniFindGroup(hiaFile, LocDescriptGroup ) ) then
                    allocate(plt % header % locDescripts(plt % header % nLoc))
                    plt % header % locDescripts = ''
                    do while ( DioIniNextIntKeyLine(hiaFile, indx, locName) )
                        if ( indx > 0 .and. indx <= plt % header % nLoc ) then
                            plt % header % locDescripts(indx) = locName
                        endif
                    enddo
                endif
            endif

            ! Read Parameter Descriptions

            if (DioIniFindGroup(hiaFile, ParDescriptGroup ) ) then
                allocate(plt % header % parDescripts(plt % header % nPar))
                plt % header % parDescripts = ''
                do while ( DioIniNextIntKeyLine(hiaFile, indx, parName) )
                    if ( indx > 0 .and. indx <= plt % header % nPar ) then
                        plt % header % parDescripts(indx) = parName
                    endif
                enddo
            endif

        endif

        call DioIniFileClose(hiaFile)

    endif

end subroutine HiaReadFile


!******************************************************************************
!* PRIVATE FUNCTIONS FOR WRITING/READING DATA TO ASCII/BIN FORMAT
!******************************************************************************

!
! Write PLT Header to ASCII or Binary file
!

subroutine DioPltWriteHeaderASCIIorBIN(plt)

    ! arguments

    type(DioPltType), target        :: plt         ! dataset

    ! locals

    type(DioPltHeaderType), pointer :: header      ! dataset header
    integer                         :: p           ! counter
    character (len=10)              :: hdrFormat   ! Format string par./loc.
    character (len=10)              :: hisIdFormat ! Format His Runid
    integer                         :: lun         ! stream lun
    integer                         :: sType       ! stream lun (ASCII/BIN)

    ! body: read all header info

    header => plt % header
    lun    =  plt % ds % outStream % lun
    sType  =  plt % ds % outStream % streamType

    if ( sType .eq. Dio_ASCII_Stream) then

        hdrFormat='(Axxx)'
        write(hdrFormat(3:5), '(I3.3)') DioMaxParLen
        hisIdFormat='(Axxx)'
        write(hisIdFormat(3:5), '(I3.3)') HisRunIdSize

        write(lun,'(A12,I5)') 'Parameters',header % nPar
        do p = 1, header % nPar
            write(lun,hdrFormat) header % pars(p)
        enddo

        write(lun,'(A12,I5)') 'Locations', header % nLoc
        do p = 1, header % nLoc
            write(lun,hdrFormat) header % locs(p)
        enddo

        write(lun,'(A20,L1)') 'HisInfo available ', associated(header % hisRunId)
        if ( associated(header % hisRunId) ) then
            do p = 1, HisRunIdDim
                write(lun,hisIdFormat) header % hisRunId(p)
            enddo
        endif

        write(lun,'(A12,I5)') 'VarType',   header % varType

    else if ( sType .eq. Dio_Binary_Stream) then

        write(lun)                 header % nPar
        write(lun) header % pars
        write(lun)                 header % nLoc
        write(lun) header % locs
        write(lun)                 associated(header % hisRunId)
        if ( associated(header % hisRunId) ) then
            write(lun) header % hisRunId
        endif
        write(lun)                 header % varType

    endif

end subroutine DioPltWriteHeaderASCIIorBIN


!
! Read PLT Header from ASCII or Binary file
!

subroutine DioPltReadHeaderASCIIorBIN(plt)

    ! arguments

    type(DioPltType), target        :: plt         ! dataset

    ! locals

    type(DioPltHeaderType), pointer :: header      ! dataset header
    integer                         :: p           ! counter
    character (len=10)              :: hdrFormat   ! Format string par./loc.
    character (len=10)              :: hisIdFormat ! Format His Runid
    character(DioMaxParLen)         :: dummy       ! dummy for reading strings
    integer                         :: lun         ! stream lun
    integer                         :: sType       ! stream lun (ASCII/BIN)
    logical                         :: hasRunId    ! RUNID present?

    ! body: write all header info

    header => plt % header
    lun    =  plt % ds % inStream % lun
    sType  =  plt % ds % inStream % streamType

    rewind(lun)

    if ( sType .eq. Dio_ASCII_Stream) then

        hdrFormat='(Axxx)'
        write(hdrFormat(3:5), '(I3.3)') DioMaxParLen
        hisIdFormat='(Axxx)'
        write(hisIdFormat(3:5), '(I3.3)') HisRunIdSize

        read (lun,'(A12,I5)') dummy,  header % nPar
        allocate(header % pars(header % nPar))
        ! write(*,*) 'ALLOC pars'
        if ( header % nPar .gt. 0 ) then
            do p = 1, header % nPar
                read (lun,hdrFormat) header % pars(p)
            enddo
        endif

        read (lun,'(A12,I5)') dummy,  header % nLoc
        allocate(header % locs(header % nLoc))
        ! write(*,*) 'ALLOC locs'
        if ( header % nLoc .gt. 0 ) then
            do p = 1, header % nLoc
                read (lun,hdrFormat) header % locs(p)
            enddo
        endif

        read(lun,'(A20,L1)') dummy, hasRunid
        if ( hasRunid ) then
            if ( .not. associated(plt % header % hisRunId) ) &
                allocate(plt % header % hisRunId(HisRunIdDim))
            do p = 1, HisRunIdDim
                read(lun,hisIdFormat) header % hisRunId(p)
            enddo
        endif

        read (lun,'(A12,I5)') dummy,  header % varType

    else if ( sType .eq. Dio_Binary_Stream) then

        read (lun) header % nPar
        allocate(header % pars(header % nPar))
        ! write(*,*) 'ALLOC pars'
        if ( header % nPar .gt. 0 ) then
            read (lun) header % pars
        endif

        read (lun) header % nLoc
        allocate(header % locs(header % nLoc))
        ! write(*,*) 'ALLOC locs'
        if ( header % nLoc .gt. 0 ) then
            read (lun) header % locs
        endif

        read(lun) hasRunid
        if ( hasRunid ) then
            allocate(header % hisRunId(HisRunIdDim))
            read(lun) header % hisRunId
        endif

        read (lun) header % varType

    endif

end subroutine DioPltReadHeaderASCIIorBIN


end module Dio_plt_rw

