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
!!! Dio-const-rw: Datasets for one const
!!!
!!! (c) Deltares, nov 2002
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!! File organization:
!!!
!!!    Declarations
!!!    - decl. of public constants, enumerations, datatypes and interfaces
!!!    - decl. of private module functions/subroutines
!!!
!!!    Implementation of public functions
!!!    - public functions for defining a Const dataset
!!!    - public functions for getting a Const dataset
!!!    - public functions to set/get values in Const dataset
!!!    - public functions to read/write Const dataset
!!!
!!!    Implementation of private File Type Specific functions
!!!    - writing/reading to/from NEFIS file
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module Dio_const_rw

use Dio_ds

implicit none

!******************************************************************************
!* PUBLIC CONSTANTS, ENUMERATIONS, DATATYPES and INTERFACES
!******************************************************************************

!*
!* Definition of data types
!*

!
! Const Dataset
!
type DioConstType
    type(DioDsType)  :: ds        ! const dataset
    integer          :: vartype   ! real, int, etc.
    integer          :: intval    ! dataset value int
    real             :: realval   ! dataset value real
    double precision :: doubleval ! dataset value dble
    logical          :: logval    ! dataset value logical
end type DioConstType


!*
!* PUBLIC INTERFACES (overloaded functions)
!*

interface DioConstSetValue
    module procedure DioConstSetValueReal
    module procedure DioConstSetValueDouble
    module procedure DioConstSetValueInteger
end interface

interface DioConstGetValue
    module procedure DioConstGetValueReal
    module procedure DioConstGetValueDouble
    module procedure DioConstGetValueInteger
end interface


interface DioConstPut
    module procedure DioConstPutReal
    module procedure DioConstPutDouble
    module procedure DioConstPutInteger
end interface


interface DioConstGetDataset
    module procedure DioConstGetDataset
end interface


interface DioConstGet
    module procedure DioConstGetReal
    module procedure DioConstGetDouble
    module procedure DioConstGetInteger
end interface

!******************************************************************************
!* DECL. OF PRIVATE FUNCTIONS/SUBROUTINES AND CONSTANTS
!******************************************************************************


!*
!* private functions/subroutines
!*

private :: DioConstCanWrite
private :: DioConstCanRead

private :: DioConstNefisWriteHeader
private :: DioConstNefisWriteValue
private :: DioConstNefisReadHeader
private :: DioConstNefisReadValue


contains


!******************************************************************************
!******************************************************************************
!* PRIVATE FUNCTIONS
!******************************************************************************
!******************************************************************************


!*
!* PRIVATE Functions to check if Const can Read/Write
!*

function DioConstCanWrite(const) result(retVal)

    ! return value
    logical                         :: retVal ! success

    ! arguments
    type(DioConstType), intent(in) :: const ! dataset

    ! body
    if ( const % ds % outStream % synched ) then
        call DioStreamError(552, 'Consts not supported for synched streams, ', trim(const % ds % name))
    else
        if ( const % ds % outStream % streamType .ne. dio_Nefis_stream ) then
            call DioStreamError(553, 'Consts only supported for Nefis, ', trim(const % ds % name))
        else
            if ( .not. const % ds % outStream % opened ) then
                call DioStreamError(554, 'File ', const % ds % outStream % name, ' not opened')
            else
                retVal = .true.
            endif
        endif
    endif

end function DioConstCanWrite


function DioConstCanRead(const) result(retVal)

    ! return value
    logical                         :: retVal ! success

    ! arguments
    type(DioConstType), intent(in) :: const ! dataset

    ! body
    if ( const % ds % inStream % synched ) then
        call DioStreamError(552, 'Consts not supported for synched streams, ', trim(const % ds % name))
    else
        if ( const % ds % inStream % streamType .ne. dio_Nefis_stream ) then
            call DioStreamError(553, 'Consts only supported for Nefis, ', trim(const % ds % name))
        else
            if ( .not. const % ds % inStream % opened ) then
                call DioStreamError(554, 'File ', const % ds % inStream % name, ' not opened')
            else
                retVal = .true.
            endif
        endif
    endif

end function DioConstCanRead


!*
!* PRIVATE Functions to Read/Write
!*

!
! Write header to stream
!
subroutine DioConstWriteHeader(const)

    ! arguments
    type(DioConstType), intent(in) :: const  ! dataset

    ! locals
    logical                        :: retVal ! result of write call

    ! body
    if ( DioConstCanWrite(const) ) then
        retVal = DioConstNefisWriteHeader(const)
        if (retVal) retVal = DioConstNefisWriteValue(const)
    endif

end subroutine DioConstWriteHeader


!
! Write values to stream
!
subroutine DioConstWriteValue(const)

    ! arguments
    type(DioConstType), intent(in) :: const ! dataset

    ! locals
    logical                        :: retVal ! result of write header call

    ! body
    if ( DioConstCanWrite(const) ) then
        retVal =  DioConstNefisWriteValue(const)
    endif

end subroutine DioConstWriteValue


!
! Read header from stream
!

function DioConstReadHeader(const) result(retVal)

    ! result value
    logical             :: retVal ! .true. : success

    ! arguments
    type(DioConstType), intent(in) :: const ! dataset

    ! body
    if ( DioConstCanRead(const) ) then
        retVal = DioConstNefisReadHeader(const)
    endif

end function DioConstReadHeader


function DioConstReadValue(const) result(retVal)

    ! result value
    logical             :: retVal ! .true. : success

    ! arguments
    type(DioConstType), intent(in) :: const ! dataset

    ! body
    if ( DioConstCanRead(const) ) then
        retVal = DioConstNefisReadValue(const)
    endif
end function DioConstReadValue


!******************************************************************************
!******************************************************************************
!* PUBLIC FUNCTIONS
!******************************************************************************
!******************************************************************************


!******************************************************************************
!* PUBLIC FUNCTIONS TO CREATE/DESTROY SCALAR
!******************************************************************************

!
! Create Const dataset
!
function DioConstCreate(name, varType) result(const)

    ! return value
    type(DioConstType)       :: const  ! dataset

    ! arguments
    character*(*), intent(in) :: name    ! dataset name
    integer                   :: varType ! type of variabe

    ! create the ds and the header
    const % ds       = DioDsCreate(name)
    const % varType  = varType

    const % intval   = 0
    const % realval  = 0.
    const % doubleval= 0.D+00
    const % logval   = .false.

end function DioConstCreate


!
! Destroy Const dataset
!

subroutine DioConstDestroy(const)

    ! arguments
    type(DioConstType)            :: const      ! dataset

    ! body
    call DioDsDestroy(const % ds)

end subroutine DioConstDestroy


!******************************************************************************
!* PUBLIC FUNCTIONS FOR SETTING/GETTING SCALAR DATA
!* - set real / double / int value
!******************************************************************************

!!
!! Set values into Const datasets
!!

subroutine DioConstSetValueReal(const, value)

    ! arguments
    type(DioConstType) :: const  ! dataset
    real, intent(in)    :: value   ! value to be stored

    ! body
    if ( const % varType == Dio_Var_Real ) then
        const % realVal  = value
    else
        call DioStreamError(551, 'Wrong var type for const dataset, ', trim(const % ds % name))
    endif

end subroutine DioConstSetValueReal


subroutine DioConstSetValueDouble(const, value)

    ! arguments
    type(DioConstType)          :: const  ! dataset
    double precision, intent(in) :: value   ! value to be stored

    ! body
    if ( const % varType == Dio_Var_Double ) then
        const % doubleVal  = value
    else
        call DioStreamError(551, 'Wrong var type for const dataset, ', trim(const % ds % name))
    endif

end subroutine DioConstSetValueDouble


subroutine DioConstSetValueInteger(const, value)

    ! arguments
    type(DioConstType) :: const  ! dataset
    integer, intent(in) :: value   ! value to be stored

    ! body
    if ( const % varType == Dio_Var_Integer ) then
        const % intVal  = value
    else
        call DioStreamError(551, 'Wrong var type for const dataset, ', trim(const % ds % name))
    endif

end subroutine DioConstSetValueInteger


!!
!! Get values from Const datasets
!!

function DioConstGetValueReal(const, value) result(retVal)

    ! return value
    logical                         :: retVal ! success

    ! arguments
    type(DioConstType), intent(in) :: const   ! dataset
    real, intent(out)               :: value    ! Real Value in dataset

    retVal = .false.
    ! body
    if ( const % varType == Dio_Var_Real ) then
        value = const % realVal
        retVal = .true.
    else
        call DioStreamError(551, 'Wrong var type for const dataset ', trim(const % ds % name))
    endif

end function DioConstGetValueReal


function DioConstGetValueDouble(const, value) result(retVal)

    ! return value
    logical                         :: retVal ! success

    ! arguments
    type(DioConstType), intent(in) :: const   ! dataset
    double precision, intent(out)   :: value    ! Double value in dataset

    ! body
    retVal = .false.
    if ( const % varType == Dio_Var_Double ) then
        value = const % doubleVal
        retVal = .true.
    else
        call DioStreamError(551, 'Wrong var type for const dataset ', trim(const % ds % name))
    endif

end function DioConstGetValueDouble


function DioConstGetValueInteger(const, value) result(retVal)

    ! return value
    logical                         :: retVal ! success

    ! arguments
    type(DioConstType), intent(in) :: const   ! dataset
    integer, intent(out)            :: value    ! int value in dataset

    ! body
    retVal = .false.
    if ( const % varType == Dio_Var_Integer ) then
        value = const % intVal
        retVal = .true.
    else
        call DioStreamError(551, 'Wrong var type for const dataset ', trim(const % ds % name))
    endif

end function DioConstGetValueInteger


!******************************************************************************
!* PUBLIC functions to define a Const on a stream / Get a Const from a stream
!******************************************************************************

!!
!! PUBLIC Interface functions
!!

!
! Define dataset
!
function DioConstDefine(stream, name, varType) result(const)

    ! return value
    type(DioConstType)                      :: const             ! dataset

    ! arguments
    type(DioStreamType), target, intent(in) :: stream         ! stream
    character*(*)              , intent(in) :: name           ! const name
    integer                    , intent(in) :: varType        ! type of var to be stored

    ! body
    const = DioConstCreate(name, varType)
    const % ds % outStream = stream
    call DioConstWriteHeader(const)

end function DioConstDefine


!******************************************************************************
!* PUBLIC functions to PUT (=write) / GET (=read) values
!* - floats / doubles / integers
!******************************************************************************

!*
!* Put functions
!*

subroutine DioConstPutReal(const, value)

    ! arguments
    type(DioConstType) :: const      ! dataset
    real, intent(in)    :: value

    ! body
    call DioConstSetValue(const, value)
    call DioConstWriteValue(const)

end subroutine DioConstPutReal


subroutine DioConstPutDouble(const, value)

    ! arguments
    type(DioConstType)          :: const      ! dataset
    double precision, intent(in) :: value

    ! body
    call DioConstSetValue(const, value)
    call DioConstWriteValue(const)

end subroutine DioConstPutDouble


subroutine DioConstPutInteger(const, value)

    ! arguments
    type(DioConstType) :: const      ! dataset
    integer, intent(in) :: value

    ! body
    call DioConstSetValue(const, value)
    call DioConstWriteValue(const)

end subroutine DioConstPutInteger


!*
!* Put functions
!*

function DioConstGetDataset(stream, name) result(const)

    ! return value
    type(DioConstType)                      :: const       ! dataset

    ! arguments
    type(DioStreamType), target, intent(in) :: stream  ! stream
    character*(*)              , intent(in) :: name    ! dataset name
    logical                                 :: retVal ! result of read call

    ! body
    const = DioConstCreate(name, Dio_Var_Unknown)
    const % ds % inStream = stream
    retVal = DioConstReadHeader(const)

end function DioConstGetDataset


function DioConstGetReal(const, value) result(retVal)

    ! result value
    logical             :: retVal ! .true. : success

    ! arguments
    type(DioConstType) :: const ! dataset
    real                :: value  ! resulting value

    ! body
    retVal = .false.
    if (DioConstReadValue(const)) then
        value = const % realVal
        retVal = .true.
    endif

end function DioConstGetReal


function DioConstGetDouble(const, value) result(retVal)

    ! result value
    logical             :: retVal ! .true. : success

    ! arguments
    type(DioConstType) :: const ! dataset
    double precision    :: value  ! resulting value

    ! body
    retVal = .false.
    if (DioConstReadValue(const)) then
        value = const % doubleVal
        retVal = .true.
    endif

end function DioConstGetDouble


function DioConstGetInteger(const, value) result(retVal)

    ! result value
    logical             :: retVal ! .true. : success

    ! arguments
    type(DioConstType) :: const ! dataset
    integer             :: value  ! resulting value

    ! body
    retVal = .false.
    if (DioConstReadValue(const)) then
        value = const % intVal
        retVal = .true.
    endif

end function DioConstGetInteger


!******************************************************************************
!******************************************************************************
!* PRIVATE FILE TYPE SPECIFIC FUNCTIONS
!******************************************************************************
!******************************************************************************


!******************************************************************************
!* PRIVATE FUNCTIONS FOR WRITING/READING NEFIS FORMAT
!******************************************************************************

!
! Write SCALAR Header to NEFIS file
!

function DioConstNefisWriteHeader(const) result(retVal)

    ! result value
    logical                              :: retVal      ! .true.: success

    ! arguments
    type(DioConstType), target           :: const       ! dataset

    ! externals
    integer, external :: defcel, defelm, defgrp, credat, &
                         putelt, putels, flsdef, flsdat

    ! locals
    type(DioStreamType), pointer         :: stream
    type(DioDsType), pointer             :: ds
    character(len = DioMaxElmNameLen), &
                            dimension(3) :: elms
    character(len = DioMaxElmNameLen), &
                            dimension(1) :: celNames
    character(len = DioMaxElmNameLen)    :: grpName
    integer                              :: grpNumDim
    integer, dimension(1)                :: grpDimens
    integer, dimension(1)                :: grpOrder
    integer                              :: ierror
    character(len=DioMaxElmNameLen)      :: varTypeName
    integer                              :: varTypeSize
    integer, dimension(3,1)              :: uindex

    ds     => const % ds
    stream => ds % outStream

    varTypeSize = 0
    select case ( const % varType )
        case (Dio_Var_Real)
            varTypeName = 'Real'
            varTypeSize = 4
        case (Dio_Var_Double)
            varTypeName = 'Real'
            varTypeSize = 8
        case (Dio_Var_Integer)
            varTypeName = 'Integer'
            varTypeSize = 4
        case (Dio_Var_Logical)
            varTypeName = 'Logical'
            varTypeSize = 4
    end select


    if ( varTypeSize .ne. 0 ) then

        elms(1) = trim(ds % name) // '_Name'
        elms(2) = trim(ds % name) // '_VarType'
        elms(3) = trim(ds % name) // '_Value'

        celNames(1) = trim(ds % name) // DSHeaderExtension
        grpName     = celNames(1)

        grpNumDim    = 1
        grpDimens(1) = 1
        grpOrder (1) = 1

        ierror  = DEFELM(stream % nefFileHandle , elms(1), &
                        'CHARACTER',DioMaxDsNameLen, &
                        ' ',' ', &
                        'Dataset name',0, 0)
        if ( ierror .ne. 0 ) goto 999

        ierror  = DEFELM(stream % nefFileHandle , elms(2), &
                        'INTEGER',4, &
                        ' ',' ', &
                        'Variable Type',0, 0)
        if ( ierror .ne. 0 ) goto 999

        ierror  = DEFELM(stream % nefFileHandle , elms(3), &
                        varTypeName, varTypeSize, &
                        ' ',' ', &
                        'Value',0, 0)
        if ( ierror .ne. 0 ) goto 999

        ierror  = DEFCEL(stream % nefFileHandle,celNames,size(elms),elms)
        if ( ierror .ne. 0 ) goto 999

        ierror  = DEFGRP(stream % nefFileHandle, &
                                grpName, celNames, &
                                grpNumDim, grpDimens, grpOrder)
        if ( ierror .ne. 0 ) goto 999

        ierror  = CREDAT(stream % nefFileHandle, &
                                grpName, grpName)
        if ( ierror .ne. 0 ) goto 999

        uindex(1,1) = 1
        uindex(2,1) = 1
        uindex(3,1) = 1

        ierror  = PUTELS(stream % nefFileHandle, &
                                grpName, elms(1), uindex, 1, ds % name)
        if ( ierror .ne. 0 ) goto 999

        ierror  = PUTELT(stream % nefFileHandle, &
                                grpName, elms(2), uindex, 1, const % varType)
        if ( ierror .ne. 0 ) goto 999

        ierror  = FLSDEF(stream % nefFileHandle)
        if ( ierror .ne. 0 ) goto 999

        retVal = .true.

    endif

    return

    ! Error Handling: one of the functions returned an error
999 continue
    call DioStreamError(561, trim(ds % name),' could not be defined in Nefisfile ',&
                                         trim(ds % outStream % name) )

end function DioConstNefisWriteHeader


function DioConstNefisWriteValue(const) result(retVal)

    ! result value
    logical                     :: retVal ! .true. : success

    ! arguments
    type(DioConstType), target :: const ! dataset

    ! externals
    integer, external :: putelt, flsdat

    ! locals
    type(DioStreamType), pointer                    :: stream
    character(len = DioMaxElmNameLen), dimension(1) :: elms
    character(len = DioMaxElmNameLen)               :: grpName
    integer                                         :: ierror
    integer                                         :: uindex(3,1)

    ! body

    retVal = .false.

    stream => const % ds % outStream

    grpName = trim(const % ds % name) // DSHeaderExtension
    elms(1) = trim(const % ds % name) // '_Value'

    uindex(1,1) = 1
    uindex(2,1) = 1
    uindex(3,1) = 1

    select case ( const % varType )
        case (Dio_Var_Real)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, const % realVal)
        case (Dio_Var_Double)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, const % doubleVal)
        case (Dio_Var_Integer)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, const % intVal)
        case (Dio_Var_Logical)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, const % logVal)
    end select

    if ( ierror .ne. 0 ) then
        call DioStreamError(562, trim(const % ds % name),&
                         ': Value could not be put to Nefisfile ',&
                         trim(const % ds % outStream % name) )
    else
        ierror  = FLSDAT(stream % nefFileHandle)
        retVal = .true.
    endif

end function DioConstNefisWriteValue


function DioConstNefisReadHeader(const) result(retVal)

    ! result value
    logical                             :: retVal ! .true.: success

    ! arguments
    type(DioConstType), target          :: const ! dataset

    ! externals
    integer, external :: getelt, getels

    ! locals
    type(DioStreamType), pointer        :: stream
    type(DioDsType), pointer            :: ds
    integer                             :: ierror
    integer, dimension(3,1)             :: uindex
    character(len = DioMaxElmNameLen), &
                           dimension(3) :: elms
    character(DioMaxElmNameLen)         :: grpName
    integer                             :: varTypeSize = 4

    ! body

    retVal = .false.

    ds     => const % ds
    stream => ds % inStream

    uindex(1,1) = 1
    uindex(2,1) = 1
    uindex(3,1) = 1

    elms(1) = trim(ds % name) // '_Name'
    elms(2) = trim(ds % name) // '_VarType'

    grpName = trim(const % ds % name) // DSHeaderExtension

    ierror = getels(stream % nefFileHandle, &
                        grpName, elms(1), uindex, 1, DioMaxDsNameLen, &
                        ds % name)

    if ( ierror .eq. 0 ) then
        ierror = getelt(stream % nefFileHandle, &
                            grpName, elms(2), uindex, 1, varTypeSize, &
                            const% varType)
    endif

    if ( ierror .ne. 0 ) then
        call DioStreamError(563, trim(const % ds % name),&
                         ': Header could not be read from Nefisfile ',&
                         trim(stream % name) )
    else
        retVal = .true.
    endif

end function DioConstNefisReadHeader


function DioConstNefisReadValue(const) result(retVal)

    ! result value
    logical                     :: retVal ! .true. : success

    ! arguments
    type(DioConstType), target :: const ! dataset

    ! externals
    integer, external :: getelt

    ! locals
    type(DioStreamType), pointer                    :: stream
    character(len = DioMaxElmNameLen), dimension(1) :: elms
    character(len = DioMaxElmNameLen)               :: grpName
    integer                                         :: ierror
    integer                                         :: uindex(3,1)

    ! body

    retVal = .false.

    stream => const % ds % inStream

    grpName = trim(const % ds % name) // DSHeaderExtension
    elms(1) = trim(const % ds % name) // '_Value'

    uindex(1,1) = 1
    uindex(2,1) = 1
    uindex(3,1) = 1

    select case ( const % varType )
        case (Dio_Var_Real)
            ierror  = GETELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, 4, const % realVal)
        case (Dio_Var_Double)
            ierror  = GETELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, 8, const % doubleVal)
        case (Dio_Var_Integer)
            ierror  = GETELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, 4, const % intVal)
        case (Dio_Var_Logical)
            ierror  = GETELT(stream % nefFileHandle, &
                                     grpName, elms(1), uindex, 1, 4, const % logVal)
    end select

    if ( ierror .ne. 0 ) then
        call DioStreamError(564, trim(const % ds % name),&
                         ': Value could not be read from Nefisfile ',&
                         trim(const % ds % inStream % name) )
    else
        retVal = .true.
    endif

end function DioConstNefisReadValue



end module Dio_const_rw
