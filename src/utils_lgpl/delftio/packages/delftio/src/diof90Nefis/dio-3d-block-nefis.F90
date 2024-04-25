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
!!! Dio-3D-Block-Nefis: Nefis I/O for 3D-value blocks
!!!
!!! (c) Deltares, okt 2002
!!!
!!! David.Levelt@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


function Dio3DDefineValuesNefis(ds, numM, numN, varType) result(retVal)

    use dio_ds
    use dio_3d_block

    implicit none

    ! result value
    logical :: retVal    ! .true. : success

    include 'dio-time-support.inc'

    ! arguments

    type(DioDsType), target, intent(IN)  :: ds
    integer, intent(IN)                  :: numM, numN
    integer, intent(IN)                  :: varType

    ! externals
    integer, external :: defcel, defelm, defgrp, credat, &
                         putelt, putels, flsdef, flsdat

    ! locals 

    type(DioStreamType), pointer         :: stream
    integer, dimension(2)                :: elmDims
    integer                              :: numElmDims
    character(len=DioMaxElmNameLen)      :: dataGrpName
    character(len=DioMaxElmNameLen)      :: varTypeName
    integer                              :: varTypeSize
    character(len = DioMaxElmNameLen),&
        dimension(2)                     :: elms        
    character(len = DioMaxElmNameLen),&
        dimension(1)                     :: celNames
    
    integer                              :: grpNumDim
    integer, dimension(1)                :: grpDimens
    integer, dimension(1)                :: grpOrder 
    integer                              :: ierror
    character(DioNefErrMsgLen)           :: errorString

    !-----------------------------------------------------------------------
    !-----Initialization
    !-----------------------------------------------------------------------

    retVal = .true.
    errorString = ', Nefis Error(s): '

    stream => ds % outStream

    elms(1) = trim(ds % name) // '_Value'
    elms(2) = trim(ds % name) // '_Time'

    elmDims(1) = numM
    elmDims(2) = numN
    numElmDims    = 2

    varTypeSize = 0
    select case ( varType )
        case (Dio_PLT_Real)
            varTypeName = "Real"
            varTypeSize = 4
        case (Dio_PLT_Double)
            varTypeName = "Real"
            varTypeSize = 8
        case (Dio_PLT_Logical)
            varTypeName = "Logical"
            varTypeSize = 4
        case (Dio_PLT_Integer)
            varTypeName = "Integer"
            varTypeSize = 4
    end select

    if ( varTypeSize .ne. 0 ) then
        ierror  = DEFELM(stream % nefFileHandle , elms(1), &
                        varTypeName, varTypeSize, &
                        '-', '-', '-',numElmDims, elmDims)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = DEFELM(stream % nefFileHandle , elms(2), &
                        'REAL',8, 'Date/Time','Julian', &
                        'Time Stamp', 0, 0)
        call checkNefisError(retVal, ierror, errorString)


        celNames(1) = trim(ds % name) // DSDataExtension

        ierror  = DEFCEL(stream % nefFileHandle,celNames(1),size(elms),elms)
        call checkNefisError(retVal, ierror, errorString)

        dataGrpName = trim(ds % name) // DSDataExtension

        grpNumDim    = 1
        grpDimens(1) = 0 ! variabele dimensie 
        grpOrder (1) = 1

        ierror  = DEFGRP(stream % nefFileHandle, &
                                dataGrpName, celNames, &
                                grpNumDim, grpDimens, grpOrder)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = CREDAT(stream % nefFileHandle, &
                                dataGrpName,dataGrpName)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = FLSDEF(stream % nefFileHandle)
        call checkNefisError(retVal, ierror, errorString)
        ierror  = FLSDAT(stream % nefFileHandle)
        call checkNefisError(retVal, ierror, errorString)

        if ( .not. retVal ) then
           call DioStreamError(591, 'Error defining 3D-block Nefis Values', errorString)
        endif

    endif

end function Dio3DDefineValuesNefis


function Dio3DWriteValuesNefis(ds, numM, numN, values) result(retVal)

    use dio_ds
    use dio_3d_block

    implicit none

    include 'dio-time-support.inc'

    ! result value
    logical               :: retVal    ! .true. : success

    ! arguments
    type(DioDsType), target, intent(IN)          :: ds
    integer                                      :: numM, numN
    type(Dio3DType)                              :: values      ! dataset values

    ! externals
    integer, external :: putelt, flsdat

    ! locals 
    type(DioStreamType), pointer                 :: stream
    integer, dimension(2)                        :: elmDims
    integer                                      :: numElmDims
    integer, dimension(3,1)                      :: uindex
    character(len=DioMaxElmNameLen)              :: dataGrpName, infoGrpName
    character(len=DioMaxElmNameLen),dimension(2) :: elms        
    integer                                      :: ierror
    character(DioNefErrMsgLen)                   :: errorString

    !-----------------------------------------------------------------------
    !-----Initialization
    !-----------------------------------------------------------------------

    retVal = .true.
    errorString = ', Nefis Error(s): '

    stream => ds % outStream

    elms(1) = trim(ds % name) // '_Value'
    elms(2) = trim(ds % name) // '_Time'

    elmDims(1) = numM
    elmDims(2) = numN
    numElmDims = 2

    dataGrpName = trim(ds % name) // DSDataExtension
    infoGrpName = trim(ds % name) // DSHeaderExtension

    ! write values for current timeStep

    uindex(1,1) = ds % curPutGetCount
    uindex(2,1) = ds % curPutGetCount
    uindex(3,1) = 1

    select case ( values % varType )
        case (Dio_PLT_Real)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     dataGrpName, elms(1), uindex, 1, values % reals(:,:,ds % curDataIndex))
            call checkNefisError(retVal, ierror, errorString)
        case (Dio_PLT_Double)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     dataGrpName, elms(1), uindex, 1, values % doubles(:,:,ds % curDataIndex))
            call checkNefisError(retVal, ierror, errorString)
        case (Dio_PLT_Logical)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     dataGrpName, elms(1), uindex, 1, values % logs(:,:,ds % curDataIndex))
            call checkNefisError(retVal, ierror, errorString)
        case (Dio_PLT_Integer)
            ierror  = PUTELT(stream % nefFileHandle, &
                                     dataGrpName, elms(1), uindex, 1, values % ints(:,:,ds % curDataIndex))
            call checkNefisError(retVal, ierror, errorString)
    end select

    ierror  = PUTELT(stream % nefFileHandle, & 
                            dataGrpName, elms(2), uindex, 1, ds % timeStep ( ds % curTimeIndex ))
    call checkNefisError(retVal, ierror, errorString)

    uindex(1,1) = 1
    uindex(2,1) = 1
    uindex(3,1) = 1

    ierror  = PUTELT(stream % nefFileHandle, &
                            infoGrpName, trim(ds % name) // '_NTimes', uindex, 1, ds % curPutGetCount)
    call checkNefisError(retVal, ierror, errorString)

    ierror  = FLSDAT(stream % nefFileHandle)
    call checkNefisError(retVal, ierror, errorString)

    if ( .not. retVal ) then
        call DioStreamError(592, 'Error writing 3D-block Nefis Values', errorString)
    endif

end function Dio3DWriteValuesNefis


function Dio3DReadValuesNefis(ds, numM, numN, values) result(retVal)

    use dio_ds
    use dio_3d_block

    implicit none

    ! return value
    logical                             :: retval      ! .true. : succes

    ! arguments
    type(DioDsType), target             :: ds          ! dsInfo part
    integer                             :: numM, numN  ! dataset sizes
    type(Dio3DType)                     :: values      ! dataset values

    ! externals
    integer, external :: getelt

    ! locals

    type(DioStreamType), pointer        :: stream      ! nefis stream handle
    integer                             :: ierror      ! result of nefis call

    character(DioMaxDsNameLen)          :: infoGrpName ! name of info group
    character(DioMaxDsNameLen)          :: dataGrpName ! name of data group
    character(len = DioMaxDsNameLen), &
                         dimension(2)   :: elms        ! elements in group
    integer, dimension(2)               :: elmsNBytes  ! #bytes in elements
    integer, dimension(3,1)             :: uindex      ! step selection

    ! body

    retVal = .false.

    stream => ds % inStream
    
    elms(1) = trim(ds % name) // '_Value'
    elms(2) = trim(ds % name) // '_Time'

    elmsNBytes(1) = numM * numN * 4
    if ( values % varType .eq. Dio_Plt_Double ) elmsNBytes(1) = elmsNBytes(1) * 2
    elmsNBytes(2) = 8
 
    dataGrpName = trim(ds % name) // DSDataExtension
    infoGrpName = trim(ds % name) // DSHeaderExtension

    uindex(1,1) = 1
    uindex(2,1) = 1
    uindex(3,1) = 1

    ierror  = getelt(stream % nefFileHandle, &
                            infoGrpName, trim(ds % name) // '_NTimes', uindex, 1, 4, &
                            ds % nTimes)

    if ( ierror .eq. 0 .and. ds % nTimes >= ds % curPutGetCount) then

        uindex(1,1) = ds % curPutGetCount
        uindex(2,1) = ds % curPutGetCount
        uindex(3,1) = 1

        ierror = getelt(stream % nefFileHandle, &
                            dataGrpName, elms(2), uindex, 1, elmsNbytes(2), &
                            ds % timeStep ( ds % curTimeIndex ))

        if ( ierror .eq. 0 ) then

            select case ( values % varType )

                case (Dio_PLT_Real)
                    ierror = getelt(stream % nefFileHandle, &
                                            dataGrpName, elms(1), uindex, 1, elmsNbytes(1), &
                                            values % reals(:,:,ds % curDataIndex) )
                case (Dio_PLT_Double)
                    ierror = getelt(stream % nefFileHandle, &
                                            dataGrpName, elms(1), uindex, 1, elmsNbytes(1), &
                                            values % doubles(:,:,ds % curDataIndex) )
                case (Dio_PLT_Logical)
                    ierror = getelt(stream % nefFileHandle, &
                                            dataGrpName, elms(1), uindex, 1, elmsNbytes(1), &
                                            values % logs(:,:,ds % curDataIndex) )
                case (Dio_PLT_Integer)
                    ierror = getelt(stream % nefFileHandle, &
                                            dataGrpName, elms(1), uindex, 1, elmsNbytes(1), &
                                            values % ints(:,:,ds % curDataIndex) )
            end select

            if ( ierror .eq. 0 ) then
                retVal = .true.
            else
                call DioStreamError(501, 'Could not get: ', trim(elms(1)), &
                                    'from nefis file', stream % name )
            endif

        else
            call DioStreamError(502, 'Could not get: ', trim(elms(2)), &
                                    'from nefis file', stream % name )
        endif

    else
        call DioStreamError(503, ds % name, ': Could not get nTimes from nefis file', stream % name )
    endif

end function Dio3DReadValuesNefis

