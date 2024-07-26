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
!!! Dio-2DF-Nefis: Nefis I/O for 2d-field datasets
!!!
!!! (c) Deltares, july 2001
!!!
!!! David.Levelt@deltares.nl / Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


function Dio2DFWriteHeaderNefis(f2D) result(retVal)

    use dio_2dfield_rw

    implicit none

    ! return value
    logical :: retVal

    ! arguments
    type(Dio2DFType), target, intent(IN) :: f2D

    ! externals
    integer, external :: defcel, defelm, defgrp, credat, &
                         putelt, putels, flsdef
    logical, external :: Dio3DDefineValuesNefis

    ! locals

    type(DioStreamType), pointer         :: stream
    type(Dio2DFHeaderType), pointer      :: header
    type(DioDsType), pointer             :: ds

    character(len = DioMaxElmNameLen), &
                            dimension(5) :: elms
    character(len = DioMaxElmNameLen), &
                            dimension(1) :: celNames
    character(len = DioMaxElmNameLen)    :: grpName
    integer                              :: grpNumDim
    integer, dimension(1)                :: grpDimens
    integer, dimension(1)                :: grpOrder
    integer, dimension(3,1)              :: uindex
    integer                              :: ierror
    integer                              :: nTimes = 0
    character(DioNefErrMsgLen)           :: errorString

    !-----------------------------------------------------------------------
    !-----Initialization
    !-----------------------------------------------------------------------


    retVal = .false.
    errorString = ', Nefis Error(s): '

    header => f2D % header
    ds     => f2D % ds
    stream => ds % outStream

    if ( stream % opened ) then

        retVal = .true.

        elms(1) = trim(ds % name) // '_Name'
        elms(2) = trim(ds % name) // '_VarType'
        elms(3) = trim(ds % name) // '_numM'
        elms(4) = trim(ds % name) // '_numN'
        elms(5) = trim(ds % name) // '_NTimes'

        celNames(1) = trim(ds % name) // DSHeaderExtension
        grpName     = celNames(1)

        grpNumDim    = 1
        grpDimens(1) = 1
        grpOrder (1) = 1

        ierror  = DEFELM(stream % nefFileHandle , elms(1), &
                        'CHARACTER',DioMaxDsNameLen, &
                        ' ',' ', &
                        'Name of dataset',0, 0)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = DEFELM(stream % nefFileHandle , elms(2), &
                        'INTEGER',4, &
                        ' ',' ', &
                        'VarType of dataset',0, 0)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = DEFELM(stream % nefFileHandle , elms(3), &
                        'INTEGER',4, &
                        ' ',' ', &
                        'Number of Parameters of dataset',0, 0)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = DEFELM(stream % nefFileHandle , elms(4), &
                        'INTEGER',4, &
                        ' ',' ', &
                        'Number of Locations of dataset',0, 0)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = DEFELM(stream % nefFileHandle , elms(5), &
                        'INTEGER',4, &
                        ' ',' ', &
                        'Number of Timesteps of dataset',0, 0)
        call checkNefisError(retVal, ierror, errorString)


        ierror  = DEFCEL(stream % nefFileHandle,celNames,size(elms),elms)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = DEFGRP(stream % nefFileHandle, &
                                grpName, celNames, grpNumDim, grpDimens, grpOrder)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = CREDAT(stream % nefFileHandle, grpName, grpName)
        call checkNefisError(retVal, ierror, errorString)


        uindex(1,1) = 1
        uindex(2,1) = 1
        uindex(3,1) = 1

        ierror  = PUTELS(stream % nefFileHandle, &
                                grpName, elms(1), uindex, 1, ds % name)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = PUTELT(stream % nefFileHandle, &
                                grpName, elms(2), uindex, 1, header % varType)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = PUTELT(stream % nefFileHandle, &
                                grpName, elms(3), uindex, 1, header % numM)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = PUTELT(stream % nefFileHandle, &
                                grpName, elms(4), uindex, 1, header % numN)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = PUTELT(stream % nefFileHandle, &
                                grpName, elms(5), uindex, 1, nTimes)
        call checkNefisError(retVal, ierror, errorString)

        ierror  = FLSDEF(stream % nefFileHandle)
        call checkNefisError(retVal, ierror, errorString)

        if ( retVal ) then
            retval = Dio3DDefineValuesNefis(ds, header % numM, header % numN, header % varType)
        endif

        if ( .not. retVal ) then
           call DioStreamError(391, 'Error writing 2DF Nefis Header', ds % name, errorString)
        endif

    endif

end function Dio2DFWriteHeaderNefis


function Dio2DFReadHeaderNefis(f2D) result(retVal)

    use dio_2dfield_rw

    implicit none

    ! return value
    logical :: retVal

    ! arguments
    type(Dio2DFType), target, intent(IN)            :: f2D

    ! externals
    integer, external :: getels, getelt

    ! locals
    type(DioStreamType), pointer                    :: stream
    type(Dio2DFHeaderType), pointer                 :: header
    type(DioDsType), pointer                        :: ds
    integer                                         :: ierror
    integer, dimension(3,1)                         :: uindex
    character(len = DioMaxElmNameLen),dimension(5)  :: elms
    integer, dimension(5)                           :: elmsNBytes
    character(DioMaxElmNameLen)                     :: grpName
    character(DioNefErrMsgLen)                      :: errorString

    ! body

    retVal = .false.
    errorString = ', Nefis Error(s): '

    header => f2D % header
    ds     => f2D % ds
    stream => ds % inStream

    if ( stream % opened ) then

        retVal = .true.

        uindex(1,1) = 1
        uindex(2,1) = 1
        uindex(3,1) = 1

        elms(1) = trim(ds % name) // '_Name'
        elms(2) = trim(ds % name) // '_VarType'
        elms(3) = trim(ds % name) // '_numM'
        elms(4) = trim(ds % name) // '_numN'
        elms(5) = trim(ds % name) // '_NTimes'

        elmsNBytes(1) = DioMaxDsNameLen
        elmsNBytes(2) = 4
        elmsNBytes(3) = 4
        elmsNBytes(4) = 4
        elmsNBytes(5) = 4

        grpName = trim(ds % name) // DSHeaderExtension

        ierror = getels(stream % nefFileHandle, &
                            grpName, elms(1), uindex, 1, elmsNbytes(1), &
                            ds % name)
        call checkNefisError(retVal, ierror, errorString)

        ierror = getelt(stream % nefFileHandle, &
                            grpName, elms(2), uindex, 1, elmsNbytes(2), &
                            header % varType)
        call checkNefisError(retVal, ierror, errorString)

        ierror = getelt(stream % nefFileHandle, &
                            grpName, elms(3), uindex, 1, elmsNbytes(3), &
                            header % numM)
        call checkNefisError(retVal, ierror, errorString)

        ierror = getelt(stream % nefFileHandle, &
                            grpName, elms(4), uindex, 1, elmsNbytes(4), &
                            header % numN)
        call checkNefisError(retVal, ierror, errorString)

        ierror = getelt(stream % nefFileHandle, &
                            grpName, elms(5), uindex, 1, elmsNbytes(5), &
                            ds % nTimes)
        call checkNefisError(retVal, ierror, errorString)

        if ( .not. retVal ) then
            call DioStreamError(392, 'Error reading 2DF Nefis Header', ds % name, errorString)
        endif
    endif

end function Dio2DFReadHeaderNefis


function Dio2DFNefisReadAllTimes(f2D) result(retVal)

    use dio_2dfield_rw

    ! return value
    logical :: retVal

    ! arguments
    type(Dio2DFType), target, intent(IN) :: f2D

    ! externals
    integer, external :: getelt, getels

    ! locals
    type(DioStreamType), pointer      :: stream
    type(DioDsType), pointer          :: ds
    integer                           :: ierror
    integer, dimension(3,1)           :: uindex
    character(len = DioMaxElmNameLen) :: elmName
    integer                           :: nElmNBytes
    character(DioMaxElmNameLen)       :: grpName
    character(DioNefErrMsgLen)        :: errorString

    ! body

    retVal = .false.
    errorString = ', Nefis Error(s): '

    ds     => f2D % ds
    stream => ds % inStream

    if ( stream % opened ) then

        retVal = .true.

        uindex(1,1) = 1
        uindex(2,1) = 1
        uindex(3,1) = 1

        grpName = trim(ds % name) // DSHeaderExtension
        elmName = trim(ds % name) // '_NTimes'
        nElmNBytes = 4
        ierror = getelt(stream % nefFileHandle, &
                        grpName, elmName, uindex, 1, nElmNBytes, &
                        ds % nTimes)
        if ( ierror .ne. 0 ) then
            call DioStreamError(393, 'Error reading #times for dataset ', ds % name)
            retVal = .false.
        else
            grpName = trim(ds % name) // DSDataExtension
            elmName = trim(ds % name) // '_NTimes'
            nElmNBytes = 4
            ierror = getelt(stream % nefFileHandle, &
                            grpName, elmName, uindex, 1, nElmNBytes, &
                            ds % nTimes)
            if ( ierror .ne. 0 ) then
                call DioStreamError(393, 'Error reading #times for dataset ', ds % name)
                retVal = .false.
            endif
        endif
    endif

end function Dio2DFNefisReadAllTimes


