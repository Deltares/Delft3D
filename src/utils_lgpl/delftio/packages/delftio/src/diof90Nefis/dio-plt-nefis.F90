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
!!! Dio-PLT-Nefis: Nefis I/O for Param./Loc./Time datasets
!!!
!!! (c) Deltares, dec 2000
!!!
!!! David.Levelt@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


function DioPltWriteHeaderNefis(plt) result(retVal)

    use dio_plt_rw
    implicit none

    ! return value
    logical :: retVal

    ! arguments
    type(DioPltType), target, intent(IN)            :: plt

    ! externals
    integer, external :: defcel, defelm, defgrp, credat, &
                         putelt, putels, flsdef
    logical, external :: Dio3DDefineValuesNefis

    ! locals

    type(DioStreamType), pointer                    :: stream
    type(DioPltHeaderType), pointer                 :: header
    type(DioDsType), pointer                        :: ds

    character(len = DioMaxElmNameLen), dimension(7) :: elms        
    character(len = DioMaxElmNameLen), dimension(1) :: celNames
    character(len = DioMaxElmNameLen)               :: grpName
    integer                                         :: grpNumDim
    integer, dimension(1)                           :: grpDimens
    integer, dimension(1)                           :: grpOrder 
    integer, dimension(3,1)                         :: uindex
    integer                                         :: ierror
    integer                                         :: nTimes = 0
    character(DioNefErrMsgLen)                      :: errorString

    !-----------------------------------------------------------------------
    !-----Initialization
    !-----------------------------------------------------------------------


    retVal = .true.
    errorString = ', Nefis Error(s): '

    header => plt % header
    ds     => plt % ds
    stream => ds % outStream

    write(*,*) 'pars:', header % pars
    write(*,*) 'locs:', header % locs
    elms(1) = trim(ds % name) // '_Name'
    elms(2) = trim(ds % name) // '_VarType'
    elms(3) = trim(ds % name) // '_NPar'
    elms(4) = trim(ds % name) // '_NLoc'
    elms(5) = trim(ds % name) // '_NTimes'
    elms(6) = trim(ds % name) // '_Pars'
    elms(7) = trim(ds % name) // '_Locs'

    ierror  = DEFELM(stream % nefFileHandle , elms(1), &
                    'CHARACTER',DioMaxDsNameLen, &
                    ' ',' ', &
                    'Name of dataset',0, 0)
    if ( ierror .ne. 0 ) retVal = .false.

    ierror  = DEFELM(stream % nefFileHandle , elms(2), &
                    'INTEGER',4, &
                    ' ',' ', &
                    'VarType of dataset',0, 0)
    if ( ierror .ne. 0 ) retVal = .false.

    ierror  = DEFELM(stream % nefFileHandle , elms(3), &
                    'INTEGER',4, &
                    ' ',' ', &
                    'Number of Parameters of dataset',0, 0)
    if ( ierror .ne. 0 ) retVal = .false.

    ierror  = DEFELM(stream % nefFileHandle , elms(4), &
                    'INTEGER',4, &
                    ' ',' ', &
                    'Number of Locations of dataset',0, 0)
    if ( ierror .ne. 0 ) retVal = .false.

    ierror  = DEFELM(stream % nefFileHandle , elms(5), &
                    'INTEGER',4, &
                    ' ',' ', &
                    'Number of Timesteps of dataset',0, 0)
    if ( ierror .ne. 0 ) retVal = .false.

    ierror  = DEFELM(stream % nefFileHandle , elms(6), &
                    'CHARACTER',DioMaxParLen, &
                    ' ',' ', &
                    'Parameter Names in dataset',1, header % nPar)
    if ( ierror .ne. 0 ) retVal = .false.

    ierror  = DEFELM(stream % nefFileHandle , elms(7), &
                    'CHARACTER',DioMaxLocLen, &
                    ' ',' ', &
                    'Location Names in dataset',1, header % nLoc)
    if ( ierror .ne. 0 ) retVal = .false.

    celNames(1) = trim(ds % name) // DSHeaderExtension
    grpName     = celNames(1)

    grpNumDim    = 1
    grpDimens(1) = 1
    grpOrder (1) = 1

    ierror  = DEFCEL(stream % nefFileHandle,celNames,size(elms),elms)
    if ( ierror .ne. 0 ) retVal = .false.


    ierror  = DEFGRP(stream % nefFileHandle, &
                            grpName, celNames, grpNumDim, grpDimens, grpOrder)
    if ( ierror .ne. 0 ) retVal = .false.

    ierror  = CREDAT(stream % nefFileHandle, &
                            grpName, grpName)
    if ( ierror .ne. 0 ) retVal = .false.

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
                            grpName, elms(3), uindex, 1, header % nPar)
    call checkNefisError(retVal, ierror, errorString)

    ierror  = PUTELT(stream % nefFileHandle, &
                            grpName, elms(4), uindex, 1, header % nLoc)
    call checkNefisError(retVal, ierror, errorString)

    ierror  = PUTELT(stream % nefFileHandle, &
                            grpName, elms(5), uindex, 1, nTimes)
    call checkNefisError(retVal, ierror, errorString)

    ierror  = PUTELS(stream % nefFileHandle, &
                            grpName, elms(6), uindex, 1, header % pars)
    call checkNefisError(retVal, ierror, errorString)

    ierror  = PUTELS(stream % nefFileHandle, &
                            grpName, elms(7), uindex, 1, header % locs)
    call checkNefisError(retVal, ierror, errorString)

    ierror  = FLSDEF(stream % nefFileHandle)
    call checkNefisError(retVal, ierror, errorString)

    if ( .not. retVal ) then
        call DioStreamError(291, 'Error reading PLT Nefis Header', ds % name, errorString)
    else
        retVal = Dio3DDefineValuesNefis(ds, header % nPar, header % nLoc, header % varType)
    endif
    return

end function DioPltWriteHeaderNefis


function DioPltReadHeaderNefis(plt) result(retVal)

    use dio_plt_rw
    implicit none

    ! return value
    logical :: retVal

    ! arguments

    type(DioPltType), target, intent(IN)            :: plt

    ! externals
    integer, external :: getelt, getels

    ! locals

    type(DioStreamType), pointer                    :: stream
    type(DioPltHeaderType), pointer                 :: header
    type(DioDsType), pointer                        :: ds
    integer                                         :: ierror
    integer                                         :: uindex(3,1)
    character(len = DioMaxElmNameLen),dimension(7)  :: elms
    integer, dimension(5)                           :: elmsNBytes
    character(DioMaxElmNameLen)                     :: grpName
    character(DioNefErrMsgLen)                      :: errorString

    ! body

    retVal = .true.
    errorString = ', Nefis Error(s): '

    header => plt % header
    ds     => plt % ds
    stream => ds % inStream

    uindex(1,1) = 1
    uindex(2,1) = 1
    uindex(3,1) = 1

    elms(1) = trim(ds % name) // '_Name'
    elms(2) = trim(ds % name) // '_VarType'
    elms(3) = trim(ds % name) // '_NPar'
    elms(4) = trim(ds % name) // '_NLoc'
    elms(5) = trim(ds % name) // '_NTimes'
    elms(6) = trim(ds % name) // '_Pars'
    elms(7) = trim(ds % name) // '_Locs'

    elmsNBytes(1) = DioMaxDsNameLen
    elmsNBytes(2) = 4
    elmsNBytes(3) = 4
    elmsNBytes(4) = 4
    elmsNBytes(5) = 4

    grpName = trim(ds % name) // DSHeaderExtension

    ierror = getels(stream % nefFileHandle, &
                        grpName, elms(1), uindex, 1, elmsNbytes(1), &
                        ds % name)

    ierror = getelt(stream % nefFileHandle, &
                        grpName, elms(2), uindex, 1, elmsNbytes(2), &
                        header % varType)

    ierror = getelt(stream % nefFileHandle, &
                        grpName, elms(3), uindex, 1, elmsNbytes(3), &
                        header % nPar)

    ierror = getelt(stream % nefFileHandle, &
                        grpName, elms(4), uindex, 1, elmsNbytes(4), &
                        header % nLoc)

    ierror = getelt(stream % nefFileHandle, &
                        grpName, elms(5), uindex, 1, elmsNbytes(5), &
                        ds % nTimes)


    if ( header % nPar .gt. 0 ) then
        allocate(header % pars(header % nPar))
        ierror = getels(stream % nefFileHandle, &
                        grpName, elms(6), uindex, 1, &
                        size(header % pars)*DioMaxParLen, header % pars)
    endif

    if ( header % nLoc .gt. 0 ) then
        allocate(header % locs(header % nLoc))
        ierror = getels(stream % nefFileHandle, &
                        grpName, elms(7), uindex, 1, &
                        size(header % locs)*DioMaxLocLen, header % locs)
    endif
    call checkNefisError(retVal, ierror, errorString)

    if ( .not. retVal ) then
       call DioStreamError(292, 'Error reading PLT Nefis Header', ds % name, errorString)
    endif

end function DioPltReadHeaderNefis


