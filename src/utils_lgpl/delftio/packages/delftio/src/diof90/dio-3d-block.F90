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
!!! Dio-3d-block: 2D+Time (=3d) block functions
!!!
!!! (c) Deltares, jun 2001
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module Dio_3d_block

use dio_shm

!!
!! Types of var.s that can be stored in a 3d blck
!!

integer, parameter :: Dio_Plt_Unknown  = 0
integer, parameter :: Dio_Plt_Integer  = 1
integer, parameter :: Dio_Plt_Real     = 2
integer, parameter :: Dio_Plt_Double   = 3
integer, parameter :: Dio_Plt_Logical  = 4

integer, parameter :: Dio3dBlockAllocSize = 16

!
! 3D data blck values
!
type Dio3DType

    integer                         :: varType     ! int, real,
                                                   ! double, logical
    integer                         :: numM        ! first dim
    integer                         :: numN        ! last dim

    real, pointer, &
                   dimension(:,:,:) :: reals ! real*4 values
    double precision, pointer, &
                   dimension(:,:,:) :: doubles ! real*8 values
    integer, pointer, &
                   dimension(:,:,:) :: ints    ! integer values
    logical, pointer, &
                   dimension(:,:,:) :: logs  ! logical values


end type Dio3DType


interface Dio3DCreate
    module procedure Dio3DCreate
    module procedure Dio3DCreateEmpty
end interface


contains

!
! Allocate space for dataset nPar*nLoc * 'timeSize' values
!
subroutine Dio3DAllocateValues(blck, dataSize)
    ! arguments
    type(Dio3DType)        :: blck     ! 3d data blck
    integer                :: dataSize ! 3th dimension
    ! locals
    integer                :: varType, numM, numN
    integer                :: oldSize, newSize
    real, pointer, &
        dimension(:,:,:)   :: newReals ! real*4 values
    double precision, pointer, &
        dimension(:,:,:)   :: newDoubles ! real*8 values
    integer, pointer, &
        dimension(:,:,:)   :: newInts    ! integer values
    logical, pointer, &
        dimension(:,:,:)   :: newLogs  ! logical values

    varType = blck % varType
    numM = blck % numM
    numN = blck % numN
    oldSize = 0

    if ( varType .eq. Dio_Plt_Real) then
        if ( associated(blck % reals)) then
             oldSize = size(blck % reals, 3)
        endif
        if(oldSize.lt.dataSize) then
            newSize = oldSize + Dio3dBlockAllocSize
            if (newSize.lt.dataSize .or. dataSize .eq. 1) then
                newSize = dataSize
            endif
            allocate (newReals(numM, numN,newSize) )
            if ( associated(blck % reals)) then
                newReals(:,:,1:oldSize) = blck % reals
                deallocate(blck % reals) ; nullify(blck % reals)
            endif
            blck % reals => newReals
        endif
    else if ( varType .eq. Dio_Plt_Double) then
        if ( associated(blck % doubles)) then
             oldSize = size(blck % doubles, 3)
        endif
        if(oldSize.lt.dataSize) then
            newSize = oldSize + Dio3dBlockAllocSize
            if (newSize.lt.dataSize .or. dataSize .eq. 1) then
                newSize = dataSize
            endif
            allocate (newDoubles(numM, numN,newSize) )
            if ( associated(blck % doubles)) then
                newDoubles(:,:,1:oldSize) = blck % doubles
                deallocate(blck % doubles) ; nullify(blck % doubles)
            endif
            blck % doubles => newDoubles
        endif
    else if ( varType .eq. Dio_Plt_Integer) then
        if ( associated(blck % ints)) then
             oldSize = size(blck % ints, 3)
        endif
        if(oldSize.lt.dataSize) then
            newSize = oldSize + Dio3dBlockAllocSize
            if (newSize.lt.dataSize .or. dataSize .eq. 1) then
                newSize = dataSize
            endif
            allocate (newInts(numM, numN,newSize) )
            if ( associated(blck % ints)) then
                newInts(:,:,1:oldSize) = blck % ints
                deallocate(blck % ints) ; nullify(blck % ints)
            endif
            blck % ints => newInts
        endif
    else if ( varType .eq. Dio_Plt_Logical) then
        if ( associated(blck % logs)) then
             oldSize = size(blck % logs, 3)
        endif
        if(oldSize.lt.dataSize) then
            newSize = oldSize + Dio3dBlockAllocSize
            if (newSize.lt.dataSize .or. dataSize .eq. 1) then
                newSize = dataSize
            endif
            allocate (newLogs(numM, numN,newSize) )
            if ( associated(blck % logs)) then
                newLogs(:,:,1:oldSize) = blck % logs
                deallocate(blck % logs) ; nullify(blck % logs)
            endif
            blck % logs => newLogs
        endif
    endif
end subroutine Dio3DAllocateValues


#if 0
!
! Allocate space for selection block (TODO: REMOVE or USE)
!
subroutine Dio3DAllocateSelection(blck, numM, numN, numT)
    ! arguments
    type(Dio3DType)        :: blck     ! 3d data blck
    integer                :: numM, numN, numT ! #sizes

    select case ( blck % varType )

        case (Dio_Plt_Real)
            if ( associated(blck % reals)) deallocate(blck % reals)
            allocate(blck % reals(numM, numN, numT))

        case (Dio_Plt_Double)
            if ( associated(blck % doubles)) deallocate(blck % doubles)
            allocate(blck % doubles(numM, numN, numT))

        case (Dio_Plt_Integer)
            if ( associated(blck % ints)) deallocate(blck % ints)
            allocate(blck % ints(numM, numN, numT))

        case (Dio_Plt_Logical)
            if ( associated(blck % logs)) deallocate(blck % logs)
            allocate(blck % logs(numM, numN, numT))
    end select

end subroutine Dio3DAllocateSelection
#endif


!
! Destroy space for dataset values
!
subroutine Dio3DDestroyValues(blck)
    ! arguments
    type(Dio3DType)                :: blck      ! dataset
    ! body
    if ( associated (blck % reals) ) then
        deallocate(blck % reals) ; nullify(blck % reals)
    else if ( associated (blck % doubles) ) then
        deallocate(blck % doubles) ; nullify(blck % doubles)
    else if ( associated (blck % ints) ) then
        deallocate(blck % ints) ; nullify(blck % ints)
    else if ( associated (blck % logs) ) then
        deallocate(blck % logs) ; nullify(blck % logs)
    endif
end subroutine Dio3DDestroyValues


!
! Create empty 3D data block
!
function Dio3DCreateEmpty(varType) result(blck)
    ! return value
    type(Dio3DType)      :: blck    ! dataset
    ! arguments
    integer, intent(IN)  :: varType ! type of variabe
    ! body
    nullify(blck % reals)
    nullify(blck % doubles)
    nullify(blck % ints)
    nullify(blck % logs)

    blck % varType = varType
    blck % numM    = 0
    blck % numN    = 0
end function Dio3DCreateEmpty


!
! Create empty 3D data block
!
function Dio3DCreate(varType, nM, nN) result(blck)
    ! return value
    type(Dio3DType)      :: blck            ! dataset
    ! arguments
    integer, intent(IN)  :: varType, nM, nN ! type, sizes
    ! body
    blck = Dio3DCreateEmpty(varType)
    blck % numM    = nM
    blck % numN    = nN
end function Dio3DCreate


!
! Cleanup 3D data block
!
subroutine Dio3DDestroy(blck)
    ! arguments
    type(Dio3DType)      :: blck    ! dataset
    ! boddy
    call Dio3DDestroyValues(blck)
    blck % numM    = 0
    blck % numN    = 0
end subroutine Dio3DDestroy


!!
!! IO-functions
!!


subroutine Dio3DWriteValuesASCII(blck, lun, indx)

    ! arguments

    type(Dio3DType) :: blck     ! values block
    integer         :: lun      ! stream lun
    integer         :: indx     ! 3th array index

    ! locals

    integer         :: n, m     ! counters

    ! body

    if ( associated (blck % reals) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                write (lun,*) blck % reals(m,n,indx)
            enddo
        enddo
    else if ( associated (blck % doubles) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                write (lun,*) blck % doubles(m,n,indx)
            enddo
        enddo
    else if ( associated (blck % ints) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                write (lun,*) blck % ints(m,n,indx)
            enddo
        enddo
    else if ( associated (blck % logs) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                write (lun,*) blck % logs(m,n,indx)
            enddo
        enddo
    endif

end subroutine Dio3DWriteValuesASCII


function Dio3DReadValuesASCII(blck, lun, indx) result(retVal)

    ! return value

    logical         :: retVal   ! .true.: read succesful

    ! arguments

    type(Dio3DType) :: blck     ! values block
    integer         :: lun      ! stream lun
    integer         :: indx     ! 3th array index

    ! locals

    integer         :: n, m     ! counters

    ! body

    retVal = .false.
    if ( associated (blck % reals) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                read (lun,*, err=999, end=999) blck % reals(m,n,indx)
            enddo
        enddo
    else if ( associated (blck % doubles) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                read (lun,*, err=999, end=999) blck % doubles(m,n,indx)
            enddo
        enddo
    else if ( associated (blck % ints) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                read (lun,*, err=999, end=999) blck % ints(m,n,indx)
            enddo
        enddo
    else if ( associated (blck % logs) ) then
        do m = 1, blck % numM
            do n = 1, blck % numN
                read (lun,*, err=999, end=999) blck % logs(m,n,indx)
            enddo
        enddo
    endif

    retVal = .true.
    return

999 continue

end function Dio3DReadValuesASCII


subroutine Dio3DWriteValuesBinary(blck, lun, indx)

    ! arguments

    type(Dio3DType)           :: blck      ! value block
    integer                   :: lun       ! stream lun
    integer                   :: indx      ! currentTimeIndex

    ! body

    if ( associated (blck % reals) ) then
        write (lun) blck % reals(:,:, indx)
    else if ( associated (blck % doubles) ) then
        write (lun) blck % doubles(:,:, indx)
    else if ( associated (blck % ints) ) then
        write (lun) blck % ints(:,:, indx)
    else if ( associated (blck % logs) ) then
        write (lun) blck % logs(:,:, indx)
    endif

end subroutine Dio3DWriteValuesBinary


function Dio3DReadValuesBinary(blck, lun, indx) result(retVal)

    ! return value

    logical         :: retVal   ! .true.: read succesful

    ! arguments

    type(Dio3DType) :: blck     ! value block
    integer         :: lun      ! stream lun
    integer         :: indx     ! currentTimeIndex

    ! body

    retVal = .true.

    if ( associated (blck % reals) ) then
        read (lun, err=999, end=999) blck % reals(:,:, indx)
    else if ( associated (blck % doubles) ) then
        read (lun, err=999, end=999) blck % doubles(:,:, indx)
    else if ( associated (blck % ints) ) then
        read (lun, err=999, end=999) blck % ints(:,:, indx)
    else if ( associated (blck % logs) ) then
        read (lun, err=999, end=999) blck % logs(:,:, indx)
    endif

    return

999 continue

    retVal = .false.

end function Dio3DReadValuesBinary


function Dio3DWriteValuesShm(blck, shmHandle, indx) result(retVal)

    ! return value

    logical         :: retVal   ! .true.: read succesful

    ! arguments

    type(Dio3DType)                   :: blck      ! values block
    type(DsShmHandle)                 :: shmHandle ! shared mem handle
    integer                           :: indx      ! 3th array index

    ! body

    retVal =  .false.
    if ( DioShmDsStartWrite(shmHandle) ) then
        if ( associated (blck % reals) ) then
            call DioShmDsWrite(shmHandle, blck % reals(:,:,indx))
        else if ( associated (blck % doubles) ) then
            call DioShmDsWrite(shmHandle, blck % doubles(:,:,indx))
        else if ( associated (blck % ints) ) then
            call DioShmDsWrite(shmHandle, blck % ints(:,:,indx))
        else if ( associated (blck % logs) ) then
            write (*,*) 'blck % logs(m,n,indx) not imp yet'
        endif
        call DioShmDsEndWrite(shmHandle)
        retVal =  .true.
    endif

end function Dio3DWriteValuesShm


function Dio3DReadValuesShm(blck, shmHandle, indx) result(retVal)

    ! return value

    logical            :: retVal    ! .true.: read succesful

    ! arguments

    type(Dio3DType)    :: blck      ! values block
    type(DsShmHandle)  :: shmHandle ! shared mem handle
    integer            :: indx      ! 3th array index

    ! body

    retVal = .false.
    if ( DioShmDsStartRead(shmHandle) ) then
        if ( associated (blck % reals) ) then
            call DioShmDsRead(shmHandle, blck % reals(:,:,indx))
        else if ( associated (blck % doubles) ) then
            call DioShmDsRead(shmHandle, blck % doubles(:,:,indx))
        else if ( associated (blck % ints) ) then
            call DioShmDsRead(shmHandle, blck % ints(:,:,indx))
        else if ( associated (blck % logs) ) then
            write (*,*) 'blck % logs(m,n,indx) not imp yet'
        endif
        call DioShmDsEndRead(shmHandle)
        retVal = .true.
    endif

end function Dio3DReadValuesShm


end module Dio_3d_block
