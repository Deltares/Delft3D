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
!!! Dio_shm_f2c_f: F90 part of DioShm F90-to-C++ interface
!!!
!!! (c) Deltares, aug 2002
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module dio_shm

use precision
!!
!! Enumeration for Memory Type (must be equal to enum in dio_shm.h)
!!
integer, parameter :: DioShmUnknown   = 0
integer, parameter :: DioShmInMem     = 1
integer, parameter :: DioShmSharedMem = 2


!!
!! Enumeration for header/data part (must be equal to enum in dio_sync.h)
!!
integer, parameter :: DioShmHeaderPart = 0
integer, parameter :: DioShmDataPart   = 1


!!
!! F90 handle to c++-pointer
!!
type DsShmHandle
    integer(pntrsize) :: cppPntr
end type DsShmHandle


!!
!! Interfaces
!!

interface DioShmDsCreate
    module procedure DioShmDsCreateWithSize
    module procedure DioShmDsCreateNoSize
end interface


interface DioShmDsWriteHdr
    module procedure DioShmDsWriteHdrReals
    module procedure DioShmDsWriteHdrReals2D
    module procedure DioShmDsWriteHdrDoubles2D
    module procedure DioShmDsWriteHdrInts
    module procedure DioShmDsWriteHdrInts2D
    module procedure DioShmDsWriteHdrInt
    module procedure DioShmDsWriteHdrStrings
end interface

interface DioShmDsWrite
    module procedure DioShmDsWriteReals
    module procedure DioShmDsWriteReals2D
    module procedure DioShmDsWriteDoubles2D
    module procedure DioShmDsWriteInts
    module procedure DioShmDsWriteInts2D
    module procedure DioShmDsWriteInt
    module procedure DioShmDsWriteStrings
end interface


interface DioShmDsReadHdr
    module procedure DioShmDsReadHdrReals
    module procedure DioShmDsReadHdrReals2D
    module procedure DioShmDsReadHdrDoubles2D
    module procedure DioShmDsReadHdrInts
    module procedure DioShmDsReadHdrInts2D
    module procedure DioShmDsReadHdrInt
    module procedure DioShmDsReadHdrStrings
end interface

interface DioShmDsRead
    module procedure DioShmDsReadReals
    module procedure DioShmDsReadReals2D
    module procedure DioShmDsReadDoubles2D
    module procedure DioShmDsReadInts
    module procedure DioShmDsReadInts2D
    module procedure DioShmDsReadInt
    module procedure DioShmDsReadStrings
end interface


interface DioShmDataBlockPut
    module procedure DioShmDataBlockPutDouble
end interface

interface DioShmDataBlockGet
    module procedure DioShmDataBlockGetDouble
end interface

contains


!!
!! Create / Destroy datasets:
!! - Create with size known in advance
!! - Create with unknown size,
!!   Set Size later on.
!! - Destroy dataset
!!

function DioShmDsCreateWithSize(f90Handle, hdrSize, dataSize, memType, dsName) result(retVal)

    integer          :: retVal

    type(DsShmHandle), pointer :: f90Handle
    integer          :: hdrSize, dataSize
    integer          :: memType
    character(Len=*) :: dsName

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    allocate(f90Handle)

    call DIO_SHM_F2C_DS_DEFINE_C(retVal, hdrSize, dataSize, memType, f90Handle % cppPntr, dsName)
    if ( retVal .ne. 0 ) then
        deallocate(f90Handle) ; nullify(f90Handle)
    endif
    
#endif

end function DioShmDsCreateWithSize


function DioShmDsSetsize(f90Handle, headerSize, dataSize) result(retVal)

    logical       :: retVal

    type(DsShmHandle), pointer :: f90Handle
    integer       :: headerSize
    integer       :: dataSize

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = .false.
#else

    integer       :: locRetVal

    call DIO_SHM_F2C_DS_SETSIZE_C(locRetVal, f90Handle % cppPntr, headerSize, dataSize)
    retVal = (locRetVal .ne. 0)

#endif

end function DioShmDsSetsize


function DioShmDsSetsizePart(f90Handle, part, partSize) result(retVal)

    logical       :: retVal

    type(DsShmHandle), pointer :: f90Handle
    integer       :: part
    integer       :: partSize

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = .false.
#else

    integer       :: locRetVal

    call DIO_SHM_F2C_DS_SETSIZEPART_C(locRetVal, f90Handle % cppPntr, part, partSize)
    retVal = (locRetVal .ne. 0)

#endif

end function DioShmDsSetsizePart


function DioShmDsCreateNoSize(f90Handle, memType, dsName) result(retVal)

    integer       :: retVal

    type(DsShmHandle), pointer :: f90Handle
    integer                    :: memType
    character(Len=*)           :: dsName

    allocate(f90Handle)

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = -99
#else

    call DIO_SHM_F2C_DS_GETINFO_C(retVal, memType, f90Handle % cppPntr, dsName)
    if ( retVal .ne. 0 ) then
        deallocate(f90Handle) ; nullify(f90Handle)
    endif
#endif

    
end function DioShmDsCreateNoSize


subroutine DioShmDsDestroy(f90Handle)

    type(DsShmHandle), pointer :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_DESTROY_C(f90Handle % cppPntr)
    deallocate(f90Handle) ; nullify(f90Handle)

#endif

end subroutine DioShmDsDestroy


!!
!! Synchronisation Functions:
!! - Start/End of Writing data to header part
!! - Start/End of Reading data to header part
!! - Start/End of Writing data to data part
!! - Start/End of Reading data to data part
!!

function DioShmDsStartWriteHdr(f90Handle) result(retVal)

    logical :: retVal
    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = .false.
#else

    integer :: locRetVal

    call DIO_SHM_F2C_START_WRITE_C(f90Handle % cppPntr, DioShmHeaderPart, locRetVal)
    retVal = (locRetVal .ne. 0)
    
#endif

end function DioShmDsStartWriteHdr


subroutine DioShmDsEndWriteHdr(f90Handle)

    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_END_WRITE_C(f90Handle % cppPntr, DioShmHeaderPart)
    
#endif

end subroutine DioShmDsEndWriteHdr


function DioShmDsStartReadHdr(f90Handle) result(retVal)

    logical :: retVal
    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = .false.
#else

    integer :: locRetVal

    call DIO_SHM_F2C_START_READ_C(f90Handle % cppPntr, DioShmHeaderPart, locRetVal)
    retVal = (locRetVal .ne. 0)
    
#endif

end function DioShmDsStartReadHdr


subroutine DioShmDsEndReadHdr(f90Handle)

    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_END_READ_C(f90Handle % cppPntr, DioShmHeaderPart)
    
#endif

end subroutine DioShmDsEndReadHdr


function DioShmDsStartWrite(f90Handle) result(retVal)

    logical :: retVal
    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = .false.
#else

    integer :: locRetVal

    call DIO_SHM_F2C_START_WRITE_C(f90Handle % cppPntr, DioShmDataPart, locRetVal)
    retVal = (locRetVal .ne. 0)
    
#endif

end function DioShmDsStartWrite


subroutine DioShmDsEndWrite(f90Handle)

    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_END_WRITE_C(f90Handle % cppPntr, DioShmDataPart)

#endif
    
end subroutine DioShmDsEndWrite


function DioShmDsStartRead(f90Handle) result(retVal)

    logical :: retVal
    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = .false.
#else

    integer :: locRetVal

    call DIO_SHM_F2C_START_READ_C(f90Handle % cppPntr, DioShmDataPart, locRetVal)
    retVal = (locRetVal .ne. 0)
    
#endif

end function DioShmDsStartRead


subroutine DioShmDsEndRead(f90Handle)

    type(DsShmHandle) :: f90Handle

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_END_READ_C(f90Handle % cppPntr, DioShmDataPart)
    
end subroutine DioShmDsEndRead


!!
!! Write data to the header part
!! - Write function for every variable type
!!

subroutine DioShmDsWriteHdrReals(f90Handle, nReals, reals)

    type(DsShmHandle)  :: f90Handle
    integer            :: nReals
    real, dimension(:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_WRITE_REALS_C(f90Handle % cppPntr, DioShmHeaderPart, nReals, reals)
    
#endif

end subroutine DioShmDsWriteHdrReals


subroutine DioShmDsWriteHdrReals2D(f90Handle, reals)

    type(DsShmHandle)    :: f90Handle
    real, dimension(:,:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer              :: nReals

    nReals = size(reals,1) * size(reals,2) 
    call DIO_SHM_F2C_DS_WRITE_REALS_C(f90Handle % cppPntr, DioShmHeaderPart, nReals, reals)
    
#endif

end subroutine DioShmDsWriteHdrReals2D


subroutine DioShmDsWriteHdrDoubles2D(f90Handle, doubles)

    type(DsShmHandle)                :: f90Handle
    double precision, dimension(:,:) :: doubles

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer                          :: nDoubles

    nDoubles = size(doubles,1) * size(doubles,2) 
    call DIO_SHM_F2C_DS_WRITE_DOUBLES_C(f90Handle % cppPntr, DioShmHeaderPart, nDoubles, doubles)
    
#endif

end subroutine DioShmDsWriteHdrDoubles2D


subroutine DioShmDsWriteHdrInts(f90Handle, nInts, ints)

    type(DsShmHandle)     :: f90Handle
    integer, dimension(:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer               :: nInts

    call DIO_SHM_F2C_DS_WRITE_INTS_C(f90Handle % cppPntr, DioShmHeaderPart, nInts, ints)
    
#endif

end subroutine DioShmDsWriteHdrInts


subroutine DioShmDsWriteHdrInts2D(f90Handle, ints)

    type(DsShmHandle)       :: f90Handle
    integer, dimension(:,:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer                 :: nInts

    nInts = size(ints,1) * size(ints,2) 
    call DIO_SHM_F2C_DS_WRITE_INTS_C(f90Handle % cppPntr, DioShmHeaderPart, nInts, ints)
    
#endif

end subroutine DioShmDsWriteHdrInts2D


subroutine DioShmDsWriteHdrInt(f90Handle, int)

    type(DsShmHandle) :: f90Handle
    integer           :: int

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_WRITE_INT_C(f90Handle % cppPntr, DioShmHeaderPart, int)
    
#endif

end subroutine DioShmDsWriteHdrInt


subroutine DioShmDsWriteHdrStrings(f90Handle, nStrings, strSize, strings)

    type(DsShmHandle) :: f90Handle
    integer           :: nStrings, strSize
    character(Len=strSize), dimension(:) :: strings

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_WRITE_CHARS_C(f90Handle % cppPntr, DioShmHeaderPart, nStrings*strSize, strings(1))
    
#endif

end subroutine DioShmDsWriteHdrStrings



!!
!! Read data from the header part
!! - Read function for every variable type
!!

subroutine DioShmDsReadHdrReals(f90Handle, nReals, reals)

    type(DsShmHandle) :: f90Handle
    integer       :: nReals
    real, dimension(:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_READ_REALS_C(f90Handle % cppPntr, DioShmHeaderPart, nReals, reals)
    
#endif

end subroutine DioShmDsReadHdrReals


subroutine DioShmDsReadHdrReals2D(f90Handle, reals)

    type(DsShmHandle) :: f90Handle
    real, dimension(:,:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer       :: nReals

    nReals = size(reals,1) * size(reals,2) 
    call DIO_SHM_F2C_DS_READ_REALS_C(f90Handle % cppPntr, DioShmHeaderPart, nReals, reals)
    
#endif

end subroutine DioShmDsReadHdrReals2D


subroutine DioShmDsReadHdrDoubles2D(f90Handle, doubles)

    type(DsShmHandle) :: f90Handle
    double precision, dimension(:,:) :: doubles

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer       :: nDoubles

    nDoubles = size(doubles,1) * size(doubles,2) 
    call DIO_SHM_F2C_DS_READ_DOUBLES_C(f90Handle % cppPntr, DioShmHeaderPart, nDoubles, doubles)
    
#endif

end subroutine DioShmDsReadHdrDoubles2D


subroutine DioShmDsReadHdrInts(f90Handle, nInts, ints)

    type(DsShmHandle) :: f90Handle
    integer, dimension(:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer       :: nInts

    call DIO_SHM_F2C_DS_READ_INTS_C(f90Handle % cppPntr, DioShmHeaderPart, nInts, ints)
    
#endif

end subroutine DioShmDsReadHdrInts


subroutine DioShmDsReadHdrInt(f90Handle, int)

    type(DsShmHandle) :: f90Handle
    integer       :: int

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_READ_INT_C(f90Handle % cppPntr, DioShmHeaderPart, int)
    
#endif

end subroutine DioShmDsReadHdrInt


subroutine DioShmDsReadHdrInts2D(f90Handle, ints)

    type(DsShmHandle) :: f90Handle
    integer, dimension(:,:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer       :: nInts

    nInts = size(ints,1) * size(ints,2) 
    call DIO_SHM_F2C_DS_READ_INTS_C(f90Handle % cppPntr, DioShmHeaderPart, nInts, ints)
    
#endif

end subroutine DioShmDsReadHdrInts2D


subroutine DioShmDsReadHdrStrings(f90Handle, nStrings, strSize, strings)

    type(DsShmHandle) :: f90Handle
    integer       :: nStrings, strSize
    character(Len=strSize), dimension(:) :: strings

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_READ_CHARS_C(f90Handle % cppPntr, DioShmHeaderPart, nStrings*strSize, strings(1))
    
#endif

end subroutine DioShmDsReadHdrStrings


!!
!! Write data to the data part
!! - Write function for every variable type
!!

subroutine DioShmDsWriteReals(f90Handle, nReals, reals)

    type(DsShmHandle)  :: f90Handle
    integer            :: nReals
    real, dimension(:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_WRITE_REALS_C(f90Handle % cppPntr, DioShmDataPart, nReals, reals)
    
#endif

end subroutine DioShmDsWriteReals


subroutine DioShmDsWriteReals2D(f90Handle, reals)

    type(DsShmHandle)    :: f90Handle
    real, dimension(:,:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer              :: nReals

    nReals = size(reals,1) * size(reals,2) 
    call DIO_SHM_F2C_DS_WRITE_REALS_C(f90Handle % cppPntr, DioShmDataPart, nReals, reals)
    
#endif

end subroutine DioShmDsWriteReals2D


subroutine DioShmDsWriteDoubles2D(f90Handle, doubles)

    type(DsShmHandle)                :: f90Handle
    double precision, dimension(:,:) :: doubles

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer                          :: nDoubles

    nDoubles = size(doubles,1) * size(doubles,2) 
    call DIO_SHM_F2C_DS_WRITE_DOUBLES_C(f90Handle % cppPntr, DioShmDataPart, nDoubles, doubles)
    
#endif

end subroutine DioShmDsWriteDoubles2D


subroutine DioShmDsWriteInts(f90Handle, nInts, ints)

    type(DsShmHandle)     :: f90Handle
    integer               :: nInts
    integer, dimension(:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_WRITE_INTS_C(f90Handle % cppPntr, DioShmDataPart, nInts, ints)
    
#endif

end subroutine DioShmDsWriteInts


subroutine DioShmDsWriteInts2D(f90Handle, ints)

    type(DsShmHandle)       :: f90Handle
    integer, dimension(:,:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer                 :: nInts

    nInts = size(ints,1) * size(ints,2) 
    call DIO_SHM_F2C_DS_WRITE_INTS_C(f90Handle % cppPntr, DioShmDataPart, nInts, ints)
    
#endif

end subroutine DioShmDsWriteInts2D


subroutine DioShmDsWriteInt(f90Handle, int)

    type(DsShmHandle) :: f90Handle
    integer           :: int

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_WRITE_INT_C(f90Handle % cppPntr, DioShmDataPart, int)
    
#endif

end subroutine DioShmDsWriteInt


subroutine DioShmDsWriteStrings(f90Handle, nStrings, strSize, strings)

    type(DsShmHandle) :: f90Handle
    integer           :: nStrings, strSize
    character(Len=strSize), dimension(:) :: strings

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_WRITE_CHARS_C(f90Handle % cppPntr, DioShmDataPart, nStrings*strSize, strings(1))
    
#endif

end subroutine DioShmDsWriteStrings


!!
!! Read data from the data  part
!! - Read function for every variable type
!!

subroutine DioShmDsReadReals(f90Handle, nReals, reals)

    type(DsShmHandle) :: f90Handle
    integer       :: nReals
    real, dimension(:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_READ_REALS_C(f90Handle % cppPntr, DioShmDataPart, nReals, reals)
    
#endif

end subroutine DioShmDsReadReals


subroutine DioShmDsReadReals2D(f90Handle, reals)

    type(DsShmHandle) :: f90Handle
    real, dimension(:,:) :: reals

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer       :: nReals

    nReals = size(reals,1) * size(reals,2) 
    call DIO_SHM_F2C_DS_READ_REALS_C(f90Handle % cppPntr, DioShmDataPart, nReals, reals)
    
#endif

end subroutine DioShmDsReadReals2D


subroutine DioShmDsReadDoubles2D(f90Handle, doubles)

    type(DsShmHandle) :: f90Handle
    double precision, dimension(:,:) :: doubles

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer       :: nDoubles

    nDoubles = size(doubles,1) * size(doubles,2) 
    call DIO_SHM_F2C_DS_READ_DOUBLES_C(f90Handle % cppPntr, DioShmDataPart, nDoubles, doubles)
    
#endif

end subroutine DioShmDsReadDoubles2D


subroutine DioShmDsReadInts(f90Handle, nInts, ints)

    type(DsShmHandle) :: f90Handle
    integer       :: nInts
    integer, dimension(:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_READ_INTS_C(f90Handle % cppPntr, DioShmDataPart, nInts, ints)
    
#endif

end subroutine DioShmDsReadInts


subroutine DioShmDsReadInt(f90Handle, int)

    type(DsShmHandle) :: f90Handle
    integer       :: int

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_READ_INT_C(f90Handle % cppPntr, DioShmDataPart, int)
    
#endif

end subroutine DioShmDsReadInt


subroutine DioShmDsReadInts2D(f90Handle, ints)

    type(DsShmHandle) :: f90Handle
    integer, dimension(:,:) :: ints

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    integer       :: nInts

    nInts = size(ints,1) * size(ints,2) 
    call DIO_SHM_F2C_DS_READ_INTS_C(f90Handle % cppPntr, DioShmDataPart, nInts, ints)
    
#endif

end subroutine DioShmDsReadInts2D


subroutine DioShmDsReadStrings(f90Handle, nStrings, strSize, strings)

    type(DsShmHandle) :: f90Handle
    integer       :: nStrings, strSize
    character(Len=strSize), dimension(:) :: strings

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DS_READ_CHARS_C(f90Handle % cppPntr, DioShmDataPart, nStrings*strSize, strings(1))
    
#endif

end subroutine DioShmDsReadStrings


!!
!! F90 interface to Shared Mem DataBlock functions
!! - put a block of named data for a certain var. type
!! - get a block of named data for a certain var. type
!! remark: currently only implements one double, for control-lib
!!

subroutine DioShmDataBlockPutDouble(name, dble)

    character(Len=*) :: name
    double precision :: dble

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_PUTDB_DOUBLES_C(1, dble, name)

#endif

end subroutine DioShmDataBlockPutDouble


function DioShmDataBlockGetDouble(name, dble) result(retVal)

    logical          :: retVal

    character(Len=*) :: name
    double precision :: dble

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
    retVal = .false.
#else

    integer          :: locRetVal

    call DIO_SHM_F2C_GETDB_DOUBLES_C(locRetVal, 1, dble, name)
    retVal = (locRetVal .eq. 1)

#endif

end function DioShmDataBlockGetDouble


subroutine DioShmDataBlockFree(name)

    character(Len=*) :: name

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_FREEDB_C(name)

#endif

end subroutine DioShmDataBlockFree


subroutine DioShmDataBlockCleanup()

#if(defined(LAHEY_WIN))
    write(*,'(A)') 'Dio Error: Shared Memory not implemented for Lahey'
#else

    call DIO_SHM_F2C_DBCLEANUP_C()

#endif

#endif

end subroutine DioShmDataBlockCleanup


end module dio_shm

!
! F77 interface to Shared Mem DataBlock functions
!


subroutine dio_shm_f77_putdb_double(name, dble)

    use dio_shm

    character(Len=*) :: name
    double precision :: dble

    call DioShmDataBlockPut(name, dble)

end subroutine dio_shm_f77_putdb_double


subroutine dio_shm_f77_getdb_double(name, dble)

    use dio_shm

    character(Len=*) :: name
    double precision :: dble
    logical          :: locRetVal

    locRetVal = DioShmDataBlockGet(name, dble)
    if ( .not. locRetVal ) then
        write(*,*) 'DIO SHM f77_getdb_double ERROR'
    endif

end subroutine dio_shm_f77_getdb_double


subroutine dio_shm_f77_dbcleanup()

    use dio_shm

    call DioShmDataBlockCleanup()

end subroutine dio_shm_f77_dbcleanup


