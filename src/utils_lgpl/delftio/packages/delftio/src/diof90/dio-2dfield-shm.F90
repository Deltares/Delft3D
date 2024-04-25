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
!!! Dio-2DF-Shm: (sobek)HIS I/O for Param./Loc./Time datasets
!!!
!!! (c) Deltares, mar 2001
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!!
!! Write 2D-Field Header to Shared Memory
!!

subroutine Dio2DFWriteHeaderShm(f2d)

    use dio_2dfield_rw

    ! arguments

    type(Dio2DFType), target          :: f2d       ! dataset

    ! locals

    type(Dio2DFHeaderType), pointer   :: header    ! dataset header
    type(DsShmHandle), pointer        :: shmHandle ! shared men dataset handle
    integer                           :: headerSize ! numBytes in header info
    integer                           :: dataSize   ! numBytes in actual data

    ! body

    header => f2d % header
    shmHandle => f2d % ds % outStream % shmHandle

    !
    ! Determine Memory Data Size (maximum of header and data per time step)
    !

    if ( header % varType .eq. Dio_Plt_Double ) then
        dataSize = 8
    else
        dataSize = 4
    endif
    dataSize = dataSize * header % numM * header % numN
    headerSize = 12

    !
    ! Set Size. If succesful, wait for permission to write.
    ! Write data, and indicate that write has been performed.
    !

    if (DioShmDsSetsize(shmHandle, headerSize, dataSize)) then
        if (DioShmDsStartWriteHdr(shmHandle )) then

            call  DioShmDsWriteHdr(shmHandle, header % numM)
            call  DioShmDsWriteHdr(shmHandle, header % numN)
            call  DioShmDsWriteHdr(shmHandle, header % varType)

            call  DioShmDsEndWriteHdr(shmHandle )
        endif
    endif

end subroutine Dio2DFWriteHeaderShm


!!
!! Read 2D-Field Header from Shared Memory
!!

subroutine Dio2DFReadHeaderShm(f2d)

    use dio_2dfield_rw

    ! arguments

    type(Dio2DFType), target          :: f2d       ! dataset

    ! locals

    type(Dio2DFHeaderType), pointer   :: header    ! dataset header
    type(DsShmHandle), pointer       :: shmHandle ! shared men dataset handle

    ! body

    header    => f2d % header
    shmHandle => f2d % ds % inStream % shmHandle

    !
    ! Wait for permission to read.
    ! Read data, and indicate that read has been performed.
    !

    if (DioShmDsStartReadHdr(shmHandle )) then

        call  DioShmDsReadHdr(shmHandle, header % numM)
        call  DioShmDsReadHdr(shmHandle, header % numN)
        call  DioShmDsReadHdr(shmHandle, header % varType)

        call  DioShmDsEndReadHdr(shmHandle )
    endif

end subroutine Dio2DFReadHeaderShm

