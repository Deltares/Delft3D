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
!!! Dio-PLT-Shm: (sobek)HIS I/O for Param./Loc./Time datasets
!!!
!!! (c) Deltares, mar 2001
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!
!! Write PLT Header to Shared Memory
!!

subroutine DioPltWriteHeaderShm(plt)

    use dio_plt_rw

    ! arguments

    type(DioPltType), target          :: plt        ! dataset

    ! locals

    type(DioPltHeaderType), pointer   :: header     ! dataset header
    type(DsShmHandle), pointer        :: shmHandle  ! sharedMem datasethandle
    integer                           :: headerSize ! numBytes in header info
    integer                           :: dataSize   ! numBytes in actual data
    ! body

    header => plt % header
    shmHandle => plt % ds % outStream % shmHandle

    if ( header % varType .eq. Dio_Plt_Double ) then
        dataSize = 8
    else
        dataSize = 4
    endif
    dataSize = dataSize * header % nPar * header % nLoc
    headerSize = 12 + header % nPar * DioMaxParLen&
                    + header % nLoc * DioMaxLocLen

    if (DioShmDsSetsize(shmHandle, headerSize, dataSize)) then
        if (DioShmDsStartWriteHdr(shmHandle )) then
            call  DioShmDsWriteHdr(shmHandle, header % npar)
            call  DioShmDsWriteHdr(shmHandle, header % nloc)

            if ( header % nPar .gt. 0 ) then
                call  DioShmDsWriteHdr(shmHandle, header % nPar, DioMaxParLen, header % pars)
            endif
            if ( header % nLoc .gt. 0 ) then
                if (plt % ds % outStream % streamType == Dio_WQMap_Stream) then
                   call  DioShmDsWriteHdr(shmHandle, header % nLoc)
                else
                   call  DioShmDsWriteHdr(shmHandle, header % nLoc, DioMaxLocLen, header % locs)
                endif
            endif

            call  DioShmDsWriteHdr(shmHandle, header % varType)

            call  DioShmDsEndWriteHdr(shmHandle )
        endif
    endif

end subroutine DioPltWriteHeaderShm


!!
!! Read PLT Header from Shared Memory
!!

subroutine DioPltReadHeaderShm(plt)

    use dio_plt_rw

    ! arguments

    type(DioPltType), target          :: plt       ! dataset

    ! locals

    type(DioPltHeaderType), pointer   :: header    ! dataset header
    type(DsShmHandle), pointer       :: shmHandle ! shared men dataset handle

    ! body

    header    => plt % header
    shmHandle => plt % ds % inStream % shmHandle

    if (DioShmDsStartReadHdr(shmHandle )) then
        call  DioShmDsReadHdr(shmHandle, header % npar)
        call DioShmDsReadHdr(shmHandle, header % nloc)
        if ( header % nPar .gt. 0 ) then
            allocate(header % pars(header % nPar))
            header % pars = ''
            call  DioShmDsReadHdr(shmHandle, header % nPar, DioMaxParLen, header % pars)
        endif
        if ( header % nLoc .gt. 0 .and. plt % ds % inStream % streamType /= Dio_WQMap_Stream) then
            allocate(header % locs(header % nLoc))
            header % locs = ''
            call  DioShmDsReadHdr(shmHandle, header % nLoc, DioMaxLocLen, header % locs)
        endif

        call  DioShmDsReadHdr(shmHandle, header % varType)

        call  DioShmDsEndReadHdr(shmHandle )
    endif

end subroutine DioPltReadHeaderShm

