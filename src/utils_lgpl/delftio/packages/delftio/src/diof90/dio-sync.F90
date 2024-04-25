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
!!! DIO-sync: synchronisation functions for DelftIO
!!!
!!! (c) Deltares, dec 2000
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!
!! Synchronisation functions for streams
!!

subroutine DioSyncSetStreamAvailable(stream)
    use dio_streams
    implicit none
    ! arguments
    type(DioStreamType)       :: stream         ! handle to DIO stream
    ! locals
    character(Len=3)          :: dummyName = '' ! dummy ds name
    ! body
    call DioSyncSetItemAvailable(stream, dummyName, '.stream')

end subroutine DioSyncSetStreamAvailable


function DioSyncGetStreamAvailable(stream) result(retVal)
    use dio_streams
    implicit none
    include 'dio-sync-support.inc'
    ! return value
    logical                   :: retVal         ! .true. succes
    ! arguments
    type(DioStreamType)       :: stream         ! handle to DIO stream
    ! locals
    character(Len=3)          :: dummyName = '' ! dummy ds name
    ! body
    retVal = DioSyncGetItemAvailable(stream, dummyName, '.stream')
end function DioSyncGetStreamAvailable


subroutine DioSyncSetStreamReceived(stream)
    use dio_streams
    implicit none
    ! arguments
    type(DioStreamType)       :: stream         ! handle to DIO stream
    ! locals
    character(Len=3)          :: dummyName = '' ! dummy ds name
    ! body
    call DioSyncSetItemReceived(stream, dummyName, '.stream')

end subroutine DioSyncSetStreamReceived


!!
!! Synchronisation functions for datasets
!!

function DioSyncDsItemCanBeSent(ds) result(retVal)
    use dio_ds
    use dio_shm
    implicit none
    ! result
    logical                       :: retVal     ! .true.: success
    ! arguments    
    type(DioDsType)               :: ds         ! dataset
    ! locals
    character(Len=DioMaxStreamLen):: fname      ! vars to check file existence
    logical                       :: exists
    integer                       :: timeWaited
    character(Len=DioMaxStreamLen):: dsBaseName ! base name of dataset

    ! body

    retVal =.true.
    if ( associated (ds % outStream % shmHandle) ) then
        write (*,*) 'DioSyncDsItemCanBeSent: don not call for SHM'
    else if ( ds % outStream % synched ) then
        call DioSyncMakeBaseName( ds % name, dsBaseName )
        call DioStreamDelay
        timeWaited = 0
        fname = trim(ds % outStream % name) // '_' // trim(dsBaseName) //'.data'
20      continue
        inquire(file=fname,exist=exists)
        if (exists) then
            if (DioStreamSleep(timeWaited)) then
                goto 20
            else
                retVal =.false.
            endif
        endif
        call DioStreamDelay
    endif

end function DioSyncDsItemCanBeSent


subroutine DioSyncDsItemSent(ds)
    use dio_ds
    use dio_shm
    implicit none
    ! arguments
    type(DioDsType)        :: ds             ! dataset
    ! body
    if (associated(ds % outStream % shmHandle) ) then
        write (*,*) 'DioSyncDsItemSent: don not call for SHM'
    else
        call DioSyncSetItemAvailable(ds % outStream, ds % name, '.data')
    endif
end subroutine DioSyncDsItemSent


function DioSyncDsItemAvailable(ds) result(retVal)
    use dio_ds
    use dio_shm
    implicit none
    include 'dio-sync-support.inc'
    ! return value
    logical                :: retVal         ! .true. succes
    ! arguments
    type(DioDsType)        :: ds             ! dataset
    ! body
    if (associated(ds % inStream % shmHandle) ) then
        write (*,*) 'DioSyncDsItemAvailable: don not call for SHM'
    else
        retVal = DioSyncGetItemAvailable(ds % inStream, ds % name, '.data')
    endif
end function DioSyncDsItemAvailable


subroutine DioSyncDsItemReceived(ds)
    use dio_ds
    use dio_shm
    implicit none
    ! arguments
    type(DioDsType)        :: ds             ! dataset
    ! body
    if (associated(ds % inStream % shmHandle) ) then
        write (*,*) 'DioSyncDsItemReceived: don not call for SHM'
    else
        call DioSyncSetItemReceived(ds % inStream, ds % name, '.data')
    endif
end subroutine DioSyncDsItemReceived


