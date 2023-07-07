!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2023.
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
    use waq_dio_streams
    implicit none
    ! arguments
    type(waq_DioStreamType)       :: stream         ! handle to DIO stream
    ! locals
    character(Len=3)          :: dummyName = '' ! dummy ds name
    ! body
    call DioSyncSetItemAvailable(stream, dummyName, '.stream')

end subroutine DioSyncSetStreamAvailable


function DioSyncGetStreamAvailable(stream) result(retVal)
    use waq_dio_streams
    implicit none
    include 'dio-sync-support.inc'
    ! return value
    logical                   :: retVal         ! .true. succes
    ! arguments
    type(waq_DioStreamType)       :: stream         ! handle to DIO stream
    ! locals
    character(Len=3)          :: dummyName = '' ! dummy ds name
    ! body
    retVal = DioSyncGetItemAvailable(stream, dummyName, '.stream')
end function DioSyncGetStreamAvailable


subroutine DioSyncSetStreamReceived(stream)
    use waq_dio_streams
    implicit none
    ! arguments
    type(waq_DioStreamType)       :: stream         ! handle to DIO stream
    ! locals
    character(Len=3)          :: dummyName = '' ! dummy ds name
    ! body
    call DioSyncSetItemReceived(stream, dummyName, '.stream')

end subroutine DioSyncSetStreamReceived


!!
!! Synchronisation functions for datasets
!!

function DioSyncDsItemCanBeSent(ds) result(retVal)
    use waq_Dio_ds
    implicit none
    ! result
    logical                       :: retVal     ! .true.: success
    ! arguments
    type(waq_DioDsType)               :: ds         ! dataset
    ! locals
    character(Len=DioMaxStreamLen):: fname      ! vars to check file existence
    logical                       :: exists
    integer                       :: timeWaited
    character(Len=DioMaxStreamLen):: dsBaseName ! base name of dataset

    ! body

    retVal =.true.
    if ( ds % outStream % synched ) then
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
    use waq_Dio_ds

    implicit none
    ! arguments
    type(waq_DioDsType)        :: ds             ! dataset
    ! body

    call DioSyncSetItemAvailable(ds % outStream, ds % name, '.data')
    
end subroutine DioSyncDsItemSent


function DioSyncDsItemAvailable(ds) result(retVal)
    use waq_Dio_ds

    implicit none
    include 'dio-sync-support.inc'
    ! return value
    logical                :: retVal         ! .true. succes
    ! arguments
    type(waq_DioDsType)        :: ds             ! dataset
    ! body

    retVal = DioSyncGetItemAvailable(ds % inStream, ds % name, '.data')
    
end function DioSyncDsItemAvailable


subroutine DioSyncDsItemReceived(ds)
    use waq_Dio_ds

    implicit none
    ! arguments
    type(waq_DioDsType)        :: ds             ! dataset
    ! body
    call DioSyncSetItemReceived(ds % inStream, ds % name, '.data')
    
end subroutine DioSyncDsItemReceived


