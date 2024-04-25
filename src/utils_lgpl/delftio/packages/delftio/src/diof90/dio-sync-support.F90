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
!!! DIO-sync-support: synchronisation support functions
!!!
!!! (c) Deltares, dec 2000
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!
!! Support Functions called from dio-sync
!!

subroutine DioSyncMakeBaseName(itemName, baseName)
    character(Len=*), intent(IN) :: itemName  ! ds name
    character(Len=*), intent(OUT):: baseName  ! ds name without path
    integer                      :: slashPos  ! pos. of last slash
    character(Len=1)             :: slash     ! slash or backslash

#if (defined(WIN32))
    slash = '\'
#else
    slash = '/'
#endif

    ! value in case of no slash

    baseName = itemName

    ! find slash pos. from end. If found, skip path.

    slashPos = index(itemName, slash, .true.)
    if ( slashPos .ne. 0 ) then
        baseName = itemName(slashPos+1:)
    endif

end subroutine DioSyncMakeBaseName

!
! Mark an item as being available for the receiver
!

subroutine DioSyncSetItemAvailable(stream, itemName, availType)

    use dio_streams

    implicit none

    ! arguments
    
    type(DioStreamType) :: stream         ! handle to DIO stream
    character(Len=*)    :: itemName       ! empty (for stream), or dataset name
    character(Len=*)    :: availType      ! '.stream' or '.data'

    ! locals
    
    character(Len=DioMaxStreamLen) :: dsBaseName ! base name of dataset
    character(Len=DioMaxStreamLen) :: fname      ! vars to open file
    integer                        :: ierr
    integer                        :: iunit

    ! body

    if ( stream % synched ) then

        call DioSyncMakeBaseName( itemName, dsBaseName )

        call DioStreamDelay
        fname = trim(stream % name) // '_' // trim(dsBaseName) // availType
#if (defined(WIN32))
        open(newunit=iunit,file=fname,status='new', action= 'write', iostat=ierr, share='DENYRW')
#else
        open(newunit=iunit,file=fname,status='new', action= 'write', iostat=ierr)
#endif
        if (ierr.gt.0) then
            call DioStreamError(901, 'Could not set available:', fname)
        else
            close(iunit)
        endif
        call DioStreamDelay
    endif

end subroutine DioSyncSetItemAvailable


!
! Check if an item has been provided by a sender
!

function DioSyncGetItemAvailable(stream, itemName, availType) result(retVal)

    use dio_streams

    implicit none

    ! return value
    
    logical             :: retVal         ! .true. succes

    ! arguments
    
    type(DioStreamType) :: stream         ! handle to DIO stream
    character(Len=*)    :: itemName       ! empty (for strea), or dataset name
    character(Len=*)    :: availType      ! 'header' or 'data'

    ! locals
    
    character(Len=DioMaxStreamLen) :: dsBaseName ! base name of dataset
    character(Len=DioMaxStreamLen) :: fname      ! vars to open file
    integer                        :: ierr
    logical                        :: exists
    integer                        :: timeWaited
    integer                        :: iunit

    ! body
    
    retVal =.true.

    if ( stream % synched ) then

        call DioSyncMakeBaseName( itemName, dsBaseName )

        call DioStreamDelay
        timeWaited = 0
        fname = trim(stream % name) // '_' // trim(dsBaseName) // availType
10      continue
        inquire(file=fname,exist=exists)
        if (exists) then
#if (defined(WIN32))
            open(newunit=iunit,file=fname,status='old', iostat=ierr, share='DENYRW')
#else
            open(newunit=iunit,file=fname,status='old', iostat=ierr)
#endif
            if (ierr.gt.0) then
                if (DioStreamSleep(timeWaited)) then
                    goto 10
                else
                    retVal =.false.
                endif
            endif
            close(iunit)
        else
            if (DioStreamSleep(timeWaited)) then
                goto 10
            else
                retVal =.false.
            endif
        endif
        call DioStreamDelay
    endif

end function DioSyncGetItemAvailable


subroutine DioSyncSetItemReceived(stream, itemName, availType)

    use dio_streams

    implicit none

    ! arguments
    
    type(DioStreamType) :: stream         ! handle to DIO stream
    character(Len=*)    :: itemName       ! empty (for stream), or dataset name
    character(Len=*)    :: availType      ! 'header' or 'data'

    ! locals
    
    character(Len=DioMaxStreamLen) :: dsBaseName ! base name of dataset
    character(Len=DioMaxStreamLen) :: fname      ! vars to open file
    integer                        :: ierr
    integer                        :: iunit

    ! body
    
    if ( stream % synched ) then

        call DioSyncMakeBaseName( itemName, dsBaseName )

        call DioStreamDelay
        fname = trim(stream % name) // '_' // trim(dsBaseName) // availType
        open(newunit=iunit,file=fname,status='old', iostat=ierr)
        if (ierr.gt.0) then
            call DioStreamError(902, 'Could not open ', trim(fname), ' for deletion')
        else
            close(iunit,status='delete')
            !
            ! clean up stream
            !
            if ( availType .ne. '.stream' .and. DioStreamUsesLun(stream)) then
                fname = stream % name
                open(newunit=iunit,file=fname,status='old', iostat=ierr)
                if (ierr.gt.0) then
                    call DioStreamError(903, 'Could not open ', trim(fname), ' for deletion')
                else
                    close(iunit,status='delete')
                endif
            endif
        endif
        call DioStreamDelay
    endif

end subroutine DioSyncSetItemReceived


