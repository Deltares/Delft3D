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
!!! DIO-sem-support: semaphore support functions
!!!
!!! (c) Deltares, dec 2000
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!
!! Support Functions called from dio-sem
!!

!
! Check if an item has been provided by a sender
!

subroutine DioSemGetLock(name)

    use dio_streams

    implicit none


    ! arguments
    
    character(Len=*)              :: name        ! lock name

    ! locals
    
    character(Len=DioMaxStreamLen):: fname  ! vars to check file existence
    integer                       :: ierr
    logical                       :: exists
    integer                       :: timeWaited
    logical                       :: succes
    
    integer                       :: iunit

    ! body
    
    succes =.true.

    timeWaited = 0
    fname = trim(name) // '.lock'
10  continue
    inquire(file=fname,exist=exists)
    if (exists) then
        if (DioStreamSleep(timeWaited)) then
            goto 10
        else
            succes =.false.
        endif
    else
#if (defined(WIN32))
        open(newunit=iunit,file=fname,status='new', iostat=ierr, share='DENYRW')
#else
        open(newunit=iunit,file=fname,status='new', iostat=ierr)
#endif
        if (ierr.gt.0) then
            if (DioStreamSleep(timeWaited)) then
                goto 10
            else
                succes =.false.
            endif
        endif
        close(iunit)
    endif

end subroutine DioSemGetLock


subroutine DioSemReleaseLock(name)

    use dio_streams

    implicit none

    ! arguments
    
    character(Len=*)              :: name   ! lock name

    ! locals
    
    character(Len=DioMaxStreamLen):: fname  ! vars to check file existence
    integer                       :: ierr
    integer                       :: iunit  

    ! body
    
    fname = trim(name) // '.lock'
    open(newunit=iunit,file=fname,status='old', iostat=ierr)
    if (ierr.gt.0) then
        call DioStreamError(991, 'Could not open sem-file', trim(fname), ' for deletion')
    else
        close(iunit,status='delete')
    endif

end subroutine DioSemReleaseLock


