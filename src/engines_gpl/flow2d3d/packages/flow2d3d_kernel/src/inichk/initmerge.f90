subroutine initmerge (nmmax, lsed, runid, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!!--description-----------------------------------------------------------------
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: lundia
    integer                              , pointer :: mergehandle
    real(hp)              , dimension(:) , pointer :: mergebuf
    character(256)                       , pointer :: mmsyncfilnam
    type (morpar_type)                   , pointer :: gdmorpar
!
! Global variables
!
    integer        :: nmmax
    integer        :: lsed
    character(*)   :: runid
!
! Local variables
!
    integer                :: conditionend
    integer                :: i
    integer                :: istat
    integer, external      :: getstream
    integer                :: lunfil
    integer, external      :: newlun
    integer                :: pathlen
    real(hp), dimension(2) :: rn
    logical                :: ex
    character(1)           :: slash
    character(256)         :: condition
    character(256)         :: streamfile
    character(256)         :: filhand
!
!! executable statements -------------------------------------------------------
!
    lundia              => gdp%gdinout%lundia
    mergehandle         => gdp%gdmorpar%mergehandle
    mergebuf            => gdp%gdmorpar%mergebuf
    mmsyncfilnam        => gdp%gdmorpar%mmsyncfilnam
    gdmorpar            => gdp%gdmorpar
    !
    write(*,'(10x,a)')'- Waiting for connection with mormerge...'
    filhand = ' '
    inquire(file='streamfile', exist=ex)
    if (ex) then
       open (newunit=lunfil, file='streamfile')      
       read (lunfil,'(a)') filhand
       close(lunfil)
       write(filhand,'(2a)') trim(filhand), trim(runid)
       !
       ! filhand is assumed to be:
       ! <path><condition>stream<runid>
       ! mmsync file name is going to be:
       ! <path>/sync/<condition>flow<runid>
       !
       if (gdp%arch == 'win32' .or. gdp%arch == 'win64') then
          slash = '\'
          !
          ! In filhand: replace all occurences of / by \
          ! Needed for further parsing
          !
          do i=1,len(filhand)
             if (filhand(i:i) == '/') then
                filhand(i:i) = '\'
             endif
          enddo
       else
          slash = '/'
          !
          ! In filhand: replace all occurences of \ by /
          ! Needed for further parsing
          !
          do i=1,len(filhand)
             if (filhand(i:i) == '\') then
                filhand(i:i) = '/'
             endif
          enddo
       endif
       pathlen = len_trim(filhand)
       do while ( filhand(pathlen:pathlen) /= slash .and. pathlen>1)
          pathlen = pathlen - 1
       enddo
       conditionend = index(filhand, 'stream', .true.)
       !
       ! The position of the first character of the word 'stream' (s),
       ! behind the condition should be at least pathlen+2
       !
       if (conditionend < pathlen+2) conditionend = pathlen + 4
       condition = filhand(pathlen+1 : conditionend-1)
       write(mmsyncfilnam,'(6a)') filhand(:pathlen), 'sync', slash, &
                               & trim(condition)  , 'flow', trim(runid)
       open (newunit=lunfil, file=mmsyncfilnam, position='append', action='write', iostat=istat)
       if (istat /= 0) then
          write(*,*)' *** WARNING: unable to write in file ',trim(mmsyncfilnam)
       else
          write(lunfil,'(a)') 'Initialized'
          close(lunfil)
       endif
       !
       ! This is the actual connection with mormerge
       !
       mergehandle = getstream(filhand)
    else
       call prterr(lundia, 'U021', 'File named streamfile not found')
       call d3stop(1,gdp)
    endif
    rn(1) = nmmax * 1.0_hp
    rn(2) = lsed  * 1.0_hp
    ! write(*,*)nmmax, lsed, rn
    call putarray(mergehandle, rn, 2)
    !
    ! allocate buffer array 
    !
    allocate (gdp%gdmorpar%mergebuf(nmmax*lsed), stat = istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'Initmerge: memory alloc error')
       call d3stop(1,gdp)
    endif
end subroutine initmerge
