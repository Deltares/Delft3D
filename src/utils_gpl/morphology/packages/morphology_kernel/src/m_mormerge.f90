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
module m_mormerge

contains

!> initilize mormerge
subroutine initialize_mormerge (iresult, nmmax, lsed, runidIn, gdmorpar)
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
    use morphology_data_module, only: morpar_type
!
! Global variables
!
    integer        ,intent(out) :: iresult
    integer                     :: nmmax
    integer                     :: lsed
    character(*)                :: runidIn
    type (morpar_type), pointer :: gdmorpar
!
! Local variables
!
    integer                                        :: conditionend
    integer                                        :: i
    integer                                        :: istat
    integer, external                              :: getstream
    integer                                        :: lunfil
    integer, external                              :: newlun
    integer                                        :: pathlen
    real(hp), dimension(2)                         :: rn
    logical                                        :: ex
    character(1)                                   :: slash
    character(1)                                   :: otherslash
    character(256)                                 :: condition
    character(256)                                 :: streamfile
    character(256)                                 :: filhand
    character(256)                                 :: runid
    character(256)                       , pointer :: mmsyncfilnam
    character(256)                                 :: tmpstring
!
!! executable statements -------------------------------------------------------
!
    iresult = 0
    runid   = runidIn
    !
    mmsyncfilnam        => gdmorpar%mmsyncfilnam
    !
    write(*,'(10x,a)')'- Waiting for connection with mormerge...'
    filhand = ' '
    streamfile = 'streamfile'
    inquire(file=trim(streamfile), exist=ex)
    if (.not. ex) then
       streamfile = '../streamfile'
       inquire(file=trim(streamfile), exist=ex)
    endif
    if (ex) then
       open (newunit=lunfil, file=trim(streamfile))      
       read (lunfil,'(a)') filhand
       close(lunfil)
       write(tmpstring,'(2a)') trim(filhand), trim(runid)
       filhand = tmpstring
       !
       ! filhand is assumed to be:
       ! <path><condition>stream<runid>
       ! mmsync file name is going to be:
       ! <path>/sync/<condition>flow<runid>
       !
       slash = get_dirsep()
       if (slash == '\') then
          otherslash = '/'
       else
          otherslash = '\'
       endif
       !
       ! In filhand: replace all occurences of otherslash by slash
       ! Needed for further parsing
       !
       do i=1,len(filhand)
          if (filhand(i:i) == otherslash) then
             filhand(i:i) = slash
          endif
       enddo
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
       gdmorpar%mergehandle = getstream(filhand)
    else
       write(*,*) 'ERROR: File named "streamfile" or "../streamfile" not found'
       iresult = 1
       return
    endif
    rn(1) = nmmax * 1.0_hp
    rn(2) = lsed  * 1.0_hp
    call putarray(gdmorpar%mergehandle, rn, 2)
    !
end subroutine initialize_mormerge

!> send to stream and receives back time step
subroutine put_get_time_step(mergehandle, time_step)
    use precision
   implicit none

   integer,          intent(in)           :: mergehandle !<  stream handle for communication with mormerge
   double precision, intent(inout)        :: time_step   !< User specified or internal time step (s) for external forcing update.

   real(hp),          parameter           :: DIM_REAL = 1_hp
   
   call putarray (mergehandle, DIM_REAL, 1)
   call putarray (mergehandle, time_step, 1)
   call getarray (mergehandle, time_step, 1)
   
end subroutine put_get_time_step

!> put merge buffer to mormerge then gets it back
subroutine put_get_mergebuffer(mergehandle, buffer_size, mergebuffer)
   use precision
   implicit none

   integer,          intent(in)      :: mergehandle              !<  stream handle for communication with mormerge
   integer        ,  intent(in)      :: buffer_size              !< size of merge buffer
   real(hp)       ,  intent(inout)   :: mergebuffer(buffer_size) !< a buffer for mormerge data 

   call putarray (mergehandle, real(buffer_size,hp), 1)
   call putarray (mergehandle, mergebuffer, buffer_size)
   call getarray (mergehandle, mergebuffer, buffer_size)
    
end subroutine put_get_mergebuffer
            
end module m_mormerge
