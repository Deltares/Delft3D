subroutine removeDuplicateDDBFiles(runid, ddbfile, gdp)
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
!!--pseudo code and references--------------------------------------------------
!
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat)     , target    :: gdp
    integer           , pointer   :: lundia
!
! Global variables
!
    character(*)                       , intent(in)  :: runid   ! Run identification code for the current simulation
    character(*)                       , intent(in)  :: ddbfile ! Name of freshly created ddbfile
!
! Local variables
!
    integer                              :: fillun
    integer                              :: fillundef
    integer                              :: istat
    integer,external                     :: newlun
    logical                              :: ex
    logical                              :: identicalFiles
    character(300)                       :: line
    character(300)                       :: lineb
    character(300)                       :: message        ! string to pass message
    character(256)                       :: defaultfile
!
!! executable statements -------------------------------------------------------
!
    lundia   => gdp%gdinout%lundia
    !
    defaultfile = trim(runid) // ".ddb"
    inquire (file = trim(defaultfile), exist = ex)
    if (ex) then
       !
       ! Compare defaultfile and ddbfile
       identicalFiles = .true.
       open(newunit=fillun, file=trim(ddbfile), action="READWRITE", iostat = istat)
       if (istat==0) open(newunit=fillundef, file=trim(defaultfile), action="READ", iostat = istat)
       if (istat /= 0) then
          identicalFiles = .false.
          write(message,'(5a)') "Unable to open files """, trim(ddbfile), """ and """, trim(defaultfile), """. Skipping moving."
          call prterr(lundia, 'U190', trim(message))
       else
          istat = 0
          do while (istat == 0)
             read(fillun, '(a)', iostat=istat) line
             if (istat == 0) then
                read(fillundef, '(a)', iostat=istat) lineb
                if (istat == 0) then
                   ! No problem reading a line from both files. Compare them
                   if (line /= lineb) then
                      identicalFiles = .false.
                      exit
                   endif
                else
                   ! Only problem reading default file. Assume the files are different
                   identicalFiles = .false.
                   exit
                endif
             else
                read(fillundef, '(a)', iostat=istat) lineb
                if (istat == 0) then
                   ! Only problem reading ddb file. Assume the files are different
                   identicalFiles = .false.
                   exit
                else
                   ! Problem reading a line in both files. Assume the files are different unless it's EOF
                   if (istat /= -1) then
                      identicalFiles = .false.
                      exit
                   endif
                endif
             endif
          enddo
       endif
       close(fillundef)
       if (identicalFiles) then
          close(fillun, status="delete")
       else
          close(fillun)
       endif
    else
       !
       ! Move ddbfile to defaultfile
       !
       open(newunit=fillun, file=trim(ddbfile), action="READWRITE", iostat = istat)
       if (istat==0) open(newunit=fillundef, file=trim(defaultfile), action="WRITE", iostat = istat)
       if (istat /= 0) then
          write(message,'(5a)') "Unable to open files """, trim(ddbfile), """ and """, trim(defaultfile), """. Skipping moving."
          call prterr(lundia, 'U190', trim(message))
       else
          istat = 0
          do while (istat == 0)
             read(fillun, '(a)', iostat=istat) line
             if (istat == 0) then
                write(fillundef, '(a)') line
             endif
          enddo
       endif
       if (istat == -1) then
          ! eof. Normal end. Remove new file
          close(fillun, status="delete")
       else
          ! Something went wrong. Don't delete the new file
          close(fillun)
       endif
       close(fillundef)
    endif
end subroutine removeDuplicateDDBFiles
