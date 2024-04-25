subroutine prihis(gdp)
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
!    Function: Checks whether the tri-prt file exists and is open.
!              If not, the file is created.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                       , pointer :: lundia
    integer                       , pointer :: lunprt
!
! Global variables
!
!
! Local variables
!
    integer        :: iocond
    integer        :: lrid    ! Help var. to determine the actual length of RUNID 
    integer        :: n
    integer        :: newlun
    integer        :: nrec
    logical        :: ex      ! Help flag = TRUE when file is found 
    logical        :: opend   ! Help flag = TRUE when file is still open (DELFT3D) 
    character(256) :: errmsg  ! String containing error messages 
    character(256) :: filtmp  ! Help var. to specify file name 
!
!! executable statements -------------------------------------------------------
!
    lundia                => gdp%gdinout%lundia
    lunprt                => gdp%gdinout%lunprt
    !
    ! open LUNPRT
    !
    lrid = len(trim(gdp%runid))
    filtmp(1:8 + lrid) = 'tri-prt.' // gdp%runid(1:lrid)
    inquire (file = filtmp(1:8 + lrid), exist = ex)
    !
    ! if the tri-prt file exists an append should be performed
    !
    if (ex) then
       !
       ! if LUNPRT is closed, then re-open file and read to end of
       ! file before appending (presumed is that the lunprt unit
       ! number is coupled to the print file !!)
       !
       inquire (file = filtmp(1:8 + lrid), opened = opend)
       if (.not.opend) then
          open (newunit=lunprt, file = filtmp(1:8 + lrid), form = 'formatted')
          nrec = 0
          ! -->
  210        continue
          nrec = nrec + 1
          read (lunprt, '(a)', iostat = iocond)
          if (iocond==0) goto 210
          ! <--
          !
          ! End-of-file encountered, read till end
          ! and write 1 blank line
          !
          if (iocond<0) then
             nrec = nrec - 1
             rewind (lunprt)
             do n = 1, nrec
                read (lunprt, '(a)')
             enddo
             write (lunprt, '(a)')
          else
             !
             ! Error occured while reading, delete file and re-open
             !
             close (lunprt, status = 'delete')
             open (newunit=lunprt, file = filtmp(1:8 + lrid), form = 'formatted',    &
                 & status = 'new')
          endif
       else
          !
          ! Get unit number of prt file
          !
          inquire (file = filtmp(1:8 + lrid), number = lunprt)
       endif
    else
       open (newunit=lunprt, file = filtmp(1:8 + lrid), form = 'formatted',          &
            & status = 'new')
    endif
end subroutine prihis
