subroutine del_temp_files(n_swan_grids)
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
    use swan_flow_grid_maps
    use swan_input
    !
    implicit none
!
! Local parameters
!
    integer, parameter :: numtemp = 7
!
! Global variables
!
    integer         :: n_swan_grids
!
! Local variables
!
    integer           :: i
    integer           :: igrid
    integer           :: inest
    integer           :: istat
    integer           :: itide
    integer           :: fillun
    integer           :: numtempgrid
    logical           :: ex
    character(256)    :: filnam
    character(256),dimension(numtemp) :: tmpfiles
!
!! executable statements -------------------------------------------------------
!
    numtempgrid = 3
    tmpfiles(1) = 'NEST'
    tmpfiles(2) = 'SWANIN_NGRID'
    tmpfiles(3) = 'NGRID'
    do igrid=1,n_swan_grids
       !
       ! Remove temporary swan grid files
       !
       filnam = swan_grids(igrid)%tmp_name
       inquire (file = trim(filnam), exist = ex, iostat = istat)
       if (istat==0 .and. ex) then
          open (newunit=fillun, file=trim(filnam), status='unknown')
          close(fillun, status='delete')
       endif
       !
       ! Remove tempgrid<igrid> files, created by WAVE for nesting
       ! tempgrid001 will not exist (only tempgrid002, tempgrid003, etc.)
       ! But it will not harm when checking/deleting it
       ! 
       do i=1,numtempgrid
          write (filnam,'(a,i3.3)') trim(tmpfiles(i)), igrid
          inquire (file = trim(filnam), exist = ex, iostat = istat)
          if (istat==0 .and. ex) then
             open (newunit=fillun, file=trim(filnam), status='unknown')
             close(fillun, status='delete')
          endif
       enddo
    enddo
    !
    ! Remove hot files
    !
    if (comparereal(swan_run%int2keephotfile,0.0)==1) then
       !
       ! The user wants to keep some hotfiles. Do not remove the last ones.
       !
    else
       do igrid = 1, n_swan_grids
          write (filnam,'(a,i0,5a)') 'hot_', igrid, '_', trim(swan_run%writehottime(1:8)), '_', trim(swan_run%writehottime(10:15)), '.nc'
          inquire (file = trim(filnam), exist = ex, iostat = istat)
          if (istat==0 .and. ex) then
             open (newunit=fillun, file = trim(filnam))
             close (fillun, status = 'delete')
          endif
       enddo
    endif
    !
    ! Remove all known temporary files
    !
    tmpfiles(1) = 'TMP_write_wavm'
    tmpfiles(3) = 'norm_end'
    tmpfiles(4) = 'BOTNOW'
    tmpfiles(5) = 'CURNOW'
    tmpfiles(6) = 'WNDNOW'
    tmpfiles(7) = 'MUDNOW'
    do i=1,numtemp
       filnam = tmpfiles(i)
       inquire (file = trim(filnam), exist = ex, iostat = istat)
       if (istat==0 .and. ex) then
         open (newunit=fillun, file=trim(filnam), status='unknown')
         close(fillun, status='delete')
       endif
    enddo
end subroutine del_temp_files
