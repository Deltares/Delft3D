subroutine dimstr(lunmd     ,filnam    ,lundia    ,error     ,nrrec     , &
                & nodim     ,gdp       )
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
!    Function: Reads the number of U- and V-Barriers from the
!              attribute file.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer      :: lundia ! Description and declaration in inout.igs
    integer      :: lunmd  ! Description and declaration in inout.igs
    integer      :: nrrec  ! Record counter keeping the track of the last record read
    integer      :: nodim  ! nsluv
    logical      :: error  ! Flag=TRUE if an error is encountered
    character(*) :: filnam
!
! Local variables
!
    integer           :: iocond  ! Reading condition, should be 0 
    integer           :: lfile   ! Number of non blank characters of file name 
    integer           :: luntmp  ! Unit number of FILTMP 
    integer, external :: newlun
    character(132)    :: rec132
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialize formal parameters
    !
    nodim = 0
    if (filnam == ' ') then
       goto 9999
    endif
    !
    ! Barrier / Gate file name specified; Test file existence
    !
    call remove_leading_spaces(filnam    ,lfile     )
    error = .not.exifil(filnam, lundia)
    if (error) goto 9999
    !
    ! open formatted file, if not formatted IOCOND <> 0
    !
    open (newunit=luntmp, file = filnam(:lfile), form = 'formatted',              &
        & status = 'old', iostat = iocond)
    if (iocond/=0) then
       error = .true.
       call prterr(lundia    ,'G007'    ,filnam(:lfile)       )
       goto 9999
    endif
    !
    ! freeformatted file, skip lines starting with a '*'
    !
    call skipstarlines(luntmp    )
    !
    ! freeformatted file, read input and test iocond
    !
    !--->
  100     continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond>0) then
       !
       ! Error
       !
       call prterr(lundia    ,'G007'    ,filnam(:lfile)       )
       error = .true.
       close (luntmp)
       goto 9999
    elseif (iocond<0) then
       !
       ! EOF found
       !
       !  <---
       close (luntmp)
       goto 9999
    else
       ! nothing
    endif
    !
    ! Only count non-empty records
    !
    if (rec132/=' ') then
       nodim = nodim + 1
    endif
    !
    ! Read next record
    !
    goto 100
    !
 9999 continue
end subroutine dimstr
