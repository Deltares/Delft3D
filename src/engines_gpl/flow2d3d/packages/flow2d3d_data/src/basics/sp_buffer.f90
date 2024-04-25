module sp_buffer
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
implicit none

real(sp), allocatable, dimension(:),save :: sbuff    !  Single precision buffer to read from
                                                     !  or to write to, depending on the write
                                                     !  switch (WRILOG).
integer                            ,save :: sbufflen ! length of allocated array sbuff

contains

subroutine sbuff_init ()
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    sbufflen = 0
    deallocate (sbuff, STAT = istat)
end subroutine sbuff_init

subroutine sbuff_checksize (lenused)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer :: lenused
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    if (lenused > sbufflen) then
       if (sbufflen > 0) deallocate (sbuff, stat = istat)
       sbufflen = lenused
       allocate(sbuff(sbufflen))
    endif
end subroutine sbuff_checksize

subroutine sbuff_dealloc ()
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local variables
!
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    ! The check on sbufflen and setting it on zero is needed
    ! Otherwise a crash will occur in multi threaded mode, because
    ! sbuff is then deallocated more than once
    !
    if (sbufflen > 0) then
       deallocate (sbuff, stat = istat)
       sbufflen = 0
    endif
end subroutine sbuff_dealloc

end module sp_buffer
