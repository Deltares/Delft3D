function readn(n, u)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! determines whether two or three items
! are present on a record. the items are
! separated by blanks only.
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    integer, parameter :: zinlen = 256
!
! Global variables
!
    integer, intent(in)  :: n
    integer, intent(in)  :: u
    logical              :: readn
!
! Local variables
!
    integer            :: i        ! counting integer
    integer            :: nb
    integer            :: nbold
    integer            :: nt
    character(zinlen)  :: azin
!
!! executable statements -------------------------------------------------------
!
    read (u, '(A)') azin
    nbold = 1
    nt = 0
    do i = 1, zinlen
       nb = 0
       if (azin(i:i)==' ') nb = 1
       if (nb==0 .and. nbold==1) nt = nt + 1
       nbold = nb
    enddo
    if (nt==n) then
       readn = .true.
    else
       readn = .false.
    endif
    backspace u
end function readn
