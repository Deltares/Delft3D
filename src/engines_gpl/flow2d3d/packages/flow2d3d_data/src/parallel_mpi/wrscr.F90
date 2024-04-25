subroutine wrscr ( tekst, gdp )
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
!   Writes line to screen done by master i.c. of parallel Delft3D-FLOW
!
!!--pseudo code and references--------------------------------------------------
!
!
!!--declarations----------------------------------------------------------------
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: lunscr
!
! Global variables
!
    character(*), intent(in) :: tekst ! text to be put on screen
!
!! executable statements -------------------------------------------------------
!
    lunscr => gdp%gdinout%lunscr
    !
    if ( inode == master ) then
       if ( len_trim(tekst) /= 0 ) then
          write (lunscr,'(a)') trim(tekst)
       else
          write (lunscr,*)
       endif
    endif

end subroutine wrscr
