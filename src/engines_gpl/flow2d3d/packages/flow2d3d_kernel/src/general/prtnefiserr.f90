subroutine prtnefiserr(message, gdp)
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
!    Function: This routine prints a nefis message to output devices.
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
    integer , pointer :: lundia
!
! Local parameters
!
    integer, parameter :: meslen = 1024
!
! Global variables
!
    character(*), intent(in)  :: message
!
! Local variables
!
    integer           :: i
    integer           :: ierror
    integer, external :: neferr
    character(meslen) :: nefismessage
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    ierror = neferr(0,nefismessage)
    !
    ! remove the last '\n' of the nefismessage
    do i=meslen, 1, -1
       if (ichar(nefismessage(i:i)) == 10) then
          nefismessage(i:i) = ' '
          exit
       endif
    enddo
    write(*,*)
    write(lundia,*)
    write(*,'(2x,a,a)')      'Nefis error:', trim(nefismessage)
    write(lundia,'(2x,a,a)') 'Nefis error:', trim(nefismessage)
    write(*,'(2x,a)')      trim(message)
    write(lundia,'(2x,a)') trim(message)
    write(*,*)
    write(lundia,*)
end subroutine prtnefiserr
