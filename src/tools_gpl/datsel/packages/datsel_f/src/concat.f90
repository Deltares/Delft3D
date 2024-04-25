function concat(txta      ,txtb      )
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
! concatenuates two strings to one string
! without blanks
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    character(*)    :: concat
    character(*), intent(in)       :: txta
    character(*), intent(in)       :: txtb
!
!
! Local variables
!
    integer                        :: i                    !      counting integer
    integer                        :: l                    ! the number of the quantity
    integer                        :: ltxta                ! namitq within the nefis group length of non blank name string txta
    integer                        :: ltxtb                ! length of non blank name string txtb
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    ltxta = index(txta, ' ')
    ltxta = ltxta - 1 + (1 - min(ltxta, 1))*(len(txta) + 1)
    ltxtb = len(txtb)
    ! --- do while ----->
    i = 0
   10 continue
    i = i + 1
    l = index(txtb(i:i), ' ')
    if (l/=0 .and. i<ltxtb) goto 10
    ! <-- end while -----
    !
    if (ltxta>0) then
       concat = txta(:ltxta) // txtb(i:)
    else
       concat = txtb(i:)
    endif
end function concat
