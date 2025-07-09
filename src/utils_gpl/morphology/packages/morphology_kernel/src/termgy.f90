function termgy(n         ,y         ,z         )
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
! determines termgy
! a contribution to the bijker transport
! relation
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Arguments
!
    integer, intent(in)   :: n
    real(hp)              :: termgy
    real(hp), intent(in)  :: y
    real(hp), intent(in)  :: z
!
! Local variables
!
    integer  :: i      ! running index
    real(fp) :: noemer
    real(hp) :: teller
!
!! executable statements -------------------------------------------------------
!
    if (abs(real(n, hp) - z)<=1.d-10) then
       termgy = ( - 1.d0)**(n + 1)*z*log(y)*log(y)*.5D0
    elseif (n==1) then
       termgy = y**(1.d0 - z)                                                   &
              & *(((1.d0 - z)*log(y) - 1.d0)/((1.d0 - z)*(1.d0 - z)))
    else
       teller = 1.d0
       noemer = 1.
       do i = 2, n
          teller = teller*(z - real(i, hp) + 2.d0)
          noemer = noemer*real(i, fp)
       enddo
       noemer = noemer/real(n, fp)
       termgy = ( - 1.d0)**(n + 1)*((teller*y**(real(n, hp) - z))/real(noemer, hp))     &
              & *(((real(n, hp) - z)*log(y) - 1.d0)/((real(n, hp) - z)*(real(n, hp) - z)))
    endif
end function termgy
