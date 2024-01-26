function termfy(n         ,y         ,z         )
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
! determines termfy
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
    real(hp)              :: termfy
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
       termfy = ( - 1.d0)**(n + 1)*z*log(y)
    elseif (n==1) then
       termfy = y**(1.d0 - z)/(1.d0 - z)
    else
       teller = 1.d0
       noemer = 1.
       do i = 2, n
          teller = teller*(z - real(i, hp) + 2.D0)
          noemer = noemer*real(i, fp)
       enddo
       noemer = noemer/real(n, fp)
       termfy = ( - 1.d0)**(n + 1)                                              &
              & *((teller*y**(real(n, hp) - z))/(noemer*(real(n, hp) - z)))
    endif
end function termfy
