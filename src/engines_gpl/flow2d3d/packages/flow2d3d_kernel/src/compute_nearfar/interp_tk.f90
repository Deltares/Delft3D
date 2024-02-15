subroutine interp_tk  (x    ,y     ,noval ,xx    ,yy    )
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
!    Function: Interpolate to the y-value at xx
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!

    use precision
    !
    implicit none
!
! Global variables
!
    integer ,                   intent (in)   ::   noval
    real(fp), dimension(noval), intent (in)   ::   x
    real(fp), dimension(noval), intent (in)   ::   y 
    real(fp),                   intent (in)   ::   xx
    real(fp),                   intent (out)  ::   yy
!
! Local variables
!
    integer    ::   n
!
!! executable statements -------------------------------------------------------
!
    if (xx <= x(1)) then
       yy = y(1)
    elseif (xx >= x(noval)) then
       yy = y(noval)
    else
       do n = 2, noval
          if (xx >= x(n-1) .and. xx < x(n)) then
             yy = y(n-1) + ((xx - x(n-1))/(x(n) - x(n-1)))*(y(n) - y(n-1))
          endif
       enddo
    endif
end subroutine interp_tk
