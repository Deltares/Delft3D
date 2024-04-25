function tapf(tau       )
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
! Taper function used to smoothly start-up the program
! in the time dependent roller case. Also used to
! reduce wave forces near boundaries where bound waves
! should leave the domain.
!
! taper funcion runs from 0 to 1 if tau runs from 0 to 1
! function has all derivatives equal to 0 in tau=0 and tau=1
! and is constant outside the range [0,1].
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp)         :: tapf
    real(fp), intent(in) :: tau
!
! Local variables
!
    real(fp) :: eps
!
!! executable statements -------------------------------------------------------
!
    eps = 1.0E-5
    if (tau<=eps) then
       tapf = 0.0
    elseif (tau>=1.0 - eps) then
       tapf = 1.0
    else
       tapf = 0.5*(1.0 + tanh((tau - 0.5)/(1.0 - tau)/tau))
    endif
end function tapf
