function redvic (vicww, gdp)
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
! Default: 
!    vicww = max (vicww , vicoww)
!
! Low Reynolds damping switched on:
!    vicww = f(damp) * vicww
!
!!--pseudo code and references--------------------------------------------------
!
! zheng.wang@deltares.nl
!
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
!
! Return value
!
real(fp) :: redvic
!
! Global variables
!
real(fp), intent(in)  :: vicww
!
! Local variables
!
real(fp) :: arg
!
!! executable statements -------------------------------------------------------
!
if (gdp%gdprocs%lrdamp) then
   arg = gdp%gdturcoe%lrdamp_fac * vicww
   if (arg < 3.0_fp) then
      redvic = (1.0_fp - exp(-arg*arg)) * vicww
   else
      redvic = max(vicww , gdp%gdphysco%vicoww)
   endif
else
   redvic = max(vicww , gdp%gdphysco%vicoww)
endif
end function redvic
