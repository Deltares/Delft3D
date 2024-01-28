function slim(ds1,ds2)
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
! This routine computes the Van Leer limiter value based on the water elevation
! slopes ds1 & ds2.
!
!!--pseudo code and references--------------------------------------------------
!
! G.S. Stelling and S.P.A. Duijnmeijer
!    "A staggered conservative scheme for every Froude number in rapidly varied
!     shallow water flows",
!    Int. J. Numer. Meth. Fluids,  Vol. 43, pp 1329 - 1354
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Function return value
!
    real(fp) :: slim
!
! Global variables
!
    real(fp) :: ds1
    real(fp) :: ds2
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! default value of SLIM = 0.0
    !
    slim = 0.0
!    if (ds1*ds2 > 0.) then
!       slim = ds1*ds2/(ds1+ds2)
!    endif
end function slim
