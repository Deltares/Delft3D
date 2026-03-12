subroutine convert_cart_tofrom_naut(source_array, array_size, northdir, target_array)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
! Convert array source_array, containing angles (degrees) from Cartesian
! convention to Nautical convention or vice versa.
! Cartesian: 0 degrees is towards the east and 90 degrees towards the north
! Nautical : 0 degrees is from the north and 90 degrees from the east
!
! cart => naut: naut = 180.0 + northdir - cart
! naut => cart: cart = 180.0 + northdir - naut
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Parameters
!
    real, dimension(*), intent(in)  :: source_array ! May be any dimensional array
    integer           , intent(in)  :: array_size    ! Number of elements in source_array and target_array
    real              , intent(in)  :: northdir     ! direction of north in degrees (clockwise from east)
    real, dimension(*), intent(out) :: target_array ! Must have the same dimensions as source_array
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
      target_array(:array_size) = 180.0 + northdir - source_array(:array_size)
      where (target_array(:array_size) < 0.0) target_array(:array_size) = target_array(:array_size) + 360.0
      where (target_array(:array_size) >= 360.0) target_array(:array_size) = target_array(:array_size) - 360.0
end subroutine convert_cart_tofrom_naut
