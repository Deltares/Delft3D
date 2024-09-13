subroutine adjacent(L,  m,  n,  mAD,  nAD,  Lad) 
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   determine the cell adjacent to cell (n,m) on the edge L
!
!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! local variables
!
  integer, intent(in)         :: L
  integer, intent(in)         :: m
  integer, intent(in)         :: n
  integer, intent(out)        :: mAD   
  integer, intent(out)        :: nAD
  integer, intent(out)        :: Lad
!
! executable statements -------------------------------------------------------
!   
    if (L.eq.1) then
      nAD = n-1
      mAD = m
      Lad = 3 
    elseif (L.eq.2) then
      nAD = n
      mAD = m+1
      Lad = 4 
    elseif (L.eq.3) then
      nAD = n+1
      mAD = m
      Lad = 1 
    else
      nAD = n
      mAD = m-1
      Lad = 2       
    endif
!
end subroutine adjacent