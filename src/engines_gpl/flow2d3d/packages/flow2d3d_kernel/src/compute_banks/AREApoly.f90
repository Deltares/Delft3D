subroutine AREApoly(X,Y,NVERT,area) 
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
!   Function:   compute the area of a simple (i.e. not intersecting) polygon (can be concave or convex),
!               Note: the first vertex has to be coincident with the last 
!
!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
  integer, intent(in)        :: nVERT
  real(fp), intent(in)       :: X(nVERT) 
  real(fp), intent(in)       :: Y(nVERT)
  real(fp), intent(out)      :: area
!
! local variables
!
!
  integer                    :: i
!
!
! executable statements -------------------------------------------------------
!
    area = 0.0_fp
    do i=1,nVERT-1
       area = area + X(i)*Y(i+1)-X(i+1)*Y(i)
    enddo
    area = area*0.5_fp
    !
end subroutine AREApoly