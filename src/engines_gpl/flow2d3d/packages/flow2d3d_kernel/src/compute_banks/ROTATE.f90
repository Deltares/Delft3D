subroutine ROTATE(vx,vy,nx,ny,N,resx,resy)
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
!  $Id:  .f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   rotate the vector (vx,vy) in the direction of the versor (i.e. unit vector
!               (nx,ny)
!               
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
  REAL(fp), intent(in)       :: vx(*),vy(*) 
  REAL(fp), intent(in)       :: nx,ny 
  REAL(fp), intent(out)      :: resx(*),resy(*)
  INTEGER , intent(in)       :: N
!
! local variables
!
  INTEGER                    :: I
!
!
! executable statements -------------------------------------------------------
!    
    DO I=1,N
       resx(I) =  vx(I)*nx-vy(I)*ny
       resy(I) =  vx(I)*ny+vy(I)*nx
    ENDDO
!
RETURN
END