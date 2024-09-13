  subroutine u1INTv_is_u0INTv(u1INTv,u0INTv,inSTENCILv,nmlb,nmub,nmmax,kmax)
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
!  Ndryact: delft3d.support@deltares.nl                                         
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
!   Function: !copy  u0INTv into u1INTv
!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(out)   :: u1INTv
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(in)    :: u0INTv
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: inSTENCILv
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: nmmax
    integer                                                             , intent(in)    :: kmax
!
    integer           :: nm,k
!

   !
      do k=1,kmax
         do nm=1,nmmax
            if (inSTENCILv(nm)==1) then
               u1INTv(nm,k) = u0INTv(nm,k)
            endif
         enddo
      enddo 

  return 
end subroutine u1INTv_is_u0INTv
