SUBROUTINE xGL_dpL(xG_L,yG_L,xz,yz,dpL,dpH,dps,nmlb,nmub,nmmax) !
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
!   Function: set xG_L=xz,yG_L=yz,dpL=dps
!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
    integer                                                       , intent(in)    :: nmlb
    integer                                                       , intent(in)    :: nmub
    integer                                                       , intent(in)    :: nmmax
    real(fp), dimension(nmlb:nmub)                                , intent(out)   :: xG_L 
    real(fp), dimension(nmlb:nmub)                                , intent(out)   :: yG_L 
    real(fp), dimension(nmlb:nmub)                                , intent(out)   :: dpL 
    real(fp), dimension(nmlb:nmub)                                , intent(out)   :: dpH
    real(fp), dimension(nmlb:nmub)                                , intent(in)    :: xz 
    real(fp), dimension(nmlb:nmub)                                , intent(in)    :: yz
    real(prec), dimension(nmlb:nmub)                              , intent(in)    :: dps

!
! local variables
!
    integer :: nm
!
! executable
!
   !
      do nm = 1,nmmax
!         dpL(nm) =  dps(nm)  !it changes in time, no point to fix it now
!         dpH(nm) =  dps(nm)  !it changes in time, no point to fix it now
         xG_L(nm) = xz(nm) 
         yG_L(nm) = yz(nm) 
      enddo

  return 
end subroutine xGL_dpL
