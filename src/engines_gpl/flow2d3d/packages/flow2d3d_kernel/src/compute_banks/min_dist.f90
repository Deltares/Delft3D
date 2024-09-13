function min_dist(v1,v2,p,typeDIST) result(dist)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!  $Id: min_dist.f90 6021 2016-04-15 14:20:51Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/min_dist.f90 $
!!--description-----------------------------------------------------------------
!
! Return minimum distance between line segment v1-v2 and point p
! If typeDIST==1 it finds the minimum distance to the segment (non necessarily normal)
! If typeDIST==2 it finds the minimum distance to the line (always normal)
!
! Author: Alberto Canestrelli
!
    use precision
    !
    implicit none
    !
! return value
!
    real(fp)  :: dist  !  
!
! arguments
!
    real(fp), dimension(2), intent(in)    :: v1
    real(fp), dimension(2), intent(in)    :: v2
    real(fp), dimension(2), intent(inout) :: p
    integer               , intent(in)    :: typeDIST
!
! local variables
!
    real(fp)   :: dot
    real(fp)   :: t
    real(fp)   :: len2
    real(fp), dimension(2) :: proj
! executable part
! 
  len2 = (v1(1)-v2(1))**2 + (v1(2)-v2(2))**2 ! i.e. |v2-v1|^2 -  avoid a sqrt
  if (comparereal(len2,0._fp).eq.0) then
     dist = sqrt((v1(1)-p(1))**2 + (v1(2)-p(2))**2) ! v1 == v2 case
     return 
  endif
  ! Consider the line extending the segment, parameterized as v1 + t (v2 - v1).
  ! We find projection of point p onto the line. 
  ! It falls where t = [(p-v1) . (v2-v1)] / |v2-v1|^2
  dot = (p(1)-v1(1))*(v2(1)-v1(1)) + (p(2)-v1(2))*(v2(2)-v1(2))
  t = dot / len2 
  if (typeDIST==1) then
     if (comparereal(t,0._fp).le.0) then 
        dist = sqrt((v1(1)-p(1))**2 + (v1(2)-p(2))**2)  !Beyond the 'v1' end of the segment
     else if   (comparereal(t,1._fp).gt.0) then  
        dist = sqrt((v2(1)-p(1))**2 + (v2(2)-p(2))**2)  ! Beyond the 'v2' end of the segment
     else
        proj = v1 + t * (v2 - v1)  ! Projection falls on the segment
        dist = sqrt((proj(1)-p(1))**2 + (proj(2)-p(2))**2)
     endif
  elseif (typeDIST==2) then !Always the normal
     proj = v1 + t * (v2 - v1)  ! Projection falls on the segment
     dist = sqrt((proj(1)-p(1))**2 + (proj(2)-p(2))**2)
  else
     write(*,*) 'Error: typeDIST has to be 1 or 2 '
     !pause
     stop
  endif


 return
end function min_dist
