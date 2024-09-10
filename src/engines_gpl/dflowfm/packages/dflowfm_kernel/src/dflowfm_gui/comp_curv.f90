!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> compute curvature in a point on a spline
subroutine comp_curv(num, xsp, ysp, xsp2, ysp2, s, curv, dnx, dny, dsx, dsy)

   use m_sferic
   use geometry_module, only: dbdistance, getdxdy, normalout
   use m_missing, only: dmiss, dxymis
   use m_splint
   
   implicit none

   integer,                          intent(in)  :: num         !< number of spline control points

   double precision, dimension(num), intent(in)  :: xsp, ysp    !< spline control point coordinates
   double precision, dimension(num), intent(in)  :: xsp2, ysp2  !< spline control point second order derivatives of coordinates
   double precision,                 intent(in)  :: s           !< point on spline in spline-coordinates

   double precision,                 intent(out) :: curv       !< curvature in point on spline
   double precision,                 intent(out) :: dnx, dny   !< normal vector
   double precision,                 intent(out) :: dsx, dsy   !< tangential vector

   double precision                              :: A, B, x, y, xp, yp, xpp, ypp, x1, y1, csy, d1

   integer                                       :: iL, iR

   double precision, parameter                   :: EPS=1d-4
   double precision, external                    :: getdx, getdy

   iL = max(min(int(s)+1,num-1),1)
   iR = max(iL+1,1)

   A  = dble(iR-1) - s
   B  = s - dble(iL-1)

   call splint(xsp,xsp2,num,s,x)
   call splint(ysp,ysp2,num,s,y)

   xp = -xsp(iL) + xsp(iR) + ( (-3d0*A**2 + 1d0)*xsp2(iL) + (3d0*B**2-1d0)*xsp2(iR) )/6d0
   yp = -ysp(iL) + ysp(iR) + ( (-3d0*A**2 + 1d0)*ysp2(iL) + (3d0*B**2-1d0)*ysp2(iR) )/6d0

   xpp = A*xsp2(iL) + B*xsp2(iR)
   ypp = A*ysp2(iL) + B*ysp2(iR)

   if (jsferic == 1) then
      csy = cos(dg2rd*y)
      xp  = xp  * dg2rd*Ra*csy
      xpp = xpp * dg2rd*Ra*csy
      yp  = yp  * dg2rd*Ra
      ypp = ypp * dg2rd*Ra
   end if

   curv = abs(xpp*yp-ypp*xp) / (xp**2+yp**2+1d-8)**1.5

   x1 = x+EPS*xp
   y1 = y+EPS*yp
   call normalout(x,y,x1,y1,dnx,dny, jsferic, jasfer3D, dmiss, dxymis)

   d1  = dbdistance(x,y,x1,y1,jsferic, jasfer3D, dmiss)
   call getdxdy(x,y,x1,y1,dsx,dsy,jsferic)
   dsx = dsx/d1
   dsy = dsy/d1
   return
end subroutine comp_curv
