subroutine My_intersec_circle( qx     ,qy     ,q2x     ,q2y    , &
                               x_c    ,y_c    ,R       ,xi     ,yi     ,typeINTER       )

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
!   Function:   computes the intersection point(s) (Xi,Yi) of the line segment
!               (qx,qy)-(q2x,q2y) with a circle of radius R and center (x_c,y_c).
!               If there is one intersection  point typeINTER=1 and Xi and Yi contain the x- and y-coordinate of this 
!               point, respectively. If there is 2 intersection (can be coincident) typeINTER=2 and the 1x2 
!               vectors Xi and Yi contain the x- and y-coordinates of the begin and 
!               end point (in random order) of the intersection part. If there is no
!               intersection typeINTER=0 and Xi=HUGE and Yi=HUGE. 
!               If the intersection is outside the segments, it returns typeINTER = 3
!
!   Algorithm: http://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm

!
!   The two lines below intersect in i if we can find t and u such that:  p + t r = q + u s .
!
!           p+r \    / q+s                     with  s=(sx,sy) ; r=(rx,ry) 
!                \  /
!                 \/ i=(xi,yi)
!                 /\                          Solution:
!                /  \                         t = (q - p) x s / (r x s)      (x is a two-dimensional cross-product)
!    q=(qx,qy)  /    \ p=(px,py)              u = (q - p) x r / (r x s)           
!
!case 1) if r x s = 0 then the two lines are parallel. There are two subcases:
!        1a) if (q - p) x r = 0 too, then the lines are collinear
!        1b) otherwise: they never intersect. 
!case 2) Otherwise the intersection point is on the original pair of line segments if 0 =< t =< 1 and 0 =< u =< 1.
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
  integer, intent(out)        :: typeINTER
  real(fp), intent(in)        :: qx     ,qy     ,q2x     ,q2y     ,x_c     ,y_c    ,R
  real(fp), dimension(2), intent(out) :: xi       ! x-coordinates of intersection point(s)
  real(fp), dimension(2), intent(out) :: yi       ! y-coordinates of intersection point(s)
!
! local variables
!
  real(fp), parameter         :: zero=0.0_fp  
  real(fp), parameter         :: one=1.0_fp  
  real(fp), parameter         :: hugeNUM=HUGE(1.0_fp) 
!
  integer                     :: ind(4)
  real(fp)                    :: dx,dy,fx,fy,a,b,c,discriminant,t1,t2
!
! executable statements -------------------------------------------------------
!           
    dx = q2x - qx   
    dy = q2y - qy
 
    fx = qx - x_c
    fy = qy - y_c   

    a = dx**2+dy**2
    b = 2._fp*(fx*dx+fy*dy)
    c = (fx**2+fy**2) - r*r ;

    discriminant = b*b-4._fp*a*c;
    if( discriminant < 0 ) then !no intersection
    
       typeINTER=0 
       xi(:) = hugeNUM
       yi(:) = hugeNUM
    
    else
      ! ray (i.e. infinite line) didn't totally miss sphere,
      ! so there is a solution to the equation.

       discriminant = sqrt( discriminant );
      ! either solution may be on or off the ray so need to test both
      ! t1 is always the smaller value, because BOTH discriminant and
      ! a are nonnegative.
       t1 = (-b - discriminant)/(2._fp*a)
       t2 = (-b + discriminant)/(2._fp*a)

      ! three possible HIT cases:
      !          -o->             --|-->  |            |  --|->
      ! Impale(t1 hit,t2 hit), Poke(t1 hit,t2>1), ExitWound(t1<0, t2 hit), 
      ! 
      ! three possible MISS cases:
      !       ->  o                     o ->              | -> |
      ! FallShort (t1>1,t2>1), Past (t1<0,t2<0), CompletelyInside(t1<0, t2>1)

       if ( comparereal(t1,0._fp) >=0 .and. comparereal(t1,1._fp) <= 0 ) then
       
         ! t1 is an  intersection, and it's closer than t2 (since t1 uses -b - discriminant)
         ! I can have Impale or Poke
          if ( comparereal(t2,1._fp) <= 0 ) then  
             !impale
             xi(1) = qx + t1*dx
             yi(1) = qy + t1*dy  
             xi(2) = qx + t2*dx
             yi(2) = qy + t2*dy  
             typeINTER = 2
          else
             !poke
             xi(1) = qx + t1*dx
             yi(1) = qy + t1*dy  
             xi(2) = hugeNUM
             yi(2) = hugeNUM
             typeINTER = 1
          endif
             
         !here t1 didn't intersect so we are either started inside the sphere or completely past it
       elseif( comparereal(t2,0._fp) >=0 .and. comparereal(t2,1._fp) <= 0 ) then
       
         !ExitWound
          xi(1) = qx + t2*dx
          yi(1) = qy + t2*dy  
          xi(2) = hugeNUM
          yi(2) = hugeNUM
          typeINTER = 1
       
       else
          !no intersection: FallShort, Past or CompletelyInside
          xi(1) = hugeNUM
          yi(1) = hugeNUM
          xi(2) = hugeNUM
          yi(2) = hugeNUM
          typeINTER = 3
       endif
   endif
 
return
end subroutine My_intersec_circle
