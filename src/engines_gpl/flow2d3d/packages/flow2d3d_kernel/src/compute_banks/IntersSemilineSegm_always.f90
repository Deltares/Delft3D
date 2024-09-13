subroutine IntersSemilineSegm_always( qx     ,qy     ,q2x     ,q2y    , &
                                    & px     ,py     ,p2x     ,p2y    , &
                                    & xi     ,yi     ,NEAREST_or_int,  typeINTER       )
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
!   Function:   computes the intersection point(s) (Xi,Yi) of a semiline (starting from
!               the point (qx,qy), passing through point (q2x,q2y) and extending to
!               infinite) with the segment (px,py)-(p2x,p2y). If there is one intersection 
!               point typeINTER=1 and Xi and Yi contain the x- and y-coordinate of this 
!               point, respectively. If the intersection is outside the segments, it returns
!               typeINTER = 3. In this case if NEAREST_or_int=0 the point of the segment nearest
!               to point (qx,qy) is returned, while if NEAREST_or_int=1 the intersection is returned.
!               If the interseption is outside the semiline, it returns typeINTER = 4 (i.e. wrong side of the semiline)
!               IMPORTANT Note: "_always" in the subroutine name implies that the 
!               infinite lines containing the segments and the semiline always intersect somewhere,
!               i.e. the line are not parallel and the segments are not of zero length (these are
!               always verified when looking for boundary intersections in the ghost cell method)
!
!   Algorithm: (Based on Gareth Rees' 2-dimensional specialization of the 3D line intersection algorithm from the article 
!               "Intersection of two lines in three-space" by Ronald Goldman, published in Graphics Gems, page 304. 
!               In three dimensions, the usual case is that the lines are skew (neither parallel nor intersecting) 
!               in which case the method gives the points of closest approach of the two lines.)

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
! The intersection point is on the original pair of semiline/segment  if 0 =< t =< 1 and 0 =< u =< infinite.
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
  real(fp), parameter         :: zero=0.0_fp  
  real(fp), parameter         :: MINUSoneTOtheMINUS6=-0.000001_fp  
  real(fp), parameter         :: one=1.0_fp  
!
  integer                     :: ind(4)
  real(fp)                    :: sx,  sy,  rx,  ry
  real(fp)                    :: den,numt,numu,q_px,q_py,u,t
  Real(fp)                    :: X(4),Y(4),dummy(4)
  logical                     :: segm(4)
  logical                     :: strictlyLARGER
!
! global variables
!
  integer, intent(out)        :: typeINTER
  integer, intent(in)         :: NEAREST_or_int
  real(fp), intent(in)        :: qx     ,qy     ,q2x     ,q2y    ,px     ,py     ,p2x     ,p2y   
  real(fp), intent(out) :: xi       ! x-coordinates of intersection point 
  real(fp), intent(out) :: yi       ! y-coordinates of intersection point 
  logical               :: tGE0,tLE1
!
! executable statements -------------------------------------------------------
!           
    sx = q2x - qx   
    sy = q2y - qy
    rx = p2x - px
    ry = p2y - py
    den  = rx*sy - ry*sx   
!
    q_px = qx - px
    q_py = qy - py   
!                     !  an intersection inside (typeINTER=1) or outside(typeINTER=3) the segments is found and it is stored in  xi,yi
    numu = q_px*ry-q_py*rx
    numt = q_px*sy-q_py*sx 

    if (comparereal(den, zero)/=0) then ! two lines are not parallel (it can happen if one segment has two coinciding points)
    
       u = numu/den
       t = numt/den
       tGE0 = (comparereal(t,zero).ge.0)
       tLE1 = (comparereal(one,t).ge.0)
       if (tGE0.and.tLE1) then       
          typeINTER = 1   !found intersection
       else
          typeINTER = 3 !  the intersection is outside the segments (the infinite lines intersect but not the segments)
          if (NEAREST_or_int.eq.0) then
             IF(tGE0) then !IT IMPLIES THAT IT IS LARGER THEN 1 SINCE IT IS NOT IN 0<=t<=1             
                t = 1._fp
             else
                t = 0._fp
             endif
          endif
       endif      
       if (comparereal(u,MINUSoneTOtheMINUS6).lt.0) then ! i needed MINUSoneTOtheMINUS6 (i.e. a threshold less restricive) cause for porosity=0.500000000007 it happened that it wes giving typeINTER = 4 while it was actually coincident
          typeINTER = 4 !   the intersection is outside the semiline
       endif
       xi = px + t*rx
       yi = py + t*ry
    else  
       ! this part considers the case of segments that degenerate to a point and has not been checked
       if ((comparereal(sx+sy, zero)==0).and.(comparereal(rx+ry, zero)==0)) then !segments are actually two points  its a semiline so it always has an intersection on the second sement (secnd point)
         ! if (comparereal(q_px+q_py, zero)==0) then ! the two points coincides
            typeINTER=1
            xi = px
            yi = py
         ! else
          !   typeINTER = 4 !   its a semiline so it always
       elseif (comparereal(rx+ry, zero)==0) then !the second segment is a point, the first is not
          if (comparereal(numt, zero)==0) then !segment is collinear with the point
            typeINTER=3 ! note I should use 1 here since it exactly meets the end. But for VOF it is more convenient to use 3, i would not consider it a normal intersection
            xi = px
            yi = py
          else
            typeINTER=3
            xi = px
            yi = py         
          endif
       elseif (comparereal(sx+sy, zero)==0) then !the first segment is a point, the second is not. The semiline for it is indefined therefore there are infinite possible intersection. I choose the first point of second segment ( never needed in VOF code)
          typeINTER=1
          xi = px
          yi = py
       else ! two segments are of finite length (not points)
          if (comparereal(numt, zero)==0) then ! two segments are collinear, infinite intersection, I choose the first point of the second segment
            typeINTER=1
            xi = px
            yi = py
          else !parallel, no intersection
            typeINTER=4      
          endif
       endif
!
    endif
!
return
end