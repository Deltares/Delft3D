subroutine My_intersec( qx     ,qy     ,q2x     ,q2y    , &
                      & px     ,py     ,p2x     ,p2y    , &
                      & xi     ,yi     ,typeINTER       )
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
!   Function:   computes the intersection point(s) (Xi,Yi) of the lines 
!               (qx,qy)-(q2x,q2y) and (px,py)-(p2x,p2y). If there is one intersection 
!               point typeINTER=1 and Xi and Yi contain the x- and y-coordinate of this 
!               point, respectively. If the lines coincide typeINTER=2 and the 1x2 
!               vectors Xi and Yi contain the x- and y-coordinates of the begin and 
!               end point (in random order) of the intersection part. If the lines are
!               parallel but not coincident typeINTER=0 and Xi=HUGE and Yi=HUGE. 
!               If the intersection is outside the segments, it returns typeINTER = 3
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
! local variables
!
  real(fp), parameter         :: zero=0.0_fp  
  real(fp), parameter         :: one=1.0_fp  
  real(fp), parameter         :: hugeNUM=HUGE(1.0_fp) 
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
  real(fp), intent(in)        :: qx     ,qy     ,q2x     ,q2y    ,px     ,py     ,p2x     ,p2y   
  real(fp), dimension(2), intent(out) :: xi       ! x-coordinates of intersection point(s)
  real(fp), dimension(2), intent(out) :: yi       ! y-coordinates of intersection point(s)
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
!
    if (comparereal(den, zero)==0) then ! two lines are parallel
       numt = q_px*sy-q_py*sx       
       if (comparereal(numt, zero)==0) then !segments are collinear (they could have a common segment or not)     ! to be checked, I think in the case one segment has two coinciding vertices, this condition is not sufficient, see my modification after 30 september,2013 in IntersSemilineSegm_always
         X(1) = qx    
         X(2) = q2x    
         X(3) = px    
         X(4) = p2x    

         Y(1) = qy     
         Y(2) = q2y     
         Y(3) = py    
         Y(4) = p2y 
         segm(1) = .true.    !segment 1
         segm(2) = .true.    !segment 1
         segm(3) = .false.   !segment 2
         segm(4) = .false.   !segment 2
!
         strictlyLARGER = .FALSE.
         if (comparereal(qx, px).ne.0) then ! they are not parallel to y axis, I can use x to order
             CALL SORT4(X,dummy(1:4),ind(1:4))   
             strictlyLARGER = comparereal( X(ind(3)),X(ind(2)) ).gt.0
             !write(888888,'(8f15.6,4i8)') X(:),dummy(1:4),ind(1:4)
         else ! they are parallel to x axis, I use x to order
             CALL SORT4(Y,dummy(1:4),ind(1:4))   
             strictlyLARGER = comparereal( Y(ind(3)),Y(ind(2)) ).gt.0
             !write(888888,'(8f15.6,4i8)') Y(:),dummy(1:4),ind(1:4)
         endif
      !   if ((ind(3).gt.ind(2).and.ind(3).gt.ind(1)).or.(ind(3).lt.ind(2).and.ind(3).lt.ind(1))) then !there is no intersection
         if (strictlyLARGER.and.(segm(ind(3))==segm(ind(4))))  then  !i.e. if STRICTLY larger and the two larger points belong to the same segment
            xi(1) = hugeNUM
            yi(1) = hugeNUM
            xi(2) = hugeNUM   
            yi(2) = hugeNUM
            typeINTER = 0  !line collinear but no intersections
         else           
            xi(1) = X(ind(2))  
            yi(1) = Y(ind(2))  
            xi(2) = X(ind(3))   
            yi(2) = Y(ind(3)) 
            typeINTER = 2    !the lines are collinear, the intersection is the common segment and I store first and last point of the intersection (random order). If the common segment is a point (i.e. segments touch in a  point) I have as an output twice that point (to be checked)
         endif  
       else
         xi(:) = hugeNUM
         yi(:) = hugeNUM
         typeINTER = 0    !the lines are simply parallel (not collinear)
       endif    
    else                       !  an intersection inside (typeINTER=1) or outside(typeINTER=3) the segments is found and it is stored in  xi,yi
       numu = q_px*ry-q_py*rx
       numt = q_px*sy-q_py*sx 
       u = numu/den
       t = numt/den
       if (((comparereal(t,zero).ge.0).and.(comparereal(one,t).ge.0)).and. &
           ((comparereal(u,zero).ge.0).and.(comparereal(one,u).ge.0))) then
          typeINTER = 1   !found intersection
       else
          typeINTER = 3 !  the intersection is outside the segments (the infinite lines intersect but not the segments)
       endif      
       xi(1) = qx + u*sx
       yi(1) = qy + u*sy
    endif
return
end subroutine My_intersec
