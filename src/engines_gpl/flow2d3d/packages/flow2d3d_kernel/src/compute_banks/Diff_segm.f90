subroutine Diff_segm(   qx     ,qy     ,q2x     ,q2y    , &
                      & px     ,py     ,p2x     ,p2y    , &
                      & xi     ,yi     ,typeINTER       ,numSEGout ,SEGisEXTRpoint      )
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
!   Function:   computes the difference segments of the  (Xi,Yi) of the collinear lines 
!               (qx,qy)-(q2x,q2y) and (px,py)-(p2x,p2y).  vectors Xi and Yi contain the 
!               x- and y-coordinates of the begin and end point (in random order) of the
!               intersection part, that can also coincide!
!               typeINTER = 1: If segments not collinear  (eitehr If the lines are parallel
!               but not coincident or If the (infinite) line passing for the segments intercept 
!              (i.e. not parallel).
!               typeINTER = 2 if line are collinear 
!               In output:  the first column of xi and yi always has the subsegment of segment 1,
!                           and the second column of xi and yi always have the subsegment of segment 2
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
!
  integer                     :: ind(4)
  integer                     :: I1,I2
  real(fp)                    :: sx,  sy,  rx,  ry
  real(fp)                    :: den,numt,numu,q_px,q_py,u,t
  Real(fp)                    :: X(4),Y(4),dummy(4)
  logical                     :: segm(4)
  logical                     :: strictlyLARGER,RIGHTcoinc,LEFTcoinc
!
! global variables
!
  integer, intent(out)        :: typeINTER
  integer, intent(out)        :: numSEGout
  logical, intent(out)        :: SEGisEXTRpoint(2)
  real(fp), intent(in)        :: qx     ,qy     ,q2x     ,q2y    ,px     ,py     ,p2x     ,p2y   
  real(fp), dimension(2,2), intent(out) :: xi       ! x-coordinates of intersection point(s)
  real(fp), dimension(2,2), intent(out) :: yi       ! y-coordinates of intersection point(s)
!
! executable statements -------------------------------------------------------
!           
    SEGisEXTRpoint(1) = .false. 
    SEGisEXTRpoint(2) = .false. 
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
       if (comparereal(numt, zero)==0) then !segments are collinear (they could have a common segment or not)        
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
         strictlyLARGER = .FALSE. !revision: non lo farei più sctricly larger
         if (comparereal(qx, px).ne.0) then ! they are not parallel to y axis, I can use x to order
             CALL SORT4(X,dummy(1:4),ind(1:4))   
             strictlyLARGER = comparereal( X(ind(3)),X(ind(2)) ).gt.0
             LEFTcoinc  =  comparereal( X(ind(1)),X(ind(2)) ).eq.0
             RIGHTcoinc =  comparereal( X(ind(3)),X(ind(4)) ).eq.0
             !write(888888,'(8f15.6,4i8)') X(:),dummy(1:4),ind(1:4)
         else ! they are parallel to y axis, I use y to order
             CALL SORT4(Y,dummy(1:4),ind(1:4))   
             strictlyLARGER = comparereal( Y(ind(3)),Y(ind(2)) ).gt.0
             LEFTcoinc  =  comparereal( Y(ind(1)),Y(ind(2)) ).eq.0
             RIGHTcoinc =  comparereal( Y(ind(3)),Y(ind(4)) ).eq.0
             !write(888888,'(8f15.6,4i8)') Y(:),dummy(1:4),ind(1:4)
         endif
      !   if ((ind(3).gt.ind(2).and.ind(3).gt.ind(1)).or.(ind(3).lt.ind(2).and.ind(3).lt.ind(1))) then !there is no intersection
         if (strictlyLARGER.and.(segm(ind(3))==segm(ind(4))))  then  !i.e. if STRICTLY larger and the two larger points belong to the same segment
            numSEGout = 2
            xi(1,1) = qx    
            yi(1,1) = qy
            xi(2,1) = q2x
            yi(2,1) = q2y
            xi(1,2) = px  
            yi(1,2) = py 
            xi(2,2) = p2x
            yi(2,2) = p2y
            typeINTER = 0
         else !if ( segm(ind(2))==segm(ind(3)) )  then   !it is split in two segments
         !note LEFTcoinc and RIGHTcoinc can be both true but only if its a cut cell with the same dry edge. If they are both fully dry i cannot be here!
            if (LEFTcoinc) then !the left segment has nil length I  store it anyway
               if (segm(ind(4))) then !ind(3) and ind(4) belong to the bank segment 1 (i.e. bank to be eroded belonging to cell n,m)
                  I1 = 2
                  I2 = 1
                  !note: all this mess with SEGisEXTRpoint was to allow segment degenerated to point at the velocity point in order to put a ghost there
                  if(mod(ind(3),2).eq.1) then ! pick one before the last (three)
                     SEGisEXTRpoint(2) = .true. !  if mod(ind(3),2).eq.1, i.e. if the point is at the first position of the input segment (1 for segm 1,3 for segm 3, both odd) (that position has the point in the middle of the edge by construction of EDGExyBANK) then the other segment is a point on the tip (corner) of the edge. If the if is not verified, I have that the coincident point given by LEFTcoinc=true is on the corner of the edge. I consider SEGisEXTRpoint=true only for the corner not for the middle, since I want to keep track of the middle for the ghost points
                  else !if (RIGHTcoinc) then
                     SEGisEXTRpoint(1) = .true.
                  endif
               else !ind(1) and ind(2) belong to segment 1
                  I1 = 1
                  I2 = 2
                  if(mod(ind(3),2).eq.1) then ! if  ind(4) is the first point of the second segment then
                     SEGisEXTRpoint(1) = .true. 
                  else !if (RIGHTcoinc) then
                     SEGisEXTRpoint(2) = .true.
                  endif
               endif
            elseif (RIGHTcoinc) then
               if (segm(ind(1))) then !ind(1) and ind(2) belong to the bank segment 1 (i.e. bank to be eroded belonging to cell n,m)
                  I1 = 1
                  I2 = 2
                  if(mod(ind(2),2).eq.1) then ! I pick the second (its one before the last in the other side)
                     SEGisEXTRpoint(2) = .true. 
                  else !if (LEFTcoinc) then
                     SEGisEXTRpoint(1) = .true. 
                  endif
               else !ind(1) and ind(2) belong to segment 2
                  I1 = 2
                  I2 = 1
                  if(mod(ind(2),2).eq.1) then ! I pick the second (its one before the last in the other side)
                     SEGisEXTRpoint(1) = .true.
                  else !if (LEFTcoinc) then
                     SEGisEXTRpoint(2) = .true.
                  endif
               endif
            else   
               if (segm(ind(1))) then !ind(1) and ind(2) belong to segment 1
                  I1 = 1
                  I2 = 2
               else !ind(1) and ind(2) belong to segment 2
                  I1 = 2
                  I2 = 1
                  !both SEGisEXTRpoint(1:2) are false by default
               endif
            endif
            xi(1,I1) = X(ind(1))   ! the first column of xi and yi always has the subsegment of segment 1,the second column of xi and yi always have the subsegment of segment 2
            yi(1,I1) = Y(ind(1))
            xi(2,I1) = X(ind(2))   
            yi(2,I1) = Y(ind(2))
            xi(1,I2) = X(ind(3))  
            yi(1,I2) = Y(ind(3))  
            xi(2,I2) = X(ind(4))   
            yi(2,I2) = Y(ind(4))
            typeINTER = 2  ;  numSEGout = 2   !the lines are collinear !one of the 2 numSEGout can be degenerate
         endif  
       else
         numSEGout = 0
         typeINTER = 1    !the lines are not collinear 
       endif    
    else  
       numSEGout = 0                    
       typeINTER = 1   ! they are not collinear
    endif
return
end




 