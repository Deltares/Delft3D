subroutine inpolygon( testx   ,testy   ,vertx   ,verty   ,nvert   ,inp  )
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
!  $Id: inpolygon.f90 6742 2016-11-09 16:50:58Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/inpolygon.f90 $
!!--description-----------------------------------------------------------------
!
!   Function:   Initial version by Aukje Spruyt. Checks whether a point lies within a polygon
!               INP=true when the point (X,Y) is located within the polygon 
!               with the x- and y-coordinates of the polygon given in the 1 x nvert 
!               arrays XVERT and YVERT.  Otherwise INP=false.
!               based on: http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
!               Note Alberto: this version CONSIDER A POINT ON THE EDGE OR VERTEX AS INSIDE THE POLYGON
!               while the old not (see PROBLEMS WITH OLD VERSION right after "end subroutine"
!               Note: this version should work also with coincident vertex (or first coinciding with last)
!               cause they are considered as the degenerate case of horizontal edge with the two vertex coinciding   
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
        !
    implicit none
!
! Global variables
!
    real(fp),                     intent(in)  :: testx    ! x-coordinate of point
    real(fp),                     intent(in)  :: testy    ! y-coordinate of point
    real(fp), dimension(nvert),   intent(in)  :: vertx    ! x-coordinates of vertices of polygon
    real(fp), dimension(nvert),   intent(in)  :: verty    ! y-coordinates of vertices of polygon
    integer ,                     intent(in)  :: nvert    ! number of vertices 
    logical ,                     intent(out) :: inp    !       
    !
    ! Local variables
    !           
    integer                     :: Comp_vertyi_vertyj,Comp_vertyi_testy,Comp_vertyj_testy,Comp_testx_vertxi,Comp_testx_vertxj,Comp_testx_intersX
    integer                     :: i
    integer                     :: j
    real(fp)                    :: vertip1
    integer, dimension(0:nvert) :: ind
    logical, dimension(nvert)   :: foundVERT
    real(fp)                    :: intersX
    logical                     :: NOTminMAX
    integer                     :: Comp_vertyj_vertyi ,Comp_vertyi_vertip1
    integer                     :: Comp_vertyjm1_vertyj,Comp_vertyj_verti 
    integer                     :: Comp_vertyip1_vertyi 
!
!! executable statements -------------------------------------------------------
!    
    ind = (/ nvert-1,nvert, ( i, i=1, nvert-1) /); !nvert-1 is needed to compute NOTminMAX (relative max or min)
    foundVERT(:)=.FALSE.
    inp = .false.
    ! i test intersection with edges drawing a line from the test point (testx,testy) toward the positive x.
    do i = 1,nvert
        j = ind(i)
        Comp_vertyi_vertyj = comparereal(verty(i),verty(j))
        if (Comp_vertyi_vertyj/=0) then ! I use y to compare
!
           Comp_vertyi_testy = comparereal(verty(i),testy)
           Comp_vertyj_testy = comparereal(verty(j),testy)
           if ((Comp_vertyi_testy > 0 .and. Comp_vertyj_testy < 0).or.&
               (Comp_vertyi_testy < 0 .and. Comp_vertyj_testy > 0)) then  !(is y coord of the first node upper then y coord of test point).not equal. ((is y coord of the second node upper then y coord of test point)
              intersX = (vertx(j)-vertx(i)) * (testy-verty(i)) / (verty(j)-verty(i)) + vertx(i) 
              Comp_testx_intersX = comparereal(testx , intersX)
              if (Comp_testx_intersX<0 ) then
                 inp = .not. inp  
              elseif (Comp_testx_intersX==0 ) then
                 inp = .true.   ! the test point lays on a vertical edge  ( I consider it inside. Exit)
                 exit
              endif
           elseif(foundVERT(i).eq..false. .and. Comp_vertyi_testy==0.and.comparereal(testx , vertx(i))<0) then
!             if its a relative min or max:
!                                       /\           \    /
!                                      /  \           \  /
!                                     /    \           \/
!             it has to be count twice!! OR none!!! I choose none! In fact for these case:
!                ____________
!               |            |
!               |            |
!               |  A    B    |
!               |  '    /\   |     if the horizontal semiline through the point A intersects the vertex B it has to be twice or none otherwise I have even interseptions and the point A is gonna be estimated as outside the polygon
!               |______/  \__|
!
!
              if (i+1.gt.nvert) then !this if can be avoided entering the first node as the final node (i.e. dimension nvert+1 for the polygon vertexes) but it is not worth since it happens almost never to execute this part
                 vertip1 = 1
              else 
                 vertip1 = verty(i+1)
              endif
              Comp_vertyj_vertyi  = comparereal(verty(j),verty(i))
              Comp_vertyi_vertip1 = comparereal(verty(i),vertip1)
              NOTminMAX = (( Comp_vertyj_vertyi < 0 .and. Comp_vertyi_vertip1 < 0).or.&
                           ( Comp_vertyj_vertyi > 0 .and. Comp_vertyi_vertip1 > 0))! note I dont care about the == case. If its equal it is gonna be captured by the "else !if (Comp_vertyi_vertyj == 0)" below, that is going to correctly find two interseptions or exit if its on the edge itself
              if (NOTminMAX) then  
                 foundVERT(i) = .true.
                 inp = .not. inp
              endif
           elseif(foundVERT(j).eq..false. .and. Comp_vertyj_testy==0.and.comparereal(testx , vertx(j))<0) then
             !same comment as for point A and B above applies
              Comp_vertyjm1_vertyj = comparereal(verty(ind(i-1)),verty(j)) !ind(i-1) is the vertex before j ((Note it is wrong to write j-1 cause if i=2, j=ind(2)=1 and j-1=0 so it goes out of bound)
              Comp_vertyj_verti    = comparereal(verty(j),verty(i))
              NOTminMAX = (( Comp_vertyjm1_vertyj < 0 .and. Comp_vertyj_verti < 0).or.&
                           ( Comp_vertyjm1_vertyj > 0 .and. Comp_vertyj_verti > 0))
              if (NOTminMAX) then
                 foundVERT(j) = .true.
                 inp = .not. inp
              endif
           endif
!
        else !if (Comp_vertyi_vertyj == 0) then ! 
!  
           if (comparereal(verty(i),testy)==0)  then  ! all 3 at the same coordinate. 
              Comp_testx_vertxi = comparereal(testx,vertx(i))
              Comp_testx_vertxj = comparereal(testx,vertx(j))              
              if ((( Comp_testx_vertxi <= 0 ).and.( Comp_testx_vertxj >= 0 )).or.&  !check if the point stays on the edge  
                  (( Comp_testx_vertxj <= 0 ).and.( Comp_testx_vertxi >= 0 )))  then 
                 inp = .true.   ! the test point lays on a horizontal edge or at his end points 
                 exit
              elseif(comparereal(testx,min(vertx(i),vertx(j)))<0) then !if it lays on the left of the edge it itersects the polygon twice (2 vertexes) unless the vertexes were already interesected on some other edges
!              
                 if (i+1.gt.nvert) then !this if can be avoided entering the first node as the final node (i.e. dimension nvert+1 for the polygon vertexes) but it is not worth since it happens almost never to execute this part
                    vertip1 = 1
                 else 
                    vertip1 = verty(i+1)
                 endif
                 Comp_vertyjm1_vertyj =  comparereal(verty(ind(i-1)),verty(j)) !ind(i-1) is the vertex before j (Note it is wrong to write j-1 cause if i=2, j=ind(2)=1 and j-1=0 so it goes out of bound)  
                 Comp_vertyip1_vertyi =  comparereal(vertip1,verty(i))
                 if ( ( Comp_vertyjm1_vertyj <= 0 .and. Comp_vertyip1_vertyi <= 0).or.& !note I use testy since the 3 points have the same y coord, any is fine!
                      ( Comp_vertyjm1_vertyj >= 0 .and. Comp_vertyip1_vertyi >= 0)) then
                   !  both "j" and " i"   are  maximum or minumum. In the sense:
                   !   
                   !          j ______ i      here both maximum. in this case both nodes have to be counted I CHANGED THIS, NONE IS COUNTED!!. (and one could have already been counted on the adjacent edge so I check foundVERT).
                   !       ____|      |____
                   !
    
                    if (foundVERT(j).eq..false.) then
                       foundVERT(j) = .true.
                   !    inp = .not. inp
                    endif
                    if (foundVERT(i).eq..false.) then    
                       foundVERT(i) = .true.
                   !    inp = .not. inp                
                    endif
                 else
   !                it handles this case:
   !                ____________
   !               |            |
   !               |  x   ,-----'     i and j are not both maxima!!! with x right in line with the edge. In this case only one of the two vertex has to be counted!!! one could have already been count on the adjacent edge so I check foundVERT. If it was already counted im good, i just set the other foundVERT to true.
   !               |______|
   !                
                    if (foundVERT(j).eq..TRUE..AND.foundVERT(i).eq..FALSE.) THEN
                       foundVERT(i) = .TRUE. ! I DONT CHANGE INPUT, IT IS INTERSECTED ONCE AS IT SHOULD!
                    ELSEIF(foundVERT(j).eq..FALSE..AND.foundVERT(i).eq..TRUE.) THEN
                       foundVERT(j) = .FALSE. ! I DONT CHANGE INPUT, IT IS INTERSECTED ONCE AS IT SHOULD!
                    ELSEIF(foundVERT(j).eq..FALSE..AND.foundVERT(i).eq..FALSE.) THEN ! this happens only if it is the first edge checked
                       foundVERT(i) = .TRUE.
                       foundVERT(j) = .TRUE.  
                       inp = .not. inp       !  I CHANGE INPUT, SO IT IS INTERSECTED ONCE AS IT SHOULD!
                    ELSEIF(i==1.AND.j==nvert) then !(foundVERT(j).eq..TRUE..AND.foundVERT(i).eq..TRUE.) THEN !happens only when last edge checked
                  !  If both were counted i change inp (this would happen if i and j are the first and last vertex of the polygon so they are both checked and found
   !               
   !                ____________________
   !               |                    |
   !               |  x      last ,-----' first      
   !               |______________|
   !                 
                       inp = .not. inp  
                    ENDIF
                 endif
              endif        
           endif
        endif
    enddo
!
return
end subroutine inpolygon
