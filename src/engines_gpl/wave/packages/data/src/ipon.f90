subroutine ipon(xq     ,yq     ,n      ,xp     ,yp     ,inout     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!  
!  
!--description----------------------------------------------------------------
!
! Deltares                                                               *
! AUTHOR : J.A.ROELVINK                                                  *
! DATE   : 22-12-1988                                                    *
! DETERMINE WHETHER POINT (xp,yp) LIES IN POLYGON (x,y) OF n POINTS      *
! POINT n+1 IS SET EQUAL TO POINT 1                                      *
! (ARRAY MUST HAVE DIMENSION n+1 IN MAIN PROGRAMME                       *
! inpout = -1 :  OUTSIDE POLYGON                                         *
! inpout =  0 :  ON EDGE OF POLYGON                                      *
! inpout =  1 :  INSIDE POLYGON                                          *
! USED METHOD :         - DRAW A VERTICAL LINE THROUGH (xp,yp)           *
!                       - DETERMINE NUMBER OF INTERSECTIONS WITH POLYGON *
!                         UNDER yp : nunder                              *
!                       - IF nunder IS EVEN, THEN THE POINT LIES OUTSIDE *
!                         THE POLYGON, OTHERWISE IT LIES INSIDE          *
!                       - THE EDGE IS TREATED SEPARATELY                 *
!
!--pseudo code and references-------------------------------------------------
! NONE
!--declarations---------------------------------------------------------------
    use precision_basics
    !
    implicit none
!
! Global variables
!
    integer               , intent(out) :: inout
    integer               , intent(in)  :: n
    real(hp)              , intent(in)  :: xp
    real(hp)              , intent(in)  :: yp
    real(hp), dimension(*)              :: xq
    real(hp), dimension(*)              :: yq
!
! Local variables
!
    integer                             :: i
    integer                             :: ierr
    integer                             :: nunder
    real(sp)                            :: ysn
    real(sp), dimension(:), allocatable :: x
    real(sp), dimension(:), allocatable :: y
!
! executable statements ------------------------------------------------------
!
    allocate(x(n+1))
    allocate(y(n+1))
    do i = 1, n
       x(i) = real( xq(i)-xp , sp)
       y(i) = real( yq(i)-yp , sp)
    enddo
    x(n + 1) = x(1)
    y(n + 1) = y(1)
    nunder   = 0
    do i = 1, n
       if ((x(i)<0. .and. x(i + 1)>=0.).or.(x(i + 1)<0. .and. x(i)>=0.)) then
          if (y(i)<0. .and. y(i + 1)<0.) then
             nunder = nunder + 1
          elseif ((y(i)<=0. .and. y(i + 1)>=0.) .or.                       &
                & (y(i + 1)<=0. .and. y(i)>=0.)) then
             ysn = (y(i)*x(i + 1) - x(i)*y(i + 1))/(x(i + 1) - x(i))
             if (ysn<0.) then
                nunder = nunder + 1
             elseif (ysn<=0.) then
                !
                ! Edge
                !
                inout = 0
                goto 100
             else
             endif
          else
          endif
       elseif (abs(x(i))<1.0E-8 .and. abs(x(i + 1))<1.0E-8) then
          if ((y(i)<=0. .and. y(i + 1)>=0.).or.(y(i + 1)<=0..and.y(i)>=0.)) &
            & then
             !
             ! Edge
             !
             inout = 0
             goto 100
          endif
       else
       endif
    enddo
    if (mod(nunder, 2)==0) then
       !
       ! Outside
       !
       inout = -1
    else
       !
       ! Inside
       !
       inout = 1
    endif
  100 continue
  deallocate(x, stat=ierr)
  deallocate(y, stat=ierr)
end subroutine ipon
