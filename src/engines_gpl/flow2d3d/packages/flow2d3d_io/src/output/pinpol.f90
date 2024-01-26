subroutine pinpol (x0    , y0    , n     , x     , y     , inside)
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
!!--description-----------------------------------------------------------------
!
!    Function: - Check whether a point lies within (or on) a polygon
! Method used: Fortran90 version of PINPOL d.d. 11 May 1995, NESTHD1
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer               , intent(in)  :: n      ! Number of points in polygon
    real(fp), dimension(n), intent(in)  :: x      ! X co-ordinates of polygon
    real(fp)              , intent(in)  :: x0     ! X co-ordinate of point to check
    real(fp), dimension(n), intent(in)  :: y      ! Y co-ordinates of polygon
    real(fp)              , intent(in)  :: y0     ! Y co-ordinate of point to check
    logical               , intent(out) :: inside ! Flag is TRUE if point
!
! Local variables
!
    integer :: i      ! index polygon from vertex
    integer :: j      ! index polygon to vertex "i+1"
    integer :: ncount ! number of times a line crosses the (x=x0,y>y0) half line
    integer :: ncross
    integer :: nqua
    integer :: quadr1
    integer :: quadr2
    real(fp) :: det
    real(fp) :: x1
    real(fp) :: y1
    real(fp) :: x2
    real(fp) :: y2
!
!! executable statements -------------------------------------------------------
!
    inside = .false.
    ncross = 0
    quadr2 = -999
    !
    do i = 0, n
       !
       ! shift vertex coordinates, associate vertex with quadrant
       !
       j = mod(i,n)+1
       x2 = x(j)-x0
       y2 = y(j)-y0
       !
       ! determine vertex quadrant
       !
       !    4   |   1
       !        |
       !  ---(x0,y0)---
       !        |
       !    5   |   2
       !
       if (x2 > 0.0_fp) then
          if (y2 < 0.0_fp) then
             quadr2 = 2
          else
             quadr2 = 1
          endif
       elseif (x2 < 0.0_fp) then
          if (y2 > 0.0_fp) then
             quadr2 = 4
          else
             quadr2 = 5
          endif
       else
          if (y2 > 0.0_fp) then
             quadr2 = 1
          elseif (y2 < 0.0_fp) then
             quadr2 = 5
          else
             inside = .true.
             exit
          endif
       endif
       !
       if (i>0) then
          !
          ! compute intersection of line (x1,y1)-(x2,y2) with positive y-ax
          !
          nqua = quadr1 + quadr2
          !
          if (nqua==5) then
             !
             ! crossing x=x0 line above y=y0
             !
             ncross = ncross + 1
          elseif (nqua==6) then
             !
             ! diagonally crossing x=x0, might be above or below
             !
             det = x1 * y2 - x2 * y1
             if (det > 0.0_fp) then
                if (quadr1 < quadr2) ncross = ncross + 1
             elseif (det < 0.0_fp) then
                if (quadr1 > quadr2) ncross = ncross + 1
             else
                inside = .true.
                return
             endif
          endif
       endif
       !
       x1 = x2
       y1 = y2
       quadr1 = quadr2
    enddo
    inside = mod(ncross,2)/=0
end subroutine pinpol
