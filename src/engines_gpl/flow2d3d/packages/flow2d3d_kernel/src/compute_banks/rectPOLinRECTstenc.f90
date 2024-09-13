subroutine rectPOLinRECTstenc(polyX,polyY,m,n,insidePOLY, gdp)
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
!   Function:   Check if all the points of square polygon 1 (bank erosion surface) are 
!               inside rectangular polygon 2 (stencil of 9 cells)
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
!
! local variables
!
  integer :: lowN
  integer :: lowM
  integer :: upN
  integer :: upM 
  integer :: k
  logical :: insidePOINT
  real(fp):: STENCILx(5) ! x-coordinates of vertices of polygon
  real(fp):: STENCILy(5) ! y-coordinates of vertices of polygon
    real(fp), dimension(:,:), pointer :: xcor0
    real(fp), dimension(:,:), pointer :: ycor0
!
! global variables
!  
  real(fp), intent(in)    :: polyX(4)    
  real(fp), intent(in)    :: polyY(4)
  logical, intent(out)    :: insidePOLY  
  integer, intent(in)     :: m
  integer, intent(in)     :: n
!
! executable statements -------------------------------------------------------
!    
    xcor0 => gdp%gdimbound%xcor0
    ycor0 => gdp%gdimbound%ycor0
    lowN = n-2 
    lowM = m-2 
    upN  = n+1 
    upM  = m+1 
    stencilX = (/xcor0(lowN,lowM) , xcor0(lowN,upM), xcor0(upN,upM) , xcor0(upN,lowM), xcor0(lowN,lowM) /)  
    stencilY = (/ycor0(lowN,lowM) , ycor0(lowN,upM), ycor0(upN,upM) , ycor0(upN,lowM), ycor0(lowN,lowM) /)  
!
    insidePOLY = .TRUE.
    do k=1,4
      CALL inpolygon( polyX(k)   , polyY(k)    ,stencilX   ,stencilY   ,4   , insidePOINT  )
      insidePOLY = insidePOLY.and.insidePOINT
      IF (.not.insidePOLY) exit
    enddo
!
  return
end
