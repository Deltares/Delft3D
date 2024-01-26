subroutine increm(mx1       ,ny1       ,mx2       ,ny2       ,incx      , &
                & incy      ,maxinc    ,error     )
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
!    Function: Computes increments (-1, 0 or 1) in the x- and y-
!              coordinates of the given two points
!              NOTE: The angle of the line spanned by these
!                    points with respect to the numerical grid
!                    must be a multiple of 45 deg.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer         :: incx
                                   !!  Increment step calculated from two
                                   !!  x-coordinates (= 0, -1 or 1)
    integer         :: incy
                                   !!  Increment step calculated from two
                                   !!  Y-coordinates (= 0, -1 or 1)
    integer         :: maxinc
                                   !!  Maximum of (INCXA,INCYA)
    integer, intent(in)            :: mx1
                                   !!  M-coord. of the first  point
    integer, intent(in)            :: mx2
                                   !!  M-coord. of the second point
    integer, intent(in)            :: ny1
                                   !!  N-coord. of the first  point
    integer, intent(in)            :: ny2
                                   !!  N-coord. of the second point
    logical, intent(out)           :: error
                                   !!  Flag=TRUE if an error is encountered
!
!
! Local variables
!
    integer                        :: incxa                ! Absolute value of INCX 
    integer                        :: incya                ! Absolute value of INCY 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----bereken afstanden in x en y as
    !
    incx = mx2 - mx1
    incy = ny2 - ny1
    incxa = abs(incx)
    incya = abs(incy)
    maxinc = max(incxa, incya)
    !
    !-----test hoek veelvoud 45 graden
    !
    if (maxinc/=0) then
       if (incx==0) then
          incy = incy/maxinc
       elseif (incy==0) then
          incx = incx/maxinc
       elseif (incxa/=incya) then
          error = .true.
       else
          incx = incx/maxinc
          incy = incy/maxinc
       endif
    endif
end subroutine increm
