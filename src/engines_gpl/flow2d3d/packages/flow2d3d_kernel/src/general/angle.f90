subroutine angle(sferic    ,x1        ,y1        ,x2        ,y2        , &
               & ang       ,gdp       )
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
!    Function: Calculates the angle between a line through two
!              points with a parallel
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    logical, intent(in)            :: sferic !  Description and declaration in tricom.igs
    real(fp), intent(out)              :: ang
                                   !!  Resulting angle (radials)
    real(fp), intent(in)               :: x1
                                   !!  X coordinate of point 1 (deg or m)
    real(fp), intent(in)               :: x2
                                   !!  X coordinate of point 2 (deg or m)
    real(fp), intent(in)               :: y1
                                   !!  Y coordinate of point 1 (deg or m)
    real(fp), intent(in)               :: y2
                                   !!  Y coordinate of point 2 (deg or m)
!
!
! Local variables
!
    real(fp)                       :: degrad
    real(fp)                       :: dx                   ! Distance in x direction 
    real(fp)                       :: dy                   ! Distance in y direction 
    real(fp)                       :: small                ! Threshold for x/y distances to be nonzero 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !     INITIALISATION
    !     WARNING: the const-include file can not be used here
    !              inigeo is (also) called by wave, without initialization
    !              of the const-parameters.
    !
    small = 1.0E-10
    degrad = atan(1.)/45.
    if (abs(x1 - x2)<small .and. abs(y1 - y2)<small) then
       ang = 0.
    else
       if (sferic) then
          dx = (x2 - x1)*cos(degrad*(y1 + y2)/2.)
       else
          dx = x2 - x1
       endif
       dy = y2 - y1
       ang = atan2(dy, dx)
    endif
end subroutine angle
