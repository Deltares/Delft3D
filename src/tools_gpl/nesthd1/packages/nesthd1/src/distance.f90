subroutine distance(sferic    ,x1        ,y1        ,x2        ,y2        , &
                  & d12       )
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
!    Function: Calculates distance between two points on earth
! Method used: Circular distance when sferic is true,
!              Euclidic distance when sferic is false
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
! Global variables
!
    logical , intent(in)  :: sferic !  Description and declaration in tricom.igs
    real    , intent(out) :: d12    !!  Calculated distance from 1 to 2
    real    , intent(in)  :: x1     !!  X coordinate of point 1 (deg or m)
    real    , intent(in)  :: x2     !!  X coordinate of point 2 (deg or m)
    real    , intent(in)  :: y1     !!  Y coordinate of point 1 (deg or m)
    real    , intent(in)  :: y2     !!  Y coordinate of point 2 (deg or m)
!
! Local variables
!
    real     :: alpha  ! Half angle (in radials) between points 1 and 2
    real     :: d128   ! Double precision d12
    real     :: dslin  ! Linear distance between points 1 and 2
    real     :: x1rad  ! X1 in radials
    real     :: x2rad  ! X2 in radials
    real     :: y1rad  ! Y1 in radials
    real     :: y2rad  ! Y2 in radials
    real     :: xcrd1  ! X coordinate of point 1
    real     :: xcrd2  ! X coordinate of point 2
    real     :: ycrd1  ! Y coordinate of point 1
    real     :: ycrd2  ! Y coordinate of point 2
    real     :: zcrd1  ! Z coordinate of point 1
    real     :: zcrd2  ! Z coordinate of point 2

    real     :: ddeg2rad
    real     :: dearthrad
    real     :: pi
!
!! executable statements -------------------------------------------------------
!
! TODO: CALL distance.f90 IN deltares_common INSTEAD OF DOUBLE CODE
!
    pi         = acos(-1.)
    ddegrad    = pi/180.
    dearthrad  = 6378137.0
    !
    if (sferic) then
       x1rad = real(x1)*ddegrad
       x2rad = real(x2)*ddegrad
       y1rad = real(y1)*ddegrad
       y2rad = real(y2)*ddegrad
       !
       xcrd1 = cos(y1rad)*sin(x1rad)
       ycrd1 = cos(y1rad)*cos(x1rad)
       zcrd1 = sin(y1rad)
       !
       xcrd2 = cos(y2rad)*sin(x2rad)
       ycrd2 = cos(y2rad)*cos(x2rad)
       zcrd2 = sin(y2rad)
       !
       dslin = sqrt((xcrd2-xcrd1)**2 + (ycrd2-ycrd1)**2 + (zcrd2-zcrd1)**2)
       alpha = asin(dslin/2.0)
       d128  = dearthrad*2.0*alpha
    else
       xcrd1 = real(x1)
       xcrd2 = real(x2)
       ycrd1 = real(y1)
       ycrd2 = real(y2)
       d128  = sqrt((xcrd2 - xcrd1)**2 + (ycrd2 - ycrd1)**2)
    endif
    d12 = real(d128)
end subroutine distance
