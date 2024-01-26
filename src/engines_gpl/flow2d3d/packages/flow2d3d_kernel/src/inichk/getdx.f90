function getdx(sferic,x1,y1,x2,y2,gdp)
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
!    Function: Calculates "east-west" distance between two points on earth
! Method used: Circular distance when sferic is true,
!              Euclidic distance when sferic is false
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(hp) , pointer :: dearthrad
!
! Global variables
!
    real(fp) :: x1
    real(fp) :: y1
    real(fp) :: x2
    real(fp) :: y2
    real(fp) :: getdx
    logical  :: sferic
!
! Local variables
!
    real(hp) :: csphi
    real(hp) :: xx1
    real(hp) :: yy1
    real(hp) :: xx2
    real(hp) :: yy2
!
!! executable statements -------------------------------------------------------
!
    dearthrad  => gdp%gdconstd%dearthrad
    !
    if (sferic) then
       xx1   = real(x1,hp)*degrad_hp
       xx2   = real(x2,hp)*degrad_hp
       yy1   = real(y1,hp)*degrad_hp
       yy2   = real(y2,hp)*degrad_hp
       csphi = dcos(0.5_hp*(yy1+yy2))
       getdx = real(dearthrad*csphi*(xx2-xx1),fp)
    else
       getdx = x2-x1
    endif
end function getdx
