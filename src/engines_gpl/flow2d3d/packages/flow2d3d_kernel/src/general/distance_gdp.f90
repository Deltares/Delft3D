subroutine distance_gdp(sferic    ,x1        ,y1        ,x2        ,y2        , &
                  & d12       ,gdp       )
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
    logical , intent(in)  :: sferic !  Description and declaration in tricom.igs
    real(fp), intent(out) :: d12    !!  Calculated distance from 1 to 2
    real(fp), intent(in)  :: x1     !!  X coordinate of point 1 (deg or m)
    real(fp), intent(in)  :: x2     !!  X coordinate of point 2 (deg or m)
    real(fp), intent(in)  :: y1     !!  Y coordinate of point 1 (deg or m)
    real(fp), intent(in)  :: y2     !!  Y coordinate of point 2 (deg or m)
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    call distance(sferic    ,x1        ,y1        ,x2        ,y2        , &
                & d12       ,gdp%gdconstd%dearthrad       )
end subroutine distance_gdp
