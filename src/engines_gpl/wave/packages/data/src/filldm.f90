subroutine filldm(elmdms    ,ielem     ,dm1       ,dm2       ,dm3       , &
                & dm4       ,dm5       ,dm6       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! Write element dimensions in array elmdms
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                 , intent(in)  :: dm1    !!  Number of dimensions
    integer                 , intent(in)  :: dm2    !!  Size of first dimension
    integer                 , intent(in)  :: dm3    !!  Size of second dimension
    integer                 , intent(in)  :: dm4    !!  Size of third dimension
    integer                 , intent(in)  :: dm5    !!  Size of fourth dimension
    integer                 , intent(in)  :: dm6    !!  Size of fifth dimension
    integer                 , intent(in)  :: ielem  !!  Index number of element in group
    integer, dimension(6, *), intent(out) :: elmdms !  Description and declaration in nefisio.igs
!
!! executable statements -------------------------------------------------------
!
    ! define element dimensions
    !
    elmdms(1, ielem) = dm1
    elmdms(2, ielem) = dm2
    elmdms(3, ielem) = dm3
    elmdms(4, ielem) = dm4
    elmdms(5, ielem) = dm5
    elmdms(6, ielem) = dm6
end subroutine filldm
