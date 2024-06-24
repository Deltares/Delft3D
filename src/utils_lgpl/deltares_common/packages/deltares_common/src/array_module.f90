module array_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.            
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!    Function: - Various array processing routines
!
!    This operation works fine:
!       arr(:) = const
!    But for this operation, the complete array is copied to the stack:
!       arr(:) = arr(:) / const
!    resulting in a stack overflow for large arrays
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

use precision

implicit none

private 

public array_div_const
public convert_start_index
public convert_mask_to_indices

interface array_div_const
   module procedure arraySp1_div_constSp
   module procedure arrayHp1_div_constHp
   module procedure arraySp2_div_constSp
   module procedure arrayHp2_div_constHp
end interface



contains
!
!
!
!===============================================================================
subroutine arraySp1_div_constSp(arr, denominator)
   !
   ! Parameters
   real(sp), dimension(:)      , intent(inout)  :: arr
   real(sp)                    , intent(in)     :: denominator
   !
   ! Locals
   integer :: i
   !
   ! Body
   do i = lbound(arr,1), ubound(arr,1)
      arr(i) = arr(i) / denominator
   enddo
end subroutine arraySp1_div_constSp
!
!
!
!===============================================================================
subroutine arrayHp1_div_constHp(arr, denominator)
   !
   ! Parameters
   real(hp), dimension(:)      , intent(inout)  :: arr
   real(hp)                    , intent(in)     :: denominator
   !
   ! Locals
   integer :: i
   !
   ! Body
   do i = lbound(arr,1), ubound(arr,1)
      arr(i) = arr(i) / denominator
   enddo
end subroutine arrayHp1_div_constHp
!
!
!
!===============================================================================
subroutine arraySp2_div_constSp(arr, denominator)
   !
   ! Parameters
   real(sp), dimension(:,:)    , intent(inout)  :: arr
   real(sp)                    , intent(in)     :: denominator
   !
   ! Locals
   integer :: i
   integer :: j
   !
   ! Body
   do j = lbound(arr,2), ubound(arr,2)
      do i = lbound(arr,1), ubound(arr,1)
         arr(i,j) = arr(i,j) / denominator
      enddo
   enddo
end subroutine arraySp2_div_constSp
!
!
!
!===============================================================================
subroutine arrayHp2_div_constHp(arr, denominator)
   !
   ! Parameters
   real(hp), dimension(:,:)    , intent(inout)  :: arr
   real(hp)                    , intent(in)     :: denominator
   !
   ! Locals
   integer :: i
   integer :: j
   !
   ! Body
   do j = lbound(arr,2), ubound(arr,2)
      do i = lbound(arr,1), ubound(arr,1)
         arr(i,j) = arr(i,j) / denominator
      enddo
   enddo
end subroutine arrayHp2_div_constHp
!
!
!
!===============================================================================
function convert_start_index(arr, imiss, providedIndex, requestedIndex) result(ierr)
    !
    ! Parameters
    integer, dimension(:), intent(inout) :: arr
    integer, intent(in)                  :: imiss
    !
    ! Locals
    integer, intent(in)                  :: providedIndex, requestedIndex
    integer                              :: shift, ierr
    !
    ! Body
    shift = requestedIndex - providedIndex
    where(arr /= imiss) 
      arr= arr + shift   
    end where
    ierr = 0
    
end function convert_start_index

!> Function to convert logical array to array of indices where elements are true
pure function convert_mask_to_indices(mask) result(indices)
   logical, dimension(:), intent(in) :: mask !< Array to be converted

   integer, allocatable, dimension(:) :: indices !< Resulting indices where mask is true

   integer :: i

   indices = pack([(i, i = 1, size(mask))], mask)
end function convert_mask_to_indices

end module array_module
