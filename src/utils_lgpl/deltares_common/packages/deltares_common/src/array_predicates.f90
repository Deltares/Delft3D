!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> Contains procedures that test properties of arrays
module m_array_predicates
   implicit none
   private

   public is_monotonically_increasing

   interface is_monotonically_increasing
      module procedure is_monotonically_increasing_double
   end interface is_monotonically_increasing

   contains

   !> Test if the array never contains decreasing values.
   pure function is_monotonically_increasing_double(array, end_index, start_index) result(res)
      logical                                      :: res         !< Whether the array elements increase monotonically
      double precision, dimension(:),  intent(in)  :: array       !< The array to test
      integer, optional,               intent(in)  :: end_index   !< If present, only test for the subarray ending at end_index
      integer, optional,               intent(in)  :: start_index !< If present, only test for the subarray starting at start_index

      integer :: local_end_index
      integer :: local_start_index
      integer :: i

      if (present(end_index)) then
         local_end_index = end_index
      else
         local_end_index = ubound(array, dim=1)
      endif
      if (present(start_index)) then
         local_start_index = start_index
      else
         local_start_index = lbound(array, dim=1)
      endif

      do i = local_start_index, local_end_index - 1
         if (array(i) > array(i + 1)) then
            res = .false.
            return
         endif
      enddo
      res = .true.
   end function is_monotonically_increasing_double
end module m_array_predicates
