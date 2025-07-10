!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2023-2025.
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

module test_array_module
   use ftnunit
   use array_module

   implicit none
   private
   public tests_array_module

   contains

   subroutine tests_array_module()
      call test(test_convert_mask_to_indices, 'convert mask to indices')
      call test(test_convert_empty_mask_to_indices, 'convert empty mask to indices')
   end subroutine tests_array_module

   subroutine test_convert_mask_to_indices()
      logical, allocatable, dimension(:) :: mask
      integer, allocatable, dimension(:) :: indices, expected_indices

      mask = [.true., .false., .false., .true.]
      expected_indices = [1, 4]

      indices = convert_mask_to_indices(mask)
      call assert_equal(indices, expected_indices, '')
   end subroutine test_convert_mask_to_indices

   subroutine test_convert_empty_mask_to_indices()
      logical, allocatable, dimension(:) :: mask
      integer, allocatable, dimension(:) :: indices

      allocate(mask(0))

      indices = convert_mask_to_indices(mask)
      call assert_equal(allocated(indices), .true., '')
      if (allocated(indices)) then
         call assert_equal(size(indices), 0, '')
      end if
   end subroutine test_convert_empty_mask_to_indices
end module test_array_module
