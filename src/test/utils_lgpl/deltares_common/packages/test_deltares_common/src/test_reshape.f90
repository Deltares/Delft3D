!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2024.
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

module test_reshape
   use ftnunit
   use m_reshape, only: reshape_implicit, error_value
   implicit none
   private

   public tests_reshape

   double precision, parameter :: epsilon = 1e-5

   contains

   subroutine tests_reshape()
   call test(test_reshape_rank_one, 'reshape: rank one remains untouched')
   call test(test_reshape_rank_two, 'reshape: rank two dimensions are swapped')
   call test(test_reshape_rank_two_unchanged, 'reshape: rank two dimensions are preserved')
   call test(test_reshape_wrong_rank, 'reshape: wrong dimensions cause error signaling return value')
   end subroutine tests_reshape

   subroutine test_reshape_rank_one()
      double precision :: original(6), result(size(original))
      integer :: i 

      original = [(0.1618 * i, i = 1, size(original))]

      result = reshape_implicit(original, shape(original), [1])
      call assert_comparable(original, result, epsilon, 'rank one array was not preserved')
   end subroutine test_reshape_rank_one

   subroutine test_reshape_rank_two()
      integer, parameter :: dims(2) = [3, 7]
      double precision   :: original(product(dims)), result(product(dims))
      double precision   :: explicit(dims(1), dims(2)), expected(dims(2), dims(1))
      integer, parameter :: new_positions(2) = [2, 1]
      integer :: i 
      
      original = [(0.1618 * i, i = 1, size(original))]

      result = reshape_implicit(original, dims(new_positions), new_positions)

      explicit = reshape(original, dims)
      expected = reshape(explicit, dims(new_positions), order = new_positions)

      call assert_comparable(reshape(expected, shape(original)), result, epsilon, 'rank two array was not reordered correctly')
   end subroutine test_reshape_rank_two

   subroutine test_reshape_rank_two_unchanged()
      integer, parameter :: dims(2) = [3, 7]
      double precision   :: original(product(dims)), result(product(dims))
      integer, parameter :: new_positions(2) = [1, 2]
      integer :: i
      
      original = [(0.1618 * i, i = 1, size(original))]

      result = reshape_implicit(original, dims(new_positions), new_positions)

      call assert_comparable(original, result, epsilon, 'rank two array was not preserved')
   end subroutine test_reshape_rank_two_unchanged

   subroutine test_reshape_wrong_rank()
      integer, parameter :: dims(2) = [3, 7]
      double precision   :: original(product(dims)), result(product(dims))
      integer, parameter :: new_positions(2) = [1, 2]
      integer i  
      
      original = [(0.1618 * i, i = 1, size(original))]

      result = reshape_implicit(original, dims + 1, new_positions)

      call assert_comparable([(error_value, i = 1, size(original))], result, epsilon, 'no error value for wrong dimensions')
   end subroutine test_reshape_wrong_rank
end module test_reshape
