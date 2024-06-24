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

module m_reshape
   implicit none
   private

   public :: error_value

   public reshape_implicit

   double precision, parameter :: error_value = -999

   interface reshape_implicit
      module procedure reshape_implicit_double
   end interface reshape_implicit

   contains

   pure function reshape_implicit_double(source, implicit_shape, order) result(res)
      double precision, intent(in) :: source(:)         !< Rank-1 array interpreted as flattened rank-2 array
      integer,          intent(in) :: implicit_shape(:) !< Shape of the unflattened rank-2 array after reordering
      integer,          intent(in) :: order(:)          !< New order of the ranks, permutation of [1, 2, ..., size(implicit_shape)]
      double precision             :: res(size(source)) !< Reshaped and reordered source, flattened to rank 1

      if (product(implicit_shape) /= size(source) .or. size(order) /= size(implicit_shape)) then
         res = error_value
         return
      end if
      ! The reshape_implicit functions cannot make use of Fortran overloading,
      ! since they are only overloaded based on array size, which is not reflected in the type system
      select case (size(implicit_shape))
         case (1)
            res = source
         case (2)
            res = reshape_implicit_2d_double(source, implicit_shape, order)
         case (3)
            res = reshape_implicit_3d_double(source, implicit_shape, order)
         case (4)
            res = reshape_implicit_4d_double(source, implicit_shape, order)
         case (5)
            res = reshape_implicit_5d_double(source, implicit_shape, order)
         case default
            res = error_value
      end select
   end function reshape_implicit_double

   !> Interpret source array as a flattened rank-2 array, and reorder the ranks according to order,
   !! such that the result has the implicit shape implicit_shape
   pure function reshape_implicit_2d_double(source, implicit_shape, order) result(res)
      double precision, intent(in) :: source(:)         !< Rank-1 array interpreted as flattened rank-2 array
      integer,          intent(in) :: implicit_shape(2) !< Shape of the unflattened rank-2 array after reordering
      integer,          intent(in) :: order(2)          !< New order of the ranks, permutation of [1, 2]
      double precision :: res(size(source))
      res = reshape(reshape(source, implicit_shape, order = order), shape(source))
   end function reshape_implicit_2d_double

   !> Interpret source array as a flattened rank-3 array, and reorder the ranks according to order,
   !! such that the result has the implicit shape implicit_shape
   pure function reshape_implicit_3d_double(source, implicit_shape, order) result(res)
      double precision, intent(in) :: source(:)         !< Rank-1 array interpreted as flattened rank-3 array
      integer,          intent(in) :: implicit_shape(3) !< Shape of the unflattened rank-3 array after reordering
      integer,          intent(in) :: order(3)          !< New order of the ranks, permutation of [1, 2, 3]
      double precision :: res(size(source))
      res = reshape(reshape(source, implicit_shape, order = order), shape(source))
   end function reshape_implicit_3d_double

   !> Interpret source array as a flattened rank-4 array, and reorder the ranks according to order,
   !! such that the result has the implicit shape implicit_shape
   pure function reshape_implicit_4d_double(source, implicit_shape, order) result(res)
      double precision, intent(in) :: source(:)         !< Rank-1 array interpreted as flattened rank-4 array
      integer,          intent(in) :: implicit_shape(4) !< Shape of the unflattened rank-4 array after reordering
      integer,          intent(in) :: order(4)          !< New order of the ranks, permutation of [1, 2, 3, 4]
      double precision :: res(size(source))
      res = reshape(reshape(source, implicit_shape, order = order), shape(source))
   end function reshape_implicit_4d_double

   !> Interpret source array as a flattened rank-5 array, and reorder the ranks according to order,
   !! such that the result has the implicit shape implicit_shape
   pure function reshape_implicit_5d_double(source, implicit_shape, order) result(res)
      double precision, intent(in) :: source(:)         !< Rank-1 array interpreted as flattened rank-5 array
      integer,          intent(in) :: implicit_shape(5) !< Shape of the unflattened rank-5 array after reordering
      integer,          intent(in) :: order(5)          !< New order of the ranks, permutation of [1, 2, 3, 4, 5]
      double precision :: res(size(source))
      res = reshape(reshape(source, implicit_shape, order = order), shape(source))
   end function reshape_implicit_5d_double
end module m_reshape
