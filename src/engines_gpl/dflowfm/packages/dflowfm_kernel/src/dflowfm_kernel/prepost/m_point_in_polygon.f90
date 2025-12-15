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

module m_point_in_polygon
   use m_missing, only: jins, dmiss
   use precision, only: dp

   implicit none

   private

   public :: pinpok_raycast

   contains

!> Optimized ray-casting point-in-polygon test.
!! Pure function that works with array slices or full arrays.
   pure function pinpok_raycast(xl, yl, x, y, n) result(is_inside)

      real(kind=dp), intent(in) :: xl, yl !< Point coordinates to test
      integer, intent(in) :: n !< Number of polygon points
      real(kind=dp), intent(in) :: x(n), y(n) !< Polygon coordinates (at least n elements)
      logical :: is_inside !< Result: true if inside (respecting jins mode)

      ! Locals
      integer :: i, j, crossings
      real(kind=dp) :: x_j, y_j, x_i, y_i, x_intersect

      is_inside = .false.

      ! Degenerate polygon check
      if (n <= 2) then
         is_inside = .true.
         if (jins == 0) is_inside = .not. is_inside
         return
      end if

      ! Ray-casting algorithm: count crossings of horizontal ray from point to +infinity
      crossings = 0
      j = n ! Start with last point

      do i = 1, n
         ! Check for missing value (polygon separator)
         if (x(i) == dmiss) exit

         x_j = x(j)
         y_j = y(j)
         x_i = x(i)
         y_i = y(i)

         ! Check if point is exactly on a vertex
         if (xl == x_j .and. yl == y_j) then
            is_inside = .true.
            if (jins == 0) is_inside = .not. is_inside
            return
         end if

         ! Check if ray crosses this edge
         ! Edge crosses horizontal line through test point if one endpoint is above and one below
         if ((y_j > yl) .neqv. (y_i > yl)) then
            ! Compute x-coordinate of edge-ray intersection
            x_intersect = x_j + (yl - y_j) * (x_i - x_j) / (y_i - y_j)

            if (xl < x_intersect) then
               ! Ray crosses edge to the right of point
               crossings = crossings + 1
            else if (xl == x_intersect) then
               ! Point is exactly on the edge
               is_inside = .true.
               if (jins == 0) is_inside = .not. is_inside
               return
            end if
         end if

         j = i ! Current point becomes previous for next iteration
      end do

      ! Odd number of crossings = inside, even = outside
      is_inside = (mod(crossings, 2) == 1)

      ! Respect jins mode
      if (jins == 0) then
         is_inside = .not. is_inside
      end if

   end function pinpok_raycast

end module m_point_in_polygon
