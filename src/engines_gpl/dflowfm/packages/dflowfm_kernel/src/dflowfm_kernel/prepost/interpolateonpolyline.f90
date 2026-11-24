!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
module m_interpolateOnPolyline
   implicit none
contains
   !> Find a point on a polyline at a certain distance from the start.
      !! The distance is measured along the consecutive polyline segments.
   subroutine interpolateOnPolyline(polyline_x, polyline_y, polyline_z, cumulative_distance, num_points, &
                                    interpolated_x, interpolated_y, interpolated_z, target_distance, inside_polyline)
      use precision, only: dp
      integer, intent(in) :: num_points !< Number of polyline points.
      real(kind=dp), dimension(num_points), intent(in) :: polyline_x, polyline_y, polyline_z !< The polyline coordinates.
      real(kind=dp), dimension(num_points), intent(in) :: cumulative_distance !< Accumulated distances along the polyline.
      real(kind=dp), intent(out) :: interpolated_x, interpolated_y, interpolated_z !< Coordinates at the target distance.
      real(kind=dp), intent(in) :: target_distance !< Distance from the polyline start at which to interpolate.
      logical, intent(out) :: inside_polyline !< Whether the target distance is within the polyline length.

      integer :: point_index
      real(kind=dp) :: segment_length, segment_fraction
      point_index = 2
      do while (point_index < num_points .and. cumulative_distance(point_index) <= target_distance)
         point_index = point_index + 1
      end do
      inside_polyline = target_distance >= cumulative_distance(1) .and. target_distance <= cumulative_distance(num_points)
      segment_length = cumulative_distance(point_index) - cumulative_distance(point_index - 1)
      segment_fraction = 0.0_dp
      if (segment_length /= 0.0_dp) then
         segment_fraction = (target_distance - cumulative_distance(point_index - 1)) / segment_length
      end if
      interpolated_x = (1.0_dp - segment_fraction) * polyline_x(point_index - 1) + segment_fraction * polyline_x(point_index)
      interpolated_y = (1.0_dp - segment_fraction) * polyline_y(point_index - 1) + segment_fraction * polyline_y(point_index)
      interpolated_z = (1.0_dp - segment_fraction) * polyline_z(point_index - 1) + segment_fraction * polyline_z(point_index)
      return
   end subroutine interpolateOnPolyline
end module m_interpolateOnPolyline
