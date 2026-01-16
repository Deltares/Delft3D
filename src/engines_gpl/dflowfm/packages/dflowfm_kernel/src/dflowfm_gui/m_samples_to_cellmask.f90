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

!> Update cellmask from samples - optimized version with early exit per cell
module m_samples_to_cellmask

   use precision, only: dp
   implicit none

contains

   subroutine samples_to_cellmask()

      use network_data, only: cellmask, nump1d2d
      use m_samples, only: ns, xs, ys
      use m_cellmask_from_polygon_set, only: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines
      use m_alloc, only: realloc

      integer :: i, k

      ! Allocate and initialize cellmask
      call realloc(cellmask, nump1d2d, keepexisting=.false., fill=0)

      ! Early exit if no samples
      if (ns == 0) then
         return
      end if

      ! Initialize the spatial index for all netcells
      ! This builds bounding boxes and polygon data structures for fast point-in-polygon tests
      call init_cell_geom_as_polylines()

      !> Dynamic scheduling in case of unequal work, chunksize guided
      ! Loop over samples (much fewer than cells)
      !$OMP PARALLEL DO SCHEDULE(GUIDED)
      do i = 1, ns
         k = point_find_netcell(xs(i), ys(i))
         if (k > 0) then
            cellmask(k) = 1 ! Safe without ATOMIC - all threads write same value
         end if
      end do
      !$OMP END PARALLEL DO

      ! Cleanup spatial index
      call cleanup_cell_geom_polylines()

      return
   end subroutine samples_to_cellmask

end module m_samples_to_cellmask
