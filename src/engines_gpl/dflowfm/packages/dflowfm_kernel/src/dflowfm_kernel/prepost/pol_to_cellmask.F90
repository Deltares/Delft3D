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

!> Wrapper around cellmask_from_polygon_set that uses OpenMP to parallelize the loop over all points if not in MPI mode
module m_pol_to_cellmask
   use precision, only: dp
   use m_cellmask_from_polygon_set, only: cellmask_from_polygon_set_init, cellmask_from_polygon_set_cleanup, cellmask_from_polygon_set

   implicit none

   private

   public :: pol_to_cellmask, cell_mask_from_polygon_file

contains

   function pol_to_cellmask(polygon_points, x_poly, y_poly, z_poly, num_netcells, xcenters, ycenters) result(mask)
      use m_alloc, only: realloc

      integer, intent(in) :: polygon_points !< Number of polygon points
      integer, intent(in) :: num_netcells !< Number of points to mask
      real(kind=dp), intent(in) :: x_poly(polygon_points), y_poly(polygon_points), z_poly(polygon_points) !< Polygon coordinate arrays
      real(kind=dp), intent(in), dimension(:) :: xcenters, ycenters !< Point coordinates
      integer, dimension(:), allocatable :: mask !< Output mask array (1 if inside polygon, 0 if outside)

      integer :: k

      if (polygon_points == 0) then
         return
      end if

      call realloc(mask, num_netcells, keepexisting=.false., fill=0)

      call cellmask_from_polygon_set_init(polygon_points, x_poly, y_poly, z_poly)

      !> Dynamic scheduling in case of unequal work, chunksize guided
      !$OMP PARALLEL DO SCHEDULE(GUIDED)
      do k = 1, num_netcells
         mask(k) = cellmask_from_polygon_set(xcenters(k), ycenters(k))
      end do
      !$OMP END PARALLEL DO

      call cellmask_from_polygon_set_cleanup()

   end function pol_to_cellmask

!> Builds a logical selection mask over all ndxi internal cells (2D + 1D)
!! by testing each cell's circumcenter against the polygon in the given file.
!! Cells whose center falls inside the polygon are marked .true.; all others .false..
!! Returns an unallocated array when the file path is empty or the polygon is empty —
!! the caller should treat an unallocated result as "include all cells".
!!
!! 2D cell centers are taken from xzw/yzw (net cell circumcenters).
!! 1D cell centers are taken from xz/yz (flow node circumcenters, offset by ndx2d).
function cell_mask_from_polygon_file(md_polygon_file) result(mask)
   !use m_pol_to_cellmask, only: pol_to_cellmask
   use m_flowgeom, only: ndxi, ndx2d, xz, yz
   use network_data, only: nump, xzw, yzw
   use m_polygon, only: npl, xpl, ypl, zpl, savepol, restorepol
   use m_delpol, only: delpol
   use m_sferic, only: jsferic
   use m_filez, only: oldfil
   use m_reapol, only: reapol
   use m_fix_global_polygons, only: fix_global_polygons
   implicit none

   character(len=*), intent(in) :: md_polygon_file !< Path to polygon file defining the output region.
   logical, allocatable :: mask(:) !< Output mask over ndxi internal cells; unallocated when no polygon is loaded.

   integer :: minp, ndx1d
   integer, allocatable :: int_mask(:)

   if (len_trim(md_polygon_file) == 0) return

   ndx1d = ndxi - ndx2d

   ! Save any polygon currently in memory, load the output polygon, then restore afterwards.
   call savepol()
   call oldfil(minp, md_polygon_file)
   call reapol(minp, 0)

   if (npl == 0) then
      call restorepol()
      return
   end if

   if (jsferic == 1) then
      call fix_global_polygons(1, 0)
   end if

   allocate(mask(ndxi))
   mask = .false.

   ! --- 2D cells: test net cell circumcenters (xzw/yzw, 1:nump == 1:ndx2d) ---
   if (ndx2d > 0) then
      int_mask = pol_to_cellmask(npl, xpl, ypl, zpl, nump, xzw(1:nump), yzw(1:nump))
      mask(1:ndx2d) = (int_mask(1:ndx2d) /= 0)
   end if

   ! --- 1D cells: test flow node circumcenters (xz/yz, offset by ndx2d) ---
   ! pol_to_cellmask is called separately in 1D-local indexing (1:ndx1d).
   if (ndx1d > 0) then
      int_mask = pol_to_cellmask(npl, xpl, ypl, zpl, ndx1d, xz(ndx2d + 1:ndxi), yz(ndx2d + 1:ndxi))
      mask(ndx2d + 1:ndxi) = (int_mask(1:ndx1d) /= 0)
   end if

   call delpol()
   call restorepol()

end function cell_mask_from_polygon_file

end module m_pol_to_cellmask
