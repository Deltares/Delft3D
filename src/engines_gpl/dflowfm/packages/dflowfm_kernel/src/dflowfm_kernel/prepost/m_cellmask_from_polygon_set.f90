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

module m_cellmask_from_polygon_set
   use iso_fortran_env, only: int64
   use m_missing, only: jins, dmiss
   use precision, only: dp

   implicit none

   private

   public :: cellmask_from_polygon_set_init, cellmask_from_polygon_set_cleanup, cellmask_from_polygon_set, pinpok_elemental
   public :: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines
   public :: find_cells_crossed_by_polyline

   real(kind=dp), allocatable, dimension(:) :: xpl_cache
   real(kind=dp), allocatable, dimension(:) :: ypl_cache
   real(kind=dp), allocatable, dimension(:) :: zpl_cache
   integer :: npl_cache = 0 !< Number of points in the cached polygon arrays, including dmiss separators

   integer :: polygons = 0 !< Number of polygons stored in module arrays xpl, ypl, zpl
   real(kind=dp), allocatable :: x_poly_min(:), y_poly_min(:) !< Polygon bounding box min coordinates, (dim = polygons)
   real(kind=dp), allocatable :: x_poly_max(:), y_poly_max(:) !< Polygon bounding box max coordinates, (dim = polygons)
   real(kind=dp), allocatable :: polygon_type(:) !< Polygon type, positive or dmiss = drypoint , negative = enclosure (dim = polygons)
   integer, allocatable :: i_poly_start(:), i_poly_end(:) !< Polygon start and end indices in coordinate arrays (dim = polygons)
   integer, allocatable :: i_poly_bin_start(:), i_poly_num_bins(:) !< Start and number of bins for each polygon.
   integer, allocatable :: edge_bin_offsets(:), edge_indices(:) !< CSR offsets and ordered local edge indices.
   real(kind=dp), allocatable :: y_poly_bin_scale(:) !< Scale from polygon y-coordinate to bin index.
   logical :: cellmask_initialized = .false. !< Flag indicating if cellmask data structures have been initialized for safety
   logical :: enclosures_present = .false. !< Flag indicating if any enclosures are present in the polygon dataset

   integer, parameter :: min_edges_for_binning = 256 !< Small polygon sets are cheaper to scan directly.
   integer, parameter :: max_edge_bins = 1024 !< Maximum number of latitude bins per polygon.
   integer(kind=int64), parameter :: max_memberships_per_edge = 8_int64 !< Memory/work cap for vertically long edges.

contains

   !> Initialize module-level cellmask polygon data structures, such as the bounding boxes, cache and iistart/iiend
   ! this keeps the actual calculation routines elemental.
   subroutine cellmask_from_polygon_set_init(polygon_points, x_poly, y_poly, z_poly, enable_binning)
      use m_alloc
      use geometry_module, only: get_startend

      integer, intent(in) :: polygon_points !< Number of polygon points
      real(kind=dp), intent(in) :: x_poly(polygon_points), y_poly(polygon_points), z_poly(polygon_points) !< Polygon coordinate arrays
      logical, intent(in) :: enable_binning !< Whether to build a latitude-binned edge index for large polygon sets.

      integer :: i_point, i_start, i_end, i_poly

      if (cellmask_initialized) then
         call cellmask_from_polygon_set_cleanup()
      end if

      call init_geom_cache(polygon_points, x_poly, y_poly, z_poly)

      if (polygon_points == 0) then
         cellmask_initialized = .true.
         return
      end if

      !> allocate maximum size arrays
      call realloc(x_poly_min, polygon_points, keepExisting=.false.)
      call realloc(x_poly_max, polygon_points, keepExisting=.false.)
      call realloc(y_poly_min, polygon_points, keepExisting=.false.)
      call realloc(y_poly_max, polygon_points, keepExisting=.false.)
      call realloc(i_poly_start, polygon_points, keepExisting=.false.)
      call realloc(i_poly_end, polygon_points, keepExisting=.false.)
      call realloc(polygon_type, polygon_points, keepExisting=.false.)

      i_point = 1
      i_poly = 0

      do while (i_point < polygon_points)
         i_poly = i_poly + 1

         !> obtain start and end indices of polygon with generic subarray extraction routine, then correct them
         call get_startend(polygon_points - i_point + 1, x_poly(i_point:polygon_points), y_poly(i_point:polygon_points), i_start, i_end, dmiss)
         i_start = i_start + i_point - 1
         i_end = i_end + i_point - 1

         if (i_start >= i_end .or. i_end > polygon_points) then
            exit
         end if

         x_poly_min(i_poly) = minval(x_poly(i_start:i_end))
         x_poly_max(i_poly) = maxval(x_poly(i_start:i_end))
         y_poly_min(i_poly) = minval(y_poly(i_start:i_end))
         y_poly_max(i_poly) = maxval(y_poly(i_start:i_end))

         i_poly_start(i_poly) = i_start
         i_poly_end(i_poly) = i_end
         polygon_type(i_poly) = z_poly(i_start)

         i_point = i_end + 2
      end do

      polygons = i_poly

      !> resize arrays to actual number of polygons
      call realloc(x_poly_min, polygons, keepExisting=.true.)
      call realloc(x_poly_max, polygons, keepExisting=.true.)
      call realloc(y_poly_min, polygons, keepExisting=.true.)
      call realloc(y_poly_max, polygons, keepExisting=.true.)
      call realloc(i_poly_start, polygons, keepExisting=.true.)
      call realloc(i_poly_end, polygons, keepExisting=.true.)
      call realloc(polygon_type, polygons, keepExisting=.true.)

      if (enable_binning) then
         call init_binned_polygon_edges()
      end if

      ! check if there are any enclosure polygons
      do i_poly = 1, polygons
         if (polygon_type(i_poly) < 0.0_dp .and. polygon_type(i_poly) /= dmiss) then
            enclosures_present = .true.
            exit
         end if
      end do
      cellmask_initialized = .true.

   end subroutine cellmask_from_polygon_set_init

   !> Check if a point should be masked, either is_inside a dry-area polygon or outside an enclosure polygon.
   elemental function cellmask_from_polygon_set(x, y) result(mask)

      integer :: mask
      real(kind=dp), intent(in) :: x, y !< Point coordinates

      integer :: count_drypoint, i_poly, num_enclosures
      logical :: found_inside_enclosure, is_inside
      real(kind=dp) :: z_poly_val

      mask = 0
      if (.not. cellmask_initialized) then
         return
      end if

      num_enclosures = 0
      count_drypoint = 0
      found_inside_enclosure = .false.
      is_inside = .false.

      ! Single loop over all polygons
      do i_poly = 1, polygons
         z_poly_val = polygon_type(i_poly)

         ! Bounding box check
         if (x < x_poly_min(i_poly) .or. x > x_poly_max(i_poly) .or. &
             y < y_poly_min(i_poly) .or. y > y_poly_max(i_poly)) then
            cycle
         end if

         ! Point-in-polygon test
         is_inside = pinpok_elemental(x, y, i_poly)

         if (z_poly_val == dmiss .or. z_poly_val > 0.0_dp) then
            ! Dry point polygon
            if (is_inside) then
               count_drypoint = count_drypoint + 1
            end if
         else if (z_poly_val < 0.0_dp .and. is_inside) then
            found_inside_enclosure = .true.
         end if
      end do

      ! Apply odd-even rule only if counting was needed
      if (jins == 1) then
         if (mod(count_drypoint, 2) == 1) then
            mask = 1
         end if
      else
         if (mod(count_drypoint, 2) == 0) then
            mask = 1
         end if
      end if

      ! if an enclosure is present, the point must lie is_inside at least one
      ! NOTE: this means we do not handle nested enclosure polygons.
      if (enclosures_present .and. .not. found_inside_enclosure) then
         mask = 1
      end if

   end function cellmask_from_polygon_set

   !> Clean up module-level cellmask polygon data structures.
   subroutine cellmask_from_polygon_set_cleanup()

      if (allocated(x_poly_min)) then
         deallocate (x_poly_min)
      end if
      if (allocated(x_poly_max)) then
         deallocate (x_poly_max)
      end if
      if (allocated(y_poly_min)) then
         deallocate (y_poly_min)
      end if
      if (allocated(y_poly_max)) then
         deallocate (y_poly_max)
      end if
      if (allocated(polygon_type)) then
         deallocate (polygon_type)
      end if
      if (allocated(i_poly_start)) then
         deallocate (i_poly_start)
      end if
      if (allocated(i_poly_end)) then
         deallocate (i_poly_end)
      end if
      if (allocated(i_poly_bin_start)) then
         deallocate (i_poly_bin_start)
      end if
      if (allocated(i_poly_num_bins)) then
         deallocate (i_poly_num_bins)
      end if
      if (allocated(edge_bin_offsets)) then
         deallocate (edge_bin_offsets)
      end if
      if (allocated(edge_indices)) then
         deallocate (edge_indices)
      end if
      if (allocated(y_poly_bin_scale)) then
         deallocate (y_poly_bin_scale)
      end if

      polygons = 0
      cellmask_initialized = .false.
      enclosures_present = .false.

   end subroutine cellmask_from_polygon_set_cleanup

!> Elemental wrapper for cellmask operations using module-level polygon arrays
   elemental function pinpok_elemental(x, y, i_poly) result(is_inside)
      use geometry_module, only: pinpok_raycast

      real(kind=dp), intent(in) :: x, y !< Point coordinates
      integer, intent(in) :: i_poly !< Polygon index
      logical :: is_inside !< Result

      integer :: i_bin, i_start, i_end, n_points
      logical :: use_binned_edges

      ! Get bounds for this polygon from module arrays
      i_start = i_poly_start(i_poly)
      i_end = i_poly_end(i_poly)
      n_points = i_end - i_start + 1

      use_binned_edges = .false.
      if (allocated(edge_indices)) then
         use_binned_edges = i_poly_num_bins(i_poly) > 1
      end if

      if (use_binned_edges) then
         i_bin = get_edge_bin(y, y_poly_min(i_poly), y_poly_bin_scale(i_poly), i_poly_num_bins(i_poly))
         i_bin = i_poly_bin_start(i_poly) + i_bin - 1
         is_inside = pinpok_raycast(x, y, xpl_cache(i_start:i_end), ypl_cache(i_start:i_end), n_points, &
                                   edge_indices(edge_bin_offsets(i_bin):edge_bin_offsets(i_bin + 1) - 1))
      else
         is_inside = pinpok_raycast(x, y, xpl_cache(i_start:i_end), ypl_cache(i_start:i_end), n_points)
      end if

   end function pinpok_elemental

   !> Build a latitude-binned edge index if it benefits the complete polygon set.
   !!
   !! A horizontal ray can intersect only edges whose vertical extent contains the query y-coordinate.
   !! The index stores those candidate edges per latitude bin. It changes only which edges reach the
   !! existing ray-casting calculation; it does not approximate or simplify polygon geometry.
   subroutine init_binned_polygon_edges()
      integer :: bins, binned_edges, i_poly, num_edges, total_bins
      integer(kind=int64) :: memberships, total_memberships

      allocate (i_poly_bin_start(polygons), i_poly_num_bins(polygons), y_poly_bin_scale(polygons))
      binned_edges = 0
      total_bins = 0
      total_memberships = 0_int64

      do i_poly = 1, polygons
         num_edges = i_poly_end(i_poly) - i_poly_start(i_poly) + 1
         if (num_edges >= min_edges_for_binning .and. y_poly_max(i_poly) > y_poly_min(i_poly)) then
            bins = min(max_edge_bins, max(1, num_edges / 4))
            y_poly_bin_scale(i_poly) = real(bins, dp) / (y_poly_max(i_poly) - y_poly_min(i_poly))
            binned_edges = binned_edges + num_edges
         else
            bins = 1
            y_poly_bin_scale(i_poly) = 0.0_dp
         end if
         i_poly_num_bins(i_poly) = bins
         if (bins == 1) then
            cycle
         end if
         total_bins = total_bins + bins

         memberships = count_edge_memberships(i_poly, bins)
         total_memberships = total_memberships + memberships
      end do

      ! Keep the direct scan if no individual polygon benefits, or if long edges make the index too large.
      if (total_bins == 0 .or. total_memberships > max_memberships_per_edge * int(binned_edges, int64)) then
         deallocate (i_poly_bin_start, i_poly_num_bins, y_poly_bin_scale)
         return
      end if

      call build_binned_edge_index(total_bins, total_memberships)

   end subroutine init_binned_polygon_edges

   !> Allocate and populate the shared compressed-row storage for the complete polygon set.
   subroutine build_binned_edge_index(total_bins, total_memberships)
      integer, intent(in) :: total_bins !< Total number of bins in the polygon set.
      integer(kind=int64), intent(in) :: total_memberships !< Total edge entries in the polygon set.

      integer :: bin_edge_counts(max_edge_bins), bin_write_positions(max_edge_bins)
      integer :: bin_first, bin_last, bins, edge, i_bin, i_poly, num_edges
      integer :: next_bin, next_edge_entry

      ! edge_bin_offsets/edge_indices use compressed-row storage: each bin owns one contiguous slice.
      allocate (edge_bin_offsets(total_bins + 1), edge_indices(int(total_memberships)))
      next_bin = 1
      next_edge_entry = 1
      do i_poly = 1, polygons
         bins = i_poly_num_bins(i_poly)
         if (bins == 1) then
            cycle
         end if
         call count_edges_per_bin(i_poly, bins, bin_edge_counts)
         i_poly_bin_start(i_poly) = next_bin
         do i_bin = 1, bins
            edge_bin_offsets(next_bin) = next_edge_entry
            next_edge_entry = next_edge_entry + bin_edge_counts(i_bin)
            next_bin = next_bin + 1
         end do
      end do
      edge_bin_offsets(total_bins + 1) = next_edge_entry

      ! Insert local edge numbers in polygon order. The indexed ray cast therefore sums crossings
      ! in exactly the same order as the full scan, preserving floating-point and boundary behavior.
      do i_poly = 1, polygons
         bins = i_poly_num_bins(i_poly)
         if (bins == 1) then
            cycle
         end if
         do i_bin = 1, bins
            bin_write_positions(i_bin) = edge_bin_offsets(i_poly_bin_start(i_poly) + i_bin - 1)
         end do

         num_edges = i_poly_end(i_poly) - i_poly_start(i_poly) + 1
         do edge = 1, num_edges
            call edge_bin_range(i_poly, edge, bins, bin_first, bin_last)
            do i_bin = bin_first, bin_last
               edge_indices(bin_write_positions(i_bin)) = edge
               bin_write_positions(i_bin) = bin_write_positions(i_bin) + 1
            end do
         end do
      end do

   end subroutine build_binned_edge_index

   !> Count the storage needed by a polygon edge index without allocating or iterating over memberships.
   function count_edge_memberships(i_poly, bins) result(memberships)
      integer, intent(in) :: i_poly, bins !< Polygon index and number of bins.
      integer(kind=int64) :: memberships !< Total edge-to-bin memberships.

      integer :: bin_first, bin_last, edge, num_edges

      memberships = 0_int64
      num_edges = i_poly_end(i_poly) - i_poly_start(i_poly) + 1
      do edge = 1, num_edges
         call edge_bin_range(i_poly, edge, bins, bin_first, bin_last)
         memberships = memberships + int(bin_last - bin_first + 1, int64)
      end do
   end function count_edge_memberships

   !> Count candidate edges in each latitude bin for one polygon.
   subroutine count_edges_per_bin(i_poly, bins, bin_counts)
      integer, intent(in) :: i_poly, bins !< Polygon index and number of bins.
      integer, intent(out) :: bin_counts(:) !< Edge count per bin.

      integer :: bin_first, bin_last, edge, i_bin, num_edges

      bin_counts(1:bins) = 0
      num_edges = i_poly_end(i_poly) - i_poly_start(i_poly) + 1
      do edge = 1, num_edges
         call edge_bin_range(i_poly, edge, bins, bin_first, bin_last)
         do i_bin = bin_first, bin_last
            bin_counts(i_bin) = bin_counts(i_bin) + 1
         end do
      end do
   end subroutine count_edges_per_bin

   !> Get the inclusive bin range intersected by an edge.
   !! Both edge endpoints and queries use get_edge_bin, so equal y-coordinates always map to the same
   !! bin. Filling every bin from the lower endpoint through the upper endpoint preserves all edges
   !! that the full ray cast could inspect at a query's y-coordinate.
   pure subroutine edge_bin_range(i_poly, edge, bins, bin_first, bin_last)
      integer, intent(in) :: i_poly, edge, bins !< Polygon index, local edge index and number of bins.
      integer, intent(out) :: bin_first, bin_last !< First and last intersected bins.

      integer :: current_point, previous_point
      real(kind=dp) :: lower_tolerance, lower_y, upper_tolerance, upper_y

      current_point = i_poly_start(i_poly) + edge - 1
      previous_point = current_point - 1
      if (edge == 1) then
         previous_point = i_poly_end(i_poly)
      end if
      lower_y = min(ypl_cache(previous_point), ypl_cache(current_point))
      upper_y = max(ypl_cache(previous_point), ypl_cache(current_point))

      ! pinpok_raycast treats y-coordinates within 2 epsilon as equal. Use a larger halo so an edge
      ! remains a candidate when a near-equal query falls in the neighboring bin.
      lower_tolerance = 4.0_dp * epsilon(lower_y) * max(abs(lower_y), 1.0_dp)
      upper_tolerance = 4.0_dp * epsilon(upper_y) * max(abs(upper_y), 1.0_dp)
      bin_first = get_edge_bin(lower_y - lower_tolerance, y_poly_min(i_poly), y_poly_bin_scale(i_poly), bins)
      bin_last = get_edge_bin(upper_y + upper_tolerance, y_poly_min(i_poly), y_poly_bin_scale(i_poly), bins)
   end subroutine edge_bin_range

   !> Map a y-coordinate to a clamped one-based latitude bin.
   pure integer function get_edge_bin(y, y_min, bin_scale, bins) result(i_bin)
      real(kind=dp), intent(in) :: y, y_min, bin_scale !< Coordinate, polygon minimum and bin scale.
      integer, intent(in) :: bins !< Number of bins.

      i_bin = int((y - y_min) * bin_scale) + 1
      i_bin = max(1, min(bins, i_bin))
   end function get_edge_bin

   !> Manually init geometry cache (used for dry points, test_pol_to_cellmask)
   subroutine init_geom_cache(npl_init, xpl_init, ypl_init, zpl_init)
      use m_alloc

      integer, intent(in) :: npl_init
      real(kind=dp), intent(in) :: xpl_init(npl_init), ypl_init(npl_init), zpl_init(npl_init)

      call realloc(xpl_cache, npl_init, keepExisting=.false.)
      call realloc(ypl_cache, npl_init, keepExisting=.false.)
      call realloc(zpl_cache, npl_init, keepExisting=.false.)

      xpl_cache = xpl_init
      ypl_cache = ypl_init
      zpl_cache = zpl_init
      npl_cache = npl_init


   end subroutine init_geom_cache

   !> Initialize xpl, ypl, zpl arrays with all netcell geometries (called once)
   subroutine init_cell_geom_as_polylines()
      use network_data
      use m_alloc

      integer :: k, n, k1, total_points, ipoint
      real(kind=dp), allocatable, dimension(:) :: xpl_init, ypl_init, zpl_init      

      if (cellmask_initialized) then !> reuse cellmask cache boolean
         call cleanup_cell_geom_polylines()
      end if

      ! calculate total points needed: sum(netcell(k)%n + 1) for all cells
      ! +1 for dmiss separator after each polygon
      total_points = 0
      do k = 1, nump
         total_points = total_points + netcell(k)%n + 1 ! +1 for dmiss
      end do

      ! allocate or reallocate xpl, ypl, zpl
      call realloc(xpl_init, total_points, keepexisting=.false.)
      call realloc(ypl_init, total_points, keepexisting=.false.)
      call realloc(zpl_init, total_points, keepexisting=.false.)

      ! fill arrays with netcell geometry
      ipoint = 0
      do k = 1, nump
         do n = 1, netcell(k)%n
            ipoint = ipoint + 1
            k1 = netcell(k)%nod(n)
            xpl_init(ipoint) = xk(k1)
            ypl_init(ipoint) = yk(k1)
            zpl_init(ipoint) = real(k, dp) ! store cell index as z-value
         end do

         ! add separator
         ipoint = ipoint + 1
         xpl_init(ipoint) = dmiss
         ypl_init(ipoint) = dmiss
         zpl_init(ipoint) = dmiss
      end do

      npl_cache = ipoint

      ! initialize the cellmask module with these polygons
      ! this builds bounding boxes and polygon indices
      call cellmask_from_polygon_set_init(npl_cache, xpl_init, ypl_init, zpl_init, enable_binning=.false.)

   end subroutine init_cell_geom_as_polylines

   !> call general polygon cleanup and restore previous polygon data
   subroutine cleanup_cell_geom_polylines()
      call cellmask_from_polygon_set_cleanup()
   end subroutine cleanup_cell_geom_polylines

!> Fast replacement for INCELLS using cached geometry in global polygon arrays
   elemental function point_find_netcell(x, y) result(k)

      real(kind=dp), intent(in) :: x, y !< coordinates of point to locate enclosing netcell
      integer :: k !< cell number of enclosing netcell, or 0 if not found

      integer :: i_poly
      logical :: is_inside

      k = 0

      ! Loop over all netcell polygons with fast bounding box checks
      do i_poly = 1, polygons

         ! Quick bbox rejection
         if (x < x_poly_min(i_poly) .or. x > x_poly_max(i_poly) .or. &
             y < y_poly_min(i_poly) .or. y > y_poly_max(i_poly)) then
            cycle
         end if

         ! Detailed point-in-polygon check
         is_inside = pinpok_elemental(x, y, i_poly)

         if (is_inside) then
            ! cell index equals polygon index
            k = i_poly
            return
         end if
      end do

   end function point_find_netcell

!> Find all cells crossed by polyline using brute force on cached geometry. The routine is inclusive of edge cases (touching edges or vertices).
   subroutine find_cells_crossed_by_polyline(xpoly, ypoly, crossed_cells, error)
      use m_alloc, only: realloc
      use network_data, only: nump
      use m_missing, only: dmiss

      implicit none

      real(kind=dp), dimension(:), intent(in) :: xpoly !< Polyline x-coordinates
      real(kind=dp), dimension(:), intent(in) :: ypoly !< Polyline y-coordinates
      integer, dimension(:), allocatable, intent(out) :: crossed_cells !> Indices of crossed cells in network_data::netcells
      character, dimension(:), allocatable, intent(out) :: error !> Error message, empty if no error, to be handled at call site

      integer :: npoly, i
      integer, allocatable :: cellmask(:) !< (nump) Mask array for net cells

      error = ''

      npoly = size(xpoly)
      if (any(xpoly == dmiss) .or. any(ypoly == dmiss)) then
         error = 'Invalid polyline input'
         return
      end if
      
      call realloc(cellmask, nump, keepexisting=.false., fill=0)

      ! Process each segment and put the result in cellmask
      do i = 1, npoly - 1
         call find_cells_for_segment(xpoly(i), ypoly(i), xpoly(i + 1), ypoly(i + 1), cellmask)
      end do

      crossed_cells = pack([(i, i=1, nump)], mask=(cellmask == 1))
      if (size(crossed_cells) == 0) then !> check whether the whole polyline lies in a single cell if no boundaries were crossed
         i = point_find_netcell(xpoly(1),ypoly(1))
         if (i > 0) then
            crossed_cells = [i]
         end if
      end if

   end subroutine find_cells_crossed_by_polyline

!> Find all cells that a segment crosses and mark them in cellmask
   subroutine find_cells_for_segment(xa, ya, xb, yb, cellmask)

      implicit none

      real(kind=dp), intent(in) :: xa, ya, xb, yb !< Segment endpoints
      integer, intent(inout) :: cellmask(:) !< Cell mask array: 1=crossed, 0=not crossed

      real(kind=dp) :: seg_xmin, seg_xmax, seg_ymin, seg_ymax
      integer :: i_poly, i_point, i_start, i_end, n_points
      integer :: i, ip1
      logical :: intersects

      ! Segment bounding box
      seg_xmin = min(xa, xb)
      seg_xmax = max(xa, xb)
      seg_ymin = min(ya, yb)
      seg_ymax = max(ya, yb)

      !$OMP PARALLEL DO SCHEDULE(GUIDED) PRIVATE(i_start, i_end, n_points, i, i_point, ip1, intersects)
      do i_poly = 1, polygons
         ! Skip if already marked
         if (cellmask(i_poly) == 1) then
            cycle
         end if

         ! Quick bbox rejection
         if (seg_xmax < x_poly_min(i_poly) .or. seg_xmin > x_poly_max(i_poly) .or. &
             seg_ymax < y_poly_min(i_poly) .or. seg_ymin > y_poly_max(i_poly)) then
            cycle
         end if

         ! Get cached polygon geometry
         i_start = i_poly_start(i_poly)
         i_end = i_poly_end(i_poly)
         n_points = i_end - i_start + 1

         ! Check if segment crosses ANY edge of this cached polygon
         do i = 0, n_points - 1
            i_point = i_start + i
            ip1 = i_point + 1
            if (ip1 > i_end) ip1 = i_start ! Wrap around

            intersects = line_segments_intersect(xa, ya, xb, yb, xpl_cache(i_point), ypl_cache(i_point), xpl_cache(ip1), ypl_cache(ip1))

            if (intersects) then
               cellmask(i_poly) = 1
               exit ! No need to check other edges
            end if
         end do
      end do
      !$OMP END PARALLEL DO

   end subroutine find_cells_for_segment

!> Check if two line segments intersect
   elemental function line_segments_intersect(x1a, y1a, x1b, y1b, x2a, y2a, x2b, y2b) result(intersects)
      use precision, only: dp

      real(kind=dp), intent(in) :: x1a, y1a, x1b, y1b !< First line segment endpoints
      real(kind=dp), intent(in) :: x2a, y2a, x2b, y2b !< Second line segment endpoints
      logical :: intersects !< True if segments intersect

      real(kind=dp) :: dx1, dy1, dx2, dy2
      real(kind=dp) :: denom, t1, t2
      real(kind=dp), parameter :: EPS = 1.0e-10_dp

      intersects = .false.
      t1 = -1.0_dp

      dx1 = x1b - x1a
      dy1 = y1b - y1a
      dx2 = x2b - x2a
      dy2 = y2b - y2a

      denom = dx1 * dy2 - dy1 * dx2
      if (abs(denom) < EPS) then !> parallel or collinear, no intersection
         if (point_to_line_distance(x1a, y1a, x2a, y2a, x2b, y2b) < EPS) then
            intersects = .true. !> include collinear as intersecting
         end if
         return
      end if

      t1 = ((x2a - x1a) * dy2 - (y2a - y1a) * dx2) / denom
      t2 = ((x2a - x1a) * dy1 - (y2a - y1a) * dx1) / denom

      !> small epsilon margin to be inclusive of endpoints
      if (t1 > -EPS .and. t1 <= 1.0_dp + EPS .and. &
          t2 > -EPS .and. t2 <= 1.0_dp + EPS) then
         intersects = .true.
      end if

   end function line_segments_intersect

!> Compute distance from a point to the infinite extension of a line (not clamped to segment)
   elemental function point_to_line_distance(px, py, x1, y1, x2, y2) result(dist)
      use precision, only: dp

      real(kind=dp), intent(in) :: px, py !< Point coordinates x and y
      real(kind=dp), intent(in) :: x1, y1, x2, y2 !< line start and end coordinates
      real(kind=dp) :: dist

      real(kind=dp) :: dx, dy, line_length, cross_product

      dx = x2 - x1
      dy = y2 - y1
      line_length = sqrt(dx * dx + dy * dy)

      if (line_length < 1.0e-20_dp) then
         ! Degenerate line - return distance to point
         dist = sqrt((px - x1)**2 + (py - y1)**2)
         return
      end if

      ! Distance from point to line = |cross product| / |line vector|
      ! Cross product in 2D: (p - p1) × (p2 - p1)
      cross_product = abs((px - x1) * dy - (py - y1) * dx)
      dist = cross_product / line_length

   end function point_to_line_distance

end module m_cellmask_from_polygon_set
