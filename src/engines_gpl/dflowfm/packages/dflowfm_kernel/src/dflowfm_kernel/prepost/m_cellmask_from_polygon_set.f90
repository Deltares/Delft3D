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

   public :: t_polygon_set_cache

   !> Coordinates and per-polygon metadata shared by masking and net-cell lookup.
   type, private :: t_polygon_geometry
      real(kind=dp), allocatable :: x(:), y(:)
      real(kind=dp), allocatable :: x_min(:), y_min(:), x_max(:), y_max(:)
      logical, allocatable :: is_enclosure(:)
      integer, allocatable :: polygon_start(:), polygon_end(:)
      integer :: polygon_count = 0
      logical :: enclosures_present = .false.
   end type t_polygon_geometry

   interface t_polygon_geometry
      module procedure construct_polygon_geometry
   end interface t_polygon_geometry

   !> Latitude-binned lookup table containing ordered candidate polygon edges for ray casting.
   type, private :: t_binned_edge_index
      private
      integer, allocatable :: polygon_bin_start(:) !< First global bin for each polygon.
      integer, allocatable :: polygon_num_bins(:) !< Number of latitude bins for each polygon.
      integer, allocatable :: bin_offsets(:) !< CSR offsets into edge_indices for each global bin.
      integer, allocatable :: edge_indices(:) !< Ordered local edge indices for all bins.
      real(kind=dp), allocatable :: polygon_bin_scale(:) !< Scale from polygon y-coordinate to local bin.
   contains
      procedure :: point_is_inside => binned_edge_index_point_is_inside
      procedure, private :: populate_bin_storage => binned_edge_index_populate_bin_storage
      procedure, private :: count_edge_memberships => binned_edge_index_count_edge_memberships
      procedure, private :: count_edges_per_bin => binned_edge_index_count_edges_per_bin
      procedure, private :: edge_bin_range => binned_edge_index_edge_bin_range
      procedure, nopass, private :: get_bin => binned_edge_index_get_bin
   end type t_binned_edge_index

   interface t_binned_edge_index
      module procedure construct_binned_edge_index
   end interface t_binned_edge_index

   !> Complete cached polygon set, including optional acceleration data and lifecycle state.
   type :: t_polygon_set_cache
      private
      type(t_polygon_geometry) :: geometry
      type(t_binned_edge_index) :: edge_index
   contains
      procedure :: point_mask => polygon_set_cache_point_mask
      procedure :: polygon_contains_point => polygon_set_cache_polygon_contains_point
      procedure :: find_netcell => polygon_set_cache_find_netcell
      procedure :: find_cells_crossed_by_polyline => polygon_set_cache_find_cells_crossed_by_polyline
   end type t_polygon_set_cache

   interface t_polygon_set_cache
      module procedure construct_polygon_set_cache
      module procedure construct_netcell_polygon_cache
   end interface t_polygon_set_cache

   integer, parameter :: max_edge_bins = 1024 !< Maximum number of latitude bins per polygon.
   integer(kind=int64), parameter :: max_memberships_per_edge = 8_int64 !< Memory/work cap for vertically long edges.

contains

   !> Classify a point using only the ordered polygon edges in its latitude bin.
   pure function binned_edge_index_point_is_inside(this, x, y, i_poly, geometry) result(is_inside)
      use geometry_module, only: pinpok_raycast

      class(t_binned_edge_index), intent(in) :: this
      real(kind=dp), intent(in) :: x, y !< Query coordinates.
      integer, intent(in) :: i_poly !< Polygon index in this index.
      type(t_polygon_geometry), intent(in) :: geometry !< Cached polygon coordinates and metadata.
      logical :: is_inside

      integer :: candidate_first, candidate_last, first_point, global_bin, last_point, local_bin, num_points

      first_point = geometry%polygon_start(i_poly)
      last_point = geometry%polygon_end(i_poly)
      num_points = last_point - first_point + 1
      local_bin = this%get_bin(y, geometry%y_min(i_poly), this%polygon_bin_scale(i_poly), this%polygon_num_bins(i_poly))
      global_bin = this%polygon_bin_start(i_poly) + local_bin - 1
      candidate_first = this%bin_offsets(global_bin)
      candidate_last = this%bin_offsets(global_bin + 1) - 1
      is_inside = pinpok_raycast(x, y, geometry%x(first_point:last_point), geometry%y(first_point:last_point), num_points, &
                                 this%edge_indices(candidate_first:candidate_last))
   end function binned_edge_index_point_is_inside

   !> Construct a consistent polygon cache from packed coordinates separated by missing values.
   function construct_polygon_set_cache(x_poly, y_poly, z_poly, enable_binning) result(cache)
      real(kind=dp), intent(in) :: x_poly(:), y_poly(:), z_poly(:) !< Packed polygon coordinate arrays.
      logical, intent(in) :: enable_binning !< Whether to build a latitude-binned edge index for large polygon sets.
      type(t_polygon_set_cache) :: cache

      cache%geometry = t_polygon_geometry(x_poly, y_poly, z_poly)
      if (enable_binning .and. cache%geometry%polygon_count > 0) then
         cache%edge_index = t_binned_edge_index(cache%geometry)
      end if

   end function construct_polygon_set_cache

   !> Construct polygon geometry and metadata from packed coordinates separated by missing values.
   function construct_polygon_geometry(x_poly, y_poly, z_poly) result(geometry)
      use m_alloc
      use geometry_module, only: get_startend

      real(kind=dp), intent(in) :: x_poly(:), y_poly(:), z_poly(:) !< Packed polygon coordinate arrays.
      type(t_polygon_geometry) :: geometry

      integer :: i_point, i_start, i_end, i_poly, polygon_points

      polygon_points = size(x_poly)
      call realloc(geometry%x, polygon_points, keepExisting=.false.)
      call realloc(geometry%y, polygon_points, keepExisting=.false.)
      geometry%x = x_poly
      geometry%y = y_poly

      if (polygon_points == 0) then
         return
      end if

      !> allocate maximum size arrays
      call realloc(geometry%x_min, polygon_points, keepExisting=.false.)
      call realloc(geometry%x_max, polygon_points, keepExisting=.false.)
      call realloc(geometry%y_min, polygon_points, keepExisting=.false.)
      call realloc(geometry%y_max, polygon_points, keepExisting=.false.)
      call realloc(geometry%polygon_start, polygon_points, keepExisting=.false.)
      call realloc(geometry%polygon_end, polygon_points, keepExisting=.false.)
      call realloc(geometry%is_enclosure, polygon_points, keepExisting=.false.)

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

         geometry%x_min(i_poly) = minval(x_poly(i_start:i_end))
         geometry%x_max(i_poly) = maxval(x_poly(i_start:i_end))
         geometry%y_min(i_poly) = minval(y_poly(i_start:i_end))
         geometry%y_max(i_poly) = maxval(y_poly(i_start:i_end))

         geometry%polygon_start(i_poly) = i_start
         geometry%polygon_end(i_poly) = i_end
         geometry%is_enclosure(i_poly) = z_poly(i_start) /= dmiss .and. z_poly(i_start) <= 0.0_dp

         i_point = i_end + 2
      end do

      geometry%polygon_count = i_poly

      !> resize arrays to actual number of polygons
      call realloc(geometry%x_min, geometry%polygon_count, keepExisting=.true.)
      call realloc(geometry%x_max, geometry%polygon_count, keepExisting=.true.)
      call realloc(geometry%y_min, geometry%polygon_count, keepExisting=.true.)
      call realloc(geometry%y_max, geometry%polygon_count, keepExisting=.true.)
      call realloc(geometry%polygon_start, geometry%polygon_count, keepExisting=.true.)
      call realloc(geometry%polygon_end, geometry%polygon_count, keepExisting=.true.)
      call realloc(geometry%is_enclosure, geometry%polygon_count, keepExisting=.true.)
      geometry%enclosures_present = any(geometry%is_enclosure)

   end function construct_polygon_geometry

   !> Check if a point should be masked, either is_inside a dry-area polygon or outside an enclosure polygon.
   elemental function polygon_set_cache_point_mask(this, x, y) result(mask)
      class(t_polygon_set_cache), intent(in) :: this
      integer :: mask
      real(kind=dp), intent(in) :: x, y !< Point coordinates

      integer :: count_drypoint, i_poly
      logical :: found_inside_enclosure, is_inside

      mask = 0
      associate (geometry => this%geometry)
         count_drypoint = 0
         found_inside_enclosure = .false.

         ! Single loop over all polygons
         do i_poly = 1, geometry%polygon_count
            ! Bounding box check
            if (x < geometry%x_min(i_poly) .or. x > geometry%x_max(i_poly) .or. &
                y < geometry%y_min(i_poly) .or. y > geometry%y_max(i_poly)) then
               cycle
            end if

            ! Point-in-polygon test
            is_inside = this%polygon_contains_point(x, y, i_poly)

            if (is_inside) then
               if (geometry%is_enclosure(i_poly)) then
                  found_inside_enclosure = .true.
               else
                  count_drypoint = count_drypoint + 1
               end if
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
         if (geometry%enclosures_present .and. .not. found_inside_enclosure) then
            mask = 1
         end if
      end associate

   end function polygon_set_cache_point_mask

   !> Test whether a point lies inside one cached polygon.
   elemental function polygon_set_cache_polygon_contains_point(this, x, y, i_poly) result(is_inside)
      use geometry_module, only: pinpok_raycast

      class(t_polygon_set_cache), intent(in) :: this
      real(kind=dp), intent(in) :: x, y !< Point coordinates
      integer, intent(in) :: i_poly !< Polygon index
      logical :: is_inside !< Result

      integer :: i_start, i_end, n_points

      ! Get bounds for this polygon from module arrays
      associate (geometry => this%geometry)
         i_start = geometry%polygon_start(i_poly)
         i_end = geometry%polygon_end(i_poly)
         n_points = i_end - i_start + 1

         if (allocated(this%edge_index%edge_indices)) then
            is_inside = this%edge_index%point_is_inside(x, y, i_poly, geometry)
         else
            is_inside = pinpok_raycast(x, y, geometry%x(i_start:i_end), geometry%y(i_start:i_end), n_points)
         end if
      end associate

   end function polygon_set_cache_polygon_contains_point

   !> Build a latitude-binned edge index for the complete polygon set.
   !!
   !! A horizontal ray can intersect only edges whose vertical extent contains the query y-coordinate.
   !! The index stores those candidate edges per latitude bin. It changes only which edges reach the
   !! existing ray-casting calculation; it does not approximate or simplify polygon geometry.
   function construct_binned_edge_index(geometry) result(index)
      type(t_polygon_geometry), intent(in) :: geometry !< Polygon geometry to index.
      type(t_binned_edge_index) :: index

      integer :: bins, i_poly, num_edges, total_bins, total_edges
      integer(kind=int64) :: memberships, total_memberships

      allocate (index%polygon_bin_start(geometry%polygon_count), index%polygon_num_bins(geometry%polygon_count), &
                index%polygon_bin_scale(geometry%polygon_count))
      total_edges = 0
      total_bins = 0
      total_memberships = 0_int64

      do i_poly = 1, geometry%polygon_count
         num_edges = geometry%polygon_end(i_poly) - geometry%polygon_start(i_poly) + 1
         if (geometry%y_max(i_poly) > geometry%y_min(i_poly)) then
            bins = min(max_edge_bins, max(1, num_edges / 4))
            index%polygon_bin_scale(i_poly) = real(bins, dp) / (geometry%y_max(i_poly) - geometry%y_min(i_poly))
         else
            bins = 1
            index%polygon_bin_scale(i_poly) = 0.0_dp
         end if
         index%polygon_num_bins(i_poly) = bins
         total_edges = total_edges + num_edges
         total_bins = total_bins + bins

         memberships = index%count_edge_memberships(geometry, i_poly, bins)
         total_memberships = total_memberships + memberships
      end do

      ! Keep the direct scan if long edges make the index too large.
      if (total_memberships > max_memberships_per_edge * int(total_edges, int64)) then
         deallocate (index%polygon_bin_start, index%polygon_num_bins, index%polygon_bin_scale)
         return
      end if

      call index%populate_bin_storage(geometry, total_bins, total_memberships)

   end function construct_binned_edge_index

   !> Allocate and populate the shared compressed-row storage for the complete polygon set.
   subroutine binned_edge_index_populate_bin_storage(this, geometry, total_bins, total_memberships)
      class(t_binned_edge_index), intent(inout) :: this
      type(t_polygon_geometry), intent(in) :: geometry !< Polygon geometry being indexed.
      integer, intent(in) :: total_bins !< Total number of bins in the polygon set.
      integer(kind=int64), intent(in) :: total_memberships !< Total edge entries in the polygon set.

      integer :: bin_edge_counts(max_edge_bins), bin_write_positions(max_edge_bins)
      integer :: bin_first, bin_last, bins, edge, i_bin, i_poly, num_edges
      integer :: next_bin, next_edge_entry

      ! edge_bin_offsets/edge_indices use compressed-row storage: each bin owns one contiguous slice.
      allocate (this%bin_offsets(total_bins + 1), this%edge_indices(int(total_memberships)))
      next_bin = 1
      next_edge_entry = 1
      do i_poly = 1, geometry%polygon_count
         bins = this%polygon_num_bins(i_poly)
         call this%count_edges_per_bin(geometry, i_poly, bins, bin_edge_counts)
         this%polygon_bin_start(i_poly) = next_bin
         do i_bin = 1, bins
            this%bin_offsets(next_bin) = next_edge_entry
            next_edge_entry = next_edge_entry + bin_edge_counts(i_bin)
            next_bin = next_bin + 1
         end do
      end do
      this%bin_offsets(total_bins + 1) = next_edge_entry

      ! Insert local edge numbers in polygon order. The indexed ray cast therefore sums crossings
      ! in exactly the same order as the full scan, preserving floating-point and boundary behavior.
      do i_poly = 1, geometry%polygon_count
         bins = this%polygon_num_bins(i_poly)
         do i_bin = 1, bins
            bin_write_positions(i_bin) = &
               this%bin_offsets(this%polygon_bin_start(i_poly) + i_bin - 1)
         end do

         num_edges = geometry%polygon_end(i_poly) - geometry%polygon_start(i_poly) + 1
         do edge = 1, num_edges
            call this%edge_bin_range(geometry, i_poly, edge, bins, bin_first, bin_last)
            do i_bin = bin_first, bin_last
               this%edge_indices(bin_write_positions(i_bin)) = edge
               bin_write_positions(i_bin) = bin_write_positions(i_bin) + 1
            end do
         end do
      end do

   end subroutine binned_edge_index_populate_bin_storage

   !> Count memberships in O(edges), without visiting every bin that an edge spans.
   function binned_edge_index_count_edge_memberships(this, geometry, i_poly, bins) result(memberships)
      class(t_binned_edge_index), intent(in) :: this
      type(t_polygon_geometry), intent(in) :: geometry !< Polygon geometry being indexed.
      integer, intent(in) :: i_poly, bins !< Polygon index and number of bins.
      integer(kind=int64) :: memberships !< Total edge-to-bin memberships.

      integer :: bin_first, bin_last, edge, num_edges

      memberships = 0_int64
      num_edges = geometry%polygon_end(i_poly) - geometry%polygon_start(i_poly) + 1
      do edge = 1, num_edges
         call this%edge_bin_range(geometry, i_poly, edge, bins, bin_first, bin_last)
         memberships = memberships + int(bin_last - bin_first + 1, int64)
      end do
   end function binned_edge_index_count_edge_memberships

   !> Count candidate edges in each latitude bin for one polygon.
   subroutine binned_edge_index_count_edges_per_bin(this, geometry, i_poly, bins, bin_counts)
      class(t_binned_edge_index), intent(in) :: this
      type(t_polygon_geometry), intent(in) :: geometry !< Polygon geometry being indexed.
      integer, intent(in) :: i_poly, bins !< Polygon index and number of bins.
      integer, intent(out) :: bin_counts(:) !< Edge count per bin.

      integer :: bin_first, bin_last, edge, i_bin, num_edges

      bin_counts(1:bins) = 0
      num_edges = geometry%polygon_end(i_poly) - geometry%polygon_start(i_poly) + 1
      do edge = 1, num_edges
         call this%edge_bin_range(geometry, i_poly, edge, bins, bin_first, bin_last)
         do i_bin = bin_first, bin_last
            bin_counts(i_bin) = bin_counts(i_bin) + 1
         end do
      end do
   end subroutine binned_edge_index_count_edges_per_bin

   !> Get the inclusive bin range intersected by an edge.
   !! Both edge endpoints and queries use get_edge_bin, so equal y-coordinates always map to the same
   !! bin. Filling every bin from the lower endpoint through the upper endpoint preserves all edges
   !! that the full ray cast could inspect at a query's y-coordinate.
   pure subroutine binned_edge_index_edge_bin_range(this, geometry, i_poly, edge, bins, bin_first, bin_last)
      class(t_binned_edge_index), intent(in) :: this
      type(t_polygon_geometry), intent(in) :: geometry !< Polygon geometry being indexed.
      integer, intent(in) :: i_poly, edge, bins !< Polygon index, local edge index and number of bins.
      integer, intent(out) :: bin_first, bin_last !< First and last intersected bins.

      integer :: current_point, previous_point
      real(kind=dp) :: lower_tolerance, lower_y, upper_tolerance, upper_y

      current_point = geometry%polygon_start(i_poly) + edge - 1
      previous_point = current_point - 1
      if (edge == 1) then
         previous_point = geometry%polygon_end(i_poly)
      end if
      lower_y = min(geometry%y(previous_point), geometry%y(current_point))
      upper_y = max(geometry%y(previous_point), geometry%y(current_point))

      ! pinpok_raycast treats y-coordinates within 2 epsilon as equal. Use a larger halo so an edge
      ! remains a candidate when a near-equal query falls in the neighboring bin.
      lower_tolerance = 4.0_dp * epsilon(lower_y) * max(abs(lower_y), 1.0_dp)
      upper_tolerance = 4.0_dp * epsilon(upper_y) * max(abs(upper_y), 1.0_dp)
      bin_first = this%get_bin(lower_y - lower_tolerance, geometry%y_min(i_poly), this%polygon_bin_scale(i_poly), bins)
      bin_last = this%get_bin(upper_y + upper_tolerance, geometry%y_min(i_poly), this%polygon_bin_scale(i_poly), bins)
   end subroutine binned_edge_index_edge_bin_range

   !> Map a y-coordinate to a clamped one-based latitude bin.
   pure integer function binned_edge_index_get_bin(y, y_min, bin_scale, bins) result(i_bin)
      real(kind=dp), intent(in) :: y, y_min, bin_scale !< Coordinate, polygon minimum and bin scale.
      integer, intent(in) :: bins !< Number of bins.

      i_bin = int((y - y_min) * bin_scale) + 1
      i_bin = max(1, min(bins, i_bin))
   end function binned_edge_index_get_bin

   !> Construct a polygon cache containing all net-cell geometries.
   function construct_netcell_polygon_cache() result(cache)
      use network_data
      use m_alloc

      type(t_polygon_set_cache) :: cache
      integer :: k, n, k1, total_points, ipoint
      real(kind=dp), allocatable, dimension(:) :: xpl_init, ypl_init, zpl_init

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

      cache = t_polygon_set_cache(xpl_init, ypl_init, zpl_init, enable_binning=.false.)

   end function construct_netcell_polygon_cache

!> Fast replacement for INCELLS using cached net-cell geometry.
   elemental function polygon_set_cache_find_netcell(this, x, y) result(k)

      class(t_polygon_set_cache), intent(in) :: this
      real(kind=dp), intent(in) :: x, y !< coordinates of point to locate enclosing netcell
      integer :: k !< cell number of enclosing netcell, or 0 if not found

      integer :: i_poly
      logical :: is_inside

      k = 0

      associate (geometry => this%geometry)
         ! Loop over all netcell polygons with fast bounding box checks
         do i_poly = 1, geometry%polygon_count

            ! Quick bbox rejection
            if (x < geometry%x_min(i_poly) .or. x > geometry%x_max(i_poly) .or. &
                y < geometry%y_min(i_poly) .or. y > geometry%y_max(i_poly)) then
               cycle
            end if

            ! Detailed point-in-polygon check
            is_inside = this%polygon_contains_point(x, y, i_poly)

            if (is_inside) then
               ! cell index equals polygon index
               k = i_poly
               return
            end if
         end do
      end associate

   end function polygon_set_cache_find_netcell

!> Find all cells crossed by polyline using brute force on cached geometry. The routine is inclusive of edge cases (touching edges or vertices).
   subroutine polygon_set_cache_find_cells_crossed_by_polyline(this, xpoly, ypoly, crossed_cells, error)
      use m_alloc, only: realloc
      use network_data, only: nump
      use m_missing, only: dmiss

      implicit none

      class(t_polygon_set_cache), intent(in) :: this
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
         call find_cells_for_segment(this, xpoly(i), ypoly(i), xpoly(i + 1), ypoly(i + 1), cellmask)
      end do

      crossed_cells = pack([(i, i=1, nump)], mask=(cellmask == 1))
      if (size(crossed_cells) == 0) then !> check whether the whole polyline lies in a single cell if no boundaries were crossed
         i = this%find_netcell(xpoly(1), ypoly(1))
         if (i > 0) then
            crossed_cells = [i]
         end if
      end if

   end subroutine polygon_set_cache_find_cells_crossed_by_polyline

!> Find all cells that a segment crosses and mark them in cellmask
   subroutine find_cells_for_segment(cache, xa, ya, xb, yb, cellmask)

      implicit none

      class(t_polygon_set_cache), intent(in) :: cache
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

      associate (geometry => cache%geometry)
         !$OMP PARALLEL DO SCHEDULE(GUIDED) PRIVATE(i_start, i_end, n_points, i, i_point, ip1, intersects)
         do i_poly = 1, geometry%polygon_count
            ! Skip if already marked
            if (cellmask(i_poly) == 1) then
               cycle
            end if

            ! Quick bbox rejection
            if (seg_xmax < geometry%x_min(i_poly) .or. seg_xmin > geometry%x_max(i_poly) .or. &
                seg_ymax < geometry%y_min(i_poly) .or. seg_ymin > geometry%y_max(i_poly)) then
               cycle
            end if

            ! Get cached polygon geometry
            i_start = geometry%polygon_start(i_poly)
            i_end = geometry%polygon_end(i_poly)
            n_points = i_end - i_start + 1

            ! Check if segment crosses ANY edge of this cached polygon
            do i = 0, n_points - 1
               i_point = i_start + i
               ip1 = i_point + 1
               if (ip1 > i_end) ip1 = i_start ! Wrap around

               intersects = line_segments_intersect(xa, ya, xb, yb, geometry%x(i_point), geometry%y(i_point), &
                                                    geometry%x(ip1), geometry%y(ip1))

               if (intersects) then
                  cellmask(i_poly) = 1
                  exit ! No need to check other edges
               end if
            end do
         end do
         !$OMP END PARALLEL DO
      end associate

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
