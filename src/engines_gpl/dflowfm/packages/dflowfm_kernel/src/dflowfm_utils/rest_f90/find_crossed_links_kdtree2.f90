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
module m_find_crossed_links_kdtree2
   implicit none

   integer, parameter :: ITYPE_NETLINK_DUAL = 1 !< cross with dual netlink
   integer, parameter :: ITYPE_FLOWLINK = 2 !< cross with flowlink
   integer, parameter :: ITYPE_NETLINK = 3 !< cross with netlink
   integer, parameter :: ITYPE_FLOWLINK_1D_DUAL = 4 !< cross with dual 1D flowlink

   integer, parameter :: BOUNDARY_NONE = 0 !< do not include boundary links
   integer, parameter :: BOUNDARY_ALL = 1 !< include all boundary links
   integer, parameter :: BOUNDARY_2D = 2 !< include only 2d boundary links

contains
!---------------------------------------------------------------
! the following subroutines use kdtree2
!---------------------------------------------------------------
!> find links crossed by polyline with kdtree2
   subroutine find_crossed_links_kdtree2(treeinst, NPL, xpl, ypl, itype, n_links_polyline_nodes, jaboundarylinks, intersection_count, crossed_links, polygon_nodes, polygon_segment_weights, ierror)
      use precision, only: dp
      use network_data, only: numL, kn, xk, yk
      use m_flowgeom, only: lnx, lnxi, lnx1db, ln, xz, yz, lnx1d, lncn
      use kdtree2Factory, only: kdtree_instance, build_kdtree, make_queryvector_kdtree, kdtree2_r_count, realloc_results_kdtree, kdtree2_n_nearest, itree_empty, delete_kdtree2
      use m_sferic, only: jsferic, jasfer3d
      use m_readyy, only: readyy
      use m_get_link_neighboring_cell_coords, only: get_link_neighboringcellcoords
      use m_movabs, only: movabs
      use m_lnabs, only: lnabs
      use messagehandling, only: LEVEL_INFO, LEVEL_ERROR, mess
      use m_missing, only: dmiss
      use geometry_module, only: dbdistance, crossinbox

      type(kdtree_instance), intent(inout) :: treeinst
      integer, intent(in) :: NPL !< polyline length
      real(kind=dp), dimension(NPL), intent(in) :: xpl, ypl !< polyline node coordinates
      integer, intent(in) :: itype !< Type of intersection (ITYPE_NETLINK_DUAL, ITYPE_FLOWLINK, ITYPE_NETLINK, ITYPE_FLOWLINK_1D_DUAL)
      integer, intent(in) :: n_links_polyline_nodes !< array_size e.g. number of links ( Lnx for flowlinks, numL for netlinks) or npl for number of polyline nodes
      integer, intent(in) :: jaboundarylinks !< include boundary links ( BOUNDARY_NONE, BOUNDARY_ALL, BOUNDARY_2D )
      integer, intent(out) :: intersection_count !< number of link intersections
      integer, dimension(n_links_polyline_nodes), intent(inout) :: crossed_links !< crossed flowlinks
      integer, dimension(n_links_polyline_nodes), intent(inout) :: polygon_nodes !< list of polygon starting nodes
      real(kind=dp), dimension(n_links_polyline_nodes), intent(inout) :: polygon_segment_weights !< polygon section cross location

      integer, intent(out) :: ierror !< ierror (1) or not (0)

      real(kind=dp), dimension(:), allocatable :: x, y

      integer, dimension(:), allocatable :: ipolsection
      real(kind=dp) :: dmaxpollen, dlinlen, R2search
      integer :: num
      integer, parameter :: jakdtree = 1
      integer, parameter :: MAXFIND = 100
      integer, parameter :: MINTREESIZE = 0

      real(kind=dp) :: SL, SM, XCR, YCR, CRP
      real(kind=dp) :: xa, ya, xb, yb, af
      real(kind=dp) :: xc, yc, xd, yd
      integer :: i, k, L, N1, N2, NN
      integer :: jacros, kint
      integer :: LnxiORLnx
      integer :: isactive

      ierror = 1

      intersection_count = 0

      if (NPL < 1) then
         goto 1234 ! nothing to do
      end if

      LnxiORLnx = 0

      if (itype == ITYPE_NETLINK_DUAL .or. itype == ITYPE_NETLINK) then ! netlinks
         LnxiORLnx = numL
      else ! flowlinks
         if (jaboundarylinks == BOUNDARY_ALL .or. jaboundarylinks == BOUNDARY_2D) then
            LnxiORLnx = Lnx
         else
            LnxiORLnx = Lnxi
         end if
      end if

!     allocate
      allocate (ipolsection(NPL - 1))

!     determine maximum polygon section length, and administer polygon sections
      dmaxpollen = 0.0_dp
      num = 0
      do i = 1, NPL - 1
         if (xpl(i) /= DMISS .and. xpl(i + 1) /= DMISS) then
            num = num + 1
            ipolsection(num) = i
            dmaxpollen = max(dmaxpollen, dbdistance(xpl(i), ypl(i), xpl(i + 1), ypl(i + 1), jsferic, jasfer3D, dmiss))
         end if
      end do

!     check tree size and exit if the tree is too small
      if (num < MINTREESIZE) then
         goto 1234
      end if

      NN = min(MAXFIND, num)

!     allocate
      allocate (x(num), y(num))

!     fill coordinates
      do k = 1, num
         i = ipolsection(k)
         x(k) = xpl(i)
         y(k) = ypl(i)
      end do

      call build_kdtree(treeinst, num, x, y, ierror, jsferic, dmiss)
      if (ierror /= 0) then
         goto 1234
      end if

!     find crossed flowlinks
      call mess(LEVEL_INFO, 'Finding crossed flowlinks...')

      kint = max(LnxiORLnx / 1000, 1)

      do L = 1, LnxiORLnx
         if (mod(L, kint) == 0) then
            af = real(L, kind=dp) / real(LnxiORLnx, kind=dp)
            call readyy('Finding crossed links', af)
!            write(6,"(F4.1, ' %')") af*100d0
         end if

         if (jaboundarylinks == BOUNDARY_2D .and. L > lnxi .and. L <= lnx1db) then
            ! Skip 1d boundaries
            cycle
         end if

         if (itype == ITYPE_NETLINK_DUAL) then ! netlinks, cross with dual links
            call get_link_neighboringcellcoords(L, isactive, xa, ya, xb, yb)
            if (isactive /= 1) then
               cycle
            end if
         else if (itype == ITYPE_FLOWLINK) then ! flowlinks
            n1 = ln(1, L)
            n2 = ln(2, L)
            xa = xz(n1)
            ya = yz(n1)
            xb = xz(n2)
            yb = yz(n2)
         else if (itype == ITYPE_NETLINK) then ! netlinks, cross with netlinks
            n1 = kn(1, L)
            n2 = kn(2, L)
            xa = xk(n1)
            ya = yk(n1)
            xb = xk(n2)
            yb = yk(n2)
         else if (itype == ITYPE_FLOWLINK_1D_DUAL) then
            if (L <= lnx1D) then ! flowlinks, cross with perpendicular in 1D
               n1 = ln(1, L)
               n2 = ln(2, L)
               xc = xz(n1)
               yc = yz(n1)
               xd = xz(n2)
               yd = yz(n2)
               xa = 0.5_dp * (xc + xd) - 0.5_dp * (yd - yc)
               ya = 0.5_dp * (yc + yd) + 0.5_dp * (xd - xc)
               xb = 0.5_dp * (xc + xd) + 0.5_dp * (yd - yc)
               yb = 0.5_dp * (yc + yd) - 0.5_dp * (xd - xc)
               call movabs(xa, ya)
               call lnabs(xb, yb)
            else ! flowlinks, cross with netlinks in 2D
               xa = xk(lncn(1, L))
               ya = yk(lncn(1, L))
               xb = xk(lncn(2, L))
               yb = yk(lncn(2, L))
            end if
         end if

!        fill query vector
         call make_queryvector_kdtree(treeinst, xa, ya, jsferic)

!        compute flowlink length
         dlinlen = dbdistance(xa, ya, xb, yb, jsferic, jasfer3D, dmiss)

!        determine square search radius
         R2search = 1.1_dp * (dlinlen + dmaxpollen)**2 ! 1.1d0: safety

!        count number of points in search area
         NN = kdtree2_r_count(treeinst%tree, treeinst%qv, R2search)

         if (NN == 0) then
            cycle ! no links found
         end if

!        reallocate if necessary
         call realloc_results_kdtree(treeinst, NN)

!        find nearest NN points
         call kdtree2_n_nearest(treeinst%tree, treeinst%qv, NN, treeinst%results)

         jacros = 0
         do i = 1, NN
            k = ipolsection(treeinst%results(i)%idx)
            call crossinbox(XPL(k), YPL(k), XPL(k + 1), YPL(k + 1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

            if (jacros == 1) then
               intersection_count = intersection_count + 1

               if (intersection_count > ubound(crossed_links, 1)) then
                  call mess(LEVEL_ERROR, 'find_crossed_links_kdtree2: array size too small')
               end if

               crossed_links(intersection_count) = L
               polygon_nodes(intersection_count) = k
               polygon_segment_weights(intersection_count) = SL
            end if
         end do
      end do

      call sort_crossed_links(crossed_links, polygon_nodes, polygon_segment_weights, n_links_polyline_nodes, intersection_count)

      call readyy(' ', -1.0_dp)

      call mess(LEVEL_INFO, 'done')

      ierror = 0
1234  continue

!     deallocate
      if (treeinst%itreestat /= ITREE_EMPTY) then
         call delete_kdtree2(treeinst)
      end if
      if (allocated(ipolsection)) then
         deallocate (ipolsection)
      end if
      if (allocated(x)) then
         deallocate (x)
      end if
      if (allocated(y)) then
         deallocate (y)
      end if

      return
   end subroutine find_crossed_links_kdtree2

   !> Find links crossed by a polyline using a thread-safe uniform-grid index.
   !! This routine is intended for comparing fixed-weir preprocessing performance with
   !! find_crossed_links_kdtree2. The grid only rejects candidates; CROSSinbox remains
   !! the exact intersection test. Spherical coordinates and non-flowlink intersection
   !! modes retain the existing k-d-tree implementation.
   subroutine find_crossed_links_kdtree_parallel(treeinst, NPL, xpl, ypl, itype, n_links_polyline_nodes, jaboundarylinks, intersection_count, crossed_links, polygon_nodes, polygon_segment_weights, ierror)
      use precision, only: dp
      use m_flowgeom, only: lnx, lnxi, lnx1db, ln, xz, yz
      use kdtree2Factory, only: kdtree_instance
      use m_sferic, only: jsferic
      use m_readyy, only: readyy
      use messagehandling, only: LEVEL_ERROR, LEVEL_INFO, mess
      use m_missing, only: dmiss
      use geometry_module, only: crossinbox
      use omp_lib, only: omp_get_max_threads, omp_get_thread_num

      type(kdtree_instance), intent(inout) :: treeinst
      integer, intent(in) :: NPL
      real(kind=dp), dimension(NPL), intent(in) :: xpl, ypl
      integer, intent(in) :: itype
      integer, intent(in) :: n_links_polyline_nodes
      integer, intent(in) :: jaboundarylinks
      integer, intent(out) :: intersection_count
      integer, dimension(n_links_polyline_nodes), intent(inout) :: crossed_links
      integer, dimension(n_links_polyline_nodes), intent(inout) :: polygon_nodes
      real(kind=dp), dimension(n_links_polyline_nodes), intent(inout) :: polygon_segment_weights
      integer, intent(out) :: ierror

      integer, parameter :: TARGET_LINKS_PER_BUCKET = 8
      integer, parameter :: MAX_BUCKETS_PER_LINK = 256
      integer, parameter :: MAX_GRID_BUCKETS = 65536

      integer, allocatable :: polyline_segments(:)
      integer, allocatable :: bucket_counts(:), bucket_start(:), bucket_cursor(:), bucket_links(:)
      integer, allocatable :: overflow_links(:), candidate_seen(:,:)
      real(kind=dp), allocatable :: link_xmin(:), link_xmax(:), link_ymin(:), link_ymax(:)
      integer :: link_count, segment_count, target_bucket_count, number_of_buckets
      integer :: grid_nx, grid_ny, total_bucket_entries, overflow_count, number_of_threads
      integer :: L, k, ixlo, ixhi, iylo, iyhi, bucket_count_for_link, bucket
      integer :: iseg, thread_index, ix, iy, ix_end, iy_end, step_x, step_y
      integer :: neighbour_x, neighbour_y, candidate_position, candidate_link, overflow_index, output_index
      integer :: jacros
      real(kind=dp) :: grid_xmin, grid_xmax, grid_ymin, grid_ymax, grid_hx, grid_hy
      real(kind=dp) :: xspan, yspan, aspect_ratio, xa, ya, xb, yb, dx, dy
      real(kind=dp) :: segment_xmin, segment_xmax, segment_ymin, segment_ymax
      real(kind=dp) :: tmax_x, tmax_y, tdelta_x, tdelta_y
      real(kind=dp) :: SL, SM, XCR, YCR, CRP

      ierror = 1
      intersection_count = 0

      ! The grid index is a Cartesian conservative prefilter. Preserve the existing
      ! implementation for spherical coordinates and all other intersection modes.
      if (jsferic /= 0 .or. itype /= ITYPE_FLOWLINK .or. NPL < 2) then
         call find_crossed_links_kdtree2(treeinst, NPL, xpl, ypl, itype, n_links_polyline_nodes, jaboundarylinks, intersection_count, crossed_links, polygon_nodes, polygon_segment_weights, ierror)
         return
      end if

      if (jaboundarylinks == BOUNDARY_ALL .or. jaboundarylinks == BOUNDARY_2D) then
         link_count = lnx
      else
         link_count = lnxi
      end if

      if (link_count == 0) then
         ierror = 0
         return
      end if

      allocate (polyline_segments(NPL - 1))
      segment_count = 0
      do k = 1, NPL - 1
         if (xpl(k) /= dmiss .and. xpl(k + 1) /= dmiss) then
            segment_count = segment_count + 1
            polyline_segments(segment_count) = k
         end if
      end do

      if (segment_count == 0) then
         ierror = 0
         goto 1234
      end if

      allocate (link_xmin(link_count), link_xmax(link_count), link_ymin(link_count), link_ymax(link_count))
      grid_xmin = huge(0.0_dp)
      grid_xmax = -huge(0.0_dp)
      grid_ymin = huge(0.0_dp)
      grid_ymax = -huge(0.0_dp)

      do L = 1, link_count
         xa = xz(ln(1, L))
         ya = yz(ln(1, L))
         xb = xz(ln(2, L))
         yb = yz(ln(2, L))
         link_xmin(L) = min(xa, xb)
         link_xmax(L) = max(xa, xb)
         link_ymin(L) = min(ya, yb)
         link_ymax(L) = max(ya, yb)

         grid_xmin = min(grid_xmin, link_xmin(L))
         grid_xmax = max(grid_xmax, link_xmax(L))
         grid_ymin = min(grid_ymin, link_ymin(L))
         grid_ymax = max(grid_ymax, link_ymax(L))
      end do

      xspan = grid_xmax - grid_xmin
      yspan = grid_ymax - grid_ymin
      target_bucket_count = min(MAX_GRID_BUCKETS, max(1, link_count / TARGET_LINKS_PER_BUCKET))

      if (xspan > 0.0_dp .and. yspan > 0.0_dp) then
         aspect_ratio = xspan / yspan
         grid_nx = max(1, nint(sqrt(real(target_bucket_count, dp) * aspect_ratio)))
         grid_ny = max(1, ceiling(real(target_bucket_count, dp) / real(grid_nx, dp)))
      else if (xspan > 0.0_dp) then
         grid_nx = target_bucket_count
         grid_ny = 1
      else
         grid_nx = 1
         grid_ny = target_bucket_count
      end if
      number_of_buckets = grid_nx * grid_ny

      if (xspan > 0.0_dp) then
         grid_hx = xspan / real(grid_nx, dp)
      else
         grid_hx = 1.0_dp
      end if
      if (yspan > 0.0_dp) then
         grid_hy = yspan / real(grid_ny, dp)
      else
         grid_hy = 1.0_dp
      end if

      allocate (bucket_counts(number_of_buckets), bucket_start(number_of_buckets + 1), bucket_cursor(number_of_buckets))
      bucket_counts = 0
      overflow_count = 0

      do L = 1, link_count
         if (jaboundarylinks == BOUNDARY_2D .and. L > lnxi .and. L <= lnx1db) then
            cycle
         end if
         ixlo = grid_cell_index(link_xmin(L), grid_xmin, grid_hx, grid_nx)
         ixhi = grid_cell_index(link_xmax(L), grid_xmin, grid_hx, grid_nx)
         iylo = grid_cell_index(link_ymin(L), grid_ymin, grid_hy, grid_ny)
         iyhi = grid_cell_index(link_ymax(L), grid_ymin, grid_hy, grid_ny)
         bucket_count_for_link = (ixhi - ixlo + 1) * (iyhi - iylo + 1)
         if (bucket_count_for_link > MAX_BUCKETS_PER_LINK) then
            overflow_count = overflow_count + 1
         else
            do iy = iylo, iyhi
               do ix = ixlo, ixhi
                  bucket = ix + (iy - 1) * grid_nx
                  bucket_counts(bucket) = bucket_counts(bucket) + 1
               end do
            end do
         end if
      end do

      bucket_start(1) = 1
      do bucket = 1, number_of_buckets
         bucket_start(bucket + 1) = bucket_start(bucket) + bucket_counts(bucket)
      end do
      total_bucket_entries = bucket_start(number_of_buckets + 1) - 1
      allocate (bucket_links(total_bucket_entries), overflow_links(overflow_count))
      bucket_cursor = bucket_start(1:number_of_buckets)
      overflow_count = 0

      do L = 1, link_count
         if (jaboundarylinks == BOUNDARY_2D .and. L > lnxi .and. L <= lnx1db) then
            cycle
         end if
         ixlo = grid_cell_index(link_xmin(L), grid_xmin, grid_hx, grid_nx)
         ixhi = grid_cell_index(link_xmax(L), grid_xmin, grid_hx, grid_nx)
         iylo = grid_cell_index(link_ymin(L), grid_ymin, grid_hy, grid_ny)
         iyhi = grid_cell_index(link_ymax(L), grid_ymin, grid_hy, grid_ny)
         bucket_count_for_link = (ixhi - ixlo + 1) * (iyhi - iylo + 1)
         if (bucket_count_for_link > MAX_BUCKETS_PER_LINK) then
            overflow_count = overflow_count + 1
            overflow_links(overflow_count) = L
         else
            do iy = iylo, iyhi
               do ix = ixlo, ixhi
                  bucket = ix + (iy - 1) * grid_nx
                  bucket_links(bucket_cursor(bucket)) = L
                  bucket_cursor(bucket) = bucket_cursor(bucket) + 1
               end do
            end do
         end if
      end do

      number_of_threads = max(1, omp_get_max_threads())
      allocate (candidate_seen(link_count, number_of_threads))
      candidate_seen = 0

      call mess(LEVEL_INFO, 'Finding crossed flowlinks using parallel spatial grid...')
      call readyy('Finding crossed links', 0.0_dp)

      !$OMP PARALLEL DO SCHEDULE(GUIDED) DEFAULT(SHARED) &
      !$OMP PRIVATE(iseg, thread_index, k, xa, ya, xb, yb, dx, dy, segment_xmin, segment_xmax, segment_ymin, segment_ymax, &
      !$OMP         ix, iy, ix_end, iy_end, step_x, step_y, tmax_x, tmax_y, tdelta_x, tdelta_y, neighbour_x, neighbour_y, &
      !$OMP         bucket, candidate_position, candidate_link, overflow_index, jacros, SL, SM, XCR, YCR, CRP, output_index)
      do iseg = 1, segment_count
         thread_index = omp_get_thread_num() + 1
         k = polyline_segments(iseg)
         xa = xpl(k)
         ya = ypl(k)
         xb = xpl(k + 1)
         yb = ypl(k + 1)
         dx = xb - xa
         dy = yb - ya
         segment_xmin = min(xa, xb)
         segment_xmax = max(xa, xb)
         segment_ymin = min(ya, yb)
         segment_ymax = max(ya, yb)

         ix = grid_cell_index(xa, grid_xmin, grid_hx, grid_nx)
         iy = grid_cell_index(ya, grid_ymin, grid_hy, grid_ny)
         ix_end = grid_cell_index(xb, grid_xmin, grid_hx, grid_nx)
         iy_end = grid_cell_index(yb, grid_ymin, grid_hy, grid_ny)

         if (dx > 0.0_dp) then
            step_x = 1
            tmax_x = (grid_xmin + real(ix, dp) * grid_hx - xa) / dx
            tdelta_x = grid_hx / dx
         else if (dx < 0.0_dp) then
            step_x = -1
            tmax_x = (grid_xmin + real(ix - 1, dp) * grid_hx - xa) / dx
            tdelta_x = -grid_hx / dx
         else
            step_x = 0
            tmax_x = huge(0.0_dp)
            tdelta_x = huge(0.0_dp)
         end if
         if (dy > 0.0_dp) then
            step_y = 1
            tmax_y = (grid_ymin + real(iy, dp) * grid_hy - ya) / dy
            tdelta_y = grid_hy / dy
         else if (dy < 0.0_dp) then
            step_y = -1
            tmax_y = (grid_ymin + real(iy - 1, dp) * grid_hy - ya) / dy
            tdelta_y = -grid_hy / dy
         else
            step_y = 0
            tmax_y = huge(0.0_dp)
            tdelta_y = huge(0.0_dp)
         end if

         do
            ! Check neighbouring buckets as well, making grid-edge and grid-corner
            ! intersections conservative without widening the query to the full bbox.
            do neighbour_y = max(1, iy - 1), min(grid_ny, iy + 1)
               do neighbour_x = max(1, ix - 1), min(grid_nx, ix + 1)
                  bucket = neighbour_x + (neighbour_y - 1) * grid_nx
                  do candidate_position = bucket_start(bucket), bucket_start(bucket + 1) - 1
                     candidate_link = bucket_links(candidate_position)
                     if (candidate_seen(candidate_link, thread_index) == iseg) then
                        cycle
                     end if
                     candidate_seen(candidate_link, thread_index) = iseg
                     if (link_xmax(candidate_link) < segment_xmin .or. link_xmin(candidate_link) > segment_xmax .or. &
                         link_ymax(candidate_link) < segment_ymin .or. link_ymin(candidate_link) > segment_ymax) then
                        cycle
                     end if
                      CRP = 0.0_dp
                     call crossinbox(xa, ya, xb, yb, xz(ln(1, candidate_link)), yz(ln(1, candidate_link)), &
                                      xz(ln(2, candidate_link)), yz(ln(2, candidate_link)), jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
                     if (jacros == 1) then
                        !$OMP ATOMIC CAPTURE
                        output_index = intersection_count
                        intersection_count = intersection_count + 1
                        !$OMP END ATOMIC
                        if (output_index < n_links_polyline_nodes) then
                           crossed_links(output_index + 1) = candidate_link
                           polygon_nodes(output_index + 1) = k
                           polygon_segment_weights(output_index + 1) = SL
                        end if
                     end if
                  end do
               end do
            end do

            if (ix == ix_end .and. iy == iy_end) then
               exit
            else if (tmax_x < tmax_y) then
               ix = ix + step_x
               tmax_x = tmax_x + tdelta_x
            else if (tmax_y < tmax_x) then
               iy = iy + step_y
               tmax_y = tmax_y + tdelta_y
            else
               ix = ix + step_x
               iy = iy + step_y
               tmax_x = tmax_x + tdelta_x
               tmax_y = tmax_y + tdelta_y
            end if
         end do

         do overflow_index = 1, overflow_count
            candidate_link = overflow_links(overflow_index)
            if (candidate_seen(candidate_link, thread_index) == iseg) then
               cycle
            end if
            candidate_seen(candidate_link, thread_index) = iseg
            if (link_xmax(candidate_link) < segment_xmin .or. link_xmin(candidate_link) > segment_xmax .or. &
                link_ymax(candidate_link) < segment_ymin .or. link_ymin(candidate_link) > segment_ymax) then
               cycle
            end if
            CRP = 0.0_dp
            call crossinbox(xa, ya, xb, yb, xz(ln(1, candidate_link)), yz(ln(1, candidate_link)), &
                             xz(ln(2, candidate_link)), yz(ln(2, candidate_link)), jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
            if (jacros == 1) then
               !$OMP ATOMIC CAPTURE
               output_index = intersection_count
               intersection_count = intersection_count + 1
               !$OMP END ATOMIC
               if (output_index < n_links_polyline_nodes) then
                  crossed_links(output_index + 1) = candidate_link
                  polygon_nodes(output_index + 1) = k
                  polygon_segment_weights(output_index + 1) = SL
               end if
            end if
         end do
      end do
      !$OMP END PARALLEL DO

      call readyy(' ', -1.0_dp)
      if (intersection_count > n_links_polyline_nodes) then
         call mess(LEVEL_ERROR, 'find_crossed_links_kdtree_parallel: array size too small')
         ierror = 1
         goto 1234
      end if

      call sort_crossed_link_records(crossed_links, polygon_nodes, polygon_segment_weights, n_links_polyline_nodes, intersection_count)
      call mess(LEVEL_INFO, 'done')
      ierror = 0

1234  continue

   end subroutine find_crossed_links_kdtree_parallel

   !> Convert a coordinate to a clamped uniform-grid index.
   pure integer function grid_cell_index(value, grid_min, grid_width, grid_count) result(index)
      use precision, only: dp

      real(kind=dp), intent(in) :: value, grid_min, grid_width
      integer, intent(in) :: grid_count

      index = int((value - grid_min) / grid_width) + 1
      index = max(1, min(grid_count, index))

   end function grid_cell_index

   !> Sort parallel intersection records first by link and then by polygon index.
   subroutine sort_crossed_link_records(crossed_links, polygon_nodes, polygon_segment_weights, n_links_polyline_nodes, intersection_count)
      use stdlib_sorting, only: sort_index
      use precision, only: dp

      integer, dimension(n_links_polyline_nodes), intent(inout) :: crossed_links
      integer, dimension(n_links_polyline_nodes), intent(inout) :: polygon_nodes
      real(kind=dp), dimension(n_links_polyline_nodes), intent(inout) :: polygon_segment_weights
      integer, intent(in) :: n_links_polyline_nodes, intersection_count

      integer, allocatable :: new_index(:), polygon_nodes_copy(:)
      real(kind=dp), allocatable :: polygon_segment_weights_copy(:)
      integer :: i

      if (intersection_count < 2) then
         return
      end if

      allocate (new_index(intersection_count), polygon_nodes_copy(intersection_count), polygon_segment_weights_copy(intersection_count))
      polygon_nodes_copy = polygon_nodes(1:intersection_count)
      polygon_segment_weights_copy = polygon_segment_weights(1:intersection_count)

      call sort_index(crossed_links(1:intersection_count), new_index)
      do i = 1, intersection_count
         polygon_nodes(i) = polygon_nodes_copy(new_index(i))
         polygon_segment_weights(i) = polygon_segment_weights_copy(new_index(i))
      end do
      call sort_crossed_links(crossed_links, polygon_nodes, polygon_segment_weights, n_links_polyline_nodes, intersection_count)

   end subroutine sort_crossed_link_records

   !> sort intersections of crossed_links first on links and subsequently polygon index
   subroutine sort_crossed_links(crossed_links, polygon_nodes, polygon_segment_weights, n_links_polyline_nodes, intersection_count)
      use stdlib_sorting, only: sort_index
      use precision, only: dp

      integer, dimension(n_links_polyline_nodes), intent(in) :: crossed_links !< crossed link indices
      integer, dimension(n_links_polyline_nodes), intent(inout) :: polygon_nodes !< starting node of intersected polygon segment
      real(kind=dp), dimension(n_links_polyline_nodes), intent(inout) :: polygon_segment_weights !< relative length of polygon segment from starting node till intersection
      integer, intent(in) :: n_links_polyline_nodes !< array_size e.g. number of links ( Lnx for flowlinks, numL for netlinks) or npl for number of polyline nodes
      integer, intent(in) :: intersection_count !< number of link intersections

      integer, dimension(:), allocatable :: new_index !< index of sorted polygon_nodes
      real(kind=dp), dimension(:), allocatable :: polygon_segment_weights_copy !< copy of intersection length polygon_segment_weights

      integer :: k
      integer :: n_start
      integer :: n_end

      if (intersection_count < 2) then
         return
      end if

      allocate (new_index(n_links_polyline_nodes))
      allocate (polygon_segment_weights_copy(n_links_polyline_nodes))

      polygon_segment_weights_copy = polygon_segment_weights
      ! Find each consecutive group with the same crossed-link value.
      n_start = 1
      do while (n_start <= intersection_count)
         n_end = n_start
         do while (n_end < intersection_count)
            if (crossed_links(n_end + 1) /= crossed_links(n_start)) then
               exit
            end if
            n_end = n_end + 1
         end do
         call sort_index(polygon_nodes(n_start:n_end), new_index(n_start:n_end)) ! sorts polygon_nodes, and returns sorting order in new_index
         do k = n_start, n_end
            polygon_segment_weights(k) = polygon_segment_weights_copy(n_start - 1 + new_index(k)) ! sort polygon_segment_weights according to the same ordering
         end do
         n_start = n_end + 1
      end do

   end subroutine

end module m_find_crossed_links_kdtree2
