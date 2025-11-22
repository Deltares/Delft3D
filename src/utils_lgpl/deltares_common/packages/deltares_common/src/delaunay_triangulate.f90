!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!

!> Delaunay Triangulation Subroutine
!! This subroutine generates a Delaunay triangulation mesh from given x,y coordinates
!! Based on the triangulation routines used in Delft3D ec_module
!
module m_delaunay_triangulate
   use precision, only: dp
   implicit none

   private
   public :: delaunay_triangulate, delaunay_triangulate_with_edges

contains

   !> Generate Delaunay triangulation mesh from x,y coordinates
   !!
   !! @param[in]  x            x-coordinates of input points
   !! @param[in]  y            y-coordinates of input points
   !! @param[in]  npoints      number of input points
   !! @param[out] triangles    triangle connectivity (3, numtriangles) - 1-based indexing
   !! @param[out] numtriangles number of triangles generated
   !! @param[in]  maxtriangles maximum number of triangles allowed
   !! @param[out] ierr         error flag (0=success, /=0 error)
   subroutine delaunay_triangulate(x, y, npoints, triangles, num_triangles, max_triangles, ierr)
      implicit none

      ! Arguments
      real(kind=dp), intent(in) :: x(:) !< x-coordinates
      real(kind=dp), intent(in) :: y(:) !< y-coordinates
      integer, intent(in) :: npoints !< number of points
      integer, intent(out) :: triangles(:, :) !< triangle connectivity (3, maxtriangles)
      integer, intent(out) :: num_triangles !< number of triangles generated
      integer, intent(in) :: max_triangles !< maximum triangles allowed
      integer, intent(out) :: ierr !< error flag

      ! Local variables
      real(kind=dp), allocatable :: x_work(:), y_work(:) ! working arrays (need +4 for artificial points)
      real(kind=dp), allocatable :: xcent(:), ycent(:) ! triangle circumcenters
      integer, allocatable :: triangles_local(:, :) ! triangle connectivity (local)

      integer :: i, j, k, l, ie, je, in, inew, inewe
      integer :: i1, i2, i3, nn, nart, newel, ndel, match
      integer :: interval, max_tri, nsm

      real(kind=dp) :: cx, cy, den, dx, dy, dxy, r2, rn2, z, zero
      real(kind=dp) :: xmin, xmax, ymin, ymax, xl, xr, yl, yr
      real(kind=dp) :: x2, x3, y2, y3, xn1

      ! Initialize
      ierr = 0
      triangles = 0

      ! Check input
      if (npoints < 3) then
         ierr = 1 ! Not enough points for triangulation
         return
      end if

      if (max_triangles < 2 * npoints) then
         ierr = 2 ! Output array too small
         return
      end if

      ! Allocate working arrays (need +4 for artificial boundary points)
      allocate (x_work(npoints + 4), y_work(npoints + 4), stat=ierr)
      if (ierr /= 0) then
         ierr = 3 ! Memory allocation error
         return
      end if

      ! Estimate maximum number of triangles (typically ~2*npoints)
      nsm = 3 * npoints + 20
      allocate (xcent(nsm), ycent(nsm), triangles_local(3, nsm), stat=ierr)
      if (ierr /= 0) then
         ierr = 3 ! Memory allocation error
         deallocate (x_work, y_work)
         return
      end if

      ! Copy input coordinates to working arrays
      x_work(1:npoints) = x(1:npoints)
      y_work(1:npoints) = y(1:npoints)

      ! Set tolerance for geometric calculations
      zero = 0.01d0

      ! Find bounding box of input points
      xmin = minval(x(1:npoints))
      xmax = maxval(x(1:npoints))
      ymin = minval(y(1:npoints))
      ymax = maxval(y(1:npoints))

      ! Extend bounding box and create artificial boundary points
      dx = xmax - xmin
      dy = ymax - ymin
      dxy = 0.1d0 * max(dx, dy)
      zero = 10.0d0 * dxy * 1.0d-9

      xl = xmin - 4.0d0 * dx - dxy
      xr = xmax + 4.0d0 * dx + dxy
      yl = ymin - 4.0d0 * dy - dxy
      yr = ymax + 4.0d0 * dy + dxy

      ! Add four artificial boundary points
      x_work(npoints + 1) = xl
      y_work(npoints + 1) = yl
      x_work(npoints + 2) = xl
      y_work(npoints + 2) = yr
      x_work(npoints + 3) = xr
      y_work(npoints + 3) = yr
      x_work(npoints + 4) = xr
      y_work(npoints + 4) = yl

      ! Create first two artificial triangles covering the extended domain
      triangles_local(1, 1) = npoints + 1
      triangles_local(2, 1) = npoints + 2
      triangles_local(3, 1) = npoints + 3
      triangles_local(1, 2) = npoints + 3
      triangles_local(2, 2) = npoints + 4
      triangles_local(3, 2) = npoints + 1
      num_triangles = 2

      ! Calculate circumcenters for initial triangles
      do ie = 1, 2
         i1 = triangles_local(1, ie)
         i2 = triangles_local(2, ie)
         i3 = triangles_local(3, ie)
         x2 = x_work(i2) - x_work(i1)
         x3 = x_work(i3) - x_work(i1)
         y2 = y_work(i2) - y_work(i1)
         y3 = y_work(i3) - y_work(i1)
         den = y2 * x3 - y3 * x2
         if (abs(den) > zero) then
            z = (x2 * (x2 - x3) + y2 * (y2 - y3)) / den
         else
            z = 0.0d0
         end if
         xcent(ie) = 0.5d0 * (x3 - z * y3)
         ycent(ie) = 0.5d0 * (y3 + z * x3)
      end do

      ! Main triangulation loop - add one point at a time
      interval = max(1, npoints / 100)
      max_tri = 2

      do in = 1, npoints
         ! Add one mesh point at a time and remesh locally if necessary
         ndel = 0
         newel = 0

         do ie = 1, num_triangles
            ! Check if point is inside circumcircle of triangle ie
            i1 = triangles_local(1, ie)
            i2 = triangles_local(2, ie)
            i3 = triangles_local(3, ie)
            cx = xcent(ie)
            cy = ycent(ie)
            r2 = cx**2 + cy**2
            xn1 = x_work(in) - x_work(i1)
            rn2 = (xn1 - cx)**2 + (y_work(in) - y_work(i1) - cy)**2

            if (rn2 > r2) cycle ! Point is outside circumcircle

            ! Point is inside circumcircle - create new triangles and mark old for deletion
            do j = 1, 3
               do k = 1, 3
                  triangles_local(k, num_triangles + newel + j) = triangles_local(k, ie)
               end do
               max_tri = max(max_tri, num_triangles + newel + 3)
               if (max_tri > nsm) then
                  ierr = 4 ! Exceeded maximum triangles
                  goto 999
               end if
               triangles_local(j, num_triangles + newel + j) = in
            end do

            ! Calculate circumcenters for new triangles
            do inew = 1, 3
               inewe = num_triangles + newel + inew
               max_tri = max(max_tri, inewe)
               if (max_tri > nsm) then
                  ierr = 4 ! Exceeded maximum triangles
                  goto 999
               end if
               i1 = triangles_local(1, inewe)
               i2 = triangles_local(2, inewe)
               i3 = triangles_local(3, inewe)
               x2 = x_work(i2) - x_work(i1)
               x3 = x_work(i3) - x_work(i1)
               y2 = y_work(i2) - y_work(i1)
               y3 = y_work(i3) - y_work(i1)
               if (abs(y2 * x3 - y3 * x2) > zero) then
                  z = (x2 * (x2 - x3) + y2 * (y2 - y3)) / (y2 * x3 - y3 * x2)
                  cx = 0.5d0 * (x3 - z * y3)
                  cy = 0.5d0 * (y3 + z * x3)
               else
                  cx = 0.5d0 * (x3 - x2)
                  cy = 0.5d0 * (y3 - y2)
               end if
               xcent(inewe) = cx
               ycent(inewe) = cy
            end do

            newel = newel + 3
            triangles_local(1, ie) = 0 ! Mark for deletion
            ndel = ndel + 1
         end do

         ! If point was inside circumcircle of more than 1 triangle,
         ! delete duplicate triangles
         if (ndel > 1) then
            do ie = num_triangles + 1, num_triangles + newel - 1
               do je = ie + 1, num_triangles + newel
                  match = 0
                  do k = 1, 3
                     do l = 1, 3
                        if (triangles_local(k, ie) == triangles_local(l, je)) match = match + 1
                     end do
                  end do
                  if (match == 3) then
                     triangles_local(1, ie) = 0 ! Mark for deletion
                     triangles_local(1, je) = 0 ! Mark for deletion
                     ndel = ndel + 2
                  end if
               end do
            end do
         end if

         ! Remove deleted triangles by compacting arrays
         nn = num_triangles + newel
         ie = 1
         do while (ie <= nn)
            if (triangles_local(1, ie) == 0) then
               do j = ie, nn - 1
                  xcent(j) = xcent(j + 1)
                  ycent(j) = ycent(j + 1)
                  do k = 1, 3
                     triangles_local(k, j) = triangles_local(k, j + 1)
                  end do
               end do
               nn = nn - 1
               ie = ie - 1
            end if
            ie = ie + 1
         end do
         num_triangles = nn
      end do

      ! Remove triangles containing artificial boundary points
      ie = 1
      do while (ie <= num_triangles)
         nart = 0
         do l = 1, 3
            if (triangles_local(l, ie) > npoints) nart = nart + 1
         end do
         if (nart > 0) then
            do j = ie, num_triangles - 1
               xcent(j) = xcent(j + 1)
               ycent(j) = ycent(j + 1)
               do k = 1, 3
                  triangles_local(k, j) = triangles_local(k, j + 1)
               end do
            end do
            num_triangles = num_triangles - 1
            ie = ie - 1
         end if
         ie = ie + 1
      end do

      ! Copy results to output array (check bounds)
      if (num_triangles > max_triangles) then
         ierr = 5 ! Too many triangles for output array
         goto 999
      end if

      do i = 1, num_triangles
         do j = 1, 3
            triangles(j, i) = triangles_local(j, i)
         end do
      end do

999   continue
      ! Clean up
      deallocate (x_work, y_work, xcent, ycent, triangles_local)

   end subroutine delaunay_triangulate

   !> Generate Delaunay triangulation mesh with unique edge table
   !!
   !! @param[in]  x            x-coordinates of input points
   !! @param[in]  y            y-coordinates of input points
   !! @param[in]  npoints      number of input points
   !! @param[out] triangles    triangle connectivity (3, numtriangles) - 1-based indexing
   !! @param[out] numtriangles number of triangles generated
   !! @param[in]  maxtriangles maximum number of triangles allowed
   !! @param[out] edges        unique edge connectivity (2, numedges) - 1-based indexing
   !! @param[out] numedges     number of unique edges generated
   !! @param[in]  maxedges     maximum number of edges allowed
   !! @param[out] ierr         error flag (0=success, /=0 error)
   subroutine delaunay_triangulate_with_edges(x, y, npoints, triangles, num_triangles, max_triangles, &
                                              edges, num_edges, max_edges, tri2edge, ierr)
      implicit none

      ! Arguments
      real(kind=dp), intent(in) :: x(:) !< x-coordinates
      real(kind=dp), intent(in) :: y(:) !< y-coordinates
      integer, intent(in) :: npoints !< number of points
      integer, intent(out) :: triangles(:, :) !< triangle connectivity (3, maxtriangles)
      integer, intent(out) :: num_triangles !< number of triangles generated
      integer, intent(in) :: max_triangles !< maximum triangles allowed
      integer, intent(out) :: edges(:, :) !< edge connectivity (2, maxedges)
      integer, intent(out) :: num_edges !< number of unique edges generated
      integer, intent(in) :: max_edges !< maximum edges allowed
      integer, intent(out) :: tri2edge(:, :) !< edge numbers for each triangle (3, maxtriangles)
      integer, intent(out) :: ierr !< error flag

      ! Local variables for edge generation
      integer, allocatable :: edge_list(:, :) ! temporary edge list (2, 3*maxtriangles)
      integer, allocatable :: edge_map(:) ! maps each edge in edge_list to its unique edge number
      integer :: total_edges ! total edges before uniqueness check
      integer :: i, j, k, n1, n2, temp
      logical :: is_duplicate
      integer :: edge_idx

      ! First call the standard triangulation subroutine
      call delaunay_triangulate(x, y, npoints, triangles, num_triangles, max_triangles, ierr)

      if (ierr /= 0) then
         num_edges = 0
         return
      end if

      ! Initialize edge arrays
      num_edges = 0
      edges = 0
      tri2edge = 0

      ! Check if we have enough space for edges (typically ~3*numtriangles for planar graphs)
      if (max_edges < 3 * num_triangles) then
         ierr = 6 ! Not enough space for edges
         return
      end if

      ! Allocate temporary edge list (each triangle contributes 3 edges)
      total_edges = 3 * num_triangles
      allocate (edge_list(2, total_edges), edge_map(total_edges), stat=ierr)
      if (ierr /= 0) then
         ierr = 3 ! Memory allocation error
         return
      end if

      ! Extract all edges from triangles and build edge_map
      do i = 1, num_triangles
         ! Edge 1: node 1 -> node 2
         edge_list(1, 3 * (i - 1) + 1) = triangles(1, i)
         edge_list(2, 3 * (i - 1) + 1) = triangles(2, i)
         ! Edge 2: node 2 -> node 3
         edge_list(1, 3 * (i - 1) + 2) = triangles(2, i)
         edge_list(2, 3 * (i - 1) + 2) = triangles(3, i)
         ! Edge 3: node 3 -> node 1
         edge_list(1, 3 * (i - 1) + 3) = triangles(3, i)
         edge_list(2, 3 * (i - 1) + 3) = triangles(1, i)
      end do

      ! Sort each edge so that the smaller node number comes first
      do i = 1, total_edges
         n1 = edge_list(1, i)
         n2 = edge_list(2, i)
         if (n1 > n2) then
            edge_list(1, i) = n2
            edge_list(2, i) = n1
         end if
      end do

      ! Remove duplicate edges and build edge_map
      num_edges = 0
      do i = 1, total_edges
         is_duplicate = .false.
         n1 = edge_list(1, i)
         n2 = edge_list(2, i)
         edge_idx = 0
         ! Check if this edge already exists in the unique edge list
         do j = 1, num_edges
            if (edges(1, j) == n1 .and. edges(2, j) == n2) then
               is_duplicate = .true.
               edge_idx = j
               exit
            end if
         end do
         ! If not a duplicate, add to unique edge list
         if (.not. is_duplicate) then
            num_edges = num_edges + 1
            if (num_edges > max_edges) then
               ierr = 7 ! Too many edges for output array
               deallocate (edge_list, edge_map)
               return
            end if
            edges(1, num_edges) = n1
            edges(2, num_edges) = n2
            edge_idx = num_edges
         end if
         edge_map(i) = edge_idx
      end do

      ! For each triangle, store the edge numbers (indices in edges(:,:))
      do i = 1, num_triangles
         do j = 1, 3
            tri2edge(j, i) = edge_map(3 * (i - 1) + j)
         end do
      end do

      ! Clean up
      deallocate (edge_list, edge_map)

   end subroutine delaunay_triangulate_with_edges

end module m_delaunay_triangulate
