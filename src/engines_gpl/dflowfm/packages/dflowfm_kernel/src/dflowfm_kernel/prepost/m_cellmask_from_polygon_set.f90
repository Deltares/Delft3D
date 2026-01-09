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
   use m_missing, only: jins, dmiss
   use precision, only: dp
   use m_polygon, only: xpl, ypl, zpl, npl, maxpol, restorepol, savepol

   implicit none

   private

   public :: cellmask_from_polygon_set_init, cellmask_from_polygon_set_cleanup, cellmask_from_polygon_set, pinpok_elemental
   public :: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines

   integer :: polygons = 0 !< Number of polygons stored in module arrays xpl, ypl, zpl
   real(kind=dp), allocatable :: x_poly_min(:), y_poly_min(:) !< Polygon bounding box min coordinates, (dim = polygons)
   real(kind=dp), allocatable :: x_poly_max(:), y_poly_max(:) !< Polygon bounding box max coordinates, (dim = polygons)
   real(kind=dp), allocatable :: polygon_type(:) !< Polygon type, positive or dmiss = drypoint , negative = enclosure (dim = polygons)
   integer, allocatable :: i_poly_start(:), i_poly_end(:) !< Polygon start and end indices in coordinate arrays (dim = polygons)
   logical :: cellmask_initialized = .false. !< Flag indicating if cellmask data structures have been initialized for safety
   logical :: enclosures_present = .false. !< Flag indicating if any enclosures are present in the polygon dataset

contains

   !> Initialize module-level cellmask polygon data structures, such as the bounding boxes and iistart/iiend
   ! this keeps the actual calculation routines elemental.
   subroutine cellmask_from_polygon_set_init(polygon_points, x_poly, y_poly, z_poly)
      use m_alloc
      use geometry_module, only: get_startend

      integer, intent(in) :: polygon_points !< Number of polygon points
      real(kind=dp), intent(in) :: x_poly(polygon_points), y_poly(polygon_points), z_poly(polygon_points) !< Polygon coordinate arrays

      integer :: i_point, i_start, i_end, i_poly

      if (cellmask_initialized) then
         call cellmask_from_polygon_set_cleanup()
      end if

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

      polygons = 0
      cellmask_initialized = .false.
      enclosures_present = .false.

   end subroutine cellmask_from_polygon_set_cleanup

!> Elemental wrapper for cellmask operations using module-level polygon arrays
   elemental function pinpok_elemental(x, y, i_poly) result(is_inside)
      use m_polygon, only: xpl, ypl
      use geometry_module, only: pinpok_raycast

      real(kind=dp), intent(in) :: x, y !< Point coordinates
      integer, intent(in) :: i_poly !< Polygon index
      logical :: is_inside !< Result

      integer :: i_start, i_end, n_points

      ! Get bounds for this polygon from module arrays
      i_start = i_poly_start(i_poly)
      i_end = i_poly_end(i_poly)
      n_points = i_end - i_start + 1

      ! Call the shared optimized algorithm with array slice
      is_inside = pinpok_raycast(x, y, xpl(i_start:i_end), ypl(i_start:i_end), n_points)

   end function pinpok_elemental

   !> Initialize xpl, ypl, zpl arrays with all netcell geometries (called once)
   subroutine init_cell_geom_as_polylines()
      use network_data
      use m_alloc

      integer :: k, n, k1, total_points, ipoint

      if (cellmask_initialized) then !> reuse cellmask cache boolean
         return
      end if

      call savepol()

      ! calculate total points needed: sum(netcell(k)%n + 1) for all cells
      ! +1 for dmiss separator after each polygon
      total_points = 0
      do k = 1, nump
         total_points = total_points + netcell(k)%n + 1 ! +1 for dmiss
      end do

      ! allocate or reallocate xpl, ypl, zpl
      call realloc(xpl, total_points, keepexisting=.false.)
      call realloc(ypl, total_points, keepexisting=.false.)
      call realloc(zpl, total_points, keepexisting=.false.)

      ! fill arrays with netcell geometry
      ipoint = 0
      do k = 1, nump
         do n = 1, netcell(k)%n
            ipoint = ipoint + 1
            k1 = netcell(k)%nod(n)
            xpl(ipoint) = xk(k1)
            ypl(ipoint) = yk(k1)
            zpl(ipoint) = real(k, dp) ! store cell index as z-value
         end do

         ! add separator
         ipoint = ipoint + 1
         xpl(ipoint) = dmiss
         ypl(ipoint) = dmiss
         zpl(ipoint) = dmiss
      end do

      npl = ipoint

      ! initialize the cellmask module with these polygons
      ! this builds bounding boxes and polygon indices
      call cellmask_from_polygon_set_init(npl, xpl, ypl, zpl)

   end subroutine init_cell_geom_as_polylines

   !> call general polygon cleanup and restore previous polygon data
   subroutine cleanup_cell_geom_polylines()
      call cellmask_from_polygon_set_cleanup()
      maxpol = 0 !< reset maxpol to prevent unnecessarily large realloc
      call restorepol()
   end subroutine cleanup_cell_geom_polylines

!> Fast replacement for INCELLS using cached geometry in global polygon arrays
   elemental function point_find_netcell(x, y) result(k)
      use m_polygon, only: xpl, ypl, zpl

      real(kind=dp), intent(in) :: x, y !< coordinates of point to locate enclosing netcell
      integer :: k !< cell number of enclosing netcell, or 0 if not found

      integer :: i_poly
      logical :: is_inside

      k = 0

      ! Loop over all netcell polygons with fast bounding box checks
      do i_poly = 1, polygons

         ! Quick bbox rejection (most cells rejected here)
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

end module m_cellmask_from_polygon_set
