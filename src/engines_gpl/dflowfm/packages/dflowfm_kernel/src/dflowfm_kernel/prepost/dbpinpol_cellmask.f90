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

module m_cellmask_from_polygon_set
   use m_missing, only: jins, dmiss
   use precision, only: dp
   use m_polygon, only: xpl, ypl, npl

   implicit none

   private

   public :: cellmask_from_polygon_set_init, cellmask_from_polygon_set_cleanup, cellmask_from_polygon_set

   real(kind=dp), allocatable :: xpmin_cellmask(:), ypmin_cellmask(:) !< Polygon bounding box min coordinates
   real(kind=dp), allocatable :: xpmax_cellmask(:), ypmax_cellmask(:) !< Polygon bounding box max coordinates
   real(kind=dp), allocatable :: zpl_cellmask(:) !< Polygon coordinate arrays
   integer, allocatable :: iistart_cellmask(:), iiend_cellmask(:) !< Polygon start and end indices in coordinate arrays (dim = number of polygons)
   integer :: Npoly_cellmask = 0 !< Number of polygons stored in module arrays
   logical :: cellmask_initialized = .false. !< Flag indicating if cellmask data structures have been initialized for safety
   logical :: enclosures_present = .false. !< Flag indicating if any enclosures are present in the polygon dataset

contains

   !> Initialize module-level cellmask polygon data structures, such as the bounding boxes and iistart/iiend
   ! this keeps the actual calculation routines elemental.
   subroutine cellmask_from_polygon_set_init(NPL, xpl, ypl, zpl)
      use m_alloc
      use geometry_module, only: get_startend

      integer, intent(in) :: NPL !< Number of polygon points
      real(kind=dp), intent(in) :: xpl(NPL), ypl(NPL), zpl(NPL) !< Polygon coordinate arrays

      integer :: polygon_buffer_size !> polygon arrays buffer size, increases 10% every time to avoid realloc at every polygon
      integer :: ipoint, istart, iend, ipoly

      if (cellmask_initialized) then
         call cellmask_from_polygon_set_cleanup()
      end if

      if (NPL == 0) then
         cellmask_initialized = .true.
         return
      end if

      polygon_buffer_size = 1000
      call realloc(xpmin_cellmask, polygon_buffer_size, keepExisting=.false.)
      call realloc(xpmax_cellmask, polygon_buffer_size, keepExisting=.false.)
      call realloc(ypmin_cellmask, polygon_buffer_size, keepExisting=.false.)
      call realloc(ypmax_cellmask, polygon_buffer_size, keepExisting=.false.)
      call realloc(iistart_cellmask, polygon_buffer_size, keepExisting=.false.)
      call realloc(iiend_cellmask, polygon_buffer_size, keepExisting=.false.)
      call realloc(zpl_cellmask, polygon_buffer_size, keepExisting=.false.)

      ipoint = 1
      ipoly = 0

      do while (ipoint < NPL)
         ipoly = ipoly + 1
         if (ipoly > polygon_buffer_size) then
            polygon_buffer_size = ceiling(polygon_buffer_size * 1.1_dp)
            call realloc(xpmin_cellmask, polygon_buffer_size, keepExisting=.true.)
            call realloc(xpmax_cellmask, polygon_buffer_size, keepExisting=.true.)
            call realloc(ypmin_cellmask, polygon_buffer_size, keepExisting=.true.)
            call realloc(ypmax_cellmask, polygon_buffer_size, keepExisting=.true.)
            call realloc(iistart_cellmask, polygon_buffer_size, keepExisting=.true.)
            call realloc(iiend_cellmask, polygon_buffer_size, keepExisting=.true.)
            call realloc(zpl_cellmask, polygon_buffer_size, keepExisting=.true.)
         end if

         call get_startend(NPL - ipoint + 1, xpl(ipoint:NPL), ypl(ipoint:NPL), istart, iend, dmiss)
         istart = istart + ipoint - 1
         iend = iend + ipoint - 1

         if (istart >= iend .or. iend > NPL) exit

         xpmin_cellmask(ipoly) = minval(xpl(istart:iend))
         xpmax_cellmask(ipoly) = maxval(xpl(istart:iend))
         ypmin_cellmask(ipoly) = minval(ypl(istart:iend))
         ypmax_cellmask(ipoly) = maxval(ypl(istart:iend))

         iistart_cellmask(ipoly) = istart
         iiend_cellmask(ipoly) = iend
         zpl_cellmask(ipoly) = zpl(istart)

         ipoint = iend + 2
      end do

      Npoly_cellmask = ipoly

      ! check if there are any enclosure polygons
      enclosures_present = any(zpl_cellmask(1:Npoly_cellmask) < 0.0_dp .and. zpl_cellmask(1:Npoly_cellmask) /= dmiss)
      cellmask_initialized = .true.

   end subroutine cellmask_from_polygon_set_init

   !> Check if a point should be masked, either is_inside a dry-area polygon or outside an enclosure polygon.
   elemental function cellmask_from_polygon_set(xp, yp) result(mask)

      integer :: mask
      real(kind=dp), intent(in) :: xp, yp !< Point coordinates

      integer :: count_drypoint, ipoly, num_enclosures
      logical :: found_inside_enclosure, is_inside
      real(kind=dp) :: zpl_val

      mask = 0
      if (.not. cellmask_initialized) then
         return
      end if

      num_enclosures = 0
      count_drypoint = 0
      found_inside_enclosure = .false.
      is_inside = .false.

      ! Single loop over all polygons
      do ipoly = 1, Npoly_cellmask
         zpl_val = zpl_cellmask(ipoly)

         ! Bounding box check
         if (xp < xpmin_cellmask(ipoly) .or. xp > xpmax_cellmask(ipoly) .or. &
             yp < ypmin_cellmask(ipoly) .or. yp > ypmax_cellmask(ipoly)) then
            cycle
         end if

         ! Point-in-polygon test
         is_inside = pinpok_elemental(xp, yp, ipoly)

         if (zpl_val == dmiss .or. zpl_val > 0.0_dp) then
            ! Dry point polygon
            if (is_inside) then
               count_drypoint = count_drypoint + 1
            end if
         else if (zpl_val < 0.0_dp .and. is_inside) then
            found_inside_enclosure = .true.
         end if
      end do

      ! Apply odd-even rule only if counting was needed
      if (JINS == 1) then
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

      if (allocated(xpmin_cellmask)) deallocate (xpmin_cellmask)
      if (allocated(xpmax_cellmask)) deallocate (xpmax_cellmask)
      if (allocated(ypmin_cellmask)) deallocate (ypmin_cellmask)
      if (allocated(ypmax_cellmask)) deallocate (ypmax_cellmask)
      if (allocated(zpl_cellmask)) deallocate (zpl_cellmask)
      if (allocated(iistart_cellmask)) deallocate (iistart_cellmask)
      if (allocated(iiend_cellmask)) deallocate (iiend_cellmask)

      Npoly_cellmask = 0
      cellmask_initialized = .false.

   end subroutine cellmask_from_polygon_set_cleanup

   !> Optimized elemental point-in-polygon test using ray casting algorithm.
   !! Accesses polygon data via module arrays.
   elemental function pinpok_elemental(xl, yl, ipoly) result(is_inside)

      real(kind=dp), intent(in) :: xl, yl !< Point coordinates (scalar)
      integer, intent(in) :: ipoly !< Polygon index
      logical :: is_inside !< Result: .true.=is_inside, .false.=outside

      integer :: i, j, istart, iend, crossings
      real(kind=dp) :: x1, x2, y1, y2, xinters

      is_inside = .false.

      ! Get polygon bounds from module variables
      istart = iistart_cellmask(ipoly)
      iend = iiend_cellmask(ipoly)

      if (iend - istart + 1 <= 2) then
         is_inside = .true.
         goto 999
      end if

      ! Ray-casting algorithm
      crossings = 0
      j = iend

      do i = istart, iend
         if (xpl(i) == dmiss) then
            exit
         end if

         x1 = xpl(j)
         y1 = ypl(j)
         x2 = xpl(i)
         y2 = ypl(i)

         ! Check if point is on vertex
         if (xl == x1 .and. yl == y1) then
            is_inside = .true.
            goto 999
         end if

         ! Check if ray crosses edge
         if ((y1 > yl) .neqv. (y2 > yl)) then
            xinters = x1 + (yl - y1) * (x2 - x1) / (y2 - y1)

            if (xl < xinters) then
               crossings = crossings + 1
            else if (xl == xinters) then
               is_inside = .true.
               goto 999
            end if
         end if
         j = i
      end do

      is_inside = mod(crossings, 2) == 1
999   continue
      if (jins == 0) then
         is_inside = .not. is_inside
      end if

   end function pinpok_elemental

end module m_cellmask_from_polygon_set
