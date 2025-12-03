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

module m_dbpinpol_cellmask
   use m_missing, only: jins, dmiss
   use precision, only: dp

   implicit none

   private

   !> dbpinpol routines are public to avoid PetSC dependency in unit tests
   public :: dbpinpol_cellmask_init, dbpinpol_cellmask_cleanup, dbpinpol_cellmask

   real(kind=dp), allocatable :: xpmin_cellmask(:), ypmin_cellmask(:) !< Polygon bounding box min coordinates
   real(kind=dp), allocatable :: xpmax_cellmask(:), ypmax_cellmask(:) !< Polygon bounding box max coordinates
   real(kind=dp), allocatable :: xpl_cellmask(:), ypl_cellmask(:), zpl_cellmask(:) !< Polygon coordinate arrays
   integer, allocatable :: iistart_cellmask(:), iiend_cellmask(:) !< Polygon start and end indices in coordinate arrays (dim = number of polygons)
   integer :: Npoly_cellmask = 0 !< Number of polygons stored in module arrays
   logical :: cellmask_initialized = .false. !< Flag indicating if cellmask data structures have been initialized for safety
   logical :: enclosures_present = .false. !< Flag indicating if any enclosures are present in the polygon dataset

contains

   !> Initialize module-level cellmask polygon data structures, such as the bounding boxes and iistart/iiend
   ! this keeps the actual calculation routines elemental.
   subroutine dbpinpol_cellmask_init(NPL, xpl, ypl, zpl)
      use m_alloc
      use geometry_module, only: get_startend

      integer, intent(in) :: NPL !< Number of polygon points
      real(kind=dp), intent(in) :: xpl(NPL), ypl(NPL), zpl(NPL) !< Polygon coordinate arrays

      integer :: MAXPOLY
      integer :: ipoint, istart, iend, ipoly

      if (cellmask_initialized) then
         call dbpinpol_cellmask_cleanup()
      end if

      if (NPL == 0) then
         cellmask_initialized = .true.
         return
      end if

      xpl_cellmask = xpl
      ypl_cellmask = ypl

      MAXPOLY = 1000
      call realloc(xpmin_cellmask, maxpoly, keepExisting=.false.)
      call realloc(xpmax_cellmask, maxpoly, keepExisting=.false.)
      call realloc(ypmin_cellmask, maxpoly, keepExisting=.false.)
      call realloc(ypmax_cellmask, maxpoly, keepExisting=.false.)
      call realloc(iistart_cellmask, maxpoly, keepExisting=.false.)
      call realloc(iiend_cellmask, maxpoly, keepExisting=.false.)
      call realloc(zpl_cellmask, maxpoly, keepExisting=.false.)

      ipoint = 1
      ipoly = 0

      do while (ipoint < NPL)
         ipoly = ipoly + 1
         if (ipoly > maxpoly) then
            maxpoly = ceiling(maxpoly * 1.1)
            call realloc(xpmin_cellmask, maxpoly, keepExisting=.true.)
            call realloc(xpmax_cellmask, maxpoly, keepExisting=.true.)
            call realloc(ypmin_cellmask, maxpoly, keepExisting=.true.)
            call realloc(ypmax_cellmask, maxpoly, keepExisting=.true.)
            call realloc(iistart_cellmask, maxpoly, keepExisting=.true.)
            call realloc(iiend_cellmask, maxpoly, keepExisting=.true.)
            call realloc(zpl_cellmask, maxpoly, keepExisting=.true.)
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

   end subroutine dbpinpol_cellmask_init

   !> Check if a point should be masked, either inside a dry-area polygon or outside an enclosure polygon.
   elemental function dbpinpol_cellmask(xp, yp) result(mask)

      integer :: mask
      real(kind=dp), intent(in) :: xp, yp !< Point coordinates

      integer :: ipoly, in_test
      integer :: count_drypoint
      logical :: found_inside_enclosure
      integer :: num_enclosures
      real(kind=dp) :: zpl_val

      mask = 0
      if (.not. cellmask_initialized) return

      num_enclosures = 0
      count_drypoint = 0
      found_inside_enclosure = .false.

      ! Single loop over all polygons
      do ipoly = 1, Npoly_cellmask
         zpl_val = zpl_cellmask(ipoly)

         ! Bounding box check
         if (xp < xpmin_cellmask(ipoly) .or. xp > xpmax_cellmask(ipoly) .or. &
             yp < ypmin_cellmask(ipoly) .or. yp > ypmax_cellmask(ipoly)) cycle

         ! Point-in-polygon test
         in_test = pinpok_elemental(xp, yp, ipoly)

         if (zpl_val == dmiss .or. zpl_val > 0.0_dp) then
            ! Dry point polygon
            if (in_test == 1) then
               count_drypoint = count_drypoint + 1
            end if
         else if (zpl_val < 0.0_dp .and. in_test == 1) then
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

      ! if an enclosure is present, the point must lie inside at least one
      ! NOTE: this means we do not handle nested enclosure polygons.
      if (enclosures_present .and. .not. found_inside_enclosure) then
         mask = 1
      end if

   end function dbpinpol_cellmask

   !> Clean up module-level cellmask polygon data structures.
   subroutine dbpinpol_cellmask_cleanup()

      if (allocated(xpmin_cellmask)) deallocate (xpmin_cellmask)
      if (allocated(xpmax_cellmask)) deallocate (xpmax_cellmask)
      if (allocated(ypmin_cellmask)) deallocate (ypmin_cellmask)
      if (allocated(ypmax_cellmask)) deallocate (ypmax_cellmask)
      if (allocated(zpl_cellmask)) deallocate (zpl_cellmask)
      if (allocated(iistart_cellmask)) deallocate (iistart_cellmask)
      if (allocated(iiend_cellmask)) deallocate (iiend_cellmask)

      Npoly_cellmask = 0
      cellmask_initialized = .false.

   end subroutine dbpinpol_cellmask_cleanup

   !> Optimized elemental point-in-polygon test using ray casting algorithm.
   !! Accesses polygon data via module arrays.
   elemental function pinpok_elemental(xl, yl, ipoly) result(inside)

      implicit none

      real(kind=dp), intent(in) :: xl, yl !< Point coordinates (scalar)
      integer, intent(in) :: ipoly !< Polygon index
      integer :: inside !< Result: 1=inside, 0=outside

      integer :: i, j, istart, iend, crossings
      real(kind=dp) :: x1, x2, y1, y2, xinters

      inside = 0

      ! Get polygon bounds from module variables
      istart = iistart_cellmask(ipoly)
      iend = iiend_cellmask(ipoly)

      if (iend - istart + 1 <= 2) then
         inside = 1
         goto 999
      end if

      ! Ray-casting algorithm
      crossings = 0
      j = iend

      do i = istart, iend
         if (xpl_cellmask(i) == dmiss) exit

         x1 = xpl_cellmask(j)
         y1 = ypl_cellmask(j)
         x2 = xpl_cellmask(i)
         y2 = ypl_cellmask(i)

         ! Check if point is on vertex
         if (xl == x1 .and. yl == y1) then
            inside = 1
            goto 999
         end if

         ! Check if ray crosses edge
         if ((y1 > yl) .neqv. (y2 > yl)) then
            xinters = x1 + (yl - y1) * (x2 - x1) / (y2 - y1)

            if (xl < xinters) then
               crossings = crossings + 1
            else if (xl == xinters) then
               inside = 1
               goto 999
            end if
         end if
         j = i
      end do

      inside = mod(crossings, 2)
999   continue
      if (jins == 0) inside = 1 - inside

   end function pinpok_elemental

end module m_dbpinpol_cellmask
