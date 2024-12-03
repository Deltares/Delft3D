!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

!> A cross-section path is defined by a polyline.
!! On the unstructured grid it then results in a set of flow links that
!! cross the polyline (both 1D and 2D).
!! Used for cross sections, and thin dams and dykes.
module m_crspath
   use precision, only: dp
   implicit none

!> Data type for storing the the polyline path and set of crossed flow
!! links.
   type tcrspath
      integer :: np !< Nr of polyline points
      integer :: lnx !< Nr. of flow links that cross the crs path
      integer, allocatable :: ln(:) !< Flow links (size=len) (sign defines orientation)
      integer, allocatable :: indexp(:) !< Index of segment in xp by which each link is crossed.
                                                   !! (between xp(i) and xp(i+1))
      real(kind=dp), allocatable :: wfp(:) !< Weightfactor of first point in crossed segment
                                                   !! as indicated in indexp (between 0 and 1).
      real(kind=dp), allocatable :: xp(:), yp(:), &
                                    zp(:) !< Polyline points that define the crs (size=np)
      real(kind=dp), allocatable :: xk(:, :), yk(:, :) !< For plotting only (size=2,lnx).
                                                   !! for all 'lnx' flow links, store both start
                                                   !! and end point because segments will not be ordered
                                                   !! nor connected.
      integer, allocatable :: iperm(:) !! permutation array of crossed flow links in increasing arc length order along cross section polyline
      real(kind=dp), allocatable :: sp(:) !! polygon arclength of flow link, dim()
      real(kind=dp), allocatable :: wfk1k2(:) !! per-flowlink interpolation weight factor between k1 (1) and k2 (0), dim(lnx)
   end type tcrspath

contains

!> Allocates the internal data for one crs path.
!! Based on polyline length and flow links upper limit.
   subroutine increaseCrossSectionPath(path, maxnp, maxlnx)
      use m_alloc
      type(tcrspath), intent(inout) :: path !< The path structure of a cross section.
      integer, intent(in) :: maxnp !< Max number of polyline points. If 0, nothing is done.
      integer, intent(in) :: maxlnx !< Max number of crossed flow links. If 0, nothing is done.

      integer :: m, mcur

      mcur = 0
      if (allocated(path%xp)) then
         mcur = size(path%xp)
      end if

      if (maxnp > 0 .and. maxnp > mcur) then
         m = max(2, int(1.5d0 * maxnp))
         call realloc(path%xp, m)
         call realloc(path%yp, m)
         call realloc(path%zp, m)
      end if

      mcur = 0
      if (allocated(path%ln)) then
         mcur = size(path%ln)
      end if

      if (maxlnx > 0 .and. maxlnx > mcur) then
         m = max(5, int(1.5d0 * maxlnx))
         call realloc(path%ln, m)

! GD: memory problems with realloc
         if (allocated(path%xk)) then
            call realloc(path%xk, (/2, m/))
            call realloc(path%yk, (/2, m/))
         else
            allocate (path%xk(2, m))
            allocate (path%yk(2, m))
         end if

         !if(allocated(path%xk)) deallocate(path%xk)
         !allocate(path%xk(2,m))

         !if(allocated(path%yk)) deallocate(path%yk)
         !allocate(path%yk(2,m))

         call realloc(path%indexp, m)
         call realloc(path%wfp, m)
         call realloc(path%wfk1k2, m)
         call realloc(path%sp, m)
         call realloc(path%iperm, m)
      end if
   end subroutine increaseCrossSectionPath

!> Deallocates the internal data for one crs path.
   subroutine deallocCrossSectionPath(path)
      type(tcrspath), intent(inout) :: path !< The path structure of a cross section

      if (allocated(path%xp)) then
         deallocate (path%xp)
         deallocate (path%yp)
         deallocate (path%zp)
      end if
      if (allocated(path%ln)) then
         deallocate (path%ln)
         deallocate (path%indexp)
         deallocate (path%wfp)
      end if
      if (allocated(path%xk)) then
         deallocate (path%xk, path%yk)
      end if
      if (allocated(path%sp)) then
         deallocate (path%sp)
      end if
      if (allocated(path%wfk1k2)) then
         deallocate (path%wfk1k2)
      end if
      if (allocated(path%iperm)) then
         deallocate (path%iperm)
      end if
   end subroutine deallocCrossSectionPath

!> Sets the cross section definition path to specified polyline coordinates.
   subroutine setCrossSectionPathPolyline(path, xp, yp, zp)
      use precision, only: dp
      type(tcrspath), intent(inout) :: path !< The crs path to be updated.
      real(kind=dp), intent(in) :: xp(:), yp(:) !< Polyline coordinates to define the crs path.
      real(kind=dp), optional, intent(in) :: zp(:) !< Optional z-values at xp/yp coordinates.

      integer :: i, n

      n = size(xp)
      if (n <= 0) return

      call increaseCrossSectionPath(path, n, 0)
      do i = 1, n
         path%xp(i) = xp(i)
         path%yp(i) = yp(i)
      end do

      if (present(zp)) then
         do i = 1, n
            path%zp(i) = zp(i)
         end do
      end if

      path%np = n
   end subroutine setCrossSectionPathPolyline

!> Copies a crspath into another, allocating memory for all points and links.
! AvD: TODO: repeated copying will increase the xp and ln arrays (because of grow factor)
   subroutine copyCrossSectionPath(pfrom, pto)
      type(tcrspath), intent(in) :: pfrom
      type(tcrspath), intent(inout) :: pto

      !integer :: maxnp, maxlnx
      !
      !if (allocated(pfrom%xp)) then
      !   maxnp  = size(pfrom%xp)
      !else
      !   maxnp = 0
      !end if
      !
      !if (allocated(pfrom%ln)) then
      !   maxlnx = size(pfrom%ln)
      !else
      !   maxlnx = 0
      !end if
      !
      !call increaseCrossSectionPath(pto, maxnp, maxlnx)

      ! Structures may directly be copied, including their allocatable components (F2003)
      pto = pfrom
   end subroutine copyCrossSectionPath

!> Increases the size of an *array* of crspath elements.
!! All existing elements (up to #numcur) are copied.
   subroutine increaseCRSPaths(paths, numnew, numcur)
      type(tcrspath), allocatable, intent(inout) :: paths(:)
      integer, intent(inout) :: numnew !< Desired new size (may turn out larger).
      integer, intent(in) :: numcur !< Current nr of paths in array
                                                         !! (will be copied, actual array size may be larger)

      type(tcrspath), allocatable :: pathst(:)
      integer :: i, numcurmax

      if (allocated(paths)) then
         numcurmax = size(paths)
         if (numnew < numcurmax) then
            return
         end if
      else
         numcurmax = 0
      end if
      numnew = max(numnew, int(numcurmax * 1.2))

      ! Allocate temp array of cross section paths.
      allocate (pathst(numcur))

      ! Fill temp paths and deallocate each original cross section path.
      do i = 1, numcurmax
         if (i <= numcur) then
            call copyCrossSectionPath(paths(i), pathst(i))
         end if
         call deallocCrossSectionPath(paths(i))
      end do
      ! Deallocate original crspath array
      if (allocated(paths)) then
         deallocate (paths)
      end if

      ! Re-allocate original crspath array at bigger size and fill it.
      allocate (paths(numnew))
      do i = 1, numcur
         call copyCrossSectionPath(pathst(i), paths(i))
         call deallocCrossSectionPath(pathst(i))
      end do
      deallocate (pathst)
   end subroutine increaseCRSPaths

!> Check for crossing of a (flow) link by a crs path.
!! When crossed, the link info (its number and coordinates) are stored
!! in the path structure. Any existing link info is preserved!
!! This routine can be used with 'network geometry' (e.g. for thin dams)
!! and 'flow geometry' (e.g. for cross sections and fixed weirs).
   subroutine crspath_on_singlelink(path, linknr, xk3, yk3, xk4, yk4, xza, yza, xzb, yzb, zork)
      use precision, only: dp

      use geometry_module, only: crossinbox
      use m_sferic, only: jsferic
      use m_missing, only: dmiss
      implicit none

      type(tcrspath), intent(inout) :: path !< Path that is checked for link crossing, will be updated with link info.
      integer, intent(in) :: linknr !< Number of link that is being checked, will be stored in path%ln
      integer, intent(in) :: zork !< Crossing checked using xz or xk
      real(kind=dp), intent(in) :: xk3, yk3, xk4, yk4 !< Net node coordinates of this link (or fictious coords for a 1D link)
      real(kind=dp), intent(in) :: xza, yza, xzb, yzb !< cell circum. coordinates of this link.

      integer :: ip, jacros
      real(kind=dp) :: SL, SM, XCR, YCR, CRP

!   Check whether flow link intersects with a polyline segment of this cross section path.
      do ip = 1, path%np - 1
         crp = 0d0
         if (zork == 1) then
            call CROSSinbox(path%XP(ip), path%YP(ip), path%XP(ip + 1), path%YP(ip + 1), xza, yza, xzb, yzb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
         else
            call CROSSinbox(path%XP(ip), path%YP(ip), path%XP(ip + 1), path%YP(ip + 1), xk3, yk3, xk4, yk4, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
         end if
         if (jacros == 1) then
            if (SM == 1d0) then
               if (crp > 0d0) then
                  cycle
               end if
            else if (SM == 0d0) then
               if (crp < 0d0) then
                  cycle
               end if
            end if

            call increaseCrossSectionPath(path, 0, path%lnx + 1)
            path%lnx = path%lnx + 1

            path%indexp(path%lnx) = ip
            path%wfp(path%lnx) = 1d0 - SL ! SL=rel.pos on segment. Weight of left points is 1-SL
            path%wfk1k2(path%lnx) = 1d0 - SM ! SM=rel.pos on flow link       of left points is 1-SM

            if (crp < 0d0) then
               path%ln(path%lnx) = linknr
               path%xk(1, path%lnx) = xk3
               path%yk(1, path%lnx) = yk3
               path%xk(2, path%lnx) = xk4
               path%yk(2, path%lnx) = yk4
            else
!               Flip flow link orientation, such that its flow direction is rightward through crs path polygon
               path%ln(path%lnx) = -linknr
               path%xk(1, path%lnx) = xk4
               path%yk(1, path%lnx) = yk4
               path%xk(2, path%lnx) = xk3
               path%yk(2, path%lnx) = yk3
            end if

         end if
      end do
   end subroutine crspath_on_singlelink

!> Converts a set of polylines into paths.
!! The input arrays (xpl, ypl, zpl) have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
   subroutine pol_to_flowlinks(xpl, ypl, zpl, npl, ns, paths)
      use precision, only: dp
      use m_missing

      real(kind=dp), intent(in) :: xpl(:), ypl(:), zpl(:) !< Long array with one or more polylines, separated by dmiss
      integer, intent(in) :: npl !< Total number of polyline points
      type(tcrspath), allocatable :: paths(:)
      integer, intent(out) :: ns

      integer :: i, i1, i2, maxfxw

      ns = 0

      i1 = 1 ! First possible start index
      i2 = 0 ! No end index found yet.
      do i = 1, npl
         if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
               i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
               maxfxw = ns + 1
               call increaseCRSPaths(paths, maxfxw, ns)
               ns = ns + 1
               call setCrossSectionPathPolyline(paths(ns), xpl(i1:i2), ypl(i1:i2), zpl(i1:i2))
            end if
            i1 = i + 1
            cycle
         else
            i2 = i ! Advance end point by one.
         end if
      end do
   end subroutine pol_to_flowlinks

end module m_crspath
