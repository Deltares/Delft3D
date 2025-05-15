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

module m_crosssections_on_flowgeom

   implicit none

   private

   public :: crosssections_on_flowgeom

contains

   subroutine crosssections_on_flowgeom()
      use precision, only: dp
      use m_monitoring_crosssections
      use m_flowgeom, only: Lnx
      use m_missing
      use kdtree2Factory
      use dfm_error
      use unstruc_channel_flow
      use m_inquire_flowgeom
      use unstruc_caching, only: copy_cached_cross_sections, save_link_list
      use m_partitioninfo, only: jampi
      use m_readyy
      use m_wall_clock_time
      use m_find_crossed_links_kdtree2
      use m_crspath_on_flowgeom

      integer :: ic, icmod

      real(kind=dp), dimension(:), allocatable :: xx, yy
      real(kind=dp), dimension(:), allocatable :: polygon_segment_weights
      integer, dimension(:), allocatable :: crossed_links, polygon_nodes, istartcrs, numlist
      integer, dimension(:, :), allocatable :: linklist
      integer, dimension(:), allocatable :: idum

      integer :: i, num, intersection_count, ierror
      integer :: istart, iend

      integer :: jakdtree = 1
      real(kind=dp) :: t0, t1
      character(len=128) :: mesg
      integer :: linknr, ii, branchIdx
      type(t_observCrossSection), pointer :: pCrs
      logical :: success

      if (ncrs < 1) return

      intersection_count = 0

!   allocate
      allocate (istartcrs(ncrs + 1))
      istartcrs = 1

      allocate (idum(1))
      idum = 0

      if (jakdtree == 1) then
         call wall_clock_time(t0)

         call copy_cached_cross_sections(crossed_links, polygon_nodes, success)

         if (success) then
            intersection_count = size(crossed_links)
            ierror = 0
         else
            num = 0
!           determine polyline size
            do ic = 1, ncrs
               if (crs(ic)%loc2OC == 0) then ! only for crs which are polyline-based
                  num = num + crs(ic)%path%np + 1 ! add space for missing value
                  istartcrs(ic + 1) = num + 1
               end if
            end do

!           allocate
            allocate (xx(num), yy(num))

!           determine paths to single polyline map
            num = 0
            do ic = 1, ncrs
               if (crs(ic)%loc2OC == 0) then
                  do i = 1, crs(ic)%path%np
                     num = num + 1
                     xx(num) = crs(ic)%path%xp(i)
                     yy(num) = crs(ic)%path%yp(i)
                  end do
!              add missing value
                  num = num + 1
                  xx(num) = DMISS
                  yy(num) = DMISS
               end if
            end do

!           allocate
            allocate (crossed_links(Lnx))
            crossed_links = 0
            allocate (polygon_nodes(Lnx))
            polygon_nodes = 0
            allocate (polygon_segment_weights(Lnx))
            polygon_segment_weights = 0d0
            call find_crossed_links_kdtree2(treeglob, num, xx, yy, ITYPE_FLOWLINK, Lnx, BOUNDARY_ALL, intersection_count, crossed_links, polygon_nodes, polygon_segment_weights, ierror)

            call save_link_list(intersection_count, crossed_links, polygon_nodes)
         end if

         if (ierror == 0 .and. intersection_count > 0) then

!          determine crossed links per cross-section
            allocate (numlist(ncrs))
            numlist = 0
            allocate (linklist(intersection_count, ncrs))
            linklist = 0

            do i = 1, intersection_count
               do ic = 1, ncrs
                  if (crs(ic)%loc2OC == 0) then
                     istart = istartcrs(ic)
                     iend = istartcrs(ic + 1) - 1
                     if (polygon_nodes(i) >= istart .and. polygon_nodes(i) <= iend) then
                        numlist(ic) = numlist(ic) + 1
                        linklist(numlist(ic), ic) = crossed_links(i)
                     end if
                  end if
               end do
            end do

         else
!          disable kdtree
            jakdtree = 0
            ! allocate(idum(1))
            ! idum = 0

!          deallocate
            if (allocated(crossed_links)) then
               deallocate (crossed_links)
            end if
            if (allocated(polygon_nodes)) then
               deallocate (polygon_nodes)
            end if
            if (allocated(polygon_segment_weights)) then
               deallocate (polygon_segment_weights)
            end if
         end if

!       deallocate
         if (allocated(istartcrs)) then
            deallocate (istartcrs)
         end if
         if (allocated(xx)) deallocate (xx, yy)

         call wall_clock_time(t1)
         write (mesg, "('cross sections with kdtree2, elapsed time: ', G15.5, 's.')") t1 - t0
         call mess(LEVEL_INFO, trim(mesg))
      end if

      icMOD = max(1, ncrs / 100)

      call realloc(numlist, ncrs, keepExisting=.true., fill=0) ! In case pli-based cross sections have not allocated this yet.
      call realloc(linklist, (/max(intersection_count, 1), ncrs/), keepExisting=.true., fill=0) ! In addition to pli-based cross sections (if any), also support 1D branchid-based cross sections.

      call copy_cached_cross_sections(crossed_links, polygon_nodes, success)

      call READYY('Enabling cross sections on grid', 0d0)
      do ic = 1, ncrs
         if (mod(ic, icMOD) == 0) then
            call READYY('Enabling cross sections on grid', dble(ic) / dble(ncrs))
         end if
         if (crs(ic)%loc2OC == 0) then
            if (.not. success) then
               if (jakdtree == 0) then
                  call crspath_on_flowgeom(crs(ic)%path, 0, 0, 1, idum, 0, 1)
               else
                  call crspath_on_flowgeom(crs(ic)%path, 0, 1, numlist(ic), linklist(1, ic), 0, 1)
               end if
            end if
         else ! snap to only 1d flow link
            ii = crs(ic)%loc2OC
            pCrs => network%observcrs%observcross(ii)
            branchIdx = pCrs%branchIdx
            ierror = findlink(branchIdx, pCrs%chainage, linknr) ! find flow link given branchIdx and chainage
            if (linknr == -1) then
               if (ierror /= DFM_NOERR) then
                  call SetMessage(LEVEL_ERROR, 'Error occurs when snapping Observation cross section '''//trim(crs(ic)%name)//''' to a 1D flow link.')
                  call SetMessage(LEVEL_ERROR, 'Possibly wrong branchId? Given branchId is: '''//trim(pCrs%branchid)//'''.')
               else if (jampi > 0) then
                  continue ! Most probably on another domain
               else
                  ! Sequential model with correct branchId, but still no link: possibly chainage outside length range of branch? Warning only.
                  call SetMessage(LEVEL_WARN, 'Error occurs when snapping Observation cross section '''//trim(crs(ic)%name)//''' to a 1D flow link.')
                  write (msgbuf, '(a,g10.3,a,g10.3,a)') 'Possibly wrong chainage? Given chainage is: ', pCrs%chainage, &
                     ' on branchId '''//trim(pCrs%branchId)//''' (length = ', network%brs%branch(branchIdx)%length, ').'
                  call SetMessage(LEVEL_WARN, trim(msgbuf))
               end if
            else ! valid flowlink found
               numlist(ic) = 1
               linklist(1, ic) = linknr
               call crspath_on_flowgeom(crs(ic)%path, 0, 1, numlist(ic), linklist(1, ic), 1, 1)
            end if
         end if
      end do

      call READYY('Enabling cross sections on grid', -1d0)

1234  continue

!   deallocate
      if (jakdtree == 1) then
         if (allocated(crossed_links)) then
            deallocate (crossed_links)
         end if
         if (allocated(polygon_nodes)) then
            deallocate (polygon_nodes)
         end if
         if (allocated(polygon_segment_weights)) then
            deallocate (polygon_segment_weights)
         end if
         if (allocated(numlist)) then
            deallocate (numlist)
         end if
         if (allocated(linklist)) then
            deallocate (linklist)
         end if
      end if

      if (allocated(idum)) then
         deallocate (idum)
      end if

      return
   end subroutine crosssections_on_flowgeom

end module m_crosssections_on_flowgeom
