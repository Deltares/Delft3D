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
      use m_flowgeom
      use kdtree2Factory
      use m_sferic
      use messagehandling, only: LEVEL_INFO, LEVEL_ERROR, mess
      use m_missing, only: dmiss
      use geometry_module, only: dbdistance, crossinbox
      use m_readyy
      use m_get_link_neighboring_cell_coords
      use m_movabs
      use m_lnabs

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

      if (NPL < 1) goto 1234 ! nothing to do

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
      allocate (treeinst%qv(NTREEDIM))

!     determine maximum polygon section length, and administer polygon sections
      dmaxpollen = 0d0
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

!     build kdtree
      allocate (treeinst%sample_coords(NTREEDIM, NPL - 1))

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
            af = dble(L) / dble(LnxiORLnx)
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
            n1 = ln(1, L); n2 = ln(2, L)
            xa = xz(n1); ya = yz(n1)
            xb = xz(n2); yb = yz(n2)
         else if (itype == ITYPE_NETLINK) then ! netlinks, cross with netlinks
            n1 = kn(1, L)
            n2 = kn(2, L)
            xa = xk(n1)
            ya = yk(n1)
            xb = xk(n2)
            yb = yk(n2)
         else if (itype == ITYPE_FLOWLINK_1D_DUAL) then
            if (L <= lnx1D) then ! flowlinks, cross with perpendicular in 1D
               n1 = ln(1, L); n2 = ln(2, L)
               xc = xz(n1); yc = yz(n1)
               xd = xz(n2); yd = yz(n2)
               xa = 0.5d0 * (xc + xd) - 0.5d0 * (yd - yc)
               ya = 0.5d0 * (yc + yd) + 0.5d0 * (xd - xc)
               xb = 0.5d0 * (xc + xd) + 0.5d0 * (yd - yc)
               yb = 0.5d0 * (yc + yd) - 0.5d0 * (xd - xc)
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
         R2search = 1.1d0 * (dlinlen + dmaxpollen)**2 ! 1.1d0: safety

!        count number of points in search area
         NN = kdtree2_r_count(treeinst%tree, treeinst%qv, R2search)

         if (NN == 0) cycle ! no links found

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

      call readyy(' ', -1d0)

      call mess(LEVEL_INFO, 'done')

      ierror = 0
1234  continue

!     deallocate
      if (treeinst%itreestat /= ITREE_EMPTY) call delete_kdtree2(treeinst)
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

   !> sort intersections of crossed_links first on links and subsequently polygon index
   subroutine sort_crossed_links(crossed_links, polygon_nodes, polygon_segment_weights, n_links_polyline_nodes, intersection_count)
      use stdlib_sorting, only: sort_index
      use precision, only: dp

      integer, dimension(n_links_polyline_nodes), intent(in) :: crossed_links !< crossed link indices
      integer, dimension(n_links_polyline_nodes), intent(inout) :: polygon_nodes !< starting node of intersected polygon segment
      real(kind=dp), dimension(n_links_polyline_nodes), intent(inout) :: polygon_segment_weights !< relative length of polygon segment from starting node till intersection
      integer, intent(in) :: n_links_polyline_nodes !< array_size e.g. number of links ( Lnx for flowlinks, numL for netlinks) or npl for number of polyline nodes
      integer, intent(in) :: intersection_count !< number of link intersections

      integer, dimension(n_links_polyline_nodes) :: new_index !< index of sorted polygon_nodes
      real(kind=dp), dimension(n_links_polyline_nodes) :: polygon_segment_weights_copy !< copy of intersection length polygon_segment_weights

      integer :: k
      integer :: n
      integer :: n_start
      integer :: n_end

      polygon_segment_weights_copy = polygon_segment_weights
      ! The following loop finds the n_start and n_end of which have the same crossed_link value
      n_start = 1 ! initialise n_start
      do n = 1, intersection_count
         do n_end = n_start, intersection_count - 1
            if (crossed_links(n_end + 1) > crossed_links(n_start)) then
               exit ! the next crossed_link value is larger than the considered crossed_link value, so n_end is found
            end if
         end do
         call sort_index(polygon_nodes(n_start:n_end), new_index(n_start:n_end)) ! sorts polygon_nodes, and returns sorting order in new_index
         do k = n_start, n_end
            polygon_segment_weights(k) = polygon_segment_weights_copy(n_start - 1 + new_index(k)) ! sort polygon_segment_weights according to the same ordering
         end do
         n_start = n_end + 1 ! reinitialise n_start
         if (n_start > intersection_count) then
            exit ! if n_start exceeds the intersection_count then the loop is complete
         end if
      end do
   end subroutine

end module m_find_crossed_links_kdtree2
