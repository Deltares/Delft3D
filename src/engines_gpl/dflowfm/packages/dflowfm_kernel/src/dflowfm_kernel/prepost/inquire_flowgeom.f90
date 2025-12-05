!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

!> This module contains general functions for snapping locations to either flowlink numbers or flownode numbers
module m_inquire_flowgeom
   use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
   use network_data, only: LINK_1D, LINK_2D, LINK_1D2D_INTERNAL, LINK_1D2D_LONGITUDINAL, LINK_1D2D_STREETINLET, LINK_1D_MAINBRANCH, LINK_1D2D_ROOF, LINK_ALL

   implicit none

   private

   public findlink !< find flowlink number
   public findnode !< find flownode number
   public findlink_by_nodeid !< find the flow link number, using node id
   public findlink_by_contactid !< find the flow link number, using contactId

   interface findlink
      module procedure findlink_by_pli !< find the flow link number, using a polyline
      module procedure findlink_by_branchindex !< find the flow link number, using (branch index, chainage)
      module procedure findlink_by_branchid !< find the flow link number, using (branch id, chainage)
      module procedure findlink_by_structureid !< find the flow link number, using a structure id
   end interface

   interface findnode
      module procedure findnode_by_pol !< find the flow node number, using a polygon
      module procedure findnode_by_id !< find the flow node number, using node Id
      module procedure findnode_by_branchindex !< find the flow node number, using (branch index, chainage)
      module procedure findnode_by_branchid !< find the flow node number, using (branch id, chainage)
   end interface

contains

   !> Find flow link number(s) intersected by a given polyline.
   function findlink_by_pli(npl, xpl, ypl, Larr, numlinks, lftopol, sortlinks, linktype) result(ierr)
      use precision, only: dp
      use m_flowgeom, only: xz, yz, ln, lnx, lnx1D
      use stdlib_sorting, only: sort_index
      use dfm_error, only: dfm_noerr
      use m_crosspoly, only: crosspoly

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      integer, intent(in) :: npl !< Number of polyline points.
      real(kind=dp), intent(in) :: xpl(:) !< x-coordinates of the polyline.
      real(kind=dp), intent(in) :: ypl(:) !< y-coordinates of the polyline.
      integer, intent(out) :: Larr(:) !< array with flow links, intersected by the polyline. Length is the resonsibility of the call site.
      integer, intent(out) :: numlinks !< Number of found flow links.
      integer, optional, intent(in) :: sortlinks !< Indicates whether the flow links have to be sorted.
      integer, optional, intent(in) :: linktype !< Limit search to specific link types: only 1D flow links (linktype==LINK_1D), 2D (linktype==LINK_2D), or both (linktype==LINK_ALL).
      integer, optional, intent(inout) :: lftopol(:) !< Mapping array from flow link to intersecting polyline segment.

      real(kind=dp) :: xa, ya
      real(kind=dp) :: xb, yb
      real(kind=dp) :: xm, ym
      real(kind=dp) :: crpm
      real(kind=dp) :: dist
      real(kind=dp), allocatable :: distsStartPoly(:)
      integer, allocatable :: sortedIndices(:)
      integer, allocatable :: tempLinkArray(:)
      integer :: found
      integer :: size_arr
      integer :: L
      integer :: Lstart, Lend
      integer :: isec
      integer :: k1, k2

      ierr = DFM_NOERR

      size_arr = size(Larr)
      numlinks = 0
      if (present(sortLinks)) then
         allocate (distsStartPoly(lnx))
      end if

      ! select search range for flow links
      if (present(linktype)) then
         select case (linktype)
         case (LINK_1D)
            Lstart = 1
            Lend = lnx1D
         case (LINK_2D)
            Lstart = lnx1D + 1
            Lend = lnx
         case (LINK_ALL)
            Lstart = 1
            Lend = lnx
         end select
      else
         Lstart = 1
         Lend = lnx
      end if

      do L = Lstart, Lend
         k1 = ln(1, L)
         k2 = ln(2, L)
         xa = xz(k1)
         ya = yz(k1)
         xb = xz(k2)
         yb = yz(k2)

         call crosspoly(xa, ya, xb, yb, xpl, ypl, npl, XM, YM, CRPM, found, isec, dist)

         if (found == 1) then
            numlinks = numlinks + 1
            if (numlinks > size_arr) then
               ! internal error insufficient space in links array
               ierr = -1
               return
            end if
            if (present(lftopol)) then
               lftopol(numlinks) = isec
            end if

            if (crpm > 0) then
               Larr(numlinks) = -L
            else
               Larr(numlinks) = L
            end if
            if (present(sortLinks)) then
               distsStartPoly(numlinks) = dist
            end if
         end if
      end do

      ! if required, sort the links by distance along the polyline
      if (present(sortLinks) .and. numlinks > 0) then
         if (sortLinks == 1) then
            allocate (sortedIndices(numlinks))
            allocate (tempLinkArray(numlinks))

            call sort_index(distsStartPoly(1:numlinks), sortedIndices)
            do L = 1, numlinks
               tempLinkArray(L) = Larr(sortedIndices(L))
            end do
            Larr(1:numlinks) = tempLinkArray
         end if
      end if
      ! Rely on automatic de-allocation of modern Fortran
   end function findlink_by_pli

   !> Find the nearest flow link number for a given location, using (branch index, chainage).
   function findlink_by_branchindex(branchindex, chainage, L) result(ierr)
      use precision, only: dp
      use unstruc_channel_flow, only: network, getlinkindex
      use dfm_error, only: dfm_noerr

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      integer, intent(in) :: branchindex !< Branch index in network brs set.
      real(kind=dp), intent(in) :: chainage !< Chainage of item on the branch with index branchindex
      integer, intent(out) :: L !< Found flow link number, -1 when not found.

      L = -1
      ierr = DFM_NOERR

      if (branchIndex >= 1 .and. branchIndex <= network%brs%Count) then
         L = getLinkIndex(network%brs%branch(branchIndex), chainage)
      else
         ierr = -1
      end if

   end function findlink_by_branchindex

   !> Find the nearest flow link number for a given location, using (branch id, chainage).
   function findlink_by_branchid(branchid, chainage, L) result(ierr)
      use precision, only: dp
      use unstruc_channel_flow, only: network
      use m_hash_search, only: hashsearch
      use dfm_error, only: dfm_noerr
      use messagehandling, only: idlen

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      character(len=idlen), intent(in) :: branchid !< Branch Id to be searched in network brs set.
      real(kind=dp), intent(in) :: chainage !< Chainage of item on the branch with index branchindex.
      integer, intent(out) :: L !< Found flow link number, -1 when not found.

      integer :: branchindex

      L = -1
      ierr = DFM_NOERR

      branchindex = hashsearch(network%brs%hashlist, branchid)
      if (branchindex > 0) then
         ierr = findlink_by_branchindex(branchindex, chainage, L)
      else
         ierr = -1
      end if

   end function findlink_by_branchid

   !> Find the unique flow link number for a given location, using contactId.
   function findlink_by_contactid(contactId, L) result(ierr)
      use unstruc_channel_flow
      use m_flowgeom, only: lne2ln
      use m_save_ugrid_state, only: hashlist_contactids, contactnetlinks
      use m_hash_search
      use dfm_error
      use messagehandling, only: idlen

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      character(len=idlen), intent(in) :: contactId !< contactId to be searched in mesh contact set.
      integer, intent(out) :: L !< Found flow link number, -1 when not found.

      integer :: LL

      L = -1
      ierr = DFM_NOERR

      LL = hashsearch(hashlist_contactids, contactId)
      if (LL > 0) then
         L = lne2ln(contactnetlinks(LL))
      end if

   end function findlink_by_contactid

   !> find the flow link number, using node id
   function findlink_by_nodeid(nodeId, L) result(ierr)
      use dfm_error
      use m_hash_search
      use unstruc_channel_flow
      use m_branch
      use precision_basics, only: comparereal
      use m_GlobalParameters, only: flow1d_eps10
      use messagehandling, only: idlen

      integer :: ierr
      character(len=idlen), intent(in) :: nodeId !< Id of the connection node
      integer, intent(out) :: L !< Found link number, -1 when not found.

      integer :: nodeindex
      integer :: ibr, branch_count
      type(t_branch), pointer :: pbranch
      ierr = DFM_NOERR
      L = -1
      nodeindex = hashsearch(network%nds%hashlist, nodeId)
      if (nodeindex == -1) then
         ierr = -1
         return
      end if

      branch_count = network%brs%Count
      do ibr = 1, branch_count
         pbranch => network%brs%branch(ibr)

         if (pbranch%gridPointsCount == 0) then
            cycle
         end if

         ! If it is branch start node and first grid point is also on that start of branch:
         if (pbranch%fromnode%index == nodeindex &
             .and. comparereal(pbranch%gridPointsChainages(1), 0.0_dp, flow1d_eps10) == 0) then
            if (L == -1) then
               L = pbranch%lin(1)
            else
               ! Multiple branches attached to this node: cannot choose which link.
               ierr = -1
               return
            end if
            ! Else If it is branch end node and last grid point is also on that end of branch:
         elseif (pbranch%tonode%index == nodeindex &
                 .and. comparereal(pbranch%gridPointsChainages(pbranch%gridPointsCount), pbranch%length, flow1d_eps10) == 0) then
            if (L == -1) then
               L = pbranch%lin(pbranch%uPointsCount)
            else
               ! Multiple branches attached to this node: cannot choose which link.
               ierr = -1
               return
            end if
         end if
      end do

   end function findlink_by_nodeid

   !> Find flow link number for a given structure id.
   !! If not found, then L = -1 .
   function findlink_by_structureid(strucid, L) result(ierr)
      use dfm_error
      use unstruc_channel_flow
      use messagehandling, only: idlen

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      character(len=*), intent(in) :: strucid !< Structure id
      integer, intent(out) :: L !< Found flow link number, -1 when not found.

      integer :: i
      character(len=idlen) :: strucid_tmp

      L = -1
      ierr = DFM_NOERR

      do i = 1, network%sts%Count
         strucid_tmp = network%sts%struct(i)%id
         if (trim(strucid_tmp) == trim(strucid) .and. network%sts%struct(i)%numlinks > 0) then
            L = network%sts%struct(i)%linknumbers(1)
            ! NOTE: intentionally no abs() here, but also not on call site: L = -1 means not found, and for 1D structures, linknumber(1) is always > 0.
            exit
         end if
      end do

      return
   end function findlink_by_structureid

   !> find flow node number(s), enclosed in a polygon
   function findnode_by_pol(npol, xpol, ypol, points, numpoints, nodetype) result(ierr)
      use precision, only: dp
      use m_flowgeom, only: xz, yz, ndx2D, ndxi
      use m_polygon, only: xpl, ypl, npl, increasepol
      use dfm_error

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      integer, intent(in) :: npol !< number of points in the polygon
      real(kind=dp), intent(in) :: xpol(:) !< x-coordinates of the points in the polygon
      real(kind=dp), intent(in) :: ypol(:) !< y-coordinates of the points in the polygon
      integer, intent(out) :: points(:) !< array with points, inside the polygon. Length is the resonsibility of the call-side
      integer, intent(out) :: numpoints !< number of found links
      integer, optional, intent(in) :: nodetype !< select for search range only 1D nodes (nodetype==FL_1D), 2d (nodetype==FL_2D), or both (nodetype==FL_1D+FL_2D)

      integer :: n
      integer :: nstart
      integer :: nend
      integer :: in
      integer :: size_points

      ierr = DFM_NOERR

      ! 1:ndx2D, ndx2D+1:ndxi, ndxi+1:ndx1Db, ndx1Db:ndx
      if (present(nodetype)) then
         select case (nodetype)
         case (LINK_1D)
            nstart = ndx2D + 1
            nend = ndxi
         case (LINK_2D)
            nstart = 1
            nend = ndx2D
         case (LINK_ALL)
            nstart = 1
            nend = ndxi
         end select
      else
         nstart = 1
         nend = ndxi
      end if

      ! initialize polygon module.
      call increasepol(npol, 0)
      npl = npol
      xpl(1:npol) = xpol(1:npol)
      ypl(1:npol) = ypol(1:npol)

      ! make sure inwhichpolygon initializes some variables first.
      in = -1
      size_points = size(points)
      numpoints = 0
      do n = nstart, nend
         call inwhichpolygon(xz(n), yz(n), in)
         if (in > 0) then
            numpoints = numpoints + 1
            if (numpoints > size_points) then
               ierr = -1
               return
            end if
            points(numpoints) = n
         end if
      end do

   end function findnode_by_pol

   !> Find the flow node number, using node Id.
   function findnode_by_id(nodeId, nodenr) result(ierr)
      use m_hash_search
      use unstruc_channel_flow
      use dfm_error
      use messagehandling, only: idlen

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      character(len=idlen), intent(in) :: nodeId !< Id of the connection node
      integer, intent(out) :: nodenr !< Found flow node number, -1 when not found.

      integer :: nodeindex

      nodenr = -1
      ierr = DFM_NOERR

      nodeindex = hashsearch(network%nds%hashlist, nodeId)
      if (nodeindex > 0) then
         nodenr = network%nds%node(nodeindex)%gridNumber
         if (nodenr <= 0) then
            ierr = -1
         end if
      else
         ierr = -1
      end if

   end function findnode_by_id

   !> find the flow node number, using (branch id, chainage).
   function findnode_by_branchid(branchId, chainage, nodenr) result(ierr)
      use precision, only: dp
      use m_hash_search
      use unstruc_channel_flow
      use dfm_error
      use messagehandling, only: idlen

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      character(len=idlen), intent(in) :: branchid !< branch Id
      real(kind=dp), intent(in) :: chainage !< chainage of item on the branch with id branchid
      integer, intent(out) :: nodenr !< Found flow node number, -1 when not found.

      integer :: branchindex

      nodenr = -1
      ierr = DFM_NOERR

      branchindex = hashsearch(network%brs%hashlist, branchid)
      if (branchindex > 0) then
         ierr = findnode_by_branchindex(branchindex, chainage, nodenr)
      else
         ierr = -1
      end if

   end function findnode_by_branchid

   !> Find the flow node number, using (branch index, chainage).
   function findnode_by_branchindex(branchIndex, chainage, nodenr) result(ierr)
      use precision, only: dp
      use m_hash_search
      use unstruc_channel_flow
      use dfm_error

      integer :: ierr !< Result status, DFM_NOERR in case of success.
      integer, intent(in) :: branchindex !< branch index
      real(kind=dp), intent(in) :: chainage !< chainage of item on the branch with index branchindex
      integer, intent(out) :: nodenr !< Found flow node number, -1 when not found.

      nodenr = -1
      ierr = DFM_NOERR

      if (branchIndex >= 1 .and. branchIndex <= network%brs%Count) then
         nodenr = getGridPointNumber(network%brs%branch(branchindex), chainage)
         if (nodenr == 0) then
            ierr = -1
         end if
      else
         ierr = -1
      end if

   end function findnode_by_branchindex

end module m_inquire_flowgeom
