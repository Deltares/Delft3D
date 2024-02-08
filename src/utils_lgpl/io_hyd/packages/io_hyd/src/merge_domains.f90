!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
!-------------------------------------------------------------------------------
!
!
      subroutine merge_domains(hyd, domain_hyd_coll)

      ! function : merge the domains, make pointers to final domain

      ! global declarations

      use hydmod
      use m_alloc
      use precision_basics, only : comparereal
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                    ! description of the hydrodynamics
      type(t_hyd_coll)                       :: domain_hyd_coll        ! description of all domain hydrodynamics

      ! local declarations

      integer                                :: n_domain               ! number of domains
      integer                                :: i_domain               ! index in collection
      integer                                :: idmn                   ! flow like domain index (0:n_domain-1)
      type(t_hyd), pointer                   :: d_hyd                  ! description of one domain hydrodynamics
      type(t_hyd), pointer                   :: l_hyd                  ! description of a linked domain hydrodynamics
      integer                                :: nosegl                 ! total number of segments per layer
      integer                                :: nobnd                  ! total number of boundaries
      integer                                :: nobndl                 ! total number of boundaries per layer
      integer                                :: noq1                   ! total number of exchanges in first directory
      integer                                :: iseg                   ! segment index
      integer                                :: isegl                  ! segment index in layer
      integer                                :: iseg_global            ! global segment index
      integer                                :: ilay                   ! layer index
      integer                                :: iq                     ! exchange index
      integer                                :: iq_global              ! global exchange index layer
      integer                                :: ip1                    ! segment pointer index
      integer                                :: ip2                    ! segment pointer index
      integer                                :: ip3                    ! segment pointer index
      integer                                :: ip4                    ! segment pointer index
      integer                                :: no_sect                ! number of sections
      integer                                :: i_sect                 ! index of section
      integer                                :: isect                  ! index of section
      integer                                :: no_bnd                 ! number of boundaries in section
      integer                                :: i_bnd                  ! index of boundary
      type(t_openbndsect), pointer           :: openbndsect            ! single section
      type(t_openbndlin),pointer             :: openbndlin             ! single open boundary lin
      type(t_openbndsect)                    :: new_sect               ! single section new
      logical                                :: bnd_active             ! if a boundary is active
      integer                                :: iret                   ! return value
      integer                                :: ik                     ! node counter
      integer                                :: iface                  ! face index
      integer                                :: min_seg                ! minimum segment index
      integer                                :: max_seg                ! maximum segment index
      integer                                :: iedge                  ! edge index
      integer                                :: global_node            ! global node
      integer                                :: global_edge            ! global adge
      integer                                :: itype                  ! edge index
      integer                                :: edge_type              ! edge type
      integer                                :: l_iedge                ! edge index in linked domain
      integer                                :: global_face            ! global face in domain
      integer                                :: l_global_face          ! global face in linked domain
      integer                                :: inode                  ! node index
      integer                                :: inode1                 ! node
      integer                                :: inode2                 ! node
      integer                                :: l_node                 ! node index
      integer                                :: l_inode                ! node index
      integer                                :: l_face                 ! linked face
      integer                                :: face_link              ! current linked face
      double precision                       :: d_x1                   ! x-coordinate of first edge node
      double precision                       :: d_y1                   ! y-coordinate of first edge node
      double precision                       :: d_x2                   ! x-coordinate of first edge node
      double precision                       :: d_y2                   ! y-coordinate of first edge node
      double precision                       :: l_x1                   ! x-coordinate of first linked edge node
      double precision                       :: l_y1                   ! y-coordinate of first linked edge node
      integer, allocatable                   :: globface_domain(:)     ! domain of global face
      integer, allocatable                   :: globface_face(:)       ! local face number of global face
      integer, parameter                     :: edge_type_orde(4) = [1, 2, 0, 3]

      ! allocate local arrays
      n_domain = domain_hyd_coll%cursize
      d_hyd => domain_hyd_coll%hyd_pnts(1)

      ! copy projection attributes
      hyd%waqgeom%meshname = d_hyd%waqgeom%meshname
      hyd%waqgeom%dim =  d_hyd%waqgeom%dim
      hyd%waqgeom%start_index = d_hyd%waqgeom%start_index
      hyd%crs  = d_hyd%crs
      hyd%conv_type  = d_hyd%conv_type
      hyd%conv_version  = d_hyd%conv_version

      ! init totals
      hyd%nmax  = 1
      hyd%kmax  = d_hyd%kmax
      hyd%nolay = d_hyd%nolay
      hyd%geometry    = d_hyd%geometry
      hyd%layer_type  = d_hyd%layer_type
      hyd%sal_present = d_hyd%sal_present
      hyd%tem_present = d_hyd%tem_present
      hyd%tau_present = d_hyd%tau_present
      hyd%vdf_present = d_hyd%vdf_present
      hyd%description = ' '
      hyd%hyd_ref     = d_hyd%hyd_ref
      hyd%hyd_start   = d_hyd%hyd_start
      hyd%hyd_stop    = d_hyd%hyd_stop
      hyd%hyd_step    = d_hyd%hyd_step
      hyd%cnv_ref     = d_hyd%cnv_ref
      hyd%cnv_start   = d_hyd%cnv_start
      hyd%cnv_stop    = d_hyd%cnv_stop
      hyd%cnv_step    = d_hyd%cnv_step
      hyd%cnv_step_sec= d_hyd%cnv_step_sec

      hyd%openbndsect_coll%maxsize = 0
      hyd%openbndsect_coll%cursize = 0
      hyd%wasteload_coll%cursize = 0
      hyd%wasteload_coll%maxsize = 0
      hyd%dd_bound_coll%cursize = 0
      hyd%dd_bound_coll%maxsize = 0

      ! initialise
      nosegl = 0

      ! count the number of faces
      hyd%waqgeom%numface = 0
      do i_domain = 1, n_domain ! loop over domains
         idmn = i_domain - 1
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         call reallocP(d_hyd%global_edge, d_hyd%waqgeom%numedge, fill = 0)
         call reallocP(d_hyd%global_node, d_hyd%waqgeom%numnode, fill = 0)
         do iface = 1, d_hyd%waqgeom%numface  ! loop over faces
            if ( d_hyd%idomain(iface) .eq. idmn ) then
               ! not a ghost cell, so count as global cell
               nosegl = max(nosegl,d_hyd%iglobal(iface))
               hyd%waqgeom%numface = hyd%waqgeom%numface + 1
            end if
         end do
      end do

      ! backpointer for global face numbers to domain and local face number
      call realloc(globface_domain, hyd%waqgeom%numface, fill = 0)
      call realloc(globface_face, hyd%waqgeom%numface, fill = 0)
      do i_domain = 1, n_domain ! loop over domains
         idmn = i_domain - 1
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iface = 1, d_hyd%waqgeom%numface  ! loop over faces
            if ( d_hyd%idomain(iface) .eq. idmn ) then
               globface_domain(d_hyd%iglobal(iface)) = i_domain
               globface_face(d_hyd%iglobal(iface)) = iface
            end if
         end do
      end do

      ! determine maximun number of nodes per face
      hyd%waqgeom%maxnumfacenodes = 0
      do i_domain = 1, n_domain
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         hyd%waqgeom%maxnumfacenodes = max(hyd%waqgeom%maxnumfacenodes, d_hyd%waqgeom%maxnumfacenodes)
      end do

      ! determine number of edges used in the merged grid, asign global edge numbers and copy to higher domain
      hyd%waqgeom%numedge = 0
      do itype = 1, 4 ! loop over types
         edge_type = edge_type_orde(itype)
         do i_domain = 1, n_domain  ! loop over domains
            idmn = i_domain - 1
            d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iedge = 1, d_hyd%waqgeom%numedge  ! loop over edges
               if(d_hyd%edge_type(iedge) .eq. edge_type) then
                  ! edge of current type to be added to the merged domain
                  ip1 = d_hyd%waqgeom%edge_faces(1, iedge)
                  ip2 = d_hyd%waqgeom%edge_faces(2, iedge)
                  if(ip2 .gt. 0) then
                     if(min(d_hyd%idomain(ip1), d_hyd%idomain(ip2)) .eq. idmn) then
                        ! only add when the lowest domain on either side of the edge is in the current domain to avoid double inclusion
                        hyd%waqgeom%numedge = hyd%waqgeom%numedge + 1
                        d_hyd%global_edge(iedge) = hyd%waqgeom%numedge
                        if(max(d_hyd%idomain(ip1), d_hyd%idomain(ip2)) .gt. idmn) then
                           ! copy global edge number to other domain when domainnumber is higher
                           if(d_hyd%idomain(ip2).gt.idmn) then
                              global_face = d_hyd%iglobal(ip1)
                              l_global_face = d_hyd%iglobal(ip2)
                           else
                              global_face = d_hyd%iglobal(ip2)
                              l_global_face = d_hyd%iglobal(ip1)
                           end if
                           ! link to a higher ranked domain
                           l_hyd => domain_hyd_coll%hyd_pnts(globface_domain(l_global_face))
                           ! get the local face number
                           l_face = globface_face(l_global_face)
                           do l_iedge = 1, l_hyd%waqgeom%maxnumfacenodes  ! loop over nodes of linked face
                              if (l_hyd%waqgeom%face_links(l_iedge, l_face).gt.0) then
                                 if(l_hyd%iglobal(l_hyd%waqgeom%face_links(l_iedge, l_face)).eq.global_face) then
                                    ! shared edge was found
                                    l_hyd%global_edge(l_hyd%waqgeom%face_edges(l_iedge, l_face)) = hyd%waqgeom%numedge
                                 end if
                              end if
                           end do
                        end if
                     end if
                  else
                     if(d_hyd%idomain(ip1) .eq. idmn) then
                        ! or when there is only one face and we are in the current domain
                        hyd%waqgeom%numedge = hyd%waqgeom%numedge + 1
                        d_hyd%global_edge(iedge) = hyd%waqgeom%numedge
                     end if
                  end if
               end if
            end do
         end do
      end do

      ! determin number of nodes used in the merged grid and asign global node numbers
      hyd%waqgeom%numnode = 0
      do i_domain = 1, n_domain  ! loop over domains
         idmn = i_domain - 1
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iedge = 1, d_hyd%waqgeom%numedge  ! loop over edges
            ip1 = d_hyd%waqgeom%edge_faces(1, iedge)
            ip2 = d_hyd%waqgeom%edge_faces(2, iedge)
            if(ip2 .gt. 0) then
               ! two faces
               if(min(d_hyd%idomain(ip1), d_hyd%idomain(ip2)) .eq. idmn) then
                  ! only add when the lowest domain on either side of the edge is in the current domain to avoid double inclusion
                  inode1 = d_hyd%waqgeom%edge_nodes(1, iedge)
                  if(d_hyd%global_node(inode1).eq.0) then
                     ! add nodes that do not yet have a global number
                     hyd%waqgeom%numnode = hyd%waqgeom%numnode + 1
                     d_hyd%global_node(inode1) = hyd%waqgeom%numnode
                  end if
                  inode2 = d_hyd%waqgeom%edge_nodes(2, iedge)
                  if(d_hyd%global_node(inode2).eq.0) then
                     ! add nodes that do not yet have a global number
                     hyd%waqgeom%numnode = hyd%waqgeom%numnode + 1
                     d_hyd%global_node(inode2) = hyd%waqgeom%numnode
                  end if
                  if(max(d_hyd%idomain(ip1), d_hyd%idomain(ip2)) .gt. idmn) then
                     ! copy global node numbers to other domain
                     if(d_hyd%idomain(ip1).gt.idmn) then
                        face_link = ip1
                     else
                        face_link = ip2
                     end if
                     ! link to a higher ranked domain, copy global node numbers
                     d_x1 = d_hyd%waqgeom%nodex(inode1)
                     d_y1 = d_hyd%waqgeom%nodey(inode1)
                     d_x2 = d_hyd%waqgeom%nodex(inode2)
                     d_y2 = d_hyd%waqgeom%nodey(inode2)
                     l_hyd => domain_hyd_coll%hyd_pnts(globface_domain(d_hyd%iglobal(face_link)))
                     ! get the local face number
                     l_face = globface_face(d_hyd%iglobal(face_link))
                     do l_inode = 1, l_hyd%waqgeom%maxnumfacenodes  ! loop over nodes of linked face
                        l_node = l_hyd%waqgeom%face_nodes(l_inode,l_face)
                        if(l_node.gt.0) then
                           ! it is a node
                           if(l_hyd%global_node(l_node).eq.0) then
                              ! and it doesn't have a global number yet
                              l_x1 = l_hyd%waqgeom%nodex(l_node)
                              l_y1 = l_hyd%waqgeom%nodey(l_node)
                              if(comparereal(l_x1, d_x1) == 0 .and. comparereal(l_y1, d_y1) == 0) then
                                 ! it is the first node of the edge
                                 l_hyd%global_node(l_node) = d_hyd%global_node(inode1)
                              end if
                              if(comparereal(l_x1, d_x2) == 0 .and. comparereal(l_y1, d_y2) == 0) then
                                 ! it is the second node of the edge
                                 l_hyd%global_node(l_node) = d_hyd%global_node(inode2)
                              end if
                           end if
                        end if
                     end do
                  end if
               end if
            else
               if(d_hyd%idomain(ip1) .eq. idmn) then
                  ! or when there is only one face and we are in the current domain
                  ! add edges that are not not connected to another segment (external)
                  do ik = 1,2  ! loop over nodes of the edge
                     inode = d_hyd%waqgeom%edge_nodes(ik, iedge)
                     if(d_hyd%global_node(inode).eq.0) then
                        ! add nodes that do not yet have a global number
                        hyd%waqgeom%numnode = hyd%waqgeom%numnode + 1
                        d_hyd%global_node(inode) = hyd%waqgeom%numnode
                     end if
                  end do
               end if
            end if
         end do
      end do

      ! allocate all arrays needed for the waqgeom data

      ! nodes
      call reallocP(hyd%waqgeom%nodex     ,  hyd%waqgeom%numnode , fill = -999d0)                              !< x-coordinates of the mesh nodes
      call reallocP(hyd%waqgeom%nodey     ,  hyd%waqgeom%numnode , fill = -999d0)                              !< y-coordinates of the mesh nodes
      call reallocP(hyd%waqgeom%nodez     ,  hyd%waqgeom%numnode , fill = -999d0)                              !< z-coordinates of the mesh nodes

      ! edges
      call reallocP(hyd%waqgeom%edge_nodes, [2, hyd%waqgeom%numedge], fill = -999)                          !< Edge-to-node mapping array
      call reallocP(hyd%waqgeom%edge_faces, [2, hyd%waqgeom%numedge], fill = -999)                          !< Edge-to-face mapping array
      call reallocP(hyd%edge_type         ,  hyd%waqgeom%numedge , fill = -999)                                !< Edge type
      call reallocP(hyd%waqgeom%edgex     ,  hyd%waqgeom%numedge , fill = -999d0)                              !< x-coordinates of the mesh edges
      call reallocP(hyd%waqgeom%edgey     ,  hyd%waqgeom%numedge , fill = -999d0)                              !< y-coordinates of the mesh edges
      call reallocP(hyd%waqgeom%edgez     ,  hyd%waqgeom%numedge , fill = -999d0)                              !< z-coordinates of the mesh edges

      ! faces
      call reallocP(hyd%waqgeom%face_nodes, [hyd%waqgeom%maxnumfacenodes, hyd%waqgeom%numface], fill = -999) !< Face-to-node mapping array
      call reallocP(hyd%waqgeom%face_edges, [hyd%waqgeom%maxnumfacenodes, hyd%waqgeom%numface], fill = -999) !< Face-to-edge mapping array
      call reallocP(hyd%waqgeom%face_links, [hyd%waqgeom%maxnumfacenodes, hyd%waqgeom%numface], fill = -999) !< Face-to-face mapping array
      call reallocP(hyd%waqgeom%facex     ,  hyd%waqgeom%numface , fill = -999d0)                              !< x-coordinates of the mesh faces
      call reallocP(hyd%waqgeom%facey     ,  hyd%waqgeom%numface , fill = -999d0)                              !< y-coordinates of the mesh faces
      call reallocP(hyd%waqgeom%facez     ,  hyd%waqgeom%numface , fill = -999d0)                              !< z-coordinates of the mesh faces

      ! now fill all waqgeom arrays

      ! fill the nodes data
      do i_domain = 1, n_domain  ! loop over domains
         idmn = i_domain - 1
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do inode = 1, d_hyd%waqgeom%numnode
            global_node = d_hyd%global_node(inode)
            if(global_node.gt.0) then
               ! node coordinates
               hyd%waqgeom%nodex(global_node) = d_hyd%waqgeom%nodex(inode)
               hyd%waqgeom%nodey(global_node) = d_hyd%waqgeom%nodey(inode)
               hyd%waqgeom%nodez(global_node) = d_hyd%waqgeom%nodez(inode)
            end if
         end do
      end do

      ! fill the edges data
      do i_domain = 1, n_domain  ! loop over domains
         idmn = i_domain - 1
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iedge = 1, d_hyd%waqgeom%numedge
            global_edge = d_hyd%global_edge(iedge)
            if(global_edge.gt.0) then
               ! edge nodes
               hyd%waqgeom%edge_nodes(1, global_edge) = d_hyd%global_node(d_hyd%waqgeom%edge_nodes(1, iedge))
               hyd%waqgeom%edge_nodes(2, global_edge) = d_hyd%global_node(d_hyd%waqgeom%edge_nodes(2, iedge))
               ! edge faces
               hyd%waqgeom%edge_faces(1, global_edge) = d_hyd%iglobal(d_hyd%waqgeom%edge_faces(1, iedge))
               if(d_hyd%waqgeom%edge_faces(2, iedge).gt.0) then
                  hyd%waqgeom%edge_faces(2, global_edge) = d_hyd%iglobal(d_hyd%waqgeom%edge_faces(2, iedge))
               end if
               ! edge type
               hyd%edge_type(global_edge) = d_hyd%edge_type(iedge)
               ! edge coordinates
               hyd%waqgeom%edgex(global_edge) = d_hyd%waqgeom%edgex(iedge)
               hyd%waqgeom%edgey(global_edge) = d_hyd%waqgeom%edgey(iedge)
!               hyd%waqgeom%edgez(global_edge) = d_hyd%waqgeom%edgez(iedge)
            end if
         end do
      end do

      ! fill the faces data
      do i_domain = 1, n_domain  ! loop over domains
         idmn = i_domain - 1
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iface = 1, d_hyd%waqgeom%numface
            if(d_hyd%idomain(iface) .eq. idmn ) then
               global_face = d_hyd%iglobal(iface)
               do inode = 1, d_hyd%waqgeom%maxnumfacenodes
                  ! face nodes
                  if (d_hyd%waqgeom%face_nodes(inode, iface).gt.0.and.hyd%waqgeom%face_nodes(inode, global_face).lt.0) then
                     hyd%waqgeom%face_nodes(inode, global_face) = d_hyd%global_node(d_hyd%waqgeom%face_nodes(inode, iface))
                  end if
                  ! face edges
                  if (d_hyd%waqgeom%face_edges(inode, iface).gt.0.and.hyd%waqgeom%face_edges(inode, global_face).lt.0) then
                     hyd%waqgeom%face_edges(inode, global_face) = d_hyd%global_edge(d_hyd%waqgeom%face_edges(inode, iface))
                  end if
                  ! face links
                  if (d_hyd%waqgeom%face_links(inode, iface).gt.0.and.hyd%waqgeom%face_links(inode, global_face).lt.0) then
                     hyd%waqgeom%face_links(inode, global_face) = d_hyd%iglobal(d_hyd%waqgeom%face_links(inode, iface))
                  end if
                  ! face coordinates
                  hyd%waqgeom%facex(global_face) = d_hyd%waqgeom%facex(iface)
                  hyd%waqgeom%facey(global_face) = d_hyd%waqgeom%facey(iface)
!                  hyd%waqgeom%facez(global_face) = d_hyd%waqgeom%facez(iface)
               end do
            end if
         end do
      end do

      ! sequentially fill in segment numbers in the third dimension (when hyd nolay > 1)
      if (hyd%nolay.gt.1) then
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            call reallocP(d_hyd%idomain, d_hyd%nolay * d_hyd%nosegl, keepExisting = .true.)
            call reallocP(d_hyd%iglobal, d_hyd%nolay * d_hyd%nosegl, keepExisting = .true.)
            do iseg = 1, d_hyd%nosegl
               do ilay = 2, d_hyd%nolay
                  d_hyd%idomain(iseg + (ilay - 1) * d_hyd%nosegl) = d_hyd%idomain(iseg)
                  d_hyd%iglobal(iseg + (ilay - 1) * d_hyd%nosegl) = d_hyd%iglobal(iseg) + (ilay - 1) * nosegl
               end do
            end do
         end do
      end if

      ! set the dimensions of the overall domain
      hyd%nosegl = nosegl
      hyd%noseg  = nosegl * hyd%nolay
      hyd%mmax   = nosegl

      ! boundaries, add the active sections and boundaries to the collections
      do i_domain = 1, n_domain
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         call reallocP(d_hyd%ispoint_bnd, d_hyd%nobnd, fill = .false.)
         no_sect = d_hyd%openbndsect_coll%cursize
         do i_sect = 1 , no_sect
            openbndsect => d_hyd%openbndsect_coll%openbndsect_pnts(i_sect)
            no_bnd = openbndsect%openbndlin_coll%cursize
            do i_bnd = 1 , no_bnd
               openbndlin => openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)
               if (comparereal(openbndlin%x1, openbndlin%x2) == 0 .and. comparereal(openbndlin%x1, openbndlin%x2) == 0) then
                  do ilay = 1, d_hyd%nolay
                     d_hyd%ispoint_bnd(abs(openbndlin%ibnd)+(ilay-1)*d_hyd%nobndl) = .true.
                  end do
               end if
            end do
         end do
      end do

      ! exchanges
      ! gather the exchanges per layer, and in such a way that the internal exchanges come first, and overlap with the edge face table
      noq1  = 0
      nobnd = 0
      ! loop over layers
      do ilay = 1, hyd%nolay
         ! first round internal links only
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            ! determine min and max segment number of this layer in this domain
            min_seg = (ilay - 1) * d_hyd%nosegl + 1
            max_seg = ilay * d_hyd%nosegl
            do iq = 1, d_hyd%noq1
               ip1 = d_hyd%ipoint(1,iq)
               ip2 = d_hyd%ipoint(2,iq)
               if (ip1 .ge. min_seg .and. ip1 .le. max_seg .and. ip2 .gt. 0) then
                  ! ip1 is in this layer and ip2 not a boundary
                  if (min(d_hyd%idomain(ip1),d_hyd%idomain(ip2)) .eq. idmn) then
                     ! only add when the lowest domain on either side of the link is in the current domain to avoid double inclusion
                     noq1 = noq1 + 1
                     d_hyd%iglobal_link(iq) = noq1
                  end if
               end if
            end do
         end do
         ! second round the boundary links that are not point sources
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            ! determine min and max segment number of this layer in this domain
            min_seg = (ilay - 1) * d_hyd%nosegl + 1
            max_seg = ilay * d_hyd%nosegl
            do iq = 1, d_hyd%noq1
               ip1 = d_hyd%ipoint(1,iq)
               ip2 = d_hyd%ipoint(2,iq)
               if ( ip1 .lt. 0 .and. ip2 .ge. min_seg .and. ip2 .le. max_seg) then
                  ! ip1 is a boundary and ip2 is in this layer
                  if(d_hyd%idomain(ip2) .eq. idmn .and. (.not.d_hyd%ispoint_bnd(abs(ip1)))) then
                     ! only add boundary links from current domain that are not point sources
                     noq1 = noq1 + 1
                     d_hyd%iglobal_link(iq) = noq1
                     if (abs(ip1) .le. d_hyd%nobndl) then
                        nobnd = nobnd + 1
                        d_hyd%iglobal_bnd(-ip1) = -nobnd
                        call renum_bnd(d_hyd%openbndsect_coll,ip1,-nobnd)
                     end if
                  end if
               else if (ip1 .ge. min_seg .and. ip1 .le. max_seg .and. ip2 .lt. 0) then !ip1 between min +1 and max
                  ! ip2 is a boundary and ip1 is in this layer
                  if (d_hyd%idomain(ip1) .eq. idmn .and. (.not.d_hyd%ispoint_bnd(abs(ip1)))) then
                     ! only add boundary links from current domain that are not point sources
                     noq1 = noq1 + 1
                     d_hyd%iglobal_link(iq) = noq1
                     if (abs(ip2) .le. d_hyd%nobndl) then
                        nobnd = nobnd + 1
                        d_hyd%iglobal_bnd(-ip2) = -nobnd
                        call renum_bnd(d_hyd%openbndsect_coll,ip2,-nobnd)
                     end if
                  end if
               end if
            end do
         end do
      end do

      ! third round the point sources
      do i_domain = 1, n_domain
         idmn = i_domain - 1
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1, d_hyd%noq1
            ip1 = d_hyd%ipoint(1,iq)
            ip2 = d_hyd%ipoint(2,iq)
            if (ip1 .lt. 0 .and. ip2 .gt. 0) then
               ! ip1 is a boundary to a valid segment
               if (d_hyd%idomain(ip2) .eq. idmn .and. d_hyd%ispoint_bnd(abs(ip1))) then
                  ! only add when the segment is in the current domain and it is a point source
                  noq1 = noq1 + 1
                  d_hyd%iglobal_link(iq) = noq1
                  if (abs(ip1) .le. d_hyd%nobndl) then
                     nobnd = nobnd + 1
                     d_hyd%iglobal_bnd(-ip1) = -nobnd
                     call renum_bnd(d_hyd%openbndsect_coll,ip1,-nobnd)
                  end if
               end if
            else if (ip1 .gt. 0 .and. ip2 .lt. 0) then
               ! ip2 is a boundary to a valid segment
               if (d_hyd%idomain(ip1) .eq. idmn .and. d_hyd%ispoint_bnd(abs(ip2))) then
                  ! only add when the segment is in the current domain and it is a point source
                  noq1 = noq1 + 1
                  d_hyd%iglobal_link(iq) = noq1
                  if (abs(ip2) .le. d_hyd%nobndl) then
                     nobnd = nobnd + 1
                     d_hyd%iglobal_bnd(-ip2) = -nobnd
                     call renum_bnd(d_hyd%openbndsect_coll,ip2,-nobnd)
                  end if
               end if
            end if
         end do
      end do

      ! exchange totals
      hyd%noq1 = noq1
      hyd%noq2 = 0
      hyd%noq3 = hyd%nosegl*(hyd%nolay-1)
      hyd%noq4 = 0
      hyd%noq  = hyd%noq1 + hyd%noq2 + hyd%noq3 + hyd%noq4
      hyd%nobndl = nobnd
      hyd%nobnd  = nobnd*hyd%nolay

      ! fill in boundary numbers in the third dimension (when hyd nolay > 1)
      if (hyd%nolay.gt.1) then
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do i_bnd = 1, d_hyd%nobndl
               if ( d_hyd%iglobal_bnd(i_bnd).ne.0) then
                  do ilay = 2, d_hyd%nolay
                     d_hyd%iglobal_bnd(i_bnd + (ilay - 1) * d_hyd%nobndl) = d_hyd%iglobal_bnd(i_bnd) - (ilay - 1) * nobnd
                  end do
               end if
            end do
         end do
      end if

      ! make final pointer table
      nobnd  = 0
      nobndl = hyd%nobndl
      call reallocP(hyd%ipoint, [4, hyd%noq] , fill = 0)
      do i_domain = 1, n_domain
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1, d_hyd%noq1
            iq_global = d_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               ip1 = d_hyd%ipoint(1,iq)
               ip2 = d_hyd%ipoint(2,iq)
               if ( ip1 .gt. 0 ) then
                  ip1 = d_hyd%iglobal(ip1)
               elseif ( ip1 .lt. 0 ) then
                  ip1 = d_hyd%iglobal_bnd(-ip1)
               endif
               if ( ip2 .gt. 0 ) then
                  ip2 = d_hyd%iglobal(ip2)
               elseif ( ip2 .lt. 0 ) then
                  ip2 = d_hyd%iglobal_bnd(-ip2)
               endif
               hyd%ipoint(1,iq_global) = ip1
               hyd%ipoint(2,iq_global) = ip2
            endif
         enddo
      enddo

      ! pointers in third dimension
      do iseg = 1, hyd%nosegl
         do ilay = 1, hyd%nolay - 1
            iq_global = hyd%noq1 + (ilay-1)*hyd%nosegl + iseg
            ip1 = (ilay-1)*hyd%nosegl + iseg
            ip2 = (ilay  )*hyd%nosegl + iseg
            if ( ilay .ne. 1 ) then
               ip3 = (ilay-2)*hyd%nosegl + iseg
            else
               ip3 = 0
            end if
            if ( ilay .ne. hyd%nolay - 1 ) then
               ip4 = (ilay+1)*hyd%nosegl + iseg
            else
               ip4 = 0
            end if
            hyd%ipoint(1,iq_global) = ip1
            hyd%ipoint(2,iq_global) = ip2
            hyd%ipoint(3,iq_global) = ip3
            hyd%ipoint(4,iq_global) = ip4
         end do
      end do

      ! layering
      call reallocP(hyd%hyd_layers, hyd%kmax, fill = 0e0)
      call reallocP(hyd%waq_layers, hyd%nolay, fill = 0e0)
      hyd%hyd_layers = d_hyd%hyd_layers
      hyd%waq_layers = d_hyd%waq_layers

      ! boundaries, add the active sections and boundaries to the collections
      do i_domain = 1, n_domain
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         no_sect = d_hyd%openbndsect_coll%cursize
         do i_sect = 1 , no_sect
            openbndsect => d_hyd%openbndsect_coll%openbndsect_pnts(i_sect)
            no_bnd = openbndsect%openbndlin_coll%cursize
            bnd_active = .false.
            do i_bnd = 1 , no_bnd
               if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new .ne. 0 ) then
                  bnd_active = .true.
               end if
            end do
            if ( bnd_active ) then
               isect = openbndsect_coll_find( hyd%openbndsect_coll, openbndsect%name )
               if ( isect .le. 0 ) then
                  new_sect%name = openbndsect%name
                  new_sect%openbndlin_coll%cursize = 0
                  new_sect%openbndlin_coll%maxsize = 0
                  new_sect%openbndlin_coll%openbndlin_pnts => null()
                  isect = coll_add(hyd%openbndsect_coll, new_sect)
               end if
               do i_bnd = 1 , no_bnd
                  if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new .ne. 0 ) then
                     iret = coll_add(hyd%openbndsect_coll%openbndsect_pnts(isect)%openbndlin_coll,openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd))
                     hyd%openbndsect_coll%openbndsect_pnts(isect)%openbndlin_coll%openbndlin_pnts(iret)%ibnd = openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new
                  end if
               end do
            end if
         end do
      end do

      ! allocate rest of the arrays
      allocate(hyd%volume(hyd%noseg))
      allocate(hyd%area(hyd%noq))
      allocate(hyd%flow(hyd%noq))
      allocate(hyd%displen(2,hyd%noq))
      allocate(hyd%surf(hyd%noseg))
      allocate(hyd%depth(hyd%noseg))
      allocate(hyd%attributes(hyd%noseg))
      if (hyd%sal_present) allocate(hyd%sal(hyd%noseg))
      if (hyd%tem_present) allocate(hyd%tem(hyd%noseg))
      if (hyd%tau_present) allocate(hyd%tau(hyd%noseg))
      if (hyd%vdf_present) allocate(hyd%vdf(hyd%noseg))

      ! time independent items
      hyd%atr_type = ATR_FM
      hyd%no_atr = 2
      hyd%displen = 0.0
      do i_domain = 1 , n_domain
         d_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do isegl = 1 , d_hyd%nosegl
            iseg_global = d_hyd%iglobal(isegl)
            if ( iseg_global .gt. 0 ) then
               do ilay = 1, hyd%nolay
                  hyd%surf(iseg_global + (ilay - 1) * hyd%nosegl)       = d_hyd%surf(isegl + (ilay - 1) * d_hyd%nosegl)
                  hyd%attributes(iseg_global + (ilay - 1) * hyd%nosegl) = d_hyd%attributes(isegl + (ilay - 1) * d_hyd%nosegl)
               end do
               hyd%depth(iseg_global) = d_hyd%depth(isegl)
            end if
         end do
         do iq = 1, d_hyd%noq1
            iq_global = d_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               hyd%displen(1,iq_global) = d_hyd%displen(1,iq)
               hyd%displen(2,iq_global) = d_hyd%displen(2,iq)
            end if
         end do
      end do
      return
      end

      subroutine merge_domains_old(hyd, domain_hyd_coll)

      ! function : merge the domains, make pointers to final domain

      ! global declarations

      use hydmod
      use MessageHandling
      use m_alloc
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                    ! description of the hydrodynamics
      type(t_hyd_coll)                       :: domain_hyd_coll        ! description of all domain hydrodynamics

      ! local declarations

      integer                                :: n_domain               ! number of domains
      integer                                :: i_domain               ! index in collection
      integer                                :: idmn                   ! flow like domain index (0:n_domain-1)
      type(t_hyd), pointer                   :: domain_hyd             ! description of one domain hydrodynamics
      integer                                :: nosegl                 ! total number of segments per layer
      integer                                :: nobnd                  ! total number of boundaries
      integer                                :: nobndl                 ! total number of boundaries per layer
      integer                                :: nolay                  ! number of layers
      integer                                :: noq1                   ! total number of exchanges in first directory
      integer                                :: iseg                   ! segment index
      integer                                :: isegl                  ! segment index in layer
      integer                                :: iseg_glob              ! global segment index
      integer                                :: ilay                   ! layer index
      integer                                :: iq                     ! exchange index
      integer                                :: iq_global              ! global exchange index layer
      integer                                :: iq_glob                ! global exchange index overall
      integer                                :: ip1                    ! segment pointer index
      integer                                :: ip2                    ! segment pointer index
      integer                                :: ip3                    ! segment pointer index
      integer                                :: ip4                    ! segment pointer index
      integer                                :: numcontpts             ! numcontpts number of contour nodes
      integer                                :: no_sect                ! number of sections
      integer                                :: i_sect                 ! index of section
      integer                                :: isect                  ! index of section
      integer                                :: no_bnd                 ! number of boundaries in section
      integer                                :: i_bnd                  ! index of boundary
      type(t_openbndsect), pointer           :: openbndsect            ! single section
      type(t_openbndsect)                    :: new_sect               ! single section new
      logical                                :: bnd_active             ! if a boundary is active
      integer                                :: iret                   ! return value

      integer                                :: ik                     ! node counter
      integer                                :: il                     ! link counter
      integer                                :: nodeoffset             ! node offset
      integer                                :: nodelinkoffset         ! node link offset
      integer                                :: nv                     ! max node for element
      integer                                :: inv                    ! index countour node for element

      integer, allocatable                   :: iglobal_active(:)      ! does a global segment actually exist
      integer, allocatable                   :: iglobal_new(:)         ! what is the new iglobal for all old iglobal
      integer                                :: inew                   ! new number

      ! allocate local arrays
      n_domain = domain_hyd_coll%cursize

      ! copy projection attributes
      hyd%crs  = domain_hyd_coll%hyd_pnts(1)%crs

      ! init totals
      hyd%nmax  = 1
      hyd%kmax  = domain_hyd_coll%hyd_pnts(1)%kmax
      hyd%nolay = domain_hyd_coll%hyd_pnts(1)%nolay
      hyd%geometry    = domain_hyd_coll%hyd_pnts(1)%geometry
      hyd%layer_type  = domain_hyd_coll%hyd_pnts(1)%layer_type
      hyd%sal_present = domain_hyd_coll%hyd_pnts(1)%sal_present
      hyd%tem_present = domain_hyd_coll%hyd_pnts(1)%tem_present
      hyd%tau_present = domain_hyd_coll%hyd_pnts(1)%tau_present
      hyd%vdf_present = domain_hyd_coll%hyd_pnts(1)%vdf_present
      hyd%description = ' '
      hyd%hyd_ref     = domain_hyd_coll%hyd_pnts(1)%hyd_ref
      hyd%hyd_start   = domain_hyd_coll%hyd_pnts(1)%hyd_start
      hyd%hyd_stop    = domain_hyd_coll%hyd_pnts(1)%hyd_stop
      hyd%hyd_step    = domain_hyd_coll%hyd_pnts(1)%hyd_step
      hyd%cnv_ref     = domain_hyd_coll%hyd_pnts(1)%cnv_ref
      hyd%cnv_start   = domain_hyd_coll%hyd_pnts(1)%cnv_start
      hyd%cnv_stop    = domain_hyd_coll%hyd_pnts(1)%cnv_stop
      hyd%cnv_step    = domain_hyd_coll%hyd_pnts(1)%cnv_step
      hyd%cnv_step_sec= domain_hyd_coll%hyd_pnts(1)%cnv_step_sec

      hyd%openbndsect_coll%maxsize = 0
      hyd%openbndsect_coll%cursize = 0
      hyd%wasteload_coll%cursize = 0
      hyd%wasteload_coll%maxsize = 0
      hyd%dd_bound_coll%cursize = 0
      hyd%dd_bound_coll%maxsize = 0

      ! iglobal is only partially filled first look for highest number
      nosegl = 0
      hyd%numk = 0
      hyd%numl = 0
      hyd%nv = 0
      hyd%nump = 0
      do i_domain = 1, n_domain
         idmn = i_domain - 1
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iseg = 1, domain_hyd%nosegl
            if ( domain_hyd%idomain(iseg) .eq. idmn ) then
               nosegl = max(nosegl,domain_hyd%iglobal(iseg))
               hyd%nump = hyd%nump + 1
            end if
         end do
         hyd%numk = hyd%numk + domain_hyd%numk
         hyd%numl = hyd%numl + domain_hyd%numl
         hyd%nv   = max(hyd%nv, domain_hyd%nv)
      end do

      if (hyd%nump.lt.nosegl) then
         ! Apparently the highest iglobal is higher than the sum of the number of active cells of each domain.
         ! We have to skip inactive cells, create a renumber list, and update the global segment numbers in each domain.
         write (msgbuf, '(a)')  'Apparently the highest iglobal is higher than the sum of the number of active cells of each domain.'
         call msg_flush()
         write (msgbuf, '(a,i10,a,i10)') 'Highest iglobal:', nosegl, ', number of active cells:', hyd%nump
         call msg_flush()
         write (msgbuf, '(a)') 'We have to skip inactive cells, create a renumber list,'
         call msg_flush()
         write (msgbuf, '(a)') 'and update the global segment numbers in each domain.'
         call msg_flush()

         call realloc (iglobal_active, nosegl, fill=0)
         call realloc (iglobal_new, nosegl, fill=0)
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iseg = 1, domain_hyd%nosegl
               iglobal_active(domain_hyd%iglobal(iseg)) = 1
            end do
         end do
         inew = 0
         do iseg = 1, nosegl
            if (iglobal_active(iseg).eq.1) then
               inew = inew + 1
               iglobal_new(iseg) = inew
            end if
         end do
         if (inew.ne.hyd%nump) then
            write (msgbuf, '(a)') 'Unfortunatly the renumbering went wrong!'
            call msg_flush()
            write (msgbuf, '(a,i10,a,i10)') 'Highest new global number: ', inew, ', number of active cells:', hyd%nump
            call err_flush()
            write(*,*)
            stop
         end if
         write (msgbuf, '(a)')  'New numbering is fine!'
         call msg_flush()
         write (msgbuf, '(a,i10,a,i10)')  'Highest new global number: ', inew, ', number of active cells:', hyd%nump
         call msg_flush()
         nosegl = hyd%nump
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iseg = 1, domain_hyd%nosegl
               domain_hyd%iglobal(iseg) = iglobal_new(domain_hyd%iglobal(iseg))
            end do
         end do
      end if

      ! sequentially fill in segment numbers in the third dimension (when hyd nolay > 1)

      if (hyd%nolay.gt.1) then
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iseg = 1, domain_hyd%nosegl
               do ilay = 2, domain_hyd%nolay
                  domain_hyd%idomain(iseg + (ilay - 1) * domain_hyd%nosegl) = domain_hyd%idomain(iseg)
                  domain_hyd%iglobal(iseg + (ilay - 1) * domain_hyd%nosegl) = domain_hyd%iglobal(iseg) + (ilay - 1) * nosegl
               end do
            end do
         end do
      end if

      ! set the dimensions of the overall domain

      hyd%nosegl = nosegl
      hyd%noseg  = nosegl * hyd%nolay
      hyd%mmax   = nosegl

      ! global exchanges count, boundary count

      noq1  = 0
      nobnd = 0
      do i_domain = 1, n_domain
         idmn = i_domain - 1
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         domain_hyd%iglobal_link = 0
         do iq = 1, domain_hyd%noq1
            noq1 = noq1 + 1
            domain_hyd%iglobal_link(iq) = noq1
            ip1 = domain_hyd%ipoint(1,iq)
            ip2 = domain_hyd%ipoint(2,iq)
            if ( ip1 .lt. 0 ) then
               if (abs(ip1) .le. domain_hyd%nobndl .and. domain_hyd%idomain(ip2) .eq. idmn) then
                  nobnd = nobnd + 1
                  domain_hyd%iglobal_bnd(-ip1) = -nobnd
                  call renum_bnd(domain_hyd%openbndsect_coll,ip1,-nobnd)
               else if (domain_hyd%idomain(ip2) .ne. idmn) then
                  ! from cell is in ghost domain, revert addition of exchange
                  domain_hyd%iglobal_link(iq) = 0
                  noq1 = noq1 - 1
               end if
            else if ( ip2 .lt. 0 ) then
               if (abs(ip2) .le. domain_hyd%nobndl .and. domain_hyd%idomain(ip1) .eq. idmn) then
                  nobnd = nobnd + 1
                  domain_hyd%iglobal_bnd(-ip2) = -nobnd
                  call renum_bnd(domain_hyd%openbndsect_coll,ip2,-nobnd)
               else if (domain_hyd%idomain(ip1) .ne. idmn) then
                  ! from cell is in ghost domain, revert addition of exchange
                  domain_hyd%iglobal_link(iq) = 0
                  noq1 = noq1 - 1
               end if
            else if (min(domain_hyd%idomain(ip1),domain_hyd%idomain(ip2)) .ne. idmn) then
               ! one of the cells is in ghost domain with a lower domain number, revert addition of exchange to avoid double inclusion
               domain_hyd%iglobal_link(iq) = 0
               noq1 = noq1 - 1
            end if
         end do
      end do
      hyd%noq1 = noq1
      hyd%noq2 = 0
      hyd%noq3 = hyd%nosegl*(hyd%nolay-1)
      hyd%noq4 = 0
      hyd%noq  = hyd%noq1 + hyd%noq2 + hyd%noq3 + hyd%noq4
      hyd%nobndl = nobnd
      hyd%nobnd  = nobnd*hyd%nolay

      ! sequentially fill in boundary numbers in the third dimension (when hyd nolay > 1)

      if (hyd%nolay.gt.1) then
         do i_domain = 1, n_domain
            idmn = i_domain - 1
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do i_bnd = 1, domain_hyd%nobndl
               if ( domain_hyd%iglobal_bnd(i_bnd).ne.0) then
                  do ilay = 2, domain_hyd%nolay
                     domain_hyd%iglobal_bnd(i_bnd + (ilay - 1) * domain_hyd%nobndl) = domain_hyd%iglobal_bnd(i_bnd) - (ilay - 1) * nobnd
                  end do
               end if
            end do
         end do
      end if

      ! make final pointer table

      nobnd  = 0
      nobndl = hyd%nobndl
      allocate(hyd%ipoint(4,hyd%noq))
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1, domain_hyd%noq1
            iq_global = domain_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               ip1 = domain_hyd%ipoint(1,iq)
               ip2 = domain_hyd%ipoint(2,iq)
               ip3 = domain_hyd%ipoint(3,iq)
               ip4 = domain_hyd%ipoint(4,iq)
               if ( ip1 .gt. 0 ) then
                  ip1 = domain_hyd%iglobal(ip1)
               elseif ( ip1 .lt. 0 ) then
                  ip1 = domain_hyd%iglobal_bnd(-ip1)
               end if
               if ( ip2 .gt. 0 ) then
                  ip2 = domain_hyd%iglobal(ip2)
               elseif ( ip2 .lt. 0 ) then
                  ip2 = domain_hyd%iglobal_bnd(-ip2)
               end if
               if ( ip3 .gt. 0 ) then
                  ip3 = domain_hyd%iglobal(ip3)
               end if
               if ( ip4 .gt. 0 ) then
                  ip4 = domain_hyd%iglobal(ip4)
               end if
               hyd%ipoint(1,iq_global) = ip1
               hyd%ipoint(2,iq_global) = ip2
               hyd%ipoint(3,iq_global) = ip3
               hyd%ipoint(4,iq_global) = ip4
            end if
         end do
      end do

      ! pointers in third dimension
      do iseg = 1, hyd%nosegl
         do ilay = 1, hyd%nolay - 1
            iq_glob = hyd%noq1 + (ilay-1)*hyd%nosegl + iseg
            ip1 = (ilay-1)*hyd%nosegl + iseg
            ip2 = (ilay  )*hyd%nosegl + iseg
            if ( ilay .ne. 1 ) then
               ip3 = (ilay-2)*hyd%nosegl + iseg
            else
               ip3 = 0
            end if
            if ( ilay .ne. hyd%nolay - 1 ) then
               ip4 = (ilay+1)*hyd%nosegl + iseg
            else
               ip4 = 0
            end if
            hyd%ipoint(1,iq_glob) = ip1
            hyd%ipoint(2,iq_glob) = ip2
            hyd%ipoint(3,iq_glob) = ip3
            hyd%ipoint(4,iq_glob) = ip4
         end do
      end do

      ! all nodes, node links
      allocate (hyd%xk(hyd%numk))
      allocate (hyd%yk(hyd%numk))
      allocate (hyd%zk(hyd%numk))
      allocate (hyd%kn(2,hyd%numl))
      nodeoffset = 0
      nodelinkoffset = 0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do ik = 1, domain_hyd%numk
            hyd%xk(ik + nodeoffset) = domain_hyd%xk(ik)
            hyd%yk(ik + nodeoffset) = domain_hyd%yk(ik)
            hyd%zk(ik + nodeoffset) = domain_hyd%zk(ik)
         end do
         do il = 1, domain_hyd%numl
            hyd%kn(1,il + nodelinkoffset) = domain_hyd%kn(1, il) + nodeoffset
            hyd%kn(2,il + nodelinkoffset) = domain_hyd%kn(2, il) + nodeoffset
         end do
         nodeoffset = nodeoffset + domain_hyd%numk
         nodelinkoffset = nodelinkoffset + domain_hyd%numl
      end do

      ! coordinates segments
      hyd%numcontpts = 0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         hyd%numcontpts = max(hyd%numcontpts,domain_hyd%numcontpts)
      end do
      allocate(hyd%xdepth(1,hyd%nosegl))
      allocate(hyd%ydepth(1,hyd%nosegl))
      allocate(hyd%flowelemcontourx(hyd%numcontpts,hyd%nosegl))
      allocate(hyd%flowelemcontoury(hyd%numcontpts,hyd%nosegl))
      allocate(hyd%netcellnod(hyd%nv,hyd%nosegl))
      nodeoffset = 0
      hyd%xdepth           = 0.0
      hyd%ydepth           = 0.0
      hyd%flowelemcontourx = 0.0
      hyd%flowelemcontoury = 0.0
      hyd%netcellnod       = 0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         numcontpts = domain_hyd%numcontpts
         nv = domain_hyd%nv
         do iseg = 1, domain_hyd%nosegl
            iseg_glob = domain_hyd%iglobal(iseg)
            if ( iseg_glob .gt. 0 ) then
               hyd%xdepth(1,iseg_glob) = domain_hyd%xdepth(1,iseg)
               hyd%ydepth(1,iseg_glob) = domain_hyd%ydepth(1,iseg)
               hyd%flowelemcontourx(1:numcontpts,iseg_glob) = domain_hyd%flowelemcontourx(1:numcontpts,iseg)
               hyd%flowelemcontoury(1:numcontpts,iseg_glob) = domain_hyd%flowelemcontoury(1:numcontpts,iseg)
               do inv=1,nv
                  if ( domain_hyd%netcellnod(inv,iseg) .gt. 0) then
                     hyd%netcellnod(inv,iseg_glob) = domain_hyd%netcellnod(inv,iseg) + nodeoffset
                  else
                     hyd%netcellnod(inv,iseg_glob) = 0
                  end if
               end do
            end if
         end do
         nodeoffset = nodeoffset + domain_hyd%numk
      end do

      ! coordinates exchanges
      allocate(hyd%xu(noq1))
      allocate(hyd%yu(noq1))
      hyd%xu = 0.0
      hyd%yu = 0.0
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1, domain_hyd%lnx
            iq_global = domain_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               hyd%xu(iq_global) = domain_hyd%xu(iq)
               hyd%yu(iq_global) = domain_hyd%yu(iq)
            end if
         end do
      end do

      ! layering

      allocate(hyd%hyd_layers(hyd%kmax))
      allocate(hyd%waq_layers(hyd%nolay))
      hyd%hyd_layers = domain_hyd_coll%hyd_pnts(1)%hyd_layers
      hyd%waq_layers = domain_hyd_coll%hyd_pnts(1)%waq_layers

      ! boundaries, add the active sections and boundaries to the collections
      do i_domain = 1, n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         no_sect = domain_hyd%openbndsect_coll%cursize
         do i_sect = 1 , no_sect
            openbndsect => domain_hyd%openbndsect_coll%openbndsect_pnts(i_sect)
            no_bnd = openbndsect%openbndlin_coll%cursize
            bnd_active = .false.
            do i_bnd = 1 , no_bnd
               if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new .ne. 0 ) then
                  bnd_active = .true.
               end if
            end do
            if ( bnd_active ) then
               isect = openbndsect_coll_find( hyd%openbndsect_coll, openbndsect%name )
               if ( isect .le. 0 ) then
                  new_sect%name = openbndsect%name
                  new_sect%openbndlin_coll%cursize = 0
                  new_sect%openbndlin_coll%maxsize = 0
                  new_sect%openbndlin_coll%openbndlin_pnts => null()
                  isect = coll_add(hyd%openbndsect_coll, new_sect)
               end if
               do i_bnd = 1 , no_bnd
                  if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new .ne. 0 ) then
                     iret = coll_add(hyd%openbndsect_coll%openbndsect_pnts(isect)%openbndlin_coll,openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd))
                     hyd%openbndsect_coll%openbndsect_pnts(isect)%openbndlin_coll%openbndlin_pnts(iret)%ibnd = openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new
                  end if
               end do
            end if
         end do
      end do

      ! allocate rest of the arrays
      allocate(hyd%volume(hyd%noseg))
      allocate(hyd%area(hyd%noq))
      allocate(hyd%flow(hyd%noq))
      allocate(hyd%displen(2,hyd%noq))
      allocate(hyd%surf(hyd%noseg))
      allocate(hyd%depth(hyd%noseg))
      allocate(hyd%attributes(hyd%noseg))
      if (hyd%sal_present) allocate(hyd%sal(hyd%noseg))
      if (hyd%tem_present) allocate(hyd%tem(hyd%noseg))
      if (hyd%tau_present) allocate(hyd%tau(hyd%noseg))
      if (hyd%vdf_present) allocate(hyd%vdf(hyd%noseg))

      ! time independent items
      hyd%atr_type = ATR_FM
      hyd%no_atr = 2
      nolay     = hyd%nolay
      hyd%displen = 0.0

      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do isegl = 1 , domain_hyd%nosegl
            iseg_glob = domain_hyd%iglobal(isegl)
            if ( iseg_glob .gt. 0 ) then
               do ilay = 1,nolay
                  hyd%surf(iseg_glob + (ilay - 1) * hyd%nosegl) = domain_hyd%surf(isegl + (ilay - 1) * domain_hyd%nosegl)
                  hyd%attributes(iseg_glob + (ilay - 1) * hyd%nosegl) = domain_hyd%attributes(isegl + (ilay - 1) * domain_hyd%nosegl)
               end do
               hyd%depth(iseg_glob) = domain_hyd%depth(isegl)
            end if
         end do
         do iq = 1, domain_hyd%noq1
            iq_global = domain_hyd%iglobal_link(iq)
            if ( iq_global .gt. 0 ) then
               hyd%displen(1,iq_global) = domain_hyd%displen(1,iq)
               hyd%displen(2,iq_global) = domain_hyd%displen(2,iq)
            end if
         end do
      end do
      return
      end