module m_1d_networkreader
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------

   use MessageHandling
   use properties
   use m_hash_search
   use m_network
   
   implicit none

   private
   
   public read_1d_ugrid
   public construct_network_from_meshgeom

   contains
 
   !> Constructs a flow1d t_network datastructure, based on meshgeom read from a 1D UGRID file.
   !! meshgeom is only used for reading a UGRID NetCDF file, whereas network is used during
   !! a model computation.
   !! The network data structure assumes to have grid points on all start and end points of branches,
   !! but the input meshgeom often will only have one unique grid point on a connection node.
   !! In that case, parameter nodesOnBranchVertices allows to automatically create duplicate start/end points.
   integer function construct_network_from_meshgeom(network, meshgeom, branchids, branchlongnames, nodeids, nodelongnames, & !1d network character variables
      gpsID, gpsIDLongnames, network1dname, mesh1dname, nodesOnBranchVertices, jampi, my_rank) result(ierr)

   use gridgeom
   use meshdata
   use m_hash_search
   use precision_basics
   use m_alloc

   !in variables
   type(t_network),                             intent(inout) :: network
   type(t_ug_meshgeom),                         intent(in   ) :: meshgeom
   character(len=*), allocatable, dimension(:), intent(in   ) :: branchids
   character(len=*), allocatable, dimension(:), intent(in   ) :: branchlongnames
   character(len=*), allocatable, dimension(:), intent(in   ) :: nodeids
   character(len=*), allocatable, dimension(:), intent(in   ) :: nodelongnames
   character(len=*), allocatable, dimension(:), intent(inout) :: gpsID
   character(len=*), allocatable, dimension(:), intent(inout) :: gpsIDLongnames
   character(len=*),                            intent(in   ) :: network1dname
   character(len=*),                            intent(in   ) :: mesh1dname
   integer,                                     intent(in   ) :: nodesOnBranchVertices !< Whether or not (1/0) the input meshgeom itself already contains duplicate points on each connection node between multiple branches.
                                                                                       !! If not (0), additional grid points will be created.
   integer,                                     intent(in   ), optional :: jampi       !< running in parallel mode (1) or not (0)
   integer,                                     intent(in   ), optional :: my_rank     !< my rank in parallel mode, for (debugging) output

   !locals
   integer, allocatable, dimension(:)                   :: gpFirst
   integer, allocatable, dimension(:)                   :: gpLast
   integer, allocatable, dimension(:)                   :: lnkFirst
   integer, allocatable, dimension(:)                   :: lnkLast
   double precision, allocatable, dimension(:)          :: gpsX
   double precision, allocatable, dimension(:)          :: gpsY
   integer                                              :: i, ibran, jsferic
   integer                                              :: gridPointsCount, linkCount
   double precision, allocatable, dimension(:)          :: localOffsets, localUOffsets
   double precision, allocatable, dimension(:)          :: localOffsetsSorted
   integer, allocatable, dimension(:)                   :: localSortedIndexses
   double precision, allocatable, dimension(:)          :: localGpsX
   double precision, allocatable, dimension(:)          :: localGpsY
   character(len=ug_idsLen), allocatable, dimension(:)  :: localGpsID
   character(len=ug_idsLen), allocatable, dimension(:)  :: idMeshNodesInNetworkNodes
   integer                                              :: firstNode, lastNode, firstLink, LastLink
   double precision, parameter                          :: snapping_tolerance = 1d-10
   integer                                              :: jampi_, my_rank_
   integer                                              :: maxGridPointCount, maxLinkCount
   logical                                              :: startEndPointMissing(2)

   ierr = -1

   jampi_ = 0
   if (present(jampi)) jampi_ = jampi
   my_rank_ = -1
   if (present(my_rank)) my_rank_ = my_rank

   ! check data are present and correct
   if (meshgeom%numnode .eq. -1) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%numnode')
   endif
   if (meshgeom%nbranches .eq. -1) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nbranches')
   endif
   if (.not.associated(meshgeom%nodex)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nodex')
   endif
   if (.not.associated(meshgeom%nodey)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nodey')
   endif
   if (.not.allocated(nodeids)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in nodeids')
   endif
   if (.not.allocated(nodelongnames)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in nodelongnames')
   endif

   if (.not.allocated(branchids)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nbranchids')
   endif
   if (.not.associated(meshgeom%nedge_nodes)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nedge_nodes')
   endif
   if (.not.associated(meshgeom%nbranchorder)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nbranchorder')
   endif
   if (.not.associated(meshgeom%nodex)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nodex')
   endif
   if (.not.associated(meshgeom%nodey)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nodey')
   endif
   if (.not.associated(meshgeom%nodeoffsets)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%nodeoffsets')
   endif
   if (.not.allocated(gpsID)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in gpsID')
   endif
   if (.not.allocated(gpsIDLongnames)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in gpsIDLongnames')
   endif
   if (.not.associated(meshgeom%edgebranchidx)) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%edgebranchidx')
   endif
   if (meshgeom%edgebranchidx(1) < 0) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error in meshgeom%edgebranchidx: found missing values')
   endif

   ! Store Network Node Data ('connection nodes') into Data Structures.
   call storeNodes(network%nds, meshgeom%nnodes, meshgeom%nnodex, meshgeom%nnodey, nodeids, nodelongnames)

   ! Calculate mesh1d x,y coordinates (computational grid points), based on UGRID branchId-based notation.
   allocate(gpsX(meshgeom%numnode), stat = ierr)
   if (ierr == 0) allocate(gpsY(meshgeom%numnode), stat = ierr)
   if (ierr .ne. 0) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Allocating Memory for Grid Point Coordinates')
   endif
   
   if (meshgeom%epsg==4326) then ! TODO: UNST-2510: LC: %epsg is never set (unless via c_meshgeom?), so this code does not work for lat/lon 1D networks.
      jsferic = 1
   else
      jsferic = 0
   endif
   ierr = ggeo_get_xy_coordinates(meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%ngeopointx, meshgeom%ngeopointy, &
      meshgeom%nbranchgeometrynodes, meshgeom%nbranchlengths, jsferic, gpsX, gpsY)
   if (ierr /= 0) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Getting Mesh Coordinates From UGrid Data. Are all grid points ordered by branchId, chainage?')
   endif

   ! Get the starting and ending mesh1d grid point indexes for each network branch.
   allocate(gpFirst (meshgeom%nbranches), gpLast (meshgeom%nbranches), &
            lnkFirst(meshgeom%nbranches), lnkLast(meshgeom%nbranches), stat = ierr)
   if (ierr /= 0) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Allocating Memory for Branches')
   endif

   ierr = ggeo_get_start_end_nodes_of_branches(meshgeom%nodebranchidx, gpFirst, gpLast)
   if (ierr /= 0) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Getting first and last nodes of the network branches. Are all grid points ordered by branchId, chainage?')
   endif
   ierr = ggeo_get_start_end_nodes_of_branches(meshgeom%edgebranchidx, lnkFirst, lnkLast)
   if (ierr /= 0) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Getting first and last links of the network branches. Are all edges ordered by branchId, chainage?')
   endif

   ! Fill the array storing the mesh1d node ids for each network node.
   if (nodesOnBranchVertices==0) then
      allocate(idMeshNodesInNetworkNodes(meshgeom%nnodes))
      idMeshNodesInNetworkNodes = ' '
      do ibran = 1, meshgeom%nbranches
         firstNode = gpFirst(ibran)
         lastNode  = gpLast(ibran)
         if (firstNode > 0) then
            if (meshgeom%nodeoffsets(firstNode) < snapping_tolerance) then
               idMeshNodesInNetworkNodes(meshgeom%nedge_nodes(1,ibran)) = gpsID(firstNode)
            endif
         end if
         if (lastNode > 0) then
            if (comparereal(meshgeom%nodeoffsets(lastNode), meshgeom%nbranchlengths(ibran), snapping_tolerance) == 0) then
               idMeshNodesInNetworkNodes(meshgeom%nedge_nodes(2,ibran)) = gpsID(lastNode)
            endif
         endif
      enddo
   endif

   ! allocate local arrays
   maxGridPointCount = 3 + maxval(gpLast  - gpFirst)
   maxLinkCount      = 1 + maxval(lnkLast - lnkFirst)
   allocate(localOffsets(maxGridPointCount))
   allocate(localUOffsets(maxLinkCount))
   allocate(localOffsetsSorted(maxGridPointCount))
   allocate(localSortedIndexses(maxGridPointCount))
   allocate(localGpsX(maxGridPointCount))
   allocate(localGpsY(maxGridPointCount))
   allocate(localGpsID(maxGridPointCount))

   ! Store the branches + computational grid points on them into Data Structures.
   do ibran = 1, meshgeom%nbranches

      firstNode = gpFirst(ibran)
      lastNode  = gpLast(ibran)

      if (firstNode < 0 .or. lastNode < 0) then
         ! end node and begin node are missing and no internal gridpoints on branch.
         gridpointscount = 0
         ! localOffsets, localGpsX, localGpsY and localGpsID will be filled in add_point
      else
         gridPointsCount                 = lastNode - firstNode + 1
         localOffsets(1:gridPointsCount) = meshgeom%nodeoffsets(firstNode:lastNode)
         localGpsX(1:gridPointsCount)    = gpsX(firstNode:lastNode)
         localGpsY(1:gridPointsCount)    = gpsY(firstNode:lastNode)
         localGpsID(1:gridPointsCount)   = gpsID(firstNode:lastNode)
      endif

      firstLink = lnkFirst(ibran)
      lastLink  = lnkLast(ibran)
      if (firstLink < 0 .or. lastLink < 0) then
         linkCount = 0
      else
         linkCount                       = lastLink - firstLink + 1
         localUOffsets(1:linkCount)      = meshgeom%edgeoffsets(firstLink:lastLink)
      end if

      startEndPointMissing(1:2) = .false.
      if (nodesOnBranchVertices==0 .and. linkCount /= 0) then
         ! Optionally add the start and/or end point on a branch, this may even be necessary for MPI-parallel models.
         ! Reason: if the first and/or last u-point has no left, resp. right grid point (due to the gridpoint
         ! lying on a connected branch), then such gridpoint(s) *must* be added to this branch%grd administration.
         if (gridPointsCount > 0) then
            startEndPointMissing(1) = (localUOffsets(1)         < localOffsets(1))
            startEndPointMissing(2) = (localUOffsets(linkCount) > localOffsets(gridPointsCount))
         else
            startEndPointMissing(1:2) = .true. ! both are missing
         end if
         do i = 1, 2
            if (startEndPointMissing(i)) then
               call add_point(i == 1, localOffsets, localGpsX, localGpsY, localGpsID, idMeshNodesInNetworkNodes, gridPointsCount, ibran, meshgeom)
            endif
         enddo
      endif

      call storeBranch(network%brs, network%nds, branchids(ibran), nodeids(meshgeom%nedge_nodes(1,ibran)), &
         nodeids(meshgeom%nedge_nodes(2,ibran)), meshgeom%nbranchlengths(ibran), meshgeom%nbranchorder(ibran), gridPointsCount, localGpsX(1:gridPointsCount), &
         localGpsY(1:gridPointsCount),localOffsets(1:gridPointsCount), localUoffsets(1:linkCount), &
         localGpsID(1:gridPointsCount), my_rank_)

      ! Prepare mask array that defines which grd points were in input, and which not (used in admin_branch() later).
      call realloc(network%brs%branch(ibran)%grd_input, gridPointsCount, keepExisting=.false., fill=1)
      if (startEndPointMissing(1)) then
         network%brs%branch(ibran)%grd_input(1) = 0
      end if
      if (startEndPointMissing(2)) then
         network%brs%branch(ibran)%grd_input(gridPointsCount) = 0
      end if
   enddo

   call adminBranchOrders(network%brs)
   call fill_hashtable(network%brs)
   
   network%loaded = .true.

   end function construct_network_from_meshgeom

   !> helper function to add a point at the start or end of a branch
   subroutine add_point(atStart, localOffsets, localGpsX, localGpsY, localGpsID, idMeshNodesInNetworkNodes, gridPointsCount, ibran, meshgeom)
   use meshdata, only : t_ug_meshgeom
   use precision
   logical,                                         intent(in   ) :: atStart     !< add point at start (true) or end (false) of branch
   double precision,     allocatable, dimension(:), intent(inout) :: localGpsX
   double precision,     allocatable, dimension(:), intent(inout) :: localGpsY
   double precision,     allocatable, dimension(:), intent(inout) :: localOffsets
   character(len=*),     allocatable, dimension(:), intent(inout) :: localGpsID
   character(len=*),     allocatable, dimension(:), intent(in   ) :: idMeshNodesInNetworkNodes
   integer,                                         intent(inout) :: gridPointsCount
   integer,                                         intent(in   ) :: ibran
   type(t_ug_meshgeom),                             intent(in   ) :: meshgeom

   if (atStart) then
      localOffsets(1:gridPointsCount+1)=(/ 0.0d0, localOffsets(1:gridPointsCount) /)
      localGpsX(1:gridPointsCount+1)   =(/ meshgeom%nnodex(meshgeom%nedge_nodes(1,ibran)), localGpsX(1:gridPointsCount) /)
      localGpsY(1:gridPointsCount+1)   =(/ meshgeom%nnodey(meshgeom%nedge_nodes(1,ibran)), localGpsY(1:gridPointsCount) /)
      localGpsID(1:gridPointsCount+1)  =(/ idMeshNodesInNetworkNodes(meshgeom%nedge_nodes(1,ibran)), localGpsID(1:gridPointsCount) /)
   else
      localOffsets(gridPointsCount+1)= meshgeom%nbranchlengths(ibran)
      localGpsX(gridPointsCount+1)   = meshgeom%nnodex(meshgeom%nedge_nodes(2,ibran))
      localGpsY(gridPointsCount+1)   = meshgeom%nnodey(meshgeom%nedge_nodes(2,ibran))
      localGpsID(gridPointsCount+1)  = idMeshNodesInNetworkNodes(meshgeom%nedge_nodes(2,ibran))
   end if
   gridPointsCount = gridPointsCount + 1
   end subroutine add_point

   subroutine read_1d_ugrid(network, ioncid, dflowfm)

   use io_netcdf
   use io_ugrid
   use m_hash_search
   use gridgeom
   use meshdata

   implicit none

   type(t_network), target, intent(inout) :: network
   integer, intent(in)                    :: ioncid
   logical, optional, intent(inout)       :: dflowfm

   integer                   :: ierr
   integer                   :: numMesh
   integer                   :: meshIndex
   integer                   :: networkIndex
   integer                   :: startIndex
   
   character(len=ug_idsLen), allocatable, dimension(:)              :: branchids
   character(len=ug_idsLongNamesLen), allocatable, dimension(:)     :: branchlongnames
   character(len=ug_idsLen), allocatable, dimension(:)              :: nodeids
   character(len=ug_idsLongNamesLen), allocatable, dimension(:)     :: nodelongnames
   character(len=ug_idsLen), allocatable, dimension(:)              :: gpsID
   character(len=ug_idsLongNamesLen), allocatable, dimension(:)     :: gpsIDLongnames
   character(len=ug_idsLongNamesLen)                                :: network1dname
   character(len=ug_idsLongNamesLen)                                :: mesh1dname
   integer, parameter                                               :: nodesOnBranchVertices = 0


   !< Structure where all mesh is stored. Internal arrays are all pointers
   type(t_ug_meshgeom) :: meshgeom

   ! Make indexes 1-based
   startIndex = 1


   ierr = ionc_get_mesh_count(ioncid, numMesh)
   if (ierr .ne. 0) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Reading Number of Meshes')
   endif
   if (numMesh < 1) then
      call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Data Missing')
   endif

   ! Get Index of 1D-Mesh
   ierr = ionc_get_1d_mesh_id_ugrid(ioncid, meshIndex)
   if (ierr .ne. 0 .or. meshIndex <= 0) then
      if (present(dflowfm)) then
         call SetMessage(LEVEL_INFO, 'Network UGRID-File: No 1D-Mesh Present, Skipped 1D')
         network%loaded = .false.
         return
      else
         call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Reading Mesh ID')
      endif
   endif

   ! Get Index of 1D-Network
   ierr = ionc_get_1d_network_id_ugrid(ioncid, networkIndex)
   if (ierr .ne. 0 .or. networkIndex <= 0) then
      if (present(dflowfm)) then
         call SetMessage(LEVEL_INFO, 'Network UGRID-File: No 1D-Network Present, Skipped 1D')
         network%loaded = .false.
         return
      else
         call SetMessage(LEVEL_FATAL, 'Network UGRID-File: Error Reading Mesh ID')
      endif
   endif

   ! Get all data in one call: mesh and network
   ierr =  ionc_get_meshgeom(ioncid, meshIndex, networkIndex, meshgeom, startIndex, .true., &
      branchids, branchlongnames, nodeids, nodelongnames, & !1d network character variables
      gpsID, gpsIDLongnames, network1dname, mesh1dname)     !1d grid character variables

   ! Fill the flow1d m_network::network data structure based on the meshgeom from file.
   ierr = construct_network_from_meshgeom(network, meshgeom, branchids, branchlongnames, nodeids, nodelongnames, & 
      gpsID, gpsIDLongnames, network1dname, mesh1dname, nodesOnBranchVertices)

   
   !deallocate memory
   ierr = t_ug_meshgeom_destructor(meshgeom)
   deallocate(branchids)
   deallocate(branchlongnames)
   deallocate(nodeids)
   deallocate(nodelongnames)
   deallocate(gpsID)
   deallocate(gpsIDLongnames)
   
   end subroutine read_1d_ugrid

   
   subroutine adminBranchOrders(brs)
      type (t_branchset), intent(inout) :: brs
      
      integer i, ibr, j
      type(t_branch), pointer :: pbr, pbr2
      
      do ibr = 1, brs%count
         pbr => brs%branch(ibr)
         if (pbr%orderNumber > 0) then
            do j = 1, 2
               if (pbr%nextBranch(j) < 0) then
                  ! find neighbouring branch
                  do i = ibr+1, brs%count
                     pbr2 => brs%branch(i)
                     if (pbr%ordernumber == pbr2%ordernumber) then
                        if (pbr%nodeIndex(j) == pbr2%nodeIndex(1)) then
                           ! found one
                           pbr%nextBranch(j) = i
                           pbr2%nextBranch(1)= ibr
                           ! finished
                           cycle
                        elseif (pbr%nodeIndex(j) == pbr2%nodeIndex(2)) then
                           ! found one
                           pbr%nextBranch(j) = i
                           pbr2%nextBranch(2)= ibr
                           ! finished
                           cycle
                        endif
                     endif
                  enddo
               endif
            enddo
         endif
      enddo
      
   end subroutine adminBranchOrders

   subroutine readNode(nds, md_ptr)
   
      implicit none
   
      type(t_nodeSet), target, intent(inout) :: nds
      type(tree_data), pointer, intent(in)   :: md_ptr    

      character(len=IdLen)                   :: nodeId
      character(len=IdLen)                   :: nodeName
      double precision                       :: x
      double precision                       :: y
      logical                                :: success
      
      call prop_get(md_ptr, 'node', 'id', nodeId, success)
      if (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Node ID')
      endif
      
      call prop_get(md_ptr, 'node', 'name', nodeName, success)

      call prop_get(md_ptr, 'node', 'x', x, success)
      if (success) call prop_get(md_ptr, 'node', 'y', y, success)
      
      if (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Node '''//trim(nodeId)//'''')
      endif

      nds%Count = nds%Count+1
      if (nds%Count > nds%Size) then
         call realloc(nds)
      endif
      
      nds%node(nds%Count)%id       = nodeId
      nds%node(nds%Count)%name     = nodeName
      nds%node(nds%Count)%index    = nds%count
      nds%node(nds%Count)%nodetype = nt_NotSet
      nds%node(nds%Count)%numberOfConnections = 0
      nds%node(nds%Count)%x = x
      nds%node(nds%Count)%y = y
      
   end subroutine readNode
   
   subroutine storeNodes(nds, nNodes, nodesX, nodesY, nodeids, nodelongnames)
   
      implicit none
   
      type(t_nodeSet), target, intent(inout)             :: nds
      integer, intent(in)                                :: nNodes
      double precision, dimension(nNodes), intent(in)    :: nodesX 
      double precision, dimension(nNodes), intent(in)    :: nodesY
      character(len=*), dimension(nNodes), intent(in)    :: nodeids
      character(len=*), dimension(nNodes) , intent(in)   :: nodelongnames
      
      integer                                :: iNode
      
      
      do iNode = 1, nNodes
      
         nds%Count = nds%Count + 1
         if (nds%Count > nds%Size) then
            call realloc(nds)
         endif
      
         nds%node(nds%Count)%id                  = nodeids(iNode)
         nds%node(nds%Count)%name                = nodelongnames(iNode)
         nds%node(nds%Count)%index               = nds%count
         nds%node(nds%Count)%nodetype            = nt_NotSet
         nds%node(nds%Count)%numberOfConnections = 0
         nds%node(nds%Count)%x                   = nodesX(iNode)
         nds%node(nds%Count)%y                   = nodesY(iNode)
         
      enddo
      
      call fill_hashtable(nds)
      
   end subroutine storeNodes
   
   subroutine readBranch(brs, nds, md_ptr)
   
      use m_branch
      
      implicit none

      type(t_branchSet), target, intent(inout) :: brs
      type(t_nodeSet), target, intent(inout)   :: nds
      type(tree_data), pointer, intent(in)     :: md_ptr

      ! Local Variables
      integer                                  :: ibr
      type(t_branch), pointer                  :: pbr
      type(t_node), pointer                    :: node
      logical                                  :: success
      integer                                  :: istat
      integer                                  :: ibegNode
      integer                                  :: iendNode
      integer                                  :: orderNumber
      integer                                  :: gridPointsCount
      integer                                  :: uPointsCount
      integer                                  :: igr
      integer                                  :: gridIndex
      integer                                  :: j
      integer                                  :: ip1
      integer                                  :: ip2
      character(len=IdLen)                     :: branchId
      character(len=IdLen)                     :: begNodeId
      character(len=IdLen)                     :: endNodeId
      character(len=IdLen)                     :: Chainage
      
      double precision, allocatable, dimension(:)     :: gpX
      double precision, allocatable, dimension(:)     :: gpY
      double precision, allocatable, dimension(:)     :: gpchainages
      character(len=IdLen), allocatable, dimension(:) :: gpID
      
      brs%Count = brs%Count + 1
      ibr = brs%Count
      if (brs%Count > brs%Size) then
         call realloc(brs)
      endif
      
      pbr =>brs%branch(brs%Count)
      
      call prop_get(md_ptr, 'branch', 'id', branchId, success)
      if (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Branch ID')
      endif

      call prop_get(md_ptr, 'branch', 'fromnode', begNodeId, success)
      if (success) call prop_get(md_ptr, 'branch', 'tonode', endNodeId, success)
      if (success) call prop_get(md_ptr, 'branch', 'order', ordernumber, success)
      if (success) call prop_get(md_ptr, 'branch', 'gridpointscount', gridPointsCount, success)

      if (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Branch '''//trim(branchId)//'''')
      endif
      
      ibegNode = hashsearch(nds%hashlist, begNodeId)
      if (ibegNode <= 0) then
         write(msgbuf, '(4a)') trim(branchId), ': fromNode ''', trim(begNodeId), ''' does not exist'
         call SetMessage(LEVEL_FATAL, msgbuf)
      endif
      
      iendNode = hashsearch(nds%hashlist, endNodeId)
      if (iendNode <= 0) then
         write(msgbuf ,'(4a)') trim(branchId), ': toNode ''', trim(endNodeId), ''' does not exist'
         call SetMessage(LEVEL_FATAL, msgbuf)
      endif

      if (ibegNode == iendNode) then
         write(msgbuf, '(5a)') trim(branchId), ': fromNode ''', trim(begNodeId) , ''' is identical to toNode ''', trim(endNodeId)//''''
         call SetMessage(LEVEL_FATAL, msgbuf)
      endif
      
      pbr%id                           = branchID
      pbr%index                        = ibr
      pbr%FromNode                     => nds%node(ibegNode)
      pbr%FromNode%numberOfConnections = pbr%FromNode%numberOfConnections + 1
      pbr%ToNode                       => nds%node(iendNode)
      pbr%ToNode%numberOfConnections   = pbr%toNode%numberOfConnections + 1
      pbr%orderNumber                  = orderNumber
      pbr%nextBranch                   = -1
      pbr%nodeIndex(1)                 = ibegNode
      pbr%nodeIndex(2)                 = iendNode
      ! The Gridpoints
      call realloc(gpX, gridPointsCount, stat=istat)
      if (istat == 0) call realloc(gpY, gridPointsCount, stat=istat)
      if (istat == 0) call realloc(gpchainages, gridPointsCount, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading Branch: Error allocating Gridpoint Arrays')
      endif
      
      call prop_get(md_ptr, 'branch', 'gridPointX', gpX, gridPointsCount, success)
      if (success) call prop_get(md_ptr, 'branch', 'gridPointY', gpY, gridPointsCount, success)
      if (success) call prop_get(md_ptr, 'branch', 'gridPointOffsets', gpchainages, gridPointsCount, success)
      if (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Grid Data from Branch '''//trim(branchId)//'''')
      endif
      
      ! Check chainages of Grid Points
      do igr = 1, gridPointsCount - 1
         if (gpchainages(igr) + minSectionLength > gpchainages(igr + 1)) then
            ! Two adjacent gridpoints too close
            write (msgbuf, '(a, a, a, g11.4, a, g11.4)' ) 'Two grid points on branch ''', trim(branchid), ''' are too close at chainage ',               &
                                    gpchainages(igr), ' and ', gpchainages(igr + 1)
            call SetMessage(LEVEL_WARN, msgbuf)
         endif 
      enddo

      pbr%gridPointsCount = gridPointsCount
      uPointsCount        = pbr%gridPointsCount - 1
      pbr%uPointsCount    = uPointsCount
      
      call realloc(pbr%gridPointschainages, pbr%gridPointsCount)
      call realloc(pbr%uPointschainages, pbr%uPointsCount)
      call realloc(pbr%Xs, pbr%gridPointsCount)
      call realloc(pbr%Ys, pbr%gridPointsCount)

      ip1 = brs%gridPointsCount + 1
      brs%gridPointsCount = brs%gridPointsCount + gridPointsCount
      ip2 = brs%gridPointsCount
      pbr%StartPoint        = ip1
      pbr%gridPointschainages = gpchainages
      pbr%uPointschainages    = (pbr%gridPointschainages(1:uPointsCount) + pbr%gridPointschainages(2:uPointsCount+1) ) / 2.0d0
      pbr%length            = gpchainages(gridPointsCount)
      pbr%Xs                = gpX
      pbr%Ys                = gpY
      pbr%fromNode%x        = gpX(1)
      pbr%fromNode%y        = gpY(1)
      pbr%toNode%x          = gpX(gridPointsCount)
      pbr%toNode%y          = gpY(gridPointsCount)

      do j = 1, 2
         if (j==1) then
            node => pbr%fromNode
            gridIndex = ip1
         else
            node => pbr%toNode
            gridIndex = ip2
         endif
         if (node%nodeType == nt_NotSet) then
            ! probably end node (until proved otherwise
            node%nodeType = nt_endNode
         node%gridNumber = gridIndex ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
         elseif (node%nodeType == nt_endNode) then
            ! Already one branch connected, so not an endNode
            node%nodeType = nt_LinkNode
            node%gridNumber = 0
         endif
         if (node%numberOfConnections > nds%maxNumberOfConnections) then
            nds%maxNumberOfConnections = node%numberOfConnections
         endif
      enddo
      
      ! Get and Set grid point IDs
      call realloc(gpID, gridPointsCount, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading Branch: Error allocating Gridpoint Array for IDs')
      endif
      gpID = ' '

      call prop_get(md_ptr, 'branch', 'gridPointIds', gridPointsCount, gpID, success)
      call realloc(pbr%gridPointIDs, gridPointsCount)

      if (success) then
         pbr%gridPointIDs = gpID
      else
         ! Create grid point IDs
         pbr%gridPointIDs = ' '
         do igr = 1, gridPointsCount
            write(Chainage, '(F35.3)') pbr%gridPointschainages(igr)
            pbr%gridPointIDs(igr) = trim(pbr%id)//'_'//trim(adjustl(Chainage))
         enddo
         
      endif
      
      ! Clear Grid Point Arrays
      if (allocated(gpX)) deallocate(gpX, stat=istat)
      if (istat == 0 .and. allocated(gpY)) deallocate(gpY, stat=istat)
      if (istat == 0 .and. allocated(gpchainages)) deallocate(gpchainages, stat=istat)
      if (istat == 0 .and. allocated(gpID)) deallocate(gpID, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_FATAL, 'Reading Branch: Error Deallocating Gridpoint Arrays')
      endif
      
   end subroutine readBranch

   subroutine storeBranch(brs, nds, branchId, begNodeId, endNodeId, branchlength, ordernumber, gridPointsCount, gpX, gpY, &
                          gpchainages, gpUchainages, gpID, my_rank)
   
      use m_branch
      use messagehandling
      use precision_basics, only: comparereal
      
      implicit none

      type(t_branchSet), target, intent(inout)       :: brs
      type(t_nodeSet), target, intent(inout)         :: nds
      character(len=*), intent(in)                   :: branchId
      character(len=*), intent(in)                   :: begNodeId
      character(len=*), intent(in)                   :: endNodeId
      double precision, intent(in)                   :: branchlength !< Actual length of this branch
      integer, intent(in)                            :: orderNumber

      integer, intent(in)                            :: gridPointsCount
      double precision, dimension(:), intent(in)     :: gpX
      double precision, dimension(:), intent(in)     :: gpY
      double precision, dimension(:), intent(in)     :: gpchainages
      double precision, dimension(:), intent(in)     :: gpUchainages
      character(len=*), dimension(:), intent(in)     :: gpID
      integer                       , intent(in)     :: my_rank

      ! Local Variables
      integer                                  :: ibr
      type(t_branch), pointer                  :: pbr
      type(t_node), pointer                    :: node
      integer                                  :: ibegNode
      integer                                  :: iendNode
      integer                                  :: uPointsCount
      integer                                  :: igr
      integer                                  :: gridIndex
      integer                                  :: j
      integer                                  :: ip1
      integer                                  :: ip2
      integer                                  :: igpFrom
      integer                                  :: igpTo
      character(len=IdLen)                     :: Chainage
      character(len=4)                         :: cnum
      
      brs%Count = brs%Count + 1
      ibr = brs%Count
      if (brs%Count > brs%Size) then
         call realloc(brs)
      endif
      
      pbr =>brs%branch(brs%Count)
      
      ibegNode = hashsearch(nds%hashlist, begNodeId)
      if (ibegNode <= 0) then
         write(msgbuf, '(4a)') trim(branchId), ': fromNode ''', trim(begNodeId), ''' does not exist'
         call SetMessage(LEVEL_FATAL, msgbuf)
      endif
      
      iendNode = hashsearch(nds%hashlist, endNodeId)
      if (iendNode <= 0) then
         write(msgbuf ,'(4a)') trim(branchId), ': toNode ''', trim(endNodeId), ''' does not exist'
         call SetMessage(LEVEL_FATAL, msgbuf)
      endif

      if (ibegNode == iendNode) then
         write(msgbuf, '(5a)') trim(branchId), ': fromNode ''', trim(begNodeId) , ''' is identical to toNode ''', trim(endNodeId)//''''
         call SetMessage(LEVEL_FATAL, msgbuf)
      endif
      
      pbr%id                           = branchID
      pbr%index                        = ibr
      pbr%FromNode                     => nds%node(ibegNode)
      pbr%FromNode%numberOfConnections = pbr%FromNode%numberOfConnections + 1
      pbr%ToNode                       => nds%node(iendNode)
      pbr%ToNode%numberOfConnections   = pbr%toNode%numberOfConnections + 1
      pbr%orderNumber                  = orderNumber
      pbr%nextBranch                   = -1
      pbr%nodeIndex(1)                 = ibegNode
      pbr%nodeIndex(2)                 = iendNode

      ! The Gridpoints

      ! Check chainages of Grid Points
      do igr = 1, gridPointsCount - 1
         if (gpchainages(igr) + minSectionLength > gpchainages(igr + 1)) then
            ! Two adjacent gridpoints too close
            write (msgbuf, '(3a, g11.4, a, g11.4)' ) 'Two grid points on branch ''', trim(branchid), ''' are too close at chainage ',               &
                                    gpchainages(igr), ' and ', gpchainages(igr + 1)
            if (my_rank >= 0) then
               write(cnum, '(i4)') my_rank
               msgbuf = 'Rank = ' // cnum // ': ' // msgbuf
            end if
            call SetMessage(LEVEL_WARN, msgbuf)
         endif 
      enddo

      pbr%gridPointsCount = gridPointsCount
      uPointsCount        = size(gpUchainages)
      pbr%uPointsCount    = uPointsCount

      call realloc(pbr%gridPointschainages, pbr%gridPointsCount)
      call realloc(pbr%uPointschainages, pbr%uPointsCount)
      call realloc(pbr%Xs, pbr%gridPointsCount)
      call realloc(pbr%Ys, pbr%gridPointsCount)

      ip1 = brs%gridPointsCount + 1
      brs%gridPointsCount = brs%gridPointsCount + gridPointsCount
      ip2 = brs%gridPointsCount
      pbr%StartPoint         = ip1
      pbr%gridPointschainages = gpchainages
      pbr%uPointschainages    = gpUchainages(1:uPointsCount)
      pbr%length            = branchlength ! NOTE: in parallel models, this is not necessarily equal to gpchainages(gridPointsCount)
      pbr%Xs                = gpX
      pbr%Ys                = gpY

      igpFrom = -1
      igpTo   = -1
      if (gridPointsCount > 0) then
         if (comparereal(gpchainages(1), 0d0) == 0) then
            pbr%fromNode%x        = gpX(1)
            pbr%fromNode%y        = gpY(1)
            igpFrom               = ip1
         end if
         if (comparereal(gpchainages(gridPointsCount), branchlength) == 0) then
            pbr%toNode%x          = gpX(gridPointsCount)
            pbr%toNode%y          = gpY(gridPointsCount)
            igpTo                 = ip2
         elseif (comparereal(gpchainages(gridPointsCount), branchlength) > 0) then
            ! Only check for points beyond the branch length, in parallel mode a missing end node is possible.
            msgbuf = 'The chainage of the last gridpoint on branch '''// trim(pbr%Id)// ''' is larger than the edge length of this branch'
            call err_flush()
         end if
      end if

      do j = 1, 2
         if (j==1) then
            node => pbr%fromNode
            gridIndex = igpFrom
         else
            node => pbr%toNode
            gridIndex = igpTo
         endif
         if (node%nodeType == nt_NotSet) then
            ! probably end node (until proved otherwise)
            node%nodeType = nt_endNode
            node%gridNumber = gridIndex
         elseif (node%nodeType == nt_endNode) then
            ! Already one branch connected, so not an endNode
            node%nodeType = nt_LinkNode
            node%gridNumber = 0
         endif
         if (node%numberOfConnections > nds%maxNumberOfConnections) then
            nds%maxNumberOfConnections = node%numberOfConnections
         endif
      enddo
      
      ! Set grid point IDs
      call realloc(pbr%gridPointIDs, gridPointsCount)

      do igr = 1, gridPointsCount
      
         if (gpID(igr) .eq. ' ') then
            write(Chainage, '(F35.3)') pbr%gridPointschainages(igr)
            pbr%gridPointIDs(igr) = trim(pbr%id)//'_'//trim(adjustl(Chainage))
         else
            pbr%gridPointIDs(igr) = gpID(igr)
         endif
         
      enddo
      
   end subroutine storeBranch
 
end module m_1d_networkreader
