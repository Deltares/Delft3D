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

module m_oned_functions
   use m_missing, only: dmiss
   implicit none
   private

   public set_1d_roughnesses
   public set_1d_indices_in_network
   public save_1d_nrd_vars_in_stm
   public setbobs_1d
   public gridpoint2cross
   public computePump_all_links
   public convert_cross_to_prof
   public set_ground_level_for_1d_nodes
   public set_max_volume_for_1d_nodes
   public updateFreeboard
   public updateTimeWetOnGround
   public updateDepthOnGround
   public updateVolOnGround
   public updateTotalInflow1d2d
   public updateTotalInflowLat
   public updateS1Gradient
   public interpolateRoughnessParameters
   public reCalculateConveyanceTables
   public shiftTimeDependentRoughnessValues
   
   type, public :: t_gridp2cs
      integer :: num_cross_sections
      integer, allocatable, dimension(:) :: cross
   end type

   type(t_gridp2cs), allocatable, dimension(:) :: gridpoint2cross

   contains

   !> IFRCUTP and FRCu are filled, using 1D roughness values from Network structure 
   subroutine set_1d_roughnesses()
      use m_flowgeom
      use m_flow, only: frcu, ifrcutp, frcu_mor
      use unstruc_channel_flow

      implicit none

      ! FRCU and FRCU_MOR should only be used after SETAU - VOL12D. 
      ! Therefore initialise these arrays with a negative value.
      if (network%loaded) then
         where (kcu(1:lnx1d) == 1)
            frcu(1:lnx1d)     = dmiss
            ifrcutp(1:lnx1d)  = 0
            frcu_mor(1:lnx1d) = dmiss
         end where
      endif

   end subroutine set_1d_roughnesses

   !> Sets the flowgeom link and node numbers of the computational grid 
   !! into the 1D network structure for branches, storage nodes, 
   !! cross sections and structures, etc. 
   subroutine set_1d_indices_in_network()
      use timers
      use m_sediment
      use m_flowgeom
      use m_flow
      use m_cross_helper
      use m_flowparameters
      use unstruc_channel_flow
      
      
      implicit none
      integer handle_tot
      integer handle

      handle_tot = 0
      call timstrt('Set 1d indices', handle_tot)
      default_width = wu1DUNI
      
      if (network%loaded) then
         ! nonlinear computation is required for 1d flow
         if (nonlin1D == 0) then
            nonLin1D = 1
         elseif (nonlin1D >= 2) then
            CSCalculationOption = CS_TYPE_PLUS
         endif
         
         nonlin = max(nonlin, nonlin1D)
      endif
      
      if (.not. network%initialized) then

         handle = 0
         call timstrt('Set linknumbers in branches', handle)
         call set_linknumbers_in_branches()
         call timstop(handle)

         handle = 0
         call timstrt('Set node numbers for storage nodes', handle)
         call set_node_numbers_for_storage_nodes()
         call timstop(handle)

         handle = 0
         call timstrt('Set structure grid numbers', handle)
         call set_structure_grid_numbers()
         call timstop(handle)
      
         if (jased > 0 .and. stm_included) then
            ! 
            handle = 0
            call timstrt('Set cross sections to grid points', handle)
            call set_cross_sections_to_gridpoints()
            call timstop(handle)
         endif
         handle = 0
         
         network%initialized = .true.         
      endif
      call timstop(handle_tot)
      
   end subroutine set_1d_indices_in_network

   !> set the flowgeom linknumbers and node numbers in the branches
   !! (This replaces the netlink/netnode numbers that were originally
   !! filled in during the network reading stage.)
   subroutine set_linknumbers_in_branches()
      use unstruc_channel_flow
      use m_flowgeom
      use m_sediment
      use messageHandling
      use precision_basics, only: comparereal
      use m_GlobalParameters, only: flow1d_eps10

      implicit none

      integer :: L
      integer                                 :: ibr
      integer :: inod
      integer :: nbr
      integer :: i
      integer :: k1, k2
      integer :: is, ip1, ip2
      type(t_branch), pointer                 :: pbr
      integer, dimension(:), pointer          :: lin
      integer, dimension(:), pointer          :: grd

      ! First reset the gridnumber of all network nodes.
      ! These nodes are exactly the set into which the branches's fromNode/toNode are pointing.
      do inod = 1,network%nds%count
         network%nds%node(inod)%gridNumber = -1
      end do

      nbr = network%brs%count
      do ibr = 1, nbr
         pbr => network%brs%branch(ibr)

         ! Set flow node numbers via all flow link numbers on current branch.
         ! Grid points sequences with singleton grid points find the flow node using original input grid point index.
         if (pbr%gridPointsCount > 0) then
            call realloc(pbr%grd, pbr%gridPointsCount)
         else 
            cycle
         end if
         if (pbr%uPointsCount > 0) then
            call realloc(pbr%lin, pbr%uPointsCount)
         end if
         lin => pbr%lin
         grd => pbr%grd
         do is=1,pbr%gridpointsseqcount
            ip1 = pbr%k1gridpointsseq(is)
            ip2 = pbr%k2gridpointsseq(is)
            do i=ip1,ip2-1 ! upoints loop
               L = lin(i-(is-1)) ! is-1 corrects for any "holes" in between the previous is-1 gridpointssequences.
               k1 = abs(ln(1,L))
               grd(i) = k1
            enddo
            if (ip2 > ip1) then
               ! For gridpointsequence with 2 or more gridpoints, make sure to add the last one.
               L = ip2-1-(is-1)
               k2 = ln(2,iabs(lin(L)))
               grd(ip2) = k2
            else ! ip1 == ip2: singleton/orphan grid point
               k2 = ndx2d + pbr%grd_input(ip2)
               if (nd(k2)%nod(1) /= pbr%grd_input(ip2)) then
                  write(msgbuf, '(a,i0,a,i0,a,i0,a)') 'set_link_numbers_in_branches(): 1D grd_input(',ip2,')=', pbr%grd_input(ip2), &
                     ' does not match flow nd(',k2, ')%nod(1)=', nd(k2)%nod(1)
                  call err_flush()
               else
                  grd(ip2) = k2
               end if
            end if
         end do
         k1  =  grd(1)
         if (pbr%FromNode%gridNumber == -1 .and. comparereal(pbr%gridPointsChainages(1), 0d0, flow1d_eps10) == 0) then
            pbr%FromNode%gridNumber = k1 ! Only when exactly at the branch start (need not be so in parallel models).
         end if
         k2  =  grd(pbr%gridPointsCount)
         if (pbr%toNode%gridnumber == -1 .and. comparereal(pbr%gridPointsChainages(pbr%gridPointsCount), pbr%length, flow1d_eps10) == 0)  then
            pbr%toNode%gridnumber = k2 ! Only when exactly at the branch end (need not be so in parallel models).
         end if
      enddo
   end subroutine set_linknumbers_in_branches

   !> Set the node numbers from flowgeom for the storage nodes
   subroutine set_node_numbers_for_storage_nodes()
   
      use unstruc_channel_flow
      use m_flowgeom
      use m_sediment
      use messageHandling
      use m_GlobalParameters, only: INDTP_ALL
      use m_partitioninfo, only: jampi
      use m_inquire_flowgeom

      implicit none

      integer :: i
      type(t_storage), pointer                :: pstor
      integer, allocatable                    :: ixy2stor(:), k_tmp(:)
      double precision, allocatable           :: x_tmp(:), y_tmp(:)
      character(len=IdLen), allocatable       :: name_tmp(:)
      integer                                 :: nxy, countxy, jakdtree, ierr

      
      countxy = network%storS%Count_xy
      if (countxy > 0) then
         call realloc(ixy2stor,    countxy, keepExisting=.false.)
         call realloc(k_tmp,       countxy, keepExisting=.false.)
         call realloc(x_tmp,       countxy, keepExisting=.false.)
         call realloc(y_tmp,       countxy, keepExisting=.false.)
         call realloc(name_tmp,    countxy, keepExisting=.false.)
      end if
      
      nxy = 0
      do i = 1, network%storS%count
         pstor => network%storS%stor(i)
         if (pstor%node_index > 0) then
            pStor%grid_point = network%nds%node(pstor%node_index)%gridNumber
         else if (pstor%branch_index > 0) then
            ierr = findnode(pStor%branch_index, pstor%chainage, pstor%grid_point)
         else
            nxy = nxy + 1
            ixy2stor(nxy) = i
            x_tmp(nxy)    = pstor%x
            y_tmp(nxy)    = pstor%y
            name_tmp(nxy) = pstor%id
         end if
      end do
      
      if (nxy > 0) then ! find flow nodes for storage nodes that are defined by x-, y-coordinates
         jakdtree = 1
         call find_flownode(nxy, x_tmp(1:nxy), y_tmp(1:nxy), name_tmp(1:nxy), k_tmp(1:nxy), jakdtree, 0, INDTP_1D)
         do i = 1, nxy
            if (k_tmp(i) > 0) then
               pstor => network%storS%stor(ixy2stor(i))
               pstor%grid_point = k_tmp(i)
            else if (jampi == 0) then
               call SetMessage(LEVEL_ERROR, 'Error when snapping storage node '''//trim(name_tmp(i))//''' to a flow node. Are coordinates correct?')
            end if
         end do
      
         if (allocated(k_tmp))    deallocate(k_tmp)
         if (allocated(x_tmp))    deallocate(x_tmp)
         if (allocated(y_tmp))    deallocate(y_tmp)
         if (allocated(ixy2stor)) deallocate(ixy2stor)
         if (allocated(name_tmp)) deallocate(name_tmp)
      end if
      
      
   end subroutine set_node_numbers_for_storage_nodes
   
   subroutine set_structure_grid_numbers()
      use unstruc_channel_flow
      use m_flowgeom
      use m_flowexternalforcings
      use m_inquire_flowgeom

      implicit none
    
      integer :: nstru

      nstru = network%sts%count
      if (nstru>0) then
         call realloc(L1strucsg, nstru)
         call realloc(L2strucsg, nstru)
      endif
      
      
   end subroutine set_structure_grid_numbers
   
   !> For sediment transport on each node a cross section is required
   !! Fills gridpoint2cross with for each gridpoint a cross section index. \n
   !! Note: On connection nodes we have multiple cross sections (one for each 
   !!       incoming or outgoing branch (link).) \n
   !!       A connection node is located at the beginning or end of the branch.
   subroutine set_cross_sections_to_gridpoints()
   
      use unstruc_channel_flow
      use m_flowgeom
      use m_sediment
      use messageHandling

      implicit none

      integer :: L
      integer :: ibr
      integer :: nbr, pointscount
      integer :: i, j, jpos, linkcount
      integer :: k1, igrid
      integer :: c1, c2
      double precision :: d1, d2, dh
      type(t_branch), pointer                 :: pbr
      integer, dimension(:), pointer          :: lin
      integer, dimension(:), pointer          :: grd
      double precision, dimension(:), pointer :: chainage
      type(t_chainage2cross), dimension(:,:), pointer :: line2cross

      ! cross sections (in case of sediment transport every gridpoint requires a unique
      ! cross section)
      line2cross => network%adm%line2cross
      if (jased > 0 .and. stm_included) then
         if (allocated(gridpoint2cross)) deallocate(gridpoint2cross)
         allocate(gridpoint2cross(ndxi))
         do i = 1, ndxi
            gridpoint2cross(i)%num_cross_sections = 0
         enddo

         ! allocate space for local cross section numbers on connection nodes (multiple cross sections)
         do i = 1, network%nds%count
            k1 = network%nds%node(i)%gridNumber
            if (k1 <= 0) then
                ! TODO: Only partially prepared for parallel models (check gridpointsseq as introduced in UNST-5013)
               cycle
            end if
            
            linkcount = nd(k1)%lnx
            if (allocated(gridpoint2cross(k1)%cross)) deallocate(gridpoint2cross(k1)%cross)
            allocate(gridpoint2cross(k1)%cross(linkcount))
            gridpoint2cross(k1)%num_cross_sections = linkcount
            gridpoint2cross(k1)%cross = -999
         enddo
         
         igrid = 0
         nbr = network%brs%count
         do ibr = 1, nbr
            pbr => network%brs%branch(ibr)
            lin => pbr%lin
            grd => pbr%grd
            chainage => pbr%gridPointschainages
            pointscount = pbr%gridPointsCount
            do i = 1, pointscount
               igrid = igrid+1
               k1 = grd(i)
               if (i==1 .or. i==pointscount) then
                  ! search for correct location
                  ! this entry (gridpoint2cross(k1)) is already allocated
                  if (i==1) then 
                     L = lin(1)
                     dh = (chainage(i+1)-chainage(i))/2d0
                  else
                     L = lin(i-1)
                     dh = (chainage(i)-chainage(i-1))/2d0
                  endif
                  do j = 1,nd(k1)%lnx
                     if (L == iabs(nd(k1)%ln(j))) then
                        jpos = j
                     endif
                  enddo
               else
                  ! Internal gridpoint on branch, only 1 cross section attached
                  L = lin(i-1)
                  if (allocated(gridpoint2cross(k1)%cross)) deallocate(gridpoint2cross(k1)%cross)
                  allocate(gridpoint2cross(k1)%cross(1))
                  gridpoint2cross(k1)%num_cross_sections = 1
                  jpos = 1
                  dh = min(chainage(i)-chainage(i-1),chainage(i+1)-chainage(i))/2d0
                  
               endif
               if (i == 1) then
                  ! See assignment of L in previous part. First point L points to the outgoing flow link
                  c1 = line2cross(L,1)%c1
                  c2 = line2cross(L,1)%c2
               else
                  ! See assignment of L in previous part. All points but the first L points to the incoming flow link
                  c1 = line2cross(L,3)%c1
                  c2 = line2cross(L,3)%c2
               endif
               d1 = abs(network%crs%cross(c1)%chainage - chainage(i))
               d2 = abs(network%crs%cross(c2)%chainage - chainage(i))
               ! cross1%branchid and cross2%branchid should correspond to ibr
               if (d1 < dh) then
                  gridpoint2cross(k1)%cross(jpos) = c1
               elseif (d2 < dh) then
                  gridpoint2cross(k1)%cross(jpos) = c2
               else
                  gridpoint2cross(k1)%cross(jpos) = -999
               endif
            enddo
         enddo
      endif
   end subroutine set_cross_sections_to_gridpoints
      
   ! function to store variables related to the nodal relation variables
   subroutine save_1d_nrd_vars_in_stm
      use m_branch
      use m_node
      use m_sediment, only: stmpar
      use unstruc_channel_flow
      use morphology_data_module, only : t_nodefraction, t_noderelation
      use string_module

      implicit none

      integer :: ibr, iFrac, iNodeRel
      type(t_branch), pointer :: pbr
      type(t_nodefraction), pointer          :: pFrac
      type(t_noderelation),pointer           :: pNodRel

      if (network%loaded) then
          do iFrac = 1, stmpar%nrd%nFractions
              pFrac => stmpar%nrd%nodefractions(iFrac)
              do iNodeRel = 1, pFrac%nNodeRelations
                  pNodRel => pFrac%noderelations(iNodeRel)
                  do ibr = 1, network%brs%Count
                      pbr => network%brs%branch(ibr)
                      if (pNodRel%node == pbr%fromNode%id) then
                          pNodRel%nodeIdx = pbr%fromNode%gridnumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
                      endif
                      if (pNodRel%node == pbr%toNode%id) then
                          pNodRel%nodeIdx = pbr%toNode%gridnumber
                      endif
                      if (pNodRel%BranchIn == pbr%id) then
                          if (pNodRel%node == pbr%fromNode%id) then
                              pNodRel%BranchInLn = pbr%lin(1)                    ! (negative = at start of branch)
                          elseif (pNodRel%node == pbr%toNode%id) then
                              pNodRel%BranchInLn = pbr%lin(pbr%uPointsCount)     ! (positive = at end of branch)
                          endif
                      endif
                      if (pNodRel%BranchOut1 == pbr%id) then
                          if (pNodRel%node == pbr%fromNode%id) then
                              pNodRel%BranchOut1Ln = pbr%lin(1)                  ! (negative = at start of branch)
                          elseif (pNodRel%node == pbr%toNode%id) then
                              pNodRel%BranchOut1Ln = pbr%lin(pbr%uPointsCount)   ! (positive = at end of branch)
                          endif
                      endif
                      if (pNodRel%BranchOut2 == pbr%id) then
                          if (pNodRel%node == pbr%fromNode%id) then
                              pNodRel%BranchOut2Ln = pbr%lin(1)                  ! (negative = at start of branch)
                          elseif (pNodRel%node == pbr%toNode%id) then
                              pNodRel%BranchOut2Ln = pbr%lin(pbr%uPointsCount)   ! (positive = at end of branch)
                          endif
                      endif
                  enddo
              enddo
          enddo
      endif

   end subroutine save_1d_nrd_vars_in_stm

   subroutine setbobs_1d()
   
   use m_network
   use m_flowgeom
   use m_flowtimes
   use messagehandling
   use unstruc_messages
   use unstruc_channel_flow
   use m_1d_structures
   use m_cross_helper
   use network_data
   
   implicit none
   
   integer :: i
   integer :: L, L0
   integer :: n1
   integer :: n2
   integer :: nstor
   integer :: nstruc
   double precision :: crest_level
   type(t_structure), pointer :: pstruc
   type(t_storage),   pointer :: pstor
   
   do i = ndx2D+1, ndxi
      bl(i) = huge(1d0)
   enddo
   
   
   nstor = network%storS%count
   do i = 1, nstor
      pstor => network%storS%stor(i)
      n1 = pstor%grid_point
      if (n1 > 0) then
      bl(n1) = min(bl(n1), pstor%storage_area%x(1))
      end if
   enddo
      
   do L = 1, lnx1D
      if (kcu(L) ==1) then
         bob(:,L)  = getbobs(network, L)
         bob0(:,L) = bob(:,L)
         n1  = ln(1,L)
         n2 = ln(2,L)                    ! flow ref
         bl(n1) = min(bl(n1), bob(1,L))
         bl(n2) = min(bl(n2), bob(2,L))
      endif
   enddo

   ! In case of compound structures bob is set to the lowest crest level
   ! Pumping stations get the bob of the channel
   ! First step is to initialise all structure bobs to huge
   nstruc = network%sts%count
   do i = 1, nstruc
      pstruc => network%sts%struct(i)
      do L0 = 1, pstruc%numlinks
         L = abs(pstruc%linknumbers(L0))
         bob(:,L) = huge(1d0)
      enddo
   enddo
   
   nstruc = network%sts%count
   do i = 1, nstruc
      pstruc => network%sts%struct(i)
      crest_level = get_crest_level(pstruc)
      do L0 = 1, pstruc%numlinks
         L = abs(pstruc%linknumbers(L0))
         if (crest_level < huge(1d0)) then
            bob(1,L) = min(bob(1,L), crest_level)
            bob(2,L) = min(bob(2,L), crest_level)
         else
            ! pumping station
            bob(1,L) = min(bob(1,L), bob0(1,L))
            bob(2,L) = min(bob(2,L), bob0(2,L))
         endif
      enddo
   enddo
   
   if (time_user<= tstart_user) then
      ! check if all manholes are lower than or equal to the invert level of all incoming pipes
      nstor = network%storS%count
      do i = 1, nstor
         pstor => network%storS%stor(i)
         n1 = pstor%grid_point
         if (n1 <= 0) cycle
         if (bl(n1) < pstor%storage_area%x(1)) then
            call setmessage(LEVEL_WARN, 'At node '//trim(network%nds%node(i)%id)//' the bedlevel is below the bedlevel of the assigned storage area.')
            write(msgbuf, '(''The bedlevel (due to invert levels of incoming channels/pipes) = '', g14.2, '' and the bottom level of the storage area is '', g14.2)') &
                        bl(n1), pstor%storage_area%x(1)
            call setmessage(-LEVEL_WARN, msgbuf)
         
         endif
      
      enddo
   endif
   

   do i = ndx2D+1, ndxi
      if (bl(i) > 0.5d0*huge(1d0)) then
         write(msgbuf, '(a,i0,a)') 'Bedlevel is missing on calculation flow node ', i, '. No nearby cross sections nor storage nodes.'
         call warn_flush()
         bl(i) = zkuni
      end if
   enddo

   ! look for missing bobs
   do L = 1, lnx1d
      if (bob(1,L) > 0.5d0*huge(1d0)) then
         bob(1,L)  = bl(ln(1,L))
         bob0(1,L) = bob(1,L)
      endif
      if (bob(2,L) > 0.5d0*huge(1d0)) then
         bob(2,L)  = bl(ln(2,L))
         bob0(2,L) = bob(2,L)
      endif
   enddo
   
   do L = lnxi+1, lnx1Db
       ! mirror 1d bed level points at boundary 
       n1 = ln(1,L)
       n2 = ln(2,L)
       bl(n1) = bl(n2)
       bob(1,L)  = bl(n1)
       bob(2,L)  = bl(n2)       
       bob0(1,L) = bl(n1)
       bob0(2,L) = bl(n2)       
   enddo    
   
   end subroutine setbobs_1d

   !> Compute FU and RU coefficients for each flow link that is part of 
   !! the pump. Values are stored in struct%fu(:), etc. and *also* set
   !! in m_flow::fu(:), etc.
   subroutine computePump_all_links(struct)
      use m_1d_structures
      use m_pump
      use m_flowtimes
      use m_flowgeom 
      use m_flow
      use messageHandling

      type(t_structure), intent(inout) :: struct !< The parent structure of the pump (which also contains the flow link information).

      double precision     :: s1k1
      double precision     :: s1k2
      double precision     :: qp
      double precision     :: ap
      double precision     :: vp1, vp2, vp
      integer              :: L   
      integer              :: L0   
      integer              :: k1   
      integer              :: k2
      integer              :: dir
      
      if (struct%numlinks == 0) then
         ! Possibly outside of this partition
         return
      end if

      ! First compute average waterlevels on suction side and delivery side of the pump
      s1k1 = 0d0
      s1k2 = 0d0
      ap = 0d0
      vp1 = 0d0
      vp2 = 0d0
      vp = 0d0
      qp = 0d0
      do L0 = 1, struct%numlinks
         L = struct%linknumbers(L0)
         ! Note: Link L may have negative sign if flow link is opposite pump's orientation
         ! (pump spatial orientation is polyline+righthand rule, or network branch direction).
         ! Note 2: do not account for pumping direction here, that is done in prepareComputePump.
         dir = sign(1, L) ! only includes flow link w.r.t. structure spatial orientation.
         L = iabs(L)
         if ( dir > 0) then
            k1 = ln(1,L)
            k2 = ln(2,L)
         else
            k1 = ln(2,L)
            k2 = ln(1,L)
         endif
         
         if (hs(k1) > 1d-2) then
            ! NOTE: pump area-weighting across links is uniform for all links (au=1).
            au(L) = 1d0
            hu(L) = 1d0 ! UNST-5835: restored original hu(L) = 1d0, originally set in furu(). Currently furu() resets it to 0d0 already while treating "old" structures.
            ap    = ap + au(L)
            vp1    = vp1 + vol1(k1)
            vp2    = vp2 + vol1(k2)
            s1k1 = s1k1 + au(L)*s1(k1)
            s1k2 = s1k2 + au(L)*s1(k2)
         endif
      enddo
      
      ! With these average waterlevels, evaluate the pump discharge.
      if (ap > 0d0) then
         s1k1 = s1k1/ap
         s1k2 = s1k2/ap
         call PrepareComputePump(struct%pump, s1k1, s1k2)
         qp    = struct%pump%discharge ! Already in our local structure spatial orientation.
         
         ! Choose available volume on suction side.
         if (qp > 0d0) then
            vp = vp1
         else
            vp = vp2
         end if
      else
         struct%pump%ss_trigger = .false.
         struct%pump%ds_trigger = .false.
      endif          

      ! Finally, redistribute the requested pump discharge across all flow links.
      if (qp == 0d0 .or. ap == 0 .or. vp == 0d0) then
         ! Pump is off
         struct%fu = 0d0
         struct%ru = 0d0
         struct%au = 0d0
      else

         ! Limit the pump discharge in case the volume in the cells at the suction side is limited.
          if (abs(qp) > 0.9d0*vp/dts) then
            qp = sign(0.9d0*vp/dts, qp)
            call setmessage(LEVEL_WARN, 'Discharge through pump ' &
                & //trim(struct%id)//' is limited below capacity '//&
                & 'by water volume on suction side.')
         endif

         do L0  = 1, struct%numlinks
            L = struct%linknumbers(L0)
            dir = int(sign(1d0, L*qp)) ! Includes both pumping direction and flow link w.r.t. structure spatial orientation.
            L = iabs(L)
            if ( dir > 0) then         
               k1 = ln(1,L)
            else
               k1 = ln(2,L)
            endif
         
            if (hs(k1) > 1d-2) then
               struct%fu(L0) =  0d0
               struct%ru(L0) =  qp/ap
               struct%au(L0) =  ap
            else 
               struct%fu(L0) = 0d0
               struct%ru(L0) = 0d0
               struct%au(L0) = 0d0
            endif
         enddo
      endif
      
      do L0  = 1, struct%numlinks
         L = iabs(struct%linknumbers(L0))
         fu(L) = struct%fu(L0)
         ru(L) = struct%ru(L0)
         au(L) = struct%au(L0)
      enddo

   end subroutine computePump_all_links


   !> Converts the currently active cross sections in network%crs to
   !! old-format profloc/profdef input.
   subroutine convert_cross_to_prof(basename)
   use unstruc_channel_flow
   use m_inquire_flowgeom
   use m_samples
   use m_polygon
   use m_missing
   use m_flowgeom
   
   character(len=*), intent(in) :: basename !< Basename for the profdef/loc output files.

   integer :: ic, it, nprof, LF, ierr, mloc, mdef, mxyz, nyz, numxyztype
   integer, allocatable :: cs2prof(:)
   type(t_CrossSection), pointer :: pcrs
   type(t_CSType), pointer :: pcs
   character(len=255) :: proflocfile, profdeffile, profdefxyzfile

   call savesam()
   call savepol()

   proflocfile = trim(basename)//'_profloc.xyz'
   profdeffile = trim(basename)//'_profdef.txt'
   profdefxyzfile = trim(basename)//'_profdefxyz.pliz'

   !defs%CS(pcross%iTabDef)
   call newfil(mdef, profdeffile)
   call newfil(mxyz, profdefxyzfile)

   allocate(cs2prof(network%CSDefinitions%Count))

   nprof = 0
   npl = 0
   numxyztype = 0
   call realloc(nampli, network%CSDefinitions%Count, fill=' ')
   do it=1,network%CSDefinitions%Count
      pcs => network%CSDefinitions%CS(it)
      select case (pcs%crossType)
      case(CS_YZ_PROF)
         nprof = nprof+1
         cs2prof(it) = nprof

         numxyztype = numxyztype+1
         ! First write definition one-liner
         write (mdef, '(a,i0,a)') 'PROFNR=', nprof, '     TYPE=201' ! FRCTP=1 FRCCF=.035

         ! Then write xyz definition pliz
         write (nampli(numxyztype), '(a,i0)') 'PROFNR=', nprof
         nyz = pcs%levelsCount
         xpl(npl+1:npl+nyz) = 0d0
         ypl(npl+1:npl+nyz) = pcs%y(1:nyz)
         zpl(npl+1:npl+nyz) = pcs%z(1:nyz)
         npl = npl+nyz+1
         xpl(npl) = dmiss; ypl(npl) = dmiss; zpl(npl) = dmiss ! Separator between pli/csdef
      case default
         call QNERROR('Error in convert_cross_to_prof(), profile type not supported:',CSTypeName(pcs%crossType),'')
      end select
   end do

   call wripol(mxyz)


   call doclose(mxyz)
   call doclose(mdef)

   call increasesam(network%crs%Count)
   NS = 0
   do ic=1,network%crs%Count
      pcrs => network%crs%cross(ic)
      
      ierr = findlink(pcrs%branchid, pcrs%chainage, Lf)
      if (Lf > 0) then
         NS = NS+1
         XS(NS) = xu(Lf)
         YS(NS) = yu(Lf)
         ZS(NS) = cs2prof(pcrs%iTabDef)
      end if
   end do
   
   call newfil(mloc, proflocfile)
   call wrisam(mloc)
   call doclose(mloc)
   
   !call restoresam()
   !call restorepol()
   
   end subroutine convert_cross_to_prof
   
   !> Set groundLevel and groundStoarge arrays for 1d nodes.
   !! Ground level should not be confused with bed level.
   !! It is defined as:
   !! * street level for storage nodes that have no storage table (and storageType is either reservoir or closed),
   !! * the highest nearby cross section level ("embankment") for other nodes,
   !! * dmiss, i.e. not applicable, if no cross section is defined at the node.
   subroutine set_ground_level_for_1d_nodes(network)
   use m_flowgeom, only: groundLevel, groundStorage, ndxi, ndx2d, nd, lnxi, kcu
   use m_Storage
   use m_CrossSections
   use m_network
   implicit none
   type(t_network), intent(inout), target :: network
   type(t_storage), pointer               :: pSto
   type(t_administration_1d), pointer     :: adm
   integer                                :: i, istor, cc1, cc2, length, L, Lindex
   double precision                       :: f
   double precision, parameter            :: help = -huge(1d0)

   groundlevel(:) = help
   groundStorage(:) = 0
   adm => network%adm

   ! set for all 1D nodes, the ground level equals to the highest cross section "embankment" value
   do i = 1, ndxi-ndx2d
      do Lindex = 1, nd(i+ndx2d)%lnx
         L = nd(i+ndx2d)%ln(Lindex)
         if (kcu(abs(L)) /= 1) then
            cycle
         else if (L > 0) then
            cc1 = adm%line2cross(L,3)%c1
            cc2 = adm%line2cross(L,3)%c2
            f = adm%line2cross(L,3)%f
         else
            L = -L
            cc1 = adm%line2cross(L,1)%c1
            cc2 = adm%line2cross(L,1)%c2
            f = adm%line2cross(L,1)%f
         end if
            
         if (cc1 > 0 .and. cc2 > 0) then ! if there are defined cross sections
            groundLevel(i) = max(groundlevel(i),getHighest1dLevel(network%crs%cross(cc1), network%crs%cross(cc2), f))
            ! Note that for closed cross sections, the 'ground level' contains now the pipe roof level. This is intentional: for computing freeboard inside pipes.
            if (.not. network%crs%cross(cc1)%closed) then
               groundStorage(i) = 1
            end if
         else
            continue ! dmiss + 0 defaults.
         end if
      end do
   end do

   ! set for storage nodes (either from a table, or a prescribed street level (storageType is reservoir or closed))
    do istor = 1, network%storS%Count
      pSto => network%storS%stor(istor)
      if (pSto%grid_point < 1) cycle
      i = pSto%grid_point-ndx2d
      if (.not. pSto%use_table) then ! Manhole
         if (pSto%use_street_storage) then
            groundLevel(i) = pSto%street_area%x(1)
            if (pSto%storage_type == nt_closed) then
               groundStorage(i) = 0
            else
               groundStorage(i) = 1
            end if
         else
            groundStorage(i) = 0
         end if
      else ! storage node on open storage table, the ground level is set to the top level of the definition table.
         length = pSto%storage_area%length
         groundLevel(i) = maxval(pSto%storage_area%x(1:length))
         groundStorage(i) = 1
      end if
    end do

   ! Set the groundlevel entries that are not set to DMISS
   do i = 1, ndxi-ndx2d
      if (groundlevel(i) == help) then
         groundlevel(i) = dmiss
      endif
   enddo
    
   end subroutine set_ground_level_for_1d_nodes
   
   !> Set maximal volume for 1d nodes, later used for computation of volOnGround(:).
   subroutine set_max_volume_for_1d_nodes()
   use m_flowgeom, only: groundLevel, volMaxUnderground, ndx, ndxi, ndx2d
   use m_flow,     only: s1, vol1, a1, vol1_f, a1m, s1m, nonlin
   use m_alloc
   use unstruc_channel_flow, only: network
   implicit none
   double precision, allocatable :: s1_tmp(:), vol1_tmp(:), a1_tmp(:), vol1_ftmp(:), a1m_tmp(:), s1m_tmp(:)
   integer                       :: ndx1d
   logical, allocatable :: hysteresis_tmp(:,:)

   ndx1d = ndxi-ndx2d
   if (ndx1d == 0) then
      return
   end if

   ! 1. copy current s1, vol1, vol1_f, a1, a1m and hysteresis to a temporary array
   allocate(s1_tmp(ndx))
   s1_tmp = s1
   
   if (nonlin >= 2) then
      allocate(s1m_tmp(ndx))
      s1m_tmp = s1m

      allocate(a1m_tmp(ndx))
      a1m_tmp = a1m
   end if

   allocate(vol1_tmp(ndx))
   vol1_tmp = vol1
   
   allocate(vol1_ftmp(ndx))
   vol1_ftmp = vol1_f
   
   allocate(a1_tmp(ndx))
   a1_tmp = a1
   
   if (network%loaded) then
      if (network%numl > 0) then
         allocate(hysteresis_tmp(2, network%numl))
         hysteresis_tmp(:,:) = network%adm%hysteresis_for_summerdike
      end if
   end if

   ! 2. set s1 to be the ground level
   s1(ndx2d+1:ndxi)  = groundLevel(1:ndx1d)
   if (nonlin >= 2) then
      s1m(ndx2d+1:ndxi) = groundLevel(1:ndx1d)
      a1m    = 0d0
   end if

   vol1   = 0d0
   vol1_f = 0d0
   a1     = 0d0

   ! 3. compute the maximal volume
   call vol12d(0)
   volMaxUnderground(1:ndx1d) = vol1(ndx2d+1:ndxi)

   ! 4. set s1, vol1, vol1_f, a1, a1m and hysteresis back
   s1     = s1_tmp
   vol1   = vol1_tmp
   vol1_f = vol1_ftmp
   a1     = a1_tmp
   if (nonlin >= 2) then
      s1m    = s1m_tmp
      a1m    = a1m_tmp
   end if

   if (network%loaded) then
      if (network%numl > 0) then
         network%adm%hysteresis_for_summerdike = hysteresis_tmp
      end if
   end if
   end subroutine set_max_volume_for_1d_nodes


   !> Update freeboard for each 1d node.
   !! Freeboard is the vertical distance between the ground level, i.e., not bed level, and the water surface.
   !! It can be negative value if the water level is above the ground level
   !! It has minimal value 0d0 if it has storageType "closed", or if the relevant cross sections are closed.
   !! If the relevant cross sections are closed, freeboard is the vertical distance between the highest nearby cross section level ("embankment") and the water level.
   subroutine updateFreeboard(network)
   use m_flow, only: freeboard, s1
   use m_flowgeom, only: ndxi, ndx2d, groundLevel, groundStorage
   use m_network
   implicit none
   type(t_network), intent(inout), target :: network
   integer :: i, ii

   freeboard = dmiss
   do i = ndx2d+1, ndxi
      ii = i- ndx2d
      if (groundLevel(ii) .ne. dmiss) then ! if ground level is applicable
         if (groundStorage(ii) == 1) then ! also storage above ground: allow negative freeboard.
            freeboard(ii) = groundLevel(ii) - s1(i)
         else
            freeboard(ii) = max(0d0, groundLevel(ii) - s1(i))
         end if
      end if
   end do

   end subroutine updateFreeboard


   !> Compute the cumulative time when water is above ground level.
   subroutine updateTimeWetOnGround(dts)
   use m_flowparameters, only: epswetout
   use m_flowtimes, only: time_wetground
   use m_flow, only: s1
   use m_flowgeom,only: ndxi, ndx2d, groundLevel, groundStorage
   implicit none
   double precision, intent(in) :: dts !< computational time step
   integer                      :: i, ii
   
   do i = ndx2d+1, ndxi
      ii = i - ndx2d
      if (groundLevel(ii) .ne. dmiss .and. groundStorage(ii) == 1 .and. s1(i) - groundLevel(ii) >= epswetout) then
         time_wetground(i) = time_wetground(i) + dts
      end if
   end do 

   end subroutine updateTimeWetOnGround


   !> Update waterdepth above ground level for each 1d node.
   !! This is the vertical distance between the water surface and the ground level, i.e. waterLevel minus groundLevel.
   !! It has minimal value 0d0.
   !! It equals dmiss if the ground level is not applicable: node has storageType closed, or if the relevant cross sections are closed, or no cross section is defined.
   subroutine updateDepthOnGround(network)
   use m_flow, only: hsOnGround, s1
   use m_flowparameters, only: epswetout
   use m_network
   use m_flowgeom, only: ndxi, ndx2d, groundLevel, groundStorage
   implicit none
   type(t_network), intent(inout), target :: network !< 1D network from flow1d.

   integer                                :: i, ii

   hsOnGround = dmiss
   do i = ndx2d+1, ndxi
      ii = i-ndx2d
      if (groundLevel(ii) .ne. dmiss .and. groundStorage(ii) == 1 .and. s1(i) - groundLevel(ii) >= epswetout) then ! if groundLevel is applicable
         hsOnGround(ii) = max(0d0, s1(i) - groundLevel(ii))
      end if
   end do

   end subroutine updateDepthOnGround


   !> Update volume above ground level for each 1d node.
   !! It has minimal value 0d0
   !! It equals to dmiss if the node has storageType closed, or if the relevant cross sections are closed, or no cross section is defined.
   subroutine updateVolOnGround(network)
   use m_flow, only: vol1, volOnGround, s1
   use m_flowparameters, only: epswetout
   use m_flowgeom, only: volMaxUnderground, ndxi, ndx2d, groundLevel, groundStorage
   use m_network
   implicit none
   type(t_network), intent(inout), target :: network
   integer:: i, ii

   volOnGround = dmiss
   do i = ndx2d+1, ndxi
      ii = i-ndx2d
      if (groundLevel(ii) .ne. dmiss .and. groundStorage(ii) == 1 .and. s1(i) - groundLevel(ii) >= epswetout) then ! if groundLevel is applicable
         volOnGround(ii) = max(0d0, vol1(i) - volMaxUnderground(ii))
      end if
   end do

   end subroutine updateVolOnGround


   !> Update total net inflow through all connected 1d2d links for each 1d node with given computational time step.
   !! Value in vTot1d2d is cumulative in time since TStart.
   subroutine updateTotalInflow1d2d(dts)
   use m_flow, only: vTot1d2d, qCur1d2d, q1
   use m_flowgeom, only: ndx2d, lnx1d, kcu, ln
   implicit none
   double precision, intent(in) :: dts ! current computational time step

   integer          :: Lf, n
   double precision :: flowdir

   qCur1d2d = 0d0
   ! Don't reset vTot1d2d
   do Lf = 1, lnx1d
      if (kcu(Lf) == 3 .or. kcu(Lf) == 4 .or. kcu(Lf) == 5 .or. kcu(Lf) == 7) then
         n = ln(1, Lf)
         if (n < ndx2d) then
            n = ln(2, Lf)
            flowdir = 1d0  ! Flow link orientation *towards* 1D n
         else
            flowdir = -1d0 ! Flow link orientation *away from* 1D n
         end if
         ! n is now a 1d node
         qCur1d2d(n) = qCur1d2d(n) + flowdir*q1(Lf)
         vTot1d2d(n) = vTot1d2d(n) + flowdir*q1(Lf)*dts
      end if
   end do

   end subroutine updateTotalInflow1d2d

   
   !> Update total net inflow of all laterals for each 1d node with given computational time step.
   subroutine updateTotalInflowLat(dts)
   use m_flow, only: vTotLat, qCurLat
   use m_flowgeom, only: ndx2d, ndxi
   use m_lateral, only: qqlat
   implicit none
   double precision, intent(in) :: dts ! current computational time step
   integer                      :: n

   qCurLat = 0d0
   ! Don't reset vTotLat
   if (allocated(qqlat)) then
      do n = ndx2d+1, ndxi ! all 1d nodes
         qCurLat(n) = qCurLat(n) + qqlat(n)
         vTotLat(n) = vTotLat(n) + qqlat(n)*dts
      end do
   else
      return
   end if

   end subroutine updateTotalInflowLat
   
   !> Update water level gradient on 1D flow links.
   !! Preparation for map output.
   subroutine updateS1Gradient()
   use m_flow, only: s1Gradient, s1, hu, epshu
   use m_flowgeom, only: lnx1d, ln, dx
   implicit none
   integer :: k1, k2, L
   
   s1Gradient = dmiss
   do L=1,lnx1d
      if (hu(L) > epshu) then
         k1 = ln(1,L)
         k2 = ln(2,L)
         s1Gradient(L) = (s1(k1) - s1(k2)) / dx(L)
      end if
   end do
   
   end subroutine updateS1Gradient

   !> Update the friction parameters in case of time dependent roughness, and if necessary update the conveyance table
   subroutine reCalculateConveyanceTables(network)

      use m_network
      use m_CrossSections
      use m_readCrossSections

      type(t_network), intent(inout) :: network !< Network definition
   
      integer        :: count, i
      type(t_CrossSection), dimension(:), pointer :: cross

      
      count = network%crs%Count
      cross => network%crs%cross
      do i = 1, count
         if (cross(i)%tabDef%crossType == CS_YZ_PROF) then
            call getRoughnessForProfile(network, cross(i))
            if (cross(i)%hasTimeDependentConveyance) then
               call CalcConveyance(cross(i))
            endif
         endif
      enddo
   end subroutine reCalculateConveyanceTables
   
   ! Perform a time interpolation of the roughness parameters and store them in the currentValues array
   subroutine interpolateRoughnessParameters(rgs, times_update_roughness, tim)
      use m_Roughness

      type(t_RoughnessSet),          intent(inout)    :: rgs                        !< Roughness set
      double precision, dimension(2),intent(in   )    :: times_update_roughness     !< Times at which the time dependent values are set
      double precision,              intent(in   )    :: tim                        !< Current time

      double precision, pointer, dimension(:)      :: currentValues
      double precision, pointer, dimension(:,:)    :: timeDepValues
      integer irgh, i, timeseries_count
      double precision f

      f = (tim - times_update_roughness(1))/(times_update_roughness(2)-times_update_roughness(1))
      do irgh = 1, rgs%Count
         timeseries_count = rgs%rough(irgh)%timeSeriesIds%id_count 
         if (timeseries_count> 0) then 
            currentValues => rgs%rough(irgh)%currentValues
            timeDepValues    =>  rgs%rough(irgh)%timeDepValues
            do i = 1, timeseries_count
               currentValues(i) = (1d0-f) * timeDepValues(i,1) + f * timeDepValues(i,2)
            enddo
         endif
      enddo
   end subroutine interpolateRoughnessParameters

   !> Shift the time dependent values, called prior to the update of the roughness parameters for a new time level
   subroutine shiftTimeDependentRoughnessValues(rgs)
      use m_Roughness

      type(t_RoughnessSet),          intent(inout)    :: rgs            !< Roughness set

      double precision, pointer, dimension(:,:)    :: timeDepValues
      integer :: irgh

      do irgh = 1, rgs%Count
         if (rgs%rough(irgh)%timeSeriesIds%id_count> 0) then 
            timeDepValues    =>  rgs%rough(irgh)%timeDepValues
            timeDepValues(:,1) = timeDepValues(:,2)
         endif
      enddo
   end subroutine shiftTimeDependentRoughnessValues

end module m_oned_functions
