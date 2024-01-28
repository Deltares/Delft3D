!> Module containing data definition of branches in Delft_model_data
module m_branch
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
   use m_alloc
   use m_node
   use m_hash_search
   
   implicit none
   
   private

   public realloc
   public dealloc
   public fill_hashtable
   public getLinkIndex
   public getGridPointNumber
   public admin_branch
   
   interface fill_hashtable
      module procedure fill_hashtable_brs
   end interface 

   interface realloc
      module procedure reallocbranch
   end interface

   interface dealloc
      module procedure deallocbranch
   end interface dealloc

   !> branch information of network
   type, public :: t_branch
      character(IdLen)               :: id                      !< unique identification
      integer                        :: index                   !< sequence number
      double precision               :: length                  !< length of branch
      integer                        :: orderNumber             !< order number to interpolate cross sections over branches

      integer                        :: nextBranch(2)           !< neighbouring branch with same ordernumber at start or end of branch
      integer                        :: nodeIndex(2)            !< indexes of Begin Node and End Node
      type(t_node), pointer          :: FromNode => null()      !< node at start of branch
      type(t_node), pointer          :: ToNode => null()        !< node at end of branch


      integer                        :: gridPointsCount         !< number of grid points on branch
      double precision, allocatable  :: gridPointsChainages(:)  !< chainage of grid points on branch
      character(IdLen), allocatable  :: gridPointIDs(:)         !< ID's of grid points on branch
      double precision, allocatable  :: Xs(:)                   !< X-coordinates of grid points
      double precision, allocatable  :: Ys(:)                   !< Y-coordinates of grid points

      integer                        :: uPointsCount            !< number of u points on branch (gridpointsCount -1)
      double precision, allocatable  :: uPointsChainages(:)     !< chainage of velocity points on branch (each upoint 

      !!
      !! Concept: Grid points sequences.
      !! A network branch will typically be covered by grid points, e.g., in the gridPointsChainages(:) array.
      !! In a single domain model, this will be a list of grid points from the branch's fromNode to its endNode.
      !! In a parallel multi domain model, this may be several sequences of consecutive grid points, separated
      !! by "holes" where the original grid points are owned by other domain(s).
      !!
      integer                        :: gridPointsSeqCount      !< Number of "gridpoint sequences" on a single branch.
      integer, allocatable           :: k1gridPointsSeq(:)      !< Start indexes for all "gridpoint sequences" on a single branch. Can be used to index into arrays such as gridPointsChainages(:).
      integer, allocatable           :: k2gridPointsSeq(:)      !< End indexes for all "gridpoint sequences" on a single branch. Can be used to index into arrays such as gridPointsChainages(:).
      logical                        :: isGPFirstAtBranchStart = .true. !< Whether the first grid point lies at the start of the network branch (always true for sequential models, not per se for multi-partition parallel models.)
      logical                        :: isGPLastAtBranchEnd    = .true. !< Whether the last grid point lies at the end of the network branch (always true for sequential models, not per se for multi-partition parallel models.)


      integer                        :: StartPoint              !< Calculation Point at Start of Branch
      integer, allocatable           :: lin(:)                  !< link numbers for links in this channel
      integer, allocatable           :: grd(:)                  !< gridpoint numbers pressure points in this channel, may include automatically-added grid points at start and/or end or branch
      integer, allocatable           :: grd_input(:)            !< original input gridpoint numbers for pressure points in this channel, only set for any not-automatically-added grid points at start and/or end of branch,
   end type t_branch

   !> Set of branches in network
   type, public :: t_branchSet
      integer                                               :: Size=0                !< current length of array branch
      integer                                               :: growsBy= 2000         !< used increment for extending array branch
      integer                                               :: Count=0               !< number of registered branches
      integer                                               :: gridpointsCount = 0   !< nr of grid points in network (total over branches) 
      type(t_branch), pointer, dimension(:)                 :: branch => null()      !< array containing branch information
      type(t_hashlist)                                      :: hashlist
   end type t_branchSet

   integer, parameter    :: BR_BOUNDARY  = -1 
   integer, parameter    :: BR_EMBEDDED  = 0
   integer, parameter    :: BR_ISOLATED  = 1
   integer, parameter    :: BR_CONNECTED = 2
   
   contains
   
   !> Deallocate given branch
   subroutine deallocbranch(brs)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_branchSet), intent(inout) :: brs !< Current branch set

      ! Local variables
      integer                       :: i
      integer                       :: length
  
      ! Program code
      if (associated(brs%branch)) then
         length = size(brs%branch)
         do i = 1, length
            if (allocated(brs%branch(i)%gridPointschainages)) deallocate(brs%branch(i)%gridPointschainages)
            if (allocated(brs%branch(i)%gridPointIDs))      deallocate(brs%branch(i)%gridPointIDs)
            if (allocated(brs%branch(i)%uPointschainages))    deallocate(brs%branch(i)%uPointschainages)
            if (allocated(brs%branch(i)%xs))                deallocate(brs%branch(i)%xs)
            if (allocated(brs%branch(i)%ys))                deallocate(brs%branch(i)%ys)
            if (allocated(brs%branch(i)%lin))               deallocate(brs%branch(i)%lin)
            if (allocated(brs%branch(i)%grd))               deallocate(brs%branch(i)%grd)
            if (allocated(brs%branch(i)%grd_input))         deallocate(brs%branch(i)%grd_input)
         enddo   
         deallocate(brs%branch)
      endif
      call dealloc(brs%hashlist)
      
      brs%branch => null()
      brs%Size  = 0
      brs%Count = 0
      brs%gridpointsCount = 0    ! nr of points
   end subroutine
!
   !> Reallocate given branch
   subroutine reallocbranch(brs)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_branchSet), intent(inout)          :: brs !< Current branch set
      
      ! Local variables
      type(t_branch), pointer, dimension(:)     :: oldBrs
      
      ! Program code
      
      if (brs%Size > 0) then
         oldBrs=>brs%branch
      else
         brs%gridpointsCount = 0
      endif
      
      if (brs%growsBy <=0) then
         brs%growsBy = 200
      endif
      allocate(brs%branch(brs%Size+brs%growsBy))
      
      if (brs%Size > 0) then
         brs%branch(1:brs%Size) = oldBrs(1:brs%Size)
         deallocate(oldBrs)
      endif
      brs%Size = brs%Size+brs%growsBy
   end subroutine
   
   !> this function returns the link index based on (branch/chainage), to be used in FM, where subarray
   !! LIN is filled correctly
   integer function getLinkIndex(branch, chainage) 
      use precision_basics, only: comparereal
      use m_GlobalParameters, only: flow1d_eps10

       type(t_branch)                  :: branch
       double precision, intent(in)    :: chainage  !< Chainage

       integer                         :: i, is, k1, k2

       getLinkIndex = -1
       do is=1,branch%gridPointsSeqCount
          k1 = branch%k1gridPointsSeq(is)
          k2 = branch%k2gridPointsSeq(is)
          if (comparereal(chainage, branch%gridPointschainages(k1), flow1d_eps10) >= 0 &
             .and. comparereal(chainage, branch%gridPointschainages(k2), flow1d_eps10) <= 0) then
             do i=k1+1,k2
                if (comparereal(branch%gridPointschainages(i), chainage, flow1d_eps10) >= 0) then !found
                   getLinkIndex = branch%lin(i-1 - (is-1)) ! is-1 corrects for any "holes" in between the previous is-1 gridpointssequences.
                   exit
                end if
             end do
             exit ! No need check any of the other gridpointssequences.
          end if
       end do

   end function getLinkIndex
   

   !> Gets the grid point number for a location on a specific branch
   !! at a specific offset/chainage.
   !! The selected grid point is the furthest gridpoint whose following
   !! u-point lies beyond the requested chainage.
   !! For parallel models, an additional check is done that the location
   !! lies in the current domain's gridpoint sequences.
   integer function getGridPointNumber(branch, chainage)
      use precision_basics, only: comparereal
      use m_GlobalParameters, only: flow1d_eps10
      type (t_branch)   , intent(in   ) :: branch              !< branch object
      double precision  , intent(in   ) :: chainage            !< chainage of object on branch
      
      integer :: i, is, k1, k2, L1, L2
      double precision :: lastchainage
      
      getGridPointNumber = 0
      do is=1,branch%gridPointsSeqCount
         k1 = branch%k1gridPointsSeq(is)
         k2 = branch%k2gridPointsSeq(is)
         L1 = k1 - (is-1) ! is-1 corrects for any "holes" in between the previous is-1 gridpointssequences.
         L2 = k2 - is
         ! Note: UNST-5013: the following "inside gridpointssequence check" has the unavoidable effect
         ! that for branches with holes, if the chainage lies just after the end of an interior
         ! gridpoints sequence, the current domain will not find the grid point, but the
         ! neighbouring domain will find it.
         if (comparereal(chainage, branch%gridPointschainages(k1), flow1d_eps10) >= 0 &
            .and. comparereal(chainage, branch%gridPointschainages(k2), flow1d_eps10) <= 0) then
            do i=L1,L2
               if (comparereal(branch%uPointschainages(i), chainage, flow1d_eps10) > 0) then !found
                  getGridPointNumber = branch%grd(i + (is-1))
                  exit
               end if
            end do
            exit ! No need check any of the other gridpointssequences.
         end if
      end do

      ! for exceptional input, return start/end point of branch
      if (getGridPointNumber <= 0 .and. branch%gridPointsCount > 0) then
         if (comparereal(chainage, 0d0, flow1d_eps10) == -1 .and. branch%isGPFirstAtBranchStart) then
            getGridPointNumber = branch%grd(1)
         else
            if (branch%uPointsCount > 0) then
               ! Requested point chainage might lie *after* last uPointChainage, but *before* end of branch.
               lastchainage = branch%uPointsChainages(branch%uPointsCount)
            else
               lastchainage = branch%length
            end if
            if (comparereal(chainage, lastchainage, flow1d_eps10) >= 0 .and. branch%isGPLastAtBranchEnd) then
               getGridPointNumber = branch%grd(branch%gridPointsCount)
            end if
         end if
      end if

   end function getGridPointNumber
  
   subroutine fill_hashtable_brs(brs)
   
      type (t_branchSet), intent(inout), target :: brs
      
      integer ibr
      character(len=idlen), dimension(:), pointer :: ids
      
      allocate(brs%hashlist%id_list(brs%Count))
      brs%hashlist%id_count = brs%Count
      ids => brs%hashlist%id_list
      
      do ibr= 1, brs%count
         ids(ibr) = brs%branch(ibr)%id
      enddo
      
      call hashfill(brs%hashlist)
   end subroutine fill_hashtable_brs

   !> Sets up the administration for all branches:
   !! * from/to topology and %grd and %lin discretization points.
   !! * the gridPointsSequence administration per branch
   subroutine admin_branch(brs, nlink)
      use precision_basics, only: comparereal
      use m_GlobalParameters, only: flow1d_eps10

      type (t_branchSet), target, intent(inout) :: brs   !< Branch set from the network.
      integer,                    intent(inout) :: nlink !< Total number of links. (Upon input, any existing links from the call site.
                                                         !< Upon output: total number of links after administering all branches.)

      integer                 :: ibr, i, ngrid, ngrid_input, iUCandidate
      type(t_branch), pointer :: pbr
      logical                 :: newsequence
      logical                 :: admin_input !< Whether or not to make administration for input grid points

      if (brs%count > 0) then
         admin_input = allocated(brs%branch(1)%grd_input)
      else
         admin_input = .false.
      end if
      ngrid_input = 0

      do ibr= 1, brs%count
         pbr => brs%branch(ibr)
         call realloc(pbr%lin, pbr%uPointsCount, keepExisting = .false.)
         do i = 1, pbr%uPointsCount
            nlink = nlink + 1
            pbr%lin(i) = nlink
         enddo

         ! NB: the values for pbr%...Node%gridNumber and pbr%grd(:) will be recalculated in set_linknumbers_in_branches
         ! NB: the gridpointscount may included extra added grid points are start/end of branch, which were not in input (see construct_network_from_meshgeom())
         call realloc(pbr%grd, pbr%gridPointsCount, keepExisting = .false.)
         ngrid = pbr%StartPoint - 1

         ! Administer first grid points sequence, if present:
         if (pbr%gridPointsCount <= 0) then
            pbr%gridPointsSeqCount = 0

            pbr%isGPFirstAtBranchStart = .false.
            pbr%isGPLastAtBranchEnd    = .false.
            cycle
         else
            pbr%gridPointsSeqCount = 1
            call realloc(pbr%k1gridPointsSeq, pbr%gridPointsSeqCount, keepExisting = .false., fill = 0)
            call realloc(pbr%k2gridPointsSeq, pbr%gridPointsSeqCount, keepExisting = .false., fill = 0)
            pbr%k1gridPointsSeq(pbr%gridPointsSeqCount) = 1
            iUCandidate = 1 ! This is the first u-point to be checked for this sequence.

            pbr%isGPFirstAtBranchStart = (comparereal(pbr%gridPointsChainages(1), 0d0, flow1d_eps10) == 0)
            pbr%isGPLastAtBranchEnd    = (comparereal(pbr%gridPointsChainages(pbr%gridPointsCount), pbr%length, flow1d_eps10) == 0)
         end if

         if (pbr%FromNode%gridNumber == -1 .and. pbr%isGPFirstAtBranchStart) then
            pbr%FromNode%gridNumber = ngrid + 1
         endif

         ! Loop over consecutive gridpoints and detect the gridpointssequence(s) it is composed of.
         do i = 1, pbr%gridPointsCount-1
            ngrid = ngrid + 1
            pbr%grd(i) = ngrid

            if (admin_input) then
               if (pbr%grd_input(i) /= 0) then
                  ! Store the original input grid point number, without auto-added
                  ! start/end points on branch.
                  ngrid_input = ngrid_input+1
                  pbr%grd_input(i) = ngrid_input
               end if
            end if

            ! Administer grid points sequences:
            if (iUCandidate <= pbr%uPointsCount) then
               ! When next u-point does not lie between current gridpoint #i and next #i+1,
               ! then that must be a new gridpoints sequence
               newsequence = (pbr%uPointsChainages(iUCandidate) > pbr%gridPointsChainages(i+1))
            else
               ! No more u-points, but still gridpoint(s) left, so all of these
               ! remaining gridpoints must be singleton 1D points, each forming
               ! a 1-points sequence on its own.
               ! This is possible when 1D2D links near partition boundaries
               ! have their 1D endpoints as loose ghost nodes in this partition.
               newsequence = .true.
            end if

            if (newsequence) then
               if (pbr%gridPointsSeqCount > 0) then
                  ! Administer end point of current sequence.
                  pbr%k2gridPointsSeq(pbr%gridPointsSeqCount)   = i
               end if
               if (i < pbr%gridPointsCount) then
                  ! Administer start point of a new sequence.
                  pbr%gridPointsSeqCount = pbr%gridPointsSeqCount + 1
                  call realloc(pbr%k1gridPointsSeq, pbr%gridPointsSeqCount, keepExisting = .true., fill = 0)
                  call realloc(pbr%k2gridPointsSeq, pbr%gridPointsSeqCount, keepExisting = .true., fill = 0)
                  pbr%k1gridPointsSeq(pbr%gridPointsSeqCount) = i+1
               end if
            else
               ! Next u-point simply lies between current gridpoint #i and next #i+1,
               ! so this sequence still continues: advance u-point candidate index for next loop step.
               iUCandidate = iUCandidate + 1
            end if

         enddo
         if (pbr%gridPointsSeqCount > 0) then
            ! Administer end point of last sequence.
            pbr%k2gridPointsSeq(pbr%gridPointsSeqCount) = pbr%gridPointsCount
         end if

         ngrid = ngrid + 1
         pbr%grd(i) = ngrid
         if (admin_input) then
            if (pbr%grd_input(i) /= 0) then
               ! Store the original input grid point number, without auto-added
               ! start/end points on branch.
               ngrid_input = ngrid_input+1
               pbr%grd_input(i) = ngrid_input
            end if
         end if

         if (pbr%ToNode%gridNumber == -1.and. pbr%isGPLastAtBranchEnd) then
            pbr%ToNode%gridNumber = ngrid
         endif
      enddo

   end subroutine admin_branch

end module m_branch
