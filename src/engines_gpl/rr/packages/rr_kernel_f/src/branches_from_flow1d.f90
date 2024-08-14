!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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

 !!> Module containing data definition of branches in Delft_model_data
!module m_branch
!!----- AGPL --------------------------------------------------------------------
!!
!!  Copyright (C)  Stichting Deltares, 2017-2019.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU Affero General Public License as      
!!  published by the Free Software Foundation version 3.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU Affero General Public License for more details.
!!
!!  You should have received a copy of the GNU Affero General Public License     
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!!  are registered trademarks of Stichting Deltares, and remain the property of
!!  Stichting Deltares. All rights reserved.
!!
!!-------------------------------------------------------------------------------
!!  $Id: branches_from_flow1d.f90 64977 2019-12-13 15:10:55Z wilms $
!!  $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/engines/rr/packages/rr_kernel_f/src/branches_from_flow1d.f90 $
!!-------------------------------------------------------------------------------
!
!   use MessageHandling
!   use m_alloc
!   use m_node
!   use m_hash_search_rr
!
!   implicit none
!
!   private
!
!   public realloc
!   public dealloc
!   public getCalcPoints
!   public getCalcPoint
!   public get2CalcPoints
!   public admin_branch
!   public fill_hashtable
!   public getLinkIndex
!
!   public BR_EMBEDDED, BR_CONNECTED, BR_ISOLATED, BR_BOUNDARY
!
!   interface fill_hashtable
!      module procedure fill_hashtable_brs
!   end interface
!
!   interface realloc
!      module procedure reallocbranch
!   end interface
!
!   interface dealloc
!      module procedure deallocbranch
!   end interface dealloc
!
!   !> branch information of network
!   type, public :: t_branch
!      character(IdLen)               :: id                      !< unique identification
!      integer                        :: index                   !< sequence number
!      character(IdLen)               :: name                    !< Name of the branch
!
!      character(IdLen)               :: fromnode2
!      character(IdLen)               :: tonode2
!
!
!      double precision               :: length                  !< length of branch
!      integer                        :: orderNumber             !< order number to interpolate cross sections over branches
!
!      integer                        :: brType                  !< channel type of 1D channnel
!      integer                        :: iTrench                 !< Trench Index, 0 = No Trench
!
!      integer                        :: flapGate = 0            !< 0 = None, 1 = Only Positive Flow, 2 = Only Negative Flow
!                                                                !< Not implemeted in Readers yet
!
!      integer                        :: nextBranch(2)           !< neighbouring branch with same ordernumber at start or end of branch
!      integer                        :: nodeIndex(2)            !< indexes of Begin Node and End Node
!      type(t_node), pointer          :: FromNode => null()      !< node at start of branch
!      type(t_node), pointer          :: ToNode => null()        !< node at end of branch
!
!
!      integer                        :: gridPointsCount         !< number of grid points on branch
!      double precision, allocatable  :: gridPointsOffsets(:)    !< offset of grid points on branch
!      character(IdLen), allocatable  :: gridPointIDs(:)         !< ID's of grid points on branch
!      double precision, allocatable  :: X(:)                   !< X-coordinates of grid points
!      double precision, allocatable  :: Ys(:)                   !< Y-coordinates of grid points
!
!      integer                        :: uPointsCount            !< number of u points on branch (gridpointsCount -1)
!      double precision, allocatable  :: uPointsOffsets(:)       !< offset of velocity points on branch (each upoint
!      double precision, allocatable  :: Xu(:)                   !< X-coordinates of u points
!      double precision, allocatable  :: Yu(:)                   !< Y-coordinates of u points
!      double precision, allocatable  :: dx(:)                   !< distance between two gridpoints
!
!      integer                        :: Points(2)               !< Calculation Points at Start and End of Branch
!      integer                        :: uPoints(2)              !< Velocity Points at Start and End of Branch
!
!      integer, allocatable           :: lin(:)                  !< link numbers for links in this channel, allocated and filled by admin_network
!      integer, allocatable           :: grd(:)                  !< gridpoint numbers for links in this channel, allocated and filled by admin_network
!      integer, allocatable           :: grd_buf(:)              !< dflowfm gridpoint numbers for links in this channel, allocated and filled by admin_network
!                                                                !< used to keep dflowfm grd-values
!
!   end type t_branch
!
!   !> Set of branches in network
!   type, public :: t_branchSet
!      integer                                               :: Size=0                !< current length of array branch
!      integer                                               :: growsBy= 2000         !< used increment for extending array branch
!      integer                                               :: Count=0               !< number of registered branches
!      integer                                               :: gridpointsCount = 0   !< nr of grid points in network (total over branches)
!      type(t_branch), pointer, dimension(:)                 :: branch => null()      !< array containing branch information
!      type(t_hashlist)                                      :: hashlist
!   end type t_branchSet
!
!   integer, parameter    :: BR_BOUNDARY  = -1
!   integer, parameter    :: BR_EMBEDDED  = 0
!   integer, parameter    :: BR_ISOLATED  = 1
!   integer, parameter    :: BR_CONNECTED = 2
!
!   contains
!
!   !> Deallocate given branch
!   subroutine deallocbranch(brs)
!      ! Modules
!
!      implicit none
!
!      ! Input/output parameters
!      type(t_branchSet), intent(inout) :: brs !< Current branch set
!
!      ! Local variables
!      integer                       :: i
!      integer                       :: length
!
!      ! Program code
!      if (associated(brs%branch)) then
!         length = size(brs%branch)
!         do i = 1, length
!            if (allocated(brs%branch(i)%gridPointsOffsets)) deallocate(brs%branch(i)%gridPointsOffsets)
!            if (allocated(brs%branch(i)%gridPointIDs))      deallocate(brs%branch(i)%gridPointIDs)
!            if (allocated(brs%branch(i)%uPointsOffsets))    deallocate(brs%branch(i)%uPointsOffsets)
!            if (allocated(brs%branch(i)%dx))                deallocate(brs%branch(i)%dx)
!            if (allocated(brs%branch(i)%x))                deallocate(brs%branch(i)%x)
!            if (allocated(brs%branch(i)%ys))                deallocate(brs%branch(i)%ys)
!            if (allocated(brs%branch(i)%xu))                deallocate(brs%branch(i)%xu)
!            if (allocated(brs%branch(i)%yu))                deallocate(brs%branch(i)%yu)
!            if (allocated(brs%branch(i)%lin))               deallocate(brs%branch(i)%lin)
!            if (allocated(brs%branch(i)%grd))               deallocate(brs%branch(i)%grd)
!            if (allocated(brs%branch(i)%grd_buf))           deallocate(brs%branch(i)%grd_buf)
!         enddo
!         deallocate(brs%branch)
!      endif
!      call dealloc(brs%hashlist)
!
!      brs%branch => null()
!      brs%Size  = 0
!      brs%Count = 0
!      brs%gridpointsCount = 0    ! nr of points
!   end subroutine
!!
!   !> Reallocate given branch
!   subroutine reallocbranch(brs)
!      ! Modules
!
!      implicit none
!
!      ! Input/output parameters
!      type(t_branchSet), intent(inout)          :: brs !< Current branch set
!
!      ! Local variables
!      type(t_branch), pointer, dimension(:)     :: oldBrs
!
!      ! Program code
!
!      if (brs%Size > 0) then
!         oldBrs=>brs%branch
!      else
!         brs%gridpointsCount = 0
!      endif
!
!      if (brs%growsBy <=0) then
!         brs%growsBy = 200
!      endif
!      allocate(brs%branch(brs%Size+brs%growsBy))
!
!      if (brs%Size > 0) then
!         brs%branch(1:brs%Size) = oldBrs(1:brs%Size)
!         deallocate(oldBrs)
!      endif
!      brs%Size = brs%Size+brs%growsBy
!   end subroutine
!
!   !> this function returns the link index based on (branch/chainage), to be used in FM, where subarray
!   !! LIN is filled correctly
!   integer function getLinkIndex(branch, chainage)
!
!       type(t_branch)                  :: branch
!       double precision, intent(in)    :: chainage  !< Chainage
!
!       integer                         :: i
!
!       do i = 2, branch%gridPointsCount
!           if (branch%gridPointsOffsets(i) >= chainage) then !found
!              getLinkIndex = branch%lin(i-1)
!              exit
!           endif
!       enddo
!       if (branch%gridPointsOffsets(branch%gridPointsCount) < chainage) then
!          getLinkIndex = branch%lin(branch%gridPointsCount-1)
!       endif
!
!   end function getLinkIndex
!
!   !> Get calculation points just before and after structure location on a branch (used in SOBEK)
!   subroutine getCalcPoints(brs, ibranch, dist, leftcalc, rightcalc, ilink, distcalc)
!
!       type(t_branchSet)               :: brs       !< Current branche set
!       integer, intent(in)             :: ibranch   !< Current branch
!       double precision, intent(inout) :: dist      !< Distance along current branch
!       integer, intent(out)            :: leftcalc  !< most left calculation point (i.e. lower index)
!       integer, intent(out)            :: rightcalc !< most right calculation point (i.e. upper index)
!       integer, intent(out)            :: ilink     !< linknumber of link between upper and lower
!       double precision, intent(out)   :: distcalc  !< distance of structure, at least 0.2 [m] from nodes
!
!       integer                         :: i
!       double precision                :: dist_in_b
!       type(t_branch), pointer         :: pbran
!
!       pbran => brs%branch(ibranch)
!
!       dist_in_b= 0.0
!       distcalc = 0.0
!       leftcalc = 0
!       rightcalc= 0
!       dist = max(0.2, min(dist, pbran%length-0.2)) !< JanM: Waarom is de afstand afgeknot en bestaat er een minSectionLength
!       do i = 1, pbran%gridPointsCount
!           if (pbran%gridPointsOffsets(i) > dist) then !found
!               leftcalc  = pbran%Points(1) - 1 + i - 1
!               rightcalc = pbran%Points(1) - 1 + i
!               ilink     = pbran%uPoints(1) - 1 + i - 1
!               distcalc  = dist
!               exit
!           endif
!       enddo
!
!       !error checks: JanM Not yet implemented
!       if (leftcalc < pbran%Points(1) .or. rightcalc > pbran%Points(2)) then
!          call setMessage(LEVEL_ERROR, "Calculation Point Out of Range in Branch: '"//trim(pbran%id)//"'")
!       endif
!
!       if (leftcalc == 0 .or. rightcalc == 0) then
!          call setMessage(LEVEL_ERROR, "No Calculation Point Found in Branch: '"//trim(pbran%id)//"'")
!       endif
!
!       if (dist > pbran%length ) then
!          call setMessage(LEVEL_ERROR, "Structure Location Outside Branch: '"//trim(pbran%id)//"'")
!       endif
!
!   end subroutine
!
!   integer function getLinkNumber(brs, ibranch, dist)
!       type(t_branchSet)               :: brs       !< Current branche set
!       integer, intent(in)             :: ibranch   !< Current branch
!       double precision, intent(inout) :: dist      !< Distance along current branch
!
!       integer                         :: i
!       double precision                :: dist_in_b
!       type(t_branch), pointer         :: pbran
!
!       pbran => brs%branch(ibranch)
!
!       dist_in_b= 0.0
!       dist = max(0.2, min(dist, pbran%length-0.2)) !< JanM: Waarom is de afstand afgeknot en bestaat er een minSectionLength
!       do i = 2, pbran%gridPointsCount
!           if (pbran%gridPointsOffsets(i) > dist) then !found
!              getLinkNumber = pbran%lin(i-1)
!              return
!           endif
!       enddo
!
!       getlinknumber = -1
!   end function getLinkNumber
!
!   !> Get calculation point and/or segment number closest to given distance along branch
!   integer function getCalcPoint(brs, ibranch, dist)
!       type(t_branchSet)              :: brs           !< Current branche set
!       integer, intent(in)            :: ibranch       !< Current branch
!       double precision, intent(in)   :: dist          !< Distance along current branch
!
!      integer              :: p1, p2
!      double precision     :: weight
!
!      call Get2Points(brs%branch(ibranch)%gridPointsOffsets, &
!                 brs%branch(ibranch)%gridpointsCount, dist, p1, p2, Weight)
!      p1 = brs%branch(ibranch)%Points(1) -1 + p1
!      p2 = brs%branch(ibranch)%Points(1) -1 + p2
!      if (weight > 0.5) then
!         getCalcPoint = p1
!      else
!         getCalcPoint = p2
!      endif
!   end function
!
!   subroutine get2CalcPoints(brs, ibranch, dist, p1, p2, pointWeight, l1, l2, linkWeight)
!      type(t_branchSet)              :: brs           !< Current branche set
!      integer, intent(in)            :: ibranch       !< Current branch
!      double precision, intent(in)   :: dist          !< Distance along current branch
!      integer, intent(out), optional :: p1            !< first gridpoint index
!      integer, intent(out), optional :: p2            !< second gridpoint index
!      double precision, intent(out)  :: pointWeight   !< weight for determining function value depending on function value in P1 and P2
!      integer, intent(out), optional :: l1            !< first u-point index
!      integer, intent(out), optional :: l2            !< second u-point index
!      double precision, intent(out)  :: linkWeight    !< weight for determining function value depending on function value in l1 and l2
!
!      call Get2Points(brs%branch(ibranch)%gridPointsOffsets, &
!                 brs%branch(ibranch)%gridpointsCount, dist, p1, p2, pointWeight)
!      p1 = brs%branch(ibranch)%Points(1) -1 + p1
!      p2 = brs%branch(ibranch)%Points(1) -1 + p2
!
!      call Get2Points(brs%branch(ibranch)%uPointsOffsets, &
!                 brs%branch(ibranch)%upointsCount, dist, l1, l2, linkWeight)
!      l1 = brs%branch(ibranch)%uPoints(1) -1 + l1
!      l2 = brs%branch(ibranch)%uPoints(1) -1 + l2
!
!   end subroutine
!
!
!   subroutine Get2Points(offsets, offsetCount, offset, p1, p2, weight)
!
!      integer                       :: offsetCount          !<
!      double precision              :: offset
!      double precision, dimension(offsetCount) :: offsets
!      integer :: p1
!      integer :: p2
!      double precision :: weight
!
!      integer        :: i
!
!      do i=2, offsetCount ! only internal points of the branch
!         ! look up calculation point and/or segment in which the grid point lies
!         if (Offsets(i) > offset) then !found
!            p1 = i-1
!            p2 = i
!            weight = (offsets(i)-offset)/(offsets(i)-offsets(i-1))
!            if (weight < 0.0) then
!               weight = 0.0
!            elseif (weight > 1.0) then
!               weight = 1.0
!            endif
!
!            return
!         endif
!      enddo
!
!      p1 = offsetCount
!      p2 = offsetCount
!      weight = 0d0
!   end subroutine get2Points
!
!   subroutine admin_branch(brs, ngrid, nlink)
!
!      type (t_branchSet), intent(inout), target :: brs
!      integer, intent(inout) :: ngrid
!      integer, intent(inout) :: nlink
!
!      integer ibr, i
!      type(t_branch), pointer :: pbr
!
!      do ibr= 1, brs%count
!         pbr => brs%branch(ibr)
!         if (allocated(pbr%lin)) deallocate(pbr%lin)
!         allocate(pbr%lin(pbr%uPointsCount))
!         do i = 1, pbr%uPointsCount
!            nlink = nlink + 1
!            pbr%lin(i) = nlink
!         enddo
!
!         !TODO: change this part in order to remove double gridpoint counting:
!         if (allocated(pbr%grd)) deallocate(pbr%grd)
!         if (allocated(pbr%grd_buf)) deallocate(pbr%grd_buf)
!         allocate(pbr%grd(pbr%gridPointsCount))
!         allocate(pbr%grd_buf(pbr%gridPointsCount))
!         ngrid = pbr%Points(1) - 1
!         if (pbr%FromNode%gridNumber == -1) then
!            pbr%FromNode%gridNumber = ngrid + 1
!         endif
!         do i = 1, pbr%gridPointsCount
!            ngrid = ngrid + 1
!            pbr%grd(i) = ngrid
!         enddo
!         if (pbr%ToNode%gridNumber == -1) then
!            pbr%ToNode%gridNumber = ngrid
!         endif
!      enddo
!
!   end subroutine admin_branch
!
!   subroutine fill_hashtable_brs(brs)
!
!      type (t_branchSet), intent(inout), target :: brs
!
!      integer ibr
!      character(len=idlen), dimension(:), pointer :: ids
!
!      allocate(brs%hashlist%id_list(brs%Count))
!      brs%hashlist%id_count = brs%Count
!      ids => brs%hashlist%id_list
!
!      do ibr= 1, brs%count
!         ids(ibr) = brs%branch(ibr)%id
!      enddo
!
!      call hashfill_rr(brs%hashlist)
!   end subroutine fill_hashtable_brs
!
!end module m_branch
