module m_network
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

   use m_GlobalParameters
   use m_branch
   use m_forcinglist
   use m_crossSections
   use m_1d_structures
   use m_roughness
   use m_ObservCrossSections
   use m_compound
   use m_spatial_data
   use m_Storage
   use m_ObservationPoints
   
   implicit none

   public realloc
   public dealloc
   public admin_network
   public initialize_1dadmin
   public getFrictionValue
   public update_flow1d_admin
   public getRoughnessForProfile
   
   
   interface realloc
      module procedure realloc_1dadmin
   end interface realloc

    interface dealloc
       module procedure deallocNetwork
      module procedure dealloc_1dadmin
    end interface dealloc
   

   ! !TODO JN: zorg voor allocatie en initialisatie. en vullen van lin2ibr en lin2local uit adm%lin. -1 is missing value e.g. for a 2d link, length LINALL
   
   type, public :: t_administration_1d
      integer, allocatable            :: lin2str(:)                        !< Indirection list, containing structure numbers for flowlinks.
                                                                           !< These structure numbers refer to the elements of network%sts%struct.
      integer, allocatable            :: lin2ibr(:)                        !< Indirection list, containing branch number on which the flow link is positioned 
      integer, allocatable            :: lin2local(:)                      !< Indirection list, containing relative index of link on branch adm%lin2ibr(l)
      
      integer, allocatable            :: lin2grid(:)
      type(t_chainage2cross), pointer :: line2cross(:,:) => null()         !< List containing cross section indices per flow link: (L,1) for gridpoint at 
                                                                           !< the start of the flow link, (L,2) for the flow link itself and (L,1) for gridpoint at 
                                                                           !< the end of the flow link.
      logical, allocatable            :: hysteresis_for_summerdike(:,:)    !< array indicating for hysteresis in summerdikes

   end type

   type, public   :: t_network
      integer                                   :: numk                    !< total number of links (internal and boundary)
      integer                                   :: numl                    !< total number of links (internal and boundary)
      logical                                   :: sferic                  !< flag indicating whether the used coordinate system is sferical or metric
      type(t_administration_1d)                 :: adm                     !< network administration
      type(t_nodeSet)                           :: nds                     !< set of nodes
      type(t_branchSet)                         :: brs                     !< set of branches
      type(t_CrossSectionSet)                   :: crs                     !< set of Cross-Sections
      type(t_StructureSet)                      :: sts                     !< structure list
      type(t_CompoundSet)                       :: cmps                    !< Administration compound structures
      type(t_RoughnessSet)                      :: rgs                     !< set containing roughness sections
      type(t_ObservationPointSet)               :: obs                     !< set of observation points
      type(t_storage_set)                       :: storS                   !< set containing storage in gridpoints
      type(t_CSDefinitionSet)                   :: CSDefinitions
      type(t_spatial_dataSet)                   :: spData
      type(t_ObservCrossSectionSet)             :: observcrs               !< set of observation Cross-Sections 
      type(t_forcingList)                       :: forcinglist             !< Work list of read-in (structure) forcing data, to be initialized by calling kernel later.
      logical                                   :: loaded      = .false.
      logical                                   :: initialized = .false.
   end type
   
contains

   subroutine realloc_1dadmin(adm, links_count_1d, links_count_all, gridp_count_1d)

      type(t_administration_1d)  :: adm
      integer, intent(in)        ::  links_count_1d  !< Maximum number of 1D flow links in model
      integer, intent(in)        ::  links_count_all !< Maximum number of 1D+2D flow links in model
      integer, intent(in)        ::  gridp_count_1d  !< Maximum number of 1D grid points in model
      
      if (.not. allocated(adm%lin2str))      allocate(adm%lin2str   (links_count_all))  
      if (.not. allocated(adm%lin2ibr))      allocate(adm%lin2ibr   (links_count_1d))   
      if (.not. allocated(adm%lin2local))    allocate(adm%lin2local (links_count_1d)) 
      if (.not. allocated(adm%lin2grid))     allocate(adm%lin2grid  (links_count_1d)) 
      if (.not. associated(adm%line2cross))  allocate(adm%line2cross(links_count_1d, 3))
      if (.not. allocated(adm%hysteresis_for_summerdike)) allocate(adm%hysteresis_for_summerdike(2,links_count_1d))
      adm%hysteresis_for_summerdike = .true.
      
   end subroutine realloc_1dadmin

   subroutine dealloc_1dadmin(adm)
      type(t_administration_1d)     :: adm

      if (allocated(adm%lin2str))      deallocate(adm%lin2str)
      if (allocated(adm%lin2ibr))      deallocate(adm%lin2ibr)
      if (allocated(adm%lin2local))    deallocate(adm%lin2local)
      if (associated(adm%line2cross))  deallocate(adm%line2cross)
      if (allocated(adm%lin2grid))    deallocate(adm%lin2grid)
      if (allocated(adm%hysteresis_for_summerdike)) deallocate(adm%hysteresis_for_summerdike)

   end subroutine dealloc_1dadmin


   subroutine deallocNetwork(network)
      ! Modules
      use messagehandling
   
      implicit none
      ! Input/output parameters
      type(t_network), intent(inout) :: network
   
      ! Local variables

      ! Program code
      call dealloc(network%adm)
      call dealloc(network%nds)
      call dealloc(network%brs)
      call dealloc(network%crs)
      call dealloc(network%sts)
      call dealloc(network%cmps)
      call dealloc(network%rgs)
      call dealloc(network%obs)
      call dealloc(network%storS)
      call dealloc(network%CSDefinitions)
      call dealloc(network%spData)
      call dealloc(network%observcrs)
      network%loaded = .false.
   
   end subroutine deallocNetwork

   subroutine admin_network(network, nlink)
      use m_node
      use m_branch
   
      type(t_network), intent(inout) :: network
      integer, intent(inout) :: nlink
   
      call admin_branch(network%brs, nlink)

   end subroutine admin_network


   subroutine initialize_1dadmin(network, linall_1d, linall_all)
   
      use m_CrossSections
      use m_GlobalParameters
      use Timers
      type(t_network), intent(inout), target :: network
      integer, intent(in)            :: linall_1d          !< Maximum number of 1D links, used for (re)allocation.
      integer, intent(in)            :: linall_all         !< Maximum number of all flow links, used for (re)allocation.
      
      integer :: is, k1, k2, L1, L2
      integer :: ilnk
      integer :: igpt
      integer :: ibran
      integer :: m
      integer :: icrs1
      integer :: icrs2
      integer :: L
      
      double precision                   :: f
      double precision                   :: chainage1
      double precision                   :: chainage2
      double precision                   :: chainage
      double precision                   :: chainageg
      type(t_administration_1d), pointer          :: adm
      type(t_branch), pointer            :: pbran
      type(t_CrossSection), pointer      :: C1, C2
      integer, allocatable, dimension(:) :: crossOrder
      integer, allocatable, dimension(:) :: lastAtBran
      integer                            :: icrsBeg
      integer                            :: icrsEnd
      double precision                   :: xBeg
      double precision                   :: xEnd
      integer                            :: i, j
      integer                            :: timerHandle
      logical                            :: interpolDone
      logical                            :: initError = .false.

      call realloc(network%adm, linall_1d, linall_all, linall_1d + network%brs%Count)

      timerHandle = 0
      call timstrt('line administration', timerHandle)
      adm => network%adm
      
      call update_lin2str_admin(network)

      adm%lin2ibr   = -huge(1)
      adm%lin2local = -huge(1)
      adm%lin2grid  = -huge(1)
      
      do ibran = 1, network%brs%Count
         pbran => network%brs%branch(ibran)
         do is = 1,pbran%gridPointsSeqCount
            k1 = pbran%k1gridPointsSeq(is)
            k2 = pbran%k2gridPointsSeq(is)
            L1 = k1 - (is-1) ! is-1 corrects for any "holes" in between the previous is-1 gridpointssequences.
            L2 = k2 - is
            do m=L1,L2 ! Loop over u-points
               if (pbran%lin(m) > 0 .and. pbran%lin(m) <= size(adm%lin2ibr)) then
                  ! Skip links not in this partition
                  adm%lin2ibr(pbran%lin(m)) = ibran
                  adm%lin2local(pbran%lin(m)) = m
                  adm%lin2grid(pbran%lin(m))  = pbran%grd(m + (is-1))
               endif
            enddo
         end do
      enddo
      call timstop(timerhandle)
      
      if (network%crs%Count > 0) then

         timerHandle = 0
         call timstrt('Apply branch orders', timerHandle)
         
         call useBranchOrders(network%crs, network%brs)
         call reIndexCrossSections(network%sts, network%crs) ! Update re-sorted cross sections for culverts and bridges

         call timstop(timerhandle)
         
         timerHandle = 0
         call timstrt('Sort cross sections', timerHandle)
         allocate(crossOrder(network%crs%Count))
         allocate(lastAtBran(network%brs%Count))
         
         call crossSectionsSort(network%crs, network%brs, crossOrder, lastAtBran)
         
         ! Fill in the Index for checking later
         do icrs1 = 1, network%crs%Count
            network%crs%cross(icrs1)%crossIndx = icrs1
         enddo
         call timstop(timerhandle)
         
         ! Cross-Section indices for links
         
         timerHandle = 0
         call timstrt('Calculate weighing table for cross section interpolation', timerHandle)
         do ibran = 1, network%brs%Count
            
            pbran   => network%brs%branch(ibran)
            
            if (ibran .eq. 1) then
               icrsBeg = 1
            else
               icrsBeg =lastAtBran(ibran - 1) + 1
            endif
            icrsEnd = lastAtBran(ibran)
            
            
            if (icrsbeg > icrsend) then
               
               call setmessage(LEVEL_WARN, 'No cross sections found on branch '//trim(pbran%id)//'. Using default rectangular cross section')
               do i = 1, pbran%uPointsCount
                  ilnk = pbran%lin(i)
                  adm%line2cross(ilnk, :)%c1 = -1
                  adm%line2cross(ilnk, :)%c2 = -1
                  adm%line2cross(ilnk, :)%f  = 1.0d0
                  adm%line2cross(ilnk, :)%distance  = 0d0
               enddo
               
               cycle
            endif
            
            icrs1 = icrsBeg
            icrs2 = icrsBeg
            
            xBeg = network%crs%cross(crossOrder(icrsBeg))%chainage
            xEnd = network%crs%cross(crossOrder(icrsEnd))%chainage
            
            ! loop over all grid points and upoints
            do is = 1,pbran%gridPointsSeqCount
               k1 = pbran%k1gridPointsSeq(is)
               k2 = pbran%k2gridPointsSeq(is)
               L1 = k1 - (is-1) ! is-1 corrects for any "holes" in between the previous is-1 gridpointssequences.
               L2 = k2 - is
               do m=L1,L2 ! Loop over u-points
                  do j = 1, 3
                     if (j==1) then
                        chainage = pbran%gridPointschainages(m + (is-1))
                     elseif (j==2) then
                        chainage = pbran%uPointschainages(m)
                     elseif (j==3) then
                        chainage = pbran%gridPointschainages(m+1 + (is-1))
                     endif
                     ilnk = pbran%lin(m)
                  
                     if (m > L1 .and. j==1) then
                        ! Just copy data from the previous value
                        adm%line2cross(ilnk, j)%c1 = adm%line2cross(pbran%lin(m-1), 3)%c1
                        adm%line2cross(ilnk, j)%c2 = adm%line2cross(pbran%lin(m-1), 3)%c2
                        adm%line2cross(ilnk, j)%f  = adm%line2cross(pbran%lin(m-1), 3)%f 
                        adm%line2cross(ilnk, j)%distance  = adm%line2cross(pbran%lin(m-1), j)%distance

                     elseif (icrsBeg == icrsEnd) then
                     
                        ! Just one Cross-Section
                        adm%line2cross(ilnk, j)%c1 = crossOrder(icrsBeg)
                        adm%line2cross(ilnk, j)%c2 = crossOrder(icrsBeg)
                        adm%line2cross(ilnk, j)%f  = 1.0d0
                        adm%line2cross(ilnk, j)%distance  = 0d0
                        interpolDone            = .true.
                     
                     elseif (chainage <= xBeg) then
                        
                        ! Before First Cross-Section
                        adm%line2cross(ilnk, j)%c1 = crossOrder(icrsBeg)
                        adm%line2cross(ilnk, j)%c2 = crossOrder(icrsBeg)
                        adm%line2cross(ilnk, j)%f  = 1.0d0
                        adm%line2cross(ilnk, j)%distance  = 0d0
                        interpolDone            = .true.
                     
                     elseif (chainage >= xEnd) then
                     
                        ! After Last Cross-Section
                        adm%line2cross(ilnk, j)%c1 = crossOrder(icrsEnd)
                        adm%line2cross(ilnk, j)%c2 = crossOrder(icrsEnd)
                        adm%line2cross(ilnk, j)%f  = 1.0d0
                        adm%line2cross(ilnk, j)%distance  = 0d0
                        interpolDone            = .true.
                     
                     else if (ilnk <= size(adm%line2cross)) then
                     
                        ! Skip links not in this partition
                        chainage1 = network%crs%cross(crossOrder(icrs1))%chainage
                        chainage2 = network%crs%cross(crossOrder(icrs2))%chainage
                        adm%line2cross(ilnk, 2)%distance  = chainage2 - chainage1
                     
                        if (.not. ((chainage1 <= chainage) .and. (chainage2 >= chainage))) then
                        
                           do i = icrs1, icrsEnd
                              if (network%crs%cross(crossOrder(i))%chainage >= chainage) then
                                 chainage2 = network%crs%cross(crossOrder(i))%chainage
                                 icrs2 = i
                                 exit
                              endif
                           enddo
                        
                           do i = icrsEnd, icrsBeg, -1
                              if (network%crs%cross(crossOrder(i))%chainage <= chainage) then
                                 chainage1 = network%crs%cross(crossOrder(i))%chainage
                                 icrs1 = i
                                 exit
                              endif
                           enddo
                        
                        endif
                        
                        interpolDone = .false.
                        
                     endif
                        
                        if (ilnk > 0 .and. ilnk <= size(adm%line2cross)) then
                           ! Skip links not in this partition
                        
                           if (ibran == network%crs%cross(crossOrder(icrs2))%branchid) then
                           
                              if (.not. interpolDone) then
                                 if (icrs1 == icrs2) then 
                                    adm%line2cross(ilnk, j)%c1 = crossOrder(icrs1)
                                    adm%line2cross(ilnk, j)%c2 = crossOrder(icrs2)
                                    f = 1.0d0
                                    adm%line2cross(ilnk, 2)%f = f
                                 else    
                                    adm%line2cross(ilnk, j)%c1 = crossOrder(icrs1)
                                    adm%line2cross(ilnk, j)%c2 = crossOrder(icrs2)
                                    f  = (chainage - chainage1) / (chainage2 - chainage1)
                                    f = max(f, 0.0d0) 
                                    f = min(f, 1.0d0) 
                                    adm%line2cross(ilnk, j)%f = f
                              endif 
                           endif
                     
                        else
                           adm%line2cross(ilnk, j)%c1 = crossOrder(icrs1)
                           adm%line2cross(ilnk, j)%c2 = crossOrder(icrs1)
                           adm%line2cross(ilnk, j)%f  = 1.0d0
                        endif
                     
                     endif
                  enddo
               enddo
            end do
         enddo
         call timstop(timerhandle)
         
         call resetMaxerrorLevel()
         do L = 1, linall_1d
            do i = 1,3
               if (adm%line2cross(L,i)%C1 > 0 .and. adm%line2cross(L,i)%C2 > 0) then
                  c1 => network%crs%cross(adm%line2cross(L,i)%C1)
                  c2 => network%crs%cross(adm%line2cross(L,i)%C2)
                  if (c1%crosstype /= c2%crosstype) then
                     call SetMessage(LEVEL_WARN, 'Incorrect CrossSection input for CrossSections '''//trim(c1%csid)//''', '''//trim(c2%csid)//''' on branch '''//trim(pbran%id)// &
                        '''. CrossSections must be of the same type!')
                  endif
                  if (c1%closed .neqv. c2%closed) then
                     call SetMessage(LEVEL_WARN, 'Incorrect CrossSection input for CrossSections '''//trim(c1%csid)//''', '''//trim(c2%csid)//''' on branch '''//trim(pbran%id)// &
                        '''. CrossSections must have same closed state!')
                  endif
               endif
            enddo
         enddo
         if (getMaxErrorLevel() > 0) then
            call SetMessage(LEVEL_FATAL, 'Incompatible CrossSections have been detected, see previous messages.')
         endif

         deallocate(crossOrder)
         deallocate(lastAtBran)
         
      endif

      if (initError) then 
         call setmessage(LEVEL_FATAL, 'Error initialising network')
      endif 
      
   end subroutine initialize_1dadmin

   subroutine reassign_pointers(network)

      use m_culvert
   
      type (t_network) :: network
   
      !type (t_branch), pointer :: pbr
      type(t_CrossSection), pointer :: pcs
      type(t_structure), pointer :: pstru
      type(t_culvert), pointer :: pcul
      integer i
   
      !do i = 1, network%brs%count
      !   pbr => network%brs%branch(i)
      !   pbr 
      !enddo
   
      do i = 1, network%crs%Count
         pcs => network%crs%cross(i)
         pcs%tabdef => network%CSDefinitions%cs(pcs%iTabDef)
      enddo
   
      do i = 1, network%sts%Count
         pstru => network%sts%struct(i)
         select case (pstru%type) 
         case (ST_CULVERT) 
            pcul => pstru%culvert
            pcul%pcross => network%crs%cross(pcul%crosssectionnr)
         end select
      enddo
   
   end subroutine reassign_pointers

   !> Updates the network%adm%lin2str administration after the full set
   !! of structures has been filled.
   subroutine update_lin2str_admin(network)
      type(t_network), intent(inout) :: network !< The overal network containing structures and administration.

      integer :: i
      type (t_structure), pointer :: pstru

      network%adm%lin2str = -huge(1)
      do i = 1, network%sts%count
         pstru => network%sts%struct(i)
         if (associated(pstru%linknumbers)) then
            network%adm%lin2str(abs(pstru%linknumbers(1:pstru%numlinks))) = i
         end if
      enddo
   end subroutine update_lin2str_admin


   subroutine set_network_pointers(network)
      ! Modules
   
      implicit none
      ! Input/output parameters
      type(t_network), intent(inout) :: network
   
      ! Local variables
      type(t_CrossSection), pointer :: pcross
      integer :: istru
      integer :: icrs
   
      ! Program code
      do istru = 1, network%sts%count
         if (network%sts%struct(istru)%type==ST_CULVERT) then
            icrs = network%sts%struct(istru)%culvert%crosssectionnr
            pcross => network%crs%cross(icrs)
            network%sts%struct(istru)%culvert%pcross => pcross
         endif
      enddo
   
   end subroutine set_network_pointers
   
   !> In this subroutine arrays crossorder and lastAtBran are filled \n
   !! crossorder contains the cross section indices, where the cross sections are ordered 
   !! in ascending branchid and subsequently in chainage.\n
   !! crossorder(lastAtBran(ibr-1)+1) .. crossorder(lastAtBran(ibr)) contain the cross
   !! sections on branch ibr, in ascending branch chainage.
   !! sections on branch ibr, in ascending branch chainage.
   subroutine crossSectionsSort(crs, brs, crossOrder, lastAtBran)

      ! Ordering crs's on branches and x on branches
   
      implicit none
      
      ! Global Variables
      type(t_CrossSectionSet), intent(in)       :: crs
      type(t_branchSet), intent(in)             :: brs
      integer, intent(out),dimension(:)         :: crossOrder
      integer, intent(out), dimension(:)        :: lastAtBran
   
      ! Local Variables
      logical                                   :: changed
      integer                                   :: icrsn
      integer                                   :: ihulp
      integer                                   :: ilast
      integer                                   :: ibran
      integer                                   :: ifirst
      integer                                   :: crossCount
      integer                                   :: i

      do icrsn = 1, crs%count
          crossOrder(icrsn) = icrsn
      enddo

       ! Sort available crsn's in crossOrder on branch number
      crossCount = crs%count
      do i = crs%count, 1, -1
         
         if (.not. crs%cross(i)%branchid > 0) then
            
            ihulp                  = crossOrder(i)
            crossOrder(i)          = crossOrder(crossCount)
            crossOrder(crossCount) = ihulp
            crossCount             = crossCount - 1
            
         endif
         
      enddo

      changed = .true.
      
      do while (changed)
         
         changed = .false.
         
         do icrsn = 1, crossCount - 1
            
            if (crs%cross(crossOrder(icrsn))%branchid .gt. &
                crs%cross(crossOrder(icrsn + 1))%branchid) then
               
               ihulp                 = crossOrder(icrsn)
               crossOrder(icrsn)     = crossOrder(icrsn + 1)
               crossOrder(icrsn + 1) = ihulp
               changed               = .true.
               
            endif
                
         enddo
         
      enddo

!     fill temporal administration array lastAtBran
!     last index in crossOrder for branch(i)

      do ibran=1, brs%count !nbran
         lastAtBran(ibran) = 0
      enddo

      if (crossCount .gt. 0) then
         
         ilast = crs%cross(crossOrder(1))%branchid
         
         do icrsn = 2, crossCount
            if (ilast .ne. crs%cross(crossOrder(icrsn))%branchid ) then
              lastAtBran(ilast) = icrsn - 1
           endif
           ilast = crs%cross(crossOrder(icrsn))%branchid
         enddo
         
         lastAtBran(ilast) = crossCount
         
      endif

!     sort available crsn's in crossOrder on distance in branch

      do ibran = 1, brs%count !nbran

         ! Find range for sorting procedure per branch

         if ( ibran .eq. 1 ) then
            ifirst = 1
         else
            if (lastAtBran(ibran - 1) == 0) then
               lastAtBran(ibran-1) = lastAtBran(max(1,ibran-2))
            endif
            ifirst = lastAtBran(ibran - 1) + 1
         endif
         
         ilast = lastAtBran(ibran)

         ! Sorting procedure per branch

         changed = .true.
          
         do while (changed)
             
            changed = .false.
              
            do icrsn = ifirst, ilast - 1
                 
               if (crs%cross(crossOrder(icrsn))%chainage .gt.  &
                   crs%cross(crossOrder(icrsn + 1))%chainage) then
                  
                  ihulp               = crossOrder(icrsn)
                  crossOrder(icrsn)   = crossOrder(icrsn+1)
                  crossOrder(icrsn+1) = ihulp
                  changed             = .true.
                  
               endif
                     
            enddo
            
         enddo

      enddo

   end subroutine crossSectionsSort
   
   function getStorageNodeId(network, gridpoint) result(id)
   
      use m_Storage
      use m_node
   
      character(len=80)              :: id
      type(t_network), intent(in)    :: network
      integer, intent(in)            :: gridpoint
      
      integer :: i
      type(t_storage), pointer, dimension(:) :: stor
      
      stor => network%stors%stor
      
      do i = 1, network%stors%count
         if (stor(i)%grid_point == gridpoint) then
            id = stor(i)%id
            return
         endif
      enddo

      id = 'No storage area assigned to node '//getnodeid(network%nds, gridpoint)
      
   end function getStorageNodeId


!> Get friction value (Chezy) for a specific point in the network
!> This function is moved from Roughness.f90 to Network.f90 to avoid circular references
double precision function getFrictionValue(rgs, spdata, cross, ibranch, section, igrid, h, q, u, r, d, chainage)

use m_tables
use m_tablematrices
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !=======================================================================
    !                       Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:
    !
    ! Function:           getFrictionValue, replacement of old FLCHZT (FLow CHeZy Friction coeff)
    !
    ! Module description: Chezy coefficient is computed for a certain gridpoint
    !
    !
    !
    !     update information
    !     person                    date
    !     Kuipers                   5-9-2001
    !     Van Putten                11-8-2011
    !
    !     Use stored table counters
    !
    !
    !
    !
    !     Declaration of Parameters:
    !

    implicit none
!
! Global variables
!
    type t_parr
       type(t_CSType), pointer        :: cross
    end type
    

    type(t_RoughnessSet), intent(in)        :: rgs         !< Roughness data
    type(t_spatial_dataSet), intent(in)     :: spData      !< spatial data
    type(t_CSType), pointer, intent(in)     :: cross
    integer, intent(in)                     :: igrid       !< gridpoint index
    integer, intent(in)                     :: ibranch     !< branch index
    integer, intent(in)                     :: section     !< section index (0=main, 1=Flood plane 1, 2=Flood plane 2)
    double precision, intent(in)            :: d           !< water depth
    double precision, intent(in)            :: h           !< water level
    double precision, intent(in)            :: q           !< discharge
    double precision, intent(in)            :: r           !< hydraulic radius
    double precision, intent(in)            :: u           !< velocity
    double precision, intent(in)            :: chainage    !< chainage (location on branch)
    
!
!
! Local variables
!
    integer                         :: isec, i
    double precision                :: cpar
    double precision                :: cz
    double precision                :: dep
    double precision                :: ys
    double precision                :: rad
    logical                         :: checkDirectRoughness
    type(t_Roughness), pointer      :: rgh 
    type(t_spatial_data), pointer   :: values
    integer, dimension(:), pointer  :: rgh_type
    integer, dimension(:), pointer  :: fun_type

    !     Explanation:
    !     -----------
    !
    !     1. Each Chezy formula, apart from Engelund bed friction, is defined
    !        by 1 constant parameter. This constant is stored in bfricp.
    !        An exception is the Engelund bed friction defined by 10 parameters.
    !     2. For the Engelund bed friction the specific parameters are stored
    !        in the array engpar.
    !
    !
    !     Prevention against zero hydraulic radius and depth
    !
    rad = max(r, 1.d-6)
    dep = max(d, 1.d-6)
    !
    if (associated(cross)) then
       ! section refers to the roughness section *within* the cross section. Actual friction section index then comes from lookup.
       if (cross%frictionSectionsCount > 0) then ! FB: this is also true if there is no friction section index as count is always >= 1
         isec = cross%frictionsectionIndex(min(cross%frictionSectionsCount, section))
       else
         isec = section
       endif
    else
       ! No cross section definition: section directly refers to a friction section index.
       isec = section
    endif
    
    if (rgs%version == 1) then
      rgh => rgs%rough(isec)
      values    => spData%quant(rgh%spd_pos_idx)
      rgh_type  => rgh%rgh_type_pos 
      fun_type  => rgh%fun_type_pos 
       if (fun_type(ibranch) == R_FunctionDischarge) then
          cpar = interpolate(values%tables%tb(values%tblIndex(igrid))%table,  abs(q))
       !
       !        Roughness function of water level depending on flow direction
       !
       elseif (fun_type(ibranch) == R_FunctionLevel) then
          cpar = interpolate(values%tables%tb(values%tblIndex(igrid))%table,  h)
       !
       !        Roughness constant depending on flow direction
       !
       else
          cpar = values%values(igrid)
       endif
       getFrictionValue = GetChezy(rgh_type(ibranch), cpar, rad, dep, u)    !
    !     Formulation = .not. Engelund
    !
    else ! Version 2 roughness
       do i = 1, 2
          checkDirectRoughness = .false.
          if (associated(cross)) checkDirectRoughness = (cross%frictionSectionID(cross%frictionSectionsCount) == '')
          if (checkDirectRoughness) then
             ! Current cross section does *not* refer to a friction section index, but has defined direct roughness type+coefficient.
             cz = GetChezy(cross%frictionType(section), cross%frictionValue(section), rad, dep, u)
          else
             rgh => rgs%rough(isec)
             if (rgh%useGlobalFriction)then
                cz = GetChezy(rgh%frictionType, rgh%frictionValue, rad, dep, u)
             elseif (rgh%timeSeriesIndexes(ibranch) > 0 ) then
                ! time dependent roughness
                rgh_type  => rgh%rgh_type_pos 
                cpar = rgh%currentValues(rgh%timeSeriesIndexes(ibranch))
                cz = GetChezy(rgh_type(ibranch), cpar, rad, dep, u)
             else
                ! For now, direction independent, always *_pos values.
                rgh_type  => rgh%rgh_type_pos 
                fun_type  => rgh%fun_type_pos 
                if (rgh_type(ibranch) == -1)  then ! This branch has no own roughness definition, use global.
                   cz = GetChezy(rgh%frictionType, rgh%frictionValue, rad, dep, u)
                else
                   if (fun_type(ibranch) == R_FunctionDischarge) then
                      ys = abs(q)
                   elseif (fun_type(ibranch) == R_FunctionLevel) then
                      ys = h
                   else
                      ys = 0d0
                   endif
       
                   cpar = interpolate(rgh%table(ibranch), chainage, ys)
                   cz = GetChezy(rgh_type(ibranch), cpar, rad, dep, u)
                endif
             endif
          endif
       enddo
       getFrictionValue = cz
    endif
    
    
end function getFrictionValue

!> Get friction paramter value for a specific point in the network
!> This function is moved from Roughness.f90 to Network.f90 to avoid circular references
double precision function getFrictionCparValue(rgs, spdata, cross, ibranch, section, igrid, h, q, u, r, d, chainage)

use m_tables
use m_tablematrices
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !=======================================================================
    !                       Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:
    !
    ! Function:           getFrictionValue, replacement of old FLCHZT (FLow CHeZy Friction coeff)
    !
    ! Module description: Chezy coefficient is computed for a certain gridpoint
    !
    !
    !
    !     update information
    !     person                    date
    !     Kuipers                   5-9-2001
    !     Van Putten                11-8-2011
    !
    !     Use stored table counters
    !
    !
    !
    !
    !     Declaration of Parameters:
    !

    implicit none
!
! Global variables
!
    type t_parr
       type(t_CSType), pointer        :: cross
    end type
    

    type(t_RoughnessSet), intent(in)        :: rgs         !< Roughness data
    type(t_spatial_dataSet), intent(in)     :: spData      !< spatial data
    type(t_CSType), pointer, intent(in)     :: cross
    integer, intent(in)                     :: igrid       !< gridpoint index
    integer, intent(in)                     :: ibranch     !< branch index
    integer, intent(in)                     :: section     !< section index (0=main, 1=Flood plane 1, 2=Flood plane 2)
    double precision, intent(in)            :: d           !< water depth
    double precision, intent(in)            :: h           !< water level
    double precision, intent(in)            :: q           !< discharge
    double precision, intent(in)            :: r           !< hydraulic radius
    double precision, intent(in)            :: u           !< velocity
    double precision, intent(in)            :: chainage    !< chainage (location on branch)
    
!
!
! Local variables
!
    integer                         :: isec, i
    double precision                :: cpar
    double precision                :: cz
    double precision                :: dep
    double precision                :: ys
    double precision                :: rad
    type(t_Roughness), pointer      :: rgh 
    type(t_spatial_data), pointer   :: values
    integer, dimension(:), pointer  :: rgh_type
    integer, dimension(:), pointer  :: fun_type

    !     Explanation:
    !     -----------
    !
    !     1. Each Chezy formula, apart from Engelund bed friction, is defined
    !        by 1 constant parameter. This constant is stored in bfricp.
    !        An exception is the Engelund bed friction defined by 10 parameters.
    !     2. For the Engelund bed friction the specific parameters are stored
    !        in the array engpar.
    !
    !
    !     Prevention against zero hydraulic radius and depth
    !
    rad = max(r, 1.d-6)
    dep = max(d, 1.d-6)
    !
    if (associated(cross)) then
       ! section refers to the roughness section *within* the cross section. Actual friction section index then comes from lookup.
       if (cross%frictionSectionsCount > 0) then
         isec = cross%frictionsectionIndex(min(cross%frictionSectionsCount, section))
       else
          isec = section
       endif
    else
       ! No cross section definition: section directly refers to a friction section index.
       isec = section
    endif
    
    if (rgs%version == 1) then
      rgh => rgs%rough(isec)
      values    => spData%quant(rgh%spd_pos_idx)
      rgh_type  => rgh%rgh_type_pos 
      fun_type  => rgh%fun_type_pos 
       if (fun_type(ibranch) == R_FunctionDischarge) then
          cpar = interpolate(values%tables%tb(values%tblIndex(igrid))%table,  abs(q))
       !
       !        Roughness function of water level depending on flow direction
       !
       elseif (fun_type(ibranch) == R_FunctionLevel) then
          cpar = interpolate(values%tables%tb(values%tblIndex(igrid))%table,  h)
       !
       !        Roughness constant depending on flow direction
       !
       else
          cpar = values%values(igrid)
       endif
    !     Formulation = .not. Engelund
    !
    else ! Version 2 roughness
       do i = 1, 2
          if (isec < 0) then
             ! Current cross section does *not* refer to a friction section index, but has defined direct roughness type+coefficient.
             cz = GetChezy(cross%frictionType(section), cross%frictionValue(section), rad, dep, u)
          else
             rgh => rgs%rough(isec)
             if (rgh%useGlobalFriction)then
                cz = GetChezy(rgh%frictionType, rgh%frictionValue, rad, dep, u)
             else
                ! For now, direction independent, always *_pos values.
                rgh_type  => rgh%rgh_type_pos 
                fun_type  => rgh%fun_type_pos 
                if (rgh_type(ibranch) == -1)  then ! This branch has no own roughness definition, use global.
                   cz = GetChezy(rgh%frictionType, rgh%frictionValue, rad, dep, u)
                else
                   if (fun_type(ibranch) == R_FunctionDischarge) then
                      ys = abs(q)
                   elseif (fun_type(ibranch) == R_FunctionLevel) then
                      ys = h
                   else
                      ys = 0d0
                   endif
       
                   cpar = interpolate(rgh%table(ibranch), chainage, ys)
                endif
             endif
          endif
       enddo
    endif
    
    getFrictionCparValue = cpar
    
end function getFrictionCparValue

subroutine getRoughnessForProfile(network, crs)
   use precision_basics
   use m_hash_search

   type(t_network),        intent(inout) :: network      !< Network structure
   type(t_CrossSection),   intent(inout) :: crs          !< cross section
   
   integer                        :: i
   integer                        :: iRough
   type(t_Roughness), pointer     :: pRgs
   type(t_spatial_data), pointer  :: pSpData
   double precision               :: frictionValue
   integer                        :: frictionType
   integer                        :: iStatus
   if (crs%frictionSectionsCount <= 0) then
      call SetMessage(LEVEL_ERROR, 'No Friction Section Data for Cross-Section ID: '//trim(crs%csid))
      return
   endif
        
   do i = 1, crs%frictionSectionsCount
        
      iRough = hashsearch(network%rgs%hashlist, crs%frictionSectionID(i))
      if (iRough <= 0) then
         call SetMessage(LEVEL_FATAL, 'No Data found for Section '//trim(crs%frictionSectionID(i))//' of Cross-Section ID: '//trim(crs%csid))
         cycle
      endif
      
      pRgs => network%rgs%rough(iRough)

      if (network%rgs%version == network%rgs%roughnessFileMajorVersion) then
         frictionValue = crs%frictionValuePos(i)
         call getFrictionParameters(pRgs,  crs%branchid, crs%chainage, crs%frictionTypePos(i), crs%frictionValuePos(i))
         if (comparereal(frictionValue, crs%frictionValuePos(i)) /= 0) then
            crs%hasTimeDependentConveyance = .true.
         endif
         frictionValue = crs%frictionValueNeg(i)
         call getFrictionParameters(pRgs, crs%branchid, crs%chainage, crs%frictionTypeNeg(i), crs%frictionValueNeg(i))
         if (comparereal(frictionValue, crs%frictionValueNeg(i)) /= 0) then
               crs%hasTimeDependentConveyance = .true.
         endif
         cycle
      endif
      
      if (pRgs%iSection == 0) then
         ! roughness section does not exist
         call setMessage(LEVEL_ERROR, 'Roughness section '// trim(crs%frictionSectionID(i)) //', used in '//trim(crs%csid)//' does not exist')
         cycle
      endif
         
      
      crs%frictionTypePos(i) = pRgs%rgh_type_pos(crs%branchid)
      
      if (pRgs%spd_pos_idx <= 0) then
         call SetMessage(LEVEL_ERROR, 'No Spatial Data specified for Section '//trim(crs%frictionSectionID(i))//' of Cross-Section ID: '//trim(crs%csid))
         cycle
      endif
      
      ! Positive direction
      if (pRgs%spd_pos_idx > 0) then
      
         pSpData => network%spData%quant(pRgs%spd_pos_idx)
         
         iStatus = getValueAtLocation(pSpData, crs%branchid, crs%chainage, frictionValue, frictionType)
         
         if (istatus >= 0) crs%frictionValuePos(i) = frictionValue
         if (istatus > 0)  crs%frictionTypePos(i)  = frictionType

      endif
        
   enddo
      
      
end subroutine getRoughnessForProfile

!> Remove already removed links (administered in LC-array) from the branch administration
subroutine update_flow1d_admin(network, lc)
   use m_branch
   use messageHandling
   
   type(t_network),       intent(inout)         :: network      !< network data structure
   integer, dimension(:), intent(in   )         :: lc           !< contains the links to be removed from branch administration
   
   integer                 :: numbranch
   integer                 :: lnew
   integer                 :: ibr
   type(t_branch), pointer :: pbr
   integer                 :: LL
   integer                 :: LL_new
   integer                 :: upointscount
   integer                 :: Ltoberemoved_index
   logical                 :: errorsfound
   
   numbranch = network%brs%Count
   errorsfound = .false.
   Lnew = 0
   Ltoberemoved_index = 1
   do ibr = 1, numbranch
      pbr => network%brs%branch(ibr)
      upointscount = pbr%uPointsCount
      LL_new = 0
      pbr%gridPointsChainages(1) = pbr%gridPointsChainages(1)
      pbr%gridPointIDs(1)        = pbr%gridPointIDs(1)       
      pbr%Xs(1)                  = pbr%Xs(1)                 
      pbr%Ys(1)                  = pbr%Ys(1)                 
      pbr%grd(1)                 = pbr%grd(1)                
      ! TODO: is this code already safe for gridpointssequences?
      do LL = 1, upointscount
         if (pbr%lin(LL)==LC(Ltoberemoved_index) ) then
            Ltoberemoved_index = Ltoberemoved_index + 1
            
            ! this link was removed
            if (pbr%upointscount == 1) then
               ! this link cannot be removed. 
               msgbuf = 'All flow links on branch '''//trim(pbr%id)//''' are removed. Please check your input'
               call warn_flush()
               errorsfound = .true.
            else
               write(msgbuf, '(a, a, a, f10.2, a )')'Flow link on branch ''', trim(pbr%id), ''' at chainage ', pbr%uPointsChainages(LL), ' is removed.'
               call msg_flush()

               pbr%uPointsCount = pbr%uPointsCount -1
               pbr%gridPointsCount = pbr%gridPointsCount - 1
            endif
         else
            LL_new = LL_new + 1
            Lnew   = Lnew + 1 
            pbr%gridPointsChainages(LL_new) = pbr%gridPointsChainages(LL)
            pbr%gridPointIDs(LL_new+1)        = pbr%gridPointIDs(LL+1)       
            pbr%Xs(LL_new+1)                  = pbr%Xs(LL+1)                 
            pbr%Ys(LL_new+1)                  = pbr%Ys(LL+1)                 
            pbr%uPointsChainages(LL_new)      = pbr%uPointsChainages(LL)   
            pbr%lin(LL_new)                   = Lnew               
            pbr%grd(LL_new+1)                 = pbr%grd(LL+1)                
            ! TODO: %grd_input
         endif
      enddo
   enddo
   if (errorsfound) then
      msgbuf = 'Errors found, check previous warnings'
      call err_flush()
   endif
   
end subroutine update_flow1d_admin
end module m_network
