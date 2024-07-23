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

 ! Last changed
! by:               $Author::  Wilms           $
! at:               $Modtime:: 07-10-2019      $
!
! current revision: $Revision:: 9              $



module m_1d_networkreader_rr


    !uses:

    use ReadLib
    use globals
    use TREE_DATA_TYPES
    use TREE_STRUCTURES
    use properties
    use m_hash_search_rr
    use m_node
    use m_branch
    use m_network
    use CONF_ARR

    implicit none
    private::prop_get



    contains



    subroutine lowercase_copy(string    ,lenstr    )
        implicit none
        !
        ! Global variables
        !
        integer     , intent(in) :: lenstr
        character(*)             :: string
        !
        ! Local variables
        !
        integer :: i
        integer :: j
        integer :: newlen
        !
        !! executable statements -------------------------------------------------------
        !
        newlen = min(lenstr, len(string))
        do i = 1, newlen
           j = ichar(string(i:i))
           if ((j>64) .and. (j<91)) then
              j = j + 32
              string(i:i) = char(j)
           endif
        enddo
    end subroutine lowercase_copy

    subroutine reallocnode(nds)
        ! Modules

        implicit none

        ! Input/output parameters
        type(t_nodeSet), intent(inout)          :: nds

        ! Local variables
        type(t_node), pointer, dimension(:)     :: oldnds

        ! Program code

        if (nds%Size > 0) then
            oldnds=>nds%node
        else
            nds%bndCount = 0
        endif

        if (nds%growsBy <=0) then
            nds%growsBy = 200
        endif
        allocate(nds%node(nds%Size+nds%growsBy))

        if (nds%Size > 0) then
            nds%node(1:nds%Size) = oldnds(1:nds%Size)
            deallocate(oldnds)
        endif
        nds%Size = nds%Size+nds%growsBy
    end subroutine




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
        integer                                :: nodeType
        double precision                       :: x
        double precision                       :: y
        logical                                :: success


        call  prop_get_string(md_ptr, 'node', 'id', nodeId, success)

        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Node ID')
        endif

        call prop_get_string(md_ptr, 'node', 'name', nodeName, success)
        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Node name')
        endif


        call prop_get_integer(md_ptr, 'node', 'type', nodeType, success)
        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Node type')
        endif


        call prop_get_double(md_ptr, 'node', 'x', x, success)
        if (success) call prop_get_double(md_ptr, 'node', 'y', y, success)

        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Node '''//trim(nodeId)//'''')
        endif

        nds%Count = nds%Count+1


        if (nds%Count > nds%Size) then
            call reallocnode(nds)
        endif

        nds%node(nds%Count)%id       = nodeId


        nds%node(nds%Count)%name     = nodeName

        nds%node(nds%Count)%index    = nds%count
        nds%node(nds%Count)%nodetype = nodeType
        nds%node(nds%Count)%numberOfConnections = 0
        nds%node(nds%Count)%x = x
        nds%node(nds%Count)%y = y

    end subroutine readNode

    !> Reallocate given branch
    subroutine reallocbranch(brs)
        ! Modules

        implicit none

        ! Input/output parameters
        type(t_branchSet), intent(inout)          :: brs !< Current branch set

        ! Local variables
        type(t_branch), pointer, dimension(:)     :: oldBrs



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


    subroutine readBranch(brs, nds, md_ptr, checkBranchExists)

        !use m_branch

        implicit none
        double precision                 :: minSectionLength              = 1.0
        type(t_branchSet), target, intent(inout) :: brs
        type(t_nodeSet), target, intent(inout)   :: nds
        type(tree_data), pointer, intent(in)     :: md_ptr
        logical, intent(in)                     :: checkBranchExists

        ! Local Variables
        integer                                  :: ibr
        type(t_branch), pointer                  :: pbr
        type(t_node), pointer                    :: node
        logical                                  :: success
        integer                                  :: istat
        integer                                  :: ibegNode
        integer                                  :: iendNode
        integer                                  :: igr
        integer                                  :: gridIndex
        integer                                  :: j
        integer                                  :: ip1
        integer                                  :: ip2
        integer                                  :: branchType
        character(len=IdLen)                     :: branchId
        character(len=IdLen)                     :: branchName
        character(len=IdLen)                     :: begNodeId
        character(len=IdLen)                     :: endNodeId
        character(len=IdLen)                     :: Chainage

        double precision, allocatable, dimension(:)     :: gpX
        double precision, allocatable, dimension(:)     :: gpY
        double precision, allocatable, dimension(:)     :: gpOffsets
        character(len=IdLen), allocatable, dimension(:) :: gpID

        brs%Count = brs%Count + 1
        ibr = brs%Count
        if (brs%Count > brs%Size) then
            call reallocbranch(brs)
        endif

        pbr =>brs%branch(brs%Count)

        call  prop_get_string(md_ptr, 'branch', 'id', branchId, success)
        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Branch ID')
        endif

        call  prop_get_string(md_ptr, 'branch', 'name', branchName, success)
        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Branch ID')
        endif

        call  prop_get_integer(md_ptr, 'branch', 'brType', branchType, success)
        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Branch branch type')
        endif
        
        call  prop_get_string(md_ptr, 'branch', 'fromnode', begNodeId, success)
        
       
        if (success) call  prop_get_string(md_ptr, 'branch', 'tonode', endNodeId, success)

        if (.not. success) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Branch '''//trim(branchId)//'''')
        endif
        
        
        if (checkBranchExists) then
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
       
            
            pbr%FromNode                     => nds%node(ibegNode)
            pbr%ToNode                       => nds%node(iendNode)
          
            
        endif
     

        pbr%id                           = branchID
        pbr%index                        = ibr
        pbr%name                         = branchName
        

        pbr%FromNode2                     = trim(begNodeId)
        pbr%toNode2                     =  trim(endNodeId)

        pbr%brType                       = branchType
        pbr%nextBranch                   = -1

    end subroutine readBranch
       



    subroutine NetworkReader(network, networkFile, checkBranch)
        use m_hash_search

        implicit none

        type(t_network), target, intent(inout) :: network
        character(len=*), intent(in) :: networkFile
        logical, intent(in) :: checkBranch

        type(tree_data), pointer  :: md_ptr
        integer :: istat
        integer :: maxlenpar=10000
        integer :: numstr
        integer :: i
        Character(1000) ::inifile_01
        
        
      
        call tree_create(trim(networkFile), md_ptr, maxlenpar)
      
        call prop_inifile(trim(networkFile), md_ptr, istat)
        
      

        numstr = 0
        if (associated(md_ptr%child_nodes)) then
             numstr = size(md_ptr%child_nodes)
        end if
        
       

        ! Get the Nodes First
        do i = 1, numstr

            if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'node') then
                call readNode(network%nds, md_ptr%child_nodes(i)%node_ptr)
            endif
        enddo
       
       

        call fill_hashtable(network%nds)
       
       
        ! Get the Branches
        do i = 1, numstr
            if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'branch') then
                call readBranch(network%brs, network%nds, md_ptr%child_nodes(i)%node_ptr, checkBranch)
            endif

        enddo

        call adminBranchOrders(network%brs)
        call fill_hashtable(network%brs)
        call tree_destroy(md_ptr)
        
    end subroutine NetworkReader
    


end module m_1d_networkreader_rr


