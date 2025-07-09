module m_node
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
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
   use m_hash_search
   
   implicit none
   
   private 
     
   interface getnodeid
      module procedure getnodeid_fun
   end interface

   ! node types
   integer, public, parameter :: nt_NotSet    = -1
   integer, public, parameter :: nt_EndNode   = 0
   integer, public, parameter :: nt_LinkNode  = 1
  
   public realloc
   public dealloc
   public fill_hashtable
   public getnodeid

   interface fill_hashtable
      module procedure fill_hashtable_nds
   end interface 
   
   interface realloc
      module procedure reallocnode
   end interface

   interface dealloc
      module procedure deallocnode
   end interface dealloc

   type, public :: t_node                                !< Node information
    
      character(IdLen)            :: id                  !< nodeid(nnode): id of node
      character(IdLen)            :: name                !< nodeid(nnode): id of node
      integer                     :: index
      integer                     :: nodeType            !> node(1,:), possible values
                                                         !! - -1    not set
                                                         !! -  0    node with one reach connected
                                                         !! -  1    connection node with more than one reach connected
               
      double precision            :: x                   !< x-coordinate of node.
      double precision            :: y                   !< x-coordinate of node.
      integer                     :: gridNumber          !< node(2,:) gridpoint number
      integer                     :: numberOfConnections !< number of connections

   end type
   
   type, public :: t_nodeSet
      integer                                      :: maxNumberOfConnections=0    ! maximum nr of connections to a node
      integer                                      :: Size = 0
      integer                                      :: growsBy = 2000
      integer                                      :: Count= 0
      integer                                      :: LevelBoundaryCount= 0
      integer                                      :: DisBoundaryCount= 0
      integer                                      :: bndCount = 0
      type(t_node), pointer, dimension(:)          :: node
      type(t_hashlist)                             :: hashlist
   end type t_nodeSet
   
contains
    
   subroutine deallocnode(nds)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_nodeSet), intent(inout)          :: nds
      
      ! Local variables
   
      ! Program code
      if (associated(nds%node)) deallocate(nds%node)
      nds%node => null()
      call dealloc(nds%hashlist)
      nds%Size  = 0
      nds%growsBy = 200
      nds%LevelBoundaryCount= 0
      nds%DisBoundaryCount= 0      
      nds%Count = 0
   end subroutine
!
!
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
   

   subroutine fill_hashtable_nds(nds)
   
      type (t_nodeSet), intent(inout), target :: nds
      
      integer ind
      character(len=idlen), dimension(:), pointer :: ins
      
      allocate(nds%hashlist%id_list(nds%Count))
      nds%hashlist%id_count = nds%Count
      ins => nds%hashlist%id_list
      
      do ind= 1, nds%count
         ins(ind) = nds%node(ind)%id
      enddo
      
      call hashfill(nds%hashlist)
      
   end subroutine fill_hashtable_nds
   
   function getnodeId_fun(nds, gridpoint) result(id)
   
      character(len=80)    :: id
      type(t_nodeset), intent(in)      :: nds
      integer        , intent(in)      :: gridpoint
      
      integer i
      
      do i = 1, nds%count
         if (nds%node(i)%gridNumber == gridpoint) then
            id  = nds%node(i)%id
            return
         endif
         
      enddo
      
      id = 'NODEID not found'
   end function getnodeId_fun
   
end module m_node