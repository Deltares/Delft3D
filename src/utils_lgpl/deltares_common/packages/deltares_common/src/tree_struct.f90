!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
! tree_struct.f90 --
!    Module that implements a general tree structure in Fortran 90
!
! General information:
!    The tree is stored via a derived type TREE_DATA. A tree can
!    have an arbitrary number of nodes, each of which can again
!    have an arbitrary number of subnodes and so on.
!    The data type for a node is the same as for a tree - there
!    is no difference, except that the routine tree_create()
!    returns the initialised root of a new tree.
!    A node (and the root as well) can have the following
!    properties:
!    - A name (at most 80 characters)
!    - Arbitrary data
!    - A string indicating the type of data
!    - Zero, one or more subnodes
!    The storage is fairly efficient (via the TRANSFER() function
!    all data and strings are converted to arrays of default
!    integers). A node does not "know" its parent though and
!    there is no check on circularity.
!
module TREE_DATA_TYPES
   type TREE_DATA
      character(len=1), dimension(:), pointer         :: node_name => null()
      character(len=1), dimension(:), pointer         :: node_data => null()
      character(len=1), dimension(:), pointer         :: node_data_type => null()
      integer                                         :: node_visit     !< Zeroed upon construction, incremented upon node_data request (properties.f90: prop_get_string)
      type(TREE_DATA_PTR), dimension(:), pointer :: child_nodes
      type(TREE_DATA),                   pointer :: bf_next_node => null() ! Breadth-first next node (same level)
   end type

   type TREE_DATA_PTR
      type(TREE_DATA), pointer               :: node_ptr
   end type
end module

module TREE_STRUCTURES
   use TREE_DATA_TYPES
   use string_module
   implicit none

   private
   !
   ! A variable that indicates the type of all data
   !
   character(len=1), dimension(1:1), public,save :: node_value

   !
   ! Auxiliary variable
   !

   integer, public, save                              :: maxlen = 300          ! 300 default value
   integer,save                                       :: traverse_level = 0
   ! character(len=1), allocatable, public, save      :: node_value_helper(:)  ! flow_io variable
   !
   ! Public routines, types and parameters
   !
   public  :: TREE_DATA
   public  :: tree_create, tree_create_node, tree_add_node, tree_get_node_by_name, tree_num_nodes, &
              tree_count_nodes_byname, tree_disconnect_node,                                       &
              tree_get_data_ptr, tree_put_data, tree_get_name, tree_get_data,                      &
              tree_get_datatype, tree_get_data_string,                                             &
              tree_traverse, tree_traverse_level, print_tree,                                      &
              tree_fold, tree_destroy, tree_get_data_alloc_string, tree_remove_child_by_name
   ! nested function has to be public for gfortran
   public ::  dealloc_tree_data

contains

! tree_create --
!    Create a new tree
!
! Arguments:
!    name         Name of the new tree
!    tree         Pointer to the new tree
! Result:
!    The argument tree points to a new, empty tree structure or is
!    not associated
!
subroutine tree_create( name, tree, maxlenpar )
   character(len=*), intent(in)    :: name
   type(TREE_DATA), pointer        :: tree

   integer                         :: error
   integer                         :: newsize
   integer, optional               :: maxlenpar

   if (present(maxlenpar)) maxlen  = maxlenpar
!   GD: memory leak here
!   if(associated(tree)) then
!     deallocate(tree)
!   end if

   allocate( tree, stat = error )

   if ( error /= 0 ) then
      nullify( tree )
   else
      newsize = size( transfer( name, node_value ) )
      !GD: memory leak here
      !if(associated(tree%node_name)) deallocate(tree%node_name)
      allocate( tree%node_name(1:newsize), stat = error )
      if ( error /= 0 ) then
         deallocate( tree )
         return
      else
         tree%node_name(1:newsize) = transfer( name, node_value )
         tree%node_visit = 0
         nullify( tree%node_data )
         nullify( tree%node_data_type )
         nullify( tree%child_nodes )
      endif
   endif
end subroutine tree_create

! tree_create_node --
!    Create a new node to the given tree or node
!
! Arguments:
!    tree         The tree or node to which to append the new node
!    name         Name of the new node
!    node         Pointer to the new node
! Result:
!    The argument node points to a new, empty node or is
!    not associated
!
subroutine tree_create_node( tree, name, node )
   character(len=*), intent(in)    :: name
   type(TREE_DATA), pointer        :: tree
   type(TREE_DATA), pointer        :: node

   integer :: ierror

   !
   ! Check for uniqueness
   !
!   call tree_get_node_by_name( tree, name, node )
!   if ( associated( node ) ) then
!      return
!   endif

   !
   ! Create a new node, store it in the array of child nodes
   ! for this (sub)tree
   !
   call tree_create( name, node )

   if ( associated( node ) ) then
      call tree_add_node(tree, node, ierror)
      if (ierror /= 0) then
         deallocate(node)
      end if
   endif
end subroutine tree_create_node

subroutine tree_remove_child_by_name(tree,name,ierror)
  character(len=*), intent(in)            :: name   !< name of child node to be removed
  type(TREE_DATA), intent(inout), pointer :: tree   !< tree from which the node has to be removed
  
  type(TREE_DATA_PTR), dimension(:), pointer :: children
  integer :: removeindex
  integer :: i
  integer,            intent(out) :: ierror !< Error status, 0 if succesful.
  integer                         :: newsize
  
  ierror = 0
  
  if ( .not. associated(tree) ) then
    ierror = 1
    return
  end if
  
  newsize = 0
  if ( associated( tree%child_nodes ) ) then
    newsize =  size( tree%child_nodes ) -1
  endif
  
  removeindex = -1
  do i = 1, newsize+1
    if ( str_tolower(tree_get_name(tree%child_nodes(i)%node_ptr)) == str_tolower(name) ) then
      removeindex = i
    endif
  enddo
  
  if (removeindex == -1) then
    ierror = 1
    return
  end if
  
  allocate( children(1:newsize), stat = ierror )
  if ( ierror /= 0 ) then
    return
  else
    if ( newsize > 1 ) then
      children(1:removeindex-1)     = tree%child_nodes(1:removeindex-1)
      children(removeindex:newsize) = tree%child_nodes(removeindex+1:newsize+1)
      deallocate( tree%child_nodes )
    endif
  
    tree%child_nodes => children
  
  endif
  
end subroutine tree_remove_child_by_name
!> Adds an existing tree node to the children array of a tree.
!! Both the tree and the new node are pointers, use this to efficiently
!! create or extend a tree with already existing subtrees.
subroutine tree_add_node(tree, node, ierror)
   type(TREE_DATA), pointer        :: tree   !< Pointer to the root of an existing tree, to which the node should be added.
   type(TREE_DATA), pointer        :: node   !< Pointer to an existing ('sub')tree, which will be added to the root tree.
   integer,            intent(out) :: ierror !< Error status, 0 if succesful.

   type(TREE_DATA_PTR), dimension(:), pointer :: children

   integer                         :: newsize

   ierror = 0

   if (.not. associated(tree)) then
      ierror = 1
      return
   end if

   if ( associated( node ) ) then
      newsize = 1
      if ( associated( tree%child_nodes ) ) then
         newsize = 1 + size( tree%child_nodes )
      endif

      allocate( children(1:newsize), stat = ierror )
      if ( ierror /= 0 ) then
         return
      else
         if ( newsize > 1 ) then
            children(1:newsize-1) = tree%child_nodes
            deallocate( tree%child_nodes )
            children(newsize-1)%node_ptr%bf_next_node => node    ! chain previous node in the breadth-first sense to the new node
         endif

         tree%child_nodes => children
         tree%child_nodes(newsize)%node_ptr => node
      endif
   else
      ierror = 2
   endif
end subroutine tree_add_node


!> Disconnect an existing node , in the children array, from a tree.
!! Both the tree and the new node are pointers, use this to efficiently
!! disconnect existing trees without destroying them.
subroutine tree_disconnect_node(tree, inode, ierror)
   type(TREE_DATA), pointer        :: tree   !< Pointer to the root of an existing tree, from which the node should be disconnected.
   integer,            intent(in ) :: inode  !< Index of child node to be disconnected
   integer,            intent(out) :: ierror !< Error status, 0 if succesful.
   !
   ! Locals
   type(TREE_DATA_PTR), dimension(:), pointer :: children
   integer                                    :: i
   integer                                    :: i_new
   integer                                    :: newsize
   !
   ! Body
   ierror = 0

   if (.not. associated(tree)) then
      ierror = 1
      return
   end if
   if (.not. associated(tree%child_nodes) ) then
      ierror = 1
      return
   end if
       
   newsize = size(tree%child_nodes) - 1
   if (newsize <= 0) then
       deallocate(tree%child_nodes, stat=ierror)
       ierror = 0
       return
   endif
   
   allocate( children(1:newsize), stat=ierror)
   if ( ierror .ne. 0 ) then
      return
   else
      i_new = 1
      do i=1, size(tree%child_nodes)
         if ( i /= inode ) then
            children(i_new) = tree%child_nodes(i)
            i_new = i_new + 1
         endif
      enddo
      tree%child_nodes => children
   endif
end subroutine tree_disconnect_node


!> Returns the number of nodes in a tree.
!! This node count is NOT recursive: it represents the number of nodes directly under the root level.
!! When the tree pointer itself is not associated, 0 is returned.
function tree_num_nodes(tree) result(num_nodes)
   type(TREE_DATA), pointer :: tree !< Tree pointer for which to determine the number of child nodes.

   integer                  :: num_nodes !< Number of child nodes in the specified tree, or 0 if unassociated.

   num_nodes = 0

   if (associated(tree)) then
      if (associated(tree%child_nodes)) then
         num_nodes = size(tree%child_nodes)
      end if
   end if
end function tree_num_nodes


!> Counts the number of toplevel tree nodes whose name
!! are equal to the given name (case insensitive).
function tree_count_nodes_byname(tree, name) result(count_nodes)
   type(TREE_DATA), pointer               :: tree !< Tree pointer for which to count the number of matching child nodes.
   character(len=*),        intent(in   ) :: name !< The name to search for.
   integer                                :: count_nodes !< The counted number of child nodes with matching name.

   integer :: i, num_nodes
   character(len=len_trim(name)) :: namei
   character(len=80)             :: node_name   

   count_nodes = 0

   namei = str_tolower(name) ! input name to lowercase

   num_nodes = tree_num_nodes(tree)
   do i=1,num_nodes
      node_name = str_tolower(tree_get_name(tree%child_nodes(i)%node_ptr))

      if (node_name == namei) then
         count_nodes = count_nodes + 1
      end if
   end do

end function tree_count_nodes_byname


! tree_get_name --
!    Return the name of the tree or node
!
! Arguments:
!    tree         The tree or node
!
function tree_get_name( tree ) result( node_name )
   type(TREE_DATA), pointer :: tree
   character(len=80)        :: node_name

   integer                  :: length
   integer                  :: i

   length    = min(80, size( tree%node_name ))
   node_name = ' '
   do i=1,length
      node_name(i:i) =  tree%node_name(i)
   end do
end function tree_get_name

! tree_get_data --
!    Return the data of the tree or node
!
! Arguments:
!    tree         The tree or node
!
function tree_get_data( tree ) result( node_data )
   type(TREE_DATA), pointer :: tree
   character(len=80)        :: node_data

   integer                  :: length
   integer                  :: i

   length    = min(80, size( tree%node_data ))
   node_data = ' '
   do i=1,length
      node_data(i:i) =  tree%node_data(i)
   end do
end function tree_get_data

! tree_get_datatype --
!    Return the data type for the data stored in the tree or node
!
! Arguments:
!    tree         The tree or node
!
function tree_get_datatype( tree ) result( data_type )
   type(TREE_DATA), pointer        :: tree
   character(len=40)               :: data_type

   integer                         :: length
   integer                         :: i

   data_type = '?'
   if ( associated( tree%node_data_type ) ) then
      length    = size( tree%node_data_type )
      do i=1,length
         data_type(i:i) = tree%node_data_type(i)
      end do
   endif
end function tree_get_datatype

! tree_get_node_by_name --
!    Return the child node by name
!
! Arguments:
!    tree         The tree or node to which to append the new node
!    name         Name of the node to find
!    node         Pointer to the node or "null"
! Result:
!    The argument node points to a new, empty node or is
!    not associated
!
subroutine tree_get_node_by_name( tree, name, node, i_return )
   character(len=*), intent(in)    :: name
   type(TREE_DATA), pointer        :: tree
   type(TREE_DATA), pointer        :: node

   character(len=80)               :: node_name
   character(len=80)               :: low_name
   integer, optional               :: i_return

   integer                         :: i

   if (present(i_return)) i_return = 0
   nullify( node )
   low_name = str_tolower(name)

   node_name = tree_get_name( tree )

   if ( node_name == low_name ) then
      node => tree
   elseif ( associated(tree%child_nodes) ) then
      do i = 1,size(tree%child_nodes)
         node_name = str_tolower(tree_get_name( tree%child_nodes(i)%node_ptr ))

         if ( node_name == low_name ) then
            node => tree%child_nodes(i)%node_ptr
            if (present(i_return)) i_return = i
            exit
         endif
      enddo
   endif

end subroutine tree_get_node_by_name

! tree_get_data_ptr --
!    Return a pointer to the tree/node's data
!
! Arguments:
!    tree        The tree or node from which to get the data
!    data_ptr    Pointer to the node/tree data
!    data_type   String indicating the type
! Result:
!    The argument data_ptr points to the stored data or is
!    not associated
!
subroutine tree_get_data_ptr( tree, data_ptr, data_type )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:), pointer  :: data_ptr
    character(len=*)                         :: data_type

    nullify( data_ptr )

    data_type = '?'
    if ( associated( tree%node_data) ) then
       data_ptr  => tree%node_data
       data_type =  tree_get_datatype( tree )
    endif

end subroutine tree_get_data_ptr

! tree_put_data --
!    Put (a copy of) the data in the tree/node
!
! Arguments:
!    tree        The tree or node with which to attach the data
!    data        Array of integers
!    data_type   Optional string indicating the type
!    success     True if all went well, false otherwise
! Result:
!    The tree structure points to a copy of the data
! Note:
!    A direct call to this routine will look something like:
!
!       call tree_put_data( tree, transfer( some_data, node_value ) )
!
!    where node_value acts as the mold for transferring the data
!
subroutine tree_put_data( tree, data, data_type, success )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:)  :: data
    character(len=*), optional      :: data_type
    logical, intent(out), optional  :: success

    integer                         :: error

    if ( associated(tree%node_data) ) then
       deallocate( tree%node_data )
    endif

    if ( associated(tree%node_data_type) ) then
       deallocate( tree%node_data_type )
    endif

!    GD: memory leak
!    if(associated(tree%node_data)) deallocate(tree%node_data)
    allocate( tree%node_data(1:size(data)), stat = error )
    if ( error == 0 ) then
       tree%node_data = data
       allocate( tree%node_data_type(1:len_trim(data_type)), &
          stat = error )
       if ( error == 0 ) then
          tree%node_data_type = transfer( data_type, tree%node_data_type )
       endif
    endif

    if ( present( success ) ) then
       success = error == 0
    endif

end subroutine tree_put_data

! tree_traverse_level --
!    Convenience function: level of the node during traversal
!
! Arguments:
!    None
!
integer function tree_traverse_level( )
   tree_traverse_level = traverse_level
end function tree_traverse_level

! tree_traverse --
!    Traverse a tree and handle the nodes by a depth-first method
!
! Arguments:
!    tree        The tree or node to traverse
!    handler     Routine to handle each node
!    data        Arbitrary data to be passed to the handler
!    stop        Whether to continue or stop (if set true)
! Result:
!    Each tree node is visited (unless the traversal is
!    prematurely ended by setting "stop" to true)
!
recursive subroutine tree_traverse( tree, handler, data, stop )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:)  :: data
    logical, intent(out)            :: stop

    interface
       subroutine handler( node, data, stop )
          use TREE_DATA_TYPES
          type(TREE_DATA), pointer                    :: node
          character(len=1), dimension(:), intent(in)  :: data
          logical, intent(inout)                      :: stop
       end subroutine handler
    end interface

    integer                         :: i

    stop = .false.
    if ( .not. associated( tree ) ) then
       return
    endif

    !
    ! First call the handler for the current node/tree
    !
    call handler( tree, data, stop )
    if ( stop ) then
       return
    endif

    !
    ! Then recurse through the child nodes (if any)
    !
    if ( associated( tree%child_nodes) ) then
       do i = 1,size(tree%child_nodes)
          traverse_level = traverse_level + 1
          call tree_traverse( tree%child_nodes(i)%node_ptr, &
                              handler, data, stop )
          traverse_level = traverse_level - 1
          if ( stop ) then
             exit
          endif
       enddo
    endif

end subroutine tree_traverse


! tree_traverse_bottomup --
!    Traverse a tree and handle the nodes by a depth-first method.
!    The callback subroutine is first performed on the children of each
!    node and then on the node itself.
!
! Arguments:
!    tree        The tree or node to traverse
!    handler     Routine to handle each node
!    data        Arbitrary data to be passed to the handler
!    stop        Whether to continue or stop (if set true)
! Result:
!    Each tree node is visited (unless the traversal is
!    prematurely ended by setting "stop" to true)
!
recursive subroutine tree_traverse_bottomup( tree, handler, data, stop )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:)  :: data
    logical, intent(out)            :: stop

    interface
       subroutine handler( node, data, stop )
          use TREE_DATA_TYPES
          type(TREE_DATA), pointer                    :: node
          character(len=1), dimension(:), intent(in)  :: data
          logical, intent(inout)                      :: stop
       end subroutine handler
    end interface

    integer                         :: i

    stop = .false.
    if ( .not. associated( tree ) ) then
       return
    endif

    !
    ! First recurse through the child nodes (if any)
    !
    if ( associated( tree%child_nodes) ) then
       do i = 1,size(tree%child_nodes)
          traverse_level = traverse_level + 1
          call tree_traverse_bottomup( tree%child_nodes(i)%node_ptr, &
                              handler, data, stop )
          traverse_level = traverse_level - 1
          if ( stop ) then
             exit
          endif
       enddo
    endif
    if ( stop ) then
       return
    endif

    !
    ! Then call the handler for the current node/tree
    !
    call handler( tree, data, stop )

end subroutine tree_traverse_bottomup


!> Destroys a tree freeing up all its memory. (don't use a nested subroutine)
subroutine tree_destroy(tree)
    type(TREE_DATA), pointer                   :: tree    !< Tree that should be destroyed.
    logical :: dummylog

    call tree_traverse_bottomup(tree, dealloc_tree_data, node_value, dummylog)
    nullify(tree)
end subroutine tree_destroy

!> Deallocates all node data for a tree root.
!! Assumes that all child nodes's data is already deallocated.
!! This subroutine is intended for use in tree_traverse_bottomup.
subroutine dealloc_tree_data(tree, data, stop)
    type(TREE_DATA), pointer                      :: tree !< Tree whose root should be printed.
    character(len=1), dimension(:), intent(in)    :: data !< Help data, not used, may be empty
    logical,                        intent(inout) :: stop !< Whether to continue or stop.

    integer :: error

    if (associated(tree)) then
        if (associated(tree%node_name)) then
            deallocate(tree%node_name, stat = error )
        endif
        if (associated(tree%node_data)) then
            deallocate(tree%node_data, stat = error )
        endif
        if (associated(tree%node_data_type)) then
            deallocate(tree%node_data_type, stat = error )
        endif
        if (associated(tree%child_nodes)) then
            deallocate(tree%child_nodes, stat = error )
        endif
        deallocate(tree, stat = error )
   end if
end subroutine dealloc_tree_data



!> 'Fold' a tree together, using operations on child data, in a bottomup fashion.
!! Two callback routines are required:
!! * one that operates on leaves and puts the result in the data(:) variable.
!! * one that operates on a tree node with the children already folded, using
!!   results in childdata(:,:).
!! The data and childdata can contain any type by  'transfer(.., node_value)'.
recursive subroutine tree_fold( tree, tree_handler, leaf_handler, data, stop )
    type(TREE_DATA), pointer        :: tree
    character(len=1), dimension(:)  :: data
    logical, intent(out)            :: stop

    interface
       subroutine tree_handler( node, childdata, data, stop )
          use TREE_DATA_TYPES
          type(TREE_DATA), pointer                        :: node
          character(len=1), dimension(:,:), intent(in)    :: childdata
          character(len=1), dimension(:),   intent(out)   :: data
          logical,                          intent(inout) :: stop
       end subroutine tree_handler
    end interface
    interface
       subroutine leaf_handler( node, data, stop )
          use TREE_DATA_TYPES
          type(TREE_DATA), pointer                        :: node
          character(len=1), dimension(:),   intent(out)   :: data
          logical,                          intent(inout) :: stop
       end subroutine leaf_handler
    end interface

    character(len=1), allocatable   :: childdata(:,:)

    integer                         :: i

    stop = .false.
    if ( .not. associated( tree ) ) then
       return
    endif

    !
    ! First recurse through the child nodes (if any)
    !
    if ( associated( tree%child_nodes) ) then
       allocate(childdata(size(data), size(tree%child_nodes)))
       do i = 1,size(tree%child_nodes)
          traverse_level = traverse_level + 1
          call tree_fold( tree%child_nodes(i)%node_ptr, &
                          tree_handler, leaf_handler, childdata(:,i), stop )
          traverse_level = traverse_level - 1
          if ( stop ) then
             exit
          endif
       enddo

       !
       ! Then call the handler for the childdata+current node/tree
       !
       call tree_handler( tree, childdata, data, stop )

       deallocate(childdata)
    else
       !
       ! Otherwise compute the data in a leaf node
       !
       call leaf_handler(tree, data, stop)
    endif


    if ( stop ) then
       return
    endif


end subroutine tree_fold


!> Return data as a simple string
!    The string is filled with the data stored in the node
!    not associated. The routine is successful if:
!    - there is data associated with the node/tree
!    - the data type is "STRING"
!    If the routine is not successful, the string is not changed.
subroutine tree_get_data_string( tree, string, success )
   type(TREE_DATA), pointer                 :: tree    !< The tree or node from which to get the data
   character(len=*), intent(out)            :: string  !< String to be filled
   logical, intent(out)                     :: success !< Whether successful or not

   character(len=1), dimension(:), pointer  :: data_ptr
   character(len=40)                        :: data_type
   integer                                  :: length
   integer                                  :: i

   success = .false.
   if ( associated(tree) ) then
      call tree_get_data_ptr( tree, data_ptr, data_type )

      if ( .not. associated(data_ptr) ) then
         return
      endif
      if ( data_type(1:6) /= 'STRING' ) then
         return
      endif

      success = .true.
      length  = size(data_ptr)
      string  = ' '
      length = min(length,len(string))
      do i=1, length
         string(i:i) = data_ptr(i)
      end do
   endif

end subroutine tree_get_data_string

!> Return data as a (allocatable) string
!    The string is filled with the data stored in the node
!    not associated. The routine is successful if:
!    - there is data associated with the node/tree
!    - the data type is "STRING"
!    If the routine is not successful, the string is not changed.
subroutine tree_get_data_alloc_string( tree, string, success )
   type(TREE_DATA), pointer                   :: tree    !< The tree or node from which to get the data
   character(len=:), allocatable, intent(out) :: string  !< String to be filled
   logical, intent(out)                       :: success !< Whether successful or not

   character(len=1), dimension(:), pointer  :: data_ptr
   character(len=40)                        :: data_type
   integer                                  :: length
   integer                                  :: i

   success = .false.
   if ( associated(tree) ) then
      call tree_get_data_ptr( tree, data_ptr, data_type )

      if ( .not. associated(data_ptr) ) then
         return
      endif
      if ( data_type(1:6) /= 'STRING' ) then
         return
      endif

      success = .true.
      length  = size(data_ptr)
      allocate(character(len=length)::string)
      do i=1, length
         string(i:i) = data_ptr(i)
      end do
   endif

end subroutine tree_get_data_alloc_string

subroutine print_tree( tree, data, stop )
   type(TREE_DATA), pointer               :: tree
   character(len=1), dimension(:), intent(in) :: data
   logical, intent(inout)                 :: stop

   character(len=1), dimension(:),pointer :: data_ptr
   character(len=60)                      :: string
   character(len=40)                      :: type_string

   integer,dimension(:),pointer    :: intarr
   integer       :: level
   integer       :: i
   real   ,dimension(:),pointer    :: realarr
   logical       :: success

   success = .true.
   level   = tree_traverse_level()
   write(*,*) ('   ', i=1,level), 'Node: ', trim(tree_get_name(tree))
   call tree_get_data_ptr( tree, data_ptr, type_string )

   select case (type_string)
   case ('STRING')
      string = '(no data)'
      call tree_get_data_string( tree, string, success )
      write(*,*) ('   ', i=1,level+1), trim(string), ' -- ', &
                 trim(type_string), ' -- ', success
   case ('INTEGER ARRAY')
      write(*,*) ('   ', i=1,level+1), transfer(tree%node_data,intarr), ' -- ', &
                 trim(type_string), ' -- ', success
   case ('REAL ARRAY')
      write(*,*) ('   ', i=1,level+1), transfer(tree%node_data,realarr), ' -- ', &
                 trim(type_string), ' -- ', success
   case default
      string = '(unknown data type)'
      write(*,*) ('   ', i=1,level+1), trim(string), ' -- ', &
                 trim(type_string), ' -- ', success
   end select
end subroutine print_tree

end module TREE_STRUCTURES

