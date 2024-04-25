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
subroutine test

use properties
use tree_structures

implicit none

   type(tree_data), pointer   :: tree
   type(tree_data), pointer   :: node1, node2, dadptr, polptr

   character(len=1), dimension(1)          :: dummy = ' '
   character(len=1), dimension(:), pointer :: data_ptr
   character(len=8) :: node_name, node_type, stored_data, type

   logical                                 :: stop
   logical                                 :: success
   logical                                 :: morupd

integer               :: i
integer               :: fileok
integer, dimension(10):: ivalues
real   , dimension(10):: rvalues
character(4)          :: sedtyp
character(20)         :: inifile
character(len=100)    :: drgfile

   !
   ! TREE:
   !    NODE A (data: STORED A)
   !    NODE B (data: --)
   !       NODE C (data: STORED C)
   !

   call tree_create( "TREE", tree )

   call tree_create_node( tree, "NODE A", node1 )
   call tree_put_data( node1, transfer("STORED A",node_value), "STRING" )

   call tree_create_node( tree, "NODE B", node1 )
   call tree_create_node( node1, "NODE C", node2 )
   call tree_put_data( node2, transfer("STORED C",node_value), "STRING" )

   call tree_create_node( tree, "NODE D", node1 )

   call tree_get_node_by_name( tree, "NODE A", node1 )
   call tree_get_data_ptr( node1, data_ptr, type )
   stored_data = transfer( data_ptr, stored_data )
   write(*,*) 'NODE A:', trim(stored_data)

   call tree_get_node_by_name( tree, "NODE B", node1 )
   call tree_get_data_ptr( node1, data_ptr, type )
   if ( associated( data_ptr ) ) then
      stored_data = transfer( data_ptr, stored_data )
      write(*,*) 'NODE B:', stored_data
   else
      write(*,*) 'NODE B - no data'
   endif

   call tree_get_node_by_name( node1, "NODE C", node2 )
   call tree_get_data_ptr( node2, data_ptr, type )
   stored_data = transfer( data_ptr, stored_data )
   write(*,*) 'NODE C:', stored_data

   write(*,*) ' '
   write(*,*) 'Traverse the tree:'

   call tree_traverse( tree, print_tree, dummy, stop )

end subroutine test
