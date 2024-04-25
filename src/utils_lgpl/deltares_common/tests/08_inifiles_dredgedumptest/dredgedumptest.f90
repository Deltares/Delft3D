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
   character(len=40) :: node_name, node_type, stored_data, type

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

   ivalues = -999
   rvalues = -999.0
   inifile = 'dredge.dad'
   call tree_create( "Delft3D-FLOW input", tree )
   call tree_create_node( tree, "Dredge and Dump", dadptr )
   call tree_put_data( dadptr, transfer(inifile,node_value), "STRING" )
   call prop_file('ini',inifile,dadptr,fileok)
   if (fileok /= 0) then
      write(*,*)'Error ',fileok,' occured on reading file ',inifile
      stop
   endif

   call tree_get_node_by_name( dadptr, "Information", node1 )
   if (associated (node1)) then
      call tree_get_node_by_name( node1 , "Polygon File", polptr )
      if (associated (polptr)) then
         inifile = '(no data)'
         call tree_get_data_string( polptr, inifile, success )
         call prop_file('tekal',inifile,polptr,fileok)
         if (fileok /= 0) then
            write(*,*)'Error ',fileok,' occured on reading file ',inifile
            stop
         endif
      else
         write(*,*) 'error: polygon file not found in tree'
      endif
   else
      write(*,*) 'error: information not found in tree'
   endif

   write(*,*) '=====tree print start ================'
   call tree_traverse( dadptr, print_tree, dummy, stop )
   write(*,*) '=====tree print end   ================'

   call tree_get_node_by_name( polptr, "kuil2", node1 )
   if (associated (node1)) then
      call tree_get_data_ptr( node1, data_ptr, node_type )
      ivalues = transfer( data_ptr, ivalues )
      do i=1,2
         write(*,*) 'ivalue ',i,':',ivalues(i)
      enddo
   else
      write(*,*)'kuil2 in polptr not found'
   endif

   call tree_get_node_by_name( node1, "row_1", node2 )
   call tree_get_data_ptr( node2, data_ptr, node_type )
   rvalues = transfer( data_ptr, rvalues )
   do i=1,2
      write(*,*) 'rvalue ',i,':',rvalues(i)
   enddo

end subroutine test
