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
   rvalues = -999.999
   inifile = 'test.ini'
   call tree_create( inifile, tree )
   call prop_file('ini',inifile,tree,fileok)
   if (fileok /= 0) then
      write(*,*)'Error ',fileok,' occured on reading file ',inifile
      goto 9999
   endif
   call tree_traverse( tree, print_tree, dummy, stop )
   morupd = .false.
   call prop_get_logical(tree, '*', 'morupd', morupd)
   write(*,*)'morupd = ',morupd
   call prop_get_string(tree, '*', 'SEDTYP', sedtyp)
   write(*,*)'sedtyp = @',sedtyp,'@'
   call prop_get_real(tree, '*', 'eropar', rvalues(1))
   write(*,*)'eropar = ',rvalues(1)

   call prop_get_integers(tree,'area 1','dredgepoint1',ivalues, 8)
   do i=1,10
      write(*,*) 'ivalue ',i,':',ivalues(i)
   enddo

   
   call prop_get_real(tree,'*','DredgeInputFileVersion',rvalues(1))
   
   call prop_get_reals(tree,'area 1','dumppoint1',rvalues, 6)
   do i=1,10
      write(*,*) 'rvalue ',i,':',rvalues(i)
   enddo
   
   call prop_get_string(tree,'*','fildrg',drgfile)
   write(*,'(a,a)') 'drgfile:',drgfile

   
   9999 continue
   

end subroutine test
