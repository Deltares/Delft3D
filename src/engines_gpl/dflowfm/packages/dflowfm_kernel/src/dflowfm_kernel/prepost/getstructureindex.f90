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

!> Returns the index of a structure in the controllable value arrays.
!! Structure is identified by strtypename, e.g. 'pumps', and structure name, e.g., 'Pump01'.
!! Returned index can be used to directly address variables like, fm_external_forcings_data::qpump, zgate, etc.
subroutine getStructureIndex(strtypename, strname, index, is_in_network)
! NOTE: this will only return the GUI-used structures (i.e., the new gates and weirs via general structure, not the old ext-based damlevel and gateloweredgelevel).
! TODO: longer-term all structure sets run via channel_flow and t_structureset, cleanup this function then.
   use fm_external_forcings_data
   use m_hash_search, only: hashsearch
   use unstruc_channel_flow, only: network
   use m_longculverts

   implicit none
   character(len=*), intent(in   ) :: strtypename   !< the type of the structure: 'pumps', 'weirs', 'gates', ...
   character(len=*), intent(in   ) :: strname       !< Id/name of the requested structure, e.g. 'Pump01'
   integer,          intent(  out) :: index         !< Returned index of the found structure in its controllable value arrays. -1 when not found.
   logical,          intent(  out) :: is_in_network !< Whether or not the found structure is inside the network%sts set, or in FM global structure set. No meaning when structure not found.

   integer :: i, nstr, icgen
   integer, pointer :: cgen_mapping(:)
   index = -1
   is_in_network = .false.

   if (network%sts%count > 0) then
      ! TODO: when we allow non-unique ids between different structure types, select proper hashlist.
      index = hashsearch(network%sts%hashlist_structure, trim(strname))
      if (index > 0) then
         is_in_network = .true.
         return
      else
         ! Retry on the 2D structures in code below
         continue
      end if
   end if  

   if (trim(strtypename) == 'pumps') then
      do i=1,npumpsg
         if (trim(pump_ids(i)) == trim(strname)) then
            if (L2pumpsg(i) - L1pumpsg(i) >= 0) then
               ! Only return this pump index if pump is active in flowgeom (i.e., at least 1 flow link associated)
               index = i
               exit
            end if
         end if
      end do
   else if (trim(strtypename) == 'sourcesinks') then
      do i=1,numsrc
         if (trim(srcname(i)) == trim(strname)) then
            index = i
            exit
         end if
      end do
   else if (trim(strtypename) == 'dambreak') then
      do i=1,ndambreaksignals
         if (trim(dambreak_ids(i)) == trim(strname)) then
            if (L2dambreaksg(i) - L1dambreaksg(i) >= 0) then
               ! Only return this dambreak index if dambreak is active in flowgeom (i.e., at least 1 flow link associated)
               index = i
               exit
            end if
         end if
      end do
   else if (trim(strtypename) == 'longculverts') then
      do i=1,nlongculverts
         if (trim(longculverts(i)%id) == trim(strname)) then
            index = i
            exit
         end if
      end do
   else  ! generalstructure-based types
      select case(strtypename)
      case('weirs')
         cgen_mapping => weir2cgen
         nstr = nweirgen
      case('gates')
         cgen_mapping => gate2cgen
         nstr = ngategen
      case('generalstructures')
         cgen_mapping => genstru2cgen
         nstr = ngenstru
      case default
         nstr = 0
      end select

      do i=1,nstr
         icgen = cgen_mapping(i)
         if (trim(cgen_ids(icgen)) == trim(strname)) then
            if (L2cgensg(icgen) - L1cgensg(icgen) >= 0) then
               ! Only return this structure index if structure is active in flowgeom (i.e., at least 1 flow link associated)
               index = icgen
               exit
            end if
         end if
      end do
   end if

end subroutine getStructureIndex
