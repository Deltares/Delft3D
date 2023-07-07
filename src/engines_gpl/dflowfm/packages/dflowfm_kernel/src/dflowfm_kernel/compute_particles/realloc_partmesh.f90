!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

!> (re)allocate partmesh data
subroutine realloc_partmesh()
   use m_partmesh
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   integer :: N

   call realloc(edge2node, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(edge2cell, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(xnode, numnodes, fill=0d0, keepExisting=.false.)
   call realloc(ynode, numnodes, fill=0d0, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(znode, numnodes, fill=0d0, keepExisting=.false.)
   end if

   call realloc(xzwcell, numcells, fill=DMISS, keepExisting=.false.)
   call realloc(yzwcell, numcells, fill=DMISS, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(zzwcell, numcells, fill=DMISS, keepExisting=.false.)
   end if
   call realloc(areacell, numcells, fill=DMISS, keepExisting=.false.)

   if ( jsferic.eq.0 ) then
      call realloc(dnx, (/1, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/1, numedges/), fill=DMISS, keepExisting=.false.)
   else
      call realloc(dnx, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnz, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnn, (/3, numcells/), fill=DMISS, keepExisting=.false.)
   end if
   call realloc(w, numedges, fill=DMISS, keepExisting=.false.)

   call realloc(edge2link, numedges, fill=0, keepExisting=.false.)
!   call realloc(nod2cell, numcells, fill=0, keepExisting=.false.)
   call realloc(cell2nod, numcells, fill=0, keepExisting=.false.)

   call realloc(jcell2edge, numcells+1, fill=1, keepExisting=.false.)
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)

   return
end subroutine realloc_partmesh
