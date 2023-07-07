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

!> make CRS-formatted send list
subroutine part_makesendlist(N,icells)
   use m_partmesh
   use m_partitioninfo
   use m_partparallel
   use m_alloc
   implicit none

   integer                          :: N        !< number of cells to be send
   integer, dimension(N)            :: icells   !< cell numbers

!   integer, dimension(0:ndomains)   :: jsend    !< startpointers of send list
!   integer, dimension(N)            :: isend    !< send list

   integer                          :: i, icell, idmn, j, k
   integer                          :: numnew, numsend

!  count number of cells to be sent to other domains
   jsend = 0
   do i=1,N
!     get cell number
      icell = icells(i)

      if ( icell.eq.0 ) cycle

!     get flownode/netcell number
      k = iabs(cell2nod(icell))

!     get domain number
      idmn = idomain(k)

      if ( idmn.eq.my_rank ) cycle

!     update counters
      jsend(idmn+1) = jsend(idmn+1)+1
   end do

!  accumulate
   jsend(0) = 1
   do idmn=0,ndomains-1
      jsend(idmn+1) = jsend(idmn)+jsend(idmn+1)
   end do

   numsend = jsend(ndomains)-1

   if ( numsend.gt.ubound(isend,1) ) then
!     reallocate
      numnew = 1+int(1.2d0*dble(numsend))
      call realloc(isend,numnew,keepExisting=.false.,fill=0)
   end if

!  fill send list
   jpoint = jsend
   do i=1,N
!     get cell number
      icell = icells(i)

      if ( icell.eq.0 ) cycle

      k = iabs(cell2nod(icell))

!     get domain number
      idmn = idomain(k)

      if ( idmn.eq.my_rank ) cycle

      j = jpoint(idmn)

      isend(j) = i

      jpoint(idmn) = jpoint(idmn)+1
   end do

   return
end subroutine part_makesendlist
