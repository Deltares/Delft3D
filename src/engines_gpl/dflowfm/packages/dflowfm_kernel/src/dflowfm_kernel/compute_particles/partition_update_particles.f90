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

! send/receive paticles from other subdomains
subroutine partition_update_particles()
   use m_particles
   use m_partmesh
   use m_partitioninfo
   use m_partparallel
   use m_alloc
   implicit none

   integer                                       :: i, icell, idmn, j, k
   integer                                       :: numsend, numrecv, numnew
   integer                                       :: N, Nadd, Nsize
   integer                                       :: ipoint
   integer                                       :: Nreplace

   integer                                       :: ierror

   if ( jampi.eq.0 ) return

!  make sendlist
   call part_makesendlist(Npart,kpart)

   numsend = jsend(ndomains)-1

!  copy particles to send array
   call part2send(jsend,isend)

!  send/recv data
   call sendrecv_particledata(NDIM,jsend,jrecv)

!  deactive sent particles
   do j=1,jsend(ndomains)-1
      i = isend(j)
      kpart(i) = 0
   end do

!  add received particles
   call recv2part(jrecv(ndomains)-1,workrecv)

   return
end subroutine partition_update_particles
