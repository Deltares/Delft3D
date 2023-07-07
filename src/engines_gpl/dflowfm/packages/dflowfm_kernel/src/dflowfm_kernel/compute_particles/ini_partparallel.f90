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

!> initialization for parallel computations
subroutine ini_partparallel()
   use m_particles, only: japart
   use m_partmesh
   use m_partparallel
   use m_partitioninfo, only: jampi, ndomains, DFM_COMM_DFMWORLD, my_rank
   use m_sferic
   use m_alloc
   use geometry_module, only: Cart3Dtospher
   implicit none

   double precision, dimension(:),   allocatable :: xrecv, yrecv, zrecv

   integer,          dimension(:),   allocatable :: icells, irecv

   double precision                              :: xref

   integer                                       :: i, icell, idmn, k
   integer                                       :: numsend, numrecv
   integer                                       :: N, nrequest

   integer                                       :: ierror

   integer                                       :: itag = 4

   if ( jampi.eq.0 .or. japart.eq.0 ) return

   if ( jsferic.eq.0 ) then
      NDIM = 5 ! for updating particles
      INDX_ZPART = 0
      N = 3
   else
      NDIM = 6 ! for updating particles
      INDX_ZPART = NDIM
      N = 4
   end if

!  allocate
   call alloc_partparallel()

!  get other subdomain cellnumbers
   allocate(icells(numcells))
   icells = (/ (i, i=1,numcells) /)

!  make sendlist
   call part_makesendlist(numcells,icells)

!  fill send data
   call realloc(worksnd, (/ N,numcells /), keepExisting=.false., fill=0d0)
   do i=1,jsend(ndomains)-1
      icell = icells(isend(i))
      worksnd(1,i) = dble(icell)
      worksnd(2,i) = xzwcell(icell)
      worksnd(3,i) = yzwcell(icell)
      if ( jsferic.ne.0 ) then
         worksnd(4,i) = zzwcell(icell)
      end if
   end do

!  send/receive data
   call sendrecv_particledata(N,jsend,jrecv)

!  process received data
   numrecv = jrecv(ndomains)-1

   allocate(irecv(numrecv))
   allocate(xrecv(numrecv))
   allocate(yrecv(numrecv))
   if ( jsferic.ne.0 ) then
      allocate(zrecv(numrecv))
   end if

   xref = 0d0
   do i=1,jrecv(ndomains)-1
      irecv(i) = int(workrecv(1,i))
      if ( jsferic.eq.0 ) then
         xrecv(i) = workrecv(2,i)
         yrecv(i) = workrecv(3,i)
      else
         call Cart3Dtospher(workrecv(2,i),workrecv(3,i),workrecv(4,i),xrecv(i),yrecv(i),xref)
      end if
   end do

!  find which cells correspond to received cell coordinates
   call realloc(icells,numrecv,keepExisting=.false.,fill=0)
   call part_findcell(numrecv, xrecv, yrecv, icells, ierror)

!  send found cells back to other subdomains (recv is now send)
   call realloc(worksnd, (/ 1,numrecv /), keepExisting=.false., fill=0d0)
   do i=1,numrecv
      worksnd(1,i) = dble(icells(i))
   end do
   call sendrecv_particledata(1,jrecv,jsend)

!  fill other domain cell numbers
   numsend = jsend(ndomains)-1
   call realloc(icellother,numcells,keepExisting=.false.,fill=0)
   do i=1,numsend
      icell = isend(i)
      icellother(icell) = int(workrecv(1,i))
   end do

!  deallocate
   if ( allocated(irecv) ) deallocate(irecv)
   if ( allocated(xrecv) ) deallocate(xrecv)
   if ( allocated(yrecv) ) deallocate(yrecv)
   if ( allocated(zrecv) ) deallocate(zrecv)

   japartsaved = 0

   return
end subroutine ini_partparallel
