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

!> send/receive data from sendlist to/from worksend/workrecv and update recvlist
subroutine sendrecv_particledata(N,jsend,jrecv)
   use m_alloc
   use m_partparallel, only: worksnd, workrecv, irequest, numsendarr, numrecvarr
   use m_partitioninfo
#ifdef HAVE_MPI
   use mpi
#endif
   implicit none

   integer,                        intent(in)  :: N      !< size of data
   integer, dimension(0:ndomains), intent(in)  :: jsend  !< subdomain startpointers in sent data arrays
   integer, dimension(0:ndomains), intent(out) :: jrecv  !< subdomain startpointers in received data arrays

#ifdef HAVE_MPI
   integer, dimension(MPI_STATUS_SIZE)         :: istat

   integer                                     :: i, idmn
   integer                                     :: numnew, numrecvtot
   integer                                     :: nrequest
   integer                                     :: ierror

   integer                                     :: itag1=3
   integer                                     :: itag2=4

!  send data
   nrequest = 0
   do idmn=0,ndomains-1
      if ( idmn.eq.my_rank ) cycle
      nrequest = nrequest+1
      numsendarr(idmn) = jsend(idmn+1)-jsend(idmn)
      call mpi_isend(numsendarr(idmn), 1, MPI_INTEGER, idmn, itag1, DFM_COMM_DFMWORLD, irequest(nrequest), ierror)

!      write(6,"('send ', I0, ' to ', I0, ': ', I0)") my_rank, idmn, numsendarr(idmn)
      if ( numsendarr(idmn).gt.0 ) then
         nrequest = nrequest+1
         call mpi_isend(worksnd(1,jsend(idmn)), N*numsendarr(idmn), MPI_DOUBLE_PRECISION, idmn, itag2, DFM_COMM_DFMWORLD, irequest(nrequest), ierror)
      end if
   end do

!  receive data
   jrecv(0) = 1
   do idmn=0,ndomains-1
      if ( idmn.eq.my_rank ) then
         jrecv(idmn+1) = jrecv(idmn)
         cycle
      end if

      call mpi_recv(numrecvarr(idmn),1,MPI_INTEGER,idmn,itag1,DFM_COMM_DFMWORLD,istat,ierror)
!      write(6,"('receive ', I0, ' from ', I0, ': ', I0)") my_rank, idmn, numrecvarr(idmn)

      jrecv(idmn+1) = jrecv(idmn)+numrecvarr(idmn)

      if ( numrecvarr(idmn).gt.0 ) then
         numrecvtot = jrecv(idmn+1)-1
         if ( N.gt.ubound(workrecv,1) .or. numrecvtot.gt.ubound(workrecv,1) ) then
!           reallocate
            numnew = 1+int(1.2d0*dble(numrecvtot))
            call realloc(workrecv,(/N,numnew/),keepExisting=.true.)
         end if
         call mpi_recv(workrecv(1,jrecv(idmn)),N*numrecvarr(idmn),MPI_DOUBLE_PRECISION,idmn,itag2,DFM_COMM_DFMWORLD,istat,ierror)
!         nrequest = nrequest+1
!         call mpi_irecv(workrecv(1,jrecv(idmn)),N*numrecvarr(idmn),MPI_DOUBLE_PRECISION,idmn,itag2,DFM_COMM_DFMWORLD,irequest(nrequest),ierror)

      end if
   end do

!  terminate send (safety)
   do i=1,nrequest
      call mpi_wait(irequest(i),istat,ierror)
   end do
#else
   jrecv = 1
#endif

   return
end subroutine sendrecv_particledata
