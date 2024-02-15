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

!< Reduce runup values over domains
subroutine updateValuesOnRunupGauges_mpi()
   use m_monitoring_runupgauges
   use m_partitioninfo
   use m_timer
   use mpi

   implicit none

   integer                                        :: irug, ierror
   double precision, allocatable, dimension(:,:)  :: ruh
   double precision, allocatable, dimension(:,:)  :: xy, xy_red

   if (.not. (allocated(ruh))) then
      allocate(ruh(2,nrug))
      allocate(xy(2,nrug))
      allocate(xy_red(2,nrug))
   endif

   ruh    = 0d0 ! safety
   xy     = 0d0
   xy_red = 0d0

   do irug = 1, nrug
      ruh(1,irug) = rug(irug)%maxruh
   enddo
   ruh(2,:) = my_rank

   ! Obtain value of maximum runup across domains, and domainnr of max value
   if ( jatimer.eq.1 ) call starttimer(IOUTPUTMPI)
   call reduce_rug(ruh, nrug)
   if ( jatimer.eq.1 ) call stoptimer(IOUTPUTMPI)

   ! Reduce ruh and retrieve coordinates of maximum ruh
   do irug = 1, nrug
      rug(irug)%maxruh = ruh(1,irug)
      if (int(ruh(2,irug))==my_rank) then
         xy(1,irug) = rug(irug)%maxx
         xy(2,irug) = rug(irug)%maxy
      endif
   enddo

   ! Reduction of the sum of the coordinates, Could be mpi_reduce(rank=0)
   if ( jatimer.eq.1 ) call starttimer(IOUTPUTMPI)
   call mpi_allreduce(xy,xy_red,2*nrug,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
   if ( jatimer.eq.1 ) call stoptimer(IOUTPUTMPI)
   if (ierror .ne. 0) then
      goto 1234
   endif

   do irug = 1, nrug
      rug(irug)%maxx = xy_red(1,irug)
      rug(irug)%maxy = xy_red(2,irug)
   enddo


1234 continue
   deallocate(xy_red)
   deallocate(xy)
   deallocate(ruh)

end subroutine updateValuesOnRunupGauges_mpi
