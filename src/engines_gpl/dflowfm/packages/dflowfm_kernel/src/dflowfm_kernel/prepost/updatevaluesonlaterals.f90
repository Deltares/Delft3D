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

!> Updates values on laterals for history output, starting from the starting time of history output
!! ! Note: if it is a parallel simulation, qplat is already for all subdomains, so no need for mpi communication.
subroutine updateValuesOnLaterals(tim1, timestep)
   use m_flowtimes, only: ti_his, time_his, ti_hiss
   use m_lateral, only: qqLat, numlatsg, qplat, qplatCum, qplatCumPre, qplatAve, qLatReal, &
                     qLatRealCum, qLatRealCumPre, qLatRealAve, n1latsg,  n1latsg, n2latsg, nnlat
   use precision
   use m_alloc
   use m_flowparameters, only: eps10
   use m_partitioninfo, only: jampi, reduce_double_sum, is_ghost_node
   implicit none
   double precision, intent(in) :: tim1     !< Current (new) time
   double precision, intent(in) :: timestep !< Timestep is the difference between tim1 and the last update time

   integer :: i, k, k1
   double precision, allocatable :: qLatRealCumTmp(:), qLatRealMPI(:)

   ! If current time has not reached the history output start time yet, do not update
   if (comparereal(tim1, ti_hiss, eps10) < 0) then
      return
   end if

   ! Compute realized discharge
   qLatReal = 0d0
   do i = 1,numlatsg
      do k1=n1latsg(i),n2latsg(i)
         k = nnlat(k1)
         if (k > 0) then
            if (.not. is_ghost_node(k)) then
               qLatReal(i) = qLatReal(i) + qqLat(k)
            end if
         end if
      end do
   end do

   ! At the starting time of history output, average discharge is 0, and skip the following computing
   if (comparereal(tim1, ti_hiss, eps10)== 0) then
      return
   end if

   !! Compute average discharge
   ! cumulative discharge from starting time of history output
   do i = 1, numlatsg
      qplatCum(i) = qplatCum(i) + timestep*qplat(i)
      qLatRealCum(i) = qLatRealCum(i) + timestep*qLatReal(i)
   enddo

   ! At the history output time, compute average discharge in the past His-interval
   if (comparereal(tim1, time_his, eps10)== 0 .and. ti_his > 0) then
      if (jampi == 1) then
         call realloc(qLatRealMPI, numlatsg, keepExisting = .false., fill = 0d0)
         call reduce_double_sum(numlatsg, qLatReal, qLatRealMPI)
         qLatReal(1:numlatsg) = qLatRealMPI(1:numlatsg)

         call realloc(qLatRealCumTmp, numlatsg, keepExisting = .false., fill=0d0)
         call reduce_double_sum(numlatsg, qLatRealCum, qLatRealCumTmp)
      end if
      do i = 1, numlatsg
         qplatAve(i) = (qplatCum(i) - qplatCumPre(i)) / ti_his
         qplatCumPre(i) = qplatCum(i)
         if (jampi == 1) then
            qLatRealAve(i) = (qLatRealCumTmp(i) - qLatRealCumPre(i)) / ti_his
            qLatRealCumPre(i) = qLatRealCumTmp(i)
         else
            qLatRealAve(i) = (qLatRealCum(i) - qLatRealCumPre(i)) / ti_his
            qLatRealCumPre(i) = qLatRealCum(i)
         end if
      enddo
   endif

end subroutine updateValuesOnLaterals
