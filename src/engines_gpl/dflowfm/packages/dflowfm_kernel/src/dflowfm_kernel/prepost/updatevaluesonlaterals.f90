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
module m_updatevaluesonlaterals

   implicit none

   private

   public :: updatevaluesonlaterals

contains

   subroutine updateValuesOnLaterals(tim1, timestep)
      use m_flowtimes, only: ti_his, time_his, ti_hiss
      use m_laterals, only: qqLat, numlatsg, num_layers, qplat, qplatCum, qplatCumPre, qplatAve, qLatReal, &
                            qLatRealCum, qLatRealCumPre, qLatRealAve, n1latsg, n1latsg, n2latsg, nnlat
      use precision
      use m_alloc
      use m_flowparameters, only: eps10
      use m_partitioninfo, only: jampi, reduce_double_sum, is_ghost_node

      real(kind=dp), intent(in) :: tim1 !< Current (new) time
      real(kind=dp), intent(in) :: timestep !< Timestep is the difference between tim1 and the last update time

      integer :: k1, i, i_lat, i_layer, i_node
      real(kind=dp), allocatable :: qLatRealCumTmp(:), qLatRealMPI(:)

      ! If current time has not reached the history output start time yet, do not update
      if (comparereal(tim1, ti_hiss, eps10) < 0) then
         return
      end if

      ! Compute realized discharge
      qLatReal = 0d0
      ! sum over 3rd dimension of qqlat
      do i = 1, numlatsg
         do k1 = n1latsg(i), n2latsg(i)
            i_node = nnlat(k1)
            if (i_node > 0) then
               if (.not. is_ghost_node(i_node)) then
                  ! sum over 2nd dimension of qqlat
                  do i_layer = 1, num_layers
                     qLatReal(i) = qLatReal(i) + qqLat(i_layer, k1)
                  end do
               end if
            end if
         end do
      end do

      ! At the starting time of history output, average discharge is 0, and skip the following computing
      if (comparereal(tim1, ti_hiss, eps10) == 0) then
         return
      end if

   !! Compute average discharge
      ! cumulative discharge from starting time of history output
      do i_lat = 1, numlatsg
         do i_layer = 1, num_layers
            qplatCum(i_lat) = qplatCum(i_lat) + timestep * qplat(i_layer, i_lat)
         end do
         qLatRealCum(i_lat) = qLatRealCum(i_lat) + timestep * qLatReal(i_lat)
      end do

      ! At the history output time, compute average discharge in the past His-interval
      if (comparereal(tim1, time_his, eps10) == 0 .and. ti_his > 0) then
         if (jampi == 1) then
            call realloc(qLatRealMPI, numlatsg, keepExisting=.false., fill=0d0)
            call reduce_double_sum(numlatsg, qLatReal, qLatRealMPI)
            qLatReal(1:numlatsg) = qLatRealMPI(1:numlatsg)

            call realloc(qLatRealCumTmp, numlatsg, keepExisting=.false., fill=0d0)
            call reduce_double_sum(numlatsg, qLatRealCum, qLatRealCumTmp)
         end if
         do i_lat = 1, numlatsg
            qplatAve(i_lat) = (qplatCum(i_lat) - qplatCumPre(i_lat)) / ti_his
            qplatCumPre(i_lat) = qplatCum(i_lat)
            if (jampi == 1) then
               qLatRealAve(i_lat) = (qLatRealCumTmp(i_lat) - qLatRealCumPre(i_lat)) / ti_his
               qLatRealCumPre(i_lat) = qLatRealCumTmp(i_lat)
            else
               qLatRealAve(i_lat) = (qLatRealCum(i_lat) - qLatRealCumPre(i_lat)) / ti_his
               qLatRealCumPre(i_lat) = qLatRealCum(i_lat)
            end if
         end do
      end if

   end subroutine updateValuesOnLaterals

end module m_updatevaluesonlaterals
