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

   integer :: irug, ierror
   double precision, allocatable, dimension(:, :) :: ruh
   double precision, allocatable, dimension(:, :) :: xy, xy_red

   if (.not. (allocated(ruh))) then
      allocate (ruh(2, num_rugs))
      allocate (xy(2, num_rugs))
      allocate (xy_red(2, num_rugs))
   end if

   ruh = 0d0 ! safety
   xy = 0d0
   xy_red = 0d0

   do irug = 1, num_rugs
      ruh(1, irug) = rug(irug)%max_rug_height
   end do
   ruh(2, :) = my_rank

   ! Obtain value of maximum runup across domains, and domainnr of max value
   if (jatimer == 1) call starttimer(IOUTPUTMPI)
   call reduce_rug(ruh, num_rugs)
   if (jatimer == 1) call stoptimer(IOUTPUTMPI)

   ! Reduce ruh and retrieve coordinates of maximum ruh
   do irug = 1, num_rugs
      rug(irug)%max_rug_height = ruh(1, irug)
      if (int(ruh(2, irug)) == my_rank) then
         xy(1, irug) = rug(irug)%max_x
         xy(2, irug) = rug(irug)%max_y
      end if
   end do

   ! Reduction of the sum of the coordinates, Could be mpi_reduce(rank=0)
   if (jatimer == 1) call starttimer(IOUTPUTMPI)
   call mpi_allreduce(xy, xy_red, 2 * num_rugs, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
   if (jatimer == 1) call stoptimer(IOUTPUTMPI)
   if (ierror /= 0) then
      goto 1234
   end if

   do irug = 1, num_rugs
      rug(irug)%max_x = xy_red(1, irug)
      rug(irug)%max_y = xy_red(2, irug)
   end do

1234 continue
   deallocate (xy_red)
   deallocate (xy)
   deallocate (ruh)

end subroutine updateValuesOnRunupGauges_mpi
