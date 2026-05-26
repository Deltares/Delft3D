!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_updatevaluesonsourcesinks

   implicit none

   private

   public :: updatevaluesonsourcesinks

contains

   subroutine updateValuesOnSourceSinks(tim1)
      use m_source_sink, only: source_sinks, num_source_sink, source_sink_average_discharge_previous, source_sink_cumulative_volume, source_sink_cumulative_volume_previous
      use precision, only: dp, comparereal
      use m_flowtimes, only: ti_his, time_his
      use m_flowparameters, only: EPS10

      real(kind=dp), intent(in) :: tim1 !< Current (new) time

      real(kind=dp), save :: timprev = -1.0_dp ! TODO: save is unsafe, replace by using time1 and time0, also two other occurrences
      real(kind=dp) :: timstep
      integer :: i

      if (timprev < 0.0_dp) then
         ! This realloc should not be needed
         call source_sinks%resize(num_source_sink)
      else
         timstep = tim1 - timprev
         ! cumulative volume from Tstart
         do i = 1, num_source_sink
            source_sink_cumulative_volume(i) = source_sink_cumulative_volume(i) + timstep * source_sinks%discharge(i)
         end do

         if (comparereal(tim1, time_his, EPS10) == 0) then
            do i = 1, num_source_sink
               source_sink_average_discharge_previous(i) = (source_sink_cumulative_volume(i) - source_sink_cumulative_volume_previous(i)) / ti_his ! average discharge in the past His-interval
               source_sink_cumulative_volume_previous(i) = source_sink_cumulative_volume(i)
            end do
         end if
      end if

      timprev = tim1
   end subroutine updateValuesOnSourceSinks

end module m_updatevaluesonsourcesinks
