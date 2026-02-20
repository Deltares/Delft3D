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
      use m_reallocsrc, only: reallocsrc
      use fm_external_forcings_data, only: source_sink_water_discharge, qsrcavg, source_sink_cum_volume, source_sink_cum_volume_prev, num_source_sink
      use precision, only: dp, comparereal
      use m_flowtimes, only: ti_his, time_his
      use m_flowparameters, only: eps10

      real(kind=dp), intent(in) :: tim1 !< Current (new) time

      real(kind=dp), save :: timprev = -1.0_dp ! TODO: save is unsafe, replace by using time1 and time0, also two other occurrences
      real(kind=dp) :: timstep
      integer :: i

      if (timprev < 0.0_dp) then
         ! This realloc should not be needed
         call reallocsrc(num_source_sink, 0)
      else
         timstep = tim1 - timprev
         ! cumulative volume from Tstart
         do i = 1, num_source_sink
            source_sink_cum_volume(i) = source_sink_cum_volume(i) + timstep * source_sink_water_discharge(i)
         end do

         if (comparereal(tim1, time_his, eps10) == 0) then
            do i = 1, num_source_sink
               qsrcavg(i) = (source_sink_cum_volume(i) - source_sink_cum_volume_prev(i)) / ti_his ! average discharge in the past His-interval
               source_sink_cum_volume_prev(i) = source_sink_cum_volume(i)
            end do
         end if
      end if

      timprev = tim1
   end subroutine updateValuesOnSourceSinks

end module m_updatevaluesonsourcesinks
