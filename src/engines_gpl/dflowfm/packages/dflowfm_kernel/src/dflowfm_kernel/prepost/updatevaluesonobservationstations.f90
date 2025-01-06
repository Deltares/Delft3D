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

!> update observation station data
module m_updatevaluesonobservationstations

implicit none

private

public :: updatevaluesonobservationstations

contains

subroutine updateValuesOnObservationStations()
   use m_fill_valobs, only: fill_valobs
   use m_flowtimes, only: time1
   use m_flowparameters, only: eps10
   use m_observations_data, only: IPNT_NUM, numobs, nummovobs, valobs, valobs_last_update_time
   use m_partitioninfo, only: jampi, reduce_valobs
   use m_timer, only: jatimer, IOUTPUTMPI, starttimer, stoptimer
   use precision_basics, only: comparereal

   if (comparereal(time1, valobs_last_update_time, eps10) == 0) then
      return
   end if
   valobs_last_update_time = time1

   call fill_valobs()

   if (jampi == 1) then
      if (jatimer == 1) call starttimer(IOUTPUTMPI)
      call reduce_valobs(IPNT_NUM, numobs + nummovobs, valobs)
      if (jatimer == 1) call stoptimer(IOUTPUTMPI)
   end if

   return
end subroutine updateValuesOnObservationStations

end module m_updatevaluesonobservationstations
