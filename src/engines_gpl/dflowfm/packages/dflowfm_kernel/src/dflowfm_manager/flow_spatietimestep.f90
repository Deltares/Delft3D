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

module m_flow_spatietimestep
use m_inctime_user, only: inctime_user

implicit none

private

public :: flow_spatietimestep

contains

 subroutine flow_spatietimestep() ! do flowstep
    use m_flow_externaloutput, only: flow_externaloutput
    use m_updatevaluesonobservationstations, only: updatevaluesonobservationstations
    use m_flow_single_timestep, only: flow_single_timestep
    use m_setzcs, only: setzcs
    use m_flowtimes
    use m_flowgeom, only: ndx
    use m_flowparameters, only: janudge
    use fm_external_forcings
    use m_flow_modelinit, only: flow_modelinit

    integer :: key, ierr

    if (ndx == 0) then
       ierr = flow_modelinit()
    end if

    if (ndx == 0) return ! No valid flow network was initialized

    call inctime_user()
    if (time0 >= time_user) then
       Tstop_user = tstop_user + dt_user
       time_user = time_user + dt_user
    end if
    ! ipv time0
    tim1fld = max(time_user, tim1fld)
    if (janudge == 1) call setzcs()
    call set_external_forcings(tim1fld, .false., ierr) ! set field oriented forcings. boundary oriented forcings are in

    ! call flow_externalinput(time_user)                  ! receive RTC signals etc

    call flow_single_timestep(key, ierr)

    call updateValuesOnObservationStations()

    call flow_externaloutput(time1) ! receive signals etc, write map, his etc
    ! these two functions are explicit. therefore, they are in the usertimestep
 end subroutine flow_spatietimestep

end module m_flow_spatietimestep
