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

!> Data used for validating the current flow state to determine whether simulation should be aborted.
module m_flow_validatestate_data

   use precision, only: dp
   implicit none

   integer, parameter :: VALIDATESTATEWINDOWSIZE = 100 !< Number of time steps to include in the sliding average, don't set this too optimistic to avoid too fast simulation breaks.
   integer :: window_start !< Start index in dtavg_window for the moving time window to compute sliding average dt
   integer :: window_current !< Current index in dtavg_window for the moving time window to compute sliding average dt

   real(kind=dp), parameter :: VALIDATESTATEWINDOWSIZE_double = real(VALIDATESTATEWINDOWSIZE, kind=dp)
   real(kind=dp) :: validatestate_window_length !< Current length in dtavg_window for the moving time window to compute sliding average dt

   !flow_validationstate variables
   !These variable names follow the pattern [variable]_[condition]_[type] where condition is either 'max' or 'min' and type is either 'err' (simulation breaks) or 'warn' (warning is issued).
   real(kind=dp) :: s01_max_err !< absolute water level change threshold (m) - breaks when this value is exceeded
   real(kind=dp) :: u01_max_err !< absolute velocity change threshold (m/s) - breaks when this value is exceeded
   real(kind=dp) :: umag_max_err !< velocity magnitude threshold (m/s) - breaks when this value is exceeded
   real(kind=dp) :: ssc_max_err !< suspended sediment concentration threshold (kg/m3) - breaks when this value is exceeded
   real(kind=dp) :: dtavg_min_err !< minimum smallest allowed timestep checked on a sliding average of several timesteps (in s) - breaks if the timestep is lower than the given value
   real(kind=dp) :: s01maxavg_min_err !< minimum smallest allowed absolute water level change checked on a sliding average of several timesteps (in s) - breaks if the absolute water level change is lower than the given value

   real(kind=dp) :: s1_max_warn !< water level threshold (m) - warns when this value is exceeded
   real(kind=dp) :: u1abs_max_warn !< absolute velocity threshold (m/s) - warns when this value is exceeded
   real(kind=dp) :: umag_max_warn !< velocity magnitude threshold (m/s) - warns when this value is exceeded

   real(kind=dp), dimension(:), allocatable :: dtavg_window !< Array to store the last VALIDATESTATEWINDOWSIZE timesteps for sliding average computation of dtavg.
   real(kind=dp), dimension(:), allocatable :: s01maxavg_window !< Array to store the last VALIDATESTATEWINDOWSIZE timesteps for sliding average computation of s01maxavg.

end module m_flow_validatestate_data
