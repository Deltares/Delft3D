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
module m_waveconst

   implicit none

   ! wavemodelnr
   integer, parameter :: NO_WAVES = 0
   integer, parameter :: WAVE_FETCH_HURDLE = 1
   integer, parameter :: WAVE_FETCH_YOUNG = 2
   integer, parameter :: WAVE_SWAN_ONLINE = 3
   integer, parameter :: WAVE_SURFBEAT = 4
   integer, parameter :: WAVE_UNIFORM = 5
   integer, parameter :: WAVE_NC_OFFLINE = 7
   
   ! wave forcing
   integer, parameter :: NO_WAVEFORCES = 0
   integer, parameter :: WAVEFORCES_RADIATIONSTRESS = 1
   integer, parameter :: WAVEFORCES_DISSIPATION_TOTAL = 2
   integer, parameter :: WAVEFORCES_DISSIPATION_3D = 3
   
   ! Stokes drift profile
   integer, parameter :: NO_STOKES_DRIFT = 0
   integer, parameter :: STOKES_DRIFT_DEPTHUNIFORM = 1
   integer, parameter :: STOKES_DRIFT_2NDORDER = 2
   integer, parameter :: STOKES_DRIFT_2NDORDER_ADVE = 3
   integer, parameter :: STOKES_DRIFT_2NDORDER_ADVE_VISC = 4

end module m_waveconst
