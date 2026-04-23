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
module m_boundary_condition_type
   implicit none
   private

   integer, parameter, public :: BOUNDARY_WATER_LEVEL = 1 ! water level boundary
   integer, parameter, public :: BOUNDARY_WATER_LEVEL_NEUMANN = 2 ! water level neumann
   integer, parameter, public :: BOUNDARY_VELOCITY_NORMAL_INFLOW = 3 ! velocity normal ingoing component
   integer, parameter, public :: BOUNDARY_VELOCITY_FLUX = 4 ! velocity flux boundary
   integer, parameter, public :: BOUNDARY_VELOCITY_RIEMANN = 5 ! velocity Riemann boundary
   integer, parameter, public :: BOUNDARY_WATER_LEVEL_OUTFLOW = 6 ! water level outflow
   integer, parameter, public :: BOUNDARY_DISCHARGE_HEAD = 7 ! discharge-head (qh) boundary
end module m_boundary_condition_type
