!----AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_density_parameters

   implicit none

   private

   integer, public :: idensform !< 0 = Uniform density, 1 = Eckart, 2 = UNESCO, 3 = UNESCO83
   logical, public :: apply_thermobaricity !< Check if density is pressure dependent
   logical, public :: thermobaricity_in_pressure_gradient !< Apply thermobaricity in computing the baroclinic pressure gradient
   integer, public :: max_iterations_pressure_density = 1 !< max nr of density-pressure iterations
   integer, public :: Jabarocponbnd = 1 !< Baroclinic pressure on open boundaries yes/no

end module m_density_parameters
