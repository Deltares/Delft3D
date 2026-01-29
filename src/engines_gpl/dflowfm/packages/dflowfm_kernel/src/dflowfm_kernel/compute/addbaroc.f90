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

module m_add_baroclinic_pressure_2d
   implicit none

   private

   public :: add_baroclinic_pressure_2d

contains

   !> Computes and adds the baroclinic pressure gradient (2D-model) contributions to the momentum equations
   subroutine add_baroclinic_pressure_2d(link_index_2d)
      use precision, only: dp
      use m_flowgeom, only: ln, dxi
      use m_flow, only: hu, adve
      use m_physcoef, only: ag
      use m_turbulence, only: rho, rhou
      use m_flowparameters, only: jarhoxu

      integer, intent(in) :: link_index_2d !< Horizontal link index

      real(kind=dp) :: baroclinic_force
      integer :: k1, k2

      k1 = ln(1, link_index_2d)
      k2 = ln(2, link_index_2d)
      baroclinic_force = ag * (rho(k1) - rho(k2)) * hu(link_index_2d) * dxi(link_index_2d) / ((rho(k2) + rho(k1)))
      if (jarhoxu > 0) then
         rhou(link_index_2d) = 0.5_dp * (rho(k2) + rho(k1))
      end if
      adve(link_index_2d) = adve(link_index_2d) - baroclinic_force
   end subroutine add_baroclinic_pressure_2d
end module m_add_baroclinic_pressure_2d
