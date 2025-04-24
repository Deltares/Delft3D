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

module m_addbaroc
   implicit none

   private

   public :: addbaroc

contains

   subroutine addbaroc(LL)
      use precision, only: dp
      use m_flowgeom, only: ln, dxi
      use m_flow, only: hu, adve
      use m_physcoef, only: ag
      use m_turbulence, only: rho, rhou
      use m_flowparameters, only: jarhoxu

      integer, intent(in) :: LL

      real(kind=dp) :: barocl
      integer :: k1, k2

      k1 = ln(1, LL); k2 = ln(2, LL)
      barocl = ag * (rho(k1) - rho(k2)) * hu(LL) * dxi(LL) / ((rho(k2) + rho(k1)))
      if (jarhoxu > 0) then
         rhou(LL) = 0.5_dp * (rho(k2) + rho(k1))
      end if
      adve(LL) = adve(LL) - barocL
   end subroutine addbaroc
end module m_addbaroc
