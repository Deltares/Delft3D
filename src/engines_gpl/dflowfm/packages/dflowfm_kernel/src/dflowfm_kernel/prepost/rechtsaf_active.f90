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

module m_rechtsaf_active

   implicit none

contains

   logical function rechtsaf_active(K1, K2, K3)

      use network_data, only: xk, yk
      use geometry_module, only: duitpl
      use m_sferic, only: jsferic

      integer, intent(in) :: K1, K2, K3

      double precision :: sig

      rechtsaf_active = .false.

      call duitpl(xk(k1), yk(k1), xk(k2), yk(k2), xk(k2), yk(k2), xk(k3), yk(k3), sig, jsferic)
      if (sig < 0) then
         rechtsaf_active = .true.
      else
         rechtsaf_active = .false.
      end if

      return
   end function rechtsaf_active
end module
