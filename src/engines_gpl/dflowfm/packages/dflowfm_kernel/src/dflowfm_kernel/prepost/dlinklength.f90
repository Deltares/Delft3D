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

!>    gives link length
submodule(m_dlinklength) m_dlinklength_

   implicit none

contains

   real(kind=dp) module function dLinklength(L)
      use precision, only: dp
      use network_data, only: xk, yk, kn
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dbdistance

      integer, intent(in) :: L !< link number
      integer :: La, k1, k2

      La = abs(L)
      k1 = kn(1, La)
      k2 = kn(2, La)

      dLinklength = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)

   end function dLinklength

end submodule m_dlinklength_
