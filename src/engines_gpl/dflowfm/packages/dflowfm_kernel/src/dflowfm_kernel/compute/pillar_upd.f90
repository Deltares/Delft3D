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

module m_pillar_upd

   implicit none

contains

   ! =================================================================================================
   ! =================================================================================================
   subroutine pillar_upd()
      use precision, only: dp
      use fm_external_forcings_data, only: Cpil
      use m_flowgeom, only: lnx, ln, dx
      use m_flow, only: u1, v, advi
      use m_flowparameters, only: japillar
      implicit none
      integer :: L, k1, k2
      real(kind=dp) :: CpilL, uv

      if (japillar == 1) then
         do L = 1, lnx
            k1 = ln(1, L)
            k2 = ln(2, L)
            CpilL = (Cpil(k1) + Cpil(k2)) * 0.5d0
            uv = sqrt(u1(L) * u1(L) + v(L) * v(L))
            advi(L) = advi(L) + CpilL * uv / dx(L)
         end do
      else if (japillar == 3) then
         do L = 1, lnx
            if (Cpil(L) == 0d0) cycle
            CpilL = Cpil(L)
            uv = sqrt(u1(L) * u1(L) + v(L) * v(L))
            advi(L) = advi(L) + CpilL * uv / dx(L)
         end do
      end if

   end subroutine pillar_upd

end module m_pillar_upd
