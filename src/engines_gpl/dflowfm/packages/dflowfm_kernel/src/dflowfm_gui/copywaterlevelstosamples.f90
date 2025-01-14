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

module m_copywaterlevelstosamples

   implicit none

contains

   subroutine copywaterlevelstosamples()
      use m_samples, only: ns, xs, ys, zs, increasesam
      use m_flowgeom, only: ndx, xz, yz
      use m_flow, only: hs
      use unstruc_display_data, only: wetplot
      use m_znod

      integer :: k, n

      k = 0
      do n = 1, ndx
         if (hs(n) >= wetplot) then
            k = k + 1
            call increasesam(k)
            xs(k) = xz(n); ys(k) = yz(n); zs(k) = znod(n)
         end if
      end do
      ns = k
   end subroutine copywaterlevelstosamples

end module m_copywaterlevelstosamples
