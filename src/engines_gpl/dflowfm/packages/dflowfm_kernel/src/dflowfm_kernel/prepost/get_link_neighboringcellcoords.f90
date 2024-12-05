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
module m_get_link_neighboring_cell_coords
   implicit none
contains
!> get neighboring cell center coordinates
   subroutine get_link_neighboringcellcoords(L, isactive, xza, yza, xzb, yzb)
      use precision, only: dp
      use network_data
      use m_flowgeom, only: xz, yz ! Note that xz,yz are already filled after findcells.

      integer, intent(in) :: L !< link number
      integer, intent(out) :: isactive !< active link (1) or not (0)
      real(kind=dp), intent(out) :: xza, yza, xzb, yzb !< left- and right-neighboring cell centers

      integer :: n1, n2

      isactive = 1

      if (kn(3, L) == 1 .or. kn(3, L) == 3 .or. kn(3, L) == 4) then
         n1 = kn(1, L)
         n2 = kn(2, L)
         xza = xk(n1); yza = yk(n1)
         xzb = xk(n2); yzb = yk(n2)
      else
         n1 = lne(1, L); n2 = lne(2, L)
         if (lnn(L) < 2 .or. n1 <= 0 .or. n2 <= 0 .or. n1 > nump .or. n2 > nump) then
            isactive = 0
            return
         end if
         xza = xz(n1); yza = yz(n1)
         xzb = xz(n2); yzb = yz(n2)
      end if

      return
   end subroutine get_link_neighboringcellcoords
end module m_get_link_neighboring_cell_coords
