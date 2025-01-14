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

!> convert quadrilaterals, pentagons and hexagons to triangles
module m_triangulate_cells

   implicit none

   private

   public :: triangulate_cells

contains

   subroutine triangulate_cells()
      use m_netw
      use m_new_link

      integer :: k, k0, k1, kk, Lnew, N

      do k = 1, nump ! loop over the cells
         N = netcell(k)%n
         if (N < 4) cycle
!     make the triangles by connecting the 3rd, 4th, etc. node to the first one
         k0 = netcell(k)%nod(1)
         do kk = 3, N - 1
            k1 = netcell(k)%nod(kk)
            call newlink(k0, k1, Lnew)
         end do
      end do

   end subroutine triangulate_cells

end module m_triangulate_cells
