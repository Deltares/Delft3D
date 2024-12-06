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

module m_isosmoothnet

   implicit none

contains

   subroutine isosmoothnet(k) ! smooth isolines in net cells
      use precision, only: dp
      use m_isofil
      use m_netw, only: netcell, rnod, xk, yk

      integer :: k

      integer :: nn4, n, inode
      real(kind=dp) :: xx(10), yy(10), zz(10)

      nn4 = size(netcell(k)%nod)
      do n = 1, nn4
         inode = netcell(k)%nod(n)
         xx(n) = xk(inode)
         yy(n) = yk(inode)
         zz(n) = rnod(inode)
      end do
      call isofil(xx, yy, zz, nn4, 0)
   end subroutine isosmoothnet

end module m_isosmoothnet
