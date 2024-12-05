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

module m_hkcircle

   implicit none

contains

   subroutine hkcircle(x, y, r) ! plotdevice routine interacter is niet goed, zie file fout.bmp
      use precision, only: dp
      use m_movabs
      use m_lnabs
      implicit none
      real(kind=dp) :: x, y, r
      real(kind=dp) :: twopi, phi
      integer :: k
      twopi = 2 * acos(-1d0)
      call movabs(x + r, y)
      do k = 1, 360
         phi = twopi * dble(k) / 360.
         call lnabs(x + r * cos(phi), y + r * sin(phi))
      end do
   end subroutine hkcircle

end module m_hkcircle
