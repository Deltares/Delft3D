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
module m_isoline
   use m_isofil

   implicit none
contains
   subroutine isoline(xa, ya, za, xb, yb, zb)
      use precision, only: dp
      use unstruc_display, only: rcir

      real(kind=dp) :: xa, ya, za, xb, yb, zb, dx, s, c, d, xh(4), yh(4), zh(4)
      dx = 0.2d0 * rcir
      call sincosdis(xa, ya, xb, yb, s, c, d)
      xh(1) = xa + dx * s
      yh(1) = ya - dx * c
      xh(2) = xb + dx * s
      yh(2) = yb - dx * c
      xh(3) = xb - dx * s
      yh(3) = yb + dx * c
      xh(4) = xa - dx * s
      yh(4) = ya + dx * c
      zh(1) = za
      zh(2) = zb
      zh(3) = zb
      zh(4) = za
      call ISOFIL(Xh, Yh, Zh, 4, 0)
   end subroutine isoline
end module m_isoline
