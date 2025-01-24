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

module m_getdeltay

   implicit none

   private

   public :: getdeltay

contains

   subroutine getdeltay(y, dx0, dy0) ! find dy=dx*cos(y0+0.5*dy) newton iteration
      use precision, only: dp
      use m_sferic

      real(kind=dp) :: y, dx0, dy0, f, df, yd, c, s, phi
      integer :: k
      dy0 = dx0 * cos(dg2rd * y)
      do k = 1, 5
         phi = dg2rd * (y + 0.5 * dy0); c = cos(phi); s = sqrt(1d0 - c * c)
         f = dy0 - dx0 * c
         df = 1d0 + 0.5d0 * dg2rd * dx0 * s
         yd = f / df
         dy0 = dy0 - yd
         if (yd < 1d-14) return
      end do
   end

end module m_getdeltay
