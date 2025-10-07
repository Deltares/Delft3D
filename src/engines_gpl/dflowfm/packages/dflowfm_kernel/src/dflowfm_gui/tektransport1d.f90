!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_tektransport1d

   implicit none

contains

   subroutine tektransport1D(tim)
      use precision, only: dp
      use m_sferic, only: twopi
      use m_statistics, only: avedif
      use m_flowgeom, only: ndxi, xz
      use m_transport, only: constituents, isalt
      use m_movabs, only: movabs
      use m_lnabs, only: lnabs
      implicit none
      real(kind=dp) :: tim
      real(kind=dp) :: cwave, period, omeg, wlen, rk, phi, xx, yy, dif
      integer :: k

      cwave = 60.0_dp * sqrt(10.0_dp * 1.0e-4_dp) ! chezy
      period = 90.0_dp * 60.0_dp
      omeg = twopi / period ! s
      wlen = cwave * period
      rk = twopi / wlen
      do k = 1, 600
         xx = -50.0_dp + (k - 1) * 100.0_dp
         phi = rk * xx - omeg * tim
         yy = 15.0_dp + 10.0_dp * cos(phi)
         if (k == 1) then
            call movabs(xx, yy)
         else
            call lnabs(xx, yy)
         end if
      end do

      if (ndxi < 1) return

      avedif = 0.0_dp
      do k = 1, ndxi
         xx = xz(k)
         phi = rk * xx - omeg * tim
         yy = 15.0_dp + 10.0_dp * cos(phi)
         dif = abs(constituents(isalt, k) - yy)
         avedif = avedif + dif
      end do
      avedif = avedif / ndxi

   end subroutine tektransport1D

end module m_tektransport1d
