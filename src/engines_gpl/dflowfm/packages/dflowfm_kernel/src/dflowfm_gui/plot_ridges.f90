!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!> plot the ridges
module m_plot_ridges

   implicit none

contains

   subroutine plot_ridges(ierror)
      use precision, only: dp

      use m_samples, only: mxsam, mysam, xs, ys
      use m_samples_refine, only: ihesstat, ihesstat_ok, zss
      use m_set_col, only: setcol
      use m_movabs, only: movabs
      use m_lnabs, only: lnabs
      use m_comp_sampleDh, only: comp_sampledh
      use m_missing, only: dmiss

      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: i, j, ip

      real(kind=dp) :: Dx, Dy, dum, Dh, x0, y0, x1, y1, x2, y2

      ierror = 1

      if (iHesstat /= iHesstat_OK) then
         goto 1234
      end if

!  plot ridge
      do i = 1, MXSAM
         do j = 1, MYSAM
!        compute sample mesh width
            Dh = comp_sampleDh(i, j)

            ip = i + (j - 1) * MXSAM

            if (abs(zss(5, i, j)) > 0.5_dp * Dh .or. zss(4, i, j) > -1.0e-8_dp .or. zss(5, i, j) == DMISS) then
               cycle
            end if

            Dx = zss(3, i, j)
            Dy = -zss(2, i, j)
            dum = Dh / sqrt(Dx**2 + Dy**2 + 1.0e-16_dp)
            Dx = Dx * dum
            Dy = Dy * dum

            call setcol(204)

            x0 = xs(ip) + zss(2, i, j) * zss(5, i, j)
            y0 = ys(ip) + zss(3, i, j) * zss(5, i, j)
            x1 = min(max(x0 - Dx, xs(ip) - 0.5_dp * Dh), xs(ip) + 0.5 * Dh)
            y1 = min(max(y0 - Dy, ys(ip) - 0.5_dp * Dh), ys(ip) + 0.5 * Dh)
            x2 = min(max(x0 + Dx, xs(ip) - 0.5_dp * Dh), xs(ip) + 0.5 * Dh)
            y2 = min(max(y0 + Dy, ys(ip) - 0.5_dp * Dh), ys(ip) + 0.5 * Dh)

            call movabs(x1, y1)
            call lnabs(x2, y2)
         end do
      end do

!   call qnerror(' ', ' ', ' ')

      ierror = 0
1234  continue

      return
   end subroutine plot_ridges

end module m_plot_ridges
