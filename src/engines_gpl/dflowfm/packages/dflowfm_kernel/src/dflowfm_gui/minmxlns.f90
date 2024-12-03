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

module m_minmxlns

   implicit none

contains

   subroutine minmxlns()
      use precision, only: dp

      use m_flowgeom, only: ln, lnx, xz, yz
      use m_flow, only: lnmin, lnmax
      use m_missing, only: dmiss
      use m_depmax2, only: vmax => vmax2, vmin => vmin2, dv => dv2, val => val2, nv => nv2, jaauto => jaauto2
      use m_inview
      use m_zlin

      real(kind=dp) :: zn
      real(kind=dp) :: rmin, rmax
      integer :: i, l, k1, k2

      if (jaauto > 0) then
         rmin = 1d30; lnmin = 0
         rmax = -1d30; lnmax = 0
         do L = 1, lnx
            k1 = ln(1, L)
            k2 = ln(2, L)
            if (inview(xz(k1), yz(k1)) .or. inview(xz(k2), yz(k2))) then
               zn = zlin(L)
               if (zn == DMISS) cycle
               if (zn < rmin) then
                  rmin = zn; lnmin = L
               end if
               if (zn > rmax) then
                  rmax = zn; lnmax = L
               end if
            end if
         end do
         vmax = rmax
         vmin = rmin
      end if

      dv = vmax - vmin
      do i = 1, nv
         val(i) = vmin + (i - 1) * dv / (nv - 1)
      end do

      return
   end subroutine minmxlns

end module m_minmxlns
