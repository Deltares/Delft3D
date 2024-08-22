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

   subroutine get_spiral3d
      use m_flow, only: ucx, ucy, spirint, zws, hs, dzslay, spiratx, spiraty
      use m_flowgeom, only: ndx

      implicit none
      integer :: k, kk, k1, kb, kt
      double precision :: uav, vav, utot, unx, uny, ut, un, zn, fn
      double precision :: sumff, sumfu

      do k = 1, ndx
         call getkbotktop(k, kb, kt)
         uav = 0d0
         vav = 0d0
         do kk = kb, kt
            k1 = kk - kb + 1
            uav = uav + ucx(kk) * dzslay(k1, 1)
            vav = vav + ucy(kk) * dzslay(k1, 1)
         end do
         utot = sqrt(uav**2 + vav**2)
         if (utot == 0d0) then
            spirint(k) = 0d0
            cycle
         end if
         spiratx(k) = uav / utot
         spiraty(k) = vav / utot
         sumff = 0d0
         sumfu = 0d0
         do kk = kb, kt
            zn = ((zws(kk) + zws(kk - 1)) * 0.5d0 - zws(kb - 1)) / hs(k)
            call findfn(zn, fn)
            ut = ucx(kk) * spiratx(k) + ucy(kk) * spiraty(k)
            unx = ucx(kk) - ut * spiratx(k)
            uny = ucy(kk) - ut * spiraty(k)
            un = -unx * spiraty(k) + uny * spiratx(k) ! transverse component
            sumff = sumff + fn * fn
            sumfu = sumfu + fn * un
         end do
         spirint(k) = 0d0
         if (sumff < 1d-6) cycle
         spirint(k) = sumfu / sumff
      end do

   end subroutine get_spiral3d
