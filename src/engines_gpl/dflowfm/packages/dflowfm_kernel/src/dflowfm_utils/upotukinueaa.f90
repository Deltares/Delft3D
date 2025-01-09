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

module m_upotukinueaa

   implicit none

   private

   public :: upotukinueaa

contains

   subroutine upotukinueaa(upot, ukin, ueaa)
      use precision, only: dp
      use m_flow, only: s1, ucx, ucy, ag, hs, ktop, kbot, vol1, jasal, rho, rhomean, &
                        zws, kmx, upot0, ukin0
      use m_flowgeom, only: ndx, bl
      use m_missing, only: dmiss

      real(kind=dp) :: upot, ukin, ueaa
      real(kind=dp) :: vtot, roav, zz, rhok, bmin
      integer k, kk

      upot = 0d0; ukin = 0d0; ueaa = 0d0; vtot = 0d0; roav = 0d0; bmin = 1d9

      do kk = 1, ndx
         bmin = min(bmin, bl(kk))
         if (hs(kk) == 0) cycle
         do k = kbot(kk), ktop(kk)
            vtot = vtot + vol1(k) ! m3
            if (jasal > 0) then
               roav = roav + vol1(k) * rho(k) ! kg
            else
               roav = roav + vol1(k) * rhomean ! kg
            end if
         end do
      end do
      if (vtot == 0d0) then
         return
      end if

      roav = roav / vtot ! kg/m3

      do kk = 1, ndx
         if (hs(kk) == 0) cycle
         do k = kbot(kk), ktop(kk)
            if (kmx > 0) then
               zz = (zws(k) + zws(k - 1)) * 0.5d0 - bmin ! m
            else
               zz = s1(k) - bmin
            end if
            if (jasal > 0) then
               rhok = rho(k)
            else
               rhok = rhomean
            end if
            ueaa = ueaa + vol1(k) * zz * (rho(k) - roav) ! kg.m
            upot = upot + vol1(k) * zz * rho(k) ! kg.m
            ukin = ukin + vol1(k) * rho(k) * (ucx(k) * ucx(k) + ucy(k) * ucy(k)) * 0.5d0 ! kg.m2/s2
         end do
      end do

      ueaa = ueaa * ag / vtot ! kg/(m.s2)
      upot = upot * ag / vtot
      ukin = ukin * 0.5 / vtot

      if (upot0 == dmiss) upot0 = upot
      if (ukin0 == dmiss) ukin0 = ukin

! upot = upot - upot0
      !
   end subroutine upotukinueaa

end module m_upotukinueaa
