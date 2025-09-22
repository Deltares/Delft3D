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

module m_wave_uorbrlabda

   implicit none

   private

   public :: wave_uorbrlabda

contains

   subroutine wave_uorbrlabda()
      use precision, only: dp
      use m_getwavenr, only: getwavenr
      use m_waves, only: uorb, wlenwav, uorbwav, twav, hwav, rlabda, jauorb, jauorbfromswan
      use m_flow, only: s1
      use m_flowgeom, only: ndx, bl
      use m_physcoef, only: ag
      use m_sferic, only: pi

      implicit none

      integer :: k
      integer :: wlenwav_from_SWAN = 0

      real(kind=dp) :: hss, per, omeg, k0, k0h, rk

      do k = 1, ndx
         hss = max(1.0e-2_dp, s1(k) - bl(k))
         per = max(1.0e-2_dp, twav(k)) ! wave period

         omeg = 2.0_dp * pi / per
         k0 = omeg * omeg / ag
         k0h = k0 * hss
         if (k0h > pi) then ! if deep water
            rk = k0
         elseif (k0h < 5.0e-3_dp) then ! if very shallow water
            rk = omeg / sqrt(ag * hss)
         else
            call getwavenr(hss, per, rk)
         end if
         if (wlenwav_from_SWAN == 1) then
            rlabda(k) = wlenwav(k)
         else
            rlabda(k) = 2.0_dp * pi / rk
         end if
         if (rk * hss < 80.0_dp) then ! if not very deep water
            if (jauorbfromswan == 1) then
               uorb(k) = uorbwav(k)
            else
               uorb(k) = 0.5_dp * hwav(k) * omeg / sinh(rk * hss)
               if (jauorb == 0) then ! old d3d convention
                  uorb(k) = uorb(k) * sqrt(pi) / 2.0_dp ! only on hrms derived value, not on SWAN read uorb
               end if
            end if
         else
            uorb(k) = 0.0_dp
         end if
      end do

   end subroutine wave_uorbrlabda

end module m_wave_uorbrlabda
