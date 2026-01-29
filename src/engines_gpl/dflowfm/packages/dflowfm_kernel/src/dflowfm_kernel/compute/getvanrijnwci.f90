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

module m_getvanrijnwci

   implicit none

contains

   subroutine getvanrijnwci(LL, umod, u2dh, taubpuLL, z0urouL)
      use precision, only: dp
      use m_flow, only: hu, epshu, epsz0, lbot, u1, jaconveyance2d, v, ag
      use m_bedform, only: bfmpar, fp
      use m_flowgeom, only: ln, acl, csu, snu, lnx1d
      use m_waves, only: uorb, hwav, twav, rlabda, phiwav, ustokes, vstokes

      implicit none

      integer, intent(in) :: LL
      real(kind=dp), intent(in) :: umod
      real(kind=dp), intent(in) :: u2dh
      real(kind=dp), intent(out) :: taubpuLL
      real(kind=dp), intent(out) :: z0urouL

      ! Locals
      integer :: k1, k2, Lb
      real(kind=dp) :: hrmsu, tpu, rlabdau, rr, t1, u11, a11, raih, rmax, uon, uoff
      real(kind=dp) :: cosk1, cosk2, sink1, sink2, cphi, sphi, ac1, ac2
      real(kind=dp) :: phi, gamma, ksc, ka, ca, uwbih, rksru, rksmru, uratio
      real(kind=dp) :: waveps, huLL, uuu, vvv, umax, uorbhs, csw, snw, abscos

      if (hu(LL) <= epshu) then ! safety
         taubpuLL = 0.0_dp
         z0urouL = epsz0
         return
      end if
      waveps = 1.0e-4_dp
      k1 = ln(1, LL)
      k2 = ln(2, LL)
      ac1 = acL(LL)
      ac2 = 1.0_dp - ac1
      Lb = Lbot(LL)
      huLL = max(hu(LL), 1.0e-3_dp) ! cfr taubot

      ! wave data on links
      uorbhs = sqrt(2.0_dp) * (0.5_dp * (uorb(k1) + uorb(k2)))
      hrmsu = 0.5_dp * (hwav(k1) + hwav(k2))
      tpu = 0.5_dp * (twav(k1) + twav(k2))
      rlabdau = 0.5_dp * (rlabda(k1) + rlabda(k2))
      cosk1 = cosd(phiwav(k1))
      cosk2 = cosd(phiwav(k2))
      sink1 = sind(phiwav(k1))
      sink2 = sind(phiwav(k2))
      csw = ac1 * cosk1 + ac2 * cosk2
      snw = ac1 * sink1 + ac2 * sink2
      !
      cphi = csw * csu(LL) + snw * snu(LL)
      sphi = -csw * snu(LL) + snw * csu(LL)
      !
      ! euler velocities
      uuu = u1(Lb) - ustokes(Lb)
      if (jaconveyance2D >= 3 .or. LL <= lnx1D) then ! based on subroutine furu
         vvv = 0.0_dp
      else
         vvv = v(Lb) - vstokes(Lb)
      end if
      !
      rr = -0.4_dp * sqrt(2.0_dp) / huLL + 1.0_dp
      umax = rr * 2.0_dp * uorbhs
      t1 = tpu * sqrt(ag / huLL)
      u11 = umax / sqrt(ag * huLL)
      a11 = -0.0049_dp * t1**2 - 0.069_dp * t1 + 0.2911_dp
      raih = max(0.5_dp, -5.25_dp - 6.1_dp * tanh(a11 * u11 - 1.76_dp))
      rmax = max(0.62_dp, min(0.75_dp, -2.5_dp * huLL / max(rlabdau, 1.0e-2_dp) + 0.85_dp))
      uon = umax * (0.5_dp + (rmax - 0.5_dp) * tanh((raih - 0.5_dp) / (rmax - 0.5_dp)))
      uoff = umax - uon
      uon = max(1.0e-5_dp, uon)
      uoff = max(1.0e-5_dp, uoff)
      uwbih = (0.5_dp * uon**3 + 0.5_dp * uoff**3)**(1.0_dp / 3.0_dp)
      rksru = 0.5_dp * (bfmpar%rksr(k1) + bfmpar%rksr(k2))
      rksmru = 0.5_dp * (bfmpar%rksmr(k1) + bfmpar%rksmr(k2))
      !
      ! Van Rijn 2004 formulation
      !
      abscos = abs(uuu * cphi + vvv * sphi) / umod
      phi = acos(sign(1.0_dp, (uuu * cphi + vvv * sphi)) * min(abscos, 1.0_dp)) ! avoid overflows
      gamma = 0.8_dp + phi - 0.3_dp * phi**2
      ksc = sqrt(rksru**2 + rksmru**2)
      uratio = min(uwbih / (u2dh + waveps), 5.0_dp)
      ka = ksc * exp(gamma * uratio)
      ka = min(ka, 10.0_dp * ksc, 0.2_dp * huLL)
      ca = 18.0_fp * log10(12.0_fp * huLL / ka)
      taubpuLL = ag * (u2dh * u2dh / umod) / ca**2
      z0urouL = max(3.33e-5_dp, ka / 30.0_dp)
   end subroutine getvanrijnwci

end module m_getvanrijnwci
