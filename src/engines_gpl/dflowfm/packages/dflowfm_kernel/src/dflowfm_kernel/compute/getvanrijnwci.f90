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

module m_getvanrijnwci

implicit none

contains

 subroutine getvanrijnwci(LL, umod, u2dh, taubpuLL, z0urouL)
    use m_flow
    use m_bedform
    use m_flowgeom
    use m_waves
    use m_physcoef

    implicit none

    integer, intent(in) :: LL
    double precision, intent(in) :: umod
    double precision, intent(in) :: u2dh
    double precision, intent(out) :: taubpuLL
    double precision, intent(out) :: z0urouL

    ! Locals
    integer :: k1, k2, Lb
    double precision :: hrmsu, tpu, rlabdau, rr, t1, u11, a11, raih, rmax, uon, uoff
    double precision :: cosk1, cosk2, sink1, sink2, cphi, sphi, ac1, ac2
    double precision :: phi, gamma, ksc, ka, ca, uwbih, rksru, rksmru, uratio
    double precision :: waveps, huLL, uuu, vvv, umax, uorbhs, csw, snw, abscos

    if (hu(LL) <= epshu) then ! safety
       taubpuLL = 0d0
       z0urouL = epsz0
       return
    end if
    waveps = 1d-4
    k1 = ln(1, LL); k2 = ln(2, LL)
    ac1 = acL(LL); ac2 = 1d0 - ac1
    Lb = Lbot(LL)
    huLL = max(hu(LL), 1d-3) ! cfr taubot

    ! wave data on links
    uorbhs = sqrt(2.0d0) * (0.5d0 * (uorb(k1) + uorb(k2)))
    hrmsu = 0.5d0 * (hwav(k1) + hwav(k2))
    tpu = 0.5d0 * (twav(k1) + twav(k2))
    rlabdau = 0.5d0 * (rlabda(k1) + rlabda(k2))
    cosk1 = cosd(phiwav(k1)); cosk2 = cosd(phiwav(k2))
    sink1 = sind(phiwav(k1)); sink2 = sind(phiwav(k2))
    csw = ac1 * cosk1 + ac2 * cosk2; snw = ac1 * sink1 + ac2 * sink2
    !
    cphi = csw * csu(LL) + snw * snu(LL)
    sphi = -csw * snu(LL) + snw * csu(LL)
    !
    ! euler velocities
    uuu = u1(Lb) - ustokes(Lb)
    if (jaconveyance2D >= 3 .or. LL <= lnx1D) then ! based on subroutine furu
       vvv = 0.0d0
    else
       vvv = v(Lb) - vstokes(Lb)
    end if
    !
    rr = -0.4d0 * sqrt(2.d0) / huLL + 1d0
    umax = rr * 2d0 * uorbhs
    t1 = tpu * sqrt(ag / huLL)
    u11 = umax / sqrt(ag * huLL)
    a11 = -0.0049d0 * t1**2 - 0.069d0 * t1 + 0.2911d0
    raih = max(0.5d0, -5.25d0 - 6.1d0 * tanh(a11 * u11 - 1.76d0))
    rmax = max(0.62d0, min(0.75d0, -2.5d0 * huLL / max(rlabdau, 1.d-2) + 0.85d0))
    uon = umax * (0.5d0 + (rmax - 0.5d0) * tanh((raih - 0.5d0) / (rmax - 0.5d0)))
    uoff = umax - uon
    uon = max(1d-5, uon)
    uoff = max(1d-5, uoff)
    uwbih = (0.5d0 * uon**3 + 0.5d0 * uoff**3)**(1.0d0 / 3.0d0)
    rksru = 0.5d0 * (bfmpar%rksr(k1) + bfmpar%rksr(k2))
    rksmru = 0.5d0 * (bfmpar%rksmr(k1) + bfmpar%rksmr(k2))
    !
    ! Van Rijn 2004 formulation
    !
    abscos = abs(uuu * cphi + vvv * sphi) / umod
    phi = acos(sign(1d0, (uuu * cphi + vvv * sphi)) * min(abscos, 1d0)) ! avoid overflows
    gamma = 0.8d0 + phi - 0.3d0 * phi**2
    ksc = sqrt(rksru**2 + rksmru**2)
    uratio = min(uwbih / (u2dh + waveps), 5d0)
    ka = ksc * exp(gamma * uratio)
    ka = min(ka, 10d0 * ksc, 0.2d0 * huLL)
    ca = 18.0_fp * log10(12.0_fp * huLL / ka)
    taubpuLL = ag * (u2dh * u2dh / umod) / ca**2
    z0urouL = max(3.33d-5, ka / 30d0)
 end subroutine getvanrijnwci

end module m_getvanrijnwci
