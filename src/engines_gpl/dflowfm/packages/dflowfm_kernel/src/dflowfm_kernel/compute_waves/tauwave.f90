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

   subroutine tauwave()
      use m_sferic
      use m_flowparameters
      use m_flow, only: rhomean, ag, hu, jaconveyance2D, u1, v, frcu, ifrcutp, z0urou, cfuhi, ifrctypuni, frcuni, taubxu, taubu
      use m_flowgeom
      use m_physcoef, only: rhomean, ee, sag, vonkar
      use m_waves
      use m_bedform, only: bfmpar
      use m_vegetation
      use m_trachy, only: trachy_resistance
      use unstruc_messages
      use unstruc_display
      use m_get_cz

      implicit none

      logical :: javegczu
      integer :: k1, k2, L
      double precision :: phivr
      double precision :: fw, astar, astarc, tauwav, taucur, cdrag, tpu, z0, uorbu, fsqrtt
      double precision :: cz, uuu, vvv, umod, umodsq, abscos, uorbhs, waveps
      double precision :: ymxpar, yparL
      double precision :: ust, ac1, ac2, rhoL, csw, snw
      double precision :: rz, cf, cwall, huL
      double precision :: hrmsu, rlabdau, rr, umax, t1, u11, a11, raih, rmax, uon, uoff, uwbih
      double precision :: rksru, rksmru, gamma, ksc, uratio, ka, ca
      double precision :: cosk1, cosk2, sink1, sink2
      double precision :: tauwci, cphi, sphi

      waveps = 1d-4 ! see taubot
      astarc = 30.*pi**2 ! critical value for astar
      fsqrtt = sqrt(0.5d0)
      javegczu = javeg > 1 .and. jabaptist > 1

      ! parameterized bottom friction models

      do L = 1, lnx
         huL = hu(L)
         if (huL <= epshu) then
            taubu(L) = 0d0 ! flow
            taubxu(L) = 0d0 ! flow
            z0urou(L) = epsz0 ! flow
            cfwavhi(L) = 0d0
            if (modind == 9) then
               cfhi_vanrijn(L) = 0d0
            end if
            cycle
         end if
         !
         huL = max(huL, 1d-2)
         k1 = ln(1, L); k2 = ln(2, L)
         ac1 = acl(L); ac2 = 1d0 - ac1
         !
         ! Use Eulerian velocities
         uuu = u1(L) - ustokes(L)

         if (jaconveyance2D >= 3 .or. L <= lnx1D) then ! based on subroutine furu
            vvv = 0.0d0
         else
            ! Use Eulerian velocities
            vvv = v(L) - vstokes(L)
         end if
         !
         umodsq = uuu * uuu + vvv * vvv
         umod = max(1.0d-4, sqrt(umodsq)) ! 3d: 1d-5
         taubu(L) = 0d0
         taubxu(L) = 0.0d0
         cfwavhi(L) = 0.0d0
         if (modind == 9) then
            cfhi_vanrijn(L) = 0d0
         end if

         ! interpolate uorbu, tpufrom flownodes to flowlinks
         uorbu = ac1 * uorb(k1) + ac2 * uorb(k2)
         tpu = ac1 * twav(k1) + ac2 * twav(k2)

         ! get water density on flow link
         rhoL = rhomean ! for now
         !
         ! get current related roughness height
         !
         if (frcu(L) > 0d0) then
            call getcz(huL, dble(frcu(L)), ifrcutp(L), cz, L)
         else
            call getcz(huL, frcuni, ifrctypuni, cz, L)
         end if
         z0 = huL / (ee * (exp(vonkar * cz / sag) - 1d0))

         if (modind > 0 .and. modind < 10) then
            !
            cosk1 = cos(phiwav(k1) * dg2rd); sink1 = sin(phiwav(k1) * dg2rd)
            cosk2 = cos(phiwav(k2) * dg2rd); sink2 = sin(phiwav(k2) * dg2rd)
            csw = ac1 * cosk1 + ac2 * cosk2; snw = ac1 * sink1 + ac2 * sink2
            !
            cphi = csw * csu(L) + snw * snu(L)
            sphi = -csw * snu(L) + snw * csu(L)
            !
            abscos = abs(uuu * cphi + vvv * sphi) / umod
            !
            ! wave friction factor and drag coefficient
            !
            astar = tpu * uorbu / max(z0, epsz0)

            if (astar > astarc) then
               fw = 0.00251d0 * exp(14.1d0 / (astar**0.19))
            else ! for relative small uorbs or large friction
               fw = 0.3d0
            end if
            !
            ! magnitude of bottom friction due to waves alone
            ! and due to current alone
            !
            tauwav = 0.5d0 * rhoL * fw * ftauw * uorbu * uorbu ! wave related bed shear stress
            if ((javegczu .and. cfuhi(L) > 0d0) .or. trachy_resistance) then ! vegetation hk/trachy
               cdrag = cfuhi(L) * huL
            else
               cdrag = ag / cz / cz
            end if
            taucur = rhoL * cdrag * umod * umod ! current related bed shear stress
            !
            ! parameterized models
            !
            call getymxpar(modind, tauwav, taucur, fw, cdrag, abscos, yparL, ymxpar)
            !
            ! bottom friction for combined waves and current
            !
            taubxu(L) = ymxpar * (taucur + tauwav) ! maximum shear stress due to waves and currents, eq to taubxu in D3D
            tauwci = yparL * (taucur + tauwav) ! mean shear stress
            taubu(L) = tauwci / umod * (u1(L) + ustokes(L)) ! in D3D, stresses for glm and stokes drift are added. This gives correct magnitude, but wrong stress direction; fixed at writing
            !
            if (jawave > 0) then
               if (modind < 9) then
                  cfwavhi(L) = tauwci / umod / rhoL / huL ! combined w+c friction factor for furu 2d
               elseif (modind == 9) then
                  uorbhs = sqrt(2.0d0) * uorbu
                  hrmsu = ac1 * hwav(k1) + ac2 * hwav(k2)
                  rlabdau = ac1 * rlabda(k1) + ac2 * rlabda(k2)
                  rr = -0.4d0 * sqrt(2d0) / huL + 1d0
                  umax = rr * 2d0 * uorbhs
                  t1 = tpu * sqrt(ag / huL)
                  u11 = umax / sqrt(ag * huL)
                  a11 = -0.0049d0 * t1**2 - 0.069d0 * t1 + 0.2911d0
                  raih = max(0.5d0, -5.25d0 - 6.1d0 * tanh(a11 * u11 - 1.76d0))
                  rmax = max(0.62d0, min(0.75, -2.5d0 * huL / max(rlabdau, 1.0d-20) + 0.85d0))
                  uon = umax * (0.5d0 + (rmax - 0.5d0) * tanh((raih - 0.5d0) / (rmax - 0.5d0)))
                  uoff = umax - uon
                  uon = max(1.0d-5, uon)
                  uoff = max(1.0d-5, uoff)
                  uwbih = (0.5d0 * uon**3 + 0.5d0 * uoff**3)**(1d0 / 3d0)
                  rksru = ac1 * bfmpar%rksr(k1) + ac2 * bfmpar%rksr(k2) ! these exist, okay
                  rksmru = ac1 * bfmpar%rksmr(k1) + ac2 * bfmpar%rksmr(k2)
                  !
                  ! Van Rijn 2004 formulation
                  !
                  phivr = acos(sign(1d0, (uuu * cphi + vvv * sphi)) * min(abscos, 1d0)) ! avoid overflows
                  gamma = 0.8d0 + phivr - 0.3d0 * phivr**2
                  ksc = sqrt(rksru**2 + rksmru**2)
                  uratio = min(uwbih / umod, 5.0d0)
                  ka = ksc * exp(gamma * uratio)
                  ka = min(ka, 10d0 * ksc, 0.2d0 * huL)
                  ca = 18d0 * log10(12d0 * huL / max(ka, waveps))
                  cfhi_vanrijn(L) = umod / huL * ag / (ca**2)
                  taubu(L) = ag / ca / ca * rhoL * umod * (u1(L) + ustokes(L))
               end if
            end if
         end if
         !
         ! Wave enhanced roughness heights
         if (modind == 0) then
            umod = sqrt(umodsq) ! no limitation
            z0urou(L) = huL * exp(-1d0 - vonkar * cz / sag)
            rz = huL / (ee * z0urou(L))
            cf = log(rz) / vonkar
            cwall = 1d0 / (cf**2)
            taubu(L) = cwall * rhoL * umod * (u1(L) + ustokes(L))
            taubxu(L) = cwall * rhoL * umod * umod
            cfwavhi(L) = cfuhi(L) * umod ! modind = 0 when running waves. Removed modind>0 statement in furu
         else if (modind == 10) then
            umod = sqrt((u1(L) - ustokes(L))**2 + (v(L) - vstokes(L))**2 + (1.16d0 * uorbu * fsqrtt)**2)
            z0urou(L) = huL * exp(-1d0 - vonkar * cz / sag)
            rz = huL / (ee * z0urou(L))
            cf = log(rz) / vonkar
            cwall = 1d0 / (cf**2)
            taubu(L) = cwall * rhoL * umod * (u1(L) + ustokes(L))
            taubxu(L) = cwall * rhoL * umod * umod
         else if (modind == 9) then
            z0urou(L) = max(3.33d-5, ka / 30d0)
         else
            umod = sqrt(umodsq) ! no limitation
            ust = sqrt(tauwci / rhoL)
            if (ust > waveps) then
               cf = min(umod / ust, 40d0) ! cz/sag
               z0urou(L) = huL * exp(-1d0 - vonkar * cf)
               z0urou(L) = min(z0urou(L), 10d0)
               !
            end if
         end if
      end do
      !
   end subroutine tauwave
