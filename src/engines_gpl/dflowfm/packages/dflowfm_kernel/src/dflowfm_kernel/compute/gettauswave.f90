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
module m_gettauswave
   use m_swart, only: swart
   use m_linkstocentercartcomp

   implicit none
contains
!> Make output arrays for bed shear stress icm jawave>0, depending on waq coupling and 2D/3D
   subroutine gettauswave(waveswartdelwaq)
      use precision, only: dp
      use m_flow
      use m_waves
      use m_flowgeom
      use m_sediment, only: sedtra, stm_included
      use m_get_kbot_ktop
      use m_get_chezy, only: get_chezy

      ! Input variables
      integer, intent(in) :: waveswartdelwaq

      ! Local variables
      integer :: L, LL, k1, k2, k, kb, kt, nn
      real(kind=dp) :: fw, ustw2, ust2, ust, cfn, wa, ar, cf, frcn, cz, z00
      real(kind=dp) :: ucxb, ucyb, ucxs, ucys, um, tauL
      real(kind=dp), allocatable :: ustv(:, :)

      taus = 0d0
      workx = 0d0 ! save 2 arrays
      worky = 0d0
      if (.not. allocated(ustv)) then
         allocate (ustv(2, max(kmx, 1)))
      end if
      ustv = 0d0

      ! Calculate magnitude
      select case (waveswartdelwaq)
         ! Regular hydrodynamic shear stress, from Soulsby/Van Rijn/Ruessink (default)
      case (0)
         do L = 1, lnx
            k1 = ln(1, L); k2 = ln(2, L)
            tauL = taubu(L)
            workx(k1) = workx(k1) + tauL * wcx1(L)
            workx(k2) = workx(k2) + tauL * wcx2(L)
            worky(k1) = worky(k1) + tauL * wcy1(L)
            worky(k2) = worky(k2) + tauL * wcy2(L)
         end do
         taus = hypot(workx(1:ndx), worky(1:ndx))

         ! Linear sum current + wave hydrodynamics (like gettau2)
      case (1)
         do k = 1, ndx
            z00 = 0d0
            wa = 0d0
            cfn = 0d0
            ust = 0d0
            do nn = 1, nd(k)%lnx
               LL = abs(nd(k)%ln(nn))
               frcn = frcu(LL)
               if (frcn > 0d0 .and. hu(LL) > 0d0) then
                  cz = get_chezy(hu(LL), frcn, u1(LL), v(LL), ifrcutp(LL))
                  cf = ag / (cz * cz)
                  ar = au(LL) * dx(LL)
                  wa = wa + ar ! area  weigthed
                  cfn = cfn + cf * ar
                  if (kmx > 0) then
                     ust = ust + ustb(LL) * ar
                  end if
                  !z00 = z00 + ar*hu(LL)*exp(-1d0 - vonkar*cz/sag)   ! z0ucur, to avoid double counting
                  z00 = z00 + ar * z0ucur(LL) ! z0ucur, to avoid double counting
               end if
            end do
            if (wa > 0d0) then
               cfn = cfn / wa
               ust = ust / wa
               z00 = z00 / wa
            end if
            z00 = max(z00, epsz0)
            !
            ust2 = 0d0
            if (kmx == 0) then
               ust2 = cfn * (ucx(k) * ucx(k) + ucy(k) * ucy(k))
            else
               ust2 = ust * ust
            end if
            !
            if (twav(k) > 1d-2) then
               call Swart(twav(k), uorb(k), z00, fw, ustw2)
               ust2 = ust2 + ftauw * ustw2
            end if
            taus(k) = rhomean * ust2
         end do

         ! Morphological bed shear stress
      case (2)
         if (stm_included) then
            taus = sedtra%taub
         else
            do L = 1, lnx
               k1 = ln(1, L); k2 = ln(2, L)
               tauL = taubxu(L)
               workx(k1) = workx(k1) + tauL * wcx1(L)
               workx(k2) = workx(k2) + tauL * wcx2(L)
               worky(k1) = worky(k1) + tauL * wcy1(L)
               worky(k2) = worky(k2) + tauL * wcy2(L)
            end do
            taus = hypot(workx(1:ndx), worky(1:ndx))
         end if
      end select

      ! Calculate direction from GLM velocities, ustokes unavailable
      if (flowwithoutwaves) then
         do k = 1, ndx
            call getkbotktop(k, kb, kt)
            ucxb = ucx(kb); ucyb = ucy(kb)
            um = max(hypot(ucxb, ucyb), 1d-4)
            if (um > 1d-4) then
               workx(k) = taus(k) * (ucxb) / um
               worky(k) = taus(k) * (ucyb) / um
            else
               workx(k) = taus(k) * cosd(phiwav(k))
               worky(k) = taus(k) * sind(phiwav(k))
            end if
         end do
      else
         ! Calculate direction from Eulerian velocities
         do k = 1, ndx
            call getkbotktop(k, kb, kt)
            call linkstocentercartcomp(k, ustokes, ustv)
            ucxb = ucx(kb); ucyb = ucy(kb); ucxs = ustv(1, 1); ucys = ustv(2, 1)
            um = max(hypot(ucxb - ucxs, ucyb - ucys), 1d-4)
            if (um > 1d-4) then
               workx(k) = taus(k) * (ucxb - ucxs) / um ! taus amplitude but euler directions
               worky(k) = taus(k) * (ucyb - ucys) / um
            else
               workx(k) = taus(k) * cosd(phiwav(k))
               worky(k) = taus(k) * sind(phiwav(k))
            end if
         end do
      end if
   end subroutine gettauswave
end module m_gettauswave
