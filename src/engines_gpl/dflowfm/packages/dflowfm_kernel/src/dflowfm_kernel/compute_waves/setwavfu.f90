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

module m_setwavfu

   implicit none

   private

   public :: setwavfu

contains

   !> subroutine to compute wave forces
   subroutine setwavfu()
      use precision, only: dp
      use m_flowparameters, only: jawaveforces, wave_forces_off, jawave, wave_swan_online, wave_nc_offline, wave_surfbeat, epshu
      use m_flowgeom, only: lnx, lnx1d, ln, acl, csu, snu
      use m_waves, only: m_waves_hminlw => hminlw, gammax, facmax, sxwav, sywav, sbxwav, sbywav, twav, fforc
      use m_xbeach_data, only: xb_hminlw => hminlw, gammaxxb
      use m_get_Lbot_Ltop, only: getlbotltop
      use m_flow, only: hu, huvli, wavfu, wavfv, rhomean, kmx
      use m_physcoef, only: sag
      use precision_basics, only: comparereal

      implicit none

      integer :: L, LL, Lb, Lt
      real(kind=dp) :: wavfx, wavfy, wavfbx, wavfby
      real(kind=dp) :: wavfu_loc, wavfbu_loc, twavL
      real(kind=dp) :: wavfv_loc, wavfbv_loc, wavfmag, force_scale, dztop
      real(kind=dp) :: fmax, ac1, ac2, hminlwi, rhoL, hminlw, gammaloc

      integer :: k1, k2

      if (jawaveforces == WAVE_FORCES_OFF) then
         wavfu = 0.0_dp
         wavfv = 0.0_dp
         return
      end if

      ! Set correct limiting depth
      if (jawave == WAVE_SWAN_ONLINE .or. jawave == WAVE_NC_OFFLINE) then
         hminlw = m_waves_hminlw
         hminlwi = 1.0_dp / m_waves_hminlw
         gammaloc = gammax
      end if

      if (jawave == WAVE_SURFBEAT) then
         hminlw = xb_hminlw
         hminlwi = 1.0_dp / xb_hminlw
         gammaloc = gammaxxb
      end if

      facmax = 0.25_dp * sag * rhomean * gammaloc**2

      wavfu = 0.0_dp
      wavfv = 0.0_dp

      if (kmx == 0) then
         do L = 1, lnx
            if (hu(L) <= epshu) then
               cycle
            end if

            ! Wave forcing is not applied to 1D links.
            if (L <= lnx1D) then
               cycle
            end if

            k1 = ln(1, L)
            k2 = ln(2, L)
            ac1 = acl(L)
            ac2 = 1.0_dp - ac1

            ! Interpolate surface force to the link.
            wavfx = ac1 * sxwav(k1) + ac2 * sxwav(k2)
            wavfy = ac1 * sywav(k1) + ac2 * sywav(k2)

            ! Add the depth-distributed body force.
            wavfx = wavfx + ac1 * sbxwav(k1) + ac2 * sbxwav(k2)
            wavfy = wavfy + ac1 * sbywav(k1) + ac2 * sbywav(k2)

            twavL = max(ac1 * twav(k1) + ac2 * twav(k2), 0.1_dp)
            fmax = facmax * hu(L)**1.5_dp / twavL

            ! Project the combined force into link-normal and tangential directions.
            wavfu_loc = wavfx * csu(L) + wavfy * snu(L)
            wavfv_loc = -wavfx * snu(L) + wavfy * csu(L)

            ! Limit the magnitude of the combined force vector.
            wavfmag = hypot(wavfu_loc, wavfv_loc)
            if (wavfmag > fmax) then
               wavfu_loc = wavfu_loc * fmax / wavfmag
               wavfv_loc = wavfv_loc * fmax / wavfmag
            end if

            ! Convert force [N/m2] to acceleration [m/s2].
            wavfu(L) = wavfu_loc * min(huvli(L), hminlwi) / rhomean
            wavfv(L) = wavfv_loc * min(huvli(L), hminlwi) / rhomean
         end do
      else ! kmx > 0
         do LL = 1, lnx
            if (hu(LL) <= epshu) then
               cycle
            end if

            ! Keep the same policy as the 2D branch.
            if (LL <= lnx1D) then
               cycle
            end if

            call getLbotLtop(LL, Lb, Lt)
            if (Lt < Lb) then
               cycle
            end if

            k1 = ln(1, LL)
            k2 = ln(2, LL)
            ac1 = acl(LL)
            ac2 = 1.0_dp - ac1

            twavL = max(ac1 * twav(k1) + ac2 * twav(k2), 0.1_dp)
            fmax = facmax * hu(LL)**1.5_dp / twavL
            rhoL = rhomean

            ! Surface force at the link.
            wavfx = ac1 * sxwav(k1) + ac2 * sxwav(k2)
            wavfy = ac1 * sywav(k1) + ac2 * sywav(k2)

            wavfu_loc = csu(LL) * wavfx + snu(LL) * wavfy
            wavfv_loc = -snu(LL) * wavfx + csu(LL) * wavfy

            ! Depth-distributed body force at the link.
            wavfbx = ac1 * sbxwav(k1) + ac2 * sbxwav(k2)
            wavfby = ac1 * sbywav(k1) + ac2 * sbywav(k2)

            wavfbu_loc = csu(LL) * wavfbx + snu(LL) * wavfby
            wavfbv_loc = -snu(LL) * wavfbx + csu(LL) * wavfby

            ! Limit the magnitude of the combined surface-plus-body force.
            wavfmag = hypot( &
                      wavfu_loc + wavfbu_loc, &
                      wavfv_loc + wavfbv_loc)

            force_scale = 1.0_dp
            if (wavfmag > fmax) then
               force_scale = fmax / wavfmag
            end if

            ! Apply the same scale to both contributions so their relative
            ! magnitude and direction are preserved.
            wavfu_loc = force_scale * wavfu_loc
            wavfv_loc = force_scale * wavfv_loc
            wavfbu_loc = force_scale * wavfbu_loc
            wavfbv_loc = force_scale * wavfbv_loc

            ! Surface force: apply to the top layer.
            dztop = hu(LL) - hu(Lt - 1)

            wavfu(Lt) = wavfu_loc / (rhoL * max(dztop, hminlw))
            wavfv(Lt) = wavfv_loc / (rhoL * max(dztop, hminlw))

            ! Body force: distribute uniformly over the water column.
            do L = Lb, Lt
               wavfu(L) = wavfu(L) + wavfbu_loc &
                          / (rhoL * max(hu(LL), hminlw))
               wavfv(L) = wavfv(L) + wavfbv_loc &
                          / (rhoL * max(hu(LL), hminlw))
            end do
         end do
      end if !
      wavfu = fforc * wavfu
      wavfv = fforc * wavfv
1234  continue
      return
   end subroutine setwavfu

end module m_setwavfu
