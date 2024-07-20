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

   !> subroutine to compute wave forces
   subroutine setwavfu()
      use unstruc_messages
      use MessageHandling
      use m_flowparameters
      use m_flowgeom
      use m_flow, only: hu, huvli, wavfu, wavfv, rhomean, kmx
      use m_waves, m_waves_hminlw => hminlw
      use m_xbeach_data, xb_hminlw => hminlw
      use m_physcoef, only: sag
      implicit none

      integer :: L, LL, Lb, Lt
      double precision :: wavfx, wavfy, wavfbx, wavfby
      double precision :: wavfu_loc, wavfbu_loc, twavL, hwavL
      double precision :: wavfv_loc, wavfbv_loc, wavfmag, wavfbmag, wavfang, wavfbang
      double precision :: fmax, ac1, ac2, hminlwi, rhoL, hminlw, gammaloc

      integer :: k1, k2

      if (jawaveforces == 0) then
         wavfu = 0d0
         wavfv = 0d0
         return
      end if

      ! Set correct limiting depth
   if (jawave==3 .or. jawave==7) then
         hminlw = m_waves_hminlw
         hminlwi = 1d0 / m_waves_hminlw
         gammaloc = gammax
      end if

      if (jawave == 4) then
         hminlw = xb_hminlw
         hminlwi = 1d0 / xb_hminlw
         gammaloc = gammaxxb
      end if

      facmax = 0.25d0 * sag * rhomean * gammaloc**2

      wavfu = 0d0
      wavfv = 0d0

      if (kmx == 0) then
         do L = 1, lnx
            if (hu(L) <= epshu) cycle
            if (L > lnx1D) then
               k1 = ln(1, L); k2 = ln(2, L)
               ac1 = acl(L)
               ac2 = 1d0 - ac1

               wavfx = ac1 * sxwav(k1) + ac2 * sxwav(k2)
               wavfy = ac1 * sywav(k1) + ac2 * sywav(k2)

               wavfbx = ac1 * sbxwav(k1) + ac2 * sbxwav(k2)
               wavfby = ac1 * sbywav(k1) + ac2 * sbywav(k2)

               twavL = ac1 * twav(k1) + ac2 * twav(k2)

               ! limit forces
               fmax = facmax * hu(L)**1.5 / max(0.1d0, twavL)

               ! projection in face-normal direction
               wavfu_loc = wavfx * csu(L) + wavfy * snu(L)
               wavfv_loc = -wavfx * snu(L) + wavfy * csu(L)
               wavfbu_loc = wavfbx * csu(L) + wavfby * snu(L)
               wavfbv_loc = -wavfbx * snu(L) + wavfby * csu(L)

               ! Should be done on the vector norm, nt separate comps
               wavfmag = min(hypot(wavfu_loc, wavfv_loc), fmax)
               wavfbmag = min(hypot(wavfbu_loc, wavfbv_loc), fmax)
               wavfang = atan2(wavfv_loc, wavfu_loc)
               wavfbang = atan2(wavfbv_loc, wavfbu_loc) ! necessary?
               wavfu_loc = wavfmag * cos(wavfang)
               wavfv_loc = wavfmag * sin(wavfang)
               wavfbu_loc = wavfbmag * cos(wavfbang)
               wavfbv_loc = wavfbmag * sin(wavfbang)

               wavfu(L) = wavfu_loc + wavfbu_loc
               wavfv(L) = wavfv_loc + wavfbv_loc
            end if
            wavfu(L) = wavfu(L) * min(huvli(L), hminlwi) / rhomean ! Dimensions [m/s^2]
            wavfv(L) = wavfv(L) * min(huvli(L), hminlwi) / rhomean ! Dimensions [m/s^2]
         end do
      else ! kmx>0
         do LL = 1, lnx
            if (hu(LL) <= epshu) cycle
            call getLbotLtop(LL, Lb, Lt)
            if (Lt < Lb) cycle
            k1 = ln(1, LL); k2 = ln(2, LL)
            ac1 = acL(LL); ac2 = 1d0 - ac1
            !
            hwavL = max(ac1 * hwav(k1) + ac2 * hwav(k2), 0.01d0)
            twavL = max(ac1 * twav(k1) + ac2 * twav(k2), 0.1d0)
            fmax = facmax * hu(LL)**1.5 / twavL
            rhoL = rhomean
            !
            ! Surface force, assign to top layer
            !
            wavfx = ac1 * sxwav(k1) + ac2 * sxwav(k2)
            wavfy = ac1 * sywav(k1) + ac2 * sywav(k2)
            wavfu_loc = csu(LL) * wavfx + snu(LL) * wavfy
            wavfv_loc = -snu(LL) * wavfx + csu(LL) * wavfy
            wavfu(Lt) = sign(min(abs(wavfu_loc), fmax), wavfu_loc) / rhoL / max(hu(LL) - hu(Lt - 1), hminlw) ! top layer, as in D3D
            wavfv(Lt) = sign(min(abs(wavfv_loc), fmax), wavfv_loc) / rhoL / max(hu(LL) - hu(Lt - 1), hminlw) ! this limitation only works in sigma layers
            !
            ! The following is pretty inaccurate for limited nr of layers:
            !
            !wavfuL    = 4d0*sign(min(abs(wavfu_loc), fmax), wavfu_loc)/hwavL
            !wavfvL    = 4d0*sign(min(abs(wavfv_loc), fmax), wavfv_loc)/hwavL          ! hwavL/4 is integral over 0.5*hwavL waterdepth of linear decrease
         !! Check if first layer is thicker than 0.5*hrms
         !! In that case, distribute over first layer
            !dzu=hu(Lt)-hu(Lt-1)
            !halfwav=0.5*hwavL
         !!
            !if (dzu > halfwav) then
            !   wavfu(Lt) = wavfuL*0.25d0*hwavL/rhoL/dzu                               ! division by 0.5*hrms done above
            !   wavfv(Lt) = wavfvL*0.25d0*hwavL/rhoL/dzu
            !else
            !zw=0.5d0*dzu
            !do L=Lt,Lb+1,-1
            !   if (zw<=halfwav) then
            !      wavfu(L) = wavfuL*(1d0-2d0*zw/hwavL)/rhoL                                     ! division by 0.5*hrms done above
            !      wavfv(L) = wavfvL*(1d0-2d0*zw/hwavL)/rhoL
            !      zw = zw + 0.5*(hu(L)-hu(L-2))
            !   elseif (zw>halfwav .and. hu(L)>(hu(LL)-halfwav)) then                        ! partial layer
            !      cc = 0.5*(hu(L)+(hu(LL)-halfwav))                                           ! replaced layer center
            !      zw = hu(LL)-cc                                                                ! depth below surface
            !      wavfu(L) = wavfuL*(1d0-zw/halfwav)/rhoL                                     ! contribution over partial layer
            !      wavfv(L) = wavfvL*(1d0-zw/halfwav)/rhoL
            !   endif
            !enddo
            !endif
            !
            ! Body forces, uniform over depth
            !
            wavfx = ac1 * sbxwav(k1) + ac2 * sbxwav(k2)
            wavfy = ac1 * sbywav(k1) + ac2 * sbywav(k2)
            wavfu_loc = csu(LL) * wavfx + snu(LL) * wavfy
            wavfv_loc = -snu(LL) * wavfx + csu(LL) * wavfy
            do L = Lb, Lt
               wavfu(L) = wavfu(L) + sign(min(abs(wavfu_loc), fmax), wavfu_loc) / rhoL / max(hu(LL), hminlw)
               wavfv(L) = wavfv(L) + sign(min(abs(wavfv_loc), fmax), wavfv_loc) / rhoL / max(hu(LL), hminlw)
            end do
         end do
      end if
1234  continue
      return
   end subroutine setwavfu
