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

module m_setextforcechkadvec

   implicit none

   private

   public :: setextforcechkadvec

contains

   subroutine setextforcechkadvec()
      use precision, only: dp
      use m_get_spiralforce
      use m_get_spiral3d
      use m_comp_gravinput, only: comp_GravInput
      use m_anticreep, only: anticreep
      use m_add_internaltidesfrictionforces, only: add_InternalTidesFrictionForces
      use m_add_baroclinic_pressure, only: add_baroclinic_pressure
      use m_flow
      use m_flowparameters, only: trshcorio
      use m_flowgeom
      use m_netw
      use MessageHandling
      use m_alloc
      use m_wind
      use m_sferic
      use m_xbeach_data, only: Lwave
      use m_fm_icecover, only: ice_pressure, fm_ice_update_press, ice_apply_pressure, ice_reduce_waves, ice_area_fraction, ice_apply_friction, ice_frict_type, ice_frcuni, FRICT_AS_DRAG_COEFF
      use m_get_Lbot_Ltop
      use m_get_chezy, only: get_chezy
      use m_links_to_centers, only: links_to_centers
      use m_waveconst

      integer :: L, LL, Lb, Lt, k1, k2, kt1, kt2
      real(kind=dp) :: dptot, tidp, trshcorioi, dzt, dztm, alf
      real(kind=dp) :: wfac, Dzk
      real(kind=dp) :: uixL, uiyL, uL, vL, uxL, uyL, duxL, duyL, duL, cdi, ice_shear

      trshcorioi = 1.0_dp / trshcorio

      if (jawind > 0) then

         if (kmx == 0) then
            do LL = 1, lnx
               if (hu(LL) > 0) then
                  wfac = 1.0_dp
                  if (jawindpartialdry == 1) then
                     Dzk = abs(zk(lncn(1, LL)) - zk(lncn(2, LL)))
                     if (Dzk > 0.0_dp) then
                        wfac = min(1.0_dp, hu(LL) / Dzk)
                     end if
                  end if
                  ! wdsu/huvli = [(m^2/s^2)*m^-1]
                  if (jawindhuorzwsbased == 0) then
                     adve(LL) = adve(LL) - wdsu(LL) * wfac / hu(LL)
                  else
                     adve(LL) = adve(LL) - wdsu(LL) * wfac * huvli(LL)
                  end if
               end if
            end do

         else

            do LL = 1, lnx
               if (hu(LL) > 0.0_dp) then
                  wfac = 1.0_dp
                  if (jawindpartialdry == 1) then
                     Dzk = abs(zk(lncn(1, LL)) - zk(lncn(2, LL)))
                     if (Dzk > 0.0_dp) then
                        wfac = min(1.0_dp, hu(LL) / Dzk)
                     end if
                  end if

                  Lt = Ltop(LL)

                  alf = 1.0_dp
                  if (jawindhuorzwsbased == 0 .and. Lt > 1) then
                     dzt = hu(Lt) - hu(Lt - 1)
                  else
                     kt1 = ktop(ln(1, LL))
                     kt2 = ktop(ln(2, LL))
                     dzt = acL(LL) * (zws(kt1) - zws(kt1 - 1)) + (1.0_dp - acL(LL)) * (zws(kt2) - zws(kt2 - 1))
                  end if
                  if (Lbot(LL) < Lt .and. Lt > 2) then
                     dztm = hu(Lt - 1) - hu(Lt - 2)
                     if (dzt < 0.05_dp) then
                        alf = dzt / (dzt + dztm)
                        adve(Lt - 1) = adve(Lt - 1) - (1.0_dp - alf) * wdsu(LL) * wfac / dztm
                     end if
                  end if
                  adve(Lt) = adve(Lt) - alf * wdsu(LL) * wfac / dzt
               end if
            end do

         end if

      end if

      if (ice_apply_friction) then
         do LL = 1, lnx
            if (hu(LL) > 0) then
               Lt = Ltop(LL)

               ! for the moment the ice is always stagnant
               uixL = 0.0_dp
               uiyL = 0.0_dp

               ! flow velocity
               uL = U1(Lt)
               vL = v(Lt)
               uxL = uL * csu(LL) - vL * snu(LL)
               uyL = uL * snu(LL) + vL * csu(LL)

               ! velocity difference
               duxL = uixL - uxL
               duyL = uiyL - uyL
               duL = sqrt(duxL * duxL + duyL * duyL)

               ! get drag coefficient
               if (ice_frict_type == FRICT_AS_DRAG_COEFF) then
                  cdi = ice_frcuni
               else
                  cdi = 0.0_dp
               end if

               wfac = 0.5_dp * (ice_area_fraction(ln(1, LL)) + ice_area_fraction(ln(2, LL)))
               ice_shear = wfac * cdi * duL * (duxL * csu(LL) + duyL * snu(LL)) ! * rhomean?

               if (kmx > 0) then
                  alf = 1.0_dp
                  if (jawindhuorzwsbased == 0 .and. Lt > 1) then
                     dzt = hu(Lt) - hu(Lt - 1)
                  else
                     kt1 = ktop(ln(1, LL))
                     kt2 = ktop(ln(2, LL))
                     dzt = acL(LL) * (zws(kt1) - zws(kt1 - 1)) + (1.0_dp - acL(LL)) * (zws(kt2) - zws(kt2 - 1))
                  end if
                  if (Lbot(LL) < Lt .and. Lt > 2) then
                     dztm = hu(Lt - 1) - hu(Lt - 2)
                     if (dzt < 0.05_dp) then
                        alf = dzt / (dzt + dztm)
                        adve(Lt - 1) = adve(Lt - 1) - (1.0_dp - alf) * ice_shear / dztm
                     end if
                  end if
                  adve(Lt) = adve(Lt) - alf * ice_shear / dzt

               elseif (jawindhuorzwsbased == 0) then
                  adve(LL) = adve(LL) - ice_shear / hu(LL)
               else
                  adve(LL) = adve(LL) - ice_shear * huvli(LL)
               end if
            end if
         end do
      end if

      if ((jawave == WAVE_SWAN_ONLINE .or. jawave == WAVE_NC_OFFLINE .or. (jawave == WAVE_SURFBEAT .and. lwave == 1)) .and. .not. flow_without_waves) then
         ! add wave forces to adve
         if (kmx == 0) then ! 2D
            do L = 1, lnx
               wfac = 1.0_dp
               if (ice_reduce_waves) then
                  wfac = wfac * (1.0_dp - 0.5_dp * (ice_area_fraction(ln(1, L)) + ice_area_fraction(ln(2, L))))
               end if
               adve(L) = adve(L) - wfac * wavfu(L)
            end do
         else
            do LL = 1, lnx
               wfac = 1.0_dp
               if (ice_reduce_waves) then
                  wfac = wfac * (1.0_dp - 0.5_dp * (ice_area_fraction(ln(1, LL)) + ice_area_fraction(ln(2, LL))))
               end if
               call getLbotLtop(LL, Lb, Lt)
               if (Lt < Lb) then
                  cycle
               end if
               do L = Lb, Lt
                  adve(L) = adve(L) - wfac * wavfu(L) ! Dimensions [m/s^2]
               end do
            end do
         end if
      end if

      if (ice_apply_pressure) then
         call fm_ice_update_press(ag)
      end if

      if (air_pressure_available .or. pseudo_air_pressure_available .or. water_level_correction_available &
          .or. jatidep > 0 .or. ice_apply_pressure) then
         do L = 1, lnx
            if (hu(L) > 0) then
               k1 = ln(1, L)
               k2 = ln(2, L)

               dptot = 0.0_dp
               if (air_pressure_available) then
                  dptot = dptot + (air_pressure(k2) - air_pressure(k1)) * dxi(L) / rhomean
               end if
               if (pseudo_air_pressure_available) then
                  dptot = dptot + (pseudo_air_pressure(k2) - pseudo_air_pressure(k1)) * dxi(L) / rhomean
               end if
               if (water_level_correction_available) then
                  dptot = dptot + (water_level_correction(k2) - water_level_correction(k1)) * dxi(L) * ag
               end if
               if (ice_apply_pressure) then
                  dptot = dptot + (ice_pressure(k2) - ice_pressure(k1)) * dxi(L) / rhomean
               end if
               if (jatidep > 0 .or. jaselfal > 0) then
                  if (jatidep == 1) then
                     tidp = (tidep(1, k2) - tidep(1, k1)) * dxi(L)
                  else
                     tidp = tidef(L)
                  end if
                  if (hu(L) < trshcorio) then
                     tidp = tidp * hu(L) * trshcorioi
                  end if
                  dptot = dptot - tidp
                  if (jatidep == 1) then ! todo: check if you now get desired outputting if jatidep==2
                     tidef(L) = tidp ! add to tidal forces
                  end if
               end if

               if (kmx == 0) then
                  adve(L) = adve(L) + dptot
               else
                  do LL = Lbot(L), Ltop(L)
                     adve(LL) = adve(LL) + dptot
                  end do
               end if
            end if
         end do

         if (jatidep > 0 .or. jaselfal > 0 .and. kmx == 0) then
            call comp_GravInput()
         end if

      end if

      ! Anti-creep
      if (kmx < 2 .and. jacreep == 1) then ! A warning due to kmx<2 and anticreep on
         call mess(LEVEL_INFO, 'Error : Anti-creep must be switched off in a 1d/2d model!')
      end if

      if (use_density()) then ! Baroclinic pressure
         if (jacreep == 1) then
            dsalL = 0.0_dp
            dtemL = 0.0_dp
            do L = 1, lnx
               if (hu(L) > 0.0_dp) then
                  call anticreep(L)
               end if
            end do
         else
            call add_baroclinic_pressure()
         end if
      end if

      if (jasecflow > 0) then ! Secondary Flow

         do LL = 1, lnx
            czusf(LL) = get_chezy(hu(LL), frcu(LL), u1(LL), v(LL), ifrcutp(LL)) ! calculating chezy coefficient on the flow links
         end do

         if (kmx < 2) then
            call links_to_centers(czssf, czusf) ! converting chezy coefficient to the flow nodes
            if (spirbeta > 0.0_dp) then
               call get_spiralforce()
            end if
         else
            call get_spiral3d() ! compute equivalent secondary flow intensity
         end if
      end if

      if (jaFrcInternalTides2D > 0 .and. kmx == 0) then ! internal tides friction (2D only)
         call add_InternalTidesFrictionForces()
      end if

      if (chkadvd > 0) then ! avoid drying from advection, stress or wind (all within adve)

         if (kmx == 0) then

            do L = 1, lnx

               if (hu(L) > 0) then
                  k1 = ln(1, L)
                  k2 = ln(2, L)

                  if (hs(k1) < 0.5_dp * hs(k2)) then
                     if (adve(L) < 0 .and. hs(k1) < chkadvd) then
                        adve(L) = adve(L) * hs(k1) / chkadvd
                     end if
                  else if (hs(k2) < 0.5_dp * hs(k1)) then
                     if (adve(L) > 0 .and. hs(k2) < chkadvd) then
                        adve(L) = adve(L) * hs(k2) / chkadvd
                     end if
                  end if
               end if

            end do

         else

            do LL = 1, lnx
               if (hu(LL) > 0.0_dp) then
                  call getLbotLtop(LL, Lb, Lt)
                  k1 = ln(1, LL)
                  k2 = ln(2, LL)
                  if (hs(k1) < 0.5_dp * hs(k2)) then
                     do L = Lb, Lt
                        if (adve(L) < 0 .and. hs(k1) < chkadvd) then
                           adve(L) = adve(L) * hs(k1) / chkadvd
                        end if
                     end do
                  else if (hs(k2) < 0.5_dp * hs(k1)) then
                     do L = Lb, Lt
                        if (adve(L) > 0 .and. hs(k2) < chkadvd) then
                           adve(L) = adve(L) * hs(k2) / chkadvd
                        end if
                     end do
                  end if
               end if
            end do

         end if

      end if

   end subroutine setextforcechkadvec

end module m_setextforcechkadvec
