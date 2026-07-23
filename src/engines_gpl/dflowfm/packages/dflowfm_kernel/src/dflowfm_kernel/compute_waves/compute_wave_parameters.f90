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

module m_compute_wave_parameters
   use m_wave_uorbrlabda, only: wave_uorbrlabda
   use m_wave_comp_stokes_velocities, only: wave_comp_stokes_velocities
   use m_wave_shear_velocity, only: compute_wave_shear_velocity
   use m_waveconst

   implicit none

   private

   public :: compute_wave_parameters

contains

   ! compute uorb, rlabda for input in other subroutines
   subroutine compute_wave_parameters()
      use precision, only: dp
      use m_waves, only: hwav, gammax, ustokes, vstokes, twav, hwavcom, twavcom, phiwav, sxwav, sywav, mxwav, mywav, &
                distot, dsurf, dwcap, jonswapgamma0, sbxwav, sbywav, hwavuni, offline_wave_input_requirements
      use m_waveconst, only: wave_swan_online, no_stokes_drift, wave_nc_offline, wave_surfbeat, wave_uniform
      use m_flow, only: jawave, s1, kmx, jawavestokes, hu, flow_without_waves, epshu, ag, hs, waveforcing, jawaveforces
      use m_flowgeom, only: bl, lnx, ln, csu, snu, ndx
      use mathconsts, only: sqrt2_hp
      use m_transform_wave_physics, only: transform_wave_physics_hp, transform_wave_period_hp
      use m_wind, only: wx, wy

      integer :: k1, k2, k, L
      integer :: ierror
      real(kind=dp) :: hh, hw, tw, cs, sn, uorbi, rkw, ustt, uwi

      ! Fetch models
      !
      if (jawave < WAVE_SWAN_ONLINE .and. .not. flow_without_waves) then ! Every timestep, not only at getfetch updates, as waterdepth changes
         ! get ustokes, vstokes for 2D, else in update_verticalprofiles getustwav
         hwav = min(hwav, gammax * max(s1 - bl, 0.0_dp))
         if (kmx == 0 .and. jawavestokes > NO_STOKES_DRIFT) then
            do L = 1, lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               hh = hu(L)
               if (hh <= epshu) then
                  ustokes(L) = 0.0_dp
                  vstokes(L) = 0.0_dp
               else
                  hw = 0.5_dp * (hwav(k1) + hwav(k2))
                  tw = 0.5_dp * (twav(k1) + twav(k2))
                  uwi = sqrt(wx(L) * wx(L) + wy(L) * wy(L))
                  if (uwi > 0.0_dp) then
                     cs = wx(L) / uwi
                     sn = wy(L) / uwi
                  else
                     cs = 1.0_dp
                     sn = 0.0_dp
                  end if
                  call compute_wave_shear_velocity(hw, tw, hh, uorbi, rkw, ustt)
                  ustokes(L) = ustt * (csu(L) * cs + snu(L) * sn)
                  vstokes(L) = ustt * (-snu(L) * cs + csu(L) * sn)
               end if
            end do
         end if
         !
         call wave_uorbrlabda()

      end if

      ! Online SWAN coupling
      if (jawave == WAVE_SWAN_ONLINE) then
         hwav = min(hwavcom, gammax * hs)
         twav = twavcom
         call wave_uorbrlabda()
         if (kmx == 0 .and. .not. flow_without_waves) then
            call wave_comp_stokes_velocities()
         end if
      end if

      ! Offline wave coupling
      if (jawave == WAVE_NC_OFFLINE) then
         call compute_offline_wave_parameters()
      end if
      !
      ! Surfbeat model
      if (jawave == WAVE_SURFBEAT) then
         ! pro memore
      end if
      !
      ! Uniform wave field
      if (jawave == WAVE_UNIFORM .and. .not. flow_without_waves) then
         do k = 1, ndx
            hwav(k) = min(hwavuni, gammax * (s1(k) - bl(k)))
         end do
         if (kmx == 0 .and. jawavestokes > NO_STOKES_DRIFT) then
            do L = 1, lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               hh = hu(L)
               if (hh <= epshu) then
                  ustokes(L) = 0.0_dp
                  vstokes(L) = 0.0_dp
               else
                  hw = 0.5_dp * (hwav(k1) + hwav(k2))
                  tw = 0.5_dp * (twav(k1) + twav(k2))
                  cs = 0.5_dp * (cosd(phiwav(k1)) + cosd(phiwav(k2)))
                  sn = 0.5_dp * (sind(phiwav(k1)) + sind(phiwav(k2)))
                  call compute_wave_shear_velocity(hw, tw, hh, uorbi, rkw, ustt)
                  ustokes(L) = ustt * (csu(L) * cs + snu(L) * sn)
                  vstokes(L) = ustt * (-snu(L) * cs + csu(L) * sn)
               end if
            end do
         end if
         !
         call wave_uorbrlabda()
         !
      end if

      ! shortcut to switch off stokes influence
      if (jawavestokes == 0) then
         ustokes = 0.0_dp
         vstokes = 0.0_dp
      end if

1234  continue
      return

   contains

      !> Convert only the wave quantities needed by the active offline configuration.
      subroutine compute_offline_wave_parameters()
         integer :: forcing_for_transformation
         logical :: complete_wave_kinematics

         complete_wave_kinematics = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT) .and. &
                                    wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_PERIOD) .and. &
                                    wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_DIRECTION)

         if (complete_wave_kinematics) then
            forcing_for_transformation = WAVEFORCING_NO_WAVEFORCES
            if (jawaveforces > WAVE_FORCES_OFF) then
               forcing_for_transformation = waveforcing
            end if

            call transform_wave_physics_hp(hwavcom, phiwav, twavcom, hs, &
                                           sxwav, sywav, mxwav, mywav, &
                                           distot, dsurf, dwcap, &
                                           ndx, 1, hwav, twav, &
                                           ag, .true., forcing_for_transformation, &
                                           JONSWAPgamma0, sbxwav, sbywav, ierror)
            hwav = min(hwav, gammax * hs)
            call wave_uorbrlabda()

            if (kmx == 0 .and. .not. flow_without_waves) then
               call wave_comp_stokes_velocities()
            end if
         else
            hwav = 0.0_dp
            twav = 0.0_dp
            mxwav = 0.0_dp
            mywav = 0.0_dp
            sbxwav = 0.0_dp
            sbywav = 0.0_dp

            if (wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_PERIOD)) then
               call transform_wave_period_hp(twavcom, ndx, 1, JONSWAPgamma0, twav, ierror)
            end if
         end if
      end subroutine compute_offline_wave_parameters

   end subroutine compute_wave_parameters

end module m_compute_wave_parameters
