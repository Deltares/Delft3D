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

module m_compute_wave_parameters
   use m_wave_uorbrlabda, only: wave_uorbrlabda
   use m_wave_comp_stokes_velocities, only: wave_comp_stokes_velocities
   use m_tauwavehk, only: tauwavehk

   implicit none

   private

   public :: compute_wave_parameters

contains

   ! compute uorb, rlabda for input in other subroutines
   subroutine compute_wave_parameters()
      use precision, only: dp
      use m_xbeach_data
      use m_waves
      use m_flow, only: jawave, s1, kmx, jawavestokes, hu, flowwithoutwaves, epshu, wx, wy, ag, hs, waveforcing
      use m_flowgeom, only: bl, lnx, ln, csu, snu, ndx
      !   use m_sferic
      !   use m_flowtimes
      use mathconsts, only: sqrt2_hp
      use m_transform_wave_physics, only: transform_wave_physics_hp
      !   use unstruc_display

      integer :: k1, k2, k, L
      integer :: ierror
      real(kind=dp) :: hh, hw, tw, cs, sn, uorbi, rkw, ustt, uwi

      ! Fetch models
      !
      if (jawave < 3 .and. .not. flowWithoutWaves) then ! Every timestep, not only at getfetch updates, as waterdepth changes
         ! get ustokes, vstokes for 2D, else in update_verticalprofiles getustwav
         hwav = min(hwav, gammax * max(s1 - bl, 0d0))
         if (kmx == 0 .and. jawavestokes > 0) then
            do L = 1, lnx
               k1 = ln(1, L); k2 = ln(2, L)
               hh = hu(L); 
               if (hh <= epshu) then
                  ustokes(L) = 0d0; vstokes(L) = 0d0
               else
                  hw = 0.5d0 * (hwav(k1) + hwav(k2)); tw = .5d0 * (twav(k1) + twav(k2))
                  uwi = sqrt(wx(L) * wx(L) + wy(L) * wy(L))
                  if (uwi > 0d0) then
                     cs = wx(L) / uwi
                     sn = wy(L) / uwi
                  else
                     cs = 1d0; sn = 0d0
                  end if
                  call tauwavehk(hw, tw, hh, uorbi, rkw, ustt)
                  ustokes(L) = ustt * (csu(L) * cs + snu(L) * sn)
                  vstokes(L) = ustt * (-snu(L) * cs + csu(L) * sn)
               end if
            end do
         end if
         !
         call wave_uorbrlabda()

      end if

      ! SWAN
      if ((jawave == 3 .or. jawave >= 6) .and. .not. flowWithoutWaves) then
         if (jawave == 6 .or. jawave == 7) then
            ! HSIG is read from SWAN NetCDF file. Convert to HRMS
            hwav = hwavcom / sqrt2_hp
         else
            hwav = hwavcom
         end if
         hwav = min(hwav, gammax * hs)
         twav = twavcom
         !
         ! Needed here, because we need wave mass fluxes to calculate stokes drift
         if (jawave == 7) then
            call transform_wave_physics_hp(hwavcom, phiwav, twavcom, hs, &
                               & sxwav, sywav, mxwav, mywav, &
                               & distot, dsurf, dwcap, &
                               & ndx, 1, hwav, twav, &
                               & ag, .true., waveforcing, &
                               & JONSWAPgamma0, sbxwav, sbywav, ierror)
         end if
         !
         call wave_uorbrlabda() ! twav is potentially changed above
         !
         if (kmx == 0) then
            call wave_comp_stokes_velocities()
         end if
      end if
      !
      if ((jawave == 3 .or. jawave >= 6) .and. flowWithoutWaves) then
         ! Exceptional situation: use wave info not in FLOW, only in WAQ
         ! Only compute uorb
         ! Works both for 2D and 3D
         if (jawave == 6 .or. jawave == 7) then
            ! HSIG is read from SWAN NetCDF file. Convert to HRMS
            hwav = hwavcom / sqrt2_hp
         else
            hwav = hwavcom
         end if
         hwav = min(hwav, gammax * hs)
         twav = twavcom
         call wave_uorbrlabda() ! hwav gets depth-limited here
      end if
      !
      ! Surfbeat model
      if (jawave == 4) then
         ! pro memore
      end if
      !
      ! Uniform wave field
      if (jawave == 5 .and. .not. flowWithoutWaves) then
         do k = 1, ndx
            hwav(k) = min(hwavuni, gammax * (s1(k) - bl(k)))
         end do
         if (kmx == 0 .and. jawavestokes > 0) then
            do L = 1, lnx
               k1 = ln(1, L); k2 = ln(2, L)
               hh = hu(L); 
               if (hh <= epshu) then
                  ustokes(L) = 0d0; vstokes(L) = 0d0
               else
                  hw = 0.5d0 * (hwav(k1) + hwav(k2)); tw = .5d0 * (twav(k1) + twav(k2))
                  cs = 0.5d0 * (cosd(phiwav(k1)) + cosd(phiwav(k2)))
                  sn = 0.5d0 * (sind(phiwav(k1)) + sind(phiwav(k2)))
                  call tauwavehk(hw, tw, hh, uorbi, rkw, ustt)
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
         ustokes = 0d0; vstokes = 0d0
      end if

1234  continue
      return
   end subroutine compute_wave_parameters

end module m_compute_wave_parameters
