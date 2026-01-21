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

module m_wave_shear_velocity

   implicit none

   private

   public :: compute_wave_shear_velocity

contains

   subroutine compute_wave_shear_velocity(Hrms, Tsig, Depth, Uorbi, rlabd, ust)
      use precision, only: dp
      use m_getwavenr, only: getwavenr
      use m_sferic, only: twopi, pi
      use m_waves, only: gammax, jauorb

      implicit none
      real(kind=dp) :: Hrms, Tsig, Depth, uorbi, hrm, ust
      real(kind=dp) :: shs,omeg
      real(kind=dp) :: rk, rlabd, arms
      real(kind=dp), external :: tanhsafe, sinhsafe, sinhsafei

      if (depth < 0.01_dp .or. Tsig < 0.1_dp) then ! flume cases with wave nr 5
         Uorbi = 0.0_dp
         rlabd = 0.0_dp
         ust = 0.0_dp
      else
         call getwavenr(depth, tsig, rk)
         hrm = min(Hrms, gammax * depth)
         arms = 0.5_dp * hrm
         omeg = twopi / tsig
         shs = sinhsafei(rk * depth)
         uorbi = omeg * arms * shs !omeg*(0.5*hsig)
         if (jauorb == 0) then ! for consistency with old d3d convention
            uorbi = uorbi * sqrt(pi) / 2.0_dp
         end if
         ust = 0.5_dp * omeg * arms * arms / depth
         rlabd = twopi / rk
      end if

      return

   end subroutine compute_wave_shear_velocity

end module m_wave_shear_velocity
