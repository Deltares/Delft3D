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

module m_qsun_nominal

   implicit none

contains

   !> Calculate nominal solar radiation
   pure function calculate_nominal_solar_radiation(anglon, anglat, time_in_hours) result(nominal_solar_radiation)
      use precision, only: dp
      use m_sferic, only: twopi, dg2rd
      use m_flowtimes, only: timjan, tzone

      real(kind=dp), intent(in) :: anglon !< Longitude angle in degrees
      real(kind=dp), intent(in) :: anglat !< Latitude angle in degrees
      real(kind=dp), intent(in) :: time_in_hours !< Current model time in hours

      real(kind=dp) :: nominal_solar_radiation, decln, w0, w1, d, e, tm, snh

      ! Calculate sine of the angle of the sun above the horizon: SNH
      ! d is the declination angle
      ! June 21st is the 171st day after TM=0

      tm = timjan + time_in_hours
      tm = tm + 24.0_dp * anglon / 360.0_dp - tzone
      w0 = twopi / (365.24_dp * 24.0_dp)
      w1 = twopi / (24.0_dp)
      decln = 23.5_dp * dg2rd
      d = decln * cos(w0 * tm - 2.95_dp)
      e = anglat * dg2rd
      snh = -cos(e) * cos(d) * cos(w1 * tm) + sin(e) * sin(d)
      snh = max(0.0_dp, min(1.0_dp, snh))
      nominal_solar_radiation = 1368.0_dp * snh * 0.76_dp
   end function calculate_nominal_solar_radiation

end module m_qsun_nominal
