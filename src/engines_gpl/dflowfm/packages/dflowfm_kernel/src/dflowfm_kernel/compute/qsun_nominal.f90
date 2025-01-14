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

module m_qsun_nominal

   implicit none

contains

   subroutine qsun_nominal(rlon, rlat, timhr, qs)
      use precision, only: dp
      use m_sferic
      use m_flowtimes, only: timjan, tzone
      implicit none

      real(kind=dp) :: rlat, rlon, timhr, qs

      real(kind=dp) :: decln, w0, w1, d, e, tm, snh

      ! Calculate sine of the angle of the sun above the horizon: SNH
      ! d is the declination angle
      ! June 21st is the 171st day after TM=0

      tm = timjan + timhr
      !if (jsferic > 0) then
      tm = tm + 24.0d0 * rlon / 360.0d0 - tzone
      !endif
      w0 = twopi / (365.24d0 * 24d0)
      w1 = twopi / (24d0)
      decln = 23.5d0 * dg2rd
      d = decln * cos(w0 * tm - 2.950d0)
      e = rlat * dg2rd
      snh = -cos(e) * cos(d) * cos(w1 * tm) + sin(e) * sin(d)
      snh = max(0d0, min(1d0, snh))
      qs = 1368d0 * snh * 0.76d0
   end subroutine qsun_nominal

end module m_qsun_nominal
