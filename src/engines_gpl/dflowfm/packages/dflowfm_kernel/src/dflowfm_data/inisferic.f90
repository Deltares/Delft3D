!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module m_ini_sferic
   implicit none
contains
   !> initialize sferical coordinate frame
   subroutine inisferic()
      use precision, only: dp
      use m_sferic, only: pi, twopi, dg2rd, rd2dg, omega, fcorio, anglat, dy2dg, ra

      real(kind=dp) :: sidereal
      pi = acos(-1.0_dp)
      twopi = 2.0_dp * pi
      dg2rd = pi / 180.0_dp
      rd2dg = 180.0_dp / pi
      sidereal = 23.0_dp * 3600.0_dp + 56.0_dp * 60.0_dp + 4.1_dp
      omega = twopi / sidereal
      fcorio = 2.0_dp * omega * sin(anglat * dg2rd)
      dy2dg = rd2dg / ra
   end subroutine inisferic
end module m_ini_sferic
