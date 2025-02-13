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
module m_hydraulicallysmooth
   implicit none
   private

   public :: hydraulicallysmooth

contains

   pure function hydraulicallysmooth(umod, h) result(sqcf)
      use m_physcoef, only: viskin, vonkar
      use precision, only: dp
      implicit none

      real(kind=dp), intent(in) :: umod
      real(kind=dp), intent(in) :: h
      real(kind=dp) :: sqcf

      real(kind=dp), parameter :: rv = 123.8_dp, e = 8.84_dp, eps = 1e-2_dp
      real(kind=dp) :: r, s, sd, er, ers

      r = umod * h / viskin ! Local re-number:
      r = max(r, 1e-3_dp)
      if (r < rv) then ! Viscous sublayer:
         s = sqrt(r)
      else
         s = 12.0_dp ! In log-layer; initial trial for s:
         sd = 0.0_dp
         er = e * r
         do while (abs(sd - s) > (eps * s))
            sd = s
            ers = max(er / sd, 1.0001e0_dp)
            s = log(ers) / vonkar
         end do
      end if

      if (s > 0.0_dp) then
         sqcf = 1.0_dp / s
      else
         sqcf = 0.0_dp
      end if
   end function hydraulicallysmooth
end module m_hydraulicallysmooth
