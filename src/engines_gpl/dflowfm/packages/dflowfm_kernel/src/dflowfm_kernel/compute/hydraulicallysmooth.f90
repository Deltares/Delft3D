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

contains

   subroutine hydraulicallysmooth(umod, h, sqcf)
      use precision, only: dp
      use m_physcoef
      use m_flow
      implicit none
      real(kind=dp) :: umod, h, sqcf
      real(kind=dp) :: r, rv = 123.8d0, e = 8.84d0, eps = 1d-2, s, sd, er, ers

      r = umod * h / viskin ! Local re-number:
      r = max(r, 0.001d0)
      er = e * r
      if (r < rv) then ! Viscous sublayer:
         s = sqrt(r)
      else

         s = 12d0 ! In log-layer; initial trial for s:
100      continue
         sd = s
         ers = max(er / sd, 1.0001d0)
         s = log(ers) / vonkar

         if (abs(sd - s) > (eps * s)) then
            go to 100 ! Convergence criterium:
         end if
      end if

      if (s > 0d0) then
         sqcf = 1d0 / s
      else
         sqcf = 0d0
      end if

   end subroutine hydraulicallysmooth

end module m_hydraulicallysmooth
