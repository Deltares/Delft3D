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
module m_dslim
   implicit none
contains
   real(kind=dp) function dslim(d1, d2, limtyp)
      use precision, only: dp
      use m_dminmod
      use m_dvanleer
      use m_dkoren
      use m_dcentral
      use m_dlimiter
      use m_dcminmod
      use m_dsuperbee
      use m_dlimitercentral

      real(kind=dp) d1, d2 ! voorslope, naslope
      integer limtyp

      ! In order to translate psi to limiter, you have to multiply the psi function with ds2
      ! e.g. lax wendroff central: psi=1, dslimiter=d2

      if (limtyp == 0) then
         dslim = 0
      else if (limtyp == 1) then ! codering guus, met voorslope
         dslim = d1 * dminmod(d1, d2)
      else if (limtyp == 2) then ! codering guus, met voorslope
         dslim = d1 * dvanleer(d1, d2)
      else if (limtyp == 3) then ! codering guus, met voorslope
         dslim = d1 * dkoren(d1, d2)
      else if (limtyp == 4) then ! monotonized central no division
         dslim = dcentral(d1, d2)
      else if (limtyp == 5) then ! monotonized central Sander with division
         dslim = dlimiter(d1, d2, limtyp) * d2
      else if (limtyp == 6) then ! monotonized central Sander with division, upwind slope ds1 at central cel
         dslim = dlimitercentral(d1, d2, limtyp)
      else if (limtyp == 11) then ! standaard codering
         dslim = d2 * dminmod(d1, d2)
      else if (limtyp == 12) then ! standaard codering
         dslim = d2 * dvanleer(d1, d2)
      else if (limtyp == 13) then ! standaard codering
         dslim = d2 * dkoren(d1, d2)
      else if (limtyp == 14) then ! monotonized central, == 4
         dslim = dcentral(d2, d1)
      else if (limtyp == 15) then ! minmod central
         dslim = dcminmod(d2, d1)
      else if (limtyp == 20) then ! leftbiased, beam&warming
         dslim = d1
      else if (limtyp == 21) then ! central
         dslim = d2
      else if (limtyp == 22) then ! superbee
         dslim = dsuperbee(d1, d2)
      else
         dslim = 0d0
      end if
      return
   end function dslim
end module m_dslim
