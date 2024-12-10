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

module m_duneaval

   implicit none

   private

   public :: duneaval

contains

   subroutine duneaval(error)
      use precision, only: dp
      use m_fm_erosed
      use m_sediment
      use m_flowgeom
      use m_flow
      use message_module

      implicit none

      logical, intent(out) :: error

      integer :: k1, k2, L, lsd, ac1, ac2
      real(kind=dp) :: slp, slpmax, avflux, maxflux
      real(kind=dp) :: fixf, frc

      error = .true.
      avalflux = 0d0

      do L = 1, lnx
         if (wu_mor(L) == 0d0) cycle
         k1 = ln(1, L); k2 = ln(2, L)
         ac1 = acL(L); ac2 = 1d0 - ac1
         if (hs(k1) > hswitch .or. hs(k2) > hswitch) then
            slpmax = wetslope
         else
            slpmax = dryslope
         end if
         !
         slp = sqrt(e_dzdn(L) * e_dzdn(L) + e_dzdt(L) * e_dzdt(L))
         if (slp > slpmax) then
            avflux = (bl(k2) - bl(k1) + slpmax * e_dzdn(L) / slp * Dx(L)) / avaltime / max(morfac, 1d0)
            do lsd = 1, lsedtot
               !
               ! Apply upwind sediment availability for structures
               !
               if (L > lnxi) then ! boundary link
                  fixf = fixfac(k2, lsd)
                  frc = frac(k2, lsd)
               else ! interior link
                  if (avflux >= 0) then
                     fixf = fixfac(k1, lsd) ! outward positive
                     frc = frac(k1, lsd)
                  else
                     fixf = fixfac(k2, lsd)
                     frc = frac(k2, lsd)
                  end if
               end if

               avflux = avflux * fixf * frc
               maxflux = dzmaxdune / max(morfac, 1d0)

               if (abs(maxflux) < abs(avflux)) then
                  if (avflux > 0) then
                     avflux = min(avflux, maxflux)
                  else
                     avflux = max(avflux, -maxflux)
                  end if
               end if
               !
               avalflux(L, lsd) = avalflux(L, lsd) - ba(k1) * ba(k2) / (ba(k1) + ba(k2)) * avflux * rhosol(lsd) / wu_mor(L)
            end do
         end if
      end do
      !
      error = .false.
1234  continue
   end subroutine duneaval

end module m_duneaval
