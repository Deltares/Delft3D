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

module m_afhouwendammit
   use m_inkade

   implicit none

   private

   public :: afhouwendammit

contains

   subroutine afhouwendammit()
      use precision, only: dp
      use m_ship
      use m_shipcoor
      implicit none
      integer :: n, i, j
      real(kind=dp) :: sx1, sy1, sx2, sy2, eas, easm, frc

      ! kinetic e = potential e
      ! 0.5*m*u*u = 0.5*eas*dx*dx, u   = 5m/s, dx = 10 m indeuking => eas = deadw   potential energy = kinetic energy
      ! 0.5*m*u*u = 0.5*frc*u*dx   frc = mu/dx                                      friction labour  = kinetic energy

      do n = 1, nshiptxy

         eas = 0.25d0 * deadw(n); easm = 0.5d0 * eas
         frc = 0.5d0 * deadw(n)
         fextx(n) = 0d0; fexty(n) = 0d0; fextm(n) = 0d0

         sx1 = 0.9d0; sy1 = 0d0
         call shipcoor(n, sx1, sy1, sx2, sy2) ! midvoor
         call inkade(sx2, sy2, i, j)
         if (i == 1) then
            fextx(n) = fextx(n) + eas * (xmxs - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmxs - sx2) * (sy2 - shy(n))
         end if

         if (i == -1) then
            fextx(n) = fextx(n) + eas * (xmns - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmns - sx2) * (sy2 - shy(n))
         end if

         if (j == 1) then
            fexty(n) = fexty(n) + eas * (ymxs - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymxs - sy2) * (sx2 - shx(n))
         end if

         if (j == -1) then
            fexty(n) = fexty(n) + eas * (ymns - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymns - sy2) * (sx2 - shx(n))
         end if

         sx1 = 0.9d0; sy1 = 1d0
         call shipcoor(n, sx1, sy1, sx2, sy2) ! linksvoor
         call inkade(sx2, sy2, i, j)
         if (i == 1) then
            fextx(n) = fextx(n) + eas * (xmxs - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmxs - sx2) * (sy2 - shy(n))
         end if

         if (i == -1) then
            fextx(n) = fextx(n) + eas * (xmns - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmns - sx2) * (sy2 - shy(n))
         end if

         if (j == 1) then
            fexty(n) = fexty(n) + eas * (ymxs - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymxs - sy2) * (sx2 - shx(n))
         end if

         if (j == -1) then
            fexty(n) = fexty(n) + eas * (ymns - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymns - sy2) * (sx2 - shx(n))
         end if

         sx1 = 0.9d0; sy1 = -1d0
         call shipcoor(n, sx1, sy1, sx2, sy2) ! rechtsvoor
         call inkade(sx2, sy2, i, j)
         if (i == 1) then
            fextx(n) = fextx(n) + eas * (xmxs - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmxs - sx2) * (sy2 - shy(n))
         end if

         if (i == -1) then
            fextx(n) = fextx(n) + eas * (xmns - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmns - sx2) * (sy2 - shy(n))
         end if

         if (j == 1) then
            fexty(n) = fexty(n) + eas * (ymxs - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymxs - sy2) * (sx2 - shx(n))
         end if

         if (j == -1) then
            fexty(n) = fexty(n) + eas * (ymns - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymns - sy2) * (sx2 - shx(n))
         end if

         sx1 = -1d0; sy1 = 1d0
         call shipcoor(n, sx1, sy1, sx2, sy2) ! linksachter
         call inkade(sx2, sy2, i, j)
         if (i == 1) then
            fextx(n) = fextx(n) + eas * (xmxs - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmxs - sx2) * (sy2 - shy(n))
         end if

         if (i == -1) then
            fextx(n) = fextx(n) + eas * (xmns - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmns - sx2) * (sy2 - shy(n))
         end if

         if (j == 1) then
            fexty(n) = fexty(n) + eas * (ymxs - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymxs - sy2) * (sx2 - shx(n))
         end if

         if (j == -1) then
            fexty(n) = fexty(n) + eas * (ymns - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymns - sy2) * (sx2 - shx(n))
         end if

         sx1 = -1d0; sy1 = -1d0
         call shipcoor(n, sx1, sy1, sx2, sy2) ! rechtsachter
         call inkade(sx2, sy2, i, j)
         if (i == 1) then
            fextx(n) = fextx(n) + eas * (xmxs - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmxs - sx2) * (sy2 - shy(n))
         end if

         if (i == -1) then
            fextx(n) = fextx(n) + eas * (xmns - sx2)
            fricxi(n) = fricxi(n) + frc
            fextm(n) = fextm(n) - easm * (xmns - sx2) * (sy2 - shy(n))
         end if

         if (j == 1) then
            fexty(n) = fexty(n) + eas * (ymxs - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymxs - sy2) * (sx2 - shx(n))
         end if

         if (j == -1) then
            fexty(n) = fexty(n) + eas * (ymns - sy2)
            fricyi(n) = fricyi(n) + frc
            fextm(n) = fextm(n) + easm * (ymns - sy2) * (sx2 - shx(n))
         end if

      end do

   end subroutine afhouwendammit

end module m_afhouwendammit
