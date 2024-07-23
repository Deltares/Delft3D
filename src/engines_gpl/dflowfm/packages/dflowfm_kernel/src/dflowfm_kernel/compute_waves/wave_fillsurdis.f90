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

   subroutine wave_fillsurdis(k, surdis)
      use m_waves
      use m_xbeach_data, only: DR, D, roller
      use m_flowparameters, only: jawave
      use m_flow, only: s1, epshu
      use m_flowgeom, only: bl
      use m_sferic

      implicit none

      integer, intent(in) :: k
      double precision, intent(out) :: surdis

      double precision :: rk
      double precision :: hsk

      select case (jawave)
      case (3)
         surdis = dsurf(k) + dwcap(k)
      case (4)
         if (roller > 0) then
            surdis = DR(k)
         else
            surdis = D(k)
         end if
      case (1, 2, 5)
         hsk = s1(k) - bl(k)
         if (hsk > epshu) then
            if (twav(k) < 0.1d0) then
               surdis = 0d0
               return
            end if
            rk = 2 * pi / rlabda(k)
            call wave_statbreakerdis(hsk, hwav(k), twav(k), rk, surdis)
         else
            surdis = 0d0
         end if
      end select

   end subroutine wave_fillsurdis
