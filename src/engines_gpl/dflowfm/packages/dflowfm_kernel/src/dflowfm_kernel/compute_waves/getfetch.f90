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

module m_getfetch

   implicit none

   private

   public :: getfetch

contains

   subroutine getfetch(k, U10, FetchL, FetchD) !and windspeed
      use precision, only: dp
      use m_flow, only: Hs, Wx, Wy
      use m_waves, only: fetch, nwf, fetdp
      use m_sferic, only: twopi
      use m_get_link1
      implicit none

      integer :: k
      real(kind=dp) :: U10, FetchL, FetchD

      integer :: L, nw1, nw2
      real(kind=dp) :: alfa1, alfa2, dir

      FetchL = 0d0; FetchD = 0d0

      if (Hs(k) > 0d0) then
         call getlink1(k, L) ! het is maar voor wind
         U10 = sqrt(WX(L) * WX(L) + WY(L) * WY(L))
         if (U10 < 1d0) return

         DIR = atan2(WY(L), WX(L))
         if (DIR < 0d0) DIR = DIR + TWOPI

         dir = dir / twopi
         if (dir >= 1d0) dir = 0d0
         NW1 = DIR * (nwf - 1) + 1
         NW2 = NW1 + 1

         if (fetch(nw1, k) > 0d0 .or. fetch(nw2, k) > 0d0) then
            alfa2 = (nwf - 1) * (dir - dble(nw1 - 1) / dble(nwf - 1))
            alfa1 = 1d0 - alfa2
            fetchL = alfa1 * fetch(nw1, k) + alfa2 * fetch(nw2, k)
            fetchD = alfa1 * fetdp(nw1, k) + alfa2 * fetdp(nw2, k)
         end if
      end if
   end subroutine getfetch

end module m_getfetch
