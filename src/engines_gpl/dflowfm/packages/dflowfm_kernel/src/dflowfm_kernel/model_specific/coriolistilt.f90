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

module m_coriolistilt
   use m_statisticsonemorepoint, only: statisticsonemorepoint
   use m_statisticsnewstep, only: statisticsnewstep
   use m_statisticsfinalise, only: statisticsfinalise

   implicit none

   private

   public :: coriolistilt

contains

   subroutine coriolistilt(tim)
      use precision, only: dp
      use m_netw
      use m_flowgeom
      use m_flow
      use m_sferic
      use unstruc_display
      use m_set_bobs

      integer :: k, L
      real(kind=dp) :: s1k, yy, samp, ux, uy, dif, alf, tim

      ux = 0.1d0; uy = 0d0; samp = ux * fcorio / ag
      if (tim == 0d0) then

         do k = 1, numk
            alf = (yk(k) - ykmin) / (ykmax - ykmin)
            zk(k) = -600d0 + 500d0 * cos(pi * alf)
         end do

         call setbobs()

         do L = 1, lnx
            u1(L) = csu(L) * ux + snu(L) * uy
         end do
      end if

      call statisticsnewstep()

      do k = 1, ndx
         yy = yz(k)
         s1k = -samp * yy

         if (tim == 0d0) then
            s1(k) = max(bl(k), s1k); s0(k) = s1(k)
         end if

         dif = abs(s1(k) - s1k)
         call statisticsonemorepoint(dif)
      end do

      call statisticsfinalise()
   end subroutine coriolistilt

end module m_coriolistilt
