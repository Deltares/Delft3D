!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_fill_rho

   implicit none

   private

   public :: fill_rho

contains

   subroutine fill_rho()
      use precision, only: dp
      use m_transport, only: constituents, const_sour, const_sink
      use m_flowgeom, only: ndx
      use m_flow, only: ndkx, sa1, rho, vol1, sq
      use timers, only: timon, timstrt, timstop
      use m_get_kbot_ktop, only: getkbotktop

      implicit none

      integer :: kk, k, kb, kt
      real(kind=dp) :: dvoli, dtol = 1.0e-8_dp

      integer(4) :: ithndl = 0

      if (timon) then
         call timstrt("fill_rho", ithndl)
      end if

      do k = 1, Ndkx
         sa1(k) = constituents(1, k)
         constituents(1, k) = rho(k)
      end do

!  sources
      do kk = 1, Ndx
         call getkbotktop(kk, kb, kt)
         do k = kb, kt
            dvoli = 1.0_dp / max(vol1(k), dtol)
            const_sour(1, k) = -rho(k) * sq(k) * dvoli
            const_sink(1, k) = 0.0_dp
         end do
      end do

      if (timon) then
         call timstop(ithndl)
      end if
      return
   end subroutine fill_rho

end module m_fill_rho
