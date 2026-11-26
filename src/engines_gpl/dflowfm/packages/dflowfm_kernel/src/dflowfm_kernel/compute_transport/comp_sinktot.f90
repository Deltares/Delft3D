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

module m_comp_sinktot

   implicit none

   private

   public :: comp_sinktot

contains

   subroutine comp_sinktot(nsubsteps, jaupdate, ndeltasteps)
      use precision, only: dp
      use m_transport, only: ised1, isedn, sinksetot, constituents, sinkftot
      use m_sediment, only: stm_included, mxgr, sedtra, stmpar
      use timers, only: timon, timstrt, timstop
      use m_flow, only: vol1, kmx
      use m_flowgeom, only: ndx, ndxi, kfs
      use m_flowtimes, only: dts

      implicit none

      integer, intent(in) :: nsubsteps
      integer, intent(in) :: jaupdate(ndx), ndeltasteps(ndx)
      integer :: k, j, ll, cell
      real(kind=dp) :: dt_loc

      integer(4) :: ithndl = 0

      if (.not. stm_included) then
         return
      end if
      if (mxgr == 0) then
         return
      end if
      if (timon) then
         call timstrt("comp_sinktot", ithndl)
      end if

      do k = 1, ndxi
         if (kmx > 0) then
            if (kfs(k) <= 0) then
               cycle
            end if
         end if
         dt_loc = dts
         if (nsubsteps > 1) then
            if (jaupdate(k) == 0) then
               cycle
            end if
            dt_loc = dts * ndeltasteps(k)
         end if
         ! Use the same final concentration and local time interval as the
         ! implicit transport solve, including cells updated only occasionally.
         do j = ISED1, ISEDN
            ll = j - ISED1 + 1
            cell = k
            if (kmx > 0) then
               cell = sedtra%kmxsed(k, ll)
            end if
            sinksetot(j, k) = sinksetot(j, k) + vol1(cell) * sedtra%sinkse(k, ll) * constituents(j, cell) * dt_loc
            if (stmpar%morpar%flufflyr%iflufflyr > 0) then
               sinkftot(j, k) = sinkftot(j, k) + vol1(cell) * stmpar%morpar%flufflyr%sinkf(ll, k) * constituents(j, cell) * dt_loc
            end if
         end do
      end do

      if (timon) then
         call timstop(ithndl)
      end if
   end subroutine comp_sinktot

end module m_comp_sinktot
