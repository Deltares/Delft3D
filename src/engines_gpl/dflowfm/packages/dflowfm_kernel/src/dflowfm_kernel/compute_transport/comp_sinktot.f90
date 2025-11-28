!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

   subroutine comp_sinktot(comp_sinktot_method)
      use m_transport, only: ised1, isedn, sinksetot, constituents, sinkftot, jaupdate, ndeltasteps, nsubsteps
      use m_sediment, only: stm_included, mxgr, sedtra, stmpar
      use timers, only: timon, timstrt, timstop
      use m_flow, only: vol0, vol1, kmx
      use m_flowgeom, only: ndx
      use m_flowtimes, only: dts
      use precision, only: dp

      implicit none

      integer, intent(in) :: comp_sinktot_method !< 1: before transport step, 2: after transport step
      
      integer :: k, j, ll

      integer(4) :: ithndl = 0
      real(kind=dp) :: dt_loc

      if (.not. stm_included) return
      if (mxgr == 0) return
      if (timon) call timstrt("comp_sinktot", ithndl)

      if (comp_sinktot_method == 1) then 
         if (kmx < 1) then ! 2D
            do k = 1, ndx
               do j = ISED1, ISEDN
                  ll = j - ISED1 + 1
                  sinksetot(j, k) = sinksetot(j, k) + vol0(k) * sedtra%sinkse(k, ll) * constituents(j, k) * dts * real(nsubsteps, kind=dp)
                  !if (stmpar%morpar%flufflyr%iflufflyr > 0) then
                  !   sinkftot(j, k) = sinkftot(j, k) + vol0(k) * stmpar%morpar%flufflyr%sinkf(ll, k) * constituents(j, k) * dts
                  !end if
               end do
            end do
         else ! 3D
            do k = 1, ndx
               do j = ISED1, ISEDN
                  ll = j - ISED1 + 1
                  sinksetot(j, k) = sinksetot(j, k) + vol0(sedtra%kmxsed(k, ll)) * sedtra%sinkse(k, ll) * constituents(j, sedtra%kmxsed(k, ll)) * dts * real(nsubsteps, kind=dp)
                  !if (stmpar%morpar%flufflyr%iflufflyr > 0) then
                  !   sinkftot(j, k) = sinkftot(j, k) + vol0(sedtra%kmxsed(k, ll)) * stmpar%morpar%flufflyr%sinkf(ll, k) * constituents(j, sedtra%kmxsed(k, ll)) * dts
                  !end if
               end do
            end do
         end if
      else if (comp_sinktot_method == 2) then
         if (kmx < 1) then ! 2D
            do k = 1, ndx
            if (jaupdate(k) == 0) then
               cycle
            else
               dt_loc = dts * ndeltasteps(k)
            end if
            do j = ISED1, ISEDN
                  ll = j - ISED1 + 1
                  sinksetot(j, k) = sinksetot(j, k) + vol1(k) * sedtra%sink_im(k, ll) * constituents(j, k) * dt_loc
                  if (stmpar%morpar%flufflyr%iflufflyr > 0) then
                     sinkftot(j, k) = sinkftot(j, k) + vol1(k) * stmpar%morpar%flufflyr%sinkf(ll, k) * constituents(j, k) * dt_loc
                  end if
               end do
            end do
         else ! 3D
            do k = 1, ndx
               if (jaupdate(k) == 0) then
                  cycle
               else
                  dt_loc = dts * ndeltasteps(k)
               end if
               do j = ISED1, ISEDN
                  ll = j - ISED1 + 1
                  sinksetot(j, k) = sinksetot(j, k) + vol1(sedtra%kmxsed(k, ll)) * sedtra%sink_im(k, ll) * constituents(j, sedtra%kmxsed(k, ll)) * dt_loc
                  if (stmpar%morpar%flufflyr%iflufflyr > 0) then
                     sinkftot(j, k) = sinkftot(j, k) + vol1(sedtra%kmxsed(k, ll)) * stmpar%morpar%flufflyr%sinkf(ll, k) * constituents(j, sedtra%kmxsed(k, ll)) * dt_loc
                  end if
               end do
            end do
         end if
      end if 
      if (timon) call timstop(ithndl)
   end subroutine comp_sinktot

end module m_comp_sinktot
