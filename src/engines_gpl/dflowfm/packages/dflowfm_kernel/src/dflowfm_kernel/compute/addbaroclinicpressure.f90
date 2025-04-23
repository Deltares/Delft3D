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

module m_addbaroclinicpressure

   implicit none

   private

   public :: addbaroclinicpressure

contains

   subroutine addbaroclinicpressure()
      use precision, only: dp
      use m_addbarocl, only: addbarocL, addbarocLrho_w
      use m_addbarocn, only: addbarocn, addbarocnrho_w
      use m_addbaroc, only: addbaroc
      use m_flowgeom, only: lnxi, lnx, ndx
      use m_flow, only: hu, kmx
      use m_turbulence, only: rvdn, grn
      use m_flowtimes
      use m_get_Lbot_Ltop
      use m_physcoef, only: jabarocponbnd, jarhointerfaces

      implicit none
      integer :: LL, Lb, Lt, n, lnxbc

      if (jabarocponbnd == 0) then
         lnxbc = lnxi
      else
         lnxbc = lnx
      end if

      if (kmx == 0) then
         !$OMP PARALLEL DO &
         !$OMP PRIVATE(LL)
         do LL = 1, lnxbc
            if (hu(LL) == 0.0_dp) then
               cycle
            end if
            call addbaroc(LL)
         end do
         !$OMP END PARALLEL DO
      else

         rvdn = 0.0_dp
         grn = 0.0_dp

         if (jarhointerfaces == 1) then
            !$OMP PARALLEL DO &
            !$OMP PRIVATE(n)
            do n = 1, ndx
               call addbarocnrho_w(n)
            end do
            !$OMP END PARALLEL DO

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(LL,Lb,Lt)
            do LL = 1, lnxbc
               if (hu(LL) == 0.0_dp) then
                  cycle
               end if
               call getLbotLtop(LL, Lb, Lt)
               if (Lt < Lb) then
                  cycle
               end if
               call addbarocLrho_w(LL, Lb, Lt)
            end do
            !$OMP END PARALLEL DO
         else

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(n)
            do n = 1, ndx
               call addbarocn(n)
            end do
            !$OMP END PARALLEL DO

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(LL,Lb,Lt)
            do LL = 1, lnxbc
               if (hu(LL) == 0.0_dp) then
                  cycle
               end if
               call getLbotLtop(LL, Lb, Lt)
               if (Lt < Lb) then
                  cycle
               end if
               call addbarocL(LL, Lb, Lt)
            end do
            !$OMP END PARALLEL DO
         end if
      end if
   end subroutine addbaroclinicpressure
end module m_addbaroclinicpressure
