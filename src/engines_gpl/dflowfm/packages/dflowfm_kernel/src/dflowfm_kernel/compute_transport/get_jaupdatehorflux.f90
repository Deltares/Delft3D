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

!> determine if the horizontal fluxes have to be updated (1) or not (0) from cell-based mask
module m_get_jaupdatehorflux

   implicit none

   private

   public :: get_jaupdatehorflux

contains

   subroutine get_jaupdatehorflux(nsubsteps, limtyp, jaupdate, jaupdatehorflux)
      use m_flowgeom, only: Ndx, Lnx, ln, klnup
      use timers

      implicit none

      integer, intent(in) :: nsubsteps !< number of substeps
      integer, intent(in) :: limtyp !< limited higher-order upwind (>0) or first-order upwind (0)
      integer, dimension(Ndx), intent(in) :: jaupdate !< cell updated (1) or not (0)
      integer, dimension(Lnx), intent(out) :: jaupdatehorflux !< update horizontal flux (1) or not (0)

      integer :: k1, k2, LL
      integer :: kk1L, kk2L
      integer :: kk1R, kk2R

      integer(4) :: ithndl = 0

      if (timon) call timstrt("get_jaupdatehorflux", ithndl)

      if (nsubsteps == 1) then
         jaupdatehorflux = 1
      else
         jaupdatehorflux = 0
         if (limtyp == 0) then
            do LL = 1, Lnx
               k1 = ln(1, LL)
               k2 = ln(2, LL)
               if (jaupdate(k1) == 1 .or. jaupdate(k2) == 1) then
                  jaupdatehorflux(LL) = 1 ! also for diffusion
               end if
            end do
         else
            do LL = 1, Lnx
               k1 = ln(1, LL)
               k2 = ln(2, LL)
               if (jaupdate(k1) == 1 .or. jaupdate(k2) == 1) then
                  jaupdatehorflux(LL) = 1 ! also for diffusion
                  cycle
               end if

               kk1L = klnup(1, LL)
               if (kk1L /= 0) then
                  if (jaupdate(abs(kk1L)) == 1) then
                     jaupdatehorflux(LL) = 1
                     cycle
                  end if

                  if (kk1L > 0) then
                     kk2L = klnup(2, LL)
                     if (jaupdate(abs(kk2L)) == 1) then
                        jaupdatehorflux(LL) = 1
                        cycle
                     end if
                  end if
               end if

               kk1R = klnup(4, LL)
               if (kk1R /= 0) then
                  if (jaupdate(abs(kk1R)) == 1) then
                     jaupdatehorflux(LL) = 1
                     cycle
                  end if

                  if (kk1R > 0) then
                     kk2R = klnup(5, LL)
                     if (jaupdate(abs(kk2R)) == 1) then
                        jaupdatehorflux(LL) = 1
                     end if
                  end if
               end if
            end do
         end if
      end if

      if (timon) call timstop(ithndl)
      return
   end subroutine get_jaupdatehorflux

end module m_get_jaupdatehorflux
