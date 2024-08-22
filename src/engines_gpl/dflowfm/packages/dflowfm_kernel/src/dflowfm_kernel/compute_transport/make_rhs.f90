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

!> compose right-hand side
subroutine make_rhs(NUMCONST, thetavert, Ndkx, kmx, vol1, kbot, ktop, sumhorflux, fluxver, source, sed, nsubsteps, jaupdate, ndeltasteps, rhs)
   use m_flowgeom, only: Ndxi, Ndx, ba ! static mesh information
   use m_flowtimes, only: dts
   use m_flowparameters, only: epshu, testdryflood
   use timers

   implicit none

   integer, intent(in) :: NUMCONST !< number of transported quantities
   double precision, dimension(NUMCONST), intent(in) :: thetavert !< vertical advection explicit (0) or implicit (1)
   integer, intent(in) :: Ndkx !< total number of flownodes (dynamically changing)
   integer, intent(in) :: kmx !< maximum number of layers
!   double precision, dimension(Ndkx),      intent(in)    :: sq       !< flux balance (inward positive)
   double precision, dimension(Ndkx), intent(in) :: vol1 !< volumes
   integer, dimension(Ndx), intent(in) :: kbot !< flow-node based layer administration
   integer, dimension(Ndx), intent(in) :: ktop !< flow-node based layer administration
   double precision, dimension(NUMCONST, Ndkx), intent(inout) :: sumhorflux !< sum of horizontal fluxes
   double precision, dimension(NUMCONST, Ndkx), intent(in) :: fluxver !< vertical fluxes
   double precision, dimension(NUMCONST, Ndkx), intent(in) :: source !< sources
   double precision, dimension(NUMCONST, Ndkx), intent(inout) :: sed !< transported quantities
   integer, intent(in) :: nsubsteps !< number of substeps
   integer, dimension(Ndx), intent(in) :: jaupdate !< update cell (1) or not (0)
   integer, dimension(Ndx), intent(in) :: ndeltasteps !< number of substeps between updates
   double precision, dimension(NUMCONST, Ndkx), intent(out) :: rhs ! right-hand side, dim(NUMCONST,Ndkx)

   double precision :: dvoli
   double precision :: dt_loc

   integer :: kk, k, kb, kt
   integer :: j

   double precision, parameter :: dtol = 1d-8

   integer(4) :: ithndl =  0
   
   if (timon) call timstrt("make_rhs", ithndl)

   dt_loc = dts
   rhs = 0d0

   if (kmx > 0) then
!     add vertical fluxes, sources, storage term and time derivative to right-hand side

      !$OMP PARALLEL DO                 &
      !$OMP PRIVATE(kk,kb,kt,k,dvoli,j) &
      !$OMP FIRSTPRIVATE(dt_loc)
      do kk = 1, Ndxi

         if (jaupdate(kk) == 0) then
            cycle
         else
            dt_loc = dts * ndeltasteps(kk)
         end if

         kb = kbot(kk)
         kt = ktop(kk)
         do k = kb, kt
            dvoli = 1d0 / max(vol1(k), dtol)
            if (testdryflood == 2) dvoli = 1d0 / max(vol1(k), epshu * ba(kk))

            do j = 1, NUMCONST

               rhs(j, k) = ((sumhorflux(j, k) / ndeltasteps(kk) - (1d0 - thetavert(j)) * (fluxver(j, k) - fluxver(j, k - 1))) * dvoli + source(j, k)) * dt_loc + sed(j, k)
               sumhorflux(j, k) = 0d0

            end do

         end do
      end do
      !$OMP END PARALLEL DO

   else
!     add time derivative
      if (nsubsteps == 1) then
         !$OMP PARALLEL DO       &
         !$OMP PRIVATE(k,j,dvoli )

         do k = 1, Ndxi
            dvoli = 1d0 / max(vol1(k), dtol)
            if (testdryflood == 2) dvoli = 1d0 / max(vol1(k), epshu * ba(k))

            do j = 1, NUMCONST
               rhs(j, k) = (sumhorflux(j, k) * dvoli + source(j, k)) * dts + sed(j, k)
               sumhorflux(j, k) = 0d0
            end do
         end do
         !$OMP END PARALLEL DO
      else

         !$OMP PARALLEL DO         &
         !$OMP PRIVATE(k,j,dvoli ) &
         !$OMP FIRSTPRIVATE(dt_loc)
         do k = 1, Ndxi
            if (jaupdate(k) == 0) then
               cycle
            else
               dt_loc = dts * ndeltasteps(k)
            end if

            dvoli = 1d0 / max(vol1(k), dtol)
            if (testdryflood == 2) dvoli = 1d0 / max(vol1(k), epshu * ba(k))

            do j = 1, NUMCONST
               rhs(j, k) = (sumhorflux(j, k) / ndeltasteps(k) * dvoli + source(j, k)) * dt_loc + sed(j, k)
               sumhorflux(j, k) = 0d0
            end do
         end do
         !$OMP END PARALLEL DO
      end if
   end if

   if (timon) call timstop(ithndl)
end subroutine make_rhs
