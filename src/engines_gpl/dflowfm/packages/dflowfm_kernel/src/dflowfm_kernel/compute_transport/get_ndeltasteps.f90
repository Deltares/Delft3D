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

!> get number of subtimesteps and delta subtimesteps
subroutine get_ndeltasteps()
   use m_flowgeom, only: Ndxi, Lnxi, Lnx, ln
   use m_flowtimes, only: dts
   use m_transport
   use timers

   implicit none

   double precision :: dt, dtmin
   double precision :: logtwo

   integer :: kk, LL

   double precision, external :: get_dt

   integer(4) :: ithndl =  0
   
   if (timon) call timstrt("get_ndeltasteps", ithndl)

   numnonglobal = 0

!  get smallest and largest time steps
   dtmin = dtmin_transp

   if (dtmin >= dts) then
      nsubsteps = 1
      ndeltasteps = 1
   else
      logtwo = log(2d0)
      nsubsteps = max(1, 2**int(log(dts / dtmin) / logtwo + 0.9999d0))
      dtmin = dts / nsubsteps

!     get number of substeps
      do kk = 1, Ndxi
         dt = dtmax(kk)
         if (dt < dts) then
            ndeltasteps(kk) = min(2**int(log(dt / dtmin) / logtwo), nsubsteps)
            numnonglobal = numnonglobal + 1
         else
            ndeltasteps(kk) = nsubsteps
         end if
      end do

!     fictitious boundary cells
      do LL = Lnxi + 1, Lnx
         ndeltasteps(ln(1, LL)) = ndeltasteps(ln(2, LL))
      end do

!      if ( nsubsteps.gt.1 ) then
!         write(6,*) dtmin
!      end if

   end if

   if (timon) call timstop(ithndl)
   return
end subroutine get_ndeltasteps
