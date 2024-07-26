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

! update cellmask from samples
subroutine samples_to_cellmask()

   use network_data
   use m_samples
   use m_missing, only: jins, dmiss
   use geometry_module, only: pinpok

   implicit none

   integer :: i, in, k, kk, n, nn
   double precision :: xx(6), yy(6)

   if (allocated(cellmask)) deallocate (cellmask)
   allocate (cellmask(nump1d2d)); cellmask = 0

   zs(1:ns) = 1

   do k = 1, nump
      nn = netcell(k)%N
      if (nn < 1) cycle

      do n = 1, nn
         kk = netcell(k)%nod(n)
         xx(n) = xk(kk)
         yy(n) = yk(kk)
      end do

      in = -1

      do i = 1, NS !  generate cell mask

         if (zs(i) == -1) cycle

         call pinpok(xs(i), ys(i), nn, xx, yy, in, jins, dmiss)

         if (in > 0) then
!           mask cell
            cellmask(k) = 1; zs(i) = -1
            exit
         end if

      end do
   end do

   return
end subroutine samples_to_cellmask
