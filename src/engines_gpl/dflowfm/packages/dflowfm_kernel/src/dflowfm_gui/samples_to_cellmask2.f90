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
module m_samples_to_cellmask2

   implicit none

contains

   subroutine samples_to_cellmask2()

      use network_data
      use m_samples
      use m_missing, only: dmiss
      use geometry_module ! , only: pinpok

      implicit none

      integer :: i, in, k, kk, n, nn, num

      if (allocated(cellmask)) then
         deallocate (cellmask)
      end if
      allocate (cellmask(nump1d2d)); cellmask = 0

      zs(1:ns) = 1

      call increasepol(6 * nump, 0)
      npl = 0

      do k = 1, nump
         nn = netcell(k)%N
         if (nn < 1) cycle

         do n = 1, nn
            kk = netcell(k)%nod(n)
            npl = npl + 1
            xpl(npl) = xk(kk)
            ypl(npl) = yk(kk)
            zpl(npl) = 1d0
         end do
         npl = npl + 1; xpl(npl) = dmiss; ypl(npl) = dmiss; zpl(npl) = dmiss

      end do

      in = -1

      do i = 1, NS !  generate cell mask

         !call dbpinpol(xs(i), ys(i), in, dmiss, 1, NPL, xpl, ypl, zpl) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN

         call dbpinpol_optinside_perpol2(xs(i), ys(i), 0, 0, in, num, dmiss, 1, NPL, xpl, ypl, zpl)
         ! call pinpok(xs(i), ys(i), nn, xx, yy, in, jins, dmiss)

         if (ipolyfound > 0) then
            cellmask(ipolyfound) = 1
         end if

      end do

      return
   end subroutine samples_to_cellmask2

end module m_samples_to_cellmask2
