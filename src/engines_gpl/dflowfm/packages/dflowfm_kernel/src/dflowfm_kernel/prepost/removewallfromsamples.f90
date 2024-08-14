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

      subroutine removewallfromsamples()
         use m_samples
         use m_polygon

         implicit none
         integer :: k, k3, kk, mout

         call newfil(mout, 'wall.xyz')

         call savesam()

         kk = 0
         do k = 1, ns
!         call findnearwallpoint(k,k2)
            call findneargroundpoint(k, k3)
            if (k3 /= 0) then ! .and. k2 .ne. 0) then
               npl = npl + 1
               xpl(npl) = xs(k)
               ypl(npl) = ys(k)
               write (mout, *) xs(k), ys(k), zs(k)
            else
               kk = kk + 1
               xs2(kk) = xs(k)
               ys2(kk) = ys(k)
               zs2(kk) = zs(k)
            end if
         end do

         close (mout)

         ns2 = kk
         call restoresam()

      end subroutine removewallfromsamples
