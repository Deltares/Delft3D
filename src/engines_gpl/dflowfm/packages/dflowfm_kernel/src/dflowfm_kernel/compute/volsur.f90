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
module m_volsur
   use m_vol12d, only: vol12d

   implicit none
contains
   subroutine volsur() ! volsur entirely in s1 because of s1 iteration
      use precision, only: dp
      use timers, only: timstrt, timstop
      use m_flowgeom, only: ndx2d, bl, ba, ndxi, lnxi, lnx, ln
      use m_flow, only: nonlin2d, s1, vol1, a1, nonlin, a1m

      ! locals
      integer :: japerim
      integer :: L, n, k1, k2
      real(kind=dp) :: hh
      integer, save :: handle = 0

      call timstrt('Volume calculation', handle)
      japerim = 0

! call sets01zbnd(1) ! set s1 on z-boundaries   SPvdP: not necessary, values at the boundaries were already properly filled in solve_matrix, as the boundary nodes are included in the solution vector

      if (nonlin2D == 0) then

         !$OMP PARALLEL DO                              &
         !$OMP PRIVATE(n,hh)
         do n = 1, ndx2d
            hh = max(0.0_dp, s1(n) - bl(n))
            vol1(n) = ba(n) * hh
            a1(n) = ba(n)
         end do
         !$OMP END PARALLEL DO    !   TODO OMP|

      else

         vol1(1:ndx2d) = 0.0_dp
         a1(1:ndx2d) = 0.0_dp

      end if

      if (nonlin == 0) then

         do n = ndx2d + 1, ndxi
            hh = max(0.0_dp, s1(n) - bl(n))
            vol1(n) = ba(n) * hh
            a1(n) = ba(n)
         end do

      else
         vol1(ndx2D + 1:ndxi) = 0.0_dp
         a1(ndx2D + 1:ndxi) = 0.0_dp
      end if

      if (nonlin >= 2) then
         a1m = 0.0_dp
      end if

      call VOL12D(japerim) ! and add area's and volumes of 1D links

      do L = lnxi + 1, Lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         a1(k1) = ba(k2) ! set bnd a1 to ba of inside point
         vol1(k1) = vol1(k2) ! a1(k1)*(s1(k1) - bl(k1))
      end do

      call timstop(handle)

   end subroutine volsur
end module m_volsur
