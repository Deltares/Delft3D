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

!> prepares a matrix for solver test (as in "mpitest")
subroutine make_matrix(CFL, s1)
   use precision, only: dp
   use m_reduce
   use m_flowgeom

   implicit none

   real(kind=dp), intent(in) :: CFL !< CFL-number
   real(kind=dp), dimension(Ndx), intent(in) :: s1 !< exact solution

   real(kind=dp) :: aufu

   integer :: k1, k2, L

   bbr = 1 / CFL**2
   ccr = 0d0
   do L = 1, lnx
      aufu = 1d0
      k1 = ln(1, L)
      k2 = ln(2, L)
      bbr(k1) = bbr(k1) + aufu
      bbr(k2) = bbr(k2) + aufu
      ccr(Lv2(L)) = ccr(Lv2(L)) - aufu
   end do

!      dd = 0d0
!      do n=1,Ndx
!         dd(n) = dd(n) + bb(n)*s1(n)
!      end do

   ddr = bbr * s1
   do L = 1, Lnx
      k1 = ln(1, L)
      k2 = ln(2, L)
      ddr(k1) = ddr(k1) + ccr(Lv2(L)) * s1(k2)
      ddr(k2) = ddr(k2) + ccr(Lv2(L)) * s1(k1)
   end do

   return
end subroutine make_matrix
