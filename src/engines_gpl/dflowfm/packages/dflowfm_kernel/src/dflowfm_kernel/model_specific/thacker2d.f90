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

module m_thacker2d

   implicit none

   private

   public :: thacker2d

contains

   subroutine thacker2d(t, ini)
      use precision, only: dp
      use m_netw, only: xk, yk, zk, numk
      use m_flowgeom
      use m_flow
      use m_sferic
      use m_dminmax
      use m_set_bobs

      real(kind=dp) :: t, rms
      integer :: ini, k, L, k1, k2
      real(kind=dp) :: xzmin, xzmax, yzmin, yzmax, s1k, x0, y0, r0, xx, yy, r, omeg, st, ct
      real(kind=dp) :: h0, zz0, a, a1c, a12, sa12, rr0, ur, ut, cs, sn

      call DMINMAX(xz, ndx, xzmin, xzmax, ndx)
      call DMINMAX(yz, ndx, yzmin, yzmax, ndx)

      r0 = 0.5_dp * (xzmax - xzmin) * 0.85
      x0 = 0.5_dp * (xzmax + xzmin)
      y0 = 0.5_dp * (yzmax + yzmin)
      h0 = 10.0_dp
      zz0 = 2.0_dp

      omeg = twopi / (12 * 3600) ! period = 12 hrs
      omeg = sqrt(8 * ag * h0 / (r0 * r0))

      fcorio = 0.0_dp ! omeg/2

      a = ((h0 + zz0)**2 - h0 * h0) / ((h0 + zz0)**2 + h0 * h0)

      r0 = sqrt(8.0_dp * ag * h0 / (omeg * omeg - fcorio * fcorio)) ! Casulli 2008 (31) mind you, no - sign in front of fcorio

      st = sin(omeg * t)
      ct = cos(omeg * t)

      if (ibedlevtyp == 3) then
         do k = 1, numk
            xx = xk(k) - x0
            yy = yk(k) - y0
            r = sqrt(xx * xx + yy * yy)
            rr0 = (r * r) / (r0 * r0)
            zk(k) = -h0 * (1.0_dp - rr0)
         end do
         call setbobs()
      end if

      rms = 0.0_dp
      do k = 1, ndx
         xx = xz(k) - x0
         yy = yz(k) - y0
         r = sqrt(xx * xx + yy * yy)
         rr0 = (r * r) / (r0 * r0)
         if (ibedlevtyp /= 3) then
            bl(k) = -h0 * (1.0_dp - rr0)
         end if

         a1c = 1.0_dp - a * ct
         a12 = 1.0_dp - a * a
         sa12 = sqrt(a12)

         s1k = h0 * (sa12 / a1c - 1.0_dp - rr0 * (a12 / (a1c * a1c) - 1.0_dp))
         s1k = max(bl(k), s1k)
         if (ini == 1) then
            s1(k) = s1k
            ur = omeg * r * a * st / (2.0_dp * a1c)
            ut = (fcorio * r / (2.0_dp * a1c)) * (sa12 + a * ct - 1.0_dp)
            cs = xx / r
            sn = yy / r
            ucx(k) = ur * cs - ut * sn
            ucy(k) = ur * sn + ut * cs
         else
            rms = rms + abs(s1k - s1(k)) ! **2
         end if

      end do
      ! rms = sqrt(rms)/ndx
      rms = rms / ndx

      if (ini == 1) then
         do L = 1, lnx
            k1 = ln(1, L)
            k2 = ln(2, L)
            u1(L) = (acl(L) * ucx(k1) + (1.0_dp - acl(L)) * ucx(k2)) * csu(L) &
                    + (acl(L) * ucy(k1) + (1.0_dp - acl(L)) * ucy(k2)) * snu(L)
         end do

         call setbobs()
      end if

   end subroutine thacker2d

end module m_thacker2d
