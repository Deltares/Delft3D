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

    subroutine corioliskelvin(tim)
       use m_netw
       use m_flowgeom
       use m_flow
       use m_sferic
       use unstruc_display
       implicit none

       integer :: k, L, k1, k2
       double precision :: tim, s1k, xx, yy, samp, dif, cs, sn, aer, dep, r0, x0, y0, Rossby, rr, sgh

       if (tim == 0d0) then
          call inisferic()
       end if

       dep = sini - zkuni
       sgh = sqrt(ag / dep)
       Rossby = sqrt(ag * dep) / fcorio
       r0 = 0.5d0 * (xkmax - xkmin)
       x0 = 0.5d0 * (xkmax + xkmin)
       y0 = 0.5d0 * (ykmax + ykmin)
       samp = 0.05d0

       call statisticsnewstep()

       do k = 1, ndx
          yy = yz(k) - y0
          xx = xz(k) - x0
          rr = sqrt(xx * xx + yy * yy)
          cs = xx / rr
          sn = yy / rr

          aer = samp * exp((rr - r0) / Rossby)
          s1k = aer * cs

          if (tim == 0) then
             s1(k) = max(bl(k), s1k); s0(k) = s1(k)
             ucx(k) = -s1k * sgh * sn
             ucy(k) = s1k * sgh * cs
          end if

          dif = abs(s1(k) - s1k)
          call statisticsonemorepoint(dif)
       end do

       if (tim == 0) then
          do L = 1, Lnx
             k1 = ln(1, L); k2 = ln(2, L)
             u1(L) = 0.5d0 * (ucx(k1) + ucx(k2)) * csu(L) + 0.5d0 * (ucy(k1) + ucy(k2)) * snu(L)
             u0(L) = u1(L)
          end do
       end if

       call statisticsfinalise()
    end subroutine corioliskelvin

    subroutine oceaneddy(tim)
       use m_netw
       use m_flowgeom
       use m_flow
       use m_sferic
       use unstruc_display
       implicit none

       integer :: k, L, k1, k2, LL, i, j, imx, jmx
       double precision :: tim, s1k, xx, yy, samp, cs, sn, dep, rs
       double precision :: x0, y0, Rossby, rr, sgh, uv, uvr, xff = 0.1d0, yff = 0.1d0

       if (tim == 0d0) then
          call inisferic()
       end if

       dep = sini - zkuni
       sgh = sqrt(ag / dep)
       Rossby = sqrt(ag * dep) / fcorio
       call dbdistancehk(xkmin, ykmin, xkmax, ykmax, rs)
       rs = oceaneddysizefrac * rs
       if (oceaneddysize /= 0d0) rs = oceaneddysize
       samp = oceaneddyamp
       if (oceaneddyvel > 0d0) then
          samp = oceaneddyvel * 2d0 * fcorio * rs / ag
       end if
       uv = ag / (2d0 * fcorio * rs * rs)

       !call statisticsnewstep()

       xff = oceaneddyxoff
       yff = oceaneddyyoff
       imx = 1; jmx = 1
       if (oceaneddyxoff /= 0d0) imx = 2
       if (oceaneddyyoff /= 0d0) jmx = 2

       do i = 1, imx
          if (i == 2) then
             xff = -xff
             samp = -samp
          end if

          do j = 1, jmx

             if (j == 2) then
                yff = -yff
                samp = -samp
             end if

             x0 = (0.5d0 + xff) * xkmax + (0.5d0 - xff) * xkmin
             y0 = (0.5d0 + yff) * ykmax + (0.5d0 - yff) * ykmin

             do k = 1, ndx
                call dbdistancehk(xz(k), yz(k), x0, y0, rr)
                call dbdistancehk(x0, yz(k), xz(k), yz(k), xx); if (xz(k) < x0) xx = -xx
                call dbdistancehk(xz(k), y0, xz(k), yz(k), yy); if (yz(k) < y0) yy = -yy

                cs = xx / rr
                sn = yy / rr

                s1k = samp * exp(-rr * rr / (2d0 * rs * rs))
                uvr = s1k * uv * rr

                if (tim == 0) then
                   s1(k) = s1(k) + max(bl(k), s1k); s0(k) = s1(k)
                   ucx(k) = ucx(k) + uvr * sn
                   ucy(k) = ucy(k) - uvr * cs
                end if

                !dif = abs(s1(k) - s1k)
                !call statisticsonemorepoint(dif)
             end do

          end do
       end do

       if (tim == 0) then
          call setkbotktop(1)
          do L = 1, Lnx
             k1 = ln(1, L); k2 = ln(2, L)
             u1(L) = 0.5d0 * (ucx(k1) + ucx(k2)) * csu(L) + 0.5d0 * (ucy(k1) + ucy(k2)) * snu(L)
             u0(L) = u1(L)
             Ltop(L) = Lbot(L) + kmx - 1
             do LL = Lbot(L), Ltop(L)
                u1(LL) = u1(L); u0(LL) = u1(L)
             end do
          end do
       end if

       !call statisticsfinalise()
    end subroutine oceaneddy

