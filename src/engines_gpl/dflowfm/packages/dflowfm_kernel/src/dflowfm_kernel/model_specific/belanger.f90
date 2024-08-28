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

 subroutine belanger()
    use m_physcoef

    use fm_external_forcings_data

    use m_flowgeom, only: xz, bl, dxi, ln
    use m_flow, only: s1, iadvec

    implicit none
    double precision :: chezy, cf, h0, h1, x0, x1, q, constant, bot, a, x, hav, slope, h, h3, hc, hc3, he3
    integer :: k, kb, L
    integer, parameter :: mmax = 100000, num = 200
    double precision, allocatable :: xx(:), ss(:), uu(:)

    allocate (xx(0:mmax), ss(0:mmax), uu(0:mmax))

    x0 = 0d0 ! left

    kb = kbndz(1, 1)
    x1 = xz(kb) ! right
    bot = bl(kb)

    h1 = s1(kb) - bot ! exact    right
    h0 = 20d0 ! geschat  left

    slope = abs((bl(ln(1, 3)) - bl(ln(2, 3))) * dxi(3))

    ! slope = 1d-4

    hav = 0.5 * (h0 + h1)
    call getcz(hav, frcuni, ifrctypuni, Chezy, L)
    cf = ag / Chezy**2

    q = 1500d0 / 50d0
    hc3 = q * q / ag
    hc = hc3**0.333333333d0

    constant = 0.25d0 * h1**4 - h1 * hc**3 + x1 * cf * hc**3

    call movabs(x1, h1 + bot)
    x = x1; h = h1
    xx(mmax) = x1; ss(mmax) = h1 + bot

    if (slope == 0d0) then ! analytic

       do k = 1, -num
          a = 1d0 - dble(k - 1) / dble(num - 1)
          h = h0 * (1d0 - a) + h1 * a
          x = (constant - 0.25d0 * h**4 + h * hc**3) / (cf * hc**3)
          if (x > x0) then
             call lnabs(x, h + bot)
          end if
       end do

    else
       he3 = cf * hc3 / slope
    end if

    do k = mmax - 1, 0, -1
       x = x - 1d0
       h3 = h**3
       if (slope == 0d0) then
          if (iadvec == 0) then
             h = h + (cf * hc**3) / h3 !  - hc**3)
          else
             h = h + (cf * hc**3) / (h3 - hc3)
          end if
       else
          h = h - slope * (h3 - he3) / (h3 - hc3)
       end if
       bot = bot + slope
       call lnabs(x, h + bot)
       xx(k) = x; ss(k) = h + bot
    end do

    call compareanalytic(ss, xx, mmax)

 end subroutine belanger
