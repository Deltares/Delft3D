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

 subroutine drybed(time)
    implicit none
    double precision :: time, xm, xmx, h0, dxw
    integer, parameter :: mmax = 601 !  3000
    double precision :: s(0:mmax), u(0:mmax), xx(0:mmax)
    double precision :: g, t, cw, xl, xr, sa, ua, x
    integer :: m

    g = 9.81 ! 10.0
    t = time
    h0 = 2
    cw = sqrt(9.81 * h0)
    dxw = 100.
    xl = -cw * t
    xr = t * 2 * cw

    xmx = -9999
    do m = 2, 600 ! ndx
       xm = m * dxw - 50 ! xz(m)
       x = xm - 300 * dxw
       if (x > xl .and. x < xr) then
          sa = (((2 * cw - x / t) / 3.0)**2) / g
          ua = 2 * (cw + x / t) / 3.0
          xmx = max(xmx, xm)
       else if (x < xl) then
          sa = h0
          ua = 0.0
       else
          sa = 0.0
          ua = 0.0
       end if
       s(m) = sa; u(m) = ua; xx(m) = xm
       if (m == 2) then
          call movabs(xm, sa)
       else
          call lnabs(xm, sa)
       end if
    end do
    call movabs(xmx, 0.1d0 * h0)
    call lnabs(xmx, 0.2d0 * h0)

    call compareanalytic(s, u, xx, mmax)
 end subroutine drybed
