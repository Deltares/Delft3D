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

 subroutine wetbed(time)

    use m_flowparameters, only: hwetbed
    implicit none

    integer, parameter :: mmax = 601 !  3000
    double precision :: s(0:mmax), u(0:mmax), x(0:mmax)

    double precision :: time, dxw, xc
    double precision :: g, t, dt, xd, x0, xu, h1, h0, eps, c1, c0, u20, z0, c20, &
       aa, ab, ac, ad, ba, bb, bc, bd, ca, cb, cc, cd, dd, d1, d2, d3, dz, &
       dc2, du2, z, c2, h2, u2, xm, c
    integer :: itmax, mc, i, iter, m

!c
!c initialise
!c
    g = 9.81
    t = time
    dt = 1.0
    xd = 0.0
    x0 = 0.0
    xu = 0.0
    dxw = 100.0
    h1 = 2.0
    h0 = 0.0000000001
    h0 = hwetbed
    itmax = 3600
    itmax = 1
    eps = 1.0e-6
    mc = 301
    xc = (mc - 1) * dxw
    mc = mmax
    do i = 0, mc ! mmax
       x(i) = (i - 0.5) * dxw - xc
    end do
    do i = 0, mc ! mmax
       if (x(i) < 0.0) then
          s(i) = h1
       else
          s(i) = h0
       end if
       u(i) = 0.0
    end do
!c
!c initialise
!c
    c1 = sqrt(g * h1)
    c0 = sqrt(g * h0)
    u20 = c0
    z0 = c0
    c20 = c0
    iter = 0
10  continue
    iter = iter + 1
!c
!c Newton iteration for correct coefficients of exact solution
!c
    aa = 2 * z0 - u20
    ab = -c20
    ac = -z0
    ad = 0.5 * c0**2 + u20 * z0 - z0**2 + 0.5 * (c20)**2
    ba = -c20**2 + c0**2
    bb = 2 * c20 * u20 - 2 * c20 * z0
    bc = c20**2
    bd = -c20**2 * u20 + c20**2 * z0 - c0**2 * z0
    ca = 0.0
    cb = 2.0
    cc = 1.0
    cd = 2 * c1 - u20 - 2 * c20
    dd = aa * (bb * cc - bc * cb) - ab * (ba * cc - bc * ca) + ac * (ba * cb - bb * ca)
    d1 = ad * (bb * cc - bc * cb) - ab * (bd * cc - bc * cd) + ac * (bd * cb - bb * cd)
    d2 = aa * (bd * cc - bc * cd) - ad * (ba * cc - bc * ca) + ac * (ba * cd - bd * ca)
    d3 = aa * (bb * cd - bd * cb) - ab * (ba * cd - bd * ca) + ad * (ba * cb - bb * ca)
    dz = d1 / dd
    dc2 = d2 / dd
    du2 = d3 / dd
    z0 = z0 + dz
    c20 = c20 + dc2
    u20 = u20 + du2
    if (abs(dz) > eps) goto 10
    if (abs(dc2) > eps) goto 10
    if (abs(du2) > eps) goto 10
!c
!c correct shock speeds (z, c2 and u2) are found
!c
    z = z0
    c2 = c20
    h2 = c2**2 / g
    u2 = u20
    ! WRITE(*,*) 'H2, Z, U2', H2, Z, U2

!       do itime=1,itmax
!         t=t+dt
!c
!c determination of various zones with different solutions
!c
    xd = -c1 * t
    xm = (u2 - c2) * t
    xu = z * t
    do i = 0, mc ! mmax
       if (x(i) < xd) then
          u(i) = 0.0
          s(i) = h1
       else if (x(i) >= xd .and. x(i) < xm) then
          c = (2 * c1 - x(i) / t) / 3.0
          s(i) = c**2 / g
          u(i) = 2.0 / 3.0 * (c1 + x(i) / t)
       else if (x(i) >= xm .and. x(i) < xu) then
          u(i) = 2.0 / 3.0 * (c1 + xm / t)
          s(i) = h2
       else if (x(i) >= xu) then
          u(i) = 0.0
          s(i) = h0
       end if
    end do
!       enddo

!        open(unit=33,file='wetbed.prn')
    do m = 2, mc ! mmax
       if (m == 2) then
          call movabs(x(m) + xc, s(m))
       else
          call lnabs(x(m) + xc, s(m))
       end if
    end do

    do m = 2, 0 ! mmax-1
       if (m == 2) then
          call movabs(x(m) + xc, 0.1d0 * U(m))
       else
          call lnabs(x(m) + xc, 0.1d0 * U(m))
       end if
    end do

!        write (33,'(3e15.4)') x(m)+xc, s(m), u(m)

    ! call htext(dble(h2), dble(xu+xc), dble(h2) )

    x = x + xc
    call compareanalytic(s, x, mmax)

 end subroutine wetbed
