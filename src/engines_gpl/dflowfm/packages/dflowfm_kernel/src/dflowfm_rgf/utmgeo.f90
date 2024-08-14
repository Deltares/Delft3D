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

      subroutine UTMGeo(xutm, yutm, xgeo, ygeo, IZONE, ierr)
         implicit none
!
! -----------------------------------------------------------------------------
!
!     conversion of UTM coordinates (x, y, zone) into geographical
!     coordinates (lat, lon), expressed in decimal degrees.
!
! -----------------------------------------------------------------------------
!
!     arguments:
!     xutm    i    double precision ::    easting (UTM)
!     yutm    i    double precision ::    northing (UTM)
!     Izone    i    integer   Izone (UTM)
!     a       i    double precision ::    semi-major axis of ellipsoid
!     e       i    double precision ::    excentricity of ellipsoid
!     xgeo    o    double precision ::    longitude (geographical coordinate)
!     ygeo    o    double precision ::    lattitude (geographical coordinate)
!     ierr    o    integer   error code (zero for no error)
!
         double precision :: xutm, yutm, a, e, ygeo, xgeo
         integer Izone, ierr
!
!     local variables:
!     pi           double precision ::    3.14....
!     eps          double precision ::    stopping criterion (limit in change)
!     fn           double precision ::    false northing
!     fe           double precision ::    false easting
!     cxutm        double precision ::    xutm, corrected for false eastin
!     cyutm        double precision ::    yutm, corrected for false northing
!     fi           double precision ::    geographic lattitude (equivalent to lat)
!     dl           double precision ::    longitude within zone
!     dl2          double precision ::    dl*dl
!     s            double precision ::    sin(fi)
!     ss           double precision ::    s*s
!     sc           double precision ::    sin(fi)*cos(fi)
!     c            double precision ::    cos(fi)
!     cc           double precision ::    c*c
!     cccc         double precision ::    c*c*c*c
!     f1           double precision ::    coefficient in function dm(fi)
!     f2           double precision ::    coefficient in function dm(fi)
!     f3           double precision ::    coefficient in function dm(fi)
!     f4           double precision ::    coefficient in function dm(fi)
!     e2           double precision ::    e*e
!     e4           double precision ::    e2*e2
!     e6           double precision ::    e2*e4
!     r            double precision ::    1-e2*ss
!     n            double precision ::    e*e/(1-e*e)
!     nn           double precision ::    n*n
!     x            double precision ::    UTM easting (similar to xutm)
!     dxdfi        double precision ::    partial derivative of x wrt. fi
!     dxddl        double precision ::    partial derivative of x wrt. dl
!     y            double precision ::    UTM northing (similar to yutm)
!     dydfi        double precision ::    partial derivative of y wrt. fi
!     dyddl        double precision ::    partial derivative of y wrt. dl
!     rp           double precision ::    function rp(fi)
!     drpdfi       double precision ::    derivative of rp wrt. fi
!     dm           double precision ::    function dm(fi)
!     ddmdfi       double precision ::    derivative of dm wrt. fi
!     gx           double precision ::    function gx
!     dgxdfi       double precision ::    partial derivative of gx wrt. fi
!     dgxddl       double precision ::    partial derivative of gx wrt. dl
!     gy           double precision ::    function gy
!     dgydfi       double precision ::    partial derivative of gy wrt. fi
!     dgyddl       double precision ::    partial derivative of gy wrt. dl
!     det          double precision ::    determinant
!     chanfi       double precision ::    change in fi (NR-iteration)
!     chandl       double precision ::    change in dl (NR-iteration)
!
         double precision :: pi, eps, fn, fe, cxutm, cyutm
         double precision :: fi, dl, dl2, s, ss, sc, c, cc, cccc, f1, f2, f3, f4, e2, e4, e6, r
         double precision :: n, nn, x, dxdfi, dxddl, y, dydfi, dyddl, rp, drpdfi, dm, ddmdfi
         double precision :: gx, dgxdfi, gy, dgydfi, det, chanfi, chandl
         common / ELLIPS / A, E
!
!c -----------------------------------------------------------------------------
!     t.j.zitman                                   last update: 5 december 1990
!c -----------------------------------------------------------------------------
!
!     initialize constants
!
         pi = acos(-1.d0) ! 4.0d0*atan(1.0d0)
         eps = 1.0d-05
         fe = 5.0d+05
!     fn     = 1.0E+07
         fn = 0.d0
!
         e2 = e**2
         e4 = e2**2
         e6 = e2 * e4
         n = e2 / (1d0 - e2)
         nn = n**2
         f1 = 1d0 - (1d0 / 4d0) * e2 - (3d0 / 64d0) * e4 - (5d0 / 256d0) * e6
         f2 = (3d0 / 8d0) * e2 + (3d0 / 32d0) * e4 + (45d0 / 1024d0) * e6
         f3 = (15d0 / 256d0) * e4 + (45d0 / 1024d0) * e6
         f4 = (35d0 / 3072d0) * e6
!
!     correct input for false easting and false northing
!
         cxutm = (xutm - fe) / 0.9996d0
         if (yutm >= fn) then
            cyutm = (yutm - fn) / 0.9996d0
         else
            cyutm = yutm / 0.9996d0
         end if
!
!     first estimates of dl and fi
!
         dl = xutm / a
         fi = yutm / a

!     dl     = 0.0d0
!     fi     = pi/6.0d0

!
!     Newton Raphson iteration
!
100      continue
!
!     constants, related to fi
!
         s = sin(fi)
         ss = s**2
         c = cos(fi)
         cc = c**2
         cccc = cc**2
         sc = s * c
!
!     values of sub-functions and derivatives
!
         r = 1d0 - e2 * ss
         rp = a / sqrt(r)
         drpdfi = a * e2 * sc / (r**1.5d0)
         dm = a * (f1 * fi - f2 * sin(2d0 * fi) + f3 * sin(4d0 * fi) - f4 * sin(6d0 * fi))
         ddmdfi = a * (f1 - 2d0 * f2 * cos(2d0 * fi) + 4d0 * f3 * cos(4d0 * fi) - 6d0 * f4 * cos(6d0 * fi))
         dl2 = dl**2
         gx = dl2 * (2d0 * cc - 1d0 + nn * cccc) / 6d0
         dgxdfi = -2d0 * dl2 * sc * (1d0 + nn * cc) / 3d0
         gy = dl2 * (6d0 * cc - 1d0 + 9d0 * nn * cccc) / 12d0
         dgydfi = -dl2 * sc * (1d0 - 3d0 * nn * cc)
!
!     function values x, y and derivatives
!
         x = rp * dl * c * (1d0 + gx) - cxutm
         dxdfi = dl * ((drpdfi * c - rp * s) * (1d0 + gx) + rp * c * dgxdfi)
         dxddl = rp * c * (1d0 + 3d0 * gx)
         y = dm + rp * 0.5d0 * dl2 * sc * (1d0 + gy) - cyutm
         dydfi = ddmdfi + 0.5d0 * dl2 * (sc * (drpdfi * (1d0 + gy) + rp * dgydfi) + rp * (cc - ss) * (1d0 + gy))
         dyddl = rp * dl * sc * (1d0 + 2d0 * gy)
!
!     changes in the estimates dl and fi
!
         det = dxddl * dydfi - dxdfi * dyddl
         if (det == 0d0) then
            ierr = 1
            goto 900
         end if
         chanfi = -(-x * dyddl + y * dxddl) / det
         chandl = -(x * dydfi - y * dxdfi) / det
!
!     check stopping criterion
!
         if (abs(chanfi) > abs(eps * fi) .and. abs(chandl) > abs(eps * dl)) then
            fi = fi + chanfi
            dl = dl + chandl
            goto 100
         end if
!
!     set final values
!
         ygeo = fi * 180d0 / pi
         xgeo = dl * 180d0 / pi + 6d0 * real(Izone - 1, kind=kind(xgeo)) - 177d0
         ierr = 0
!
900      continue
         return
      end subroutine UTMGeo
