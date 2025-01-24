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

module m_geoutm

   implicit none

   private

   public :: geoutm

contains

   subroutine GEOUTM(xgeo, ygeo, xutm, yutm, Izone, nzone, IERR)
      use precision, only: dp
      use m_ellips

      integer :: nzone
! ----------------------------------------------------------------------
!
!     conversion of geographical (lat, lon) --> UTM coordinates (x, y, zone)
!     geographical coordinates (lat, lon) expressed in decimal degrees.
!
! ----------------------------------------------------------------------
!     arguments:
!     xgeo    i    real(kind=dp) ::    longitude (geographical coordinate)
!     ygeo    i    real(kind=dp) ::    lattitude (geographical coordinate)
!     a       i    real(kind=dp) ::    semi-major axis of ellipsoid
!     e       i    real(kind=dp) ::    excentricity of ellipsoid
!     xutm    o    real(kind=dp) ::    easting (UTM)
!     yutm    o    real(kind=dp) ::    northing (UTM)
!     zone    o    integer   zone (UTM)
!     ierr    o    integer   error code (zero for no error)
!
      real(kind=dp) :: xgeo, ygeo, xutm, yutm
      integer Izone, ierr
!
!     local variables:
!     pi           real(kind=dp) ::    3.14....
!     fn           real(kind=dp) ::    false northing
!     fe           real(kind=dp) ::    false easting
!     fi           real(kind=dp) ::    geographic lattitude (equivalent to lat)
!     dl           real(kind=dp) ::    longitude within zone
!     dl2          real(kind=dp) ::    dl*dl
!     s            real(kind=dp) ::    sin(fi)
!     ss           real(kind=dp) ::    s*s
!     sc           real(kind=dp) ::    sin(fi)*cos(fi)
!     c            real(kind=dp) ::    cos(fi)
!     cc           real(kind=dp) ::    c*c
!     cccc         real(kind=dp) ::    c*c*c*c
!     f1           real(kind=dp) ::    coefficient in function dm(fi)
!     f2           real(kind=dp) ::    coefficient in function dm(fi)
!     f3           real(kind=dp) ::    coefficient in function dm(fi)
!     f4           real(kind=dp) ::    coefficient in function dm(fi)
!     e2           real(kind=dp) ::    e*e
!     e4           real(kind=dp) ::    e2*e2
!     e6           real(kind=dp) ::    e2*e4
!     n            real(kind=dp) ::    e*e/(1-e*e)
!     nn           real(kind=dp) ::    n*n
!     x            real(kind=dp) ::    UTM easting (similar to xutm)
!     y            real(kind=dp) ::    UTM northing (similar to yutm)
!     rp           real(kind=dp) ::    function rp(fi)
!     dm           real(kind=dp) ::    function dm(fi)
!     gx           real(kind=dp) ::    function gx(fi,dl)
!     gy           real(kind=dp) ::    function gy(fi,dl)
!
      real(kind=dp) :: pi, fn, fe
      real(kind=dp) :: fi, dl, dl2, s, ss, sc, c, cc, cccc, f1, f2, f3, f4, e2, e4, e6
      real(kind=dp) :: n, nn, x, y, rp, dm, gx, gy
!
! -----------------------------------------------------------------------------
!     t.j.zitman                                  last update: 10 december 1990
! -----------------------------------------------------------------------------
!
!     initialize constants
!
      pi = 4d0 * atan(1d0)
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
!     set false northing and false easting
!
      !  IF (ygeo.LT.0.0) then
      !    ygeo = -ygeo
      !    fn   = 1.0E+07
      !    fn   = Y_offset
      !  else
      !    fn   = 0.0d0
      !  endif

      fn = 0d0
      fe = 5d+05
!
!     determine zone
!
      Nzone = int((xgeo + 180) / 6) + 1
      if (IZONE == 0) then
         IZONE = NZONE
      end if
!
!     set fi and dl
!
      fi = ygeo * pi / 180d0
      dl = (xgeo + 177d0 - 6d0 * real(Izone - 1, kind=kind(dl))) * pi / 180d0
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
!     values of sub-functions
!
      rp = a / sqrt(1d0 - e2 * ss)
      dm = a * (f1 * fi - f2 * sin(2d0 * fi) + f3 * sin(4d0 * fi) - f4 * sin(6d0 * fi))
      dl2 = dl**2
      gx = dl2 * (2d0 * cc - 1d0 + nn * cccc) / 6d0
      gy = dl2 * (6d0 * cc - 1d0 + 9d0 * nn * cccc) / 12d0
!
!     function values x and y
!
      x = rp * dl * c * (1d0 + gx)
      y = dm + rp * 0.5d0 * dl2 * sc * (1d0 + gy)
!
!     set UTM x- and y-coordinates
!
      xutm = 0.9996d0 * x + fe
      yutm = 0.9996d0 * y + fn
!
!     set no error
!
      ierr = 0
!
      continue
      return
   end subroutine geoutm

end module m_geoutm
