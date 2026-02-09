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

module m_geoutm
   implicit none
   private
   public :: geoutm

contains

   !> conversion of geographical (lat, lon) --> UTM coordinates (x, y, zone)
   !! geographical coordinates (lat, lon) expressed in decimal degrees.
   subroutine geoutm(xgeo, ygeo, xutm, yutm, izone, nzone, ierr)
      use precision, only: dp
      use m_ellipse, only: semi_major_axis, eccentricity

      real(kind=dp), intent(in) :: xgeo !< longitude (geographical coordinate)
      real(kind=dp), intent(in) :: ygeo !< lattitude (geographical coordinate)
      real(kind=dp), intent(out) :: xutm !< easting (UTM)
      real(kind=dp), intent(out) :: yutm !< northing (UTM)
      integer, intent(out) :: izone !< zone (UTM)
      integer, intent(inout) :: nzone
      integer, intent(out) :: ierr !< error code (zero for no error)

      real(kind=dp), parameter :: pi = 4.0_dp * atan(1.0_dp)
      real(kind=dp) :: fn !< false northing
      real(kind=dp) :: fe !< false easting
      real(kind=dp) :: fi !< geographic latitude (equivalent to lat)
      real(kind=dp) :: dl !< longitude within zone
      real(kind=dp) :: dl2 !< dl*dl
      real(kind=dp) :: s !< sin(fi)
      real(kind=dp) :: ss !< s*s
      real(kind=dp) :: sc !< sin(fi)*cos(fi)
      real(kind=dp) :: c !< cos(fi)
      real(kind=dp) :: cc !< c*c
      real(kind=dp) :: cccc !< c*c*c*c
      real(kind=dp) :: f1 !< coefficient in function dm(fi)
      real(kind=dp) :: f2 !< coefficient in function dm(fi)
      real(kind=dp) :: f3 !< coefficient in function dm(fi)
      real(kind=dp) :: f4 !< coefficient in function dm(fi)
      real(kind=dp) :: e2 !< eccentricity squared
      real(kind=dp) :: e4 !< e2 squared
      real(kind=dp) :: e6 !< e2 * e4
      real(kind=dp) :: n !< eccentricity squared / (1 - eccentricity squared)
      real(kind=dp) :: nn !< n squared
      real(kind=dp) :: x !< UTM easting (similar to xutm)
      real(kind=dp) :: y !< UTM northing (similar to yutm)
      real(kind=dp) :: rp !< function rp(fi)
      real(kind=dp) :: dm !< function dm(fi)
      real(kind=dp) :: gx !< function gx(fi, dl)
      real(kind=dp) :: gy !< function gy(fi, dl)

      e2 = eccentricity**2
      e4 = e2**2
      e6 = e2 * e4
      n = e2 / (1.0_dp - e2)
      nn = n**2
      f1 = 1.0_dp - (1.0_dp / 4.0_dp) * e2 - (3.0_dp / 64.0_dp) * e4 - (5.0_dp / 256.0_dp) * e6
      f2 = (3.0_dp / 8.0_dp) * e2 + (3.0_dp / 32.0_dp) * e4 + (45.0_dp / 1024.0_dp) * e6
      f3 = (15.0_dp / 256.0_dp) * e4 + (45.0_dp / 1024.0_dp) * e6
      f4 = (35.0_dp / 3072.0_dp) * e6

      fn = 0.0_dp
      fe = 5e5_dp

      ! determine zone
      nzone = int((xgeo + 180) / 6) + 1
      if (izone == 0) then
         izone = nzone
      end if

      fi = ygeo * pi / 180.0_dp
      dl = (xgeo + 177.0_dp - 6.0_dp * real(izone - 1, kind=kind(dl))) * pi / 180.0_dp

      s = sin(fi)
      ss = s**2
      c = cos(fi)
      cc = c**2
      cccc = cc**2
      sc = s * c

      rp = semi_major_axis / sqrt(1.0_dp - e2 * ss)
      dm = semi_major_axis * (f1 * fi - f2 * sin(2.0_dp * fi) + f3 * sin(4.0_dp * fi) - f4 * sin(6.0_dp * fi))
      dl2 = dl**2
      gx = dl2 * (2.0_dp * cc - 1.0_dp + nn * cccc) / 6.0_dp
      gy = dl2 * (6.0_dp * cc - 1.0_dp + 9.0_dp * nn * cccc) / 12.0_dp

      x = rp * dl * c * (1.0_dp + gx)
      y = dm + rp * 0.5_dp * dl2 * sc * (1.0_dp + gy)

      xutm = 0.9996_dp * x + fe
      yutm = 0.9996_dp * y + fn

      ierr = 0
   end subroutine geoutm
end module m_geoutm
