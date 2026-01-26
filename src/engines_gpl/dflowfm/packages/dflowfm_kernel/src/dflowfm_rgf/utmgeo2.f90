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

!> converts UTM coords to lat/long.  Equations from USGS Bulletin 1532
!! East Longitudes are positive, West longitudes are negative.
!! North latitudes are positive, South latitudes are negative
!! Lat and Long are in decimal degrees.
!! Written by Chuck Gantz- chuck.gantz@globalstar.com
!! BY: Chuck Gantz, http://www.gpsy.com/gpsinfo/geotoutm/gantz/LatLong-UTMconversion.cpp
module m_utmgeo2

   implicit none

   private

   public :: utmgeo2

contains

   subroutine utmgeo2(xutm, yutm, xgeo, ygeo, IZONE, ihem)
      use precision, only: dp
      use m_sferic, only: rd2dg
      use m_ellipse, only: semi_major_axis, eccentricity

!     xutm    i    real(kind=dp) ::    easting (UTM)
!     yutm    i    real(kind=dp) ::    northing (UTM)
!     Izone   i    integer   Izone (UTM)
!     Ihem    i    integer hemisphere (0=north, 1 = south)
!     a       i    real(kind=dp) ::    semi-major axis of ellipsoid
!     e       i    real(kind=dp) ::    excentricity of ellipsoid
!     xgeo    o    real(kind=dp) ::    longitude (geographical coordinate)
!     ygeo    o    real(kind=dp) ::    lattitude (geographical coordinate)
!
      real(kind=dp) :: xutm, yutm, ygeo, xgeo
      integer :: izone, ihem

      real(kind=dp), parameter :: K0 = 0.9996_dp
      real(kind=dp) :: eccSquared
      real(kind=dp) :: eccPrimeSquared
      real(kind=dp) :: e1
      real(kind=dp) :: N1, T1, C1, R1, D, M
      real(kind=dp) :: LongOrigin
      real(kind=dp) :: mu, phi1, phi1Rad
      real(kind=dp) :: x, y
      integer :: ZoneNumber
      integer :: NorthernHemisphere !1 for northern hemispher, 0 for southern

      eccSquared = eccentricity * eccentricity
      e1 = (1 - sqrt(1 - eccSquared)) / (1 + sqrt(1 - eccSquared))
      ZoneNumber = izone
      NorthernHemisphere = ihem
      x = xutm - 500000.0_dp !remove 500,000 meter offset for longitude
      y = yutm

      if (ihem == 0) then
         y = y - 10000000.0_dp !remove 10,000,000 meter offset used for southern hemisphere
      end if

      LongOrigin = (ZoneNumber - 1) * 6 - 180 + 3 !  //+3 puts origin in middle of zone

      eccPrimeSquared = (eccSquared) / (1 - eccSquared)

      M = y / K0
      mu = M / (semi_major_axis * (1 - eccSquared / 4 - 3 * eccSquared * eccSquared / 64 - 5 * eccSquared * eccSquared * eccSquared / 256))

      phi1Rad = mu + (3 * e1 / 2 - 27 * e1 * e1 * e1 / 32) * sin(2 * mu) &
                + (21 * e1 * e1 / 16 - 55 * e1 * e1 * e1 * e1 / 32) * sin(4 * mu) &
                + (151 * e1 * e1 * e1 / 96) * sin(6 * mu)
      phi1 = phi1Rad * rd2dg

      N1 = semi_major_axis / sqrt(1 - eccSquared * sin(phi1Rad) * sin(phi1Rad))
      T1 = tan(phi1Rad) * tan(phi1Rad)
      C1 = eccPrimeSquared * cos(phi1Rad) * cos(phi1Rad)
      R1 = semi_major_axis * (1 - eccSquared) / (1 - eccSquared * sin(phi1Rad) * sin(phi1Rad))**1.5_dp
      D = x / (N1 * K0)

      ygeo = phi1Rad - (N1 * tan(phi1Rad) / R1) * (D * D / 2 - (5 + 3 * T1 + 10 * C1 - 4 * C1 * C1 - 9 * eccPrimeSquared) * D * D * D * D / 24 &
                                                   + (61 + 90 * T1 + 298 * C1 + 45 * T1 * T1 - 252 * eccPrimeSquared - 3 * C1 * C1) * D * D * D * D * D * D / 720)
      ygeo = ygeo * rd2dg

      xgeo = (D - (1 + 2 * T1 + C1) * D * D * D / 6 + (5 - 2 * C1 + 28 * T1 - 3 * C1 * C1 + 8 * eccPrimeSquared + 24 * T1 * T1) &
              * D * D * D * D * D / 120) / cos(phi1Rad)
      xgeo = LongOrigin + xgeo * rd2dg
   end subroutine utmgeo2
end module m_utmgeo2
