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

!> converts UTM coords to lat/long.  Equations from USGS Bulletin 1532
!! East Longitudes are positive, West longitudes are negative.
!! North latitudes are positive, South latitudes are negative
!! Lat and Long are in decimal degrees.
!! Written by Chuck Gantz- chuck.gantz@globalstar.com
!! BY: Chuck Gantz, http://www.gpsy.com/gpsinfo/geotoutm/gantz/LatLong-UTMconversion.cpp
    subroutine utmgeo2(xutm, yutm, xgeo, ygeo, IZONE, ihem, ierr)
       use m_sferic
       implicit none
!     xutm    i    double precision ::    easting (UTM)
!     yutm    i    double precision ::    northing (UTM)
!     Izone   i    integer   Izone (UTM)
!     Ihem    i    integer hemisphere (0=north, 1 = south)
!     a       i    double precision ::    semi-major axis of ellipsoid
!     e       i    double precision ::    excentricity of ellipsoid
!     xgeo    o    double precision ::    longitude (geographical coordinate)
!     ygeo    o    double precision ::    lattitude (geographical coordinate)
!     ierr    o    integer   error code (zero for no error)
!
       double precision :: xutm, yutm, a, e, ygeo, xgeo
       integer Izone, ihem, ierr
       common / ELLIPS / A, E

       double precision :: k0 = 0.9996
       double precision :: eccSquared
       double precision :: eccPrimeSquared; 
       double precision :: e1
       double precision :: N1, T1, C1, R1, D, M
       double precision :: LongOrigin
       double precision :: mu, phi1, phi1Rad
       double precision :: x, y
       integer :: ZoneNumber
       integer :: NorthernHemisphere !1 for northern hemispher, 0 for southern

       eccSquared = e * e
       e1 = (1 - sqrt(1 - eccSquared)) / (1 + sqrt(1 - eccSquared))
       ZoneNumber = izone
       NorthernHemisphere = ihem
       x = xutm - 500000.0 !remove 500,000 meter offset for longitude
       y = yutm

       if (ihem == 0) then
          y = y - 10000000.0 !remove 10,000,000 meter offset used for southern hemisphere
       end if

       LongOrigin = (ZoneNumber - 1) * 6 - 180 + 3 !  //+3 puts origin in middle of zone

       eccPrimeSquared = (eccSquared) / (1 - eccSquared)

       M = y / k0
       mu = M / (a * (1 - eccSquared / 4 - 3 * eccSquared * eccSquared / 64 - 5 * eccSquared * eccSquared * eccSquared / 256))

       phi1Rad = mu + (3 * e1 / 2 - 27 * e1 * e1 * e1 / 32) * sin(2 * mu) &
                 + (21 * e1 * e1 / 16 - 55 * e1 * e1 * e1 * e1 / 32) * sin(4 * mu) &
                 + (151 * e1 * e1 * e1 / 96) * sin(6 * mu)
       phi1 = phi1Rad * rd2dg

       N1 = a / sqrt(1 - eccSquared * sin(phi1Rad) * sin(phi1Rad))
       T1 = tan(phi1Rad) * tan(phi1Rad)
       C1 = eccPrimeSquared * cos(phi1Rad) * cos(phi1Rad)
       R1 = a * (1 - eccSquared) / (1 - eccSquared * sin(phi1Rad) * sin(phi1Rad))**1.5d0
       D = x / (N1 * k0)

       ygeo = phi1Rad - (N1 * tan(phi1Rad) / R1) * (D * D / 2 - (5 + 3 * T1 + 10 * C1 - 4 * C1 * C1 - 9 * eccPrimeSquared) * D * D * D * D / 24 &
                                                    + (61 + 90 * T1 + 298 * C1 + 45 * T1 * T1 - 252 * eccPrimeSquared - 3 * C1 * C1) * D * D * D * D * D * D / 720)
       ygeo = ygeo * rd2dg

       xgeo = (D - (1 + 2 * T1 + C1) * D * D * D / 6 + (5 - 2 * C1 + 28 * T1 - 3 * C1 * C1 + 8 * eccPrimeSquared + 24 * T1 * T1) &
               * D * D * D * D * D / 120) / cos(phi1Rad)
       xgeo = LongOrigin + xgeo * rd2dg

    end subroutine utmgeo2
