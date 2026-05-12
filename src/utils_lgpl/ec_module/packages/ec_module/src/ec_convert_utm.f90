!!  Copyright (C)  Stichting Deltares, 2012-2026.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
!!
!> Convert UTM coordinates to geographic longitude/latitude.
!!
!! This module provides utilities to:
!! - validate UTM zone strings in the form <zone><hemisphere>, e.g. 53N
!! - convert UTM easting/northing to longitude/latitude in degrees
!!
!! The conversion uses WGS84 ellipsoid constants.
module m_ec_utm_inverse

   use precision
   use m_ec_message, only: set_ec_message

   implicit none
   private
   public :: utm2deg
   public :: is_valid_utm_zone

contains

   !> Convert UTM coordinates to longitude/latitude (degrees).
   !!
   !! On invalid UTM zone input, lon/lat are set to 0 and an error message
   !! is stored via set_ec_message. If present, success is set to .false.
   !!
   !! Algorithm outline:
   !! - parse the UTM zone into numeric zone and hemisphere
   !! - remove the standard UTM false easting and, for the southern
   !!   hemisphere, remove the false northing
   !! - compute WGS84 eccentricity-related coefficients
   !! - convert meridional arc length to the footprint latitude using the
   !!   standard series expansion in the auxiliary latitude mu
   !! - compute local curvature terms and the normalized easting d
   !! - evaluate truncated inverse Transverse Mercator series for latitude
   !!   and longitude around the central meridian of the zone
   !! - convert the final angles from radians to decimal degrees
   !!
   !! The implementation follows the standard inverse UTM / inverse
   !! Transverse Mercator formulation and keeps terms through d^6 for
   !! latitude and d^5 for longitude.
   !!
   !! Algorithm based on the inverse Transverse Mercator / UTM series conventionally published 
   !! as the USGS/Snyder formulas. UTM zone definitions, false easting, false northing, and 6° zoning 
   !! follow USGS Fact Sheet 077-01 and EPSG/IOGP Guidance Note 7-2. The inverse TM equations, 
   !! including meridional arc, footprint latitude from auxiliary latitude mu, curvature terms, 
   !! normalized easting D, and truncated latitude/longitude series, follow IOGP Guidance Note 7-2, 
   !! 3.2.3 Transverse Mercator, “USGS formulas,” consistent with Snyder, J.P. (1987), Map Projections: 
   !! A Working Manual, USGS Professional Paper 1395. WGS84 ellipsoid constants follow EPSG:7030 / NGA WGS84.
   !!
   !! @param[in]  xx       UTM easting in meters.
   !! @param[in]  yy       UTM northing in meters.
   !! @param[in]  utmzone  UTM zone string: 1..60 followed by N or S.
   !! @param[out] lon      Longitude in decimal degrees.
   !! @param[out] lat      Latitude in decimal degrees.
   !! @param[out] success  Optional success flag.
   subroutine utm2deg(xx, yy, utmzone, lon, lat, success)

      use mathconsts

      real(kind=dp), intent(in) :: xx, yy
      character(len=*), intent(in) :: utmzone
      real(kind=dp), intent(out) :: lon, lat
      logical, optional, intent(out) :: success

      real(kind=dp), parameter :: sa = 6378137.0_dp
      real(kind=dp), parameter :: sb = 6356752.314245_dp
      real(kind=dp), parameter :: k0 = 0.9996_dp
      real(kind=dp), parameter :: false_easting = 500000.0_dp
      real(kind=dp), parameter :: false_northing_south = 10000000.0_dp

      integer :: huso
      character(len=1) :: hemi
      logical :: valid_utm_zone
      real(kind=dp) :: x
      real(kind=dp) :: y
      real(kind=dp) :: lon0
      real(kind=dp) :: e2
      real(kind=dp) :: e4
      real(kind=dp) :: e6
      real(kind=dp) :: ep2
      real(kind=dp) :: e1
      real(kind=dp) :: m
      real(kind=dp) :: mu
      real(kind=dp) :: phi1
      real(kind=dp) :: c1
      real(kind=dp) :: t1
      real(kind=dp) :: n1
      real(kind=dp) :: r1
      real(kind=dp) :: d
      real(kind=dp) :: j1, j2, j3, j4

      call parse_utm_zone(utmzone, huso, hemi, valid_utm_zone)
      if (.not. valid_utm_zone) then
         lon = 0.0_dp
         lat = 0.0_dp
         call set_ec_message("ERROR: ec_convert_utm::utm2deg: Invalid UTM zone '"// &
                             trim(adjustl(utmzone))//"'. Expected UTM zone 1..60 followed by hemisphere N or S, "// &
                             "for example 53N.")
         if (present(success)) success = .false.
         return
      end if
      if (present(success)) success = .true.

      x = xx - false_easting
      y = yy
      if (hemi == 'S') y = y - false_northing_south

      e2 = (sa * sa - sb * sb) / (sa * sa)
      e4 = e2 * e2
      e6 = e4 * e2
      ep2 = e2 / (1.0_dp - e2)
      e1 = (1.0_dp - sqrt(1.0_dp - e2)) / (1.0_dp + sqrt(1.0_dp - e2))

      m = y / k0
      mu = m / (sa * (1.0_dp - e2 / 4.0_dp - 3.0_dp * e4 / 64.0_dp - 5.0_dp * e6 / 256.0_dp))

      j1 = 3.0 * e1 / 2.0_dp - 27.0_dp * e1**3 / 32.0_dp
      j2 = 21.0_dp * e1**2 / 16.0_dp - 55.0_dp * e1**4 / 32.0_dp
      j3 = 151.0_dp * e1**3 / 96.0_dp
      j4 = 1097.0_dp * e1**4 / 512.0_dp
      phi1 = mu + j1 * sin(2.0_dp * mu) + j2 * sin(4.0_dp * mu) + j3 * sin(6.0_dp * mu) + j4 * sin(8.0_dp * mu)

      c1 = ep2 * cos(phi1)**2
      t1 = tan(phi1)**2
      n1 = sa / sqrt(1.0_dp - e2 * sin(phi1)**2)
      r1 = sa * (1.0_dp - e2) / (1.0_dp - e2 * sin(phi1)**2)**1.5_dp
      d = x / (n1 * k0)

      lat = phi1 - (n1 * tan(phi1) / r1) * ( &
            d**2 / 2.0_dp - (5.0_dp + 3.0_dp * t1 + 10.0_dp * c1 - 4.0_dp * c1**2 - 9.0_dp * ep2) * d**4 / 24.0_dp + &
            (61.0_dp + 90.0_dp * t1 + 298.0_dp * c1 + 45.0_dp * t1**2 - 252.0_dp * ep2 - 3.0_dp * c1**2) * d**6 / 720.0_dp)

      lon0 = ((real(huso) * 6.0_dp) - 183.0_dp) * pi_hp / 180.0_dp
      lon = lon0 + (d - (1.0_dp + 2.0_dp * t1 + c1) * d**3 / 6.0_dp + &
                    (5.0_dp - 2.0_dp * c1 + 28.0_dp * t1 - 3.0_dp * c1**2 + 8.0_dp * ep2 + 24.0_dp * t1**2) * d**5 / 120.0_dp) / cos(phi1)

      lat = lat * 180.0_dp / pi_hp
      lon = lon * 180.0_dp / pi_hp
   end subroutine utm2deg

   !> Return numeric UTM zone from a UTM zone string.
   !!
   !! @param[in] utmzone UTM zone string.
   !! @return            Zone number in range 1..60, or 0 when invalid.
   pure function zone_number(utmzone) result(huso)
      character(len=*), intent(in) :: utmzone
      integer :: huso
      character(len=1) :: hemi
      logical :: valid_utm_zone
      call parse_utm_zone(utmzone, huso, hemi, valid_utm_zone)
      if (.not. valid_utm_zone) huso = 0
   end function zone_number

   !> Check whether a UTM zone string is valid.
   !!
   !! Valid format is a numeric zone 1..60 followed by hemisphere N or S,
   !! for example 31N or 22S.
   !!
   !! @param[in] utmzone         UTM zone string to validate.
   !! @return                    .true. if the UTM zone string is valid.
   pure function is_valid_utm_zone(utmzone) result(valid_utm_zone)
      character(len=*), intent(in) :: utmzone
      logical :: valid_utm_zone
      integer :: huso
      character(len=1) :: hemi
      call parse_utm_zone(utmzone, huso, hemi, valid_utm_zone)
   end function is_valid_utm_zone

   !> Parse a UTM zone string into numeric zone and hemisphere.
   !!
   !! This helper accepts optional leading spaces and lowercase hemisphere
   !! letters, and normalizes hemisphere to uppercase.
   !!
   !! @param[in]  utmzone         UTM zone string.
   !! @param[out] huso            Parsed zone number.
   !! @param[out] hemi            Parsed hemisphere (N/S).
   !! @param[out] valid_utm_zone  .true. when utmzone is valid.
   pure subroutine parse_utm_zone(utmzone, huso, hemi, valid_utm_zone)
      character(len=*), intent(in) :: utmzone
      integer, intent(out) :: huso
      character(len=1), intent(out) :: hemi
      logical, intent(out) :: valid_utm_zone
      character(len=max(1, len(utmzone))) :: zone
      integer :: i, n

      huso = 0
      hemi = ' '
      valid_utm_zone = .false.

      zone = adjustl(utmzone)
      n = len_trim(zone)
      if (n < 2 .or. n > 3) return

      hemi = zone(n:n)
      if (hemi >= 'a' .and. hemi <= 'z') hemi = achar(iachar(hemi) - iachar('a') + iachar('A')) ! capitalize
      if (hemi /= 'N' .and. hemi /= 'S') return

      do i = 1, n - 1
         if (zone(i:i) < '0' .or. zone(i:i) > '9') return
         huso = huso * 10 + iachar(zone(i:i)) - iachar('0')
      end do

      valid_utm_zone = huso >= 1 .and. huso <= 60
   end subroutine parse_utm_zone

end module m_ec_utm_inverse
