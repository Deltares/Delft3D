module m_ec_utm_inverse

   use precision
   use m_ec_message, only: set_ec_message

   implicit none
   private
   public :: utm2deg
   public :: is_valid_utm_zone

contains

   subroutine utm2deg(xx, yy, utmzone, lo, la, success)
      real(kind=dp), intent(in) :: xx, yy
      character(len=*), intent(in) :: utmzone
      real(kind=dp), intent(out) :: lo, la
      logical, optional, intent(out) :: success

      real(kind=dp), parameter :: pi = acos(-1.0_dp)
      real(kind=dp), parameter :: sa = 6378137.0_dp
      real(kind=dp), parameter :: sb = 6356752.314245_dp
      real(kind=dp), parameter :: k0 = 0.9996_dp
      real(kind=dp), parameter :: false_easting = 500000.0_dp
      real(kind=dp), parameter :: false_northing_south = 10000000.0_dp

      integer :: huso
      character(len=1) :: hemi
      logical :: valid_utm_zone
      real(kind=dp) :: x, y, lon0
      real(kind=dp) :: e2, e4, e6, ep2, e1
      real(kind=dp) :: m, mu, phi1, c1, t1, n1, r1, d
      real(kind=dp) :: j1, j2, j3, j4

      call parse_utm_zone(utmzone, huso, hemi, valid_utm_zone)
      if (.not. valid_utm_zone) then
         lo = 0.0_dp
         la = 0.0_dp
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

      la = phi1 - (n1 * tan(phi1) / r1) * ( &
           d**2 / 2.0_dp - (5.0_dp + 3.0_dp * t1 + 10.0_dp * c1 - 4.0_dp * c1**2 - 9.0_dp * ep2) * d**4 / 24.0_dp + &
           (61.0_dp + 90.0_dp * t1 + 298.0_dp * c1 + 45.0_dp * t1**2 - 252.0_dp * ep2 - 3.0_dp * c1**2) * d**6 / 720.0_dp)

      lon0 = ((real(huso) * 6.0_dp) - 183.0_dp) * pi / 180.0_dp
      lo = lon0 + (d - (1.0_dp + 2.0_dp * t1 + c1) * d**3 / 6.0_dp + &
                   (5.0_dp - 2.0_dp * c1 + 28.0_dp * t1 - 3.0_dp * c1**2 + 8.0_dp * ep2 + 24.0_dp * t1**2) * d**5 / 120.0_dp) / cos(phi1)

      la = la * 180.0_dp / pi
      lo = lo * 180.0_dp / pi
   end subroutine utm2deg

   pure function zone_number(utmzone) result(huso)
      character(len=*), intent(in) :: utmzone
      integer :: huso
      character(len=1) :: hemi
      logical :: valid_utm_zone
      call parse_utm_zone(utmzone, huso, hemi, valid_utm_zone)
      if (.not. valid_utm_zone) huso = 0
   end function zone_number

   pure function is_valid_utm_zone(utmzone) result(valid_utm_zone)
      character(len=*), intent(in) :: utmzone
      logical :: valid_utm_zone
      integer :: huso
      character(len=1) :: hemi
      call parse_utm_zone(utmzone, huso, hemi, valid_utm_zone)
   end function is_valid_utm_zone

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
