module m_ec_utm_inverse
   
   use precision
   
   implicit none
   private
   public :: utm2deg

contains

  subroutine utm2deg(xx, yy, utmzone, lo, la)
    real(kind=dp),    intent(in)  :: xx, yy
    character(len=3), intent(in)  :: utmzone
    real(kind=dp),    intent(out) :: lo, la

    real(kind=dp), parameter :: pi = 3.14159265358979323846
    real(kind=dp), parameter :: sa = 6378137.0
    real(kind=dp), parameter :: sb = 6356752.314245
    real(kind=dp), parameter :: k0 = 0.9996

    integer :: huso
    character(len=1) :: hemi
    real(kind=dp) :: x, y, lon0
    real(kind=dp) :: e2, e4, e6, ep2, e1
    real(kind=dp) :: m, mu, phi1, c1, t1, n1, r1, d
    real(kind=dp) :: j1, j2, j3, j4

    huso = zone_number(utmzone)
    if (len_trim(utmzone) >= 1) then
      hemi = utmzone(len_trim(utmzone):len_trim(utmzone))
    else
      hemi = 'N'
    end if

    x = xx - 500000.0
    y = yy
    if (hemi == 'S' .or. hemi == 's') y = y - 9999999.0

    e2  = (sa*sa - sb*sb) / (sa*sa)
    e4  = e2*e2
    e6  = e4*e2
    ep2 = e2 / (1.0 - e2)
    e1  = (1.0 - sqrt(1.0 - e2)) / (1.0 + sqrt(1.0 - e2))

    m  = y / k0
    mu = m / (sa * (1.0 - e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0))

    j1   = 3.0*e1/2.0 - 27.0*e1**3/32.0
    j2   = 21.0*e1**2/16.0 - 55.0*e1**4/32.0
    j3   = 151.0*e1**3/96.0
    j4   = 1097.0*e1**4/512.0
    phi1 = mu + j1*sin(2.0*mu) + j2*sin(4.0*mu) + j3*sin(6.0*mu) + j4*sin(8.0*mu)

    c1 = ep2 * cos(phi1)**2
    t1 = tan(phi1)**2
    n1 = sa / sqrt(1.0 - e2*sin(phi1)**2)
    r1 = sa * (1.0 - e2) / (1.0 - e2*sin(phi1)**2)**1.5
    d  = x / (n1 * k0)

    la = phi1 - (n1*tan(phi1)/r1) * ( &
            d**2/2.0 - (5.0 + 3.0*t1 + 10.0*c1 - 4.0*c1**2 - 9.0*ep2)*d**4/24.0 + &
            (61.0 + 90.0*t1 + 298.0*c1 + 45.0*t1**2 - 252.0*ep2 - 3.0*c1**2)*d**6/720.0 )

    lon0  = ((real(huso) * 6.0) - 183.0) * pi/180.0
    lo = lon0 + ( d - (1.0 + 2.0*t1 + c1)*d**3/6.0 + &
            (5.0 - 2.0*c1 + 28.0*t1 - 3.0*c1**2 + 8.0*ep2 + 24.0*t1**2)*d**5/120.0 ) / cos(phi1)

    la = la * 180.0/pi
    lo = lo * 180.0/pi
  end subroutine utm2deg

  pure function zone_number(utmzone) result(huso)
    character(len=*), intent(in) :: utmzone
    integer :: huso
    integer :: i, n
    huso = 0
    n = min(2, len_trim(utmzone))
    do i = 1, n
      if (utmzone(i:i) >= '0' .and. utmzone(i:i) <= '9') then
        huso = huso*10 + iachar(utmzone(i:i)) - iachar('0')
      end if
    end do
  end function zone_number

end module m_ec_utm_inverse
