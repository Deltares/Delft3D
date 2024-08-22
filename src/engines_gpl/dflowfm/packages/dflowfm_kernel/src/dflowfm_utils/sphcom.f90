!
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     *                                                               *
!     *                  copyright (c) 1998-2024 by UCAR                   *
!     *                                                               *
!     *       University Corporation for Atmospheric Research         *
!     *                                                               *
!     *                      all rights reserved                      *
!     *                                                               *
!     *                      SPHEREPACK version 3.2                   *
!     *                                                               *
!     *       A Package of Fortran77 Subroutines and Programs         *
!     *                                                               *
!     *              for Modeling Geophysical Processes               *
!     *                                                               *
!     *                             by                                *
!     *                                                               *
!     *                  John Adams and Paul Swarztrauber             *
!     *                                                               *
!     *                             of                                *
!     *                                                               *
!     *         the National Center for Atmospheric Research          *
!     *                                                               *
!     *                Boulder, Colorado  (80307)  U.S.A.             *
!     *                                                               *
!     *                   which is sponsored by                       *
!     *                                                               *
!     *              the National Science Foundation                  *
!     *                                                               *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!
! ... file sphcom.f
!
!     this file must be loaded with all main program files
!     in spherepack.  it includes undocumented subroutines
!     called by some or all of main programs
!
subroutine dnlfk(m, n, cp)
   use precision_basics, only: dp

   implicit none
!
!     cp requires n/2+1 double precision locations
!
   integer, intent(in) :: m, n
   real(dp), intent(out) :: cp(1)
   
   real(dp) :: fnum, fden, fnmh, a1, b1, c1, cp2, fnnp1, fnmsq, fk, t1, t2, pm1, sc10, sc20, sc40
   parameter(sc10=1024.d0)
   parameter(sc20=sc10 * sc10)
   parameter(sc40=sc20 * sc20)
   integer :: ma, nmms2, i, l, nex
!
   cp(1) = 0.
   ma = abs(m)
   if (ma > n) return
   if (n - 1) 2, 3, 5
2  cp(1) = sqrt(2.d0)
   return
3  if (ma /= 0) go to 4
   cp(1) = sqrt(1.5d0)
   return
4  cp(1) = sqrt(.75d0)
   if (m == -1) cp(1) = -cp(1)
   return
5  if (mod(n + ma, 2) /= 0) go to 10
   nmms2 = (n - ma) / 2
   fnum = n + ma + 1
   fnmh = n - ma + 1
   pm1 = 1.d0
   go to 15
10 nmms2 = (n - ma - 1) / 2
   fnum = n + ma + 2
   fnmh = n - ma + 2
   pm1 = -1.d0
!      t1 = 1.
!      t1 = 2.d0**(n-1)
!      t1 = 1.d0/t1
15 t1 = 1.d0 / sc20
   nex = 20
   fden = 2.d0
   if (nmms2 < 1) go to 20
   do i = 1, nmms2
      t1 = fnum * t1 / fden
      if (t1 > sc20) then
         t1 = t1 / sc40
         nex = nex + 40
      end if
      fnum = fnum + 2.
      fden = fden + 2.
   end do
20 t1 = t1 / 2.d0**(n - 1 - nex)
   if (mod(ma / 2, 2) /= 0) t1 = -t1
   t2 = 1.
   if (ma == 0) go to 26
   do i = 1, ma
      t2 = fnmh * t2 / (fnmh + pm1)
      fnmh = fnmh + 2.
   end do
26 cp2 = t1 * sqrt((n + .5d0) * t2)
   fnnp1 = n * (n + 1)
   fnmsq = fnnp1 - 2.d0 * ma * ma
   l = (n + 1) / 2
   if (mod(n, 2) == 0 .and. mod(ma, 2) == 0) l = l + 1
   cp(l) = cp2
   if (m >= 0) go to 29
   if (mod(ma, 2) /= 0) cp(l) = -cp(l)
29 if (l <= 1) return
   fk = n
   a1 = (fk - 2.) * (fk - 1.) - fnnp1
   b1 = 2.*(fk * fk - fnmsq)
   cp(l - 1) = b1 * cp(l) / a1
30 l = l - 1
   if (l <= 1) return
   fk = fk - 2.
   a1 = (fk - 2.) * (fk - 1.) - fnnp1
   b1 = -2.*(fk * fk - fnmsq)
   c1 = (fk + 1.) * (fk + 2.) - fnnp1
   cp(l - 1) = -(b1 * cp(l) + c1 * cp(l + 1)) / a1
   go to 30
end

subroutine dnlft(m, n, theta, cp, pb)
   use precision_basics, only: dp

   implicit none

   integer :: m, n
   real(dp) :: theta, cp(:), pb
   
   real(dp) :: cdt, sdt, cth, sth, chh
   integer :: k, kdo, nmod, mmod 
   
   cdt = cos(theta + theta)
   sdt = sin(theta + theta)
   nmod = mod(n, 2)
   mmod = mod(m, 2)
   if (nmod) 1, 1, 2
1  if (mmod) 3, 3, 4
!
!     n even, m even
!
3  kdo = n / 2
   pb = .5 * cp(1)
   if (n == 0) return
   cth = cdt
   sth = sdt
   do k = 1, kdo
!     pb = pb+cp(k+1)*cos(2*k*theta)
      pb = pb + cp(k + 1) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     n even, m odd
!
4  kdo = n / 2
   pb = 0.
   cth = cdt
   sth = sdt
   do k = 1, kdo
!     pb = pb+cp(k)*sin(2*k*theta)
      pb = pb + cp(k) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
2  if (mmod) 13, 13, 14
!
!     n odd, m even
!
13 kdo = (n + 1) / 2
   pb = 0.
   cth = cos(theta)
   sth = sin(theta)
   do k = 1, kdo
!     pb = pb+cp(k)*cos((2*k-1)*theta)
      pb = pb + cp(k) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     n odd, m odd
!
14 kdo = (n + 1) / 2
   pb = 0.
   cth = cos(theta)
   sth = sin(theta)
   do k = 1, kdo
!     pb = pb+cp(k)*sin((2*k-1)*theta)
      pb = pb + cp(k) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
end

subroutine dnlftd(m, n, theta, cp, pb)
   use precision_basics, only: dp

   implicit none

!
!     computes the derivative of pmn(theta) with respect to theta
!
   integer, intent(in) :: m, n
   real(dp), intent(inout) :: cp(1), pb, theta
   
   real(dp) :: cdt, sdt, cth, sth, chh
   integer :: k, kdo, nmod, mmod 
   
   cdt = cos(theta + theta)
   sdt = sin(theta + theta)
   nmod = mod(n, 2)
   mmod = mod(abs(m), 2)
   if (nmod) 1, 1, 2
1  if (mmod) 3, 3, 4
!
!     n even, m even
!
3  kdo = n / 2
   pb = 0.d0
   if (n == 0) return
   cth = cdt
   sth = sdt
   do k = 1, kdo
!     pb = pb+cp(k+1)*cos(2*k*theta)
      pb = pb - 2.d0 * k * cp(k + 1) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     n even, m odd
!
4  kdo = n / 2
   pb = 0.
   cth = cdt
   sth = sdt
   do k = 1, kdo
!     pb = pb+cp(k)*sin(2*k*theta)
      pb = pb + 2.d0 * k * cp(k) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
2  if (mmod) 13, 13, 14
!
!     n odd, m even
!
13 kdo = (n + 1) / 2
   pb = 0.
   cth = cos(theta)
   sth = sin(theta)
   do k = 1, kdo
!     pb = pb+cp(k)*cos((2*k-1)*theta)
      pb = pb - (2.d0 * k - 1) * cp(k) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     n odd, m odd
!
14 kdo = (n + 1) / 2
   pb = 0.
   cth = cos(theta)
   sth = sin(theta)
   do k = 1, kdo
!     pb = pb+cp(k)*sin((2*k-1)*theta)
      pb = pb + (2.d0 * k - 1) * cp(k) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
end
    

!>     this subroutine computes legendre polynomials for n=m,...,l-1
!!     and  i=1,...,late (late=((nlat+mod(nlat,2))/2) gaussian grid
!!     in pmn(n+1,i,km) using swarztrauber's recursion formula.
!!     the vector w contains quantities precomputed in shigc.
!!     legin must be called in the order m=0,1,...,l-1
!!     (e.g., if m=10 is sought it must be preceded by calls with
!!     m=0,1,2,...,9 in that order)
subroutine legin(mode, l, nlat, m, w, pmn, km)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: mode, l, nlat, m, km
   real(dp), dimension(:), intent(inout) :: w, pmn
   
   integer :: late, i1, i2, i3, i4, i5
   
!     set size of pole to equator gaussian grid
   late = (nlat + mod(nlat, 2)) / 2
!     partition w (set pointers for p0n,p1n,abel,bbel,cbel,pmn)
   i1 = 1 + nlat
   i2 = i1 + nlat * late
   i3 = i2 + nlat * late
   i4 = i3 + (2 * nlat - l) * (l - 1) / 2
   i5 = i4 + (2 * nlat - l) * (l - 1) / 2
   call legin1(mode, l, nlat, late, m, w(i1), w(i2), w(i3), w(i4), w(i5), pmn, km)
   return
end

subroutine legin1(mode, l, nlat, late, m, p0n, p1n, abel, bbel, cbel, pmn, km)
   use precision_basics, only: dp
   implicit none

   integer , intent(inout):: mode, l, nlat, late, m, km
   real(dp), intent(inout) :: p0n(nlat, late), p1n(nlat, late)
   real(dp), intent(inout) :: abel(1), bbel(1), cbel(1), pmn(nlat, late, 3)
   
   integer :: km0, km1, km2, kmt, ms, ninc, np1, imn, i, n
   data km0, km1, km2/1, 2, 3/
   save km0, km1, km2
!     define index function used in storing triangular
!     arrays for recursion coefficients (functions of (m,n))
!     set do loop indices for full or half sphere
   ms = m + 1
   ninc = 1
   if (mode == 1) then
!     only compute pmn for n-m odd
      ms = m + 2
      ninc = 2
   else if (mode == 2) then
!     only compute pmn for n-m even
      ms = m + 1
      ninc = 2
   end if

   if (m > 1) then
      do np1 = ms, nlat, ninc
         n = np1 - 1
         imn = (n - 1) * (n - 2) / 2 + m - 1
         if (n >= l) imn = l * (l - 1) / 2 + (n - l - 1) * (l - 1) + m - 1
         do i = 1, late
            pmn(np1, i, km0) = abel(imn) * pmn(n - 1, i, km2)&
            &+ bbel(imn) * pmn(n - 1, i, km0)&
            &- cbel(imn) * pmn(np1, i, km2)
         end do
      end do

   else if (m == 0) then
      do np1 = ms, nlat, ninc
         do i = 1, late
            pmn(np1, i, km0) = p0n(np1, i)
         end do
      end do

   else if (m == 1) then
      do np1 = ms, nlat, ninc
         do i = 1, late
            pmn(np1, i, km0) = p1n(np1, i)
         end do
      end do
   end if

!     permute column indices
!     km0,km1,km2 store m,m-1,m-2 columns
   kmt = km0
   km0 = km2
   km2 = km1
   km1 = kmt
!     set current m index in output param km
   km = kmt
   return
end

subroutine zfin(isym, nlat, nlon, m, z, i3, wzfin)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: isym, nlat, nlon, m, i3
   real(dp), dimension(:), intent(inout) :: z, wzfin
   
   integer :: imid, lim, mmax, labc, iw1, iw2, iw3, iw4, ihold, i1, i2, i4
   
   imid = (nlat + 1) / 2
   lim = nlat * imid
   mmax = min(nlat, nlon / 2 + 1)
   labc = ((mmax - 2) * (nlat + nlat - mmax - 1)) / 2
   iw1 = lim + 1
   iw2 = iw1 + lim
   iw3 = iw2 + labc
   iw4 = iw3 + labc
!
!     the length of wzfin is 2*lim+3*labc
!
   call zfin1(isym, nlat, m, z, imid, i3, wzfin, wzfin(iw1), wzfin(iw2), wzfin(iw3), wzfin(iw4))
   return
end

subroutine zfin1(isym, nlat, m, z, imid, i3, zz, z1, a, b, c)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: isym, nlat, m, imid, i3
   real(dp), intent(inout) :: z(imid, nlat, 3), zz(imid, 1), z1(imid, 1), a(1), b(1), c(1)
   
   integer :: i1, i2, ihold, np1, i, ns, nstrt, nstp
   save i1, i2, ihold
   
   ihold = i1
   i1 = i2
   i2 = i3
   i3 = ihold
   if (m - 1) 25, 30, 35
25 i1 = 1
   i2 = 2
   i3 = 3
   do np1 = 1, nlat
      do i = 1, imid
         z(i, np1, i3) = zz(i, np1)
      end do
   end do
   return
30 do np1 = 2, nlat
      do i = 1, imid
         z(i, np1, i3) = z1(i, np1)
      end do
   end do
   return
35 ns = ((m - 2) * (nlat + nlat - m - 1)) / 2 + 1
   if (isym == 1) go to 36
   do i = 1, imid
      z(i, m + 1, i3) = a(ns) * z(i, m - 1, i1) - c(ns) * z(i, m + 1, i1)
   end do
36 if (m == nlat - 1) return
   if (isym == 2) go to 71
   ns = ns + 1
   do i = 1, imid
      z(i, m + 2, i3) = a(ns) * z(i, m, i1) - c(ns) * z(i, m + 2, i1)
   end do
71 nstrt = m + 3
   if (isym == 1) nstrt = m + 4
   if (nstrt > nlat) go to 80
   nstp = 2
   if (isym == 0) nstp = 1
   do np1 = nstrt, nlat, nstp
      ns = ns + nstp
      do i = 1, imid
         z(i, np1, i3) = a(ns) * z(i, np1 - 2, i1) + b(ns) * z(i, np1 - 2, i3)&
         &- c(ns) * z(i, np1, i1)
      end do
   end do
80 return
end
    
subroutine zfinit(nlat, nlon, wzfin, dwork)
   use precision_basics, only: dp

   implicit none

   integer, intent(in) :: nlat, nlon
   real(dp), dimension(:), intent(inout) :: wzfin, dwork
   
   integer :: imid, iw1
   
   imid = (nlat + 1) / 2
   iw1 = 2 * nlat * imid + 1
!
!     the length of wzfin is 3*((l-3)*l+2)/2 + 2*l*imid
!     the length of dwork is nlat+2
!
   call zfini1(nlat, nlon, imid, wzfin, wzfin(iw1), dwork, dwork(nlat / 2 + 1))
   return
end

subroutine zfini1(nlat, nlon, imid, z, abc, cz, work)
!
!     abc must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations
!     where mmax = min0(nlat,nlon/2+1)
!     cz and work must each have nlat+1 locations
!
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: nlat, nlon, imid
   real(dp), intent(inout) :: z(imid, nlat, 2), abc(1), cz(:), work(:)
   
   integer :: np1, mp1, m, n, i
   real(dp) :: pi, dt, th, zh
   
   pi = 4.*atan(1.d0)
   dt = pi / (nlat - 1)
   do mp1 = 1, 2
      m = mp1 - 1
      do np1 = mp1, nlat
         n = np1 - 1
         call dnzfk(nlat, m, n, cz, work)
         do i = 1, imid
            th = (i - 1) * dt
            call dnzft(nlat, m, n, th, cz, zh)
            z(i, np1, mp1) = zh
         end do
         z(1, np1, mp1) = .5 * z(1, np1, mp1)
      end do
   end do
   call rabcp(nlat, nlon, abc)
   return
end

!>     dnzfk computes the coefficients in the trigonometric
!!     expansion of the z functions that are used in spherical
!!     harmonic analysis.
!!
subroutine dnzfk(nlat, m, n, cz, work)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: nlat, m, n
   real(dp), intent(inout) :: cz(1), work(1)
!
!     cz and work must both have nlat/2+1 locations
!
   integer :: lc, nmod, mmod, kdo, idx, i, kp1, k
   real(dp) :: sum, sc1, t1, t2
   
   lc = (nlat + 1) / 2
   sc1 = 2.d0 / real(nlat - 1, kind=kind(sc1))
   call dnlfk(m, n, work)
   nmod = mod(n, 2)
   mmod = mod(m, 2)
   if (nmod) 1, 1, 2
1  if (mmod) 3, 3, 4
!
!     n even, m even
!
3  kdo = n / 2 + 1
   do idx = 1, lc
      i = idx + idx - 2
      sum = work(1) / (1.d0 - i * i)
      if (kdo < 2) go to 29
      do kp1 = 2, kdo
         k = kp1 - 1
         t1 = 1.d0 - (k + k + i)**2
         t2 = 1.d0 - (k + k - i)**2
8        sum = sum + work(kp1) * (t1 + t2) / (t1 * t2)
      end do
29    cz(idx) = sc1 * sum
   end do
   return
!
!     n even, m odd
!
4  kdo = n / 2
   do idx = 1, lc
      i = idx + idx - 2
      sum = 0.
      do k = 1, kdo
         t1 = 1.d0 - (k + k + i)**2
         t2 = 1.d0 - (k + k - i)**2
12       sum = sum + work(k) * (t1 - t2) / (t1 * t2)
      end do
      cz(idx) = sc1 * sum
   end do
   return
2  if (mmod) 13, 13, 14
!
!     n odd, m even
!
13 kdo = (n + 1) / 2
   do idx = 1, lc
      i = idx + idx - 1
      sum = 0.
      do k = 1, kdo
         t1 = 1.d0 - (k + k - 1 + i)**2
         t2 = 1.d0 - (k + k - 1 - i)**2
18       sum = sum + work(k) * (t1 + t2) / (t1 * t2)
      end do
      cz(idx) = sc1 * sum
   end do
   return
!
!     n odd, m odd
!
14 kdo = (n + 1) / 2
   do idx = 1, lc
      i = idx + idx - 3
      sum = 0.
      do k = 1, kdo
         t1 = 1.d0 - (k + k - 1 + i)**2
         t2 = 1.d0 - (k + k - 1 - i)**2
22       sum = sum + work(k) * (t1 - t2) / (t1 * t2)
      end do
      cz(idx) = sc1 * sum
   end do
   return
end
    
subroutine dnzft(nlat, m, n, th, cz, zh)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: nlat, m, n
   real(dp), intent(inout) :: th, cz(1), zh
   
   integer :: lmod, mmod, nmod, lc, lq, ls, k
   real(dp) :: cdt, sdt, cth, sth, chh
   
   zh = 0.
   cdt = cos(th + th)
   sdt = sin(th + th)
   lmod = mod(nlat, 2)
   mmod = mod(m, 2)
   nmod = mod(n, 2)
   if (lmod) 20, 20, 10
10 lc = (nlat + 1) / 2
   lq = lc - 1
   ls = lc - 2
   if (nmod) 1, 1, 2
1  if (mmod) 3, 3, 4
!
!     nlat odd n even m even
!
3  zh = .5 * (cz(1) + cz(lc) * cos(2 * lq * th))
   cth = cdt
   sth = sdt
   do k = 2, lq
!     zh = zh+cz(k)*cos(2*(k-1)*th)
      zh = zh + cz(k) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     nlat odd n even m odd
!
4  cth = cdt
   sth = sdt
   do k = 1, ls
!     zh = zh+cz(k+1)*sin(2*k*th)
      zh = zh + cz(k + 1) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     nlat odd n odd, m even
!
2  if (mmod) 5, 5, 6
5  cth = cos(th)
   sth = sin(th)
   do k = 1, lq
!     zh = zh+cz(k)*cos((2*k-1)*th)
      zh = zh + cz(k) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     nlat odd n odd m odd
!
6  cth = cos(th)
   sth = sin(th)
   do k = 1, lq
!     zh = zh+cz(k+1)*sin((2*k-1)*th)
      zh = zh + cz(k + 1) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
20 lc = nlat / 2
   lq = lc - 1
   if (nmod) 30, 30, 80
30 if (mmod) 40, 40, 60
!
!     nlat even n even m even
!
40 zh = .5 * cz(1)
   cth = cdt
   sth = sdt
   do k = 2, lc
!     zh = zh+cz(k)*cos(2*(k-1)*th)
      zh = zh + cz(k) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     nlat even n even m odd
!
60 cth = cdt
   sth = sdt
   do k = 1, lq
!     zh = zh+cz(k+1)*sin(2*k*th)
      zh = zh + cz(k + 1) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     nlat even n odd m even
!
80 if (mmod) 90, 90, 110
90 zh = .5 * cz(lc) * cos((nlat - 1) * th)
   cth = cos(th)
   sth = sin(th)
   do k = 1, lq
!     zh = zh+cz(k)*cos((2*k-1)*th)
      zh = zh + cz(k) * cth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
!
!     nlat even n odd m odd
!
110 cth = cos(th)
   sth = sin(th)
   do k = 1, lq
!     zh = zh+cz(k+1)*sin((2*k-1)*th)
      zh = zh + cz(k + 1) * sth
      chh = cdt * cth - sdt * sth
      sth = sdt * cth + cdt * sth
      cth = chh
   end do
   return
end
    
subroutine alin(isym, nlat, nlon, m, p, i3, walin)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: isym, nlat, nlon, m, i3
   real(dp), intent(inout) :: p(1), walin(1)
   
   integer :: imid, lim, mmax, labc, iw1, iw2, iw3, iw4
   
   imid = (nlat + 1) / 2
   lim = nlat * imid
   mmax = min(nlat, nlon / 2 + 1)
   labc = ((mmax - 2) * (nlat + nlat - mmax - 1)) / 2
   iw1 = lim + 1
   iw2 = iw1 + lim
   iw3 = iw2 + labc
   iw4 = iw3 + labc
!
!     the length of walin is ((5*l-7)*l+6)/2
!
   call alin1(isym, nlat, m, p, imid, i3, walin, walin(iw1), walin(iw2), walin(iw3), walin(iw4))
   return
end

subroutine alin1(isym, nlat, m, p, imid, i3, pz, p1, a, b, c)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: isym, nlat, m, imid, i3
   real(dp), intent(inout) :: p(imid, nlat, 3), pz(imid, 1), p1(imid, 1), a(1), b(1), c(1)
   
   integer :: ihold, i1, i2, i4, mp1, i, np1, ns, nstrt, nstp
   save i1, i2
   
   ihold = i1
   i1 = i2
   i2 = i3
   i3 = ihold
   if (m - 1) 25, 30, 35
25 i1 = 1
   i2 = 2
   i3 = 3
   do np1 = 1, nlat
      do i = 1, imid
         p(i, np1, i3) = pz(i, np1)
      end do
   end do
   return
30 do np1 = 2, nlat
      do i = 1, imid
         p(i, np1, i3) = p1(i, np1)
      end do
   end do
   return
35 ns = ((m - 2) * (nlat + nlat - m - 1)) / 2 + 1
   if (isym == 1) go to 36
   do i = 1, imid
      p(i, m + 1, i3) = a(ns) * p(i, m - 1, i1) - c(ns) * p(i, m + 1, i1)
   end do
36 if (m == nlat - 1) return
   if (isym == 2) go to 71
   ns = ns + 1
   do i = 1, imid
      p(i, m + 2, i3) = a(ns) * p(i, m, i1) - c(ns) * p(i, m + 2, i1)
   end do
71 nstrt = m + 3
   if (isym == 1) nstrt = m + 4
   if (nstrt > nlat) go to 80
   nstp = 2
   if (isym == 0) nstp = 1
   do np1 = nstrt, nlat, nstp
      ns = ns + nstp
      do i = 1, imid
         p(i, np1, i3) = a(ns) * p(i, np1 - 2, i1) + b(ns) * p(i, np1 - 2, i3)&
         &- c(ns) * p(i, np1, i1)
      end do
   end do
80 return
end

subroutine alinit(nlat, nlon, walin, dwork)
   use precision_basics, only: dp

   implicit none
   
   integer, intent(inout) :: nlat, nlon
   real(dp), intent(inout) :: walin(:), dwork(:)
   
   integer :: imid, iw1
   
   imid = (nlat + 1) / 2
   iw1 = 2 * nlat * imid + 1
!
!     the length of walin is 3*((l-3)*l+2)/2 + 2*l*imid
!     the length of work is nlat+1
!
   call alini1(nlat, nlon, imid, walin, walin(iw1), dwork)
   return
end

subroutine alini1(nlat, nlon, imid, p, abc, cp)
   use precision_basics, only: dp

   implicit none

   integer, intent(inout) :: nlat, nlon, imid
   real(dp), intent(inout) :: p(imid, nlat, 2), abc(1), cp(1)
   
   integer :: mp1, m, n, np1, i
   real(dp) :: pi, dt, th, ph
   
   pi = 4.*atan(1.d0)
   dt = pi / (nlat - 1)
   do mp1 = 1, 2
      m = mp1 - 1
      do np1 = mp1, nlat
         n = np1 - 1
         call dnlfk(m, n, cp)
         do i = 1, imid
            th = (i - 1) * dt
            call dnlft(m, n, th, cp, ph)
            p(i, np1, mp1) = ph
         end do
      end do
   end do
   call rabcp(nlat, nlon, abc)
   return
end

!>     subroutine rabcp computes the coefficients in the recurrence
!!     relation for the associated legendre fuctions. array abc
!!     must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations.
subroutine rabcp(nlat, nlon, abc)
   use precision_basics, only: dp

   implicit none

   integer, intent(in) :: nlat, nlon
   real(dp), intent(inout) :: abc(1)
   
   integer :: mmax, labc, iw1, iw2
   
   mmax = min(nlat, nlon / 2 + 1)
   labc = ((mmax - 2) * (nlat + nlat - mmax - 1)) / 2
   iw1 = labc + 1
   iw2 = iw1 + labc
   call rabcp1(nlat, nlon, abc, abc(iw1), abc(iw2))
   return
end

subroutine rabcp1(nlat, nlon, a, b, c)
!
!     coefficients a, b, and c for computing pbar(m,n,theta) are
!     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
!
   use precision_basics, only: dp

   implicit none

   integer, intent(in) :: nlat, nlon
   real(dp), intent(inout) :: a(1), b(1), c(1)
   
   integer :: mmax, mp1, m, ns, mp3, np1, n
   real(dp) :: fm, tm, temp, fn, tn, cn, fnpm, fnmm
   
   mmax = min(nlat, nlon / 2 + 1)
   do mp1 = 3, mmax
      m = mp1 - 1
      ns = ((m - 2) * (nlat + nlat - m - 1)) / 2 + 1
      fm = real(m, kind=kind(fm))
      tm = fm + fm
      temp = tm * (tm - 1.)
      a(ns) = sqrt((tm + 1.) * (tm - 2.) / temp)
      c(ns) = sqrt(2./temp)
      if (m == nlat - 1) cycle
      ns = ns + 1
      temp = tm * (tm + 1.)
      a(ns) = sqrt((tm + 3.) * (tm - 2.) / temp)
      c(ns) = sqrt(6./temp)
      mp3 = m + 3
      if (mp3 > nlat) cycle
      do np1 = mp3, nlat
         n = np1 - 1
         ns = ns + 1
         fn = real(n, kind=kind(fn))
         tn = fn + fn
         cn = (tn + 1.) / (tn - 3.)
         fnpm = fn + fm
         fnmm = fn - fm
         temp = fnpm * (fnpm - 1.)
         a(ns) = sqrt(cn * (fnpm - 3.) * (fnpm - 2.) / temp)
         b(ns) = sqrt(cn * fnmm * (fnmm - 1.) / temp)
         c(ns) = sqrt((fnmm + 1.) * (fnmm + 2.) / temp)
      end do
   end do
   return
end

