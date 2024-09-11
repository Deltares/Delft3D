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
module m_shaec
   use hrfft
   implicit none
   public shaec
contains
!
! ... file shaec.f
!
!     this file contains code and documentation for subroutines
!     shaec and shaeci
!
! ... files which must be loaded with shaec.f
!
!     sphcom.f, hrfft.f
!
!     subroutine shaec(nlat,nlon,isym,nt,g,idg,jdg,a,b,mdab,ndab,
!    +                 wshaec,lshaec,work,lwork,ierror)
!
!     subroutine shaec performs the spherical harmonic analysis
!     on the array g and stores the result in the arrays a and b.
!     the analysis is performed on an equally spaced grid.  the
!     associated legendre functions are recomputed rather than stored
!     as they are in subroutine shaes.  the analysis is described
!     below at output parameters a,b.
!
!     input parameters
!
!     nlat   the number of colatitudes on the full sphere including the
!            poles. for example, nlat = 37 for a five degree grid.
!            nlat determines the grid increment in colatitude as
!            pi/(nlat-1).  if nlat is odd the equator is located at
!            grid point i=(nlat+1)/2. if nlat is even the equator is
!            located half way between points i=nlat/2 and i=nlat/2+1.
!            nlat must be at least 3. note: on the half sphere, the
!            number of grid points in the colatitudinal direction is
!            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
!
!     nlon   the number of distinct londitude points.  nlon determines
!            the grid increment in longitude as 2*pi/nlon. for example
!            nlon = 72 for a five degree grid. nlon must be greater
!            than or equal to 4. the efficiency of the computation is
!            improved when nlon is a product of small prime numbers.
!
!     isym   = 0  no symmetries exist about the equator. the analysis
!                 is performed on the entire sphere.  i.e. on the
!                 array g(i,j) for i=1,...,nlat and j=1,...,nlon.
!                 (see description of g below)
!
!            = 1  g is antisymmetric about the equator. the analysis
!                 is performed on the northern hemisphere only.  i.e.
!                 if nlat is odd the analysis is performed on the
!                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
!                 if nlat is even the analysis is performed on the
!                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
!
!
!            = 2  g is symmetric about the equator. the analysis is
!                 performed on the northern hemisphere only.  i.e.
!                 if nlat is odd the analysis is performed on the
!                 array g(i,j) for i=1,...,(nlat+1)/2 and j=1,...,nlon.
!                 if nlat is even the analysis is performed on the
!                 array g(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
!
!     nt     the number of analyses.  in the program that calls shaec,
!            the arrays g,a and b can be three dimensional in which
!            case multiple analyses will be performed.  the third
!            index is the analysis index which assumes the values
!            k=1,...,nt.  for a single analysis set nt=1. the
!            discription of the remaining parameters is simplified
!            by assuming that nt=1 or that the arrays g,a and b
!            have only two dimensions.
!
!     g      a two or three dimensional array (see input parameter
!            nt) that contains the discrete function to be analyzed.
!            g(i,j) contains the value of the function at the colatitude
!            point theta(i) = (i-1)*pi/(nlat-1) and longitude point
!            phi(j) = (j-1)*2*pi/nlon. the index ranges are defined
!            above at the input parameter isym.
!
!
!     idg    the first dimension of the array g as it appears in the
!            program that calls shaec.  if isym equals zero then idg
!            must be at least nlat.  if isym is nonzero then idg
!            must be at least nlat/2 if nlat is even or at least
!            (nlat+1)/2 if nlat is odd.
!
!     jdg    the second dimension of the array g as it appears in the
!            program that calls shaec.  jdg must be at least nlon.
!
!     mdab   the first dimension of the arrays a and b as it appears
!            in the program that calls shaec. mdab must be at least
!            min0(nlat,(nlon+2)/2) if nlon is even or at least
!            min0(nlat,(nlon+1)/2) if nlon is odd.
!
!     ndab   the second dimension of the arrays a and b as it appears
!            in the program that calls shaec. ndab must be at least nlat
!
!     wshaec an array which must be initialized by subroutine shaeci.
!            once initialized, wshaec can be used repeatedly by shaec
!            as long as nlon and nlat remain unchanged.  wshaec must
!            not be altered between calls of shaec.
!
!     lshaec the dimension of the array wshaec as it appears in the
!            program that calls shaec. define
!
!               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
!               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
!
!            and
!
!               l2 = nlat/2        if nlat is even or
!               l2 = (nlat+1)/2    if nlat is odd
!
!            then lshaec must be at least
!
!            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
!
!
!     work   a work array that does not have to be saved.
!
!     lwork  the dimension of the array work as it appears in the
!            program that calls shaec. define
!
!               l2 = nlat/2        if nlat is even or
!               l2 = (nlat+1)/2    if nlat is odd
!
!            if isym is zero then lwork must be at least
!
!                    nlat*(nt*nlon+max0(3*l2,nlon))
!
!            if isym is not zero then lwork must be at least
!
!                    l2*(nt*nlon+max0(3*nlat,nlon))
!
!     **************************************************************
!
!     output parameters
!
!     a,b    both a,b are two or three dimensional arrays (see input
!            parameter nt) that contain the spherical harmonic
!            coefficients in the representation of g(i,j) given in the
!            discription of subroutine shsec. for isym=0, a(m,n) and
!            b(m,n) are given by the equations listed below. symmetric
!            versions are used when isym is greater than zero.
!
!
!
!     definitions
!
!     1. the normalized associated legendre functions
!
!     pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m)))
!                       *sin(theta)**m/(2**n*factorial(n)) times the
!                       (n+m)th derivative of (x**2-1)**n with respect
!                       to x=cos(theta)
!
!     2. the normalized z functions for m even
!
!     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
!                       the integral from tau = 0 to tau = pi of
!                       cos(k*theta)*cos(k*tau)*pbar(m,n,tau)*sin(tau)
!                       (first and last terms in this sum are divided
!                       by 2)
!
!     3. the normalized z functions for m odd
!
!     zbar(m,n,theta) = 2/(nlat-1) times the sum from k=0 to k=nlat-1 of
!                       of the integral from tau = 0 to tau = pi of
!                       sin(k*theta)*sin(k*tau)*pbar(m,n,tau)*sin(tau)
!
!     4. the fourier transform of g(i,j).
!
!     c(m,i)          = 2/nlon times the sum from j=1 to j=nlon
!                       of g(i,j)*cos((m-1)*(j-1)*2*pi/nlon)
!                       (the first and last terms in this sum
!                       are divided by 2)
!
!     s(m,i)          = 2/nlon times the sum from j=2 to j=nlon
!                       of g(i,j)*sin((m-1)*(j-1)*2*pi/nlon)
!
!     5. the maximum (plus one) longitudinal wave number
!
!            mmax = min0(nlat,(nlon+2)/2) if nlon is even or
!            mmax = min0(nlat,(nlon+1)/2) if nlon is odd.
!
!
!     then for m=0,...,mmax-1 and n=m,...,nlat-1 the arrays a,b
!     are given by
!
!     a(m+1,n+1)      = the sum from i=1 to i=nlat of
!                       c(m+1,i)*zbar(m,n,theta(i))
!                       (first and last terms in this sum are
!                       divided by 2)
!
!     b(m+1,n+1)      = the sum from i=1 to i=nlat of
!                       s(m+1,i)*zbar(m,n,theta(i))
!
!
!     ierror = 0  no errors
!            = 1  error in the specification of nlat
!            = 2  error in the specification of nlon
!            = 3  error in the specification of isym
!            = 4  error in the specification of nt
!            = 5  error in the specification of idg
!            = 6  error in the specification of jdg
!            = 7  error in the specification of mdab
!            = 8  error in the specification of ndab
!            = 9  error in the specification of lshaec
!            = 10 error in the specification of lwork
!
!
! ****************************************************************
!     subroutine shaeci(nlat,nlon,wshaec,lshaec,dwork,ldwork,ierror)
!
!     subroutine shaeci initializes the array wshaec which can then
!     be used repeatedly by subroutine shaec.
!
!     input parameters
!
!     nlat   the number of colatitudes on the full sphere including the
!            poles. for example, nlat = 37 for a five degree grid.
!            nlat determines the grid increment in colatitude as
!            pi/(nlat-1).  if nlat is odd the equator is located at
!            grid point i=(nlat+1)/2. if nlat is even the equator is
!            located half way between points i=nlat/2 and i=nlat/2+1.
!            nlat must be at least 3. note: on the half sphere, the
!            number of grid points in the colatitudinal direction is
!            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
!
!     nlon   the number of distinct londitude points.  nlon determines
!            the grid increment in longitude as 2*pi/nlon. for example
!            nlon = 72 for a five degree grid. nlon must be greater
!            than or equal to 4. the efficiency of the computation is
!            improved when nlon is a product of small prime numbers.
!
!     lshaec the dimension of the array wshaec as it appears in the
!            program that calls shaeci. the array wshaec is an output
!            parameter which is described below. define
!
!               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
!               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
!
!            and
!
!               l2 = nlat/2        if nlat is even or
!               l2 = (nlat+1)/2    if nlat is odd
!
!            then lshaec must be at least
!
!            2*nlat*l2+3*((l1-2)*(nlat+nlat-l1-1))/2+nlon+15
!
!     dwork  a double precision dwork array that does not have to be saved.
!
!     ldwork the dimension of the array dwork as it appears in the
!            program that calls shaeci.  ldwork  must be at least
!            nlat+1.
!
!
!     output parameters
!
!     wshaec an array which is initialized for use by subroutine shaec.
!            once initialized, wshaec can be used repeatedly by shaec
!            as long as nlon and nlat remain unchanged.  wshaec must
!            not be altered between calls of shaec.
!
!     ierror = 0  no errors
!            = 1  error in the specification of nlat
!            = 2  error in the specification of nlon
!            = 3  error in the specification of lshaec
!            = 4  error in the specification of ldwork
!
!
! *******************************************************************
   subroutine shaec(nlat, nlon, isym, nt, g, idg, jdg, a, b, mdab, ndab,&
   &wshaec, lshaec, work, lwork, ierror)
      integer, intent(in) :: nlat, nlon, idg, jdg, mdab, ndab, lwork,&
      &isym, nt, lshaec
      integer, intent(out) :: ierror
      double precision, intent(inout) :: g(idg, jdg, *), a(mdab, ndab, *),&
      &b(mdab, ndab, *), wshaec(*), work(*)
      integer :: labc, mmax, imid, lzz1, ls, nln, iw1, ist
      ierror = 1
      if (nlat < 3) return
      ierror = 2
      if (nlon < 4) return
      ierror = 3
      if (isym < 0 .or. isym > 2) return
      ierror = 4
      if (nt < 0) return
      ierror = 5
      if ((isym == 0 .and. idg < nlat) .or.&
      &(isym /= 0 .and. idg < (nlat + 1) / 2)) return
      ierror = 6
      if (jdg < nlon) return
      ierror = 7
      mmax = min(nlat, nlon / 2 + 1)
      if (mdab < mmax) return
      ierror = 8
      if (ndab < nlat) return
      ierror = 9
      imid = (nlat + 1) / 2
      lzz1 = 2 * nlat * imid
      labc = 3 * ((mmax - 2) * (nlat + nlat - mmax - 1)) / 2
      if (lshaec < lzz1 + labc + nlon + 15) return
      ierror = 10
      ls = nlat
      if (isym > 0) ls = imid
      nln = nt * ls * nlon
      if (lwork < nln + max(ls * nlon, 3 * nlat * imid)) return
      ierror = 0
      ist = 0
      if (isym == 0) ist = imid
      iw1 = lzz1 + labc + 1
      call shaec1(nlat, isym, nt, g, idg, jdg, a, b, mdab, ndab, imid, ls, nlon,&
      &work, work(ist + 1), work(nln + 1), work(nln + 1), wshaec, wshaec(iw1))
      return
   end
   subroutine shaec1(nlat, isym, nt, g, idgs, jdgs, a, b, mdab, ndab, imid,&
   &idg, jdg, ge, go, work, zb, wzfin, whrfft)
!
!     whrfft must have at least nlon+15 locations
!     wzfin must have 2*l*(nlat+1)/2 + ((l-3)*l+2)/2 locations
!     zb must have 3*l*(nlat+1)/2 locations
!     work must have ls*nlon locations
!
      integer, intent(in) :: idg, jdgs, nlat, isym, mdab, ndab, imid,&
      &idgs, jdg, nt
      double precision, intent(inout) :: g(idgs, jdgs, 1), a(mdab, ndab, 1),&
      &b(mdab, ndab, 1),&
      &ge(idg, jdg, 1), go(idg, jdg, 1), zb(imid, nlat, 3), wzfin(1),&
      &whrfft(1), work(1)

      integer :: ls, nlon, mmax, mdo, nlp1, modl, imm1, i, k, j, mp1,&
      &np1, ndo, m, mp2, i3
      double precision :: tsn, fsn

      ls = idg
      nlon = jdg
      mmax = min(nlat, nlon / 2 + 1)
      mdo = mmax
      if (mdo + mdo - 1 > nlon) mdo = mmax - 1
      nlp1 = nlat + 1
      tsn = 2./nlon
      fsn = 4./nlon
      modl = mod(nlat, 2)
      imm1 = imid
      if (modl /= 0) imm1 = imid - 1
      if (isym /= 0) go to 15
      do k = 1, nt
         do i = 1, imm1
            do j = 1, nlon
               ge(i, j, k) = tsn * (g(i, j, k) + g(nlp1 - i, j, k))
               go(i, j, k) = tsn * (g(i, j, k) - g(nlp1 - i, j, k))
            end do
         end do
      end do
      go to 30
15    do K = 1, nt
         do i = 1, imm1
            do j = 1, nlon
               ge(i, j, k) = fsn * g(i, j, k)
            end do
         end do
      end do
      if (isym == 1) go to 27
30    if (modl == 0) go to 27
      do k = 1, nt
         do j = 1, nlon
            ge(imid, j, k) = tsn * g(imid, j, k)
         end do
      end do
27    do k = 1, nt
         call hrfftf(ls, nlon, ge(1, 1, k), ls, whrfft, work)
         if (mod(nlon, 2) /= 0) cycle
         do i = 1, ls
            ge(i, nlon, k) = .5 * ge(i, nlon, k)
         end do
      end do
      do k = 1, nt
         do mp1 = 1, mmax
            do np1 = mp1, nlat
               a(mp1, np1, k) = 0.
               b(mp1, np1, k) = 0.
            end do
         end do
      end do
      if (isym == 1) go to 145
      call zfin(2, nlat, nlon, 0, zb, i3, wzfin)
      do k = 1, nt
         do i = 1, imid
            do np1 = 1, nlat, 2
               a(1, np1, k) = a(1, np1, k) + zb(i, np1, i3) * ge(i, 1, k)
            end do
         end do
      end do
      ndo = nlat
      if (mod(nlat, 2) == 0) ndo = nlat - 1
      do mp1 = 2, mdo
         m = mp1 - 1
         call zfin(2, nlat, nlon, m, zb, i3, wzfin)
         do k = 1, nt
            do i = 1, imid
               do np1 = mp1, ndo, 2
                  a(mp1, np1, k) = a(mp1, np1, k) + zb(i, np1, i3) * ge(i, 2 * mp1 - 2, k)
                  b(mp1, np1, k) = b(mp1, np1, k) + zb(i, np1, i3) * ge(i, 2 * mp1 - 1, k)
               end do
            end do
         end do
      end do
      if (mdo == mmax .or. mmax > ndo) go to 135
      call zfin(2, nlat, nlon, mdo, zb, i3, wzfin)
      do k = 1, nt
         do i = 1, imid
            do np1 = mmax, ndo, 2
               a(mmax, np1, k) = a(mmax, np1, k) + zb(i, np1, i3) * ge(i, 2 * mmax - 2, k)
            end do
         end do
      end do
135   if (isym == 2) return
145   call zfin(1, nlat, nlon, 0, zb, i3, wzfin)
      do k = 1, nt
         do i = 1, imm1
            do np1 = 2, nlat, 2
               a(1, np1, k) = a(1, np1, k) + zb(i, np1, i3) * go(i, 1, k)
            end do
         end do
      end do
      ndo = nlat
      if (mod(nlat, 2) /= 0) ndo = nlat - 1
      do mp1 = 2, mdo
         m = mp1 - 1
         mp2 = mp1 + 1
         call zfin(1, nlat, nlon, m, zb, i3, wzfin)
         do k = 1, nt
            do i = 1, imm1
               do np1 = mp2, ndo, 2
                  a(mp1, np1, k) = a(mp1, np1, k) + zb(i, np1, i3) * go(i, 2 * mp1 - 2, k)
                  b(mp1, np1, k) = b(mp1, np1, k) + zb(i, np1, i3) * go(i, 2 * mp1 - 1, k)
               end do
            end do
         end do
      end do
      mp2 = mmax + 1
      if (mdo == mmax .or. mp2 > ndo) return
      call zfin(1, nlat, nlon, mdo, zb, i3, wzfin)
      do k = 1, nt
         do i = 1, imm1
            do np1 = mp2, ndo, 2
               a(mmax, np1, k) = a(mmax, np1, k) + zb(i, np1, i3) * go(i, 2 * mmax - 2, k)
            end do
         end do
      end do
      return
   end subroutine shaec1

   subroutine shaeci(nlat, nlon, wshaec, lshaec, dwork, ldwork, ierror)
      integer, intent(in) :: nlat, nlon, lshaec, ldwork
      double precision, intent(inout) :: wshaec(lshaec)
      double precision, intent(inout) :: dwork(ldwork)
      integer, intent(out) :: ierror

      integer :: imid, mmax, lzz1, labc, iw1

      ierror = 1
      if (nlat < 3) return
      ierror = 2
      if (nlon < 4) return
      ierror = 3
      imid = (nlat + 1) / 2
      mmax = min(nlat, nlon / 2 + 1)
      lzz1 = 2 * nlat * imid
      labc = 3 * ((mmax - 2) * (nlat + nlat - mmax - 1)) / 2
      if (lshaec < lzz1 + labc + nlon + 15) return
      ierror = 4
      if (ldwork < nlat + 1) return
      ierror = 0
      call zfinit(nlat, nlon, wshaec, dwork)
      iw1 = lzz1 + labc + 1
      call hrffti(nlon, wshaec(iw1))
      return
   end subroutine shaeci
end module m_shaec
