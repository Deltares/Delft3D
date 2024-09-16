!
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     *                                                               *
!     *                  copyright (c) 1998-2024 by UCAR              *
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
! ... file hrfft.f
!
!     this file contains a multiple fft package for spherepack. it
!     includes code and documentation for performing fast fourier
!     transforms (see subroutines hrffti,hrfftf and hrfftb)
!
! **********************************************************************
!
!     subroutine hrffti(n,wsave)
!
!     subroutine hrffti initializes the array wsave which is used in
!     both hrfftf and hrfftb. the prime factorization of n together
!     with a tabulation of the trigonometric functions are computed and
!     stored in wsave.
!
!     input parameter
!
!     n       the length of the sequence to be transformed.
!
!     output parameter
!
!     wsave   a work array which must be dimensioned at least 2*n+15.
!             the same work array can be used for both hrfftf and
!             hrfftb as long as n remains unchanged. different wsave
!             arrays are required for different values of n. the
!             contents of wsave must not be changed between calls
!             of hrfftf or hrfftb.
!
! **********************************************************************
!
!     subroutine hrfftf(m,n,r,mdimr,wsave,work)
!
!     subroutine hrfftf computes the fourier coefficients of m real
!     perodic sequences (fourier analysis); i.e. hrfftf computes the
!     real fft of m sequences each with length n. the transform is
!     defined below at output parameter r.
!
!     input parameters
!
!     m       the number of sequences.
!
!     n       the length of all m sequences.  the method is most
!             efficient when n is a product of small primes. n may
!             change as long as different work arrays are provided
!
!     r       r(m,n) is a two dimensional real array that contains m
!             sequences each with length n.
!
!     mdimr   the first dimension of the r array as it appears
!             in the program that calls hrfftf. mdimr must be
!             greater than or equal to m.
!
!
!     wsave   a work array with at least least 2*n+15 locations
!             in the program that calls hrfftf. the wsave array must be
!             initialized by calling subroutine hrffti(n,wsave) and a
!             different wsave array must be used for each different
!             value of n. this initialization does not have to be
!             repeated so long as n remains unchanged thus subsequent
!             transforms can be obtained faster than the first.
!             the same wsave array can be used by hrfftf and hrfftb.
!
!     work    a real work array with m*n locations.
!
!
!     output parameters
!
!     r      for all j=1,...,m
!
!             r(j,1) = the sum from i=1 to i=n of r(j,i)
!
!             if n is even set l =n/2   , if n is odd set l = (n+1)/2
!
!               then for k = 2,...,l
!
!                  r(j,2*k-2) = the sum from i = 1 to i = n of
!
!                       r(j,i)*cos((k-1)*(i-1)*2*pi/n)
!
!                  r(j,2*k-1) = the sum from i = 1 to i = n of
!
!                      -r(j,i)*sin((k-1)*(i-1)*2*pi/n)
!
!             if n is even
!
!                  r(j,n) = the sum from i = 1 to i = n of
!
!                       (-1)**(i-1)*r(j,i)
!
!      *****  note
!                  this transform is unnormalized since a call of hrfftf
!                  followed by a call of hrfftb will multiply the input
!                  sequence by n.
!
!     wsave   contains results which must not be destroyed between
!             calls of hrfftf or hrfftb.
!
!     work    a real work array with m*n locations that does
!             not have to be saved.
!
! **********************************************************************
!
!     subroutine hrfftb(m,n,r,mdimr,wsave,work)
!
!     subroutine hrfftb computes the real perodic sequence of m
!     sequences from their fourier coefficients (fourier synthesis).
!     the transform is defined below at output parameter r.
!
!     input parameters
!
!     m       the number of sequences.
!
!     n       the length of all m sequences.  the method is most
!             efficient when n is a product of small primes. n may
!             change as long as different work arrays are provided
!
!     r       r(m,n) is a two dimensional real array that contains
!             the fourier coefficients of m sequences each with
!             length n.
!
!     mdimr   the first dimension of the r array as it appears
!             in the program that calls hrfftb. mdimr must be
!             greater than or equal to m.
!
!     wsave   a work array which must be dimensioned at least 2*n+15.
!             in the program that calls hrfftb. the wsave array must be
!             initialized by calling subroutine hrffti(n,wsave) and a
!             different wsave array must be used for each different
!             value of n. this initialization does not have to be
!             repeated so long as n remains unchanged thus subsequent
!             transforms can be obtained faster than the first.
!             the same wsave array can be used by hrfftf and hrfftb.
!
!     work    a real work array with m*n locations.
!
!
!     output parameters
!
!     r      for all j=1,...,m
!
!             for n even and for i = 1,...,n
!
!                  r(j,i) = r(j,1)+(-1)**(i-1)*r(j,n)
!
!                       plus the sum from k=2 to k=n/2 of
!
!                        2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
!
!                       -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
!
!             for n odd and for i = 1,...,n
!
!                  r(j,i) = r(j,1) plus the sum from k=2 to k=(n+1)/2 of
!
!                       2.*r(j,2*k-2)*cos((k-1)*(i-1)*2*pi/n)
!
!                      -2.*r(j,2*k-1)*sin((k-1)*(i-1)*2*pi/n)
!
!      *****  note
!                  this transform is unnormalized since a call of hrfftf
!                  followed by a call of hrfftb will multiply the input
!                  sequence by n.
!
!     wsave   contains results which must not be destroyed between
!             calls of hrfftb or hrfftf.
!
!     work    a real work array with m*n locations that does not
!             have to be saved
!
! **********************************************************************
!
module hrf
   use precision, only: dp

   real(dp) :: tfft

end module hrf
!
module hrfft
   implicit none
contains

   subroutine hrffti(n, wsave)
      use hrf
      integer, intent(in) :: n
      double precision wsave(n + 15)

      tfft = 0.
      if (n == 1) return
      call hrfti1(n, wsave(1), wsave(n + 1))
      return
   end subroutine hrffti

   subroutine hrfti1(n, wa, fac)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: n
      double precision wa(n), fac(15), ntryh(4)
      double precision tpi, argh, argld, arg
      integer :: nl, nf, i, j, ntry, l1, k1, ld, l2, ido, ii, ipm, ip
      integer :: nfm1, is, ib, nr, nq
      double precision :: fi
      data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/4, 2, 3, 5/
      nl = n
      nf = 0
      j = 0
101   j = j + 1
      if (j <= 4) then
         ntry = ntryh(j)
      else
         ntry = ntry + 2
      end if
104   continue
      nq = nl / ntry
      nr = nl - ntry * nq
      if (nr /= 0) goto 101

      nf = nf + 1
      fac(nf + 2) = ntry
      nl = nq
      if (ntry /= 2) go to 107
      if (nf == 1) go to 107
      do i = 2, nf
         ib = nf - i + 2
         fac(ib + 2) = fac(ib + 1)
      end do
      fac(3) = 2
107   if (nl /= 1) go to 104
      fac(1) = n
      fac(2) = nf
      tpi = 8.d0 * atan(1.d0)
      argh = tpi / real(n, kind=kind(argh))
      is = 0
      nfm1 = nf - 1
      l1 = 1
      if (nfm1 == 0) return
      do k1 = 1, nfm1
         ip = fac(k1 + 2)
         ld = 0
         l2 = l1 * ip
         ido = n / l2
         ipm = ip - 1
         do j = 1, ipm
            ld = ld + l1
            i = is
            argld = real(ld, kind=kind(argld)) * argh
            fi = 0.
            do ii = 3, ido, 2
               i = i + 2
               fi = fi + 1.
               arg = fi * argld
               wa(i - 1) = cos(arg)
               wa(i) = sin(arg)
            end do
            is = is + ido
         end do
         l1 = l2
      end do
      return
   end

   subroutine hrfftf(m, n, r, mdimr, whrfft, work)
!
!     a multiple fft package for spherepack
!
      use hrf

      integer, intent(in) :: m, n, mdimr
      double precision r(mdimr, n), work(1), whrfft(n + 15)

      if (n == 1) return
!     tstart = second(dum)
      call hrftf1(m, n, r, mdimr, work, whrfft, whrfft(n + 1))
!     tfft = tfft+second(dum)-tstart
      return
   end subroutine hrfftf

   subroutine hrftf1(m, n, c, mdimc, ch, wa, fac)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: m, n, mdimc
      double precision ch(m, n), c(mdimc, n), wa(n), fac(15)
      integer :: i, j, nf, na, l2, iw, ix2, ix3, ix4, idl1, ido, l1, ip
      integer :: kh, k1
      nf = fac(2)
      na = 1
      l2 = n
      iw = n
      do k1 = 1, nf
         kh = nf - k1
         ip = fac(kh + 3)
         l1 = l2 / ip
         ido = n / l2
         idl1 = ido * l1
         iw = iw - (ip - 1) * ido
         na = 1 - na
         if (ip /= 4) go to 102
         ix2 = iw + ido
         ix3 = ix2 + ido
         if (na /= 0) go to 101
         call hradf4(m, ido, l1, c, mdimc, ch, m, wa(iw), wa(ix2), wa(ix3))
         go to 110
101      call hradf4(m, ido, l1, ch, m, c, mdimc, wa(iw), wa(ix2), wa(ix3))
         go to 110
102      if (ip /= 2) go to 104
         if (na /= 0) go to 103
         call hradf2(m, ido, l1, c, mdimc, ch, m, wa(iw))
         go to 110
103      call hradf2(m, ido, l1, ch, m, c, mdimc, wa(iw))
         go to 110
104      if (ip /= 3) go to 106
         ix2 = iw + ido
         if (na /= 0) go to 105
         call hradf3(m, ido, l1, c, mdimc, ch, m, wa(iw), wa(ix2))
         go to 110
105      call hradf3(m, ido, l1, ch, m, c, mdimc, wa(iw), wa(ix2))
         go to 110
106      if (ip /= 5) go to 108
         ix2 = iw + ido
         ix3 = ix2 + ido
         ix4 = ix3 + ido
         if (na /= 0) go to 107
         call hradf5(m, ido, l1, c, mdimc, ch, m, wa(iw), wa(ix2), wa(ix3), wa(ix4))
         go to 110
107      call hradf5(m, ido, l1, ch, m, c, mdimc, wa(iw), wa(ix2), wa(ix3), wa(ix4))
         go to 110
108      if (ido == 1) na = 1 - na
         if (na /= 0) go to 109
         call hradfg(m, ido, ip, l1, idl1, c, c, c, mdimc, ch, ch, m, wa(iw))
         na = 1
         go to 110
109      call hradfg(m, ido, ip, l1, idl1, ch, ch, ch, m, c, c, mdimc, wa(iw))
         na = 0
110      l2 = l1
      end do
      if (na == 1) return
      do j = 1, n
         do i = 1, m
            c(i, j) = ch(i, j)
         end do
      end do
      return
   end subroutine hrftf1

   subroutine hradf4(mp, ido, l1, cc, mdimcc, ch, mdimch, wa1, wa2, wa3)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, l1, mdimcc, mdimch
      double precision cc(mdimcc, ido, l1, 4), ch(mdimch, ido, 4, l1),&
      &wa1(ido), wa2(ido), wa3(ido)
      double precision :: hsqt2
      integer :: i, k, m, ic, idp2
      hsqt2 = sqrt(2.0d0) / 2.0d0
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, 1, k) = (cc(m, 1, k, 2) + cc(m, 1, k, 4))&
            &+ (cc(m, 1, k, 1) + cc(m, 1, k, 3))
            ch(m, ido, 4, k) = (cc(m, 1, k, 1) + cc(m, 1, k, 3))&
            &- (cc(m, 1, k, 2) + cc(m, 1, k, 4))
            ch(m, ido, 2, k) = cc(m, 1, k, 1) - cc(m, 1, k, 3)
            ch(m, 1, 3, k) = cc(m, 1, k, 4) - cc(m, 1, k, 2)
         end do
      end do
      if (ido > 2) then
         idp2 = ido + 2
         do k = 1, l1
            do i = 3, ido, 2
               ic = idp2 - i
               do m = 1, mp
                  ch(m, i - 1, 1, k) = ((wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
                  &cc(m, i, k, 2)) + (wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) *&
                  &cc(m, i, k, 4))) + (cc(m, i - 1, k, 1) + (wa2(i - 2) * cc(m, i - 1, k, 3) +&
                  &wa2(i - 1) * cc(m, i, k, 3)))
                  ch(m, ic - 1, 4, k) = (cc(m, i - 1, k, 1) + (wa2(i - 2) * cc(m, i - 1, k, 3) +&
                  &wa2(i - 1) * cc(m, i, k, 3))) - ((wa1(i - 2) * cc(m, i - 1, k, 2) +&
                  &wa1(i - 1) * cc(m, i, k, 2)) + (wa3(i - 2) * cc(m, i - 1, k, 4) +&
                  &wa3(i - 1) * cc(m, i, k, 4)))
                  ch(m, i, 1, k) = ((wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) *&
                  &cc(m, i - 1, k, 2)) + (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
                  &cc(m, i - 1, k, 4))) + (cc(m, i, k, 1) + (wa2(i - 2) * cc(m, i, k, 3) -&
                  &wa2(i - 1) * cc(m, i - 1, k, 3)))
                  ch(m, ic, 4, k) = ((wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) *&
                  &cc(m, i - 1, k, 2)) + (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
                  &cc(m, i - 1, k, 4))) - (cc(m, i, k, 1) + (wa2(i - 2) * cc(m, i, k, 3) -&
                  &wa2(i - 1) * cc(m, i - 1, k, 3)))
                  ch(m, i - 1, 3, k) = ((wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) *&
                  &cc(m, i - 1, k, 2)) - (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
                  &cc(m, i - 1, k, 4))) + (cc(m, i - 1, k, 1) - (wa2(i - 2) * cc(m, i - 1, k, 3) +&
                  &wa2(i - 1) * cc(m, i, k, 3)))
                  ch(m, ic - 1, 2, k) = (cc(m, i - 1, k, 1) - (wa2(i - 2) * cc(m, i - 1, k, 3) +&
                  &wa2(i - 1) * cc(m, i, k, 3))) - ((wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) *&
                  &cc(m, i - 1, k, 2)) - (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
                  &cc(m, i - 1, k, 4)))
                  ch(m, i, 3, k) = ((wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) *&
                  &cc(m, i, k, 4)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
                  &cc(m, i, k, 2))) + (cc(m, i, k, 1) - (wa2(i - 2) * cc(m, i, k, 3) -&
                  &wa2(i - 1) * cc(m, i - 1, k, 3)))
                  ch(m, ic, 2, k) = ((wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) *&
                  &cc(m, i, k, 4)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
                  &cc(m, i, k, 2))) - (cc(m, i, k, 1) - (wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
                  &cc(m, i - 1, k, 3)))
               end do
            end do
         end do
         if (mod(ido, 2) == 1) return
      else if (ido == 2) then
         do k = 1, l1
            do m = 1, mp
               ch(m, ido, 1, k) = (hsqt2 * (cc(m, ido, k, 2) - cc(m, ido, k, 4))) +&
               &cc(m, ido, k, 1)
               ch(m, ido, 3, k) = cc(m, ido, k, 1) - (hsqt2 * (cc(m, ido, k, 2) -&
               &cc(m, ido, k, 4)))
               ch(m, 1, 2, k) = (-hsqt2 * (cc(m, ido, k, 2) + cc(m, ido, k, 4))) -&
               &cc(m, ido, k, 3)
               ch(m, 1, 4, k) = (-hsqt2 * (cc(m, ido, k, 2) + cc(m, ido, k, 4))) +&
               &cc(m, ido, k, 3)
            end do
         end do
      end if
   end subroutine hradf4

   subroutine hradf2(mp, ido, l1, cc, mdimcc, ch, mdimch, wa1)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, l1, mdimcc, mdimch
      double precision ch(mdimch, ido, 2, l1), cc(mdimcc, ido, l1, 2),&
      &wa1(ido)
      integer :: i, k, m, ic, idp2
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, 1, k) = cc(m, 1, k, 1) + cc(m, 1, k, 2)
            ch(m, ido, 2, k) = cc(m, 1, k, 1) - cc(m, 1, k, 2)
         end do
      end do
      if (ido > 2) then
         idp2 = ido + 2
         do k = 1, l1
            do i = 3, ido, 2
               ic = idp2 - i
               do m = 1, mp
                  ch(m, i, 1, k) = cc(m, i, k, 1) + (wa1(i - 2) * cc(m, i, k, 2) -&
                  &wa1(i - 1) * cc(m, i - 1, k, 2))
                  ch(m, ic, 2, k) = (wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) *&
                  &cc(m, i - 1, k, 2)) - cc(m, i, k, 1)
                  ch(m, i - 1, 1, k) = cc(m, i - 1, k, 1) + (wa1(i - 2) * cc(m, i - 1, k, 2) +&
                  &wa1(i - 1) * cc(m, i, k, 2))
                  ch(m, ic - 1, 2, k) = cc(m, i - 1, k, 1) - (wa1(i - 2) * cc(m, i - 1, k, 2) +&
                  &wa1(i - 1) * cc(m, i, k, 2))
               end do
            end do
         end do
         if (mod(ido, 2) == 1) return
      else if (ido == 2) then
         do k = 1, l1
            do m = 1, mp
               ch(m, 1, 2, k) = -cc(m, ido, k, 2)
               ch(m, ido, 1, k) = cc(m, ido, k, 1)
            end do
         end do
      end if
   end subroutine hradf2

   subroutine hradf3(mp, ido, l1, cc, mdimcc, ch, mdimch, wa1, wa2)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, mdimcc, mdimch, l1
      double precision ch(mdimch, ido, 3, l1), cc(mdimcc, ido, l1, 3),&
      &wa1(ido), wa2(ido)
      double precision :: arg, taur, taui
      integer :: i, k, m, ic, idp2
      arg = 2.*pimach() / 3.
      taur = cos(arg)
      taui = sin(arg)
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, 1, k) = cc(m, 1, k, 1) + (cc(m, 1, k, 2) + cc(m, 1, k, 3))
            ch(m, 1, 3, k) = taui * (cc(m, 1, k, 3) - cc(m, 1, k, 2))
            ch(m, ido, 2, k) = cc(m, 1, k, 1) + taur *&
            &(cc(m, 1, k, 2) + cc(m, 1, k, 3))
         end do
      end do
      if (ido == 1) return
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            do m = 1, mp
               ch(m, i - 1, 1, k) = cc(m, i - 1, k, 1) + ((wa1(i - 2) * cc(m, i - 1, k, 2) +&
               &wa1(i - 1) * cc(m, i, k, 2)) + (wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3)))
               ch(m, i, 1, k) = cc(m, i, k, 1) + ((wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) *&
               &cc(m, i - 1, k, 2)) + (wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3)))
               ch(m, i - 1, 3, k) = (cc(m, i - 1, k, 1) + taur * ((wa1(i - 2) *&
               &cc(m, i - 1, k, 2) + wa1(i - 1) * cc(m, i, k, 2)) + (wa2(i - 2) *&
               &cc(m, i - 1, k, 3) + wa2(i - 1) * cc(m, i, k, 3)))) + (taui * ((wa1(i - 2) *&
               &cc(m, i, k, 2) - wa1(i - 1) * cc(m, i - 1, k, 2)) - (wa2(i - 2) *&
               &cc(m, i, k, 3) - wa2(i - 1) * cc(m, i - 1, k, 3))))
               ch(m, ic - 1, 2, k) = (cc(m, i - 1, k, 1) + taur * ((wa1(i - 2) *&
               &cc(m, i - 1, k, 2) + wa1(i - 1) * cc(m, i, k, 2)) + (wa2(i - 2) *&
               &cc(m, i - 1, k, 3) + wa2(i - 1) * cc(m, i, k, 3)))) - (taui * ((wa1(i - 2) *&
               &cc(m, i, k, 2) - wa1(i - 1) * cc(m, i - 1, k, 2)) - (wa2(i - 2) *&
               &cc(m, i, k, 3) - wa2(i - 1) * cc(m, i - 1, k, 3))))
               ch(m, i, 3, k) = (cc(m, i, k, 1) + taur * ((wa1(i - 2) * cc(m, i, k, 2) -&
               &wa1(i - 1) * cc(m, i - 1, k, 2)) + (wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3)))) + (taui * ((wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
               &cc(m, i, k, 2))))
               ch(m, ic, 2, k) = (taui * ((wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
               &cc(m, i, k, 2)))) - (cc(m, i, k, 1) + taur * ((wa1(i - 2) * cc(m, i, k, 2) -&
               &wa1(i - 1) * cc(m, i - 1, k, 2)) + (wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3))))
            end do
         end do
      end do
      return
   end subroutine hradf3

   subroutine hradf5(mp, ido, l1, cc, mdimcc, ch, mdimch,&
   &wa1, wa2, wa3, wa4)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, l1, mdimcc, mdimch
      double precision cc(mdimcc, ido, l1, 5), ch(mdimch, ido, 5, l1),&
      &wa1(ido), wa2(ido), wa3(ido), wa4(ido)
      double precision :: arg, tr11, ti11, tr12, ti12
      integer :: i, k, m, ic, idp2
      arg = 2.0d0 * pimach() / 5.0d0
      tr11 = cos(arg)
      ti11 = sin(arg)
      tr12 = cos(2.*arg)
      ti12 = sin(2.*arg)
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, 1, k) = cc(m, 1, k, 1) + (cc(m, 1, k, 5) + cc(m, 1, k, 2)) +&
            &(cc(m, 1, k, 4) + cc(m, 1, k, 3))
            ch(m, ido, 2, k) = cc(m, 1, k, 1) + tr11 * (cc(m, 1, k, 5) + cc(m, 1, k, 2)) +&
            &tr12 * (cc(m, 1, k, 4) + cc(m, 1, k, 3))
            ch(m, 1, 3, k) = ti11 * (cc(m, 1, k, 5) - cc(m, 1, k, 2)) + ti12 *&
            &(cc(m, 1, k, 4) - cc(m, 1, k, 3))
            ch(m, ido, 4, k) = cc(m, 1, k, 1) + tr12 * (cc(m, 1, k, 5) + cc(m, 1, k, 2)) +&
            &tr11 * (cc(m, 1, k, 4) + cc(m, 1, k, 3))
            ch(m, 1, 5, k) = ti12 * (cc(m, 1, k, 5) - cc(m, 1, k, 2)) - ti11 *&
            &(cc(m, 1, k, 4) - cc(m, 1, k, 3))
         end do
      end do
      if (ido == 1) return
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            do m = 1, mp
               ch(m, i - 1, 1, k) = cc(m, i - 1, k, 1) + ((wa1(i - 2) * cc(m, i - 1, k, 2) +&
               &wa1(i - 1) * cc(m, i, k, 2)) + (wa4(i - 2) * cc(m, i - 1, k, 5) + wa4(i - 1) *&
               &cc(m, i, k, 5))) + ((wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3)) + (wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) * cc(m, i, k, 4)))
               ch(m, i, 1, k) = cc(m, i, k, 1) + ((wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) *&
               &cc(m, i - 1, k, 2)) + (wa4(i - 2) * cc(m, i, k, 5) - wa4(i - 1) *&
               &cc(m, i - 1, k, 5))) + ((wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3)) + (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
               &cc(m, i - 1, k, 4)))
               ch(m, i - 1, 3, k) = cc(m, i - 1, k, 1) + tr11 *&
               &(wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) * cc(m, i, k, 2)&
               &+ wa4(i - 2) * cc(m, i - 1, k, 5) + wa4(i - 1) * cc(m, i, k, 5)) + tr12 *&
               &(wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) * cc(m, i, k, 3)&
               &+ wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) * cc(m, i, k, 4)) + ti11 *&
               &(wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) * cc(m, i - 1, k, 2)&
               &- (wa4(i - 2) * cc(m, i, k, 5) - wa4(i - 1) * cc(m, i - 1, k, 5))) + ti12 *&
               &(wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) * cc(m, i - 1, k, 3)&
               &- (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) * cc(m, i - 1, k, 4)))
               ch(m, ic - 1, 2, k) = cc(m, i - 1, k, 1) + tr11 *&
               &(wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) * cc(m, i, k, 2)&
               &+ wa4(i - 2) * cc(m, i - 1, k, 5) + wa4(i - 1) * cc(m, i, k, 5)) + tr12 *&
               &(wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) * cc(m, i, k, 3)&
               &+ wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) * cc(m, i, k, 4)) - (ti11 *&
               &(wa1(i - 2) * cc(m, i, k, 2) - wa1(i - 1) * cc(m, i - 1, k, 2)&
               &- (wa4(i - 2) * cc(m, i, k, 5) - wa4(i - 1) * cc(m, i - 1, k, 5))) + ti12 *&
               &(wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) * cc(m, i - 1, k, 3)&
               &- (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) * cc(m, i - 1, k, 4))))
               ch(m, i, 3, k) = (cc(m, i, k, 1) + tr11 * ((wa1(i - 2) * cc(m, i, k, 2) -&
               &wa1(i - 1) * cc(m, i - 1, k, 2)) + (wa4(i - 2) * cc(m, i, k, 5) - wa4(i - 1) *&
               &cc(m, i - 1, k, 5))) + tr12 * ((wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3)) + (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
               &cc(m, i - 1, k, 4)))) + (ti11 * ((wa4(i - 2) * cc(m, i - 1, k, 5) +&
               &wa4(i - 1) * cc(m, i, k, 5)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
               &cc(m, i, k, 2))) + ti12 * ((wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) *&
               &cc(m, i, k, 4)) - (wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3))))
               ch(m, ic, 2, k) = (ti11 * ((wa4(i - 2) * cc(m, i - 1, k, 5) + wa4(i - 1) *&
               &cc(m, i, k, 5)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
               &cc(m, i, k, 2))) + ti12 * ((wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) *&
               &cc(m, i, k, 4)) - (wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3)))) - (cc(m, i, k, 1) + tr11 * ((wa1(i - 2) * cc(m, i, k, 2) -&
               &wa1(i - 1) * cc(m, i - 1, k, 2)) + (wa4(i - 2) * cc(m, i, k, 5) - wa4(i - 1) *&
               &cc(m, i - 1, k, 5))) + tr12 * ((wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3)) + (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
               &cc(m, i - 1, k, 4))))
               ch(m, i - 1, 5, k) = (cc(m, i - 1, k, 1) + tr12 * ((wa1(i - 2) *&
               &cc(m, i - 1, k, 2) + wa1(i - 1) * cc(m, i, k, 2)) + (wa4(i - 2) *&
               &cc(m, i - 1, k, 5) + wa4(i - 1) * cc(m, i, k, 5))) + tr11 * ((wa2(i - 2) *&
               &cc(m, i - 1, k, 3) + wa2(i - 1) * cc(m, i, k, 3)) + (wa3(i - 2) *&
               &cc(m, i - 1, k, 4) + wa3(i - 1) * cc(m, i, k, 4)))) + (ti12 * ((wa1(i - 2) *&
               &cc(m, i, k, 2) - wa1(i - 1) * cc(m, i - 1, k, 2)) - (wa4(i - 2) * cc(m, i, k, 5) -&
               &wa4(i - 1) * cc(m, i - 1, k, 5))) - ti11 * ((wa2(i - 2) * cc(m, i, k, 3) -&
               &wa2(i - 1) * cc(m, i - 1, k, 3)) - (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
               &cc(m, i - 1, k, 4))))
               ch(m, ic - 1, 4, k) = (cc(m, i - 1, k, 1) + tr12 * ((wa1(i - 2) *&
               &cc(m, i - 1, k, 2) + wa1(i - 1) * cc(m, i, k, 2)) + (wa4(i - 2) *&
               &cc(m, i - 1, k, 5) + wa4(i - 1) * cc(m, i, k, 5))) + tr11 * ((wa2(i - 2) *&
               &cc(m, i - 1, k, 3) + wa2(i - 1) * cc(m, i, k, 3)) + (wa3(i - 2) *&
               &cc(m, i - 1, k, 4) + wa3(i - 1) * cc(m, i, k, 4)))) - (ti12 * ((wa1(i - 2) *&
               &cc(m, i, k, 2) - wa1(i - 1) * cc(m, i - 1, k, 2)) - (wa4(i - 2) * cc(m, i, k, 5) -&
               &wa4(i - 1) * cc(m, i - 1, k, 5))) - ti11 * ((wa2(i - 2) * cc(m, i, k, 3) -&
               &wa2(i - 1) * cc(m, i - 1, k, 3)) - (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
               &cc(m, i - 1, k, 4))))
               ch(m, i, 5, k) = (cc(m, i, k, 1) + tr12 * ((wa1(i - 2) * cc(m, i, k, 2) -&
               &wa1(i - 1) * cc(m, i - 1, k, 2)) + (wa4(i - 2) * cc(m, i, k, 5) - wa4(i - 1) *&
               &cc(m, i - 1, k, 5))) + tr11 * ((wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3)) + (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
               &cc(m, i - 1, k, 4)))) + (ti12 * ((wa4(i - 2) * cc(m, i - 1, k, 5) +&
               &wa4(i - 1) * cc(m, i, k, 5)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
               &cc(m, i, k, 2))) - ti11 * ((wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) *&
               &cc(m, i, k, 4)) - (wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3))))
               ch(m, ic, 4, k) = (ti12 * ((wa4(i - 2) * cc(m, i - 1, k, 5) + wa4(i - 1) *&
               &cc(m, i, k, 5)) - (wa1(i - 2) * cc(m, i - 1, k, 2) + wa1(i - 1) *&
               &cc(m, i, k, 2))) - ti11 * ((wa3(i - 2) * cc(m, i - 1, k, 4) + wa3(i - 1) *&
               &cc(m, i, k, 4)) - (wa2(i - 2) * cc(m, i - 1, k, 3) + wa2(i - 1) *&
               &cc(m, i, k, 3)))) - (cc(m, i, k, 1) + tr12 * ((wa1(i - 2) * cc(m, i, k, 2) -&
               &wa1(i - 1) * cc(m, i - 1, k, 2)) + (wa4(i - 2) * cc(m, i, k, 5) - wa4(i - 1) *&
               &cc(m, i - 1, k, 5))) + tr11 * ((wa2(i - 2) * cc(m, i, k, 3) - wa2(i - 1) *&
               &cc(m, i - 1, k, 3)) + (wa3(i - 2) * cc(m, i, k, 4) - wa3(i - 1) *&
               &cc(m, i - 1, k, 4))))
            end do
         end do
      end do
      return
   end subroutine hradf5

   subroutine hradfg(mp, ido, ip, l1, idl1, cc, c1, c2, mdimcc,&
   &ch, ch2, mdimch, wa)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, ip, l1, mdimcc, mdimch, idl1
      double precision ch(mdimch, ido, l1, ip), cc(mdimcc, ido, ip, l1),&
      &c1(mdimcc, ido, l1, ip), c2(mdimcc, idl1, ip),&
      &ch2(mdimch, idl1, ip), wa(ido)
      double precision :: tpi, arg, dcp, dsp, ar1, ai1, ar1h, ar2h, ai2
      double precision :: dc2, ds2, ar2
      integer :: ipph, ipp2, idp2, nbd, i, j, k, l, m, ik, j2, lc, ic
      integer :: jc, idij, is
      tpi = 2.0d0 * pimach()
      arg = tpi / real(ip, kind=kind(arg))
      dcp = cos(arg)
      dsp = sin(arg)
      ipph = (ip + 1) / 2
      ipp2 = ip + 2
      idp2 = ido + 2
      nbd = (ido - 1) / 2
      if (ido == 1) go to 119
      do ik = 1, idl1
         do m = 1, mp
            ch2(m, ik, 1) = c2(m, ik, 1)
         end do
      end do
      do j = 2, ip
         do k = 1, l1
            do m = 1, mp
               ch(m, 1, k, j) = c1(m, 1, k, j)
            end do
         end do
      end do
      if (nbd > l1) go to 107
      is = -ido
      do j = 2, ip
         is = is + ido
         idij = is
         do i = 3, ido, 2
            idij = idij + 2
            do k = 1, l1
               do m = 1, mp
                  ch(m, i - 1, k, j) = wa(idij - 1) * c1(m, i - 1, k, j) + wa(idij)&
                                      &* c1(m, i, k, j)
                  ch(m, i, k, j) = wa(idij - 1) * c1(m, i, k, j) - wa(idij)&
                                  &* c1(m, i - 1, k, j)
               end do
            end do
         end do
      end do
      go to 111
107   is = -ido
      do j = 2, ip
         is = is + ido
         do k = 1, l1
            idij = is
            do i = 3, ido, 2
               idij = idij + 2
               do m = 1, mp
                  ch(m, i - 1, k, j) = wa(idij - 1) * c1(m, i - 1, k, j) + wa(idij)&
                                      &* c1(m, i, k, j)
                  ch(m, i, k, j) = wa(idij - 1) * c1(m, i, k, j) - wa(idij)&
                                  &* c1(m, i - 1, k, j)
               end do
            end do
         end do
      end do
111   if (nbd < l1) go to 115
      do j = 2, ipph
         jc = ipp2 - j
         do k = 1, l1
            do i = 3, ido, 2
               do m = 1, mp
                  c1(m, i - 1, k, j) = ch(m, i - 1, k, j) + ch(m, i - 1, k, jc)
                  c1(m, i - 1, k, jc) = ch(m, i, k, j) - ch(m, i, k, jc)
                  c1(m, i, k, j) = ch(m, i, k, j) + ch(m, i, k, jc)
                  c1(m, i, k, jc) = ch(m, i - 1, k, jc) - ch(m, i - 1, k, j)
               end do
            end do
         end do
      end do
      go to 121
115   do j = 2, ipph
         jc = ipp2 - j
         do i = 3, ido, 2
            do k = 1, l1
               do m = 1, mp
                  c1(m, i - 1, k, j) = ch(m, i - 1, k, j) + ch(m, i - 1, k, jc)
                  c1(m, i - 1, k, jc) = ch(m, i, k, j) - ch(m, i, k, jc)
                  c1(m, i, k, j) = ch(m, i, k, j) + ch(m, i, k, jc)
                  c1(m, i, k, jc) = ch(m, i - 1, k, jc) - ch(m, i - 1, k, j)
               end do
            end do
         end do
      end do
      go to 121
119   do ik = 1, idl1
         do m = 1, mp
            c2(m, ik, 1) = ch2(m, ik, 1)
         end do
      end do
121   do j = 2, ipph
         jc = ipp2 - j
         do k = 1, l1
            do m = 1, mp
               c1(m, 1, k, j) = ch(m, 1, k, j) + ch(m, 1, k, jc)
               c1(m, 1, k, jc) = ch(m, 1, k, jc) - ch(m, 1, k, j)
            end do
         end do
      end do
!
      ar1 = 1.
      ai1 = 0.
      do l = 2, ipph
         lc = ipp2 - l
         ar1h = dcp * ar1 - dsp * ai1
         ai1 = dcp * ai1 + dsp * ar1
         ar1 = ar1h
         do ik = 1, idl1
            do m = 1, mp
               ch2(m, ik, l) = c2(m, ik, 1) + ar1 * c2(m, ik, 2)
               ch2(m, ik, lc) = ai1 * c2(m, ik, ip)
            end do
         end do
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do j = 3, ipph
            jc = ipp2 - j
            ar2h = dc2 * ar2 - ds2 * ai2
            ai2 = dc2 * ai2 + ds2 * ar2
            ar2 = ar2h
            do ik = 1, idl1
               do m = 1, mp
                  ch2(m, ik, l) = ch2(m, ik, l) + ar2 * c2(m, ik, j)
                  ch2(m, ik, lc) = ch2(m, ik, lc) + ai2 * c2(m, ik, jc)
               end do
            end do
         end do
      end do
      do j = 2, ipph
         do ik = 1, idl1
            do m = 1, mp
               ch2(m, ik, 1) = ch2(m, ik, 1) + c2(m, ik, j)
            end do
         end do
      end do
!
      if (ido < l1) go to 132
      do k = 1, l1
         do i = 1, ido
            do m = 1, mp
               cc(m, i, 1, k) = ch(m, i, k, 1)
            end do
         end do
      end do
      go to 135
132   do i = 1, ido
         do k = 1, l1
            do m = 1, mp
               cc(m, i, 1, k) = ch(m, i, k, 1)
            end do
         end do
      end do
135   do j = 2, ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1, l1
            do m = 1, mp
               cc(m, ido, j2 - 2, k) = ch(m, 1, k, j)
               cc(m, 1, j2 - 1, k) = ch(m, 1, k, jc)
            end do
         end do
      end do
      if (ido == 1) return
      if (nbd < l1) go to 141
      do j = 2, ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1, l1
            do i = 3, ido, 2
               ic = idp2 - i
               do m = 1, mp
                  cc(m, i - 1, j2 - 1, k) = ch(m, i - 1, k, j) + ch(m, i - 1, k, jc)
                  cc(m, ic - 1, j2 - 2, k) = ch(m, i - 1, k, j) - ch(m, i - 1, k, jc)
                  cc(m, i, j2 - 1, k) = ch(m, i, k, j) + ch(m, i, k, jc)
                  cc(m, ic, j2 - 2, k) = ch(m, i, k, jc) - ch(m, i, k, j)
               end do
            end do
         end do
      end do
      return
141   do j = 2, ipph
         jc = ipp2 - j
         j2 = j + j
         do i = 3, ido, 2
            ic = idp2 - i
            do k = 1, l1
               do m = 1, mp
                  cc(m, i - 1, j2 - 1, k) = ch(m, i - 1, k, j) + ch(m, i - 1, k, jc)
                  cc(m, ic - 1, j2 - 2, k) = ch(m, i - 1, k, j) - ch(m, i - 1, k, jc)
                  cc(m, i, j2 - 1, k) = ch(m, i, k, j) + ch(m, i, k, jc)
                  cc(m, ic, j2 - 2, k) = ch(m, i, k, jc) - ch(m, i, k, j)
               end do
            end do
         end do
      end do
      return
   end subroutine hradfg

   double precision function pimach()
      pimach = 3.14159265358979d0
      return
   end function pimach

   subroutine hrfftb(m, n, r, mdimr, whrfft, work)
!
!     a multiple fft package for spherepack
!
      use hrf

      integer, intent(in) :: m, n, mdimr
      double precision r(mdimr, n), work(1), whrfft(n + 15)

      if (n == 1) return
!     tstart = second(dum)
      call hrftb1(m, n, r, mdimr, work, whrfft, whrfft(n + 1))
!     tfft = tfft+second(dum)-tstart
      return
   end subroutine hrfftb

   subroutine hrftb1(m, n, c, mdimc, ch, wa, fac)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: m, n, mdimc
      double precision ch(m, n), c(mdimc, n), wa(n), fac(15)
      integer :: i, j, ix2, ix3, ix4, idl1, ido, l2, k1, nf, na, l1, iw, ip
      nf = fac(2)
      na = 0
      l1 = 1
      iw = 1
      do k1 = 1, nf
         ip = fac(k1 + 2)
         l2 = ip * l1
         ido = n / l2
         idl1 = ido * l1
         if (ip /= 4) go to 103
         ix2 = iw + ido
         ix3 = ix2 + ido
         if (na /= 0) go to 101
         call hradb4(m, ido, l1, c, mdimc, ch, m, wa(iw), wa(ix2), wa(ix3))
         go to 102
101      call hradb4(m, ido, l1, ch, m, c, mdimc, wa(iw), wa(ix2), wa(ix3))
102      na = 1 - na
         go to 115
103      if (ip /= 2) go to 106
         if (na /= 0) go to 104
         call hradb2(m, ido, l1, c, mdimc, ch, m, wa(iw))
         go to 105
104      call hradb2(m, ido, l1, ch, m, c, mdimc, wa(iw))
105      na = 1 - na
         go to 115
106      if (ip /= 3) go to 109
         ix2 = iw + ido
         if (na /= 0) go to 107
         call hradb3(m, ido, l1, c, mdimc, ch, m, wa(iw), wa(ix2))
         go to 108
107      call hradb3(m, ido, l1, ch, m, c, mdimc, wa(iw), wa(ix2))
108      na = 1 - na
         go to 115
109      if (ip /= 5) go to 112
         ix2 = iw + ido
         ix3 = ix2 + ido
         ix4 = ix3 + ido
         if (na /= 0) go to 110
         call hradb5(m, ido, l1, c, mdimc, ch, m, wa(iw), wa(ix2), wa(ix3), wa(ix4))
         go to 111
110      call hradb5(m, ido, l1, ch, m, c, mdimc, wa(iw), wa(ix2), wa(ix3), wa(ix4))
111      na = 1 - na
         go to 115
112      if (na /= 0) go to 113
         call hradbg(m, ido, ip, l1, idl1, c, c, c, mdimc, ch, ch, m, wa(iw))
         go to 114
113      call hradbg(m, ido, ip, l1, idl1, ch, ch, ch, m, c, c, mdimc, wa(iw))
114      if (ido == 1) na = 1 - na
115      l1 = l2
         iw = iw + (ip - 1) * ido
      end do
      if (na == 0) return
      do j = 1, n
         do i = 1, m
            c(i, j) = ch(i, j)
         end do
      end do
      return
   end subroutine hrftb1

   subroutine hradbg(mp, ido, ip, l1, idl1, cc, c1, c2, mdimcc,&
   &ch, ch2, mdimch, wa)
      integer, intent(in) :: mp, ido, ip, l1, mdimcc, mdimch, idl1
!
!     a multiple fft package for spherepack
!
      double precision ch(mdimch, ido, l1, ip), cc(mdimcc, ido, ip, l1),&
      &c1(mdimcc, ido, l1, ip), c2(mdimcc, idl1, ip),&
      &ch2(mdimch, idl1, ip), wa(ido)
      double precision :: tpi, arg, dcp, dsp, ar1h, ai2, ar2h, dc2, ds2
      double precision :: ar1, ai1, ar2
      integer :: i, j, k, l, m, idp2, nbd, ipp2, ipph, jc, j2, ic, idij, ik, lc, is
      tpi = 2.*pimach()
      arg = tpi / real(ip, kind=kind(arg))
      dcp = cos(arg)
      dsp = sin(arg)
      idp2 = ido + 2
      nbd = (ido - 1) / 2
      ipp2 = ip + 2
      ipph = (ip + 1) / 2
      if (ido < l1) go to 103
      do k = 1, l1
         do i = 1, ido
            do m = 1, mp
               ch(m, i, k, 1) = cc(m, i, 1, k)
            end do
         end do
      end do
      go to 106
103   do i = 1, ido
         do k = 1, l1
            do m = 1, mp
               ch(m, i, k, 1) = cc(m, i, 1, k)
            end do
         end do
      end do
106   do j = 2, ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1, l1
            do m = 1, mp
               ch(m, 1, k, j) = cc(m, ido, j2 - 2, k) + cc(m, ido, j2 - 2, k)
               ch(m, 1, k, jc) = cc(m, 1, j2 - 1, k) + cc(m, 1, j2 - 1, k)
            end do
         end do
      end do
      if (ido == 1) go to 116
      if (nbd < l1) go to 112
      do j = 2, ipph
         jc = ipp2 - j
         do k = 1, l1
            do i = 3, ido, 2
               ic = idp2 - i
               do m = 1, mp
                  ch(m, i - 1, k, j) = cc(m, i - 1, 2 * j - 1, k) + cc(m, ic - 1, 2 * j - 2, k)
                  ch(m, i - 1, k, jc) = cc(m, i - 1, 2 * j - 1, k) - cc(m, ic - 1, 2 * j - 2, k)
                  ch(m, i, k, j) = cc(m, i, 2 * j - 1, k) - cc(m, ic, 2 * j - 2, k)
                  ch(m, i, k, jc) = cc(m, i, 2 * j - 1, k) + cc(m, ic, 2 * j - 2, k)
               end do
            end do
         end do
      end do
      go to 116
112   do j = 2, ipph
         jc = ipp2 - j
         do i = 3, ido, 2
            ic = idp2 - i
            do k = 1, l1
               do m = 1, mp
                  ch(m, i - 1, k, j) = cc(m, i - 1, 2 * j - 1, k) + cc(m, ic - 1, 2 * j - 2, k)
                  ch(m, i - 1, k, jc) = cc(m, i - 1, 2 * j - 1, k) - cc(m, ic - 1, 2 * j - 2, k)
                  ch(m, i, k, j) = cc(m, i, 2 * j - 1, k) - cc(m, ic, 2 * j - 2, k)
                  ch(m, i, k, jc) = cc(m, i, 2 * j - 1, k) + cc(m, ic, 2 * j - 2, k)
               end do
            end do
         end do
      end do
116   ar1 = 1.
      ai1 = 0.
      do l = 2, ipph
         lc = ipp2 - l
         ar1h = dcp * ar1 - dsp * ai1
         ai1 = dcp * ai1 + dsp * ar1
         ar1 = ar1h
         do ik = 1, idl1
            do m = 1, mp
               c2(m, ik, l) = ch2(m, ik, 1) + ar1 * ch2(m, ik, 2)
               c2(m, ik, lc) = ai1 * ch2(m, ik, ip)
            end do
         end do
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do j = 3, ipph
            jc = ipp2 - j
            ar2h = dc2 * ar2 - ds2 * ai2
            ai2 = dc2 * ai2 + ds2 * ar2
            ar2 = ar2h
            do ik = 1, idl1
               do m = 1, mp
                  c2(m, ik, l) = c2(m, ik, l) + ar2 * ch2(m, ik, j)
                  c2(m, ik, lc) = c2(m, ik, lc) + ai2 * ch2(m, ik, jc)
               end do
            end do
         end do
      end do
      do j = 2, ipph
         do ik = 1, idl1
            do m = 1, mp
               ch2(m, ik, 1) = ch2(m, ik, 1) + ch2(m, ik, j)
            end do
         end do
      end do
      do j = 2, ipph
         jc = ipp2 - j
         do k = 1, l1
            do m = 1, mp
               ch(m, 1, k, j) = c1(m, 1, k, j) - c1(m, 1, k, jc)
               ch(m, 1, k, jc) = c1(m, 1, k, j) + c1(m, 1, k, jc)
            end do
         end do
      end do
      if (ido == 1) go to 132
      if (nbd < l1) go to 128
      do j = 2, ipph
         jc = ipp2 - j
         do k = 1, l1
            do i = 3, ido, 2
               do m = 1, mp
                  ch(m, i - 1, k, j) = c1(m, i - 1, k, j) - c1(m, i, k, jc)
                  ch(m, i - 1, k, jc) = c1(m, i - 1, k, j) + c1(m, i, k, jc)
                  ch(m, i, k, j) = c1(m, i, k, j) + c1(m, i - 1, k, jc)
                  ch(m, i, k, jc) = c1(m, i, k, j) - c1(m, i - 1, k, jc)
               end do
            end do
         end do
      end do
      go to 132
128   do j = 2, ipph
         jc = ipp2 - j
         do i = 3, ido, 2
            do k = 1, l1
               do m = 1, mp
                  ch(m, i - 1, k, j) = c1(m, i - 1, k, j) - c1(m, i, k, jc)
                  ch(m, i - 1, k, jc) = c1(m, i - 1, k, j) + c1(m, i, k, jc)
                  ch(m, i, k, j) = c1(m, i, k, j) + c1(m, i - 1, k, jc)
                  ch(m, i, k, jc) = c1(m, i, k, j) - c1(m, i - 1, k, jc)
               end do
            end do
         end do
      end do
132   continue
      if (ido == 1) return
      do ik = 1, idl1
         do m = 1, mp
            c2(m, ik, 1) = ch2(m, ik, 1)
         end do
      end do
      do j = 2, ip
         do k = 1, l1
            do m = 1, mp
               c1(m, 1, k, j) = ch(m, 1, k, j)
            end do
         end do
      end do
      if (nbd > l1) go to 139
      is = -ido
      do j = 2, ip
         is = is + ido
         idij = is
         do i = 3, ido, 2
            idij = idij + 2
            do k = 1, l1
               do m = 1, mp
                  c1(m, i - 1, k, j) = wa(idij - 1) * ch(m, i - 1, k, j) - wa(idij) *&
                  &ch(m, i, k, j)
                  c1(m, i, k, j) = wa(idij - 1) * ch(m, i, k, j) + wa(idij) *&
                  &ch(m, i - 1, k, j)
               end do
            end do
         end do
      end do
      go to 143
139   is = -ido
      do j = 2, ip
         is = is + ido
         do k = 1, l1
            idij = is
            do i = 3, ido, 2
               idij = idij + 2
               do m = 1, mp
                  c1(m, i - 1, k, j) = wa(idij - 1) * ch(m, i - 1, k, j) - wa(idij) *&
                  &ch(m, i, k, j)
                  c1(m, i, k, j) = wa(idij - 1) * ch(m, i, k, j) + wa(idij) *&
                  &ch(m, i - 1, k, j)
               end do
            end do
         end do
      end do
143   return
   end subroutine hradbg

   subroutine hradb4(mp, ido, l1, cc, mdimcc, ch, mdimch, wa1, wa2, wa3)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, l1, mdimcc, mdimch
      double precision cc(mdimcc, ido, 4, l1), ch(mdimch, ido, l1, 4),&
      &wa1(ido), wa2(ido), wa3(ido)
      double precision :: sqrt2
      integer :: i, k, m, ic, idp2
      sqrt2 = sqrt(2.0d0)
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, k, 3) = (cc(m, 1, 1, k) + cc(m, ido, 4, k))&
            &- (cc(m, ido, 2, k) + cc(m, ido, 2, k))
            ch(m, 1, k, 1) = (cc(m, 1, 1, k) + cc(m, ido, 4, k))&
            &+ (cc(m, ido, 2, k) + cc(m, ido, 2, k))
            ch(m, 1, k, 4) = (cc(m, 1, 1, k) - cc(m, ido, 4, k))&
            &+ (cc(m, 1, 3, k) + cc(m, 1, 3, k))
            ch(m, 1, k, 2) = (cc(m, 1, 1, k) - cc(m, ido, 4, k))&
            &- (cc(m, 1, 3, k) + cc(m, 1, 3, k))
         end do
      end do
      if (ido - 2) 107, 105, 102
102   idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            do m = 1, mp
               ch(m, i - 1, k, 1) = (cc(m, i - 1, 1, k) + cc(m, ic - 1, 4, k))&
               &+ (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))
               ch(m, i, k, 1) = (cc(m, i, 1, k) - cc(m, ic, 4, k))&
               &+ (cc(m, i, 3, k) - cc(m, ic, 2, k))
               ch(m, i - 1, k, 2) = wa1(i - 2) * ((cc(m, i - 1, 1, k) - cc(m, ic - 1, 4, k))&
               &- (cc(m, i, 3, k) + cc(m, ic, 2, k))) - wa1(i - 1)&
               &* ((cc(m, i, 1, k) + cc(m, ic, 4, k)) + (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k)))
               ch(m, i, k, 2) = wa1(i - 2) * ((cc(m, i, 1, k) + cc(m, ic, 4, k))&
               &+ (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k))) + wa1(i - 1)&
               &* ((cc(m, i - 1, 1, k) - cc(m, ic - 1, 4, k)) - (cc(m, i, 3, k) + cc(m, ic, 2, k)))
               ch(m, i - 1, k, 3) = wa2(i - 2) * ((cc(m, i - 1, 1, k) + cc(m, ic - 1, 4, k))&
               &- (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))) - wa2(i - 1)&
               &* ((cc(m, i, 1, k) - cc(m, ic, 4, k)) - (cc(m, i, 3, k) - cc(m, ic, 2, k)))
               ch(m, i, k, 3) = wa2(i - 2) * ((cc(m, i, 1, k) - cc(m, ic, 4, k))&
               &- (cc(m, i, 3, k) - cc(m, ic, 2, k))) + wa2(i - 1)&
               &* ((cc(m, i - 1, 1, k) + cc(m, ic - 1, 4, k)) - (cc(m, i - 1, 3, k)&
               &+ cc(m, ic - 1, 2, k)))
               ch(m, i - 1, k, 4) = wa3(i - 2) * ((cc(m, i - 1, 1, k) - cc(m, ic - 1, 4, k))&
               &+ (cc(m, i, 3, k) + cc(m, ic, 2, k))) - wa3(i - 1)&
               &* ((cc(m, i, 1, k) + cc(m, ic, 4, k)) - (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k)))
               ch(m, i, k, 4) = wa3(i - 2) * ((cc(m, i, 1, k) + cc(m, ic, 4, k))&
               &- (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k))) + wa3(i - 1)&
               &* ((cc(m, i - 1, 1, k) - cc(m, ic - 1, 4, k)) + (cc(m, i, 3, k) + cc(m, ic, 2, k)))
            end do
         end do
      end do
      if (mod(ido, 2) == 1) return
105   continue
      do k = 1, l1
         do m = 1, mp
            ch(m, ido, k, 1) = (cc(m, ido, 1, k) + cc(m, ido, 3, k))&
            &+ (cc(m, ido, 1, k) + cc(m, ido, 3, k))
            ch(m, ido, k, 2) = sqrt2 * ((cc(m, ido, 1, k) - cc(m, ido, 3, k))&
            &- (cc(m, 1, 2, k) + cc(m, 1, 4, k)))
            ch(m, ido, k, 3) = (cc(m, 1, 4, k) - cc(m, 1, 2, k))&
            &+ (cc(m, 1, 4, k) - cc(m, 1, 2, k))
            ch(m, ido, k, 4) = -sqrt2 * ((cc(m, ido, 1, k) - cc(m, ido, 3, k))&
            &+ (cc(m, 1, 2, k) + cc(m, 1, 4, k)))
         end do
      end do
107   return
   end subroutine hradb4

   subroutine hradb2(mp, ido, l1, cc, mdimcc, ch, mdimch, wa1)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, mdimcc, mdimch, l1
      double precision cc(mdimcc, ido, 2, l1), ch(mdimch, ido, l1, 2),&
      &wa1(ido)
      integer :: i, k, m, ic, idp2
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, k, 1) = cc(m, 1, 1, k) + cc(m, ido, 2, k)
            ch(m, 1, k, 2) = cc(m, 1, 1, k) - cc(m, ido, 2, k)
         end do
      end do
      if (ido - 2) 107, 105, 102
102   idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            do m = 1, mp
               ch(m, i - 1, k, 1) = cc(m, i - 1, 1, k) + cc(m, ic - 1, 2, k)
               ch(m, i, k, 1) = cc(m, i, 1, k) - cc(m, ic, 2, k)
               ch(m, i - 1, k, 2) = wa1(i - 2) * (cc(m, i - 1, 1, k) - cc(m, ic - 1, 2, k))&
               &- wa1(i - 1) * (cc(m, i, 1, k) + cc(m, ic, 2, k))
               ch(m, i, k, 2) = wa1(i - 2) * (cc(m, i, 1, k) + cc(m, ic, 2, k)) + wa1(i - 1)&
                               &* (cc(m, i - 1, 1, k) - cc(m, ic - 1, 2, k))
            end do
         end do
      end do
      if (mod(ido, 2) == 1) return
105   do k = 1, l1
         do m = 1, mp
            ch(m, ido, k, 1) = cc(m, ido, 1, k) + cc(m, ido, 1, k)
            ch(m, ido, k, 2) = -(cc(m, 1, 2, k) + cc(m, 1, 2, k))
         end do
      end do
107   return
   end subroutine hradb2

   subroutine hradb3(mp, ido, l1, cc, mdimcc, ch, mdimch, wa1, wa2)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mp, ido, mdimcc, mdimch, l1
      double precision cc(mdimcc, ido, 3, l1), ch(mdimch, ido, l1, 3),&
      &wa1(ido), wa2(ido)
      double precision :: arg, taur, taui
      integer :: i, k, m, ic, idp2
      arg = 2.0d0 * pimach() / 3.0d0
      taur = cos(arg)
      taui = sin(arg)
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, k, 1) = cc(m, 1, 1, k) + 2.*cc(m, ido, 2, k)
            ch(m, 1, k, 2) = cc(m, 1, 1, k) + (2.*taur) * cc(m, ido, 2, k)&
            &- (2.*taui) * cc(m, 1, 3, k)
            ch(m, 1, k, 3) = cc(m, 1, 1, k) + (2.*taur) * cc(m, ido, 2, k)&
            &+ 2.*taui * cc(m, 1, 3, k)
         end do
      end do
      if (ido == 1) return
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            do m = 1, mp
               ch(m, i - 1, k, 1) = cc(m, i - 1, 1, k) + (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))
               ch(m, i, k, 1) = cc(m, i, 1, k) + (cc(m, i, 3, k) - cc(m, ic, 2, k))
               ch(m, i - 1, k, 2) = wa1(i - 2) *&
               &((cc(m, i - 1, 1, k) + taur * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))) -&
               &(taui * (cc(m, i, 3, k) + cc(m, ic, 2, k))))&
               &- wa1(i - 1) *&
               &((cc(m, i, 1, k) + taur * (cc(m, i, 3, k) - cc(m, ic, 2, k))) +&
               &(taui * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k))))
               ch(m, i, k, 2) = wa1(i - 2) *&
               &((cc(m, i, 1, k) + taur * (cc(m, i, 3, k) - cc(m, ic, 2, k))) +&
               &(taui * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k))))&
               &+ wa1(i - 1) *&
               &((cc(m, i - 1, 1, k) + taur * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))) -&
               &(taui * (cc(m, i, 3, k) + cc(m, ic, 2, k))))
               ch(m, i - 1, k, 3) = wa2(i - 2) *&
               &((cc(m, i - 1, 1, k) + taur * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))) +&
               &(taui * (cc(m, i, 3, k) + cc(m, ic, 2, k))))&
               &- wa2(i - 1) *&
               &((cc(m, i, 1, k) + taur * (cc(m, i, 3, k) - cc(m, ic, 2, k))) -&
               &(taui * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k))))
               ch(m, i, k, 3) = wa2(i - 2) *&
               &((cc(m, i, 1, k) + taur * (cc(m, i, 3, k) - cc(m, ic, 2, k))) -&
               &(taui * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k))))&
               &+ wa2(i - 1) *&
               &((cc(m, i - 1, 1, k) + taur * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))) +&
               &(taui * (cc(m, i, 3, k) + cc(m, ic, 2, k))))
            end do
         end do
      end do
      return
   end subroutine hradb3

   subroutine hradb5(mp, ido, l1, cc, mdimcc, ch, mdimch,&
   &wa1, wa2, wa3, wa4)
!
!     a multiple fft package for spherepack
!
      integer, intent(in) :: mdimch, ido, mp, l1, mdimcc
      double precision cc(mdimcc, ido, 5, l1), ch(mdimch, ido, l1, 5),&
      &wa1(ido), wa2(ido), wa3(ido), wa4(ido)
      integer :: i, k, m, ic, idp2
      double precision :: arg, tr11, ti11, ti12, tr12
      arg = 2.0d0 * pimach() / 5.0d0
      tr11 = cos(arg)
      ti11 = sin(arg)
      tr12 = cos(2.*arg)
      ti12 = sin(2.*arg)
      do k = 1, l1
         do m = 1, mp
            ch(m, 1, k, 1) = cc(m, 1, 1, k) + 2.*cc(m, ido, 2, k) + 2.*cc(m, ido, 4, k)
            ch(m, 1, k, 2) = (cc(m, 1, 1, k) + tr11 * 2.*cc(m, ido, 2, k)&
            &+ tr12 * 2.*cc(m, ido, 4, k)) - (ti11 * 2.*cc(m, 1, 3, k)&
            &+ ti12 * 2.*cc(m, 1, 5, k))
            ch(m, 1, k, 3) = (cc(m, 1, 1, k) + tr12 * 2.*cc(m, ido, 2, k)&
            &+ tr11 * 2.*cc(m, ido, 4, k)) - (ti12 * 2.*cc(m, 1, 3, k)&
            &- ti11 * 2.*cc(m, 1, 5, k))
            ch(m, 1, k, 4) = (cc(m, 1, 1, k) + tr12 * 2.*cc(m, ido, 2, k)&
            &+ tr11 * 2.*cc(m, ido, 4, k)) + (ti12 * 2.*cc(m, 1, 3, k)&
            &- ti11 * 2.*cc(m, 1, 5, k))
            ch(m, 1, k, 5) = (cc(m, 1, 1, k) + tr11 * 2.*cc(m, ido, 2, k)&
            &+ tr12 * 2.*cc(m, ido, 4, k)) + (ti11 * 2.*cc(m, 1, 3, k)&
            &+ ti12 * 2.*cc(m, 1, 5, k))
         end do
      end do
      if (ido == 1) return
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            do m = 1, mp
               ch(m, i - 1, k, 1) = cc(m, i - 1, 1, k) + (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))&
               &+ (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))
               ch(m, i, k, 1) = cc(m, i, 1, k) + (cc(m, i, 3, k) - cc(m, ic, 2, k))&
               &+ (cc(m, i, 5, k) - cc(m, ic, 4, k))
               ch(m, i - 1, k, 2) = wa1(i - 2) * ((cc(m, i - 1, 1, k) + tr11 *&
               &(cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k)) + tr12&
               &* (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))) - (ti11 * (cc(m, i, 3, k)&
               &+ cc(m, ic, 2, k)) + ti12 * (cc(m, i, 5, k) + cc(m, ic, 4, k))))&
               &- wa1(i - 1) * ((cc(m, i, 1, k) + tr11 * (cc(m, i, 3, k) - cc(m, ic, 2, k))&
               &+ tr12 * (cc(m, i, 5, k) - cc(m, ic, 4, k))) + (ti11 * (cc(m, i - 1, 3, k)&
               &- cc(m, ic - 1, 2, k)) + ti12 * (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k))))
               ch(m, i, k, 2) = wa1(i - 2) * ((cc(m, i, 1, k) + tr11 * (cc(m, i, 3, k)&
               &- cc(m, ic, 2, k)) + tr12 * (cc(m, i, 5, k) - cc(m, ic, 4, k)))&
               &+ (ti11 * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k)) + ti12&
               &* (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k)))) + wa1(i - 1)&
               &* ((cc(m, i - 1, 1, k) + tr11 * (cc(m, i - 1, 3, k)&
               &+ cc(m, ic - 1, 2, k)) + tr12 * (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k)))&
               &- (ti11 * (cc(m, i, 3, k) + cc(m, ic, 2, k)) + ti12&
               &* (cc(m, i, 5, k) + cc(m, ic, 4, k))))
               ch(m, i - 1, k, 3) = wa2(i - 2)&
               &* ((cc(m, i - 1, 1, k) + tr12 * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))&
               &+ tr11 * (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))) - (ti12 * (cc(m, i, 3, k)&
               &+ cc(m, ic, 2, k)) - ti11 * (cc(m, i, 5, k) + cc(m, ic, 4, k))))&
               &- wa2(i - 1)&
               &* ((cc(m, i, 1, k) + tr12 * (cc(m, i, 3, k) -&
               &cc(m, ic, 2, k)) + tr11 * (cc(m, i, 5, k) - cc(m, ic, 4, k)))&
               &+ (ti12 * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k)) - ti11&
               &* (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k))))
               ch(m, i, k, 3) = wa2(i - 2)&
               &* ((cc(m, i, 1, k) + tr12 * (cc(m, i, 3, k) -&
               &cc(m, ic, 2, k)) + tr11 * (cc(m, i, 5, k) - cc(m, ic, 4, k)))&
               &+ (ti12 * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k)) - ti11&
               &* (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k))))&
               &+ wa2(i - 1)&
               &* ((cc(m, i - 1, 1, k) + tr12 * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))&
               &+ tr11 * (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))) - (ti12 * (cc(m, i, 3, k)&
               &+ cc(m, ic, 2, k)) - ti11 * (cc(m, i, 5, k) + cc(m, ic, 4, k))))
               ch(m, i - 1, k, 4) = wa3(i - 2)&
               &* ((cc(m, i - 1, 1, k) + tr12 * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))&
               &+ tr11 * (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))) + (ti12 * (cc(m, i, 3, k)&
               &+ cc(m, ic, 2, k)) - ti11 * (cc(m, i, 5, k) + cc(m, ic, 4, k))))&
               &- wa3(i - 1)&
               &* ((cc(m, i, 1, k) + tr12 * (cc(m, i, 3, k) -&
               &cc(m, ic, 2, k)) + tr11 * (cc(m, i, 5, k) - cc(m, ic, 4, k)))&
               &- (ti12 * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k)) - ti11&
               &* (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k))))
               ch(m, i, k, 4) = wa3(i - 2)&
               &* ((cc(m, i, 1, k) + tr12 * (cc(m, i, 3, k) -&
               &cc(m, ic, 2, k)) + tr11 * (cc(m, i, 5, k) - cc(m, ic, 4, k)))&
               &- (ti12 * (cc(m, i - 1, 3, k) - cc(m, ic - 1, 2, k)) - ti11&
               &* (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k))))&
               &+ wa3(i - 1)&
               &* ((cc(m, i - 1, 1, k) + tr12 * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))&
               &+ tr11 * (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))) + (ti12 * (cc(m, i, 3, k)&
               &+ cc(m, ic, 2, k)) - ti11 * (cc(m, i, 5, k) + cc(m, ic, 4, k))))
               ch(m, i - 1, k, 5) = wa4(i - 2)&
               &* ((cc(m, i - 1, 1, k) + tr11 * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))&
               &+ tr12 * (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))) + (ti11 * (cc(m, i, 3, k)&
               &+ cc(m, ic, 2, k)) + ti12 * (cc(m, i, 5, k) + cc(m, ic, 4, k))))&
               &- wa4(i - 1)&
               &* ((cc(m, i, 1, k) + tr11 * (cc(m, i, 3, k) - cc(m, ic, 2, k))&
               &+ tr12 * (cc(m, i, 5, k) - cc(m, ic, 4, k))) - (ti11 * (cc(m, i - 1, 3, k)&
               &- cc(m, ic - 1, 2, k)) + ti12 * (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k))))
               ch(m, i, k, 5) = wa4(i - 2)&
               &* ((cc(m, i, 1, k) + tr11 * (cc(m, i, 3, k) - cc(m, ic, 2, k))&
               &+ tr12 * (cc(m, i, 5, k) - cc(m, ic, 4, k))) - (ti11 * (cc(m, i - 1, 3, k)&
               &- cc(m, ic - 1, 2, k)) + ti12 * (cc(m, i - 1, 5, k) - cc(m, ic - 1, 4, k))))&
               &+ wa4(i - 1)&
               &* ((cc(m, i - 1, 1, k) + tr11 * (cc(m, i - 1, 3, k) + cc(m, ic - 1, 2, k))&
               &+ tr12 * (cc(m, i - 1, 5, k) + cc(m, ic - 1, 4, k))) + (ti11 * (cc(m, i, 3, k)&
               &+ cc(m, ic, 2, k)) + ti12 * (cc(m, i, 5, k) + cc(m, ic, 4, k))))
            end do
         end do
      end do
      return
   end subroutine hradb5
end module hrfft
