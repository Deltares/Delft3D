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
module math_tools
   ! MODULE CONTAINS:
   !     1) Singleton fft transform
   !     2) Hilbert transform
   !     3) Matrix flip up to down and/or left to right
   !     4) xerf
   !     5) Random function

   !
   ! 1) Singleton fft transform:
   !
   !-----------------------------------------------------------------------------
   ! Multivariate Fast Fourier Transform
   !
   ! Fortran 90 Implementation of Singleton's mixed-radix algorithm,
   ! RC Singleton, Stanford Research Institute, Sept. 1968.
   !
   ! Adapted from fftn.c, translated from Fortran 66 to C by Mark Olesen and
   ! John Beale.
   !
   ! Fourier transforms can be computed either in place, using assumed size
   ! arguments, or by generic function, using assumed shape arguments.
   !
   !
   ! Public:
   !
   !   fftkind                              kind parameter of complex arguments
   !                                        and function results.
   !
   !   fft(array, dim, inv, stat)           generic transform function
   !    COMPLEX(fftkind), DIMENSION(:,...,:), INTENT(IN)           :: array
   !    INTEGER,          DIMENSION(:),       INTENT(IN),  OPTIONAL:: dim
   !    LOGICAL,                              INTENT(IN),  OPTIONAL:: inv
   !    INTEGER,                              INTENT(OUT), OPTIONAL:: stat
   !
   !   fftn(array, shape, dim, inv, stat)   in place transform subroutine
   !    COMPLEX(fftkind), DIMENSION(*), INTENT(INOUT)        :: array
   !    INTEGER,          DIMENSION(:), INTENT(IN)           :: shape
   !    INTEGER,          DIMENSION(:), INTENT(IN),  OPTIONAL:: dim
   !    LOGICAL,                        INTENT(IN),  OPTIONAL:: inv
   !    INTEGER,                        INTENT(OUT), OPTIONAL:: stat
   !
   !
   ! Formal Parameters:
   !
   !   array    The complex array to be transformed. array can be of arbitrary
   !            rank (i.e. up to seven).
   !
   !   shape    With subroutine fftn, the shape of the array to be transformed
   !            has to be passed separately, since fftradix - the internal trans-
   !            formation routine - will treat array always as one dimensional.
   !            The product of elements in shape must be the number of
   !            elements in array.
   !            Although passing array with assumed shape would have been nicer,
   !            I prefered assumed size in order to prevent the compiler from
   !            using a copy-in-copy-out mechanism. That would generally be
   !            necessary with fftn passing array to fftradix and with fftn
   !            being prepared for accepting non consecutive array sections.
   !            Using assumed size, it's up to the user to pass an array argu-
   !            ment, that can be addressed as continous one dimensional array
   !            without copying. Otherwise, transformation will not really be
   !            performed in place.
   !            On the other hand, since the rank of array and the size of
   !            shape needn't match, fftn is appropriate for handling more than
   !            seven dimensions.
   !            As far as function fft is concerned all this doesn't matter,
   !            because the argument will be copied anyway. Thus no extra
   !            shape argument is needed for fft.
   !
   ! Optional Parameters:
   !
   !   dim      One dimensional integer array, containing the dimensions to be
   !            transformed. Default is (/1,...,N/) with N being the rank of
   !            array, i.e. complete transform. dim can restrict transformation
   !            to a subset of available dimensions. Its size must not exceed the
   !            rank of array or the size of shape respectivly.
   !
   !   inv      If .true., inverse transformation will be performed. Default is
   !            .false., i.e. forward transformation.
   !
   !   stat     If present, a system dependent nonzero status value will be
   !            returned in stat, if allocation of temporary storage failed.
   !
   !
   ! Scaling:
   !
   !   Transformation results will always be scaled by the square root of the
   !   product of sizes of each dimension in dim. (See examples below)
   !
   !
   ! Examples:
   !
   !   Let A be a L*M*N three dimensional complex array. Then
   !
   !     result = fft(A)
   !
   !   will produce a three dimensional transform, scaled by sqrt(L*M*N), while
   !
   !     call fftn(A, SHAPE(A))
   !
   !   will do the same in place.
   !
   !     result = fft(A, dim=(/1,3/))
   !
   !   will transform with respect to the first and the third dimension, scaled
   !   by sqrt(L*N).
   !
   !     result = fft(fft(A), inv=.true.)
   !
   !   should (approximately) reproduce A.
   !   With B having the same shape as A
   !
   !     result = fft(fft(A) * CONJG(fft(B)), inv=.true.)
   !
   !   will correlate A and B.
   !
   !
   ! Remarks:
   !
   !   Following changes have been introduced with respect to fftn.c:
   !   - complex arguments and results are of type complex, rather than
   !     real an imaginary part separately.
   !   - increment parameter (magnitude of isign) has been dropped,
   !     inc is always one, direction of transform is given by inv.
   !   - maxf and maxp have been dropped. The amount of temporary storage
   !     needed is determined by the fftradix routine. Both fftn and fft
   !     can handle any size of array. (Maybe they take a lot of time and
   !     memory, but they will do it)
   !
   !   Redesigning fftradix in a way, that it handles assumed shape arrays
   !   would have been desirable. However, I found it rather hard to do this
   !   in an efficient way. Problems were:
   !   - to prevent stride multiplications when indexing arrays. At least our
   !     compiler was not clever enough to discover that in fact additions
   !     would do the job as well. On the other hand, I haven't been clever
   !     enough to find an implementation using array operations.
   !   - fftradix is rather large and different versions would be necessaray
   !     for each possible rank of array.
   !   Consequently, in place transformation still needs the argument stored
   !   in a consecutive bunch of memory and can't be performed on array
   !   sections like A(100:199:-3, 50:1020). Calling fftn with such sections
   !   will most probably imply copy-in-copy-out. However, the function fft
   !   works with everything it gets and should be convenient to use.
   !
   ! Michael Steffens, 09.12.96, <Michael.Steffens@mbox.muk.uni-hannover.de>
   !-----------------------------------------------------------------------------
   implicit none

   private
   public :: fft, fftn, fftkind, realkind, flipa, flipv, hilbert, flipiv, xerf, random

   integer, parameter :: fftkind = kind(0.0d0) !selected_real_kind(14,307) ! !--- adjust here for other precisions
   integer, parameter :: realkind = kind(0.0d0)
   real(fftkind), parameter :: sin60 = 0.86602540378443865_fftkind
   real(fftkind), parameter :: cos72 = 0.30901699437494742_fftkind
   real(fftkind), parameter :: sin72 = 0.95105651629515357_fftkind
   real(fftkind), parameter :: pi = 3.14159265358979323_fftkind

   interface fft
      module procedure fft1d
      module procedure fft2d
      module procedure fft3d
      module procedure fft4d
      module procedure fft5d
      module procedure fft6d
      module procedure fft7d
   end interface fft

contains

   function fft1d(array, dim, inv, stat) result(ft)
      ! wwvv not used: dim
      !  this whole function is not used, but I put dim in the fftn call
      !--- formal parameters
      complex(fftkind), dimension(:), intent(IN) :: array
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- function result
      complex(fftkind), dimension(size(array, 1)) :: ft
      !--- intrinsics used
      intrinsic SIZE, SHAPE
      ft = array
      call fftn(ft, shape(array), dim=dim, inv=inv, stat=stat)

   end function fft1d

   function fft2d(array, dim, inv, stat) result(ft)
      !--- formal parameters
      complex(fftkind), dimension(:, :), intent(IN) :: array
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- function result
      complex(fftkind), dimension(size(array, 1), size(array, 2)) :: ft
      !--- intrinsics used
      intrinsic SIZE, SHAPE

      ft = array
      call fftn(ft, shape(array), dim, inv, stat)

   end function fft2d

   function fft3d(array, dim, inv, stat) result(ft)
      !--- formal parameters
      complex(fftkind), dimension(:, :, :), intent(IN) :: array
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- function result
      complex(fftkind), &
         dimension(size(array, 1), size(array, 2), size(array, 3)) :: ft
      !--- intrinsics used
      intrinsic SIZE, SHAPE

      ft = array
      call fftn(ft, shape(array), dim, inv, stat)

   end function fft3d

   function fft4d(array, dim, inv, stat) result(ft)
      !--- formal parameters
      complex(fftkind), dimension(:, :, :, :), intent(IN) :: array
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- function result
      complex(fftkind), dimension( &
         size(array, 1), size(array, 2), size(array, 3), size(array, 4)) :: ft
      !--- intrinsics used
      intrinsic SIZE, SHAPE

      ft = array
      call fftn(ft, shape(array), dim, inv, stat)

   end function fft4d

   function fft5d(array, dim, inv, stat) result(ft)
      !--- formal parameters
      complex(fftkind), dimension(:, :, :, :, :), intent(IN) :: array
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- function result
      complex(fftkind), dimension( &
         size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
         size(array, 5)) :: ft
      !--- intrinsics used
      intrinsic SIZE, SHAPE

      ft = array
      call fftn(ft, shape(array), dim, inv, stat)

   end function fft5d

   function fft6d(array, dim, inv, stat) result(ft)
      !--- formal parameters
      complex(fftkind), dimension(:, :, :, :, :, :), intent(IN) :: array
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- function result
      complex(fftkind), dimension( &
         size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
         size(array, 5), size(array, 6)) :: ft
      !--- intrinsics used
      intrinsic SIZE, SHAPE

      ft = array
      call fftn(ft, shape(array), dim, inv, stat)

   end function fft6d

   function fft7d(array, dim, inv, stat) result(ft)
      !--- formal parameters
      complex(fftkind), dimension(:, :, :, :, :, :, :), intent(IN) :: array
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- function result
      complex(fftkind), dimension( &
         size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
         size(array, 5), size(array, 6), size(array, 7)) :: ft
      !--- intrinsics used
      intrinsic SIZE, SHAPE

      ft = array
      call fftn(ft, shape(array), dim, inv, stat)

   end function fft7d

   subroutine fftn(array, shape, dim, inv, stat)
      !--- formal parameters
      complex(fftkind), dimension(*), intent(INOUT) :: array
      integer, dimension(:), intent(IN) :: shape
      integer, dimension(:), intent(IN), optional :: dim
      logical, intent(IN), optional :: inv
      integer, intent(OUT), optional :: stat
      !--- local arrays
      integer, dimension(size(shape)) :: d
      !--- local scalars
      logical :: inverse
      integer :: i, ndim, ntotal
      real(fftkind) :: scale
      !--- intrinsics used
      intrinsic PRESENT, MIN, PRODUCT, SIZE, SQRT

      i = 0 ! wwvv to quiet the compiler
      !--- optional parameter settings
      if (present(inv)) then
         inverse = inv
      else
         inverse = .false.
      end if
      if (present(dim)) then
         ndim = min(size(dim), size(d))
         d(1:ndim) = dim(1:ndim)
      else
         ndim = size(d)
         d = (/(i, i=1, size(d))/)
      end if

      ntotal = product(shape)
      scale = sqrt(1.0_fftkind / product(shape(d(1:ndim))))
      do concurrent (i = 1: ntotal)
         array(i) = array(i) * scale
      end do
      do i = 1, ndim
         call fftradix(array, ntotal, shape(d(i)), product(shape(1:d(i))), &
                       inverse, stat)
         if (present(stat)) then
            if (stat /= 0) return
         end if
      end do

   end subroutine fftn

   subroutine fftradix(array, ntotal, npass, nspan, inv, stat)
      !--- formal parameters
      integer, intent(IN) :: ntotal, npass, nspan
      complex(fftkind), dimension(*), intent(INOUT) :: array
      logical, intent(IN) :: inv
      integer, intent(OUT), optional :: stat
      !--- local arrays
      integer, dimension(bit_size(0)) :: factor
      complex(fftkind), dimension(:), allocatable :: ctmp
      real(fftkind), dimension(:), allocatable :: sine, cosine
      integer, dimension(:), allocatable :: perm
      !--- local scalars
      integer :: ii, kspan, ispan
      integer :: j, jc, jf, jj, k, k1, k2, k3, k4, kk, kt, nn, ns, nt
      integer :: maxfactor, nfactor, nperm
      real(fftkind) :: s60, c72, s72, pi2
      real(fftkind) :: radf
      real(fftkind) :: c1, c2, c3, cd, ak
      real(fftkind) :: s1, s2, s3, sd
      complex(fftkind) :: cc, cj, ck, cjp, cjm, ckp, ckm
      !--- intrinsics used
      intrinsic MAXVAL, MOD, PRESENT, ISHFT, BIT_SIZE, SIN, COS, &
         CMPLX, real, AIMAG

      if (npass <= 1) return

      c72 = cos72
      if (inv) then
         s72 = sin72
         s60 = sin60
         pi2 = pi
      else
         s72 = -sin72
         s60 = -sin60
         pi2 = -pi
      end if

      nt = ntotal
      ns = nspan
      kspan = ns

      nn = nt - 1
      jc = ns / npass
      radf = pi2 * jc
      pi2 = pi2 * 2.0_fftkind !-- use 2 PI from here on

      call factorize()

      maxfactor = maxval(factor(:nfactor))
      if (nfactor - ishft(kt, 1) > 0) then
         nperm = max(nfactor + 1, product(factor(kt + 1:nfactor - kt)) - 1)
      else
         nperm = nfactor + 1
      end if

      if (present(stat)) then
         allocate (ctmp(maxfactor), sine(maxfactor), cosine(maxfactor), STAT=stat)
         if (stat /= 0) return
         call transform()
         deallocate (sine, cosine, STAT=stat)
         if (stat /= 0) return
         allocate (perm(nperm), STAT=stat)
         if (stat /= 0) return
         call permute()
         deallocate (perm, ctmp, STAT=stat)
         if (stat /= 0) return
      else
         allocate (ctmp(maxfactor), sine(maxfactor), cosine(maxfactor))
         call transform()
         deallocate (sine, cosine)
         allocate (perm(nperm))
         call permute()
         deallocate (perm, ctmp)
      end if

   contains

      subroutine factorize
         nfactor = 0
         k = npass
         do while (mod(k, 16) == 0)
            nfactor = nfactor + 1
            factor(nfactor) = 4
            k = k / 16
         end do
         j = 3
         jj = 9
         do
            do while (mod(k, jj) == 0)
               nfactor = nfactor + 1
               factor(nfactor) = j
               k = k / jj
            end do
            j = j + 2
            jj = j * j
            if (jj > k) exit
         end do
         if (k <= 4) then
            kt = nfactor
            factor(nfactor + 1) = k
            if (k /= 1) nfactor = nfactor + 1
         else
            if (k - ishft(k / 4, 2) == 0) then
               nfactor = nfactor + 1
               factor(nfactor) = 2
               k = k / 4
            end if
            kt = nfactor
            j = 2
            do
               if (mod(k, j) == 0) then
                  nfactor = nfactor + 1
                  factor(nfactor) = j
                  k = k / j
               end if
               j = ishft((j + 1) / 2, 1) + 1
               if (j > k) exit
            end do
         end if
         if (kt > 0) then
            j = kt
            do
               nfactor = nfactor + 1
               factor(nfactor) = factor(j)
               j = j - 1
               if (j == 0) exit
            end do
         end if
      end subroutine factorize

      subroutine transform !-- compute fourier transform
         ii = 0
         jf = 0
         do
            sd = radf / kspan
            cd = sin(sd)
            cd = 2.0_fftkind * cd * cd
            sd = sin(sd + sd)
            kk = 1
            ii = ii + 1

            select case (factor(ii))
            case (2)

               !-- transform for factor of 2 (including rotation factor)
               kspan = kspan / 2
               k1 = kspan + 2
               do
                  do
                     k2 = kk + kspan
                     ck = array(k2)
                     array(k2) = array(kk) - ck
                     array(kk) = array(kk) + ck
                     kk = k2 + kspan
                     if (kk > nn) exit
                  end do
                  kk = kk - nn
                  if (kk > jc) exit
               end do
               if (kk > kspan) return
               do
                  c1 = 1.0_fftkind - cd
                  s1 = sd
                  do
                     do
                        do
                           k2 = kk + kspan
                           ck = array(kk) - array(k2)
                           array(kk) = array(kk) + array(k2)
                           array(k2) = ck * cmplx(c1, s1, kind=fftkind)
                           kk = k2 + kspan
                           if (kk >= nt) exit
                        end do
                        k2 = kk - nt
                        c1 = -c1
                        kk = k1 - k2
                        if (kk <= k2) exit
                     end do
                     ak = c1 - (cd * c1 + sd * s1)
                     s1 = sd * c1 - cd * s1 + s1
                     c1 = 2.0_fftkind - (ak * ak + s1 * s1)
                     s1 = s1 * c1
                     c1 = c1 * ak
                     kk = kk + jc
                     if (kk >= k2) exit
                  end do
                  k1 = k1 + 1 + 1
                  kk = (k1 - kspan) / 2 + jc
                  if (kk > jc + jc) exit
               end do

            case (4) !-- transform for factor of 4
               ispan = kspan
               kspan = kspan / 4

               do
                  c1 = 1.0_fftkind
                  s1 = 0.0_fftkind
                  do
                     do
                        k1 = kk + kspan
                        k2 = k1 + kspan
                        k3 = k2 + kspan
                        ckp = array(kk) + array(k2)
                        ckm = array(kk) - array(k2)
                        cjp = array(k1) + array(k3)
                        cjm = array(k1) - array(k3)
                        array(kk) = ckp + cjp
                        cjp = ckp - cjp
                        if (inv) then
                           ckp = ckm + cmplx(-aimag(cjm), real(cjm), kind=fftkind)
                           ckm = ckm + cmplx(aimag(cjm), -real(cjm), kind=fftkind)
                        else
                           ckp = ckm + cmplx(aimag(cjm), -real(cjm), kind=fftkind)
                           ckm = ckm + cmplx(-aimag(cjm), real(cjm), kind=fftkind)
                        end if
                        !-- avoid useless multiplies
                        if (s1 == 0.0_fftkind) then
                           array(k1) = ckp
                           array(k2) = cjp
                           array(k3) = ckm
                        else
                           array(k1) = ckp * cmplx(c1, s1, kind=fftkind)
                           array(k2) = cjp * cmplx(c2, s2, kind=fftkind)
                           array(k3) = ckm * cmplx(c3, s3, kind=fftkind)
                        end if
                        kk = k3 + kspan
                        if (kk > nt) exit
                     end do

                     c2 = c1 - (cd * c1 + sd * s1)
                     s1 = sd * c1 - cd * s1 + s1
                     c1 = 2.0_fftkind - (c2 * c2 + s1 * s1)
                     s1 = s1 * c1
                     c1 = c1 * c2
                     !-- values of c2, c3, s2, s3 that will get used next time
                     c2 = c1 * c1 - s1 * s1
                     s2 = 2.0_fftkind * c1 * s1
                     c3 = c2 * c1 - s2 * s1
                     s3 = c2 * s1 + s2 * c1
                     kk = kk - nt + jc
                     if (kk > kspan) exit
                  end do
                  kk = kk - kspan + 1
                  if (kk > jc) exit
               end do
               if (kspan == jc) return

            case default
               !-- transform for odd factors
               k = factor(ii)
               ispan = kspan
               kspan = kspan / k

               select case (k)
               case (3) !-- transform for factor of 3 (optional code)
                  do
                     do
                        k1 = kk + kspan
                        k2 = k1 + kspan
                        ck = array(kk)
                        cj = array(k1) + array(k2)
                        array(kk) = ck + cj
                        ck = ck - 0.5_fftkind * cj
                        cj = (array(k1) - array(k2)) * s60
                        array(k1) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                        array(k2) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                        kk = k2 + kspan
                        if (kk >= nn) exit
                     end do
                     kk = kk - nn
                     if (kk > kspan) exit
                  end do

               case (5) !-- transform for factor of 5 (optional code)
                  c2 = c72 * c72 - s72 * s72
                  s2 = 2.0_fftkind * c72 * s72
                  do
                     do
                        k1 = kk + kspan
                        k2 = k1 + kspan
                        k3 = k2 + kspan
                        k4 = k3 + kspan
                        ckp = array(k1) + array(k4)
                        ckm = array(k1) - array(k4)
                        cjp = array(k2) + array(k3)
                        cjm = array(k2) - array(k3)
                        cc = array(kk)
                        array(kk) = cc + ckp + cjp
                        ck = ckp * c72 + cjp * c2 + cc
                        cj = ckm * s72 + cjm * s2
                        array(k1) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                        array(k4) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                        ck = ckp * c2 + cjp * c72 + cc
                        cj = ckm * s2 - cjm * s72
                        array(k2) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                        array(k3) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                        kk = k4 + kspan
                        if (kk >= nn) exit
                     end do
                     kk = kk - nn
                     if (kk > kspan) exit
                  end do

               case default

                  if (k /= jf) then
                     jf = k
                     s1 = pi2 / k
                     c1 = cos(s1)
                     s1 = sin(s1)
                     cosine(jf) = 1.0_fftkind
                     sine(jf) = 0.0_fftkind
                     j = 1
                     do
                        cosine(j) = cosine(k) * c1 + sine(k) * s1
                        sine(j) = cosine(k) * s1 - sine(k) * c1
                        k = k - 1
                        cosine(k) = cosine(j)
                        sine(k) = -sine(j)
                        j = j + 1
                        if (j >= k) exit
                     end do
                  end if
                  do
                     do
                        k1 = kk
                        k2 = kk + ispan
                        cc = array(kk)
                        ck = cc
                        j = 1
                        k1 = k1 + kspan
                        do
                           k2 = k2 - kspan
                           j = j + 1
                           ctmp(j) = array(k1) + array(k2)
                           ck = ck + ctmp(j)
                           j = j + 1
                           ctmp(j) = array(k1) - array(k2)
                           k1 = k1 + kspan
                           if (k1 >= k2) exit
                        end do
                        array(kk) = ck
                        k1 = kk
                        k2 = kk + ispan
                        j = 1
                        do
                           k1 = k1 + kspan
                           k2 = k2 - kspan
                           jj = j
                           ck = cc
                           cj = (0.0_fftkind, 0.0_fftkind)
                           k = 1
                           do
                              k = k + 1
                              ck = ck + ctmp(k) * cosine(jj)
                              k = k + 1
                              cj = cj + ctmp(k) * sine(jj)
                              jj = jj + j
                              if (jj > jf) jj = jj - jf
                              if (k >= jf) exit
                           end do
                           k = jf - j
                           array(k1) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                           array(k2) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                           j = j + 1
                           if (j >= k) exit
                        end do
                        kk = kk + ispan
                        if (kk > nn) exit
                     end do
                     kk = kk - nn
                     if (kk > kspan) exit
                  end do

               end select
               !--  multiply by rotation factor (except for factors of 2 and 4)
               if (ii == nfactor) return
               kk = jc + 1
               do
                  c2 = 1.0_fftkind - cd
                  s1 = sd
                  do
                     c1 = c2
                     s2 = s1
                     kk = kk + kspan
                     do
                        do
                           array(kk) = cmplx(c2, s2, kind=fftkind) * array(kk)
                           kk = kk + ispan
                           if (kk > nt) exit
                        end do
                        ak = s1 * s2
                        s2 = s1 * c2 + c1 * s2
                        c2 = c1 * c2 - ak
                        kk = kk - nt + kspan
                        if (kk > ispan) exit
                     end do
                     c2 = c1 - (cd * c1 + sd * s1)
                     s1 = s1 + sd * c1 - cd * s1
                     c1 = 2.0_fftkind - (c2 * c2 + s1 * s1)
                     s1 = s1 * c1
                     c2 = c2 * c1
                     kk = kk - ispan + jc
                     if (kk > kspan) exit
                  end do
                  kk = kk - kspan + jc + 1
                  if (kk > jc + jc) exit
               end do

            end select
         end do
      end subroutine transform

      subroutine permute
         !--  permute the results to normal order---done in two stages
         !--  permutation for square factors of n
         perm(1) = ns
         if (kt > 0) then
            k = kt + kt + 1
            if (nfactor < k) k = k - 1
            j = 1
            perm(k + 1) = jc
            do
               perm(j + 1) = perm(j) / factor(j)
               perm(k) = perm(k + 1) * factor(j)
               j = j + 1
               k = k - 1
               if (j >= k) exit
            end do
            k3 = perm(k + 1)
            kspan = perm(2)
            kk = jc + 1
            k2 = kspan + 1
            j = 1

            if (npass /= ntotal) then
               permute_multi: do
                  do
                     do
                        k = kk + jc
                        do
                           !-- swap array(kk) <> array(k2)
                           ck = array(kk)
                           array(kk) = array(k2)
                           array(k2) = ck
                           kk = kk + 1
                           k2 = k2 + 1
                           if (kk >= k) exit
                        end do
                        kk = kk + ns - jc
                        k2 = k2 + ns - jc
                        if (kk >= nt) exit
                     end do
                     kk = kk - nt + jc
                     k2 = k2 - nt + kspan
                     if (k2 >= ns) exit
                  end do
                  do
                     do
                        k2 = k2 - perm(j)
                        j = j + 1
                        k2 = perm(j + 1) + k2
                        if (k2 <= perm(j)) exit
                     end do
                     j = 1
                     do
                        if (kk < k2) cycle permute_multi
                        kk = kk + jc
                        k2 = k2 + kspan
                        if (k2 >= ns) exit
                     end do
                     if (kk >= ns) exit
                  end do
                  exit
               end do permute_multi
            else
               permute_single: do
                  do
                     !-- swap array(kk) <> array(k2)
                     ck = array(kk)
                     array(kk) = array(k2)
                     array(k2) = ck
                     kk = kk + 1
                     k2 = k2 + kspan
                     if (k2 >= ns) exit
                  end do
                  do
                     do
                        k2 = k2 - perm(j)
                        j = j + 1
                        k2 = perm(j + 1) + k2
                        if (k2 <= perm(j)) exit
                     end do
                     j = 1
                     do
                        if (kk < k2) cycle permute_single
                        kk = kk + 1
                        k2 = k2 + kspan
                        if (k2 >= ns) exit
                     end do
                     if (kk >= ns) exit
                  end do
                  exit
               end do permute_single
            end if
            jc = k3
         end if

         if (ishft(kt, 1) + 1 >= nfactor) return

         ispan = perm(kt + 1)
         !-- permutation for square-free factors of n
         j = nfactor - kt
         factor(j + 1) = 1
         do
            factor(j) = factor(j) * factor(j + 1)
            j = j - 1
            if (j == kt) exit
         end do
         kt = kt + 1
         nn = factor(kt) - 1
         j = 0
         jj = 0
         do
            k = kt + 1
            k2 = factor(kt)
            kk = factor(k)
            j = j + 1
            if (j > nn) exit !-- exit infinite loop
            jj = jj + kk
            do while (jj >= k2)
               jj = jj - k2
               k2 = kk
               k = k + 1
               kk = factor(k)
               jj = jj + kk
            end do
            perm(j) = jj
         end do
         !--  determine the permutation cycles of length greater than 1
         j = 0
         do
            do
               j = j + 1
               kk = perm(j)
               if (kk >= 0) exit
            end do
            if (kk /= j) then
               do
                  k = kk
                  kk = perm(k)
                  perm(k) = -kk
                  if (kk == j) exit
               end do
               k3 = kk
            else
               perm(j) = -j
               if (j == nn) exit !-- exit infinite loop
            end if
         end do
         !--  reorder a and b, following the permutation cycles
         do
            j = k3 + 1
            nt = nt - ispan
            ii = nt - 1 + 1
            if (nt < 0) exit !-- exit infinite loop
            do
               do
                  j = j - 1
                  if (perm(j) >= 0) exit
               end do
               jj = jc
               do
                  kspan = jj
                  if (jj > maxfactor) kspan = maxfactor
                  jj = jj - kspan
                  k = perm(j)
                  kk = jc * k + ii + jj
                  k1 = kk + kspan
                  k2 = 0
                  do
                     k2 = k2 + 1
                     ctmp(k2) = array(k1)
                     k1 = k1 - 1
                     if (k1 == kk) exit
                  end do
                  do
                     k1 = kk + kspan
                     k2 = k1 - jc * (k + perm(k))
                     k = -perm(k)
                     do
                        array(k1) = array(k2)
                        k1 = k1 - 1
                        k2 = k2 - 1
                        if (k1 == kk) exit
                     end do
                     kk = k2
                     if (k == j) exit
                  end do
                  k1 = kk + kspan
                  k2 = 0
                  do
                     k2 = k2 + 1
                     array(k1) = ctmp(k2)
                     k1 = k1 - 1
                     if (k1 == kk) exit
                  end do
                  if (jj == 0) exit
               end do
               if (j == 1) exit
            end do
         end do

      end subroutine permute

   end subroutine fftradix

   !
   !       2) Hilbert transform
   !

   !%HILBERT Hilbert transform.

   subroutine hilbert(xi, m)

      ! use math_tools

      implicit none

      integer :: m, n
      complex(fftkind), dimension(m) :: xi
      complex(fftkind), dimension(:), allocatable :: x
      integer, dimension(:), allocatable :: h
      real :: p
      real :: two, logtwo

      ! Avoid a bug in gfortran...http://stackoverflow.com/questions/10673701/can-i-call-the-fortran-log-function-with-a-number
      two = 2.0
      logtwo = log(two)

      p = log(real(m)) / logtwo
      n = ceiling(p)
      n = 2**n

      allocate (x(n))
      x = 0
      x(1:m) = real(xi)

      x = fft(x, inv=.false.)
      x = x * sqrt(real(n)) ! Scale factor

      allocate (h(n))

      h(1) = 1
      h(2:(n / 2)) = 2
      h((n / 2) + 1) = 1
      h((n / 2) + 2:n) = 0

      x = x * h

      deallocate (h)

      x = fft(x, inv=.true.)
      x = x / sqrt(real(n)) ! Scale factor

      xi = x(1:m)
      deallocate (x)

      return

   end subroutine hilbert

   !
   !     3) Matrix flip up to down and/or left to right
   !
   subroutine flipv(x, M)
      use precision_basics, only: dp

      implicit none

      ! x is input vector
      ! M is size vector

      integer :: M, i
      real(dp), dimension(M) :: x
      real(dp), dimension(:), allocatable :: temp1, temp3
      integer, dimension(:), allocatable :: temp2

      allocate (temp1(M))
      allocate (temp2(M))
      allocate (temp3(M))

      i = 0 ! wwvv to quiet the compiler

      temp1 = 0
      temp2 = 0
      temp3 = 0

      temp1 = x
      temp2 = (/(i, i=0, M - 1)/)
      temp3 = temp1(M - temp2)
      x = temp3

      deallocate (temp1)
      deallocate (temp2)
      deallocate (temp3)

      return

   end subroutine flipv

   subroutine flipiv(x, M)

      implicit none

      ! x is input vector
      ! M is size vector

      integer :: M, i
      complex(fftkind), dimension(M) :: x
      real(realkind), allocatable, dimension(:) :: temp1r, temp3r, temp1i, temp3i
      complex(realkind), parameter :: compi = (0., 1.)
      allocate (temp1r(M))
      allocate (temp3r(M))
      allocate (temp1i(M))
      allocate (temp3i(M))
      temp1r = 0.d0
      temp1i = 0.d0
      temp3r = 0.d0
      temp3i = 0.d0
      temp1r = real(x)
      temp1i = aimag(x)
      do i = 0, M - 1
         temp3r(i + 1) = temp1r(M - i)
         temp3i(i + 1) = temp1i(M - i)
      end do

      x = temp3r + compi * temp3i
      deallocate (temp1r)
      deallocate (temp3r)
      deallocate (temp1i)
      deallocate (temp3i)

      return

   end subroutine flipiv

   subroutine flipa(x, M1, M2, flip)
      use precision_basics, only: dp

      implicit none

      ! x is  2D array
      ! flip (1 or 2) is flip rows(1) or columns(2)

      integer :: M1, M2, flip, ii
      real(dp), dimension(M1, M2) :: x
      real(dp), dimension(:, :), allocatable :: temp1a, temp3a

      allocate (temp1a(M1, M2))
      allocate (temp3a(M1, M2))

      temp1a = 0
      temp3a = 0

      if (flip == 1) then ! flip rows
         temp1a = x
         do ii = 1, M1
            temp3a(:, ii) = temp1a(:, M1 + 1 - ii)
         end do
         x = temp3a
      else if (flip == 2) then ! flip columns
         temp1a = x
         do ii = 1, M2
            temp3a(:, ii) = temp1a(:, M2 + 1 - ii)
         end do
         x = temp3a
      end if

      deallocate (temp1a)
      deallocate (temp3a)

      return

   end subroutine flipa

   function xerf(x) result(y)

      implicit none

      integer :: i
      double precision, dimension(:) :: x
      double precision, dimension(size(x)) :: w, y
      integer :: k
      double precision :: t
      double precision, dimension(0:64) :: a, b

      ! based on derf.f from http://www.kurims.kyoto-u.ac.jp/~ooura/

      data(a(i), i=0, 12) / &
         0.00000000005958930743d0, -0.00000000113739022964d0, &
         0.00000001466005199839d0, -0.00000016350354461960d0, &
         0.00000164610044809620d0, -0.00001492559551950604d0, &
         0.00012055331122299265d0, -0.00085483269811296660d0, &
         0.00522397762482322257d0, -0.02686617064507733420d0, &
         0.11283791670954881569d0, -0.37612638903183748117d0, &
         1.12837916709551257377d0 /
      data(a(i), i=13, 25) / &
         0.00000000002372510631d0, -0.00000000045493253732d0, &
         0.00000000590362766598d0, -0.00000006642090827576d0, &
         0.00000067595634268133d0, -0.00000621188515924000d0, &
         0.00005103883009709690d0, -0.00037015410692956173d0, &
         0.00233307631218880978d0, -0.01254988477182192210d0, &
         0.05657061146827041994d0, -0.21379664776456006580d0, &
         0.84270079294971486929d0 /
      data(a(i), i=26, 38) / &
         0.00000000000949905026d0, -0.00000000018310229805d0, &
         0.00000000239463074000d0, -0.00000002721444369609d0, &
         0.00000028045522331686d0, -0.00000261830022482897d0, &
         0.00002195455056768781d0, -0.00016358986921372656d0, &
         0.00107052153564110318d0, -0.00608284718113590151d0, &
         0.02986978465246258244d0, -0.13055593046562267625d0, &
         0.67493323603965504676d0 /
      data(a(i), i=39, 51) / &
         0.00000000000382722073d0, -0.00000000007421598602d0, &
         0.00000000097930574080d0, -0.00000001126008898854d0, &
         0.00000011775134830784d0, -0.00000111992758382650d0, &
         0.00000962023443095201d0, -0.00007404402135070773d0, &
         0.00050689993654144881d0, -0.00307553051439272889d0, &
         0.01668977892553165586d0, -0.08548534594781312114d0, &
         0.56909076642393639985d0 /
      data(a(i), i=52, 64) / &
         0.00000000000155296588d0, -0.00000000003032205868d0, &
         0.00000000040424830707d0, -0.00000000471135111493d0, &
         0.00000005011915876293d0, -0.00000048722516178974d0, &
         0.00000430683284629395d0, -0.00003445026145385764d0, &
         0.00024879276133931664d0, -0.00162940941748079288d0, &
         0.00988786373932350462d0, -0.05962426839442303805d0, &
         0.49766113250947636708d0 /
      data(b(i), i=0, 12)/ &
         -0.00000000029734388465d0, 0.00000000269776334046d0, &
         -0.00000000640788827665d0, -0.00000001667820132100d0, &
         -0.00000021854388148686d0, 0.00000266246030457984d0, &
         0.00001612722157047886d0, -0.00025616361025506629d0, &
         0.00015380842432375365d0, 0.00815533022524927908d0, &
         -0.01402283663896319337d0, -0.19746892495383021487d0, &
         0.71511720328842845913d0 /
      data(b(i), i=13, 25)/ &
         -0.00000000001951073787d0, -0.00000000032302692214d0, &
         0.00000000522461866919d0, 0.00000000342940918551d0, &
         -0.00000035772874310272d0, 0.00000019999935792654d0, &
         0.00002687044575042908d0, -0.00011843240273775776d0, &
         -0.00080991728956032271d0, 0.00661062970502241174d0, &
         0.00909530922354827295d0, -0.20160072778491013140d0, &
         0.51169696718727644908d0 /
      data(b(i), i=26, 38) / &
         0.00000000003147682272d0, -0.00000000048465972408d0, &
         0.00000000063675740242d0, 0.00000003377623323271d0, &
         -0.00000015451139637086d0, -0.00000203340624738438d0, &
         0.00001947204525295057d0, 0.00002854147231653228d0, &
         -0.00101565063152200272d0, 0.00271187003520095655d0, &
         0.02328095035422810727d0, -0.16725021123116877197d0, &
         0.32490054966649436974d0 /
      data(b(i), i=39, 51) / &
         0.00000000002319363370d0, -0.00000000006303206648d0, &
         -0.00000000264888267434d0, 0.00000002050708040581d0, &
         0.00000011371857327578d0, -0.00000211211337219663d0, &
         0.00000368797328322935d0, 0.00009823686253424796d0, &
         -0.00065860243990455368d0, -0.00075285814895230877d0, &
         0.02585434424202960464d0, -0.11637092784486193258d0, &
         0.18267336775296612024d0 /
      data(b(i), i=52, 64)/ &
         -0.00000000000367789363d0, 0.00000000020876046746d0, &
         -0.00000000193319027226d0, -0.00000000435953392472d0, &
         0.00000018006992266137d0, -0.00000078441223763969d0, &
         -0.00000675407647949153d0, 0.00008428418334440096d0, &
         -0.00017604388937031815d0, -0.00239729611435071610d0, &
         0.02064129023876022970d0, -0.06905562880005864105d0, &
         0.09084526782065478489d0 /

      w = abs(x)

      do i = 1, size(x)
         if (w(i) < 2.2d0) then
            t = w(i) * w(i)
            k = int(t)
            t = t - k
            k = k * 13
            y(i) = ((((((((((((a(k) * t + a(k + 1)) * t + &
                              a(k + 2)) * t + a(k + 3)) * t + a(k + 4)) * t + &
                           a(k + 5)) * t + a(k + 6)) * t + a(k + 7)) * t + &
                        a(k + 8)) * t + a(k + 9)) * t + a(k + 10)) * t + &
                     a(k + 11)) * t + a(k + 12)) * w(i)
         else if (w(i) < 6.9d0) then
            k = int(w(i))
            t = w(i) - k
            k = 13 * (k - 2)
            y(i) = (((((((((((b(k) * t + b(k + 1)) * t + &
                             b(k + 2)) * t + b(k + 3)) * t + b(k + 4)) * t + &
                          b(k + 5)) * t + b(k + 6)) * t + b(k + 7)) * t + &
                       b(k + 8)) * t + b(k + 9)) * t + b(k + 10)) * t + &
                    b(k + 11)) * t + b(k + 12)
            y(i) = 1 - y(i)**16
         else
            y(i) = 1
         end if

         if (x(i) < 0) y(i) = -y(i)
      end do

   end function xerf

   real(kind=dp) function random(j)
      use precision, only: dp
      implicit none
!
! From Communications of the ACM, Vol 31 Oct 1988 number 10
! pp 1192..1201
! Stephen K. Park and Keith W. Miller
!
! Input parameter j:
!  j .ne. 0: random sequence is initialised, using abs(j) as seed
!            first random number of new series is returned
!  j .eq. 0: function returns next random number
!
! Not thread-safe because of seed
!

      integer a, m, q, r, lo, hi, test, seed
      integer, intent(in) :: j
      real(dp) :: rm
      parameter(a=16807, m=2147483647, q=127773, r=2836, rm=m)
      save seed
      data seed/1/
      if (j /= 0) then
         seed = mod(abs(j), m)
         ! Seed may not be zero for random number generation
         if (seed == 0) then
            seed = 1
         end if
      end if
      hi = seed / q
      lo = mod(seed, q)
      test = a * lo - r * hi
      if (test > 0) then
         seed = test
      else
         seed = test + m
      end if
      random = seed / rm
      return
   end function random

end module math_tools
