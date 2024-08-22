! This file was taken from SPARSKIT VERSION 2. See the original license
! text below for more information.
!
! Welcome  to SPARSKIT  VERSION  2.  SPARSKIT is  a  package of  FORTRAN
! subroutines  for working  with  sparse matrices.  It includes  general
! sparse  matrix  manipulation  routines  as  well as  a  few  iterative
! solvers, see detailed description of contents below.
!
!  Copyright (C) 2005-2024, the Regents of the University of Minnesota
!
! SPARSKIT is  free software; you  can redistribute it and/or  modify it
! under the terms of the  GNU Lesser General Public License as published
! by the  Free Software Foundation [version  2.1 of the  License, or any
! later version.]
!
!
! A copy of  the licencing agreement is attached in  the file LGPL.  For
! additional information  contact the Free Software  Foundation Inc., 59
! Temple Place - Suite 330, Boston, MA 02111, USA or visit the web-site
!
!  http://www.gnu.org/copyleft/lesser.html
!
!
! DISCLAIMER
! ----------
!
! SPARSKIT  is distributed  in  the hope  that  it will  be useful,  but
! WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
! MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
! Lesser General Public License for more details.
!
! For more information contact saad@cs.umn.edu

!
!

#define no_warning_unused_dummy_argument(x) associate( x => x ); end associate

#include "blasfm.h"

module GAMMAS
   double precision :: gammax, gammay, alpha
end module GAMMAS

module m_saad

   use GAMMAS

   integer, allocatable :: &
      iao(:), jao(:), & ! original matrix in CSR format (iao - row_ptr, jao - col_ind)
      ia(:), ja(:), & ! permuted matrix in CSR format
      perm(:), perminv(:), & ! permutation matrix, its reverse
      kolrs(:), & ! color number per node
      jlu(:), ju(:), & ! ILU preconditioner in MSR format
      iw(:), ngs(:)

   integer :: ipar(16), nx, ny, nz, i, lfil, nrow, ierr, il(11), nn, mm, NWK

   integer :: permselect ! permutation method selector
   integer :: JAPERM ! PERM ARRAY INITIALISED YES NO
   integer :: nax ! initial max nrow dim

   logical, parameter :: sym = .true. ! indicates that matrix is symmetric

   double precision, allocatable :: &
      ao(:), & ! original matrix in CSR format
      a(:), & ! permuted matrix in CSR format
      solo(:), rhso(:), & ! original solution and right-hand side
      sol(:), sol0(:), rhs(:), & ! permuted solution and right-hand side
      alu(:), & ! ILU preconditioner in MSR format
      wk(:) ! work array

   double precision :: al(6), epssaad

   character :: qbc * 3

   double precision :: tol, fpar(16), cp0, cp1, cfl
   external gen57pt, cg, bcg, dbcg, bcgstab, tfqmr, gmres, fgmres, dqgmres
   external cgnr, fom, runrc, ilut

   integer :: jasafe = 0 ! thread-safe (1) or not (0)
!
end module m_saad

subroutine inisaad(epscg_loc, maxmatvecs_loc, alpha_loc)
   use m_reduce
   use m_saad
   use m_flowparameters, only: Noderivedtypes

   implicit none

   double precision, intent(in) :: epscg_loc !< threshold in termination criterium
   integer, intent(in) :: maxmatvecs_loc !< maximum number of matrix-vector multiplications
   double precision, intent(in) :: alpha_loc !< ILU (0.0) to MILU (1.0) preconditioning

   integer :: na, n, ntot, j

   if (allocated(ngs)) deallocate (ngs) ! Guus to Saad
   ! allocate(ngs(nogauss0+nocg0) )
   allocate (ngs(nodtot)); ngs = 0

   na = 0 ! matrix counter
   if (Noderivedtypes < 5) then

      do n = nogauss0 + 1, nogauss0 + nocg0
         na = na + 1
         ndn = noel(n) ! guus index
         ntot = row(ndn)%l
         do j = 1, ntot ! init at maxdim
            na = na + 1
         end do
      end do

   else

      do n = nogauss0 + 1, nogauss0 + nocg0
         na = na + 1
         ndn = noel(n) ! guus index
         ntot = L2row(ndn) - L1row(ndn) + 1
         do j = 1, ntot ! init at maxdim
            na = na + 1
         end do
      end do

   end if

   call initsaad(nocg0, na, epscg_loc, maxmatvecs_loc, alpha_loc)
end subroutine inisaad

subroutine initsaad(NOCG0, NA, epscg, maxmatvecs, alpha_loc)

   use M_SAAD
   use m_alloc
   implicit none

   integer :: NOCG0, NA
   double precision :: epscg
   integer :: maxmatvecs, n30
   double precision, intent(in) :: alpha_loc !< ILU (0.0) to MILU (1.0) preconditioning

   NROW = NOCG0; nax = na
   ! n30  = 30
   n30 = 30

   NN = NROW; mm = n30 * nn; nwk = 2 * mm

   if (allocated(IA)) then
      deallocate (iao, jao, ia, ja, perm, kolrs, jlu, ju, iw)
      deallocate (ao, a, solo, rhso, sol, sol0, rhs, alu, wk)
   end if

   allocate (iao(nn + 1), jao(na), &
             ia(nn + 1), ja(na), &
             perm(nn), kolrs(nn), &
             jlu(mm), ju(nn), iw(nn * 3), stat=ierr)
   call aerr('iao(nn+1)', IERR, mm + 7 * nn + 2 * na)
   iao = 0; jao = 0; ia = 0; ja = 0
   jlu = 0; ju = 0; iw = 0; perm = 0

   allocate (ao(na), a(na), solo(nn), rhso(nn), sol(nn), &
             sol0(nn), rhs(nn), alu(mm), stat=ierr)
   call aerr('ao(na)', IERR, 2 * na + 5 * nn)

   ierr = -1
   do while (ierr /= 0)
      allocate (wk(nwk), stat=ierr)
      if (ierr /= 0) then
         nwk = 0.7 * nwk
      end if
   end do
   call aerr('wk(nwk)', IERR, nwk)

   ao = 0.d0; a = 0.d0
   solo = 0.d0; rhs = 0.d0; alu = 0.d0; wk = 0.d0
   sol = 0d0

   ipar = 0 ! initialize all params to 0
   ipar(2) = 1 ! no (0), left (1), right (2), both (3) precond
   ipar(3) = 1 ! stopping criteria
   ipar(4) = nwk ! number of elems in array 'wk'
   ipar(5) = 10 ! size of Krylov subspace in GMRES and variants
   ipar(6) = maxmatvecs ! max number of mat-vec multiplies

!---MB: old setting
!     fpar(1) = 1.0D-16         ! relative tolerance ('exact' solve, except
!     fpar(2) = 0.0D-16         ! absolute tolerance  for round-off errors)

!     fpar(1) = 1.0D-14         ! relative tolerance (faster convergence, and
!     fpar(2) = 0.0D-14         ! absolute tolerance  only twice larger errors)

!---MB20110330: convergence tolerance in agreement with openmp/method GS not
!   possible because method GS considers MAX(ABS(residualvector)) while in
!   SPARSKIT SQRT(DDOT(residualvector,residualvector)) is considered, which is
!   weighing of residualvector in L2 norm *times* the number of unknowns per
!   direction
   fpar = 0d0 ! initialize all params to 0
   fpar(1) = 0.0d-16 ! relative tolerance ('exact' solve, except
!     fpar(2) = 1.0D-14 * 1.0D2 ! absolute tolerance  for round-off errors)
!             = 1.0D-14 as in method GS * underestimation of number of unknowns
!     fpar(2) = 1.0D-8  * 1.0D0 ! MB: set lower tolerance
   fpar(2) = epscg

!      WRITE(*,*) 'REL, ABS '
!      READ(*,*) FPAR(1), FPAR(2)

   lfil = 3
!     alpha = 1.00000D0
!     alpha = 0.00000D0 ! SPvdP: set it to 0 for ILU preconditioning
   alpha = alpha_loc

   tol = 0.50d-2

!                       ***** PERMUTATIONS *****
!     permselect = 1    ! MB: no permutation
!     permselect = 2    ! MB: zig-zag permutation (does nothing, it seems)
!     permselect = 3    ! MB: multicolor ordering (works rather well)
!     permselect = 4    ! MB: local min ordering

!     comparison with Mart:
!      permselect = 3    ! Check if 3 still possible

   permselect = 1 ! Check if 3 still possible
   JAPERM = 0

end subroutine initsaad

subroutine cgsaad(its, na, nocg, jaini, jabcgstab, ierror, res)
   use m_saad

   implicit none

   integer :: its, na, nocg
   integer, intent(in) :: jaini !< compute preconditioner and permutation (1) or not (0), or initialization only (-1), or ILU solve only (2)
   integer, intent(in) :: jabcgstab !< use bcgstab (1) or cg (other)
   integer, intent(out) :: ierror !< error (1) or not (0)
   double precision, intent(out) :: res !< || residual ||

   integer :: n, m, maxcol, ncol, nset, job

   ierror = 0

   nrow = nocg

   if (jaini == -1 .or. jaini == 1) then

      if (JAPERM == 0) then

         select case (permselect)

         case (1) ! NO

            do n = 1, nrow
               perm(n) = n
            end do

         case (2) ! *** zig-zag permutation

            print *, ' '
            print *, '        *** ZIG-ZAG reordering ***'

            do n = 1, nrow
               m = mod(n - 1, 2 * (nx - 2)) + 1
               if (m <= nx - 2) then
                  perm(n) = n ! forward sweep of hor grid line
               else
                  perm(n) = n + 1 + 3 * (nx - 2) - 2 * m ! backward sweep of hor grid line
               end if
            end do

         case (3) ! *** multicoloring ordering

            maxcol = 4 ! maximum number of colors
            do n = 1, nrow
               perm(n) = n ! initialize permutation array
            end do

            ! print *, ' '
            ! print *, '        *** MULTICOLOR reordering ***'

            call multic(nrow, jao, iao, ncol, kolrs, il, perm, maxcol, ierr)

            ! WRITE( *, '(A,I3)' ) ' MULTICOLOR, ierr =', ierr
            ! WRITE( *, '(A,I4)' ) ' Number of colors found:', ncol

         case (4)
            ! *** independent set ordering with local minimization

            ! print *, ' '
            ! print *, '        *** LOCAL MIN reordering ***'

            call indset2(nrow, jao, iao, nset, perm, perminv, kolrs, sym, 1)

            ! WRITE( *, '(A,I8)' ) ' Number of unknowns in indep.set:', nset

         end select

         ! JAPERM = 1 ! no switch off for now
      end if

      ! permutation of matrix
      job = 1

      if (permselect > 1) then

         call dperm(nrow, ao, jao, iao, a, ja, ia, perm, perm, job)

      else

         a(1:na) = ao(1:na)
         ja(1:na) = jao(1:na)
         ia(1:nrow + 1) = iao(1:nrow + 1)

      end if

      ! *** preconditioner
      call ilud(nrow, a, ja, ia, alpha, tol, alu, jlu, ju, mm, wk, iw, ierr, na)

!         write(6,*) 'elements in ILU, check, rel.mem.use:',  &
!                    ju(nrow), ierr, real(ju(nrow)/real(nrow))

   end if

   if (jaini /= -1) then

      ! permutation of rhs
      call permsimple(nrow, rhs, wk, perm, permselect)

      ! permutation of sol
      call permsimple(nrow, sol, wk, perm, permselect)

      if (jaini == 2) then ! ILU solve only
         call lusol(nrow, rhs, sol, alu, jlu, ju, 30 * nrow)
         its = 0
      else
         call runrc2(nrow, rhs, sol, ipar, fpar, wk, a, ja, ia, alu, jlu, ju, its, epssaad, jabcgstab, ierror, 30 * nrow)
         res = fpar(5)
      end if
      call permsimpleINVERSE(nrow, sol, wk, perm, permselect)
   end if

end subroutine cgsaad

!----------------------------------------------------------------------c
!                          S P A R S K I T                             c
!----------------------------------------------------------------------c
!                   ITERATIVE SOLVERS MODULE                           c
!----------------------------------------------------------------------c
! This Version Dated: August 13, 1996. Warning: meaning of some        c
! ============ arguments have changed w.r.t. earlier versions. Some    c
!              Calling sequences may also have changed                 c
!----------------------------------------------------------------------c
! Contents:                                                            c
!-------------------------preconditioners------------------------------c
!                                                                      c
! ILUT    : Incomplete LU factorization with dual truncation strategy  c
! ILUTP   : ILUT with column  pivoting                                 c
! ILUD    : ILU with single dropping + diagonal compensation (~MILUT)  c
! ILUDP   : ILUD with column pivoting                                  c
! ILUK    : level-k ILU                                                c
! ILU0    : simple ILU(0) preconditioning                              c
! MILU0   : MILU(0) preconditioning                                    c
!                                                                      c
!----------sample-accelerator-and-LU-solvers---------------------------c
!                                                                      c
! PGMRES  : preconditioned GMRES solver                                c
! LUSOL   : forward followed by backward triangular solve (Precond.)   c
! LUTSOL  : solving v = (LU)^{-T} u (used for preconditioning)         c
!                                                                      c
!-------------------------utility-routine------------------------------c
!                                                                      c
! QSPLIT  : quick split routine used by ilut to sort out the k largest c
!           elements in absolute value                                 c
!                                                                      c
!----------------------------------------------------------------------c
!                                                                      c
! Note: all preconditioners are preprocessors to pgmres.               c
! usage: call preconditioner then call pgmres                          c
!                                                                      c
!----------------------------------------------------------------------c
subroutine ilut(n, a, ja, ia, lfil, droptol, alu, jlu, ju, iwk, w, jw, ierr)
!-----------------------------------------------------------------------
   implicit none
   integer n
   double precision :: a(*), alu(*), w(n + 1), droptol
   integer ja(*), ia(n + 1), jlu(*), ju(n), jw(2 * n), lfil, iwk, ierr
!----------------------------------------------------------------------*
!                      *** ILUT preconditioner ***                     *
!      incomplete LU factorization with dual truncation mechanism      *
!----------------------------------------------------------------------*
!     Author: Yousef Saad *May, 5, 1990, Latest revision, August 1996  *
!----------------------------------------------------------------------*
! PARAMETERS
!-----------
!
! on entry:
!==========
! n       = integer. The row dimension of the matrix A. The matrix
!
! a,ja,ia = matrix stored in Compressed Sparse Row format.
!
! lfil    = integer. The fill-in parameter. Each row of L and each row
!           of U will have a maximum of lfil elements (excluding the
!           diagonal element). lfil must be .ge. 0.
!           ** WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
!           EARLIER VERSIONS.
!
! droptol = double precision :: . Sets the threshold for dropping small terms in the
!           factorization. See below for details on dropping strategy.
!
!
! iwk     = integer. The lengths of arrays alu and jlu. If the arrays
!           are not big enough to store the ILU factorizations, ilut
!           will stop with an error message.
!
! On return:
!==========
!
! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
!           the L and U factors together. The diagonal (stored in
!           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
!           contains the i-th row of L (excluding the diagonal entry=1)
!           followed by the i-th row of U.
!
! ju      = integer array of length n containing the pointers to
!           the beginning of each row of U in the matrix alu,jlu.
!
! ierr    = integer. Error message with the following meaning.
!           ierr  = 0    --> successful return.
!           ierr .gt. 0  --> zero pivot encountered at step number ierr.
!           ierr  = -1   --> Error. input matrix may be wrong.
!                            (The elimination process has generated a
!                            row in L or U whose length is .gt.  n.)
!           ierr  = -2   --> The matrix L overflows the array al.
!           ierr  = -3   --> The matrix U overflows the array alu.
!           ierr  = -4   --> Illegal value for lfil.
!           ierr  = -5   --> zero row encountered.
!
! work arrays:
!=============
! jw      = integer work array of length 2*n.
! w       = real work array of length n+1.
!
!----------------------------------------------------------------------
! w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u]
! jw(n+1:2n)  stores nonzero indicators
!
! Notes:
! ------
! The diagonal elements of the input matrix must be  nonzero (at least
! 'structurally').
!
!----------------------------------------------------------------------*
!---- Dual drop strategy works as follows.                             *
!                                                                      *
!     1) Theresholding in L and U as set by droptol. Any element whose *
!        magnitude is less than some tolerance (relative to the abs    *
!        value of diagonal element in u) is dropped.                   *
!                                                                      *
!     2) Keeping only the largest lfil elements in the i-th row of L   *
!        and the largest lfil elements in the i-th row of U (excluding *
!        diagonal elements).                                           *
!                                                                      *
! Flexibility: one  can use  droptol=0  to get  a strategy  based on   *
! keeping  the largest  elements in  each row  of L  and U.   Taking   *
! droptol .ne.  0 but lfil=n will give  the usual threshold strategy   *
! (however, fill-in is then mpredictible).                             *
!----------------------------------------------------------------------*
!     locals
   integer ju0, k, j1, j2, j, ii, i, lenl, lenu, jj, jrow, jpos, len
   double precision :: tnorm, t, abs, s, fact
   if (lfil < 0) goto 998
!-----------------------------------------------------------------------
!     initialize ju0 (points to next element to be added to alu,jlu)
!     and pointer array.
!-----------------------------------------------------------------------
   ju0 = n + 2
   jlu(1) = ju0
!
!     initialize nonzero indicator array.
!
   do j = 1, n
      jw(n + j) = 0
   end do

!-----------------------------------------------------------------------
!     beginning of main loop.
!-----------------------------------------------------------------------
   do ii = 1, n
      j1 = ia(ii)
      j2 = ia(ii + 1) - 1
      tnorm = 0.0d0
      do k = j1, j2
         tnorm = tnorm + abs(a(k))
      end do

      if (tnorm == 0.0) goto 999
      tnorm = tnorm / real(j2 - j1 + 1)
!
!     unpack L-part and U-part of row of A in arrays w
!
      lenu = 1
      lenl = 0
      jw(ii) = ii
      w(ii) = 0.0
      jw(n + ii) = ii
!
      do j = j1, j2
         k = ja(j)
         t = a(j)
         if (k < ii) then
            lenl = lenl + 1
            jw(lenl) = k
            w(lenl) = t
            jw(n + k) = lenl
         else if (k == ii) then
            w(ii) = t
         else
            lenu = lenu + 1
            jpos = ii + lenu - 1
            jw(jpos) = k
            w(jpos) = t
            jw(n + k) = jpos
         end if
      end do

      jj = 0
      len = 0
!
!     eliminate previous rows
!
150   jj = jj + 1
      if (jj > lenl) goto 160
!-----------------------------------------------------------------------
!     in order to do the elimination in the correct order we must select
!     the smallest column index among jw(k), k=jj+1, ..., lenl.
!-----------------------------------------------------------------------
      jrow = jw(jj)
      k = jj
!
!     determine smallest column index
!
      do j = jj + 1, lenl
         if (jw(j) < jrow) then
            jrow = jw(j)
            k = j
         end if
      end do

!
      if (k /= jj) then
!     exchange in jw
         j = jw(jj)
         jw(jj) = jw(k)
         jw(k) = j
!     exchange in jr
         jw(n + jrow) = jj
         jw(n + j) = k
!     exchange in w
         s = w(jj)
         w(jj) = w(k)
         w(k) = s
      end if
!
!     zero out element in row by setting jw(n+jrow) to zero.
!
      jw(n + jrow) = 0
!
!     get the multiplier for row to be eliminated (jrow).
!
      fact = w(jj) * alu(jrow)
      if (abs(fact) <= droptol) goto 150
!
!     combine current row and row jrow
!
      do k = ju(jrow), jlu(jrow + 1) - 1
         s = fact * alu(k)
         j = jlu(k)
         jpos = jw(n + j)
         if (j >= ii) then
!
!     dealing with upper part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenu = lenu + 1
               if (lenu > n) goto 995
               i = ii + lenu - 1
               jw(i) = j
               jw(n + j) = i
               w(i) = -s
            else
!
!     this is not a fill-in element
!
               w(jpos) = w(jpos) - s

            end if
         else
!
!     dealing  with lower part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenl = lenl + 1
               if (lenl > n) goto 995
               jw(lenl) = j
               jw(n + j) = lenl
               w(lenl) = -s
            else
!
!     this is not a fill-in element
!
               w(jpos) = w(jpos) - s
            end if
         end if
      end do

!
!     store this pivot element -- (from left to right -- no danger of
!     overlap with the working elements in L (pivots).
!
      len = len + 1
      w(len) = fact
      jw(len) = jrow
      goto 150
160   continue
!
!     reset double-pointer to zero (U-part)
!
      do k = 1, lenu
         jw(n + jw(ii + k - 1)) = 0
      end do

!
!     update L-matrix
!
      lenl = len
      len = min(lenl, lfil)
!
!     sort by quick-split
!
      call qsplit(w, jw, lenl, len)
!
!     store L-part
!
      do k = 1, len
         if (ju0 > iwk) goto 996
         alu(ju0) = w(k)
         jlu(ju0) = jw(k)
         ju0 = ju0 + 1
      end do

!
!     save pointer to beginning of row ii of U
!
      ju(ii) = ju0
!
!     update U-matrix -- first apply dropping strategy
!
      len = 0
      do k = 1, lenu - 1
         if (abs(w(ii + k)) > droptol * tnorm) then
            len = len + 1
            w(ii + len) = w(ii + k)
            jw(ii + len) = jw(ii + k)
         end if
      end do
      lenu = len + 1
      len = min(lenu, lfil)
!
      call qsplit(w(ii + 1), jw(ii + 1), lenu - 1, len)
!
!     copy
!
      t = abs(w(ii))
      if (len + ju0 > iwk) goto 997
      do k = ii + 1, ii + len - 1
         jlu(ju0) = jw(k)
         alu(ju0) = w(k)
         t = t + abs(w(k))
         ju0 = ju0 + 1
      end do

!
!     store inverse of diagonal element of u
!
      if (w(ii) == 0.0) w(ii) = (0.0001 + droptol) * tnorm
!
      alu(ii) = 1.0d0 / w(ii)
!
!     update pointer to beginning of next row of U.
!
      jlu(ii + 1) = ju0
!-----------------------------------------------------------------------
!     end main loop
!-----------------------------------------------------------------------
   end do

   ierr = 0
   return
!
!     incomprehensible error. Matrix must be wrong.
!
995 ierr = -1
   return
!
!     insufficient storage in L.
!
996 ierr = -2
   return
!
!     insufficient storage in U.
!
997 ierr = -3
   return
!
!     illegal lfil entered.
!
998 ierr = -4
   return
!
!     zero row encountered
!
999 ierr = -5
   return
end subroutine ilut

subroutine ilutp(n, a, ja, ia, lfil, droptol, permtol, mbloc, alu, jlu, ju, iwk, w, jw, iperm, ierr)
   implicit none
   integer n, ja(*), ia(n + 1), lfil, jlu(*), ju(n), jw(2 * n), iwk, iperm(2 * n), ierr
   double precision :: a(*), alu(*), w(n + 1), droptol
!----------------------------------------------------------------------*
!       *** ILUTP preconditioner -- ILUT with pivoting  ***            *
!      incomplete LU factorization with dual truncation mechanism      *
!----------------------------------------------------------------------*
! author Yousef Saad *Sep 8, 1993 -- Latest revision, August 1996.     *
!----------------------------------------------------------------------*
! on entry:
!==========
! n       = integer. The dimension of the matrix A.
!
! a,ja,ia = matrix stored in Compressed Sparse Row format.
!           ON RETURN THE COLUMNS OF A ARE PERMUTED. SEE BELOW FOR
!           DETAILS.
!
! lfil    = integer. The fill-in parameter. Each row of L and each row
!           of U will have a maximum of lfil elements (excluding the
!           diagonal element). lfil must be .ge. 0.
!           ** WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
!           EARLIER VERSIONS.
!
! droptol = double precision :: . Sets the threshold for dropping small terms in the
!           factorization. See below for details on dropping strategy.
!
! lfil    = integer. The fill-in parameter. Each row of L and
!           each row of U will have a maximum of lfil elements.
!           WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
!           EARLIER VERSIONS.
!           lfil must be .ge. 0.
!
! permtol = tolerance ratio used to  determne whether or not to permute
!           two columns.  At step i columns i and j are permuted when
!
!                     abs(a(i,j))*permtol .gt. abs(a(i,i))
!
!           [0 --> never permute; good values 0.1 to 0.01]
!
! mbloc   = if desired, permuting can be done only within the diagonal
!           blocks of size mbloc. Useful for PDE problems with several
!           degrees of freedom.. If feature not wanted take mbloc=n.
!
!
! iwk     = integer. The lengths of arrays alu and jlu. If the arrays
!           are not big enough to store the ILU factorizations, ilut
!           will stop with an error message.
!
! On return:
!===========
!
! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
!           the L and U factors together. The diagonal (stored in
!           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
!           contains the i-th row of L (excluding the diagonal entry=1)
!           followed by the i-th row of U.
!
! ju      = integer array of length n containing the pointers to
!           the beginning of each row of U in the matrix alu,jlu.
!
! iperm   = contains the permutation arrays.
!           iperm(1:n) = old numbers of unknowns
!           iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! ierr    = integer. Error message with the following meaning.
!           ierr  = 0    --> successful return.
!           ierr .gt. 0  --> zero pivot encountered at step number ierr.
!           ierr  = -1   --> Error. input matrix may be wrong.
!                            (The elimination process has generated a
!                            row in L or U whose length is .gt.  n.)
!           ierr  = -2   --> The matrix L overflows the array al.
!           ierr  = -3   --> The matrix U overflows the array alu.
!           ierr  = -4   --> Illegal value for lfil.
!           ierr  = -5   --> zero row encountered.
!
! work arrays:
!=============
! jw      = integer work array of length 2*n.
! w       = real work array of length n
!
! IMPORTANR NOTE:
! --------------
! TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH LU-SOLVE,
! THE MATRIX A IS PERMUTED ON RETURN. [all column indices are
! changed]. SIMILARLY FOR THE U MATRIX.
! To permute the matrix back to its original state use the loop:
!
!      do k=ia(1), ia(n+1)-1
!         ja(k) = iperm(ja(k))
!      enddo
!
!-----------------------------------------------------------------------
!     local variables
!
   integer k, i, j, jrow, ju0, ii, j1, j2, jpos, len, imax, lenu, lenl, jj, mbloc, icut
   double precision :: s, tmp, tnorm, xmax, xmax0, fact, abs, t, permtol
!
   no_warning_unused_dummy_argument(mbloc)

   if (lfil < 0) goto 998
!-----------------------------------------------------------------------
!     initialize ju0 (points to next element to be added to alu,jlu)
!     and pointer array.
!-----------------------------------------------------------------------
   ju0 = n + 2
   jlu(1) = ju0
!
!  integer double pointer array.
!
   do j = 1, n
      jw(n + j) = 0
      iperm(j) = j
      iperm(n + j) = j
   end do

!-----------------------------------------------------------------------
!     beginning of main loop.
!-----------------------------------------------------------------------
   do ii = 1, n
      j1 = ia(ii)
      j2 = ia(ii + 1) - 1
      tnorm = 0.0d0
      do k = j1, j2
         tnorm = tnorm + abs(a(k))
      end do

      if (tnorm == 0.0) goto 999
      tnorm = tnorm / (j2 - j1 + 1)
!
!     unpack L-part and U-part of row of A in arrays  w  --
!
      lenu = 1
      lenl = 0
      jw(ii) = ii
      w(ii) = 0.0
      jw(n + ii) = ii
!
      do j = j1, j2
         k = iperm(n + ja(j))
         t = a(j)
         if (k < ii) then
            lenl = lenl + 1
            jw(lenl) = k
            w(lenl) = t
            jw(n + k) = lenl
         else if (k == ii) then
            w(ii) = t
         else
            lenu = lenu + 1
            jpos = ii + lenu - 1
            jw(jpos) = k
            w(jpos) = t
            jw(n + k) = jpos
         end if
      end do

      jj = 0
      len = 0
!
!     eliminate previous rows
!
150   jj = jj + 1
      if (jj > lenl) goto 160
!-----------------------------------------------------------------------
!     in order to do the elimination in the correct order we must select
!     the smallest column index among jw(k), k=jj+1, ..., lenl.
!-----------------------------------------------------------------------
      jrow = jw(jj)
      k = jj
!
!     determine smallest column index
!
      do j = jj + 1, lenl
         if (jw(j) < jrow) then
            jrow = jw(j)
            k = j
         end if
      end do

!
      if (k /= jj) then
!     exchange in jw
         j = jw(jj)
         jw(jj) = jw(k)
         jw(k) = j
!     exchange in jr
         jw(n + jrow) = jj
         jw(n + j) = k
!     exchange in w
         s = w(jj)
         w(jj) = w(k)
         w(k) = s
      end if
!
!     zero out element in row by resetting jw(n+jrow) to zero.
!
      jw(n + jrow) = 0
!
!     get the multiplier for row to be eliminated: jrow
!
      fact = w(jj) * alu(jrow)
!
!     drop term if small
!
      if (abs(fact) <= droptol) goto 150
!
!     combine current row and row jrow
!
      do k = ju(jrow), jlu(jrow + 1) - 1
         s = fact * alu(k)
!     new column number
         j = iperm(n + jlu(k))
         jpos = jw(n + j)
         if (j >= ii) then
!
!     dealing with upper part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenu = lenu + 1
               i = ii + lenu - 1
               if (lenu > n) goto 995
               jw(i) = j
               jw(n + j) = i
               w(i) = -s
            else
!     no fill-in element --
               w(jpos) = w(jpos) - s
            end if
         else
!
!     dealing with lower part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenl = lenl + 1
               if (lenl > n) goto 995
               jw(lenl) = j
               jw(n + j) = lenl
               w(lenl) = -s
            else
!
!     this is not a fill-in element
!
               w(jpos) = w(jpos) - s
            end if
         end if
      end do

!
!     store this pivot element -- (from left to right -- no danger of
!     overlap with the working elements in L (pivots).
!
      len = len + 1
      w(len) = fact
      jw(len) = jrow
      goto 150
160   continue
!
!     reset double-pointer to zero (U-part)
!
      do k = 1, lenu
         jw(n + jw(ii + k - 1)) = 0
      end do

!
!     update L-matrix
!
      lenl = len
      len = min(lenl, lfil)
!
!     sort by quick-split
!
      call qsplit(w, jw, lenl, len)
!
!     store L-part -- in original coordinates ..
!
      do k = 1, len
         if (ju0 > iwk) then
            goto 996
         end if
         alu(ju0) = w(k)
         jlu(ju0) = iperm(jw(k))
         ju0 = ju0 + 1
      end do

!
!     save pointer to beginning of row ii of U
!
      ju(ii) = ju0
!
!     update U-matrix -- first apply dropping strategy
!
      len = 0
      do k = 1, lenu - 1
         if (abs(w(ii + k)) > droptol * tnorm) then
            len = len + 1
            w(ii + len) = w(ii + k)
            jw(ii + len) = jw(ii + k)
         end if
      end do
      lenu = len + 1
      len = min(lenu, lfil)
      call qsplit(w(ii + 1), jw(ii + 1), lenu - 1, len)
!
!     determine next pivot --
!
      imax = ii
      xmax = abs(w(imax))
      xmax0 = xmax
!       icut = ii - 1 + mbloc - mod(ii-1,mbloc)
      do k = ii + 1, ii + len - 1
         t = abs(w(k))
         if (t > xmax .and. t * permtol > xmax0 .and. jw(k) <= icut) then
            imax = k
            xmax = t
         end if
      end do
!
!     exchange w's
!
      tmp = w(ii)
      w(ii) = w(imax)
      w(imax) = tmp
!
!     update iperm and reverse iperm
!
      j = jw(imax)
      i = iperm(ii)
      iperm(ii) = iperm(j)
      iperm(j) = i
!
!     reverse iperm
!
      iperm(n + iperm(ii)) = ii
      iperm(n + iperm(j)) = j
!-----------------------------------------------------------------------
!
      if (len + ju0 > iwk) goto 997
!
!     copy U-part in original coordinates
!
      do k = ii + 1, ii + len - 1
         jlu(ju0) = iperm(jw(k))
         alu(ju0) = w(k)
         ju0 = ju0 + 1
      end do

!
!     store inverse of diagonal element of u
!
      if (w(ii) == 0.0) w(ii) = (1.0d-4 + droptol) * tnorm
      alu(ii) = 1.0d0 / w(ii)
!
!     update pointer to beginning of next row of U.
!
      jlu(ii + 1) = ju0
!-----------------------------------------------------------------------
!     end main loop
!-----------------------------------------------------------------------
   end do
!
!     permute all column indices of LU ...
!
   do k = jlu(1), jlu(n + 1) - 1
      jlu(k) = iperm(n + jlu(k))
   end do
!
!     ...and of A
!
   do k = ia(1), ia(n + 1) - 1
      ja(k) = iperm(n + ja(k))
   end do
!
   ierr = 0
   return
!
!     incomprehensible error. Matrix must be wrong.
!
995 ierr = -1
   return
!
!     insufficient storage in L.
!
996 ierr = -2
   return
!
!     insufficient storage in U.
!
997 ierr = -3
   return
!
!     illegal lfil entered.
!
998 ierr = -4
   return
!
!     zero row encountered
!
999 ierr = -5
   return
end subroutine ilutp

subroutine ilud(n, a, ja, ia, alph, tol, alu, jlu, ju, iwk, w, jw, ierr, na)
   implicit none
   integer n, na
   double precision :: a(na), alu(*), w(2 * n), tol, alph
   integer ja(na), ia(n + 1), jlu(*), ju(n), jw(2 * n), iwk, ierr
!----------------------------------------------------------------------*
!                     *** ILUD preconditioner ***                      *
!    incomplete LU factorization with standard droppoing strategy      *
!----------------------------------------------------------------------*
! Author: Yousef Saad * Aug. 1995 --                                   *
!----------------------------------------------------------------------*
! This routine computes the ILU factorization with standard threshold  *
! dropping: at i-th step of elimination, an element a(i,j) in row i is *
! dropped  if it satisfies the criterion:                              *
!                                                                      *
!  abs(a(i,j)) < tol * [average magnitude of elements in row i of A]   *
!                                                                      *
! There is no control on memory size required for the factors as is    *
! done in ILUT. This routines computes also various diagonal compensa- *
! tion ILU's such MILU. These are defined through the parameter alph   *
!----------------------------------------------------------------------*
! on entry:
!==========
! n       = integer. The row dimension of the matrix A. The matrix
!
! a,ja,ia = matrix stored in Compressed Sparse Row format
!
! alph    = diagonal compensation parameter -- the term:
!
!           alph*(sum of all dropped out elements in a given row)
!
!           is added to the diagonal element of U of the factorization
!           Thus: alph = 0 ---> ~ ILU with threshold,
!                 alph = 1 ---> ~ MILU with threshold.
!
! tol     = Threshold parameter for dropping small terms in the
!           factorization. During the elimination, a term a(i,j) is
!           dropped whenever abs(a(i,j)) .lt. tol * [weighted norm of
!           row i]. Here weighted norm = 1-norm / number of nnz
!           elements in the row.
!
! iwk     = The length of arrays alu and jlu -- this routine will stop
!           if storage for the factors L and U is not sufficient
!
! On return:
!===========
!
! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
!           the L and U factors together. The diagonal (stored in
!           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
!           contains the i-th row of L (excluding the diagonal entry=1)
!           followed by the i-th row of U.
!
! ju      = integer array of length n containing the pointers to
!           the beginning of each row of U in the matrix alu,jlu.
!
! ierr    = integer. Error message with the following meaning.
!           ierr  = 0    --> successful return.
!           ierr .gt. 0  --> zero pivot encountered at step number ierr.
!           ierr  = -1   --> Error. input matrix may be wrong.
!                            (The elimination process has generated a
!                            row in L or U whose length is .gt.  n.)
!           ierr  = -2   --> Insufficient storage for the LU factors --
!                            arrays alu/ jalu are  overflowed.
!           ierr  = -3   --> Zero row encountered.
!
! Work Arrays:
!=============
! jw      = integer work array of length 2*n.
! w       = real work array of length n
!
!----------------------------------------------------------------------
!
! w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u]
! jw(n+1:2n)  stores the nonzero indicator.
!
! Notes:
! ------
! All diagonal elements of the input matrix must be  nonzero.
!
!-----------------------------------------------------------------------
!     locals
   integer ju0, k, j1, j2, j, ii, i, lenl, lenu, jj, jrow, jpos, len
   double precision :: tnorm, t, abs, s, fact, dropsum
!-----------------------------------------------------------------------
!     initialize ju0 (points to next element to be added to alu,jlu)
!     and pointer array.
!-----------------------------------------------------------------------
   ju0 = n + 2
   jlu(1) = ju0
!
!     initialize nonzero indicator array.
!
   do j = 1, n
      jw(n + j) = 0
   end do

!-----------------------------------------------------------------------
!     beginning of main loop.
!-----------------------------------------------------------------------
   do ii = 1, n
      j1 = ia(ii)
      j2 = ia(ii + 1) - 1
      dropsum = 0.0d0
      tnorm = 0.0d0
      do k = j1, j2
         tnorm = tnorm + abs(a(k))
      end do

      if (tnorm == 0.0) goto 997
      tnorm = tnorm / real(j2 - j1 + 1)
!
!     unpack L-part and U-part of row of A in arrays w
!
      lenu = 1
      lenl = 0
      jw(ii) = ii
      w(ii) = 0.0
      jw(n + ii) = ii
!
      do j = j1, j2
         k = ja(j)
         t = a(j)
         if (k < ii) then
            lenl = lenl + 1
            jw(lenl) = k
            w(lenl) = t
            jw(n + k) = lenl
         else if (k == ii) then
            w(ii) = t
         else
            lenu = lenu + 1
            jpos = ii + lenu - 1
            jw(jpos) = k
            w(jpos) = t
            jw(n + k) = jpos
         end if
      end do

      jj = 0
      len = 0
!
!     eliminate previous rows
!
150   jj = jj + 1
      if (jj > lenl) goto 160
!-----------------------------------------------------------------------
!     in order to do the elimination in the correct order we must select
!     the smallest column index among jw(k), k=jj+1, ..., lenl.
!-----------------------------------------------------------------------
      jrow = jw(jj)
      k = jj
!
!     determine smallest column index
!
      do j = jj + 1, lenl
         if (jw(j) < jrow) then
            jrow = jw(j)
            k = j
         end if
      end do

!
      if (k /= jj) then
!     exchange in jw
         j = jw(jj)
         jw(jj) = jw(k)
         jw(k) = j
!     exchange in jr
         jw(n + jrow) = jj
         jw(n + j) = k
!     exchange in w
         s = w(jj)
         w(jj) = w(k)
         w(k) = s
      end if
!
!     zero out element in row by setting resetting jw(n+jrow) to zero.
!
      jw(n + jrow) = 0
!
!     drop term if small
!
!         if (abs(w(jj)) .le. tol*tnorm) then
!            dropsum = dropsum + w(jj)
!            goto 150
!         endif
!
!     get the multiplier for row to be eliminated (jrow).
!
      fact = w(jj) * alu(jrow)
!
!     drop term if small
!
      if (abs(fact) <= tol) then
         dropsum = dropsum + w(jj)
         goto 150
      end if
!
!     combine current row and row jrow
!
      do k = ju(jrow), jlu(jrow + 1) - 1
         s = fact * alu(k)
         j = jlu(k)
         jpos = jw(n + j)
         if (j >= ii) then
!
!     dealing with upper part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenu = lenu + 1
               if (lenu > n) goto 995
               i = ii + lenu - 1
               jw(i) = j
               jw(n + j) = i
               w(i) = -s
            else
!
!     this is not a fill-in element
!
               w(jpos) = w(jpos) - s
            end if
         else
!
!     dealing with lower part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenl = lenl + 1
               if (lenl > n) goto 995
               jw(lenl) = j
               jw(n + j) = lenl
               w(lenl) = -s
            else
!
!     this is not a fill-in element
!
               w(jpos) = w(jpos) - s
            end if
         end if
      end do

      len = len + 1
      w(len) = fact
      jw(len) = jrow
      goto 150
160   continue
!
!     reset double-pointer to zero (For U-part only)
!
      do k = 1, lenu
         jw(n + jw(ii + k - 1)) = 0
      end do

!
!     update l-matrix
!
      do k = 1, len
         if (ju0 > iwk) then
            goto 996
         end if
         alu(ju0) = w(k)
         jlu(ju0) = jw(k)
         ju0 = ju0 + 1
      end do

!
!     save pointer to beginning of row ii of U
!
      ju(ii) = ju0
!
!     go through elements in U-part of w to determine elements to keep
!
      len = 0
      do k = 1, lenu - 1
!            if (abs(w(ii+k)) .gt. tnorm*tol) then
         if (abs(w(ii + k)) > abs(w(ii)) * tol) then
            len = len + 1
            w(ii + len) = w(ii + k)
            jw(ii + len) = jw(ii + k)
         else
            dropsum = dropsum + w(ii + k)
         end if
      end do
!
!     now update u-matrix
!
      if (ju0 + len - 1 > iwk) then
         goto 996
      end if
      do k = ii + 1, ii + len
         jlu(ju0) = jw(k)
         alu(ju0) = w(k)
         ju0 = ju0 + 1
      end do

!
!     define diagonal element
!
      w(ii) = w(ii) + alph * dropsum
!
!     store inverse of diagonal element of u
!
      if (w(ii) == 0.0) w(ii) = (0.0001 + tol) * tnorm
!
      alu(ii) = 1.0d0 / w(ii)
!
!     update pointer to beginning of next row of U.
!
      jlu(ii + 1) = ju0
!-----------------------------------------------------------------------
!     end main loop
!-----------------------------------------------------------------------
   end do
   ierr = 0
   return
!
!     incomprehensible error. Matrix must be wrong.
!
995 ierr = -1
   return
!
!     insufficient storage in alu/ jlu arrays for  L / U factors
!
996 ierr = -2
   return
!
!     zero row encountered
!
997 ierr = -3
   return
end subroutine ilud

subroutine iludp(n, a, ja, ia, alph, droptol, permtol, mbloc, alu, jlu, ju, iwk, w, jw, iperm, ierr)
   implicit none
   integer n, ja(*), ia(n + 1), mbloc, jlu(*), ju(n), jw(2 * n), iwk, iperm(2 * n), ierr
   double precision :: a(*), alu(*), w(2 * n), alph, droptol, permtol
!----------------------------------------------------------------------*
!                     *** ILUDP preconditioner ***                     *
!    incomplete LU factorization with standard droppoing strategy      *
!    and column pivoting                                               *
!----------------------------------------------------------------------*
! author Yousef Saad -- Aug 1995.                                      *
!----------------------------------------------------------------------*
! on entry:
!==========
! n       = integer. The dimension of the matrix A.
!
! a,ja,ia = matrix stored in Compressed Sparse Row format.
!           ON RETURN THE COLUMNS OF A ARE PERMUTED.
!
! alph    = diagonal compensation parameter -- the term:
!
!           alph*(sum of all dropped out elements in a given row)
!
!           is added to the diagonal element of U of the factorization
!           Thus: alph = 0 ---> ~ ILU with threshold,
!                 alph = 1 ---> ~ MILU with threshold.
!
! droptol = tolerance used for dropping elements in L and U.
!           elements are dropped if they are .lt. norm(row) x droptol
!           row = row being eliminated
!
! permtol = tolerance ratio used for determning whether to permute
!           two columns.  Two columns are permuted only when
!           abs(a(i,j))*permtol .gt. abs(a(i,i))
!           [0 --> never permute; good values 0.1 to 0.01]
!
! mbloc   = if desired, permuting can be done only within the diagonal
!           blocks of size mbloc. Useful for PDE problems with several
!           degrees of freedom.. If feature not wanted take mbloc=n.
!
! iwk     = integer. The declared lengths of arrays alu and jlu
!           if iwk is not large enough the code will stop prematurely
!           with ierr = -2 or ierr = -3 (see below).
!
! On return:
!===========
!
! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
!           the L and U factors together. The diagonal (stored in
!           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
!           contains the i-th row of L (excluding the diagonal entry=1)
!           followed by the i-th row of U.
!
! ju      = integer array of length n containing the pointers to
!           the beginning of each row of U in the matrix alu,jlu.
! iperm   = contains the permutation arrays ..
!           iperm(1:n) = old numbers of unknowns
!           iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! ierr    = integer. Error message with the following meaning.
!           ierr  = 0    --> successful return.
!           ierr .gt. 0  --> zero pivot encountered at step number ierr.
!           ierr  = -1   --> Error. input matrix may be wrong.
!                            (The elimination process has generated a
!                            row in L or U whose length is .gt.  n.)
!           ierr  = -2   --> The L/U matrix overflows the arrays alu,jlu
!           ierr  = -3   --> zero row encountered.
!
! work arrays:
!=============
! jw      = integer work array of length 2*n.
! w       = real work array of length 2*n
!
! Notes:
! ------
! IMPORTANT: TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH
! LU-SOLVE, THE MATRIX A IS PERMUTED ON RETURN. [all column indices are
! changed]. SIMILARLY FOR THE U MATRIX.
! To permute the matrix back to its original state use the loop:
!
!      do k=ia(1), ia(n+1)-1
!         ja(k) = perm(ja(k))
!      enddo
!
!-----------------------------------------------------------------------
!     local variables
!
   integer k, i, j, jrow, ju0, ii, j1, j2, jpos, len, imax, lenu, lenl, jj, icut
   double precision :: s, tmp, tnorm, xmax, xmax0, fact, abs, t, dropsum
!-----------------------------------------------------------------------
!     initialize ju0 (points to next element to be added to alu,jlu)
!     and pointer array.
!-----------------------------------------------------------------------
   no_warning_unused_dummy_argument(mbloc)

   ju0 = n + 2
   jlu(1) = ju0
!
!  integer double pointer array.
!
   do j = 1, n
      jw(n + j) = 0
      iperm(j) = j
      iperm(n + j) = j
   end do

!-----------------------------------------------------------------------
!     beginning of main loop.
!-----------------------------------------------------------------------
   do ii = 1, n
      j1 = ia(ii)
      j2 = ia(ii + 1) - 1
      dropsum = 0.0d0
      tnorm = 0.0d0
      do k = j1, j2
         tnorm = tnorm + abs(a(k))
      end do

      if (tnorm == 0.0) goto 997
      tnorm = tnorm / (j2 - j1 + 1)
!
!     unpack L-part and U-part of row of A in arrays  w  --
!
      lenu = 1
      lenl = 0
      jw(ii) = ii
      w(ii) = 0.0
      jw(n + ii) = ii
!
      do j = j1, j2
         k = iperm(n + ja(j))
         t = a(j)
         if (k < ii) then
            lenl = lenl + 1
            jw(lenl) = k
            w(lenl) = t
            jw(n + k) = lenl
         else if (k == ii) then
            w(ii) = t
         else
            lenu = lenu + 1
            jpos = ii + lenu - 1
            jw(jpos) = k
            w(jpos) = t
            jw(n + k) = jpos
         end if
      end do

      jj = 0
      len = 0
!
!     eliminate previous rows
!
150   jj = jj + 1
      if (jj > lenl) goto 160
!-----------------------------------------------------------------------
!     in order to do the elimination in the correct order we must select
!     the smallest column index among jw(k), k=jj+1, ..., lenl.
!-----------------------------------------------------------------------
      jrow = jw(jj)
      k = jj
!
!     determine smallest column index
!
      do j = jj + 1, lenl
         if (jw(j) < jrow) then
            jrow = jw(j)
            k = j
         end if
      end do

!
      if (k /= jj) then
!     exchange in jw
         j = jw(jj)
         jw(jj) = jw(k)
         jw(k) = j
!     exchange in jr
         jw(n + jrow) = jj
         jw(n + j) = k
!     exchange in w
         s = w(jj)
         w(jj) = w(k)
         w(k) = s
      end if
!
!     zero out element in row by resetting jw(n+jrow) to zero.
!
      jw(n + jrow) = 0
!
!     drop term if small
!
      if (abs(w(jj)) <= droptol * tnorm) then
         dropsum = dropsum + w(jj)
         goto 150
      end if
!
!     get the multiplier for row to be eliminated: jrow
!
      fact = w(jj) * alu(jrow)
!
!     combine current row and row jrow
!
      do k = ju(jrow), jlu(jrow + 1) - 1
         s = fact * alu(k)
!     new column number
         j = iperm(n + jlu(k))
         jpos = jw(n + j)
!
!     if fill-in element is small then disregard:
!
         if (j >= ii) then
!
!     dealing with upper part.
!
            if (jpos == 0) then
!     this is a fill-in element
               lenu = lenu + 1
               i = ii + lenu - 1
               if (lenu > n) goto 995
               jw(i) = j
               jw(n + j) = i
               w(i) = -s
            else
!     no fill-in element --
               w(jpos) = w(jpos) - s
            end if
         else
!
!     dealing with lower part.
!
            if (jpos == 0) then
!     this is a fill-in element
               lenl = lenl + 1
               if (lenl > n) goto 995
               jw(lenl) = j
               jw(n + j) = lenl
               w(lenl) = -s
            else
!     no fill-in element --
               w(jpos) = w(jpos) - s
            end if
         end if
      end do

      len = len + 1
      w(len) = fact
      jw(len) = jrow
      goto 150
160   continue
!
!     reset double-pointer to zero (U-part)
!
      do k = 1, lenu
         jw(n + jw(ii + k - 1)) = 0
      end do
!
!     update L-matrix
!
      do k = 1, len
         if (ju0 > iwk) goto 996
         alu(ju0) = w(k)
         jlu(ju0) = iperm(jw(k))
         ju0 = ju0 + 1
      end do
!
!     save pointer to beginning of row ii of U
!
      ju(ii) = ju0
!
!     update u-matrix -- first apply dropping strategy
!
      len = 0
      do k = 1, lenu - 1
         if (abs(w(ii + k)) > tnorm * droptol) then
            len = len + 1
            w(ii + len) = w(ii + k)
            jw(ii + len) = jw(ii + k)
         else
            dropsum = dropsum + w(ii + k)
         end if
      end do
!
      imax = ii
      xmax = abs(w(imax))
      xmax0 = xmax
!       icut = ii - 1 + mbloc - mod(ii-1,mbloc)
!
!     determine next pivot --
!
      do k = ii + 1, ii + len
         t = abs(w(k))
         if (t > xmax .and. t * permtol > xmax0 .and. jw(k) <= icut) then
            imax = k
            xmax = t
         end if
      end do
!
!     exchange w's
!
      tmp = w(ii)
      w(ii) = w(imax)
      w(imax) = tmp
!
!     update iperm and reverse iperm
!
      j = jw(imax)
      i = iperm(ii)
      iperm(ii) = iperm(j)
      iperm(j) = i
!     reverse iperm
      iperm(n + iperm(ii)) = ii
      iperm(n + iperm(j)) = j
!-----------------------------------------------------------------------
      if (len + ju0 - 1 > iwk) then
         goto 996
      end if
!
!     copy U-part in original coordinates
!
      do k = ii + 1, ii + len
         jlu(ju0) = iperm(jw(k))
         alu(ju0) = w(k)
         ju0 = ju0 + 1
      end do
!
!     define diagonal element
!
      w(ii) = w(ii) + alph * dropsum
!
!     store inverse of diagonal element of u
!
      if (w(ii) == 0.0) w(ii) = (1.0d-4 + droptol) * tnorm
!
      alu(ii) = 1.0d0 / w(ii)
!
!     update pointer to beginning of next row of U.
!
      jlu(ii + 1) = ju0
!-----------------------------------------------------------------------
!     end main loop
!-----------------------------------------------------------------------
   end do
!
!     permute all column indices of LU ...
!
   do k = jlu(1), jlu(n + 1) - 1
      jlu(k) = iperm(n + jlu(k))
   end do
!
!     ...and of A
!
   do k = ia(1), ia(n + 1) - 1
      ja(k) = iperm(n + ja(k))
   end do
!
   ierr = 0
   return
!
!     incomprehensible error. Matrix must be wrong.
!
995 ierr = -1
   return
!
!     insufficient storage in arrays alu, jlu to store factors
!
996 ierr = -2
   return
!
!     zero row encountered
!
997 ierr = -3
   return
end subroutine iludp

subroutine iluk(n, a, ja, ia, lfil, alu, jlu, ju, levs, iwk, w, jw, ierr)
   implicit none
   integer n
   double precision :: a(*), alu(*), w(n)
   integer ja(*), ia(n + 1), jlu(*), ju(n), levs(*), jw(3 * n), lfil, iwk, ierr
!----------------------------------------------------------------------*
!     SPARSKIT ROUTINE ILUK -- ILU WITH LEVEL OF FILL-IN OF K (ILU(k)) *
!----------------------------------------------------------------------*
!
! on entry:
!==========
! n       = integer. The row dimension of the matrix A. The matrix
!
! a,ja,ia = matrix stored in Compressed Sparse Row format.
!
! lfil    = integer. The fill-in parameter. Each element whose
!           leve-of-fill exceeds lfil during the ILU process is dropped.
!           lfil must be .ge. 0
!
! tol     = double precision :: . Sets the threshold for dropping small terms in the
!           factorization. See below for details on dropping strategy.
!
! iwk     = integer. The minimum length of arrays alu, jlu, and levs.
!
! On return:
!===========
!
! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
!           the L and U factors together. The diagonal (stored in
!           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
!           contains the i-th row of L (excluding the diagonal entry=1)
!           followed by the i-th row of U.
!
! ju      = integer array of length n containing the pointers to
!           the beginning of each row of U in the matrix alu,jlu.
!
! levs    = integer (work) array of size iwk -- which contains the
!           levels of each element in alu, jlu.
!
! ierr    = integer. Error message with the following meaning.
!           ierr  = 0    --> successful return.
!           ierr .gt. 0  --> zero pivot encountered at step number ierr.
!           ierr  = -1   --> Error. input matrix may be wrong.
!                            (The elimination process has generated a
!                            row in L or U whose length is .gt.  n.)
!           ierr  = -2   --> The matrix L overflows the array al.
!           ierr  = -3   --> The matrix U overflows the array alu.
!           ierr  = -4   --> Illegal value for lfil.
!           ierr  = -5   --> zero row encountered in A or U.
!
! work arrays:
!=============
! jw      = integer work array of length 3*n.
! w       = real work array of length n
!
! Notes/known bugs: This is not implemented efficiently storage-wise.
!       For example: Only the part of the array levs(*) associated with
!       the U-matrix is needed in the routine.. So some storage can
!       be saved if needed. The levels of fills in the LU matrix are
!       output for information only -- they are not needed by LU-solve.
!
!----------------------------------------------------------------------
! w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u]
! jw(n+1:2n)  stores the nonzero indicator.
!
! Notes:
! ------
! All the diagonal elements of the input matrix must be  nonzero.
!
!----------------------------------------------------------------------*
!     locals
   integer ju0, k, j1, j2, j, ii, i, lenl, lenu, jj, jrow, jpos, n2, jlev, min
   double precision :: t, s, fact
   if (lfil < 0) goto 998
!-----------------------------------------------------------------------
!     initialize ju0 (points to next element to be added to alu,jlu)
!     and pointer array.
!-----------------------------------------------------------------------
   n2 = n + n
   ju0 = n + 2
   jlu(1) = ju0
!
!     initialize nonzero indicator array + levs array --
!
   do j = 1, 2 * n
      jw(j) = 0
   end do
!-----------------------------------------------------------------------
!     beginning of main loop.
!-----------------------------------------------------------------------
   do ii = 1, n
      j1 = ia(ii)
      j2 = ia(ii + 1) - 1
!
!     unpack L-part and U-part of row of A in arrays w
!
      lenu = 1
      lenl = 0
      jw(ii) = ii
      w(ii) = 0.0
      jw(n + ii) = ii
!
      do j = j1, j2
         k = ja(j)
         t = a(j)
         if (t == 0.0) cycle
         if (k < ii) then
            lenl = lenl + 1
            jw(lenl) = k
            w(lenl) = t
            jw(n2 + lenl) = 0
            jw(n + k) = lenl
         else if (k == ii) then
            w(ii) = t
            jw(n2 + ii) = 0
         else
            lenu = lenu + 1
            jpos = ii + lenu - 1
            jw(jpos) = k
            w(jpos) = t
            jw(n2 + jpos) = 0
            jw(n + k) = jpos
         end if
      end do
!
      jj = 0
!
!     eliminate previous rows
!
150   jj = jj + 1
      if (jj > lenl) goto 160
!-----------------------------------------------------------------------
!     in order to do the elimination in the correct order we must select
!     the smallest column index among jw(k), k=jj+1, ..., lenl.
!-----------------------------------------------------------------------
      jrow = jw(jj)
      k = jj
!
!     determine smallest column index
!
      do j = jj + 1, lenl
         if (jw(j) < jrow) then
            jrow = jw(j)
            k = j
         end if
      end do
!
      if (k /= jj) then
!     exchange in jw
         j = jw(jj)
         jw(jj) = jw(k)
         jw(k) = j
!     exchange in jw(n+  (pointers/ nonzero indicator).
         jw(n + jrow) = jj
         jw(n + j) = k
!     exchange in jw(n2+  (levels)
         j = jw(n2 + jj)
         jw(n2 + jj) = jw(n2 + k)
         jw(n2 + k) = j
!     exchange in w
         s = w(jj)
         w(jj) = w(k)
         w(k) = s
      end if
!
!     zero out element in row by resetting jw(n+jrow) to zero.
!
      jw(n + jrow) = 0
!
!     get the multiplier for row to be eliminated (jrow) + its level
!
      fact = w(jj) * alu(jrow)
      jlev = jw(n2 + jj)
      if (jlev > lfil) goto 150
!
!     combine current row and row jrow
!
      do k = ju(jrow), jlu(jrow + 1) - 1
         s = fact * alu(k)
         j = jlu(k)
         jpos = jw(n + j)
         if (j >= ii) then
!
!     dealing with upper part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenu = lenu + 1
               if (lenu > n) goto 995
               i = ii + lenu - 1
               jw(i) = j
               jw(n + j) = i
               w(i) = -s
               jw(n2 + i) = jlev + levs(k) + 1
            else
!
!     this is not a fill-in element
!
               w(jpos) = w(jpos) - s
               jw(n2 + jpos) = min(jw(n2 + jpos), jlev + levs(k) + 1)
            end if
         else
!
!     dealing with lower part.
!
            if (jpos == 0) then
!
!     this is a fill-in element
!
               lenl = lenl + 1
               if (lenl > n) goto 995
               jw(lenl) = j
               jw(n + j) = lenl
               w(lenl) = -s
               jw(n2 + lenl) = jlev + levs(k) + 1
            else
!
!     this is not a fill-in element
!
               w(jpos) = w(jpos) - s
               jw(n2 + jpos) = min(jw(n2 + jpos), jlev + levs(k) + 1)
            end if
         end if
      end do
      w(jj) = fact
      jw(jj) = jrow
      goto 150
160   continue
!
!     reset double-pointer to zero (U-part)
!
      do k = 1, lenu
         jw(n + jw(ii + k - 1)) = 0
      end do
!
!     update l-matrix
!
      do k = 1, lenl
         if (ju0 > iwk) goto 996
         if (jw(n2 + k) <= lfil) then
            alu(ju0) = w(k)
            jlu(ju0) = jw(k)
            ju0 = ju0 + 1
         end if
      end do
!
!     save pointer to beginning of row ii of U
!
      ju(ii) = ju0
!
!     update u-matrix
!
      do k = ii + 1, ii + lenu - 1
         if (jw(n2 + k) <= lfil) then
            jlu(ju0) = jw(k)
            alu(ju0) = w(k)
            levs(ju0) = jw(n2 + k)
            ju0 = ju0 + 1
         end if
      end do

      if (w(ii) == 0.0) goto 999
!
      alu(ii) = 1.0d0 / w(ii)
!
!     update pointer to beginning of next row of U.
!
      jlu(ii + 1) = ju0
!-----------------------------------------------------------------------
!     end main loop
!-----------------------------------------------------------------------
   end do
   ierr = 0
   return
!
!     incomprehensible error. Matrix must be wrong.
!
995 ierr = -1
   return
!
!     insufficient storage in L.
!
996 ierr = -2
   return
!
!     insufficient storage in U.
!
997 ierr = -3
   return
!
!     illegal lfil entered.
!
998 ierr = -4
   return
!
!     zero row encountered in A or U.
!
999 ierr = -5
   return
end subroutine iluk

subroutine ilu0(n, a, ja, ia, alu, jlu, ju, iw, ierr)
   use precision_basics, only: dp

   implicit none

   integer :: n, ierr
   integer :: ja(*), ia(*), ju(*), jlu(*), iw(*)
   real(dp) :: a(*), alu(*)
!------------------ right preconditioner ------------------------------*
!                    ***   ilu(0) preconditioner.   ***                *
!----------------------------------------------------------------------*
! Note that this has been coded in such a way that it can be used
! with pgmres. Normally, since the data structure of the L+U matrix is
! the same as that the A matrix, savings can be made. In fact with
! some definitions (not correct for general sparse matrices) all we
! need in addition to a, ja, ia is an additional diagonal.
! ILU0 is not recommended for serious problems. It is only provided
! here for comparison purposes.
!-----------------------------------------------------------------------
!
! on entry:
!---------
! n       = dimension of matrix
! a, ja,
! ia      = original matrix in compressed sparse row storage.
!
! on return:
!-----------
! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
!           the L and U factors together. The diagonal (stored in
!           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
!           contains the i-th row of L (excluding the diagonal entry=1)
!           followed by the i-th row of U.
!
! ju    = pointer to the diagonal elements in alu, jlu.
!
! ierr     = integer indicating error code on return
!       ierr = 0 --> normal return
!       ierr = k --> code encountered a zero pivot at step k.
! work arrays:
!-------------
! iw      = integer work array of length n.
!------------
! IMPORTANT
!-----------
! it is assumed that the the elements in the input matrix are stored
!    in such a way that in each row the lower part comes first and
!    then the upper part. To get the correct ILU factorization, it is
!    also necessary to have the elements of L sorted by increasing
!    column number. It may therefore be necessary to sort the
!    elements of a, ja, ia prior to calling ilu0. This can be
!    achieved by transposing the matrix twice using csrcsc.
!
!-----------------------------------------------------------------------
   integer :: ju0, i, ii, js, j, jcol, jf, jm, jrow, jj, jw
   real(dp) :: tl

   ju0 = n + 2
   jlu(1) = ju0
!
! initialize work vector to zero's
!
   do i = 1, n
      iw(i) = 0
   end do
!
! main loop
!
   do ii = 1, n
      js = ju0
!
! generating row number ii of L and U.
!
      do j = ia(ii), ia(ii + 1) - 1
!
!     copy row ii of a, ja, ia into row ii of alu, jlu (L/U) matrix.
!
         jcol = ja(j)
         if (jcol == ii) then
            alu(ii) = a(j)
            iw(jcol) = ii
            ju(ii) = ju0
         else
            alu(ju0) = a(j)
            jlu(ju0) = ja(j)
            iw(jcol) = ju0
            ju0 = ju0 + 1
         end if
      end do
      jlu(ii + 1) = ju0
      jf = ju0 - 1
      jm = ju(ii) - 1
!
!     exit if diagonal element is reached.
!
      do j = js, jm
         jrow = jlu(j)
         tl = alu(j) * alu(jrow)
         alu(j) = tl
!
!     perform  linear combination
!
         do jj = ju(jrow), jlu(jrow + 1) - 1
            jw = iw(jlu(jj))
            if (jw /= 0) alu(jw) = alu(jw) - tl * alu(jj)
         end do
      end do
!
!     invert  and store diagonal element.
!
      if (alu(ii) == 0.0d0) goto 600
      alu(ii) = 1.0d0 / alu(ii)
!
!     reset pointer iw to zero
!
      iw(ii) = 0
      do i = js, jf
         iw(jlu(i)) = 0
      end do
   end do
   ierr = 0
   return
!
!     zero pivot :
!
600 ierr = ii
!
   return
end subroutine ilu0

subroutine milu0(n, a, ja, ia, alu, jlu, ju, iw, ierr)
   use precision_basics, only: dp

   implicit none

   integer :: n, ierr
   real(dp) :: a(*), alu(*)
   integer :: ja(*), ia(*), ju(*), jlu(*), iw(*)
!----------------------------------------------------------------------*
!                *** simple milu(0) preconditioner. ***                *
!----------------------------------------------------------------------*
! Note that this has been coded in such a way that it can be used
! with pgmres. Normally, since the data structure of a, ja, ia is
! the same as that of a, ja, ia, savings can be made. In fact with
! some definitions (not correct for general sparse matrices) all we
! need in addition to a, ja, ia is an additional diagonal.
! Ilu0 is not recommended for serious problems. It is only provided
! here for comparison purposes.
!-----------------------------------------------------------------------
!
! on entry:
!----------
! n       = dimension of matrix
! a, ja,
! ia      = original matrix in compressed sparse row storage.
!
! on return:
!----------
! alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
!           the L and U factors together. The diagonal (stored in
!           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
!           contains the i-th row of L (excluding the diagonal entry=1)
!           followed by the i-th row of U.
!
! ju    = pointer to the diagonal elements in alu, jlu.
!
! ierr     = integer indicating error code on return
!c      ierr = 0 --> normal return
!c      ierr = k --> code encountered a zero pivot at step k.
! work arrays:
!-------------
! iw      = integer work array of length n.
!------------
! Note (IMPORTANT):
!-----------
! it is assumed that the the elements in the input matrix are ordered
!    in such a way that in each row the lower part comes first and
!    then the upper part. To get the correct ILU factorization, it is
!    also necessary to have the elements of L ordered by increasing
!    column number. It may therefore be necessary to sort the
!    elements of a, ja, ia prior to calling milu0. This can be
!    achieved by transposing the matrix twice using csrcsc.
!-----------------------------------------------------------
   integer :: ju0, i, ii, js, j, jcol, jf, jm, jrow, jj, jw
   real(dp) :: s, tl

   ju0 = n + 2
   jlu(1) = ju0
! initialize work vector to zero's
   do i = 1, n
      iw(i) = 0
   end do
!
!-------------- MAIN LOOP ----------------------------------
!
   do ii = 1, n
      js = ju0
!
! generating row number ii or L and U.
!
      do j = ia(ii), ia(ii + 1) - 1
!
!     copy row ii of a, ja, ia into row ii of alu, jlu (L/U) matrix.
!
         jcol = ja(j)
         if (jcol == ii) then
            alu(ii) = a(j)
            iw(jcol) = ii
            ju(ii) = ju0
         else
            alu(ju0) = a(j)
            jlu(ju0) = ja(j)
            iw(jcol) = ju0
            ju0 = ju0 + 1
         end if
      end do
      jlu(ii + 1) = ju0
      jf = ju0 - 1
      jm = ju(ii) - 1
!     s accumulates fill-in values
      s = 0.0d0
      do j = js, jm
         jrow = jlu(j)
         tl = alu(j) * alu(jrow)
         alu(j) = tl
!-----------------------perform linear combination --------
         do jj = ju(jrow), jlu(jrow + 1) - 1
            jw = iw(jlu(jj))
            if (jw /= 0) then
               alu(jw) = alu(jw) - tl * alu(jj)
            else
               s = s + tl * alu(jj)
            end if
         end do
      end do
!----------------------- invert and store diagonal element.
      alu(ii) = alu(ii) - s
      if (alu(ii) == 0.0d0) goto 600
      alu(ii) = 1.0d0 / alu(ii)
!----------------------- reset pointer iw to zero
      iw(ii) = 0
      do i = js, jf
         iw(jlu(i)) = 0
      end do
   end do
   ierr = 0
   return
!     zero pivot :
600 ierr = ii
   return
end subroutine milu0

subroutine pgmres(n, im, rhs, sol, vv, eps, maxits, iout, aa, ja, ia, alu, jlu, ju, ierr)
   use precision_basics, only: dp

   implicit none

   integer :: n, im, maxits, iout, ierr, ja(*), ia(n + 1), jlu(*), ju(n)
   real(dp) :: vv(n, *), rhs(n), sol(n), aa(*), alu(*), eps
!----------------------------------------------------------------------*
!                                                                      *
!                 *** ILUT - Preconditioned GMRES ***                  *
!                                                                      *
!----------------------------------------------------------------------*
! This is a simple version of the ILUT preconditioned GMRES algorithm. *
! The ILUT preconditioner uses a dual strategy for dropping elements   *
! instead  of the usual level of-fill-in approach. See details in ILUT *
! subroutine documentation. PGMRES uses the L and U matrices generated *
! from the subroutine ILUT to precondition the GMRES algorithm.        *
! The preconditioning is applied to the right. The stopping criterion  *
! utilized is based simply on reducing the residual norm by epsilon.   *
! This preconditioning is more reliable than ilu0 but requires more    *
! storage. It seems to be much less prone to difficulties related to   *
! strong nonsymmetries in the matrix. We recommend using a nonzero tol *
! (tol=.005 or .001 usually give good results) in ILUT. Use a large    *
! lfil whenever possible (e.g. lfil = 5 to 10). The higher lfil the    *
! more reliable the code is. Efficiency may also be much improved.     *
! Note that lfil=n and tol=0.0 in ILUT  will yield the same factors as *
! Gaussian elimination without pivoting.                               *
!                                                                      *
! ILU(0) and MILU(0) are also provided for comparison purposes         *
! USAGE: first call ILUT or ILU0 or MILU0 to set up preconditioner and *
! then call pgmres.                                                    *
!----------------------------------------------------------------------*
! Coded by Y. Saad - This version dated May, 7, 1990.                  *
!----------------------------------------------------------------------*
! parameters                                                           *
!-----------                                                           *
! on entry:                                                            *
!==========                                                            *
!                                                                      *
! n     == integer. The dimension of the matrix.                       *
! im    == size of krylov subspace:  should not exceed 50 in this      *
!          version (can be reset by changing parameter command for     *
!          kmax below)                                                 *
! rhs   == real vector of length n containing the right hand side.     *
!          Destroyed on return.                                        *
! sol   == real vector of length n containing an initial guess to the  *
!          solution on input. approximate solution on output           *
! eps   == tolerance for stopping criterion. process is stopped        *
!          as soon as ( ||.|| is the euclidean norm):                  *
!          || current residual||/||initial residual|| <= eps           *
! maxits== maximum number of iterations allowed                        *
! iout  == output unit number number for printing intermediate results *
!          if (iout .le. 0) nothing is printed out.                    *
!                                                                      *
! aa, ja,                                                              *
! ia    == the input matrix in compressed sparse row format:           *
!          aa(1:nnz)  = nonzero elements of A stored row-wise in order *
!          ja(1:nnz) = corresponding column indices.                   *
!          ia(1:n+1) = pointer to beginning of each row in aa and ja.  *
!          here nnz = number of nonzero elements in A = ia(n+1)-ia(1)  *
!                                                                      *
! alu,jlu== A matrix stored in Modified Sparse Row format containing   *
!           the L and U factors, as computed by subroutine ilut.       *
!                                                                      *
! ju     == integer array of length n containing the pointers to       *
!           the beginning of each row of U in alu, jlu as computed     *
!           by subroutine ILUT.                                        *
!                                                                      *
! on return:                                                           *
!==========                                                            *
! sol   == contains an approximate solution (upon successful return).  *
! ierr  == integer. Error message with the following meaning.          *
!          ierr = 0 --> successful return.                             *
!          ierr = 1 --> convergence not achieved in itmax iterations.  *
!          ierr =-1 --> the initial guess seems to be the exact        *
!                       solution (initial residual computed was zero)  *
!                                                                      *
!----------------------------------------------------------------------*
!                                                                      *
! work arrays:                                                         *
!=============                                                         *
! vv    == work array of length  n x (im+1) (used to store the Arnoli  *
!          basis)                                                      *
!----------------------------------------------------------------------*
! subroutines called :                                                 *
! amux   : SPARSKIT routine to do the matrix by vector multiplication  *
!          delivers y=Ax, given x  -- see SPARSKIT/BLASSM/amux         *
! lusol : combined forward and backward solves (Preconditioning ope.) *
! BLAS1  routines.                                                     *
!----------------------------------------------------------------------*
   integer :: n1, its, j, i, i1, ii, jj, k, k1
   integer, parameter :: KMAX = 50
   real(dp) :: hh(KMAX + 1, KMAX), c(KMAX), s(KMAX), rs(KMAX + 1), t, ro, eps1, gam

   EXTERNAL_DNRM2
   EXTERNAL_DDOT
!-------------------------------------------------------------
! arnoldi size should not exceed kmax=50 in this version..
! to reset modify paramter kmax accordingly.
!-------------------------------------------------------------
   n1 = n + 1
   its = 0
!-------------------------------------------------------------
! outer loop starts here..
!-------------- compute initial residual vector --------------
   call amux(n, sol, vv, aa, ja, ia)
   do j = 1, n
      vv(j, 1) = rhs(j) - vv(j, 1)
   end do
!-------------------------------------------------------------
20 ro = dnrm2(n, vv, 1)
   if (iout > 0 .and. its == 0) write (iout, 199) its, ro
   if (ro == 0.0d0) goto 999
   t = 1.0d0 / ro
   do j = 1, n
      vv(j, 1) = vv(j, 1) * t
   end do
   if (its == 0) eps1 = eps * ro
!     ** initialize 1-st term  of rhs of hessenberg system..
   rs(1) = ro
   i = 0
4  i = i + 1
   its = its + 1
   i1 = i + 1
   call lusol(n, vv(1, i), rhs, alu, jlu, ju, 30 * n)
   call amux(n, rhs, vv(1, i1), aa, ja, ia)
!-----------------------------------------
!     modified gram - schmidt...
!-----------------------------------------
   do j = 1, i
      t = ddot(n, vv(1, j), 1, vv(1, i1), 1)
      hh(j, i) = t
      call daxpy(n, -t, vv(1, j), 1, vv(1, i1), 1)
   end do
   t = dnrm2(n, vv(1, i1), 1)
   hh(i1, i) = t
   if (t == 0.0d0) goto 58
   t = 1.0d0 / t
   do k = 1, n
      vv(k, i1) = vv(k, i1) * t
   end do
!
!     done with modified gram schimd and arnoldi step..
!     now  update factorization of hh
!
58 if (i == 1) goto 121
!--------perform previous transformations  on i-th column of h
   do k = 2, i
      k1 = k - 1
      t = hh(k1, i)
      hh(k1, i) = c(k1) * t + s(k1) * hh(k, i)
      hh(k, i) = -s(k1) * t + c(k1) * hh(k, i)
   end do
121 gam = sqrt(hh(i, i)**2 + hh(i1, i)**2)
!
!     if gamma is zero then any small value will do...
!     will affect only residual estimate
!
!      if (gam .eq. 0.0d0) gam = epsmac
!
!     get  next plane rotation
!
   c(i) = hh(i, i) / gam
   s(i) = hh(i1, i) / gam
   rs(i1) = -s(i) * rs(i)
   rs(i) = c(i) * rs(i)
!
!     detrermine residual norm and test for convergence-
!
   hh(i, i) = c(i) * hh(i, i) + s(i) * hh(i1, i)
   ro = abs(rs(i1))
   if (iout > 0) write (iout, 199) its, ro
   if (i < im .and. (ro > eps1)) goto 4
!
!     now compute solution. first solve upper triangular system.
!
   rs(i) = rs(i) / hh(i, i)
   do ii = 2, i
      k = i - ii + 1
      k1 = k + 1
      t = rs(k)
      do j = k1, i
         t = t - hh(k, j) * rs(j)
      end do
      rs(k) = t / hh(k, k)
   end do
!
!     form linear combination of v(*,i)'s to get solution
!
   t = rs(1)
   do k = 1, n
      rhs(k) = vv(k, 1) * t
   end do
   do j = 2, i
      t = rs(j)
      do k = 1, n
         rhs(k) = rhs(k) + t * vv(k, j)
      end do
   end do
!
!     call preconditioner.
!
   call lusol(n, rhs, rhs, alu, jlu, ju, 30 * n)
   do k = 1, n
      sol(k) = sol(k) + rhs(k)
   end do
!
!     restart outer loop  when necessary
!
   if (ro <= eps1) goto 990
   if (its >= maxits) goto 991
!
!     else compute residual vector and continue..
!
   do j = 1, i
      jj = i1 - j + 1
      rs(jj - 1) = -s(jj - 1) * rs(jj)
      rs(jj) = c(jj - 1) * rs(jj)
   end do
   do j = 1, i1
      t = rs(j)
      if (j == 1) t = t - 1.0d0
      call daxpy(n, t, vv(1, j), 1, vv, 1)
   end do
199 format('   its =', i4, ' res. norm =', d20.6)
!     restart outer loop.
   goto 20
990 ierr = 0
   return
991 ierr = 1
   return
999 continue
   ierr = -1
   return
end subroutine pgmres

subroutine qsplit(a, ind, n, ncut)
   use precision_basics, only: dp

   implicit none

   real(dp) :: a(n)
   integer :: ind(n), n, ncut
!-----------------------------------------------------------------------
!     does a quick-sort split of a real array.
!     on input a(1:n). is a real array
!     on output a(1:n) is permuted such that its elements satisfy:
!
!     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
!     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
!
!     ind(1:n) is an integer array which permuted in the same way as a(*).
!-----------------------------------------------------------------------
   real(dp) :: tmp, abskey
   integer itmp, first, last, mid, j

   first = 1
   last = n
   if (ncut < first .or. ncut > last) return
!
!     outer loop -- while mid .ne. ncut do
!
1  mid = first
   abskey = abs(a(mid))
   do j = first + 1, last
      if (abs(a(j)) > abskey) then
         mid = mid + 1
!     interchange
         tmp = a(mid)
         itmp = ind(mid)
         a(mid) = a(j)
         ind(mid) = ind(j)
         a(j) = tmp
         ind(j) = itmp
      end if
   end do
!
!     interchange
!
   tmp = a(mid)
   a(mid) = a(first)
   a(first) = tmp
!
   itmp = ind(mid)
   ind(mid) = ind(first)
   ind(first) = itmp
!
!     test for while loop
!
   if (mid == ncut) return
   if (mid > ncut) then
      last = mid - 1
   else
      first = mid + 1
   end if
   goto 1
end subroutine qsplit

subroutine runrc2(n, rhs, sol, ipar, fpar, wk, a, ja, ia, au, jau, ju, its, eps, jabcgstab, ierror, nau)
   implicit none
   integer n, ipar(16), ia(n + 1), ja(5 * n), ju(n), jau(nau), jabcgstab, nau
   double precision :: fpar(16), rhs(n), sol(n), wk(2 * nau), a(5 * n), au(nau), eps
   integer :: ierror !< error (1) or not (0)

!-----------------------------------------------------------------------
!     the actual tester. It starts the iterative linear system solvers
!     with a initial guess suppied by the user.
!
!     The structure {au, jau, ju} is assumed to have the output from
!     the ILU* routines in ilut.f.
!
!-----------------------------------------------------------------------
!     local variables
!
   integer iou, its
   double precision :: res
!     real dtime, dt(2), time
!     external dtime
   EXTERNAL_DNRM2
   save res

   no_warning_unused_dummy_argument(eps)

!
!     ipar(2) can be 0, 1, 2, please don't use 3
!
   if (ipar(2) > 2) then
      print *, 'I can not do both left and right preconditioning.'
      return
   end if
!
!     normal execution
!
   its = 0
   res = 0.0d0
!
   ! do i = 1, n
   !   sol(i) = guess(i)
   ! enddo
!
   iou = 6
   ipar(1) = 0

   ! time = dtime(dt)
   ! call klok(cp0)

10 if (jabcgstab == 1) then
      call BCGSTAB(n, rhs, sol, ipar, fpar, wk)
   else
      call CG(n, rhs, sol, ipar, fpar, wk)
   end if

!
!     output the residuals
!
   if (ipar(7) /= its) then
      !        write (iou, *) its, real(res)
      its = ipar(7)
   end if
   res = fpar(5)
!
   if (ipar(1) == 1) then
      call amux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
      goto 10
   else if (ipar(1) == 2) then
      call atmux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
      goto 10
   else if (ipar(1) == 3 .or. ipar(1) == 5) then
      call lusol(n, wk(ipar(8)), wk(ipar(9)), au, jau, ju, nau)
      goto 10
   else if (ipar(1) == 4 .or. ipar(1) == 6) then
      call lutsol(n, wk(ipar(8)), wk(ipar(9)), au, jau, ju)
      goto 10
   else if (ipar(1) <= 0) then
      if (ipar(1) == 0) then
         !         print *, 'Iterative sovler has satisfied convergence test.'
      else if (ipar(1) == -1) then
         print *, 'Iterative solver has iterated too many times.'
      else if (ipar(1) == -2) then
         print *, 'Iterative solver was not given enough work space.'
         print *, 'The work space should at least have ', ipar(4), ' elements.'
      else if (ipar(1) == -3) then
         print *, 'Iterative sovler is facing a break-down. a'
      else
         print *, 'Iterative solver terminated. code =', ipar(1)
      end if
   end if

   ! time = dtime(dt)
   ! write (iou, *) ipar(7), real(fpar(6))

   if (ipar(1) /= 0) then
!         write (iou, *) '# return code =', ipar(1),    '   convergence rate =', fpar(7)
      ierror = 1
   else
      ierror = 0
   end if
   ! write (iou, *) '# total execution time (sec)', time

   ! check the error

   ! call amux(n,sol,wk,a,ja,ia)

   ! eps = 0d0
   ! do i = 1, n
   !    wk(n+i) = sol(i) -1.0D0
   !    wk(i) = wk(i) - rhs(i)
   !    eps   = max(eps, abs(wk(i)) )
   ! enddo

   ! write (iou, *) '# the actual residual norm is', dnrm2(n,wk,1)
   ! write (iou, *) '# the error norm is', dnrm2(n,wk(1+n),1)

   !     WRITE (*,'(A,I4)') 'nbr of iterations =', its

   if (iou /= 6) close (iou)
   return
end subroutine runrc2

subroutine runrc(n, rhs, sol, sol0, ipar, fpar, wk, guess, a, ja, ia, au, jau, ju, solver)
   implicit none
   integer n, ipar(16), ia(n + 1), ja(5 * n), ju(n), jau(30 * n)
   double precision :: fpar(16), rhs(n), sol(n), sol0(n), guess(n), wk(2 * 30 * n), a(5 * n), au(30 * n), cp0, cp1

   external solver
!-----------------------------------------------------------------------
!     the actual tester. It starts the iterative linear system solvers
!     with a initial guess suppied by the user.
!
!     The structure {au, jau, ju} is assumed to have the output from
!     the ILU* routines in ilut.f.
!
!-----------------------------------------------------------------------
!     local variables
!
   integer i, iou, its
   double precision :: res
!     real dtime, dt(2), time
!     external dtime
   EXTERNAL_DNRM2
   save its, res
!
!     ipar(2) can be 0, 1, 2, please don't use 3
!
   if (ipar(2) > 2) then
      print *, 'I can not do both left and right preconditioning.'
      return
   end if
!
!     normal execution
!
   its = 0
   res = 0.0d0
!
   do i = 1, n
      sol(i) = guess(i)
   end do
   SOL = 0

!
   iou = 6
   ipar(1) = 0
!      time = dtime(dt)
   call klok(cp0)
10 call solver(n, rhs, sol, ipar, fpar, wk)

!
!     output the residuals
!
   if (ipar(7) /= its) then
      !        write (iou, *) its, real(res)
      its = ipar(7)
   end if
   res = fpar(5)
!
   if (ipar(1) == 1) then
      call amux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
      goto 10
   else if (ipar(1) == 2) then
      call atmux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
      goto 10
   else if (ipar(1) == 3 .or. ipar(1) == 5) then
      call lusol(n, wk(ipar(8)), wk(ipar(9)), au, jau, ju, 30 * n)
      goto 10
   else if (ipar(1) == 4 .or. ipar(1) == 6) then
      call lutsol(n, wk(ipar(8)), wk(ipar(9)), au, jau, ju)
      goto 10
   else if (ipar(1) <= 0) then
      if (ipar(1) == 0) then
         !         print *, 'Iterative sovler has satisfied convergence test.'
      else if (ipar(1) == -1) then
         print *, 'Iterative solver has iterated too many times.'
      else if (ipar(1) == -2) then
         print *, 'Iterative solver was not given enough work space.'
         print *, 'The work space should at least have ', ipar(4), ' elements.'
      else if (ipar(1) == -3) then
         print *, 'Iterative sovler is facing a break-down. b'
      else
         print *, 'Iterative solver terminated. code =', ipar(1)
      end if
   end if
!     time = dtime(dt)
!      write (iou, *) ipar(7), real(fpar(6))

   if (ipar(1) /= 0) then
      write (iou, *) '# retrun code =', ipar(1), ' convergence rate =', fpar(7)
   end if
   !   write (iou, *) '# total execution time (sec)', time
!
!     check the error
!

   call amux(n, sol, wk, a, ja, ia)
   do i = 1, n
      wk(n + i) = sol(i) - 1.0d0
      wk(i) = wk(i) - rhs(i)
   end do
!      write (iou, *) '# the actual residual norm is', dnrm2(n,wk,1)
!      write (iou, *) '# the error norm is', dnrm2(n,wk(1+n),1)
!

   call KLOK(CP1)
   write (*, '(A,F8.4,A,I4)') ' CPU time =', cp1 - cp0, &
      '   nbr of iterations =', its
   call watisdefout(n, sol, sol0)

   if (iou /= 6) close (iou)
   return
end subroutine runrc

subroutine watisdefout(n, sol, sol0)
   implicit none

   integer :: n
   double precision :: sol(n), sol0(n)

   double precision :: errmx, errav
   errmx = maxval(abs(sol - sol0))
   errav = sum(abs(sol - sol0))
   errav = errav / n
   write (*, *) 'ermx, errav ', errmx, errav
end subroutine watisdefout

function distdot(n, x, ix, y, iy)
   integer n, ix, iy
   double precision :: distdot, x(*), y(*)
   EXTERNAL_DDOT
   distdot = ddot(n, x, ix, y, iy)
   return
end function distdot

function afun(x, y, z)
   double precision :: afun, x, y, z
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   afun = -1.0d0
   return
end function afun

function bfun(x, y, z)
   double precision :: bfun, x, y, z
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   bfun = -1.0d0
   return
end function bfun

function cfun(x, y, z)
   double precision :: cfun, x, y, z
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   cfun = -1.0d0
   return
end function cfun

function dfun(x, y, z)
   use GAMMAS
   double precision :: dfun, x, y, z
   no_warning_unused_dummy_argument(z)
   dfun = gammax * exp(x * y)
   return
end function dfun

function efun(x, y, z)
   use GAMMAS
   double precision :: efun, x, y, z
   no_warning_unused_dummy_argument(z)
   efun = gammay * exp(-x * y)
   return
end function efun

function ffun(x, y, z)
   double precision :: ffun, x, y, z
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   ffun = 0.0d0
   return
end function ffun

function gfun(x, y, z)
   use GAMMAS

   double precision :: gfun, x, y, z
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   gfun = alpha
   return
end function gfun

function hfun(x, y, z)
   use GAMMAS

   double precision :: hfun, x, y, z
   hfun = alpha * sin(gammax * x + gammay * y - z)
   return
end function hfun

function betfun(side, x, y, z)
   double precision :: betfun, x, y, z
   character(len=2) side
   no_warning_unused_dummy_argument(side)
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   betfun = 1.0
   return
end function betfun

function gamfun(side, x, y, z)
   double precision :: gamfun, x, y, z
   character(len=2) side
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   if (side == 'x2') then
      gamfun = 5.0
   else if (side == 'y1') then
      gamfun = 2.0
   else if (side == 'y2') then
      gamfun = 7.0
   else
      gamfun = 0.0
   end if
   return
end function gamfun

!----------------------------------------------------------------------c
!                          S P A R S K I T                             c
!----------------------------------------------------------------------c
!         Basic Iterative Solvers with Reverse Communication           c
!----------------------------------------------------------------------c
!     This file currently has several basic iterative linear system    c
!     solvers. They are:                                               c
!     CG       -- Conjugate Gradient Method                            c
!     CGNR     -- Conjugate Gradient Method (Normal Residual equation) c
!     BCG      -- Bi-Conjugate Gradient Method                         c
!     DBCG     -- BCG with partial pivoting                            c
!     BCGSTAB  -- BCG stabilized                                       c
!     TFQMR    -- Transpose-Free Quasi-Minimum Residual method         c
!     FOM      -- Full Orthogonalization Method                        c
!     GMRES    -- Generalized Minimum RESidual method                  c
!     FGMRES   -- Flexible version of Generalized Minimum              c
!                 RESidual method                                      c
!     DQGMRES  -- Direct versions of Quasi Generalize Minimum          c
!                 Residual method                                      c
!----------------------------------------------------------------------c
!     They all have the following calling sequence:
!      subroutine solver(n, rhs, sol, ipar, fpar, w)
!      integer n, ipar(16)
!      double precision ::  rhs(n), sol(n), fpar(16), w(*)
!     Where
!     (1) 'n' is the size of the linear system,
!     (2) 'rhs' is the right-hand side of the linear system,
!     (3) 'sol' is the solution to the linear system,
!     (4) 'ipar' is an integer parameter array for the reverse
!     communication protocol,
!     (5) 'fpar' is an floating-point parameter array storing
!     information to and from the iterative solvers.
!     (6) 'w' is the work space (size is specified in ipar)
!
!     They are preconditioned iterative solvers with reverse
!     communication. The preconditioners can be applied from either
!     from left or right or both (specified by ipar(2), see below).
!
!     Author: Kesheng John Wu (kewu@mail.cs.umn.edu) 1993
!
!     NOTES:
!
!     (1) Work space required by each of the iterative solver
!     routines is as follows:
!       CG      == 5 * n
!       CGNR    == 5 * n
!       BCG     == 7 * n
!       DBCG    == 11 * n
!       BCGSTAB == 8 * n
!       TFQMR   == 11 * n
!       FOM     == (n+3)*(m+2) + (m+1)*m/2 (m = ipar(5), default m=15)
!       GMRES   == (n+3)*(m+2) + (m+1)*m/2 (m = ipar(5), default m=15)
!       FGMRES  == 2*n*(m+1) + (m+1)*m/2 + 3*m + 2 (m = ipar(5),
!                  default m=15)
!       DQGMRES == n + lb * (2*n+4) (lb=ipar(5)+1, default lb = 16)
!
!     (2) ALL iterative solvers require a user-supplied DOT-product
!     routine named DISTDOT. The prototype of DISTDOT is
!
!     double precision ::  function distdot(n,x,ix,y,iy)
!     integer n, ix, iy
!     double precision ::  x(1+(n-1)*ix), y(1+(n-1)*iy)
!
!     This interface of DISTDOT is exactly the same as that of
!     DDOT (or SDOT if real == double precision :: ) from BLAS-1. It should have
!     same functionality as DDOT on a single processor machine. On a
!     parallel/distributed environment, each processor can perform
!     DDOT on the data it has, then perform a summation on all the
!     partial results.
!
!     (3) To use this set of routines under SPMD/MIMD program paradigm,
!     several things are to be noted: (a) 'n' should be the number of
!     vector elements of 'rhs' that is present on the local processor.
!     (b) if RHS(i) is on processor j, it is expected that SOL(i)
!     will be on the same processor, i.e. the vectors are distributed
!     to each processor in the same way. (c) the preconditioning and
!     stopping criteria specifications have to be the same on all
!     processor involved, ipar and fpar have to be the same on each
!     processor. (d) DISTDOT should be replaced by a distributed
!     dot-product function.
!
!     ..................................................................
!     Reverse Communication Protocols
!
!     When a reverse-communication routine returns, it could be either
!     that the routine has terminated or it simply requires the caller
!     to perform one matrix-vector multiplication. The possible matrices
!     that involve in the matrix-vector multiplications are:
!     A       (the matrix of the linear system),
!     A^T     (A transposed),
!     Ml^{-1} (inverse of the left preconditioner),
!     Ml^{-T} (inverse of the left preconditioner transposed),
!     Mr^{-1} (inverse of the right preconditioner),
!     Mr^{-T} (inverse of the right preconditioner transposed).
!     For all the matrix vector multiplication, v = A u. The input and
!     output vectors are supposed to be part of the work space 'w', and
!     the starting positions of them are stored in ipar(8:9), see below.
!
!     The array 'ipar' is used to store the information about the solver.
!     Here is the list of what each element represents:
!
!     ipar(1) -- status of the call/return.
!     A call to the solver with ipar(1) == 0 will initialize the
!     iterative solver. On return from the iterative solver, ipar(1)
!     carries the status flag which indicates the condition of the
!     return. The status information is divided into two categories,
!     (1) a positive value indicates the solver requires a matrix-vector
!     multiplication,
!     (2) a non-positive value indicates termination of the solver.
!     Here is the current definition:
!       1 == request a matvec with A,
!       2 == request a matvec with A^T,
!       3 == request a left preconditioner solve (Ml^{-1}),
!       4 == request a left preconditioner transposed solve (Ml^{-T}),
!       5 == request a right preconditioner solve (Mr^{-1}),
!       6 == request a right preconditioner transposed solve (Mr^{-T}),
!      10 == request the caller to perform stopping test,
!       0 == normal termination of the solver, satisfied the stopping
!            criteria,
!      -1 == termination because iteration number is greater than the
!            preset limit,
!      -2 == return due to insufficient work space,
!      -3 == return due to anticipated break-down / divide by zero,
!            in the case where Arnoldi procedure is used, additional
!            error code can be found in ipar(12), where ipar(12) is
!            the error code of orthogonalization procedure MGSRO:
!               -1: zero input vector
!               -2: input vector contains abnormal numbers
!               -3: input vector is a linear combination of others
!               -4: trianguler system in GMRES/FOM/etc. has nul rank
!      -4 == the values of fpar(1) and fpar(2) are both <= 0, the valid
!            ranges are 0 <= fpar(1) < 1, 0 <= fpar(2), and they can
!            not be zero at the same time
!      -9 == while trying to detect a break-down, an abnormal number is
!            detected.
!     -10 == return due to some non-numerical reasons, e.g. invalid
!            floating-point numbers etc.
!
!     ipar(2) -- status of the preconditioning:
!       0 == no preconditioning
!       1 == left preconditioning only
!       2 == right preconditioning only
!       3 == both left and right preconditioning
!
!     ipar(3) -- stopping criteria (details of this will be
!     discussed later).
!
!     ipar(4) -- number of elements in the array 'w'. if this is less
!     than the desired size, it will be over-written with the minimum
!     requirement. In which case the status flag ipar(1) = -2.
!
!     ipar(5) -- size of the Krylov subspace (used by GMRES and its
!     variants), e.g. GMRES(ipar(5)), FGMRES(ipar(5)),
!     DQGMRES(ipar(5)).
!
!     ipar(6) -- maximum number of matrix-vector multiplies, if not a
!     positive number the iterative solver will run till convergence
!     test is satisfied.
!
!     ipar(7) -- current number of matrix-vector multiplies. It is
!     incremented after each matrix-vector multiplication. If there
!     is preconditioning, the counter is incremented after the
!     preconditioning associated with each matrix-vector multiplication.
!
!     ipar(8) -- pointer to the input vector to the requested matrix-
!     vector multiplication.
!
!     ipar(9) -- pointer to the output vector of the requested matrix-
!     vector multiplication.
!
!     To perform v = A * u, it is assumed that u is w(ipar(8):ipar(8)+n-1)
!     and v is stored as w(ipar(9):ipar(9)+n-1).
!
!     ipar(10) -- the return address (used to determine where to go to
!     inside the iterative solvers after the caller has performed the
!     requested services).
!
!     ipar(11) -- the result of the external convergence test
!     On final return from the iterative solvers, this value
!     will be reflected by ipar(1) = 0 (details discussed later)
!
!     ipar(12) -- error code of MGSRO, it is
!                  1 if the input vector to MGSRO is linear combination
!                    of others,
!                  0 if MGSRO was successful,
!                 -1 if the input vector to MGSRO is zero,
!                 -2 if the input vector contains invalid number.
!
!     ipar(13) -- number of initializations. During each initilization
!                 residual norm is computed directly from M_l(b - A x).
!
!     ipar(14) to ipar(16) are NOT defined, they are NOT USED by
!     any iterative solver at this time.
!
!     Information about the error and tolerance are stored in the array
!     FPAR. So are some internal variables that need to be saved from
!     one iteration to the next one. Since the internal variables are
!     not the same for each routine, we only define the common ones.
!
!     The first two are input parameters:
!     fpar(1) -- the relative tolerance,
!     fpar(2) -- the absolute tolerance (details discussed later),
!
!     When the iterative solver terminates,
!     fpar(3) -- initial residual/error norm,
!     fpar(4) -- target residual/error norm,
!     fpar(5) -- current residual norm (if available),
!     fpar(6) -- current residual/error norm,
!     fpar(7) -- convergence rate,
!
!     fpar(8:10) are used by some of the iterative solvers to save some
!     internal information.
!
!     fpar(11) -- number of floating-point operations. The iterative
!     solvers will add the number of FLOPS they used to this variable,
!     but they do NOT initialize it, nor add the number of FLOPS due to
!     matrix-vector multiplications (since matvec is outside of the
!     iterative solvers). To insure the correct FLOPS count, the
!     caller should set fpar(11) = 0 before invoking the iterative
!     solvers and account for the number of FLOPS from matrix-vector
!     multiplications and preconditioners.
!
!     fpar(12:16) are not used in current implementation.
!
!     Whether the content of fpar(3), fpar(4) and fpar(6) are residual
!     norms or error norms depends on ipar(3). If the requested
!     convergence test is based on the residual norm, they will be
!     residual norms. If the caller want to test convergence based the
!     error norms (estimated by the norm of the modifications applied
!     to the approximate solution), they will be error norms.
!     Convergence rate is defined by (Fortran 77 statement)
!     fpar(7) = log10(fpar(3) / fpar(6)) / (ipar(7)-ipar(13))
!     If fpar(7) = 0.5, it means that approximately every 2 (= 1/0.5)
!     steps the residual/error norm decrease by a factor of 10.
!
!     ..................................................................
!     Stopping criteria,
!
!     An iterative solver may be terminated due to (1) satisfying
!     convergence test; (2) exceeding iteration limit; (3) insufficient
!     work space; (4) break-down. Checking of the work space is
!     only done in the initialization stage, i.e. when it is called with
!     ipar(1) == 0. A complete convergence test is done after each
!     update of the solutions. Other conditions are monitored
!     continuously.
!
!     With regard to the number of iteration, when ipar(6) is positive,
!     the current iteration number will be checked against it. If
!     current iteration number is greater the ipar(6) than the solver
!     will return with status -1. If ipar(6) is not positive, the
!     iteration will continue until convergence test is satisfied.
!
!     Two things may be used in the convergence tests, one is the
!     residual 2-norm, the other one is 2-norm of the change in the
!     approximate solution. The residual and the change in approximate
!     solution are from the preconditioned system (if preconditioning
!     is applied). The DQGMRES and TFQMR use two estimates for the
!     residual norms. The estimates are not accurate, but they are
!     acceptable in most of the cases. Generally speaking, the error
!     of the TFQMR's estimate is less accurate.
!
!     The convergence test type is indicated by ipar(3). There are four
!     type convergence tests: (1) tests based on the residual norm;
!     (2) tests based on change in approximate solution; (3) caller
!     does not care, the solver choose one from above two on its own;
!     (4) caller will perform the test, the solver should simply continue.
!     Here is the complete definition:
!      -2 == || dx(i) || <= rtol * || rhs || + atol
!      -1 == || dx(i) || <= rtol * || dx(1) || + atol
!       0 == solver will choose test 1 (next)
!       1 == || residual || <= rtol * || initial residual || + atol
!       2 == || residual || <= rtol * || rhs || + atol
!     999 == caller will perform the test
!     where dx(i) denote the change in the solution at the ith update.
!     ||.|| denotes 2-norm. rtol = fpar(1) and atol = fpar(2).
!
!     If the caller is to perform the convergence test, the outcome
!     should be stored in ipar(11).
!     ipar(11) = 0 -- failed the convergence test, iterative solver
!     should continue
!     ipar(11) = 1 -- satisfied convergence test, iterative solver
!     should perform the clean up job and stop.
!
!     Upon return with ipar(1) = 10,
!     ipar(8)  points to the starting position of the change in
!              solution Sx, where the actual solution of the step is
!              x_j = x_0 + M_r^{-1} Sx.
!              Exception: ipar(8) < 0, Sx = 0. It is mostly used by
!              GMRES and variants to indicate (1) Sx was not necessary,
!              (2) intermediate result of Sx is not computed.
!     ipar(9)  points to the starting position of a work vector that
!              can be used by the caller.
!
!     NOTE: the caller should allow the iterative solver to perform
!     clean up job after the external convergence test is satisfied,
!     since some of the iterative solvers do not directly
!     update the 'sol' array. A typical clean-up stage includes
!     performing the final update of the approximate solution and
!     computing the convergence information (e.g. values of fpar(3:7)).
!
!     NOTE: fpar(4) and fpar(6) are not set by the accelerators (the
!     routines implemented here) if ipar(3) = 999.
!
!     ..................................................................
!     Usage:
!
!     To start solving a linear system, the user needs to specify
!     first 6 elements of the ipar, and first 2 elements of fpar.
!     The user may optionally set fpar(11) = 0 if one wants to count
!     the number of floating-point operations. (Note: the iterative
!     solvers will only add the floating-point operations inside
!     themselves, the caller will have to add the FLOPS from the
!     matrix-vector multiplication routines and the preconditioning
!     routines in order to account for all the arithmetic operations.)
!
!     Here is an example:
!     ipar(1) = 0 ! always 0 to start an iterative solver
!     ipar(2) = 2 ! right preconditioning
!     ipar(3) = 1 ! use convergence test scheme 1
!     ipar(4) = 10000   ! the 'w' has 10,000 elements
!     ipar(5) = 10   ! use *GMRES(10) (e.g. FGMRES(10))
!     ipar(6) = 100  ! use at most 100 matvec's
!     fpar(1) = 1.0E-6  ! relative tolerance 1.0E-6
!     fpar(2) = 1.0E-10 ! absolute tolerance 1.0E-10
!     fpar(11) = 0.0 ! clearing the FLOPS counter
!
!     After the above specifications, one can start to call an iterative
!     solver, say BCG. Here is a piece of pseudo-code showing how it can
!     be done,
!
! 10   call bcg(n,rhs,sol,ipar,fpar,w)
!      if (ipar(1).eq.1) then
!         call amux(n,w(ipar(8)),w(ipar(9)),a,ja,ia)
!         goto 10
!      else if (ipar(1).eq.2) then
!         call atmux(n,w(ipar(8)),w(ipar(9)),a,ja,ia)
!         goto 10
!      else if (ipar(1).eq.3) then
!         left preconditioner solver
!         goto 10
!      else if (ipar(1).eq.4) then
!         left preconditioner transposed solve
!         goto 10
!      else if (ipar(1).eq.5) then
!         right preconditioner solve
!         goto 10
!      else if (ipar(1).eq.6) then
!         right preconditioner transposed solve
!         goto 10
!      else if (ipar(1).eq.10) then
!         call my own stopping test routine
!         goto 10
!      else if (ipar(1).gt.0) then
!         ipar(1) is an unspecified code
!      else
!         the iterative solver terminated with code = ipar(1)
!      endif
!
!     This segment of pseudo-code assumes the matrix is in CSR format,
!     AMUX and ATMUX are two routines from the SPARSKIT MATVEC module.
!     They perform matrix-vector multiplications for CSR matrices,
!     where w(ipar(8)) is the first element of the input vectors to the
!     two routines, and w(ipar(9)) is the first element of the output
!     vectors from them. For simplicity, we did not show the name of
!     the routine that performs the preconditioning operations or the
!     convergence tests.
!-----------------------------------------------------------------------
subroutine cg(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(n, *)
!-----------------------------------------------------------------------
!     This is a implementation of the Conjugate Gradient (CG) method
!     for solving linear system.
!
!     NOTE: This is not the PCG algorithm. It is a regular CG algorithm.
!     To be consistent with the other solvers, the preconditioners are
!     applied by performing Ml^{-1} A Mr^{-1} P in place of A P in the
!     CG algorithm.  PCG uses the preconditioner differently.
!
!     fpar(7) is used here internally to store <r, r>.
!     w(:,1) -- residual vector
!     w(:,2) -- P, the conjugate direction
!     w(:,3) -- A P, matrix multiply the conjugate direction
!     w(:,4) -- temporary storage for results of preconditioning
!     w(:,5) -- change in the solution (sol) is stored here until
!               termination of this solver
!-----------------------------------------------------------------------
!     external functions used
!
   logical stopbis, brkdn
   external stopbis, brkdn, bisinit
   EXTERNAL_DDOT
!
!     local variables
!
   integer i
   double precision :: alpha
   logical lp, rp
   save
!
!     check the status of the call
!
   if (ipar(1) <= 0) ipar(10) = 0

   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 40
   else if (ipar(10) == 4) then
      goto 50
   else if (ipar(10) == 5) then
      goto 60
   else if (ipar(10) == 6) then
      goto 70
   else if (ipar(10) == 7) then
      goto 80
   end if

!
!     initialization
!
   call bisinit(ipar, fpar, 5 * n, 1, lp, rp, w)
   if (ipar(1) < 0) return
!
!     request for matrix vector multiplication A*x in the initialization
!
   ipar(1) = 1
   ipar(8) = n + 1
   ipar(9) = ipar(8) + n
   ipar(10) = 1
   do i = 1, n
      w(i, 2) = sol(i)
   end do
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = 1
   do i = 1, n
      w(i, 2) = rhs(i) - w(i, 3)
   end do
   fpar(11) = fpar(11) + n
!
!     if left preconditioned
!
   if (lp) then
      ipar(1) = 3
      ipar(9) = 1
      ipar(10) = 2
      return
   end if
!
20 if (lp) then
      do i = 1, n
         w(i, 2) = w(i, 1)
      end do
   else
      do i = 1, n
         w(i, 1) = w(i, 2)
      end do
   end if
!
   fpar(7) = ddot(n, w, 1, w, 1)
   fpar(11) = fpar(11) + 2 * n
   fpar(3) = sqrt(fpar(7))
   fpar(5) = fpar(3)
   if (abs(ipar(3)) == 2) then
      fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
      fpar(11) = fpar(11) + 2 * n
   else if (ipar(3) /= 999) then
      fpar(4) = fpar(1) * fpar(3) + fpar(2)
   end if
!
!     before iteration can continue, we need to compute A * p, which
!     includes the preconditioning operations
!
30 if (rp) then
      ipar(1) = 5
      ipar(8) = n + 1
      if (lp) then
         ipar(9) = ipar(8) + n
      else
         ipar(9) = 3 * n + 1
      end if
      ipar(10) = 3
      return
   end if
!
40 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = n + 1
   end if
   if (lp) then
      ipar(9) = 3 * n + 1
   else
      ipar(9) = n + n + 1
   end if
   ipar(10) = 4
   return
!
50 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = n + n + 1
      ipar(10) = 5
      return
   end if
!
!     continuing with the iterations
!
60 ipar(7) = ipar(7) + 1
   alpha = ddot(n, w(1, 2), 1, w(1, 3), 1)
   fpar(11) = fpar(11) + 2 * n
   if (brkdn(alpha, ipar)) goto 900
   alpha = fpar(7) / alpha
   do i = 1, n
      w(i, 5) = w(i, 5) + alpha * w(i, 2)
      w(i, 1) = w(i, 1) - alpha * w(i, 3)
   end do
   fpar(11) = fpar(11) + 4 * n
!
!     are we ready to terminate ?
!
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = 4 * n + 1
      ipar(9) = 3 * n + 1
      ipar(10) = 6
      return
   end if
70 if (ipar(3) == 999) then
      if (ipar(11) == 1) goto 900
   else if (stopbis(n, ipar, 1, fpar, w, w(1, 2), alpha)) then
      goto 900
   end if
!
!     continue the iterations
!
   alpha = fpar(5) * fpar(5) / fpar(7)
   fpar(7) = fpar(5) * fpar(5)
   do i = 1, n
      w(i, 2) = w(i, 1) + alpha * w(i, 2)
   end do
   fpar(11) = fpar(11) + 2 * n
   goto 30
!
!     clean up -- necessary to accommodate the right-preconditioning
!
900 if (rp) then
      if (ipar(1) < 0) ipar(12) = ipar(1)
      ipar(1) = 5
      ipar(8) = 4 * n + 1
      ipar(9) = ipar(8) - n
      ipar(10) = 7
      return
   end if
80 if (rp) then
      call tidycg(n, ipar, fpar, sol, w(1, 4))
   else
      call tidycg(n, ipar, fpar, sol, w(1, 5))
   end if
!
   return
end subroutine cg

subroutine cgnr(n, rhs, sol, ipar, fpar, wk)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), wk(n, *)
!-----------------------------------------------------------------------
!     CGNR -- Using CG algorithm solving A x = b by solving
!     Normal Residual equation: A^T A x = A^T b
!     As long as the matrix is not singular, A^T A is symmetric
!     positive definite, therefore CG (CGNR) will converge.
!
!     Usage of the work space:
!     wk(:,1) == residual vector R
!     wk(:,2) == the conjugate direction vector P
!     wk(:,3) == a scratch vector holds A P, or A^T R
!     wk(:,4) == a scratch vector holds intermediate results of the
!                preconditioning
!     wk(:,5) == a place to hold the modification to SOL
!
!     size of the work space WK is required = 5*n
!-----------------------------------------------------------------------
!     external functions used
!
   logical stopbis, brkdn
   external stopbis, brkdn, bisinit
   EXTERNAL_DDOT
!
!     local variables
!
   integer i
   double precision :: alpha, zz, zzm1
   logical lp, rp
   save
!
!     check the status of the call
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 40
   else if (ipar(10) == 4) then
      goto 50
   else if (ipar(10) == 5) then
      goto 60
   else if (ipar(10) == 6) then
      goto 70
   else if (ipar(10) == 7) then
      goto 80
   else if (ipar(10) == 8) then
      goto 90
   else if (ipar(10) == 9) then
      goto 100
   else if (ipar(10) == 10) then
      goto 110
   end if
!
!     initialization
!
   call bisinit(ipar, fpar, 5 * n, 1, lp, rp, wk)
   if (ipar(1) < 0) return
!
!     request for matrix vector multiplication A*x in the initialization
!
   ipar(1) = 1
   ipar(8) = 1
   ipar(9) = 1 + n
   ipar(10) = 1
   do i = 1, n
      wk(i, 1) = sol(i)
   end do
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   do i = 1, n
      wk(i, 1) = rhs(i) - wk(i, 2)
   end do
   fpar(11) = fpar(11) + n
!
!     if left preconditioned, precondition the initial residual
!
   if (lp) then
      ipar(1) = 3
      ipar(10) = 2
      return
   end if
!
20 if (lp) then
      do i = 1, n
         wk(i, 1) = wk(i, 2)
      end do
   end if
!
   zz = ddot(n, wk, 1, wk, 1)
   fpar(11) = fpar(11) + 2 * n
   fpar(3) = sqrt(zz)
   fpar(5) = fpar(3)
   if (abs(ipar(3)) == 2) then
      fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
      fpar(11) = fpar(11) + 2 * n
   else if (ipar(3) /= 999) then
      fpar(4) = fpar(1) * fpar(3) + fpar(2)
   end if
!
!     normal iteration begins here, first half of the iteration
!     computes the conjugate direction
!
30 continue
!
!     request the caller to perform a A^T r --> wk(:,3)
!
   if (lp) then
      ipar(1) = 4
      ipar(8) = 1
      if (rp) then
         ipar(9) = n + n + 1
      else
         ipar(9) = 3 * n + 1
      end if
      ipar(10) = 3
      return
   end if
!
40 ipar(1) = 2
   if (lp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = 1
   end if
   if (rp) then
      ipar(9) = 3 * n + 1
   else
      ipar(9) = n + n + 1
   end if
   ipar(10) = 4
   return
!
50 if (rp) then
      ipar(1) = 6
      ipar(8) = ipar(9)
      ipar(9) = n + n + 1
      ipar(10) = 5
      return
   end if
!
60 ipar(7) = ipar(7) + 1
   zzm1 = zz
   zz = ddot(n, wk(1, 3), 1, wk(1, 3), 1)
   fpar(11) = fpar(11) + 2 * n
   if (brkdn(zz, ipar)) goto 900
   if (ipar(7) > 3) then
      alpha = zz / zzm1
      do i = 1, n
         wk(i, 2) = wk(i, 3) + alpha * wk(i, 2)
      end do
      fpar(11) = fpar(11) + 2 * n
   else
      do i = 1, n
         wk(i, 2) = wk(i, 3)
      end do
   end if
!
!     before iteration can continue, we need to compute A * p
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = n + 1
      if (lp) then
         ipar(9) = ipar(8) + n
      else
         ipar(9) = 3 * n + 1
      end if
      ipar(10) = 6
      return
   end if
!
70 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = n + 1
   end if
   if (lp) then
      ipar(9) = 3 * n + 1
   else
      ipar(9) = n + n + 1
   end if
   ipar(10) = 7
   return
!
80 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = n + n + 1
      ipar(10) = 8
      return
   end if
!
!     update the solution -- accumulate the changes in w(:,5)
!
90 ipar(7) = ipar(7) + 1
   alpha = ddot(n, wk(1, 3), 1, wk(1, 3), 1)
   fpar(11) = fpar(11) + 2 * n
   if (brkdn(alpha, ipar)) goto 900
   alpha = zz / alpha
   do i = 1, n
      wk(i, 5) = wk(i, 5) + alpha * wk(i, 2)
      wk(i, 1) = wk(i, 1) - alpha * wk(i, 3)
   end do
   fpar(11) = fpar(11) + 4 * n
!
!     are we ready to terminate ?
!
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = 4 * n + 1
      ipar(9) = 3 * n + 1
      ipar(10) = 9
      return
   end if
100 if (ipar(3) == 999) then
      if (ipar(11) == 1) goto 900
   else if (stopbis(n, ipar, 1, fpar, wk, wk(1, 2), alpha)) then
      goto 900
   end if
!
!     continue the iterations
!
   goto 30
!
!     clean up -- necessary to accommodate the right-preconditioning
!
900 if (rp) then
      if (ipar(1) < 0) ipar(12) = ipar(1)
      ipar(1) = 5
      ipar(8) = 4 * n + 1
      ipar(9) = ipar(8) - n
      ipar(10) = 10
      return
   end if
110 if (rp) then
      call tidycg(n, ipar, fpar, sol, wk(1, 4))
   else
      call tidycg(n, ipar, fpar, sol, wk(1, 5))
   end if
   return
end subroutine cgnr

subroutine bcg(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: fpar(16), rhs(n), sol(n), w(n, *)
!-----------------------------------------------------------------------
!     BCG: Bi Conjugate Gradient method. Programmed with reverse
!     communication, see the header for detailed specifications
!     of the protocol.
!
!     in this routine, before successful return, the fpar's are
!     fpar(3) == initial residual norm
!     fpar(4) == target residual norm
!     fpar(5) == current residual norm
!     fpar(7) == current rho (rhok = <r, s>)
!     fpar(8) == previous rho (rhokm1)
!
!     w(:,1) -- r, the residual
!     w(:,2) -- s, the dual of the 'r'
!     w(:,3) -- p, the projection direction
!     w(:,4) -- q, the dual of the 'p'
!     w(:,5) -- v, a scratch vector to store A*p, or A*q.
!     w(:,6) -- a scratch vector to store intermediate results
!     w(:,7) -- changes in the solution
!-----------------------------------------------------------------------
!     external routines used
!
   logical stopbis, brkdn
   external stopbis, brkdn
   EXTERNAL_DDOT
!
   double precision :: one
   parameter(one=1.0d0)
!
!     local variables
!
   integer i
   double precision :: alpha
   logical rp, lp
   save
!
!     status of the program
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 40
   else if (ipar(10) == 4) then
      goto 50
   else if (ipar(10) == 5) then
      goto 60
   else if (ipar(10) == 6) then
      goto 70
   else if (ipar(10) == 7) then
      goto 80
   else if (ipar(10) == 8) then
      goto 90
   else if (ipar(10) == 9) then
      goto 100
   else if (ipar(10) == 10) then
      goto 110
   end if
!
!     initialization, initial residual
!
   call bisinit(ipar, fpar, 7 * n, 1, lp, rp, w)
   if (ipar(1) < 0) return
!
!     compute initial residual, request a matvecc
!
   ipar(1) = 1
   ipar(8) = 3 * n + 1
   ipar(9) = ipar(8) + n
   do i = 1, n
      w(i, 4) = sol(i)
   end do
   ipar(10) = 1
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   do i = 1, n
      w(i, 1) = rhs(i) - w(i, 5)
   end do
   fpar(11) = fpar(11) + n
   if (lp) then
      ipar(1) = 3
      ipar(8) = 1
      ipar(9) = n + 1
      ipar(10) = 2
      return
   end if
!
20 if (lp) then
      do i = 1, n
         w(i, 1) = w(i, 2)
         w(i, 3) = w(i, 2)
         w(i, 4) = w(i, 2)
      end do
   else
      do i = 1, n
         w(i, 2) = w(i, 1)
         w(i, 3) = w(i, 1)
         w(i, 4) = w(i, 1)
      end do
   end if
!
   fpar(7) = ddot(n, w, 1, w, 1)
   fpar(11) = fpar(11) + 2 * n
   fpar(3) = sqrt(fpar(7))
   fpar(5) = fpar(3)
   fpar(8) = one
   if (abs(ipar(3)) == 2) then
      fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
      fpar(11) = fpar(11) + 2 * n
   else if (ipar(3) /= 999) then
      fpar(4) = fpar(1) * fpar(3) + fpar(2)
   end if
   if (ipar(3) >= 0 .and. fpar(5) <= fpar(4)) then
      fpar(6) = fpar(5)
      goto 900
   end if
!
!     end of initialization, begin iteration, v = A p
!
30 if (rp) then
      ipar(1) = 5
      ipar(8) = n + n + 1
      if (lp) then
         ipar(9) = 4 * n + 1
      else
         ipar(9) = 5 * n + 1
      end if
      ipar(10) = 3
      return
   end if
!
40 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = n + n + 1
   end if
   if (lp) then
      ipar(9) = 5 * n + 1
   else
      ipar(9) = 4 * n + 1
   end if
   ipar(10) = 4
   return
!
50 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = 4 * n + 1
      ipar(10) = 5
      return
   end if
!
60 ipar(7) = ipar(7) + 1
   alpha = ddot(n, w(1, 4), 1, w(1, 5), 1)
   fpar(11) = fpar(11) + 2 * n
   if (brkdn(alpha, ipar)) goto 900
   alpha = fpar(7) / alpha
   do i = 1, n
      w(i, 7) = w(i, 7) + alpha * w(i, 3)
      w(i, 1) = w(i, 1) - alpha * w(i, 5)
   end do
   fpar(11) = fpar(11) + 4 * n
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = 6 * n + 1
      ipar(9) = 5 * n + 1
      ipar(10) = 6
      return
   end if
70 if (ipar(3) == 999) then
      if (ipar(11) == 1) goto 900
   else if (stopbis(n, ipar, 1, fpar, w, w(1, 3), alpha)) then
      goto 900
   end if
!
!     A^t * x
!
   if (lp) then
      ipar(1) = 4
      ipar(8) = 3 * n + 1
      if (rp) then
         ipar(9) = 4 * n + 1
      else
         ipar(9) = 5 * n + 1
      end if
      ipar(10) = 7
      return
   end if
!
80 ipar(1) = 2
   if (lp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = 3 * n + 1
   end if
   if (rp) then
      ipar(9) = 5 * n + 1
   else
      ipar(9) = 4 * n + 1
   end if
   ipar(10) = 8
   return
!
90 if (rp) then
      ipar(1) = 6
      ipar(8) = ipar(9)
      ipar(9) = 4 * n + 1
      ipar(10) = 9
      return
   end if
!
100 ipar(7) = ipar(7) + 1
   do i = 1, n
      w(i, 2) = w(i, 2) - alpha * w(i, 5)
   end do
   fpar(8) = fpar(7)
   fpar(7) = ddot(n, w, 1, w(1, 2), 1)
   fpar(11) = fpar(11) + 4 * n
   if (brkdn(fpar(7), ipar)) return
   alpha = fpar(7) / fpar(8)
   do i = 1, n
      w(i, 3) = w(i, 1) + alpha * w(i, 3)
      w(i, 4) = w(i, 2) + alpha * w(i, 4)
   end do
   fpar(11) = fpar(11) + 4 * n
!
!     end of the iterations
!
   goto 30
!
!     some clean up job to do
!
900 if (rp) then
      if (ipar(1) < 0) ipar(12) = ipar(1)
      ipar(1) = 5
      ipar(8) = 6 * n + 1
      ipar(9) = ipar(8) - n
      ipar(10) = 10
      return
   end if
110 if (rp) then
      call tidycg(n, ipar, fpar, sol, w(1, 6))
   else
      call tidycg(n, ipar, fpar, sol, w(1, 7))
   end if
   return
end subroutine bcg

subroutine bcgstab(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(n, 8)
!-----------------------------------------------------------------------
!     BCGSTAB --- Bi Conjugate Gradient stabilized (BCGSTAB)
!     This is an improved BCG routine. (1) no matrix transpose is
!     involved. (2) the convergence is smoother.
!
!
!     Algorithm:
!     Initialization - r = b - A x, r0 = r, p = r, rho = (r0, r),
!     Iterate -
!     (1) v = A p
!     (2) alpha = rho / (r0, v)
!     (3) s = r - alpha v
!     (4) t = A s
!     (5) omega = (t, s) / (t, t)
!     (6) x = x + alpha * p + omega * s
!     (7) r = s - omega * t
!     convergence test goes here
!     (8) beta = rho, rho = (r0, r), beta = rho * alpha / (beta * omega)
!         p = r + beta * (p - omega * v)
!
!     in this routine, before successful return, the fpar's are
!     fpar(3) == initial (preconditionied-)residual norm
!     fpar(4) == target (preconditionied-)residual norm
!     fpar(5) == current (preconditionied-)residual norm
!     fpar(6) == current residual norm or error
!     fpar(7) == current rho (rhok = <r, r0>)
!     fpar(8) == alpha
!     fpar(9) == omega
!
!     Usage of the work space W
!     w(:, 1) = r0, the initial residual vector
!     w(:, 2) = r, current residual vector
!     w(:, 3) = s
!     w(:, 4) = t
!     w(:, 5) = v
!     w(:, 6) = p
!     w(:, 7) = tmp, used in preconditioning, etc.
!     w(:, 8) = delta x, the correction to the answer is accumulated
!               here, so that the right-preconditioning may be applied
!               at the end
!-----------------------------------------------------------------------
!     external routines used
!
   logical stopbis, brkdn
   external stopbis, brkdn
   EXTERNAL_DDOT
!
   double precision :: one
   parameter(one=1.0d0)
!
!     local variables
!
   integer i
   double precision :: alpha, beta, rho, omega
   logical lp, rp
   save lp, rp
!
!     where to go
!
   if (ipar(1) > 0) then
      if (ipar(10) == 1) then
         goto 10
      else if (ipar(10) == 2) then
         goto 20
      else if (ipar(10) == 3) then
         goto 40
      else if (ipar(10) == 4) then
         goto 50
      else if (ipar(10) == 5) then
         goto 60
      else if (ipar(10) == 6) then
         goto 70
      else if (ipar(10) == 7) then
         goto 80
      else if (ipar(10) == 8) then
         goto 90
      else if (ipar(10) == 9) then
         goto 100
      else if (ipar(10) == 10) then
         goto 110
      end if
   else if (ipar(1) < 0) then
      goto 900
   end if
!
!     call the initialization routine
!
   call bisinit(ipar, fpar, 8 * n, 1, lp, rp, w)
   if (ipar(1) < 0) return
!
!     perform a matvec to compute the initial residual
!
   ipar(1) = 1
   ipar(8) = 1
   ipar(9) = 1 + n

   !do i = 1, n
   !   w(i,1) = sol(i)
   !enddo

   W(1:N, 1) = SOL(1:N)

   ipar(10) = 1
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1

   !$OMP PARALLEL DO                                     &
   !$OMP PRIVATE(i)
   do i = 1, n
      w(i, 1) = rhs(i) - w(i, 2)
   end do
   !$OMP END PARALLEL DO

   ! W(1:N,1) = RHS(1:N) - W(1:N,2)

   fpar(11) = fpar(11) + n
   if (lp) then
      ipar(1) = 3
      ipar(10) = 2
      return
   end if
!
20 if (lp) then
      do i = 1, n
         w(i, 1) = w(i, 2)
         w(i, 6) = w(i, 2)
      end do
   else
      do i = 1, n
         w(i, 2) = w(i, 1)
         w(i, 6) = w(i, 1)
      end do
   end if
!
   fpar(7) = ddot(n, w, 1, w, 1)
   fpar(11) = fpar(11) + 2 * n
   fpar(5) = sqrt(fpar(7))
   fpar(3) = fpar(5)
   if (abs(ipar(3)) == 2) then
      fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
      fpar(11) = fpar(11) + 2 * n
   else if (ipar(3) /= 999) then
      fpar(4) = fpar(1) * fpar(3) + fpar(2)
   end if
   if (ipar(3) >= 0) fpar(6) = fpar(5)
   if (ipar(3) >= 0 .and. fpar(5) <= fpar(4) .and. ipar(3) /= 999) then
      goto 900
   end if
!
!     beginning of the iterations
!
!     Step (1), v = A p
30 if (rp) then
      ipar(1) = 5
      ipar(8) = 5 * n + 1
      if (lp) then
         ipar(9) = 4 * n + 1
      else
         ipar(9) = 6 * n + 1
      end if
      ipar(10) = 3
      return
   end if
!
40 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = 5 * n + 1
   end if
   if (lp) then
      ipar(9) = 6 * n + 1
   else
      ipar(9) = 4 * n + 1
   end if
   ipar(10) = 4
   return
50 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = 4 * n + 1
      ipar(10) = 5
      return
   end if
!
60 ipar(7) = ipar(7) + 1
!
!     step (2)
   alpha = ddot(n, w(1, 1), 1, w(1, 5), 1)
   fpar(11) = fpar(11) + 2 * n
   if (brkdn(alpha, ipar)) goto 900
   alpha = fpar(7) / alpha
   fpar(8) = alpha
!
!     step (3)

   !$OMP PARALLEL DO                                     &
   !$OMP PRIVATE(i)
   do i = 1, n
      w(i, 3) = w(i, 2) - alpha * w(i, 5)
   end do
   !$OMP END PARALLEL DO

   ! w(1:N,3) = w(1:N,2) - alpha * w(1:N,5)

   fpar(11) = fpar(11) + 2 * n
!
!     Step (4): the second matvec -- t = A s
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = n + n + 1
      if (lp) then
         ipar(9) = ipar(8) + n
      else
         ipar(9) = 6 * n + 1
      end if
      ipar(10) = 6
      return
   end if
!
70 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = n + n + 1
   end if
   if (lp) then
      ipar(9) = 6 * n + 1
   else
      ipar(9) = 3 * n + 1
   end if
   ipar(10) = 7
   return
80 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = 3 * n + 1
      ipar(10) = 8
      return
   end if
90 ipar(7) = ipar(7) + 1
!
!     step (5)
   omega = ddot(n, w(1, 4), 1, w(1, 4), 1)
   if (omega /= 0d0) then
      fpar(11) = fpar(11) + n + n
      if (brkdn(omega, ipar)) goto 900
      omega = ddot(n, w(1, 4), 1, w(1, 3), 1) / omega
      fpar(11) = fpar(11) + n + n
      if (brkdn(omega, ipar)) goto 900
   end if

   fpar(9) = omega
   alpha = fpar(8)
!
!     step (6) and (7)

!      do i = 1, n
!         w(i,7) = alpha * w(i,6) + omega * w(i,3)
!         w(i,8) = w(i,8) + w(i,7)
!         w(i,2) = w(i,3) - omega * w(i,4)
!      enddo

   !$OMP PARALLEL DO                                     &
   !$OMP PRIVATE(i)
   do i = 1, n
      w(i, 7) = alpha * w(i, 6) + omega * w(i, 3)
      w(i, 8) = w(i, 8) + w(i, 7)
   end do
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO                                     &
   !$OMP PRIVATE(i)
   do i = 1, n
      w(i, 2) = w(i, 3) - omega * w(i, 4)
   end do
   !$OMP END PARALLEL DO

   fpar(11) = fpar(11) + 6 * n + 1
!
!     convergence test
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = 7 * n + 1
      ipar(9) = 6 * n + 1
      ipar(10) = 9
      return
   end if
   if (stopbis(n, ipar, 2, fpar, w(1, 2), w(1, 7), one)) goto 900
100 if (ipar(3) == 999 .and. ipar(11) == 1) goto 900
!
!     step (8): computing new p and rho
   rho = fpar(7)
   fpar(7) = ddot(n, w(1, 2), 1, w(1, 1), 1)
   omega = fpar(9)
   beta = fpar(7) * fpar(8) / (fpar(9) * rho)

   !$OMP PARALLEL DO                                     &
   !$OMP PRIVATE(i)
   do i = 1, n
      w(i, 6) = w(i, 2) + beta * (w(i, 6) - omega * w(i, 5))
   end do
   !$OMP END PARALLEL DO

   ! w(1:N,6) = w(1:N,2) + beta * (w(1:N,6) - omega * w(1:N,5))

   fpar(11) = fpar(11) + 6 * n + 3
   if (brkdn(fpar(7), ipar)) goto 900
!
!     end of an iteration
!
   goto 30
!
!     some clean up job to do
!
900 if (rp) then
      if (ipar(1) < 0) ipar(12) = ipar(1)
      ipar(1) = 5
      ipar(8) = 7 * n + 1
      ipar(9) = ipar(8) - n
      ipar(10) = 10
      return
   end if
110 if (rp) then
      call tidycg(n, ipar, fpar, sol, w(1, 7))
   else
      call tidycg(n, ipar, fpar, sol, w(1, 8))
   end if
!
   return
end subroutine bcgstab

! NOT THREAD-SAFE
double precision function ddotXXX(n, dx, incx, dy, incy)
   use m_saad, only: jasafe

   implicit none

   integer :: N, INCX, INCY
   double precision :: dx(N), dy(N)
   double precision :: dots
   integer :: i

   double precision, external :: DDOTORG

   ! DDOT  = ddotORG(n,dx,incx,dy,incy)
   ! return

   if (INCX == 1 .and. INCY == 1) then

      !DDOT  = SUM( DX(1:N)*DY(1:N) )
      !return

      DOTs = 0d0

      if (jasafe /= 1) then
         !$OMP PARALLEL DO                                     &
         !$OMP PRIVATE(i)                                      &
         !$OMP REDUCTION(+:DOTs)
         do I = 1, N
            DOTs = DOTs + DX(I) * DY(I)
         end do
         !$OMP END PARALLEL DO
         ddotXXX = dots
      else
         do I = 1, N
            DOTs = DOTs + DX(I) * DY(I)
         end do
         ddotXXX = dots
      end if

   else
      DDOTXXX = ddotORG(n, dx, incx, dy, incy)
   end if
end function ddotXXX

subroutine tfqmr(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(n, *)
!-----------------------------------------------------------------------
!     TFQMR --- transpose-free Quasi-Minimum Residual method
!     This is developed from BCG based on the principle of Quasi-Minimum
!     Residual, and it is transpose-free.
!
!     It uses approximate residual norm.
!
!     Internally, the fpar's are used as following:
!     fpar(3) --- initial residual norm squared
!     fpar(4) --- target residual norm squared
!     fpar(5) --- current residual norm squared
!
!     w(:,1) -- R, residual
!     w(:,2) -- R0, the initial residual
!     w(:,3) -- W
!     w(:,4) -- Y
!     w(:,5) -- Z
!     w(:,6) -- A * Y
!     w(:,7) -- A * Z
!     w(:,8) -- V
!     w(:,9) -- D
!     w(:,10) -- intermediate results of preconditioning
!     w(:,11) -- changes in the solution
!-----------------------------------------------------------------------
!     external functions
!
   logical brkdn
   external brkdn
   EXTERNAL_DDOT
!
   double precision :: one, zero
   parameter(one=1.0d0, zero=0.0d0)
!
!     local variables
!
   integer i
   logical lp, rp
   double precision :: eta, sigma, theta, te, alpha, rho, tao
   save
!
!     status of the call (where to go)
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 40
   else if (ipar(10) == 4) then
      goto 50
   else if (ipar(10) == 5) then
      goto 60
   else if (ipar(10) == 6) then
      goto 70
   else if (ipar(10) == 7) then
      goto 80
   else if (ipar(10) == 8) then
      goto 90
   else if (ipar(10) == 9) then
      goto 100
   else if (ipar(10) == 10) then
      goto 110
   end if
!
!     initializations
!
   call bisinit(ipar, fpar, 11 * n, 2, lp, rp, w)
   if (ipar(1) < 0) return
   ipar(1) = 1
   ipar(8) = 1
   ipar(9) = 1 + 6 * n
   do i = 1, n
      w(i, 1) = sol(i)
   end do
   ipar(10) = 1
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   do i = 1, n
      w(i, 1) = rhs(i) - w(i, 7)
      w(i, 9) = zero
   end do
   fpar(11) = fpar(11) + n
!
   if (lp) then
      ipar(1) = 3
      ipar(9) = n + 1
      ipar(10) = 2
      return
   end if
20 continue
   if (lp) then
      do i = 1, n
         w(i, 1) = w(i, 2)
         w(i, 3) = w(i, 2)
      end do
   else
      do i = 1, n
         w(i, 2) = w(i, 1)
         w(i, 3) = w(i, 1)
      end do
   end if
!
   fpar(5) = sqrt(ddot(n, w, 1, w, 1))
   fpar(3) = fpar(5)
   tao = fpar(5)
   fpar(11) = fpar(11) + n + n
   if (abs(ipar(3)) == 2) then
      fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
      fpar(11) = fpar(11) + n + n
   else if (ipar(3) /= 999) then
      fpar(4) = fpar(1) * tao + fpar(2)
   end if
   te = zero
   rho = zero
!
!     begin iteration
!
30 sigma = rho
   rho = ddot(n, w(1, 2), 1, w(1, 3), 1)
   fpar(11) = fpar(11) + n + n
   if (brkdn(rho, ipar)) goto 900
   if (ipar(7) == 1) then
      alpha = zero
   else
      alpha = rho / sigma
   end if
   do i = 1, n
      w(i, 4) = w(i, 3) + alpha * w(i, 5)
   end do
   fpar(11) = fpar(11) + n + n
!
!     A * x -- with preconditioning
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = 3 * n + 1
      if (lp) then
         ipar(9) = 5 * n + 1
      else
         ipar(9) = 9 * n + 1
      end if
      ipar(10) = 3
      return
   end if
!
40 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = 3 * n + 1
   end if
   if (lp) then
      ipar(9) = 9 * n + 1
   else
      ipar(9) = 5 * n + 1
   end if
   ipar(10) = 4
   return
!
50 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = 5 * n + 1
      ipar(10) = 5
      return
   end if
60 ipar(7) = ipar(7) + 1
   do i = 1, n
      w(i, 8) = w(i, 6) + alpha * (w(i, 7) + alpha * w(i, 8))
   end do
   sigma = ddot(n, w(1, 2), 1, w(1, 8), 1)
   fpar(11) = fpar(11) + 6 * n
   if (brkdn(sigma, ipar)) goto 900
   alpha = rho / sigma
   do i = 1, n
      w(i, 5) = w(i, 4) - alpha * w(i, 8)
   end do
   fpar(11) = fpar(11) + 2 * n
!
!     the second A * x
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = 4 * n + 1
      if (lp) then
         ipar(9) = 6 * n + 1
      else
         ipar(9) = 9 * n + 1
      end if
      ipar(10) = 6
      return
   end if
!
70 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = 4 * n + 1
   end if
   if (lp) then
      ipar(9) = 9 * n + 1
   else
      ipar(9) = 6 * n + 1
   end if
   ipar(10) = 7
   return
!
80 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = 6 * n + 1
      ipar(10) = 8
      return
   end if
90 ipar(7) = ipar(7) + 1
   do i = 1, n
      w(i, 3) = w(i, 3) - alpha * w(i, 6)
   end do
!
!     update I
!
   theta = ddot(n, w(1, 3), 1, w(1, 3), 1) / (tao * tao)
   sigma = one / (one + theta)
   tao = tao * sqrt(sigma * theta)
   fpar(11) = fpar(11) + 4 * n + 6
   if (brkdn(tao, ipar)) goto 900
   eta = sigma * alpha
   sigma = te / alpha
   te = theta * eta
   do i = 1, n
      w(i, 9) = w(i, 4) + sigma * w(i, 9)
      w(i, 11) = w(i, 11) + eta * w(i, 9)
      w(i, 3) = w(i, 3) - alpha * w(i, 7)
   end do
   fpar(11) = fpar(11) + 6 * n + 6
   if (ipar(7) == 1) then
      if (ipar(3) == -1) then
         fpar(3) = eta * sqrt(ddot(n, w(1, 9), 1, w(1, 9), 1))
         fpar(4) = fpar(1) * fpar(3) + fpar(2)
         fpar(11) = fpar(11) + n + n + 4
      end if
   end if
!
!     update II
!
   theta = ddot(n, w(1, 3), 1, w(1, 3), 1) / (tao * tao)
   sigma = one / (one + theta)
   tao = tao * sqrt(sigma * theta)
   fpar(11) = fpar(11) + 8 + 2 * n
   if (brkdn(tao, ipar)) goto 900
   eta = sigma * alpha
   sigma = te / alpha
   te = theta * eta
   do i = 1, n
      w(i, 9) = w(i, 5) + sigma * w(i, 9)
      w(i, 11) = w(i, 11) + eta * w(i, 9)
   end do
   fpar(11) = fpar(11) + 4 * n + 3
!
!     this is the correct over-estimate
!      fpar(5) = sqrt(real(ipar(7)+1)) * tao
!     this is an approximation
   fpar(5) = tao
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = 10 * n + 1
      ipar(9) = 9 * n + 1
      ipar(10) = 9
      return
   else if (ipar(3) < 0) then
      fpar(6) = eta * sqrt(ddot(n, w(1, 9), 1, w(1, 9), 1))
      fpar(11) = fpar(11) + n + n + 2
   else
      fpar(6) = fpar(5)
   end if
   if (fpar(6) > fpar(4) .and. (ipar(7) < ipar(6) .or. ipar(6) <= 0)) goto 30
100 if (ipar(3) == 999 .and. ipar(11) == 0) goto 30
!
!     clean up
!
900 if (rp) then
      if (ipar(1) < 0) ipar(12) = ipar(1)
      ipar(1) = 5
      ipar(8) = 10 * n + 1
      ipar(9) = ipar(8) - n
      ipar(10) = 10
      return
   end if
110 if (rp) then
      call tidycg(n, ipar, fpar, sol, w(1, 10))
   else
      call tidycg(n, ipar, fpar, sol, w(1, 11))
   end if
!
   return
end subroutine tfqmr

subroutine fom(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(*)
!-----------------------------------------------------------------------
!     This a version of The Full Orthogonalization Method (FOM)
!     implemented with reverse communication. It is a simple restart
!     version of the FOM algorithm and is implemented with plane
!     rotations similarly to GMRES.
!
!  parameters:
!  -----------
!     ipar(5) == the dimension of the Krylov subspace
!     after every ipar(5) iterations, the FOM will restart with
!     the updated solution and recomputed residual vector.
!
!     the work space in `w' is used as follows:
!     (1) the basis for the Krylov subspace, size n*(m+1);
!     (2) the Hessenberg matrix, only the upper triangular
!     portion of the matrix is stored, size (m+1)*m/2 + 1
!     (3) three vectors, all are of size m, they are
!     the cosine and sine of the Givens rotations, the third one holds
!     the residuals, it is of size m+1.
!
!     TOTAL SIZE REQUIRED == (n+3)*(m+2) + (m+1)*m/2
!     Note: m == ipar(5). The default value for this is 15 if
!     ipar(5) <= 1.
!-----------------------------------------------------------------------
!     external functions used
!
   EXTERNAL_DDOT
!
   double precision :: one, zero
   parameter(one=1.0d0, zero=0.0d0)
!
!     local variables, ptr and p2 are temporary pointers,
!     hes points to the Hessenberg matrix,
!     vs point to the sines of the Givens rotations
!     vrn points to the vectors of residual norms, more precisely
!     the right hand side of the least square problem solved.
!
   integer i, ii, idx, k, m, ptr, p2, prs, hes, vs, vrn
   double precision :: alpha, c, s
   logical lp, rp
   save
!
!     check the status of the call
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 30
   else if (ipar(10) == 4) then
      goto 40
   else if (ipar(10) == 5) then
      goto 50
   else if (ipar(10) == 6) then
      goto 60
   else if (ipar(10) == 7) then
      goto 70
   end if
!
!     initialization
!
   if (ipar(5) <= 1) then
      m = 15
   else
      m = ipar(5)
   end if
   idx = n * (m + 1)
   hes = idx + n
!     vc = hes + (m+1) * m / 2 + 1
!     vs = vc + m
   vrn = vs + m
   i = vrn + m + 1
   call bisinit(ipar, fpar, i, 1, lp, rp, w)
   if (ipar(1) < 0) return
!
!     request for matrix vector multiplication A*x in the initialization
!
100 ipar(1) = 1
   ipar(8) = n + 1
   ipar(9) = 1
   ipar(10) = 1
   k = 0
   do i = 1, n
      w(n + i) = sol(i)
   end do
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   if (lp) then
      do i = 1, n
         w(n + i) = rhs(i) - w(i)
      end do
      ipar(1) = 3
      ipar(10) = 2
      return
   else
      do i = 1, n
         w(i) = rhs(i) - w(i)
      end do
   end if
   fpar(11) = fpar(11) + n
!
20 alpha = sqrt(ddot(n, w, 1, w, 1))
   fpar(11) = fpar(11) + 2 * n + 1
   if (ipar(7) == 1 .and. ipar(3) /= 999) then
      if (abs(ipar(3)) == 2) then
         fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
         fpar(11) = fpar(11) + 2 * n
      else
         fpar(4) = fpar(1) * alpha + fpar(2)
      end if
      fpar(3) = alpha
   end if
   fpar(5) = alpha
   w(vrn + 1) = alpha
   if (alpha <= fpar(4) .and. ipar(3) >= 0 .and. ipar(3) /= 999) then
      ipar(1) = 0
      fpar(6) = alpha
      goto 300
   end if
   alpha = one / alpha
   do ii = 1, n
      w(ii) = alpha * w(ii)
   end do
   fpar(11) = fpar(11) + n
!
!     request for (1) right preconditioning
!     (2) matrix vector multiplication
!     (3) left preconditioning
!
110 k = k + 1
   if (rp) then
      ipar(1) = 5
      ipar(8) = k * n - n + 1
      if (lp) then
         ipar(9) = k * n + 1
      else
         ipar(9) = idx + 1
      end if
      ipar(10) = 3
      return
   end if
!
30 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = (k - 1) * n + 1
   end if
   if (lp) then
      ipar(9) = idx + 1
   else
      ipar(9) = 1 + k * n
   end if
   ipar(10) = 4
   return
!
40 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = k * n + 1
      ipar(10) = 5
      return
   end if
!
!     Modified Gram-Schmidt orthogonalization procedure
!     temporary pointer 'ptr' is pointing to the current column of the
!     Hessenberg matrix. 'p2' points to the new basis vector
!
50 ipar(7) = ipar(7) + 1
   ptr = k * (k - 1) / 2 + hes
   p2 = ipar(9)
   call mgsro(.false., n, n, k + 1, k + 1, fpar(11), w, w(ptr + 1), ipar(12))
   if (ipar(12) < 0) goto 200
!
!     apply previous Givens rotations to column.
!
   p2 = ptr + 1
   do i = 1, k - 1
      ptr = p2
      p2 = p2 + 1
      alpha = w(ptr)
!        c = w(vc+i)
      s = w(vs + i)
!        w(ptr) = c * alpha + s * w(p2)
!        w(p2) = c * w(p2) - s * alpha
   end do
!
!     end of one Arnoldi iteration, alpha will store the estimated
!     residual norm at current stage
!
   fpar(11) = fpar(11) + 6 * k

   prs = vrn + k
   alpha = fpar(5)
   if (w(p2) /= zero) alpha = abs(w(p2 + 1) * w(prs) / w(p2))
   fpar(5) = alpha
!
   if (k >= m .or. (ipar(3) >= 0 .and. alpha <= fpar(4)) .or. (ipar(6) > 0 .and. ipar(7) >= ipar(6))) goto 200
!
   call givens(w(p2), w(p2 + 1), c, s)
!     w(vc+k) = c
   w(vs + k) = s
   alpha = -s * w(prs)
!     w(prs) = c * w(prs)
   w(prs + 1) = alpha
!
   if (w(p2) /= zero) goto 110
!
!     update the approximate solution, first solve the upper triangular
!     system, temporary pointer ptr points to the Hessenberg matrix,
!     prs points to the right-hand-side (also the solution) of the system.
!
200 ptr = hes + k * (k + 1) / 2
   prs = vrn + k
   if (w(ptr) == zero) then
!
!     if the diagonal elements of the last column is zero, reduce k by 1
!     so that a smaller trianguler system is solved
!
      k = k - 1
      if (k > 0) then
         goto 200
      else
         ipar(1) = -3
         ipar(12) = -4
         goto 300
      end if
   end if
   w(prs) = w(prs) / w(ptr)
   do i = k - 1, 1, -1
      ptr = ptr - i - 1
      do ii = 1, i
         w(vrn + ii) = w(vrn + ii) - w(prs) * w(ptr + ii)
      end do
      prs = prs - 1
      w(prs) = w(prs) / w(ptr)
   end do
!
   do ii = 1, n
      w(ii) = w(ii) * w(prs)
   end do
   do i = 1, k - 1
      prs = prs + 1
      ptr = i * n
      do ii = 1, n
         w(ii) = w(ii) + w(prs) * w(ptr + ii)
      end do
   end do
   fpar(11) = fpar(11) + 2 * (k - 1) * n + n + k * (k + 1)
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = 1
      ipar(9) = idx + 1
      ipar(10) = 6
      return
   end if
!
60 if (rp) then
      do i = 1, n
         sol(i) = sol(i) + w(idx + i)
      end do
   else
      do i = 1, n
         sol(i) = sol(i) + w(i)
      end do
   end if
   fpar(11) = fpar(11) + n
!
!     process the complete stopping criteria
!
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = -1
      ipar(9) = idx + 1
      ipar(10) = 7
      return
   else if (ipar(3) < 0) then
      if (ipar(7) <= m + 1) then
         fpar(3) = abs(w(vrn + 1))
         if (ipar(3) == -1) fpar(4) = fpar(1) * fpar(3) + fpar(2)
      end if
      alpha = abs(w(vrn + k))
   end if
   fpar(6) = alpha
!
!     do we need to restart ?
!
70 if (ipar(12) /= 0) then
      ipar(1) = -3
      goto 300
   end if
   if (ipar(7) < ipar(6) .or. ipar(6) <= 0) then
      if (ipar(3) /= 999) then
         if (fpar(6) > fpar(4)) goto 100
      else
         if (ipar(11) == 0) goto 100
      end if
   end if
!
!     termination, set error code, compute convergence rate
!
   if (ipar(1) > 0) then
      if (ipar(3) == 999 .and. ipar(11) == 1) then
         ipar(1) = 0
      else if (ipar(3) /= 999 .and. fpar(6) <= fpar(4)) then
         ipar(1) = 0
      else if (ipar(7) >= ipar(6) .and. ipar(6) > 0) then
         ipar(1) = -1
      else
         ipar(1) = -10
      end if
   end if
300 if (fpar(3) /= zero .and. fpar(6) /= zero .and. ipar(7) > ipar(13)) then
      fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7) - ipar(13))
   else
      fpar(7) = zero
   end if
   return
end subroutine fom

subroutine gmres(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(*)
!-----------------------------------------------------------------------
!     This a version of GMRES implemented with reverse communication.
!     It is a simple restart version of the GMRES algorithm.
!
!     ipar(5) == the dimension of the Krylov subspace
!     after every ipar(5) iterations, the GMRES will restart with
!     the updated solution and recomputed residual vector.
!
!     the space of the `w' is used as follows:
!     (1) the basis for the Krylov subspace, size n*(m+1);
!     (2) the Hessenberg matrix, only the upper triangular
!     portion of the matrix is stored, size (m+1)*m/2 + 1
!     (3) three vectors, all are of size m, they are
!     the cosine and sine of the Givens rotations, the third one holds
!     the residuals, it is of size m+1.
!
!     TOTAL SIZE REQUIRED == (n+3)*(m+2) + (m+1)*m/2
!     Note: m == ipar(5). The default value for this is 15 if
!     ipar(5) <= 1.
!-----------------------------------------------------------------------
!     external functions used
!
   EXTERNAL_DDOT
!
   double precision :: one, zero
   parameter(one=1.0d0, zero=0.0d0)
!
!     local variables, ptr and p2 are temporary pointers,
!     hess points to the Hessenberg matrix,
!     vs point to the sines of the Givens rotations
!     vrn points to the vectors of residual norms, more precisely
!     the right hand side of the least square problem solved.
!
   integer i, ii, idx, k, m, ptr, p2, hess, vs, vrn
   double precision :: alpha, c, s
   logical lp, rp
   save
!
!     check the status of the call
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 30
   else if (ipar(10) == 4) then
      goto 40
   else if (ipar(10) == 5) then
      goto 50
   else if (ipar(10) == 6) then
      goto 60
   else if (ipar(10) == 7) then
      goto 70
   end if
!
!     initialization
!
   if (ipar(5) <= 1) then
      m = 15
   else
      m = ipar(5)
   end if
   idx = n * (m + 1)
   hess = idx + n
!     vc = hess + (m+1) * m / 2 + 1
!     vs = vc + m
   vrn = vs + m
   i = vrn + m + 1
   call bisinit(ipar, fpar, i, 1, lp, rp, w)
   if (ipar(1) < 0) return
!
!     request for matrix vector multiplication A*x in the initialization
!
100 ipar(1) = 1
   ipar(8) = n + 1
   ipar(9) = 1
   ipar(10) = 1
   k = 0
   do i = 1, n
      w(n + i) = sol(i)
   end do
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   if (lp) then
      do i = 1, n
         w(n + i) = rhs(i) - w(i)
      end do
      ipar(1) = 3
      ipar(10) = 2
      return
   else
      do i = 1, n
         w(i) = rhs(i) - w(i)
      end do
   end if
   fpar(11) = fpar(11) + n
!
20 alpha = sqrt(ddot(n, w, 1, w, 1))
   fpar(11) = fpar(11) + 2 * n
   if (ipar(7) == 1 .and. ipar(3) /= 999) then
      if (abs(ipar(3)) == 2) then
         fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
         fpar(11) = fpar(11) + 2 * n
      else
         fpar(4) = fpar(1) * alpha + fpar(2)
      end if
      fpar(3) = alpha
   end if
   fpar(5) = alpha
   w(vrn + 1) = alpha
   if (alpha <= fpar(4) .and. ipar(3) >= 0 .and. ipar(3) /= 999) then
      ipar(1) = 0
      fpar(6) = alpha
      goto 300
   end if
   alpha = one / alpha
   do ii = 1, n
      w(ii) = alpha * w(ii)
   end do
   fpar(11) = fpar(11) + n
!
!     request for (1) right preconditioning
!     (2) matrix vector multiplication
!     (3) left preconditioning
!
110 k = k + 1
   if (rp) then
      ipar(1) = 5
      ipar(8) = k * n - n + 1
      if (lp) then
         ipar(9) = k * n + 1
      else
         ipar(9) = idx + 1
      end if
      ipar(10) = 3
      return
   end if
!
30 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = (k - 1) * n + 1
   end if
   if (lp) then
      ipar(9) = idx + 1
   else
      ipar(9) = 1 + k * n
   end if
   ipar(10) = 4
   return
!
40 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = k * n + 1
      ipar(10) = 5
      return
   end if
!
!     Modified Gram-Schmidt orthogonalization procedure
!     temporary pointer 'ptr' is pointing to the current column of the
!     Hessenberg matrix. 'p2' points to the new basis vector
!
50 ipar(7) = ipar(7) + 1
   ptr = k * (k - 1) / 2 + hess
   p2 = ipar(9)
   call mgsro(.false., n, n, k + 1, k + 1, fpar(11), w, w(ptr + 1), ipar(12))
   if (ipar(12) < 0) goto 200
!
!     apply previous Givens rotations and generate a new one to eliminate
!     the subdiagonal element.
!
   p2 = ptr + 1
   do i = 1, k - 1
      ptr = p2
      p2 = p2 + 1
      alpha = w(ptr)
!        c = w(vc+i)
      s = w(vs + i)
!        w(ptr) = c * alpha + s * w(p2)
!        w(p2) = c * w(p2) - s * alpha
   end do
   call givens(w(p2), w(p2 + 1), c, s)
!     w(vc+k) = c
   w(vs + k) = s
   p2 = vrn + k
   alpha = -s * w(p2)
!     w(p2) = c * w(p2)
   w(p2 + 1) = alpha
!
!     end of one Arnoldi iteration, alpha will store the estimated
!     residual norm at current stage
!
   fpar(11) = fpar(11) + 6 * k + 2
   alpha = abs(alpha)
   fpar(5) = alpha
   if (k < m .and. .not. (ipar(3) >= 0 .and. alpha <= fpar(4)) .and. (ipar(6) <= 0 .or. ipar(7) < ipar(6))) goto 110
!
!     update the approximate solution, first solve the upper triangular
!     system, temporary pointer ptr points to the Hessenberg matrix,
!     p2 points to the right-hand-side (also the solution) of the system.
!
200 ptr = hess + k * (k + 1) / 2
   p2 = vrn + k
   if (w(ptr) == zero) then
!
!     if the diagonal elements of the last column is zero, reduce k by 1
!     so that a smaller trianguler system is solved [It should only
!     happen when the matrix is singular, and at most once!]
!
      k = k - 1
      if (k > 0) then
         goto 200
      else
         ipar(1) = -3
         ipar(12) = -4
         goto 300
      end if
   end if
   w(p2) = w(p2) / w(ptr)
   do i = k - 1, 1, -1
      ptr = ptr - i - 1
      do ii = 1, i
         w(vrn + ii) = w(vrn + ii) - w(p2) * w(ptr + ii)
      end do
      p2 = p2 - 1
      w(p2) = w(p2) / w(ptr)
   end do
!
   do ii = 1, n
      w(ii) = w(ii) * w(p2)
   end do
   do i = 1, k - 1
      ptr = i * n
      p2 = p2 + 1
      do ii = 1, n
         w(ii) = w(ii) + w(p2) * w(ptr + ii)
      end do
   end do
   fpar(11) = fpar(11) + 2 * k * n - n + k * (k + 1)
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = 1
      ipar(9) = idx + 1
      ipar(10) = 6
      return
   end if
!
60 if (rp) then
      do i = 1, n
         sol(i) = sol(i) + w(idx + i)
      end do
   else
      do i = 1, n
         sol(i) = sol(i) + w(i)
      end do
   end if
   fpar(11) = fpar(11) + n
!
!     process the complete stopping criteria
!
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = -1
      ipar(9) = idx + 1
      ipar(10) = 7
      return
   else if (ipar(3) < 0) then
      if (ipar(7) <= m + 1) then
         fpar(3) = abs(w(vrn + 1))
         if (ipar(3) == -1) fpar(4) = fpar(1) * fpar(3) + fpar(2)
      end if
      fpar(6) = abs(w(vrn + k))
   else
      fpar(6) = fpar(5)
   end if
!
!     do we need to restart ?
!
70 if (ipar(12) /= 0) then
      ipar(1) = -3
      goto 300
   end if
   if ((ipar(7) < ipar(6) .or. ipar(6) <= 0) .and. ((ipar(3) == 999 .and. ipar(11) == 0) .or. (ipar(3) /= 999 .and. fpar(6) > fpar(4)))) goto 100
!
!     termination, set error code, compute convergence rate
!
   if (ipar(1) > 0) then
      if (ipar(3) == 999 .and. ipar(11) == 1) then
         ipar(1) = 0
      else if (ipar(3) /= 999 .and. fpar(6) <= fpar(4)) then
         ipar(1) = 0
      else if (ipar(7) >= ipar(6) .and. ipar(6) > 0) then
         ipar(1) = -1
      else
         ipar(1) = -10
      end if
   end if
300 if (fpar(3) /= zero .and. fpar(6) /= zero .and. ipar(7) > ipar(13)) then
      fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7) - ipar(13))
   else
      fpar(7) = zero
   end if
   return
end subroutine gmres

subroutine dqgmres(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(*)
!-----------------------------------------------------------------------
!     DQGMRES -- Flexible Direct version of Quasi-General Minimum
!     Residual method. The right preconditioning can be varied from
!     step to step.
!
!     Work space used = n + lb * (2*n+4)
!     where lb = ipar(5) + 1 (default 16 if ipar(5) <= 1)
!-----------------------------------------------------------------------
!     local variables
!
   double precision :: one, zero, deps
   parameter(one=1.0d0, zero=0.0d0)
   parameter(deps=1.0d-33)
!
   integer i, ii, j, jp1, j0, k, ptrw, ptrv, iv, iw, ic, is, ihm, ihd, lb, ptr
   double precision :: alpha, beta, psi, c, s
   logical lp, rp, full
   external bisinit
   EXTERNAL_DDOT
   save
!
!     where to go
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 40
   else if (ipar(10) == 4) then
      goto 50
   else if (ipar(10) == 5) then
      goto 60
   else if (ipar(10) == 6) then
      goto 70
   end if
!
!     locations of the work arrays. The arrangement is as follows:
!     w(1:n) -- temporary storage for the results of the preconditioning
!     w(iv+1:iw) -- the V's
!     w(iw+1:ic) -- the W's
!     w(ic+1:is) -- the COSINEs of the Givens rotations
!     w(is+1:ihm) -- the SINEs of the Givens rotations
!     w(ihm+1:ihd) -- the last column of the Hessenberg matrix
!     w(ihd+1:i) -- the inverse of the diagonals of the Hessenberg matrix
!
   if (ipar(5) <= 1) then
      lb = 16
   else
      lb = ipar(5) + 1
   end if
   iv = n
   iw = iv + lb * n
!     ic = iw + lb * n
!     is = ic + lb
   ihm = is + lb
   ihd = ihm + lb
   i = ihd + lb
!
!     parameter check, initializations
!
   full = .false.
   call bisinit(ipar, fpar, i, 1, lp, rp, w)
   if (ipar(1) < 0) return
   ipar(1) = 1
   if (lp) then
      do ii = 1, n
         w(iv + ii) = sol(ii)
      end do
      ipar(8) = iv + 1
      ipar(9) = 1
   else
      do ii = 1, n
         w(ii) = sol(ii)
      end do
      ipar(8) = 1
      ipar(9) = iv + 1
   end if
   ipar(10) = 1
   return
!
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   if (lp) then
      do i = 1, n
         w(i) = rhs(i) - w(i)
      end do
      ipar(1) = 3
      ipar(8) = 1
      ipar(9) = iv + 1
      ipar(10) = 2
      return
   else
      do i = 1, n
         w(iv + i) = rhs(i) - w(iv + i)
      end do
   end if
   fpar(11) = fpar(11) + n
!
20 alpha = sqrt(ddot(n, w(iv + 1), 1, w(iv + 1), 1))
   fpar(11) = fpar(11) + (n + n)
   if (abs(ipar(3)) == 2) then
      fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
      fpar(11) = fpar(11) + 2 * n
   else if (ipar(3) /= 999) then
      fpar(4) = fpar(1) * alpha + fpar(2)
   end if
   fpar(3) = alpha
   fpar(5) = alpha
   psi = alpha
   if (alpha <= fpar(4)) then
      ipar(1) = 0
      fpar(6) = alpha
      goto 80
   end if
   alpha = one / alpha
   do i = 1, n
      w(iv + i) = w(iv + i) * alpha
   end do
   fpar(11) = fpar(11) + n
   j = 0
!
!     iterations start here
!
30 j = j + 1
   if (j > lb) j = j - lb
   jp1 = j + 1
   if (jp1 > lb) jp1 = jp1 - lb
   ptrv = iv + (j - 1) * n + 1
   ptrw = iv + (jp1 - 1) * n + 1
   if (.not. full) then
      if (j > jp1) full = .true.
   end if
   if (full) then
      j0 = jp1 + 1
      if (j0 > lb) j0 = j0 - lb
   else
      j0 = 1
   end if
!
!     request the caller to perform matrix-vector multiplication and
!     preconditioning
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = ptrv
      ipar(9) = ptrv + iw - iv
      ipar(10) = 3
      return
   else
      do i = 0, n - 1
         w(ptrv + iw - iv + i) = w(ptrv + i)
      end do
   end if
!
40 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = ptrv
   end if
   if (lp) then
      ipar(9) = 1
   else
      ipar(9) = ptrw
   end if
   ipar(10) = 4
   return
!
50 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = ptrw
      ipar(10) = 5
      return
   end if
!
!     compute the last column of the Hessenberg matrix
!     modified Gram-schmidt procedure, orthogonalize against (lb-1)
!     previous vectors
!
60 continue
   call mgsro(full, n, n, lb, jp1, fpar(11), w(iv + 1), w(ihm + 1), ipar(12))
   if (ipar(12) < 0) then
      ipar(1) = -3
      goto 80
   end if
   beta = w(ihm + jp1)
!
!     incomplete factorization (QR factorization through Givens rotations)
!     (1) apply previous rotations [(lb-1) of them]
!     (2) generate a new rotation
!
   if (full) then
      w(ihm + jp1) = w(ihm + j0) * w(is + jp1)
      w(ihm + j0) = w(ihm + j0) * w(ic + jp1)
   end if
   i = j0
   do while (i /= j)
      k = i + 1
      if (k > lb) k = k - lb
!        c = w(ic+i)
      s = w(is + i)
      alpha = w(ihm + i)
!        w(ihm+i) = c * alpha + s * w(ihm+k)
!        w(ihm+k) = c * w(ihm+k) - s * alpha
      i = k
   end do
   call givens(w(ihm + j), beta, c, s)
   if (full) then
      fpar(11) = fpar(11) + 6 * lb
   else
      fpar(11) = fpar(11) + 6 * j
   end if
!
!     detect whether diagonal element of this column is zero
!
   if (abs(w(ihm + j)) < deps) then
      ipar(1) = -3
      goto 80
   end if
   w(ihd + j) = one / w(ihm + j)
!     w(ic+j) = c
   w(is + j) = s
!
!     update the W's (the conjugate directions) -- essentially this is one
!     step of triangular solve.
!
   ptrw = iw + (j - 1) * n + 1
   if (full) then
      do i = j + 1, lb
         alpha = -w(ihm + i) * w(ihd + i)
         ptr = iw + (i - 1) * n + 1
         do ii = 0, n - 1
            w(ptrw + ii) = w(ptrw + ii) + alpha * w(ptr + ii)
         end do
      end do
   end if
   do i = 1, j - 1
      alpha = -w(ihm + i) * w(ihd + i)
      ptr = iw + (i - 1) * n + 1
      do ii = 0, n - 1
         w(ptrw + ii) = w(ptrw + ii) + alpha * w(ptr + ii)
      end do
   end do
!
!     update the solution to the linear system
!
!     alpha = psi * c * w(ihd+j)
   psi = -s * psi
   do i = 1, n
      sol(i) = sol(i) + alpha * w(ptrw - 1 + i)
   end do
   if (full) then
      fpar(11) = fpar(11) + lb * (n + n)
   else
      fpar(11) = fpar(11) + j * (n + n)
   end if
!
!     determine whether to continue,
!     compute the desired error/residual norm
!
   ipar(7) = ipar(7) + 1
   fpar(5) = abs(psi)
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = -1
      ipar(9) = 1
      ipar(10) = 6
      return
   end if
   if (ipar(3) < 0) then
      alpha = abs(alpha)
      if (ipar(7) == 2 .and. ipar(3) == -1) then
         fpar(3) = alpha * sqrt(ddot(n, w(ptrw), 1, w(ptrw), 1))
         fpar(4) = fpar(1) * fpar(3) + fpar(2)
         fpar(6) = fpar(3)
      else
         fpar(6) = alpha * sqrt(ddot(n, w(ptrw), 1, w(ptrw), 1))
      end if
      fpar(11) = fpar(11) + 2 * n
   else
      fpar(6) = fpar(5)
   end if
   if (ipar(1) >= 0 .and. fpar(6) > fpar(4) .and. (ipar(6) <= 0 .or. ipar(7) < ipar(6))) goto 30
70 if (ipar(3) == 999 .and. ipar(11) == 0) goto 30
!
!     clean up the iterative solver
!
80 fpar(7) = zero
   if (fpar(3) /= zero .and. fpar(6) /= zero .and. ipar(7) > ipar(13)) fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7) - ipar(13))
   if (ipar(1) > 0) then
      if (ipar(3) == 999 .and. ipar(11) /= 0) then
         ipar(1) = 0
      else if (fpar(6) <= fpar(4)) then
         ipar(1) = 0
      else if (ipar(6) > 0 .and. ipar(7) >= ipar(6)) then
         ipar(1) = -1
      else
         ipar(1) = -10
      end if
   end if
   return
end subroutine dqgmres

subroutine fgmres(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(*)
!-----------------------------------------------------------------------
!     This a version of FGMRES implemented with reverse communication.
!
!     ipar(5) == the dimension of the Krylov subspace
!
!     the space of the `w' is used as follows:
!     >> V: the bases for the Krylov subspace, size n*(m+1);
!     >> W: the above bases after (left-)multiplying with the
!     right-preconditioner inverse, size m*n;
!     >> a temporary vector of size n;
!     >> the Hessenberg matrix, only the upper triangular portion
!     of the matrix is stored, size (m+1)*m/2 + 1
!     >> three vectors, first two are of size m, they are the cosine
!     and sine of the Givens rotations, the third one holds the
!     residuals, it is of size m+1.
!
!     TOTAL SIZE REQUIRED == n*(2m+1) + (m+1)*m/2 + 3*m + 2
!     Note: m == ipar(5). The default value for this is 15 if
!     ipar(5) <= 1.
!-----------------------------------------------------------------------
!     external functions used
!
   EXTERNAL_DDOT
!
   double precision :: one, zero
   parameter(one=1.0d0, zero=0.0d0)
!
!     local variables, ptr and p2 are temporary pointers,
!     hess points to the Hessenberg matrix,
!     vs point to the sines of the Givens rotations
!     vrn points to the vectors of residual norms, more precisely
!     the right hand side of the least square problem solved.
!
   integer i, ii, idx, iz, k, m, ptr, p2, hess, vs, vrn
   double precision :: alpha, c, s
   logical lp, rp
   save
!
!     check the status of the call
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 10
   else if (ipar(10) == 2) then
      goto 20
   else if (ipar(10) == 3) then
      goto 30
   else if (ipar(10) == 4) then
      goto 40
   else if (ipar(10) == 5) then
      goto 50
   else if (ipar(10) == 6) then
      goto 60
   end if
!
!     initialization
!
   if (ipar(5) <= 1) then
      m = 15
   else
      m = ipar(5)
   end if
   idx = n * (m + 1)
   iz = idx + n
   hess = iz + n * m
!     vc = hess + (m+1) * m / 2 + 1
!     vs = vc + m
   vrn = vs + m
   i = vrn + m + 1
   call bisinit(ipar, fpar, i, 1, lp, rp, w)
   if (ipar(1) < 0) return
!
!     request for matrix vector multiplication A*x in the initialization
!
100 ipar(1) = 1
   ipar(8) = n + 1
   ipar(9) = 1
   ipar(10) = 1
   k = 0
   do ii = 1, n
      w(ii + n) = sol(ii)
   end do
   return
10 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   fpar(11) = fpar(11) + n
   if (lp) then
      do i = 1, n
         w(n + i) = rhs(i) - w(i)
      end do
      ipar(1) = 3
      ipar(10) = 2
      return
   else
      do i = 1, n
         w(i) = rhs(i) - w(i)
      end do
   end if
!
20 alpha = sqrt(ddot(n, w, 1, w, 1))
   fpar(11) = fpar(11) + n + n
   if (ipar(7) == 1 .and. ipar(3) /= 999) then
      if (abs(ipar(3)) == 2) then
         fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
         fpar(11) = fpar(11) + 2 * n
      else
         fpar(4) = fpar(1) * alpha + fpar(2)
      end if
      fpar(3) = alpha
   end if
   fpar(5) = alpha
   w(vrn + 1) = alpha
   if (alpha <= fpar(4) .and. ipar(3) >= 0 .and. ipar(3) /= 999) then
      ipar(1) = 0
      fpar(6) = alpha
      goto 300
   end if
   alpha = one / alpha
   do ii = 1, n
      w(ii) = w(ii) * alpha
   end do
   fpar(11) = fpar(11) + n
!
!     request for (1) right preconditioning
!     (2) matrix vector multiplication
!     (3) left preconditioning
!
110 k = k + 1
   if (rp) then
      ipar(1) = 5
      ipar(8) = k * n - n + 1
      ipar(9) = iz + ipar(8)
      ipar(10) = 3
      return
   else
      do ii = 0, n - 1
         w(iz + k * n - ii) = w(k * n - ii)
      end do
   end if
!
30 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = (k - 1) * n + 1
   end if
   if (lp) then
      ipar(9) = idx + 1
   else
      ipar(9) = 1 + k * n
   end if
   ipar(10) = 4
   return
!
40 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = k * n + 1
      ipar(10) = 5
      return
   end if
!
!     Modified Gram-Schmidt orthogonalization procedure
!     temporary pointer 'ptr' is pointing to the current column of the
!     Hessenberg matrix. 'p2' points to the new basis vector
!
50 ptr = k * (k - 1) / 2 + hess
   p2 = ipar(9)
   ipar(7) = ipar(7) + 1
   call mgsro(.false., n, n, k + 1, k + 1, fpar(11), w, w(ptr + 1), ipar(12))
   if (ipar(12) < 0) goto 200
!
!     apply previous Givens rotations and generate a new one to eliminate
!     the subdiagonal element.
!
   p2 = ptr + 1
   do i = 1, k - 1
      ptr = p2
      p2 = p2 + 1
      alpha = w(ptr)
!        c = w(vc+i)
      s = w(vs + i)
!        w(ptr) = c * alpha + s * w(p2)
!        w(p2) = c * w(p2) - s * alpha
   end do
   call givens(w(p2), w(p2 + 1), c, s)
!     w(vc+k) = c
   w(vs + k) = s
   p2 = vrn + k
   alpha = -s * w(p2)
!     w(p2) = c * w(p2)
   w(p2 + 1) = alpha
   fpar(11) = fpar(11) + 6 * k
!
!     end of one Arnoldi iteration, alpha will store the estimated
!     residual norm at current stage
!
   alpha = abs(alpha)
   fpar(5) = alpha
   if (k < m .and. .not. (ipar(3) >= 0 .and. alpha <= fpar(4)) &
       .and. (ipar(6) <= 0 .or. ipar(7) < ipar(6))) goto 110
!
!     update the approximate solution, first solve the upper triangular
!     system, temporary pointer ptr points to the Hessenberg matrix,
!     p2 points to the right-hand-side (also the solution) of the system.
!
200 ptr = hess + k * (k + 1) / 2
   p2 = vrn + k
   if (w(ptr) == zero) then
!
!     if the diagonal elements of the last column is zero, reduce k by 1
!     so that a smaller trianguler system is solved [It should only
!     happen when the matrix is singular!]
!
      k = k - 1
      if (k > 0) then
         goto 200
      else
         ipar(1) = -3
         ipar(12) = -4
         goto 300
      end if
   end if
   w(p2) = w(p2) / w(ptr)
   do i = k - 1, 1, -1
      ptr = ptr - i - 1
      do ii = 1, i
         w(vrn + ii) = w(vrn + ii) - w(p2) * w(ptr + ii)
      end do
      p2 = p2 - 1
      w(p2) = w(p2) / w(ptr)
   end do
!
   do i = 0, k - 1
      ptr = iz + i * n
      do ii = 1, n
         sol(ii) = sol(ii) + w(p2) * w(ptr + ii)
      end do
      p2 = p2 + 1
   end do
   fpar(11) = fpar(11) + 2 * k * n + k * (k + 1)
!
!     process the complete stopping criteria
!
   if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = -1
      ipar(9) = idx + 1
      ipar(10) = 6
      return
   else if (ipar(3) < 0) then
      if (ipar(7) <= m + 1) then
         fpar(3) = abs(w(vrn + 1))
         if (ipar(3) == -1) fpar(4) = fpar(1) * fpar(3) + fpar(2)
      end if
      fpar(6) = abs(w(vrn + k))
   else if (ipar(3) /= 999) then
      fpar(6) = fpar(5)
   end if
!
!     do we need to restart ?
!
60 if (ipar(12) /= 0) then
      ipar(1) = -3
      goto 300
   end if
   if ((ipar(7) < ipar(6) .or. ipar(6) <= 0) .and. &
       ((ipar(3) == 999 .and. ipar(11) == 0) .or. &
        (ipar(3) /= 999 .and. fpar(6) > fpar(4)))) goto 100
!
!     termination, set error code, compute convergence rate
!
   if (ipar(1) > 0) then
      if (ipar(3) == 999 .and. ipar(11) == 1) then
         ipar(1) = 0
      else if (ipar(3) /= 999 .and. fpar(6) <= fpar(4)) then
         ipar(1) = 0
      else if (ipar(7) >= ipar(6) .and. ipar(6) > 0) then
         ipar(1) = -1
      else
         ipar(1) = -10
      end if
   end if
300 if (fpar(3) /= zero .and. fpar(6) /= zero .and. &
       ipar(7) > ipar(13)) then
      fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7) - ipar(13))
   else
      fpar(7) = zero
   end if
   return
end subroutine fgmres

subroutine dbcg(n, rhs, sol, ipar, fpar, w)
   implicit none
   integer n, ipar(16)
   double precision :: rhs(n), sol(n), fpar(16), w(n, *)
!-----------------------------------------------------------------------
! Quasi GMRES method for solving a linear
! system of equations a * sol = y.  double precision version.
! this version is without restarting and without preconditioning.
! parameters :
! -----------
! n     = dimension of the problem
!
! y     = w(:,1) a temporary storage used for various operations
! z     = w(:,2) a work vector of length n.
! v     = w(:,3:4) size n x 2
! w     = w(:,5:6) size n x 2
! p     = w(:,7:9) work array of dimension n x 3
! del x = w(:,10)  accumulation of the changes in solution
! tmp   = w(:,11)  a temporary vector used to hold intermediate result of
!                  preconditioning, etc.
!
! sol   = the solution of the problem . at input sol must contain an
!         initial guess to the solution.
!    ***  note:   y is destroyed on return.
!
!-----------------------------------------------------------------------
! subroutines and functions called:
! 1) matrix vector multiplication and preconditioning through reverse
!     communication
!
! 2) implu, uppdir (blas)
!-----------------------------------------------------------------------
! aug. 1983  version.    author youcef saad. yale university computer
! science dept. some  changes made july 3, 1986.
! references: siam j. sci. stat. comp., vol. 5, pp. 203-228 (1984)
!-----------------------------------------------------------------------
!     local variables
!
   double precision :: one, zero
   parameter(one=1.0d0, zero=0.0d0)
!
   double precision :: t, sqrt, ss, res, beta, ss1, delta, x, zeta, umm
   integer k, j, i, i2, ip2, ju, lb, lbm1, np, indp
   logical lp, rp, full, perm(3)
   double precision :: ypiv(3), u(3), usav(3)
   external tidycg
   EXTERNAL_DDOT
   save
!
!     where to go
!
   if (ipar(1) <= 0) ipar(10) = 0
   if (ipar(10) == 1) then
      goto 110
   else if (ipar(10) == 2) then
      goto 120
   else if (ipar(10) == 3) then
      goto 130
   else if (ipar(10) == 4) then
      goto 140
   else if (ipar(10) == 5) then
      goto 150
   else if (ipar(10) == 6) then
      goto 160
   else if (ipar(10) == 7) then
      goto 170
   else if (ipar(10) == 8) then
      goto 180
   else if (ipar(10) == 9) then
      goto 190
   else if (ipar(10) == 10) then
      goto 200
   end if
!
!     initialization, parameter checking, clear the work arrays
!
   call bisinit(ipar, fpar, 11 * n, 1, lp, rp, w)
   if (ipar(1) < 0) return
   perm(1) = .false.
   perm(2) = .false.
   perm(3) = .false.
   usav(1) = zero
   usav(2) = zero
   usav(3) = zero
   ypiv(1) = zero
   ypiv(2) = zero
   ypiv(3) = zero
!-----------------------------------------------------------------------
!     initialize constants for outer loop :
!-----------------------------------------------------------------------
   lb = 3
   lbm1 = 2
!
!     get initial residual vector and norm
!
   ipar(1) = 1
   ipar(8) = 1
   ipar(9) = 1 + n
   do i = 1, n
      w(i, 1) = sol(i)
   end do
   ipar(10) = 1
   return
110 ipar(7) = ipar(7) + 1
   ipar(13) = ipar(13) + 1
   if (lp) then
      do i = 1, n
         w(i, 1) = rhs(i) - w(i, 2)
      end do
      ipar(1) = 3
      ipar(8) = 1
      ipar(9) = n + n + 1
      ipar(10) = 2
      return
   else
      do i = 1, n
         w(i, 3) = rhs(i) - w(i, 2)
      end do
   end if
   fpar(11) = fpar(11) + n
!
120 fpar(3) = sqrt(ddot(n, w(1, 3), 1, w(1, 3), 1))
   fpar(11) = fpar(11) + n + n
   fpar(5) = fpar(3)
   fpar(7) = fpar(3)
   zeta = fpar(3)
   if (abs(ipar(3)) == 2) then
      fpar(4) = fpar(1) * sqrt(ddot(n, rhs, 1, rhs, 1)) + fpar(2)
      fpar(11) = fpar(11) + 2 * n
   else if (ipar(3) /= 999) then
      fpar(4) = fpar(1) * zeta + fpar(2)
   end if
   if (ipar(3) >= 0 .and. fpar(5) <= fpar(4)) then
      fpar(6) = fpar(5)
      goto 900
   end if
!
!     normalize first arnoldi vector
!
   t = one / zeta
   do k = 1, n
      w(k, 3) = w(k, 3) * t
      w(k, 5) = w(k, 3)
   end do
   fpar(11) = fpar(11) + n
!
!     initialize constants for main loop
!
   beta = zero
   delta = zero
   i2 = 1
   indp = 0
   i = 0
!
!     main loop: i = index of the loop.
!
!-----------------------------------------------------------------------
30 i = i + 1
!
   if (rp) then
      ipar(1) = 5
      ipar(8) = (1 + i2) * n + 1
      if (lp) then
         ipar(9) = 1
      else
         ipar(9) = 10 * n + 1
      end if
      ipar(10) = 3
      return
   end if
!
130 ipar(1) = 1
   if (rp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = (1 + i2) * n + 1
   end if
   if (lp) then
      ipar(9) = 10 * n + 1
   else
      ipar(9) = 1
   end if
   ipar(10) = 4
   return
!
140 if (lp) then
      ipar(1) = 3
      ipar(8) = ipar(9)
      ipar(9) = 1
      ipar(10) = 5
      return
   end if
!
!     A^t * x
!
150 ipar(7) = ipar(7) + 1
   if (lp) then
      ipar(1) = 4
      ipar(8) = (3 + i2) * n + 1
      if (rp) then
         ipar(9) = n + 1
      else
         ipar(9) = 10 * n + 1
      end if
      ipar(10) = 6
      return
   end if
!
160 ipar(1) = 2
   if (lp) then
      ipar(8) = ipar(9)
   else
      ipar(8) = (3 + i2) * n + 1
   end if
   if (rp) then
      ipar(9) = 10 * n + 1
   else
      ipar(9) = n + 1
   end if
   ipar(10) = 7
   return
!
170 if (rp) then
      ipar(1) = 6
      ipar(8) = ipar(9)
      ipar(9) = n + 1
      ipar(10) = 8
      return
   end if
!-----------------------------------------------------------------------
!     orthogonalize current v against previous v's and
!     determine relevant part of i-th column of u(.,.) the
!     upper triangular matrix --
!-----------------------------------------------------------------------
180 ipar(7) = ipar(7) + 1
   u(1) = zero
   ju = 1
   k = i2
   if (i <= lbm1) ju = 0
   if (i < lb) k = 0
31 if (k == lbm1) k = 0
   k = k + 1
!
   if (k /= i2) then
      ss = delta
      ss1 = beta
      ju = ju + 1
      u(ju) = ss
   else
      ss = ddot(n, w(1, 1), 1, w(1, 4 + k), 1)
      fpar(11) = fpar(11) + 2 * n
      ss1 = ss
      ju = ju + 1
      u(ju) = ss
   end if
!
   do j = 1, n
      w(j, 1) = w(j, 1) - ss * w(j, k + 2)
      w(j, 2) = w(j, 2) - ss1 * w(j, k + 4)
   end do
   fpar(11) = fpar(11) + 4 * n
!
   if (k /= i2) goto 31
!
!     end of Mod. Gram. Schmidt loop
!
   t = ddot(n, w(1, 2), 1, w(1, 1), 1)
!
   beta = sqrt(abs(t))
   delta = t / beta
!
   ss = one / beta
   ss1 = one / delta
!
!     normalize and insert new vectors
!
   ip2 = i2
   if (i2 == lbm1) i2 = 0
   i2 = i2 + 1
!
   do j = 1, n
      w(j, i2 + 2) = w(j, 1) * ss
      w(j, i2 + 4) = w(j, 2) * ss1
   end do
   fpar(11) = fpar(11) + 4 * n
!-----------------------------------------------------------------------
!     end of orthogonalization.
!     now compute the coefficients u(k) of the last
!     column of the  l . u  factorization of h .
!-----------------------------------------------------------------------
   np = min(i, lb)
   full = (i >= lb)
   call implu(np, umm, beta, ypiv, u, perm, full)
!-----------------------------------------------------------------------
!     update conjugate directions and solution
!-----------------------------------------------------------------------
   do k = 1, n
      w(k, 1) = w(k, ip2 + 2)
   end do
   call uppdir(n, w(1, 7), np, lb, indp, w, u, usav, fpar(11))
!-----------------------------------------------------------------------
   if (i == 1) goto 34
   j = np - 1
   if (full) j = j - 1
   if (.not. perm(j)) zeta = -zeta * ypiv(j)
34 x = zeta / u(np)
   if (perm(np)) goto 36
   do k = 1, n
      w(k, 10) = w(k, 10) + x * w(k, 1)
   end do
   fpar(11) = fpar(11) + 2 * n
!-----------------------------------------------------------------------
36 if (ipar(3) == 999) then
      ipar(1) = 10
      ipar(8) = 9 * n + 1
      ipar(9) = 10 * n + 1
      ipar(10) = 9
      return
   end if
   res = abs(beta * zeta / umm)
   fpar(5) = res * sqrt(ddot(n, w(1, i2 + 2), 1, w(1, i2 + 2), 1))
   fpar(11) = fpar(11) + 2 * n
   if (ipar(3) < 0) then
      fpar(6) = x * sqrt(ddot(n, w, 1, w, 1))
      fpar(11) = fpar(11) + 2 * n
      if (ipar(7) <= 3) then
         fpar(3) = fpar(6)
         if (ipar(3) == -1) then
            fpar(4) = fpar(1) * sqrt(fpar(3)) + fpar(2)
         end if
      end if
   else
      fpar(6) = fpar(5)
   end if
!---- convergence test -----------------------------------------------
190 if (ipar(3) == 999 .and. ipar(11) == 0) then
      goto 30
   else if (fpar(6) > fpar(4) .and. (ipar(6) > ipar(7) .or. &
                                     ipar(6) <= 0)) then
      goto 30
   end if
!-----------------------------------------------------------------------
!     here the fact that the last step is different is accounted for.
!-----------------------------------------------------------------------
   if (.not. perm(np)) goto 900
   x = zeta / umm
   do k = 1, n
      w(k, 10) = w(k, 10) + x * w(k, 1)
   end do
   fpar(11) = fpar(11) + 2 * n
!
!     right preconditioning and clean-up jobs
!
900 if (rp) then
      if (ipar(1) < 0) ipar(12) = ipar(1)
      ipar(1) = 5
      ipar(8) = 9 * n + 1
      ipar(9) = ipar(8) + n
      ipar(10) = 10
      return
   end if
200 if (rp) then
      call tidycg(n, ipar, fpar, sol, w(1, 11))
   else
      call tidycg(n, ipar, fpar, sol, w(1, 10))
   end if
   return
end subroutine dbcg

subroutine implu(np, umm, beta, ypiv, u, permut, full)
   implicit none

   double precision :: umm, beta, ypiv(*), u(*), x, xpiv
   logical full, perm, permut(*)
   integer np, k, npm1
!-----------------------------------------------------------------------
!     performs implicitly one step of the lu factorization of a
!     banded hessenberg matrix.
!-----------------------------------------------------------------------
   if (np <= 1) goto 12
   npm1 = np - 1
!
!     -- perform  previous step of the factorization-
!
   do k = 1, npm1
      if (.not. permut(k)) goto 5
      x = u(k)
      u(k) = u(k + 1)
      u(k + 1) = x
5     u(k + 1) = u(k + 1) - ypiv(k) * u(k)
   end do
!-----------------------------------------------------------------------
!     now determine pivotal information to be used in the next call
!-----------------------------------------------------------------------
12 umm = u(np)
   perm = (beta > abs(umm))
   if (.not. perm) goto 4
   xpiv = umm / beta
   u(np) = beta
   goto 8
4  xpiv = beta / umm
8  permut(np) = perm
   ypiv(np) = xpiv
   if (.not. full) return
!     shift everything up if full...
   do k = 1, npm1
      ypiv(k) = ypiv(k + 1)
      permut(k) = permut(k + 1)
   end do
   return
end subroutine implu

subroutine uppdir(n, p, np, lbp, indp, y, u, usav, flops)
   implicit none

   double precision :: p(n, lbp), y(*), u(*), usav(*), x, flops
   integer k, np, n, npm1, j, ju, indp, lbp
!-----------------------------------------------------------------------
!     updates the conjugate directions p given the upper part of the
!     banded upper triangular matrix u.  u contains the non zero
!     elements of the column of the triangular matrix..
!-----------------------------------------------------------------------
   double precision :: zero
   parameter(zero=0.0d0)
!
   npm1 = np - 1
   if (np <= 1) goto 12
   j = indp
   ju = npm1
10 if (j <= 0) j = lbp
   x = u(ju) / usav(j)
   if (x == zero) goto 115
   do k = 1, n
      y(k) = y(k) - x * p(k, j)
   end do
   flops = flops + 2 * n
115 j = j - 1
   ju = ju - 1
   if (ju >= 1) goto 10
12 indp = indp + 1
   if (indp > lbp) indp = 1
   usav(indp) = u(np)
   do k = 1, n
      p(k, indp) = y(k)
   end do
208 return
end subroutine uppdir

subroutine givens(x, y, c, s)
   implicit none

   double precision :: x, y, c, s
!-----------------------------------------------------------------------
!     Given x and y, this subroutine generates a Givens' rotation c, s.
!     And apply the rotation on (x,y) ==> (sqrt(x**2 + y**2), 0).
!     (See P 202 of "matrix computation" by Golub and van Loan.)
!-----------------------------------------------------------------------
   double precision :: t, one, zero
   parameter(zero=0.0d0, one=1.0d0)
!
   no_warning_unused_dummy_argument(c)
   if (x == zero .and. y == zero) then
!        c = one
      s = zero
   else if (abs(y) > abs(x)) then
      t = x / y
      x = sqrt(one + t * t)
      s = sign(one / x, y)
!        c = t*s
   else if (abs(y) <= abs(x)) then
      t = y / x
      y = sqrt(one + t * t)
!        c = sign(one / y, x)
!        s = t*c
   else
!
!     X or Y must be an invalid floating-point number, set both to zero
!
      x = zero
      y = zero
!        c = one
      s = zero
   end if
   x = abs(x * y)
!
!     end of givens
!
   return
end subroutine givens

logical function stopbis(n, ipar, mvpi, fpar, r, delx, sx)
   implicit none
   integer n, mvpi, ipar(16)
   double precision :: fpar(16), r(n), delx(n), sx
   EXTERNAL_DDOT
!-----------------------------------------------------------------------
!     function for determining the stopping criteria. return value of
!     true if the stopbis criteria is satisfied.
!-----------------------------------------------------------------------
   if (ipar(11) == 1) then
      stopbis = .true.
   else
      stopbis = .false.
   end if
   if (ipar(6) > 0 .and. ipar(7) >= ipar(6)) then
      ipar(1) = -1
      stopbis = .true.
   end if
   if (stopbis) return
!
!     computes errors
!
   fpar(5) = sqrt(ddot(n, r, 1, r, 1))
   fpar(11) = fpar(11) + 2 * n
   if (ipar(3) < 0) then
!
!     compute the change in the solution vector
!
      fpar(6) = sx * sqrt(ddot(n, delx, 1, delx, 1))
      fpar(11) = fpar(11) + 2 * n
      if (ipar(7) < mvpi + mvpi + 1) then
!
!     if this is the end of the first iteration, set fpar(3:4)
!
         fpar(3) = fpar(6)
         if (ipar(3) == -1) then
            fpar(4) = fpar(1) * fpar(3) + fpar(2)
         end if
      end if
   else
      fpar(6) = fpar(5)
   end if
!
!     .. the test is struct this way so that when the value in fpar(6)
!       is not a valid number, STOPBIS is set to .true.
!
   if (fpar(6) > fpar(4)) then
      stopbis = .false.
      ipar(11) = 0
   else
      stopbis = .true.
      ipar(11) = 1
   end if
!
   return
end function stopbis

subroutine tidycg(n, ipar, fpar, sol, delx)
   implicit none
   integer n, ipar(16)
   double precision :: fpar(16), sol(n), delx(n)
!-----------------------------------------------------------------------
!     Some common operations required before terminating the CG routines
!-----------------------------------------------------------------------
   double precision :: zero
   parameter(zero=0.0d0)
!
   if (ipar(12) /= 0) then
      ipar(1) = ipar(12)
   else if (ipar(1) > 0) then
      if ((ipar(3) == 999 .and. ipar(11) == 1) .or. &
          fpar(6) <= fpar(4)) then
         ipar(1) = 0
      else if (ipar(7) >= ipar(6) .and. ipar(6) > 0) then
         ipar(1) = -1
      else
         ipar(1) = -10
      end if
   end if
   if (fpar(3) > zero .and. fpar(6) > zero .and. ipar(7) > ipar(13)) then
      fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7) - ipar(13))
   else
      fpar(7) = zero
   end if
!      do i = 1, n
!         sol(i) = sol(i) + delx(i)
!      enddo
   SOL = SOL + DELX
   return
end subroutine tidycg

logical function brkdn(alpha, ipar)
   implicit none
   integer ipar(16)
   double precision :: alpha, beta, zero, one
   parameter(zero=0.0d0, one=1.0d0)
!-----------------------------------------------------------------------
!     test whether alpha is zero or an abnormal number, if yes,
!     this routine will return .true.
!
!     If alpha == 0, ipar(1) = -3,
!     if alpha is an abnormal number, ipar(1) = -9.
!-----------------------------------------------------------------------
   brkdn = .false.
   if (alpha > zero) then
      beta = one / alpha
      if (.not. beta > zero) then
         brkdn = .true.
         ipar(1) = -9
      end if
   else if (alpha < zero) then
      beta = one / alpha
      if (.not. beta < zero) then
         brkdn = .true.
         ipar(1) = -9
      end if
   else if (alpha == zero) then
      brkdn = .true.
      ipar(1) = -3
   else
      brkdn = .true.
      ipar(1) = -9
   end if
   return
end function brkdn

subroutine bisinit(ipar, fpar, wksize, dsc, lp, rp, wk)
   implicit none
   integer i, ipar(16), wksize, dsc
   logical lp, rp
   double precision :: fpar(16), wk(*)
!-----------------------------------------------------------------------
!     some common initializations for the iterative solvers
!-----------------------------------------------------------------------
   double precision :: zero, one
   parameter(zero=0.0d0, one=1.0d0)
   no_warning_unused_dummy_argument(dsc)
!
!     ipar(1) = -2 inidcate that there are not enough space in the work
!     array
!
   if (ipar(4) < wksize) then
      ipar(1) = -2
      ipar(4) = wksize
      return
   end if
!
   if (ipar(2) > 2) then
      lp = .true.
      rp = .true.
   else if (ipar(2) == 2) then
      lp = .false.
      rp = .true.
   else if (ipar(2) == 1) then
      lp = .true.
      rp = .false.
   else
      lp = .false.
      rp = .false.
   end if
!     if (ipar(3).eq.0) ipar(3) = dsc
!     .. clear the ipar elements used
   ipar(7) = 0
   ipar(8) = 0
   ipar(9) = 0
   ipar(10) = 0
   ipar(11) = 0
   ipar(12) = 0
   ipar(13) = 0
!
!     fpar(1) must be between (0, 1), fpar(2) must be positive,
!     fpar(1) and fpar(2) can NOT both be zero
!     Normally return ipar(1) = -4 to indicate any of above error
!
   if (fpar(1) < zero .or. fpar(1) >= one .or. fpar(2) < zero .or. (fpar(1) == zero .and. fpar(2) == zero)) then
      if (ipar(1) == 0) then
         ipar(1) = -4
         return
      else
         fpar(1) = 1.0d-6
         fpar(2) = 1.0d-16
      end if
   end if
!     .. clear the fpar elements
   do i = 3, 10
      fpar(i) = zero
   end do
   if (fpar(11) < zero) fpar(11) = zero
!     .. clear the used portion of the work array to zero

   ! do i = 1, wksize
   !    wk(i) = zero
   ! enddo
!
   WK(1:WKSIZE) = ZERO

   return
end subroutine bisinit

subroutine mgsro(full, lda, n, m, ind, ops, vec, hh, ierr)
   implicit none
   logical full
   integer lda, m, n, ind, ierr
   double precision :: ops, hh(m), vec(lda, m)
!-----------------------------------------------------------------------
!     MGSRO  -- Modified Gram-Schmidt procedure with Selective Re-
!               Orthogonalization
!     The ind'th vector of VEC is orthogonalized against the rest of
!     the vectors.
!
!     The test for performing re-orthogonalization is performed for
!     each indivadual vectors. If the cosine between the two vectors
!     is greater than 0.99 (REORTH = 0.99**2), re-orthogonalization is
!     performed. The norm of the 'new' vector is kept in variable NRM0,
!     and updated after operating with each vector.
!
!     full   -- .ture. if it is necessary to orthogonalize the ind'th
!               against all the vectors vec(:,1:ind-1), vec(:,ind+2:m)
!               .false. only orthogonalize againt vec(:,1:ind-1)
!     lda    -- the leading dimension of VEC
!     n      -- length of the vector in VEC
!     m      -- number of vectors can be stored in VEC
!     ind    -- index to the vector to be changed
!     ops    -- operation counts
!     vec    -- vector of LDA X M storing the vectors
!     hh     -- coefficient of the orthogonalization
!     ierr   -- error code
!               0 : successful return
!               -1: zero input vector
!               -2: input vector contains abnormal numbers
!               -3: input vector is a linear combination of others
!
!     External routines used: double precision ::  ddot
!-----------------------------------------------------------------------
   integer i, k
   double precision :: nrm0, nrm1, fct, thr, zero, one, reorth
   parameter(zero=0.0d0, one=1.0d0, reorth=0.98d0)
   EXTERNAL_DDOT
!
!     compute the norm of the input vector
!
   nrm0 = ddot(n, vec(1, ind), 1, vec(1, ind), 1)
   ops = ops + n + n
   thr = nrm0 * reorth
   if (nrm0 <= zero) then
      ierr = -1
      return
   else if (nrm0 > zero .and. one / nrm0 > zero) then
      ierr = 0
   else
      ierr = -2
      return
   end if
!
!     Modified Gram-Schmidt loop
!
   if (full) then
      do i = ind + 1, m
         fct = ddot(n, vec(1, ind), 1, vec(1, i), 1)
         hh(i) = fct
         do k = 1, n
            vec(k, ind) = vec(k, ind) - fct * vec(k, i)
         end do
         ops = ops + 4 * n + 2
         if (fct * fct > thr) then
            fct = ddot(n, vec(1, ind), 1, vec(1, i), 1)
            hh(i) = hh(i) + fct
            do k = 1, n
               vec(k, ind) = vec(k, ind) - fct * vec(k, i)
            end do
            ops = ops + 4 * n + 1
         end if
         nrm0 = nrm0 - hh(i) * hh(i)
         if (nrm0 < zero) nrm0 = zero
         thr = nrm0 * reorth
      end do
   end if
!
   do i = 1, ind - 1
      fct = ddot(n, vec(1, ind), 1, vec(1, i), 1)
      hh(i) = fct
      do k = 1, n
         vec(k, ind) = vec(k, ind) - fct * vec(k, i)
      end do
      ops = ops + 4 * n + 2
      if (fct * fct > thr) then
         fct = ddot(n, vec(1, ind), 1, vec(1, i), 1)
         hh(i) = hh(i) + fct
         do k = 1, n
            vec(k, ind) = vec(k, ind) - fct * vec(k, i)
         end do
         ops = ops + 4 * n + 1
      end if
      nrm0 = nrm0 - hh(i) * hh(i)
      if (nrm0 < zero) nrm0 = zero
      thr = nrm0 * reorth
   end do
!
!     test the resulting vector
!
   nrm1 = sqrt(ddot(n, vec(1, ind), 1, vec(1, ind), 1))
   ops = ops + n + n
75 hh(ind) = nrm1
   if (nrm1 <= zero) then
      ierr = -3
      return
   end if
!
!     scale the resulting vector
!
   fct = one / nrm1
   do k = 1, n
      vec(k, ind) = vec(k, ind) * fct
   end do
   ops = ops + n + 1
!
!     normal return
!
   ierr = 0
   return
end subroutine mgsro

subroutine entline(outf, mat, its, kk)
   implicit none
   integer outf, kk, its(kk), k
   character mat * 70
!-----------------------------------------------------------------------
   write (outf, 100) mat(19:29), (its(k), k=1, 8)
100 format(a, ' & ', 8(i4, ' & '), '\\\\ \\hline')
   return
end subroutine entline

subroutine stb_test(n, sol, alu, jlu, ju, tmax)
   implicit none
   integer n, jlu(*), ju(*)
   double precision :: sol(n), alu(*), tmax
!-----------------------------------------------------------------------
   double precision :: max, t, abs
   integer j
!
   call lusol(n, sol, sol, alu, jlu, ju, 30 * n)
!
   tmax = 0.0
   do j = 1, n
      t = abs(sol(j))
      tmax = max(t, tmax)
   end do
   return
end subroutine stb_test

subroutine xyk(nel, xyke, x, y, ijk, node)
   use precision_basics, only: dp

   implicit none

   integer :: nel, ijk(:, :), node
   real(dp) :: xyke(2, 2), x(:), y(:)
   no_warning_unused_dummy_argument(nel)
   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(ijk)
   no_warning_unused_dummy_argument(node)
!
!     this is the identity matrix.
!
   xyke(1, 1) = 1.0d0
   xyke(2, 2) = 1.0d0
   xyke(1, 2) = 0.0d0
   xyke(2, 1) = 0.0d0

   return
end subroutine xyk
!----------------------------------------------------------------------c
!                          S P A R S K I T                             c
!----------------------------------------------------------------------c
!    MATRIX GENERATION ROUTINES  -- FINITE DIFFERENCE MATRICES         c
!----------------------------------------------------------------------c
! contents:                                                            c
!----------                                                            c
! gen57pt  : generates 5-point and 7-point matrices.                   c
! gen57bl  : generates block 5-point and 7-point matrices.             c
!                                                                      c
! supporting routines:                                                 c
!---------                                                             c
! gensten  : generate the stencil (point version)                      c
! bsten    : generate the stencil (block version)                      c
! fdaddbc  : finite difference add boundary conditions                 c
! fdreduce : reduce the system to eliminate node with known values     c
! clrow    : clear a row of a CSR matrix                               c
! lctcsr   : locate the position of A(i,j) in CSR format               c
!----------------------------------------------------------------------c
subroutine gen57pt(nx, ny, nz, al, mode, n, a, ja, ia, iau, rhs)
   implicit none

   integer ja(*), ia(*), iau(*), nx, ny, nz, mode, n
   double precision :: a(*), rhs(*), al(6)
!-----------------------------------------------------------------------
! On entry:
!
! nx      = number of grid points in x direction
! ny    = number of grid points in y direction
! nz    = number of grid points in z direction
! al      = array of size 6, carries the coefficient alpha of the
!           boundary conditions
! mode    = what to generate:
!           < 0 : generate the graph only,
!           = 0 : generate the matrix,
!           > 0 : generate the matrix and the right-hand side.
!
! On exit:
!
! n       = number of nodes with unknown values, ie number of rows
!           in the matrix
!
! a,ja,ia = resulting matrix in row-sparse format
!
! iau     = integer*n, containing the poisition of the diagonal element
!           in the a, ja, ia structure
!
! rhs     = the right-hand side
!
! External functions needed (must be supplied by caller)
!     afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
!     betfun, gamfun
! They have the following prototype:
!     double precision ::  function xfun(x, y, z)
!     double precision ::  x, y, z
!-----------------------------------------------------------------------
! This subroutine computes the sparse matrix in compressed sparse row
! format for the elliptic equation:
!       d    du    d    du    d    du      du     du     du
! L u = --(A --) + --(B --) + --(C --) + D -- + E -- + F -- + G u = H u
!       dx   dx    dy   dy    dz   dz      dx     dy     dz
!
! with general Mixed Boundary conditions, on a rectangular 1-D,
! 2-D or 3-D grid using 2nd order centered difference schemes.
!
! The functions a, b, ..., g, h are known through the
! as afun, bfun, ..., gfun, hfun in this subroutine.
! NOTE: To obtain the correct matrix, any function that is not
! needed should be set to zero.  For example for two-dimensional
! problems, nz should be set to 1 and the functions cfun and ffun
! should be zero functions.
!
! The Boundary condition is specified in the following form:
!           du
!     alpha -- + beta u = gamma
!           dn
! Where alpha is constant at each side of the boundary surfaces.  Alpha
! is represented by parameter al.  It is expected to an array that
! contains enough elements to specify the boundaries for the problem,
! 1-D case needs two elements, 2-D needs 4 and 3-D needs 6.  The order
! of the boundaries in the array is left(west), right(east),
! bottom(south), top(north), front, rear.  Beta and gamma are functions
! of type real with three arguments x, y, z.  These two functions are
! known subroutine 'addbc' as betfun and gamfun.  They should following
! the same notion as afun ... hfun.  For more restriction on afun ...
! hfun, please read the documentation follows the subroutine 'getsten',
! and, for more on betfun and gamfun, please refer to the documentation
! under subroutine 'fdaddbc'.
!
! The nodes are ordered using natural ordering, first x direction, then
! y, then z.  The mesh size h is uniform and determined by grid points
! in the x-direction.
!
! The domain specified for the problem is [0 .ge. x .ge. 1],
! [0 .ge. y .ge. (ny-1)*h] and [0 .ge. z .ge. (nz-1)*h], where h is
! 1 / (nx-1).  Thus if non-Dirichlet boundary condition is specified,
! the mesh will have nx points along the x direction, ny along y and
! nz along z.  For 1-D case, both y and z value are assumed to zero
! when calling relavent functions that have three parameters.
! Similarly, for 2-D case, z is assumed to be zero.
!
! About the expectation of nx, ny and nz:
! nx is required to be .gt. 1 always;
! if the second dimension is present in the problem, then ny should be
! .gt. 1, else 1;
! if the third dimension is present in the problem, nz .gt. 1, else 1.
! when ny is 1, nz must be 1.
!-----------------------------------------------------------------------
!
!     stencil [1:7] has the following meaning:
!
!     center point = stencil(1)
!     west point = stencil(2)
!     east point = stencil(3)
!     south point = stencil(4)
!     north point = stencil(5)
!     front point = stencil(6)
!     back point = stencil(7)
!
!     al[1:6] carry the coefficient alpha in the similar order
!
!     west  side = al(1)
!     east  side = al(2)
!     south side = al(3)
!     north side = al(4)
!     front side = al(5)
!     back  side = al(6)
!
!                           al(4)
!                           st(5)
!                            |
!                            |
!                            |           al(6)
!                            |          .st(7)
!                            |     .
!         al(1)              | .             al(2)
!         st(2) ----------- st(1) ---------- st(3)
!                       .    |
!                   .        |
!               .            |
!            st(6)           |
!            al(5)           |
!                            |
!                           st(4)
!                           al(3)
!
!-------------------------------------------------------------------
!     some constants
!
   double precision :: one
   parameter(one=1.0d0)
!
!     local variables
!
   integer ix, iy, iz, kx, ky, kz, node, iedge
   double precision :: r, h, stencil(7)
   logical value, genrhs
!
!     nx has to be larger than 1
!
   if (nx <= 1) return
   h = one / dble(nx - 1)
!
!     the mode
!
   value = (mode >= 0)
   genrhs = (mode > 0)
!
!     first generate the whole matrix as if the boundary condition does
!     not exist
!
   kx = 1
   ky = nx
   kz = nx * ny
   iedge = 1
   node = 1
   do iz = 1, nz
      do iy = 1, ny
         do ix = 1, nx
            ia(node) = iedge
!
!     compute the stencil at the current node
!
            if (value) call getsten(nx, ny, nz, mode, ix - 1, iy - 1, iz - 1, stencil, h, r)
!     west
            if (ix > 1) then
               ja(iedge) = node - kx
               if (value) a(iedge) = stencil(2)
               iedge = iedge + 1
            end if
!     south
            if (iy > 1) then
               ja(iedge) = node - ky
               if (value) a(iedge) = stencil(4)
               iedge = iedge + 1
            end if
!     front plane
            if (iz > 1) then
               ja(iedge) = node - kz
               if (value) a(iedge) = stencil(6)
               iedge = iedge + 1
            end if
!     center node
            ja(iedge) = node
            iau(node) = iedge
            if (value) a(iedge) = stencil(1)
            iedge = iedge + 1
!     east
            if (ix < nx) then
               ja(iedge) = node + kx
               if (value) a(iedge) = stencil(3)
               iedge = iedge + 1
            end if
!     north
            if (iy < ny) then
               ja(iedge) = node + ky
               if (value) a(iedge) = stencil(5)
               iedge = iedge + 1
            end if
!     back plane
            if (iz < nz) then
               ja(iedge) = node + kz
               if (value) a(iedge) = stencil(7)
               iedge = iedge + 1
            end if
!     the right-hand side
            if (genrhs) rhs(node) = r
            node = node + 1
         end do
      end do
   end do
   ia(node) = iedge
!
!     Add in the boundary conditions
!
   call fdaddbc(nx, ny, nz, a, ja, ia, iau, rhs, al, h)
!
!     eliminate the boudary nodes from the matrix
!
   call fdreduce(nx, ny, nz, al, n, a, ja, ia, iau, rhs, stencil)
!
!     done
!
   return
end subroutine gen57pt

subroutine getsten(nx, ny, nz, mode, kx, ky, kz, stencil, h, rhs)
   implicit none

   integer nx, ny, nz, mode, kx, ky, kz
   double precision :: stencil(*), h, rhs, afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
   external afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
!-----------------------------------------------------------------------
!     This subroutine calculates the correct stencil values for
!     centered difference discretization of the elliptic operator
!     and the right-hand side
!
! L u = delx( A delx u ) + dely ( B dely u) + delz ( C delz u ) +
!  delx ( D u ) + dely (E u) + delz( F u ) + G u = H
!
!   For 2-D problems the discretization formula that is used is:
!
! h**2 * Lu == A(i+1/2,j)*{u(i+1,j) - u(i,j)} +
!         A(i-1/2,j)*{u(i-1,j) - u(i,j)} +
!              B(i,j+1/2)*{u(i,j+1) - u(i,j)} +
!              B(i,j-1/2)*{u(i,j-1) - u(i,j)} +
!              (h/2)*D(i,j)*{u(i+1,j) - u(i-1,j)} +
!              (h/2)*E(i,j)*{u(i,j+1) - u(i,j-1)} +
!              (h/2)*E(i,j)*{u(i,j+1) - u(i,j-1)} +
!              (h**2)*G(i,j)*u(i,j)
!-----------------------------------------------------------------------
!     some constants
!
   double precision :: zero, half
   parameter(zero=0.0d0, half=0.5d0)
!
!     local variables
!
   integer k
   double precision :: hhalf, cntr, x, y, z, coeff
   no_warning_unused_dummy_argument(nx)
!
!     if mode < 0, we shouldn't have come here
!
   if (mode < 0) return
!
   do k = 1, 7
      stencil(k) = zero
   end do
!
   hhalf = h * half
   x = h * dble(kx)
   y = h * dble(ky)
   z = h * dble(kz)
   cntr = zero
!     differentiation wrt x:
   coeff = afun(x + hhalf, y, z)
   stencil(3) = stencil(3) + coeff
   cntr = cntr + coeff
!
   coeff = afun(x - hhalf, y, z)
   stencil(2) = stencil(2) + coeff
   cntr = cntr + coeff
!
   coeff = dfun(x, y, z) * hhalf
   stencil(3) = stencil(3) + coeff
   stencil(2) = stencil(2) - coeff
   if (ny <= 1) goto 99
!
!     differentiation wrt y:
!
   coeff = bfun(x, y + hhalf, z)
   stencil(5) = stencil(5) + coeff
   cntr = cntr + coeff
!
   coeff = bfun(x, y - hhalf, z)
   stencil(4) = stencil(4) + coeff
   cntr = cntr + coeff
!
   coeff = efun(x, y, z) * hhalf
   stencil(5) = stencil(5) + coeff
   stencil(4) = stencil(4) - coeff
   if (nz <= 1) goto 99
!
! differentiation wrt z:
!
   coeff = cfun(x, y, z + hhalf)
   stencil(7) = stencil(7) + coeff
   cntr = cntr + coeff
!
   coeff = cfun(x, y, z - hhalf)
   stencil(6) = stencil(6) + coeff
   cntr = cntr + coeff
!
   coeff = ffun(x, y, z) * hhalf
   stencil(7) = stencil(7) + coeff
   stencil(6) = stencil(6) - coeff
!
! contribution from function G:
!
99 coeff = gfun(x, y, z)
   stencil(1) = h * h * coeff - cntr
!
!     the right-hand side
!
   if (mode > 0) rhs = h * h * hfun(x, y, z)
!
   return
end subroutine getsten

subroutine gen57bl(nx, ny, nz, nfree, na, n, a, ja, ia, iau, stencil)
   implicit none
   integer ja(*), ia(*), iau(*), nx, ny, nz, nfree, na, n
   double precision :: a(na, 1), stencil(7, 1)
!--------------------------------------------------------------------
! This subroutine computes the sparse matrix in compressed
! format for the elliptic operator
!
! L u = delx( a . delx u ) + dely ( b . dely u) + delz ( c . delz u ) +
!c delx ( d . u ) + dely (e . u) + delz( f . u ) + g . u
!
! Here u is a vector of nfree componebts and each of the functions
! a, b, c, d, e, f, g   is an (nfree x nfree) matrix depending of
! the coordinate (x,y,z).
! with Dirichlet Boundary conditions, on a rectangular 1-D,
! 2-D or 3-D grid using centered difference schemes.
!
! The functions a, b, ..., g are known through the
! subroutines  afunbl, bfunbl, ..., gfunbl. (user supplied) .
!
! uses natural ordering, first x direction, then y, then z
! mesh size h is uniform and determined by grid points
! in the x-direction.
!
! The output matrix is in Block -- Sparse Row format.
!
!--------------------------------------------------------------------
! parameters:
!-------------
! Input:
! ------
! nx      = number of points in x direction
! ny    = number of points in y direction
! nz    = number of points in z direction
! nfree   = number of degrees of freedom per point
! na    = first dimension of array a as declared in calling
!           program. Must be .ge. nfree**2
!
! Output:
! ------
! n     = dimension of matrix (output)
!
! a, ja, ia = resulting matrix in  Block Sparse Row format
!           a(1:nfree**2, j ) contains a nonzero block and ja(j)
!           contains the (block) column number of this block.
!           the block dimension of the matrix is n (output) and
!           therefore the total number of (scalar) rows is n x nfree.
!
! iau     = integer*n containing the position of the diagonal element
!           in the a, ja, ia structure
!
! Work space:
!------------
! stencil =  work array of size (7,nfree**2) [stores local stencils]
!
!--------------------------------------------------------------------
!
!     stencil (1:7,*) has the following meaning:
!
!     center point = stencil(1)
!     west point   = stencil(2)
!     east point   = stencil(3)
!     south point  = stencil(4)
!     north point  = stencil(5)
!     front point  = stencil(6)
!     back point   = stencil(7)
!
!
!                           st(5)
!                            |
!                            |
!                            |
!                            |          .st(7)
!                            |     .
!                            | .
!         st(2) ----------- st(1) ---------- st(3)
!                       .    |
!                   .        |
!               .            |
!            st(6)           |
!                            |
!                            |
!                           st(4)
!
!-------------------------------------------------------------------
!     some constants
!
   double precision :: one
   parameter(one=1.0d0)
!
!     local variables
!
   integer iedge, ix, iy, iz, k, kx, ky, kz, nfree2, node
   double precision :: h
!
   h = one / dble(nx + 1)
   kx = 1
   ky = nx
   kz = nx * ny
   nfree2 = nfree * nfree
   iedge = 1
   node = 1
   do iz = 1, nz
      do iy = 1, ny
         do ix = 1, nx
            ia(node) = iedge
            call bsten(nx, ny, nz, ix, iy, iz, nfree, stencil, h)
!     west
            if (ix > 1) then
               ja(iedge) = node - kx
               do k = 1, nfree2
                  a(k, iedge) = stencil(2, k)
               end do
               iedge = iedge + 1
            end if
!     south
            if (iy > 1) then
               ja(iedge) = node - ky
               do k = 1, nfree2
                  a(k, iedge) = stencil(4, k)
               end do
               iedge = iedge + 1
            end if
!     front plane
            if (iz > 1) then
               ja(iedge) = node - kz
               do k = 1, nfree2
                  a(k, iedge) = stencil(6, k)
               end do
               iedge = iedge + 1
            end if
!     center node
            ja(iedge) = node
            iau(node) = iedge
            do k = 1, nfree2
               a(k, iedge) = stencil(1, k)
            end do
            iedge = iedge + 1
!     -- upper part
!     east
            if (ix < nx) then
               ja(iedge) = node + kx
               do k = 1, nfree2
                  a(k, iedge) = stencil(3, k)
               end do
               iedge = iedge + 1
            end if
!     north
            if (iy < ny) then
               ja(iedge) = node + ky
               do k = 1, nfree2
                  a(k, iedge) = stencil(5, k)
               end do
               iedge = iedge + 1
            end if
!     back plane
            if (iz < nz) then
               ja(iedge) = node + kz
               do k = 1, nfree2
                  a(k, iedge) = stencil(7, k)
               end do
               iedge = iedge + 1
            end if
!------next node -------------------------
            node = node + 1
         end do
      end do
   end do
   n = node - 1
   ia(node) = iedge
   return
end subroutine gen57bl

subroutine bsten(nx, ny, nz, kx, ky, kz, nfree, stencil, h)

!-----------------------------------------------------------------------
!     This subroutine calcultes the correct block-stencil values for
!     centered difference discretization of the elliptic operator
!     (block version of stencil)
!
! L u = delx( a delx u ) + dely ( b dely u) + delz ( c delz u ) +
!       d delx ( u ) + e dely (u) + f delz( u ) + g u
!
!   For 2-D problems the discretization formula that is used is:
!
! h**2 * Lu == a(i+1/2,j)*{u(i+1,j) - u(i,j)} +
!c        a(i-1/2,j)*{u(i-1,j) - u(i,j)} +
!              b(i,j+1/2)*{u(i,j+1) - u(i,j)} +
!              b(i,j-1/2)*{u(i,j-1) - u(i,j)} +
!              (h/2)*d(i,j)*{u(i+1,j) - u(i-1,j)} +
!              (h/2)*e(i,j)*{u(i,j+1) - u(i,j-1)} +
!              (h/2)*e(i,j)*{u(i,j+1) - u(i,j-1)} +
!              (h**2)*g(i,j)*u(i,j)
!-----------------------------------------------------------------------
!     some constants
!
   implicit none

   double precision :: zero, half
   parameter(zero=0.0d0, half=0.5d0)
!
!     local variables
!
   integer :: i, k, kx, ky, kz, nfree, nfree2, nx, ny, nz
   double precision :: stencil(7, *)
   double precision :: cntr(225), coeff(225), h, h2, hhalf, x, y, z, xh
   no_warning_unused_dummy_argument(nx)
!------------
   if (nfree > 15) then
      print *, ' ERROR ** nfree too large '
      stop
   end if
!
   nfree2 = nfree * nfree
   do k = 1, nfree2
      cntr(k) = zero
      do i = 1, 7
         stencil(i, k) = zero
      end do
   end do
!------------
   hhalf = h * half
   h2 = h * h
   x = h * dble(kx)
   y = h * dble(ky)
   z = h * dble(kz)
! differentiation wrt x:
   xh = x + hhalf
   call afunbl(nfree, xh, y, z, coeff)
   do k = 1, nfree2
      stencil(3, k) = stencil(3, k) + coeff(k)
      cntr(k) = cntr(k) + coeff(k)
   end do

!
   xh = x - hhalf
   call afunbl(nfree, xh, y, z, coeff)
   do k = 1, nfree2
      stencil(2, k) = stencil(2, k) + coeff(k)
      cntr(k) = cntr(k) + coeff(k)
   end do
!
   call dfunbl(nfree, x, y, z, coeff)
   do k = 1, nfree2
      stencil(3, k) = stencil(3, k) + coeff(k) * hhalf
      stencil(2, k) = stencil(2, k) - coeff(k) * hhalf
   end do
   if (ny <= 1) goto 99
!
! differentiation wrt y:
!
   call bfunbl(nfree, x, y + hhalf, z, coeff)
   do k = 1, nfree2
      stencil(5, k) = stencil(5, k) + coeff(k)
      cntr(k) = cntr(k) + coeff(k)
   end do
!
   call bfunbl(nfree, x, y - hhalf, z, coeff)
   do k = 1, nfree2
      stencil(4, k) = stencil(4, k) + coeff(k)
      cntr(k) = cntr(k) + coeff(k)
   end do
!
   call efunbl(nfree, x, y, z, coeff)
   do k = 1, nfree2
      stencil(5, k) = stencil(5, k) + coeff(k) * hhalf
      stencil(4, k) = stencil(4, k) - coeff(k) * hhalf
   end do
   if (nz <= 1) goto 99
!
! differentiation wrt z:
!
   call cfunbl(nfree, x, y, z + hhalf, coeff)
   do k = 1, nfree2
      stencil(7, k) = stencil(7, k) + coeff(k)
      cntr(k) = cntr(k) + coeff(k)
   end do
!
   call cfunbl(nfree, x, y, z - hhalf, coeff)
   do k = 1, nfree2
      stencil(6, k) = stencil(6, k) + coeff(k)
      cntr(k) = cntr(k) + coeff(k)
   end do
!
   call ffunbl(nfree, x, y, z, coeff)
   do k = 1, nfree2
      stencil(7, k) = stencil(7, k) + coeff(k) * hhalf
      stencil(6, k) = stencil(6, k) - coeff(k) * hhalf
   end do
!
! discretization of  product by g:
!
99 call gfunbl(nfree, x, y, z, coeff)
   do k = 1, nfree2
      stencil(1, k) = h2 * coeff(k) - cntr(k)
   end do
!
   return
end subroutine bsten

subroutine fdreduce(nx, ny, nz, alpha, n, a, ja, ia, iau, rhs, stencil)
   implicit none
   integer nx, ny, nz, n, ia(*), ja(*), iau(*)
   double precision :: alpha(*), a(*), rhs(*), stencil(*)
!-----------------------------------------------------------------------
! This subroutine tries to reduce the size of the matrix by looking
! for Dirichlet boundary conditions at each surface and solve the boundary
! value and modify the right-hand side of related nodes, then clapse all
! the boundary nodes.
!-----------------------------------------------------------------------
!     parameters
!
   double precision :: zero
   parameter(zero=0.0d0)
!
!     local variables
!
   integer i, j, k, kx, ky, kz, lx, ux, ly, uy, lz, uz, node, nbnode, lk, ld, iedge
   double precision :: val
   integer lctcsr
   external lctcsr
!
!     The first half of this subroutine will try to change the right-hand
!     side of all the nodes that has a neighbor with Dirichlet boundary
!     condition, since in this case the value of the boundary point is
!     known.
!     Then in the second half, we will try to eliminate the boundary
!     points with known values (with Dirichlet boundary condition).
!
   kx = 1
   ky = nx
   kz = nx * ny
   lx = 1
   ux = nx
   ly = 1
   uy = ny
   lz = 1
   uz = nz
!
!     Here goes the first part. ----------------------------------------
!
!     the left (west) side
!
   if (alpha(1) == zero) then
      lx = 2
      do k = 1, nz
         do j = 1, ny
            node = (k - 1) * kz + (j - 1) * ky + 1
            nbnode = node + kx
            lk = lctcsr(nbnode, node, ja, ia)
            ld = iau(node)
            val = rhs(node) / a(ld)
!     modify the rhs
            rhs(nbnode) = rhs(nbnode) - a(lk) * val
         end do
      end do
   end if
!
!     right (east) side
!
   if (alpha(2) == zero) then
      ux = nx - 1
      do k = 1, nz
         do j = 1, ny
            node = (k - 1) * kz + (j - 1) * ky + nx
            nbnode = node - kx
            lk = lctcsr(nbnode, node, ja, ia)
            ld = iau(node)
            val = rhs(node) / a(ld)
!     modify the rhs
            rhs(nbnode) = rhs(nbnode) - a(lk) * val
         end do
      end do
   end if
!
!     if it's only 1-D, skip the following part
!
   if (ny <= 1) goto 100
!
!     the bottom (south) side
!
   if (alpha(3) == zero) then
      ly = 2
      do k = 1, nz
         do i = lx, ux
            node = (k - 1) * kz + i
            nbnode = node + ky
            lk = lctcsr(nbnode, node, ja, ia)
            ld = iau(node)
            val = rhs(node) / a(ld)
!     modify the rhs
            rhs(nbnode) = rhs(nbnode) - a(lk) * val
         end do
      end do
   end if
!
!     top (north) side
!
   if (alpha(4) == zero) then
      uy = ny - 1
      do k = 1, nz
         do i = lx, ux
            node = (k - 1) * kz + i + (ny - 1) * ky
            nbnode = node - ky
            lk = lctcsr(nbnode, node, ja, ia)
            ld = iau(node)
            val = rhs(node) / a(ld)
!     modify the rhs
            rhs(nbnode) = rhs(nbnode) - a(lk) * val
         end do
      end do
   end if
!
!     if only 2-D skip the following section on z
!
   if (nz <= 1) goto 100
!
!     the front surface
!
   if (alpha(5) == zero) then
      lz = 2
      do j = ly, uy
         do i = lx, ux
            node = (j - 1) * ky + i
            nbnode = node + kz
            lk = lctcsr(nbnode, node, ja, ia)
            ld = iau(node)
            val = rhs(node) / a(ld)
!     modify the rhs
            rhs(nbnode) = rhs(nbnode) - a(lk) * val
         end do
      end do
   end if
!
!     rear surface
!
   if (alpha(6) == zero) then
      uz = nz - 1
      do j = ly, uy
         do i = lx, ux
            node = (nz - 1) * kz + (j - 1) * ky + i
            nbnode = node - kz
            lk = lctcsr(nbnode, node, ja, ia)
            ld = iau(node)
            val = rhs(node) / a(ld)
!     modify the rhs
            rhs(nbnode) = rhs(nbnode) - a(lk) * val
         end do
      end do
   end if
!
!     now the second part ----------------------------------------------
!
!     go through all the actual nodes with unknown values, collect all
!     of them to form a new matrix in compressed sparse row format.
!
100 kx = 1
   ky = ux - lx + 1
   kz = (uy - ly + 1) * ky
   node = 1
   iedge = 1
   do k = lz, uz
      do j = ly, uy
         do i = lx, ux
!
!     the corresponding old node number
            nbnode = ((k - 1) * ny + j - 1) * nx + i
!
!     copy the row into local stencil, copy is done is the exact
!     same order as the stencil is written into array a
            lk = ia(nbnode)
            if (i > 1) then
               stencil(2) = a(lk)
               lk = lk + 1
            end if
            if (j > 1) then
               stencil(4) = a(lk)
               lk = lk + 1
            end if
            if (k > 1) then
               stencil(6) = a(lk)
               lk = lk + 1
            end if
            stencil(1) = a(lk)
            lk = lk + 1
            if (i < nx) then
               stencil(3) = a(lk)
               lk = lk + 1
            end if
            if (j < ny) then
               stencil(5) = a(lk)
               lk = lk + 1
            end if
            if (k < nz) stencil(7) = a(lk)
!
!     first the ia pointer -- points to the beginning of each row
            ia(node) = iedge
!
!     move the values from the local stencil to the new matrix
!
!     the neighbor on the left (west)
            if (i > lx) then
               ja(iedge) = node - kx
               a(iedge) = stencil(2)
               iedge = iedge + 1
            end if
!     the neighbor below (south)
            if (j > ly) then
               ja(iedge) = node - ky
               a(iedge) = stencil(4)
               iedge = iedge + 1
            end if
!     the neighbor in the front
            if (k > lz) then
               ja(iedge) = node - kz
               a(iedge) = stencil(6)
               iedge = iedge + 1
            end if
!     center node (itself)
            ja(iedge) = node
            iau(node) = iedge
            a(iedge) = stencil(1)
            iedge = iedge + 1
!     the neighbor to the right (east)
            if (i < ux) then
               ja(iedge) = node + kx
               a(iedge) = stencil(3)
               iedge = iedge + 1
            end if
!     the neighbor above (north)
            if (j < uy) then
               ja(iedge) = node + ky
               a(iedge) = stencil(5)
               iedge = iedge + 1
            end if
!     the neighbor at the back
            if (k < uz) then
               ja(iedge) = node + kz
               a(iedge) = stencil(7)
               iedge = iedge + 1
            end if
!     the right-hand side
            rhs(node) = rhs(nbnode)
!------next node -------------------------
            node = node + 1
!
         end do
      end do
   end do
!
   ia(node) = iedge
!
!     the number of nodes in the final matrix is stored in n
!
   n = node - 1
   return
end subroutine fdreduce

subroutine fdaddbc(nx, ny, nz, a, ja, ia, iau, rhs, al, h)
   implicit none

   integer nx, ny, nz, ia(nx * ny * nz), ja(7 * nx * ny * nz), iau(nx * ny * nz)
   double precision :: h, al(6), a(7 * nx * ny * nz), rhs(nx * ny * nz)
!-----------------------------------------------------------------------
! This subroutine will add the boundary condition to the linear system
! consutructed without considering the boundary conditions
!
! The Boundary condition is specified in the following form:
!           du
!     alpha -- + beta u = gamma
!           dn
! Alpha is stored in array AL.  The six side of the boundary appares
! in AL in the following order: left(west), right(east), bottom(south),
! top(north), front, back(rear). (see also the illustration in gen57pt)
! Beta and gamma appears as the functions, betfun and gamfun.
! They have the following prototype
!
! double precision ::  function xxxfun(x, y, z)
! double precision ::  x, y, z
!
! where x, y, z are vales in the range of [0, 1][0, (ny-1)*h]
! [0, (nz-1)*h]
!
! At the corners or boundary lines, the boundary conditions are applied
! in the follow order:
! 1) if one side is Dirichlet boundary condition, the Dirichlet boundary
!    condition is used;
! 2) if more than one sides are Dirichlet, the Direichlet condition
!    specified for X direction boundary will overwrite the one specified
!    for Y direction boundary which in turn has priority over Z
!     direction boundaries.
! 3) when all sides are non-Dirichlet, the average values are used.
!-----------------------------------------------------------------------
!     some constants
!
   double precision :: half, zero, one, two
   parameter(half=0.5d0, zero=0.0d0, one=1.0d0, two=2.0d0)
!
!     local variables
!
   character(len=2) side
   integer i, j, k, kx, ky, kz, node, nbr, ly, uy, lx, ux
   double precision :: coeff, ctr, hhalf, x, y, z
   double precision :: afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
   external afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
   double precision :: betfun, gamfun
   integer lctcsr
   external lctcsr, betfun, gamfun
!
   hhalf = half * h
   kx = 1
   ky = nx
   kz = nx * ny
!
!     In 3-D case, we need to go through all 6 faces one by one. If
!     the actual dimension is lower, test on ny is performed first.
!     If ny is less or equals to 1, then the value of nz is not
!     checked.
!-----
!     the surface on the left (west) side
!     Concentrate on the contribution from the derivatives related to x,
!     The terms with derivative of x was assumed to be:
!
!     a(3/2,j,k)*[u(2,j,k)-u(1,j,k)] + a(1/2,j,k)*[u(0,j,k)-u(1,j,k)] +
!     h*d(1,j,k)*[u(2,j,k)-u(0,j,k)]/2
!
!     But they actually are:
!
!     2*{a(3/2,j,k)*[u(2,j,k)-u(1,j,k)] -
!     h*a(1,j,k)*[beta*u(1,j,k)-gamma]/alpha]} +
!     h*h*d(1,j,k)*[beta*u(1,j,k)-gamma]/alpha
!
!     Therefore, in terms of local stencil the right neighbor of a node
!     should be changed to 2*a(3/2,j,k),
!     The matrix never contains the left neighbor on this border, nothing
!     needs to be done about it.
!     The following terms should be added to the center stencil:
!     -a(3/2,j,k) + a(1/2,j,k) + [h*d(1,j,k)-2*a(1,j,k)]*h*beta/alpha
!
!     And these terms should be added to the corresponding right-hand side
!     [h*d(1,j,k)-2*a(1,j,k)]*h*gamma/alpha
!
!     Obviously, the formula do not apply for the Dirichlet Boundary
!     Condition, where alpha will be zero. In that case, we simply set
!     all the elements in the corresponding row to zero(0), then let
!     the diagonal element be beta, and the right-hand side be gamma.
!     Thus the value of u at that point will be set. Later on point
!     like this will be removed from the matrix, since they are of
!     know value before solving the system.(not done in this subroutine)
!
   x = zero
   side = 'x1'
   do k = 1, nz
      z = (k - 1) * h
      do j = 1, ny
         y = (j - 1) * h
         node = 1 + (j - 1) * ky + (k - 1) * kz
!
!     check to see if it's Dirichlet Boundary condition here
!
         if (al(1) == zero) then
            call clrow(node, a, ja, ia)
            a(iau(node)) = betfun(side, x, y, z)
            rhs(node) = gamfun(side, x, y, z)
         else
!
!     compute the terms formulated above to modify the matrix.
!
!     the right neighbor is stroed in nbr'th posiiton in the a
            nbr = lctcsr(node, node + kx, ja, ia)
!
            coeff = two * afun(x, y, z)
            ctr = (h * dfun(x, y, z) - coeff) * h / al(1)
            rhs(node) = rhs(node) + ctr * gamfun(side, x, y, z)
            ctr = afun(x - hhalf, y, z) + ctr * betfun(side, x, y, z)
            coeff = afun(x + hhalf, y, z)
            a(iau(node)) = a(iau(node)) - coeff + ctr
            a(nbr) = two * coeff
         end if
      end do
   end do
!
!     the right (east) side boudary, similarly, the contirbution from
!     the terms containing the derivatives of x were assumed to be
!
!     a(nx+1/2,j,k)*[u(nx+1,j,k)-u(nx,j,k)] +
!     a(nx-1/2,j,k)*[u(nx-1,j,k)-u(nx,j,k)] +
!     d(nx,j,k)*[u(nx+1,j,k)-u(nx-1,j,k)]*h/2
!
!     Actualy they are:
!
!     2*{h*a(nx,j,k)*[gamma-beta*u(nx,j,k)]/alpha +
!     a(nx-1/2,j,k)*[u(nx-1,j,k)-u(nx,j,k)]} +
!     h*h*d(nx,j,k)*[gamma-beta*u(nx,j,k)]/alpha
!
!     The left stencil has to be set to 2*a(nx-1/2,j,k)
!
!     The following terms have to be added to the center stencil:
!
!     -a(nx-1/2,j,k)+a(nx+1/2,j,k)-[2*a(nx,j,k)+h*d(nx,j,k)]*beta/alpha
!
!     The following terms have to be added to the right-hand side:
!
!     -[2*a(nx,j,k)+h*d(nx,j,k)]*h*gamma/alpha
!
   x = one
   side = 'x2'
   do k = 1, nz
      z = (k - 1) * h
      do j = 1, ny
         y = (j - 1) * h
         node = (k - 1) * kz + j * ky
!
         if (al(2) == zero) then
            call clrow(node, a, ja, ia)
            a(iau(node)) = betfun(side, x, y, z)
            rhs(node) = gamfun(side, x, y, z)
         else
            nbr = lctcsr(node, node - kx, ja, ia)
!
            coeff = two * afun(x, y, z)
            ctr = (coeff + h * dfun(x, y, z)) * h / al(2)
            rhs(node) = rhs(node) - ctr * gamfun(side, x, y, z)
            ctr = afun(x + hhalf, y, z) - ctr * betfun(side, x, y, z)
            coeff = afun(x - hhalf, y, z)
            a(iau(node)) = a(iau(node)) - coeff + ctr
            a(nbr) = two * coeff
         end if
      end do
   end do
!
!     If only one dimension, return now
!
   if (ny <= 1) return
!
!     the bottom (south) side suface, This similar to the situation
!     with the left side, except all the function and realted variation
!     should be on the y.
!
!     These two block if statment here is to resolve the possible conflict
!     of assign the boundary value differently by different side of the
!     Dirichlet Boundary Conditions. They ensure that the edges that have
!     be assigned a specific value will not be reassigned.
!
   if (al(1) == zero) then
      lx = 2
   else
      lx = 1
   end if
   if (al(2) == zero) then
      ux = nx - 1
   else
      ux = nx
   end if
   y = zero
   side = 'y1'
   do k = 1, nz
      z = (k - 1) * h
      do i = lx, ux
         x = (i - 1) * h
         node = i + (k - 1) * kz
!
         if (al(3) == zero) then
            call clrow(node, a, ja, ia)
            a(iau(node)) = betfun(side, x, y, z)
            rhs(node) = gamfun(side, x, y, z)
         else
            nbr = lctcsr(node, node + ky, ja, ia)
!
            coeff = two * bfun(x, y, z)
            ctr = (h * efun(x, y, z) - coeff) * h / al(3)
            rhs(node) = rhs(node) + ctr * gamfun(side, x, y, z)
            ctr = bfun(x, y - hhalf, z) + ctr * betfun(side, x, y, z)
            coeff = bfun(x, y + hhalf, z)
            a(iau(node)) = a(iau(node)) - coeff + ctr
            a(nbr) = two * coeff
         end if
      end do
   end do
!
!     The top (north) side, similar to the right side
!
   y = (ny - 1) * h
   side = 'y2'
   do k = 1, nz
      z = (k - 1) * h
      do i = lx, ux
         x = (i - 1) * h
         node = (k - 1) * kz + (ny - 1) * ky + i
!
         if (al(4) == zero) then
            call clrow(node, a, ja, ia)
            a(iau(node)) = betfun(side, x, y, z)
            rhs(node) = gamfun(side, x, y, z)
         else
            nbr = lctcsr(node, node - ky, ja, ia)
!
            coeff = two * bfun(x, y, z)
            ctr = (coeff + h * efun(x, y, z)) * h / al(4)
            rhs(node) = rhs(node) - ctr * gamfun(side, x, y, z)
            ctr = bfun(x, y + hhalf, z) - ctr * betfun(side, x, y, z)
            coeff = bfun(x, y - hhalf, z)
            a(iau(node)) = a(iau(node)) - coeff + ctr
            a(nbr) = two * coeff
         end if
      end do
   end do
!
!     If only has two dimesion to work on, return now
!
   if (nz <= 1) return
!
!     The front side boundary
!
!     If the edges of the surface has been decided by Dirichlet Boundary
!     Condition, then leave them alone.
!
   if (al(3) == zero) then
      ly = 2
   else
      ly = 1
   end if
   if (al(4) == zero) then
      uy = ny - 1
   else
      uy = ny
   end if
!
   z = zero
   side = 'z1'
   do j = ly, uy
      y = (j - 1) * h
      do i = lx, ux
         x = (i - 1) * h
         node = i + (j - 1) * ky
!
         if (al(5) == zero) then
            call clrow(node, a, ja, ia)
            a(iau(node)) = betfun(side, x, y, z)
            rhs(node) = gamfun(side, x, y, z)
         else
            nbr = lctcsr(node, node + kz, ja, ia)
!
            coeff = two * cfun(x, y, z)
            ctr = (h * ffun(x, y, z) - coeff) * h / al(5)
            rhs(node) = rhs(node) + ctr * gamfun(side, x, y, z)
            ctr = cfun(x, y, z - hhalf) + ctr * betfun(side, x, y, z)
            coeff = cfun(x, y, z + hhalf)
            a(iau(node)) = a(iau(node)) - coeff + ctr
            a(nbr) = two * coeff
         end if
      end do
   end do
!
!     Similiarly for the top side of the boundary suface
!
   z = (nz - 1) * h
   side = 'z2'
   do j = ly, uy
      y = (j - 1) * h
      do i = lx, ux
         x = (i - 1) * h
         node = (nz - 1) * kz + (j - 1) * ky + i
!
         if (al(6) == zero) then
            call clrow(node, a, ja, ia)
            a(iau(node)) = betfun(side, x, y, z)
            rhs(node) = gamfun(side, x, y, z)
         else
            nbr = lctcsr(node, node - kz, ja, ia)
!
            coeff = two * cfun(x, y, z)
            ctr = (coeff + h * ffun(x, y, z)) * h / al(6)
            rhs(node) = rhs(node) - ctr * gamfun(side, x, y, z)
            ctr = cfun(x, y, z + hhalf) - ctr * betfun(side, x, y, z)
            coeff = cfun(x, y, z - hhalf)
            a(iau(node)) = a(iau(node)) - coeff + ctr
            a(nbr) = two * coeff
         end if
      end do
   end do
!
!     all set
!
   return
end subroutine fdaddbc

subroutine clrow(i, a, ja, ia)
   use precision, only: dp
   implicit none

   integer i, ja(:), ia(*), k
   real(dp) :: a(*)
   no_warning_unused_dummy_argument(ja)
!-----------------------------------------------------------------------
!     clear the row i to all zero, but still keep the structure of the
!     CSR matrix
!-----------------------------------------------------------------------
   do k = ia(i), ia(i + 1) - 1
      a(k) = 0.0d0
   end do
!
   return
end subroutine clrow

function lctcsr(i, j, ja, ia)
   integer lctcsr, i, j, ja(*), ia(*), k
!-----------------------------------------------------------------------
!     locate the position of a matrix element in a CSR format
!     returns -1 if the desired element is zero
!-----------------------------------------------------------------------
   lctcsr = -1
   k = ia(i)
10 if (k < ia(i + 1) .and. (lctcsr == -1)) then
      if (ja(k) == j) lctcsr = k
      k = k + 1
      goto 10
   end if
!
   return
end function lctcsr

!-----------------------------------------------------------------------
!     functions for the block PDE's
!-----------------------------------------------------------------------
subroutine afunbl(nfree, x, y, z, coeff)
   use precision_basics, only: dp

   implicit none

   integer :: nfree
   real(dp) :: x, y, z, coeff(225)

   integer :: i, j

   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   do j = 1, nfree
      do i = 1, nfree
         coeff((j - 1) * nfree + i) = 0.0d0
      end do
      coeff((j - 1) * nfree + j) = -1.0d0
   end do
   return
end subroutine afunbl

subroutine bfunbl(nfree, x, y, z, coeff)
   use precision_basics, only: dp

   implicit none

   integer :: nfree
   real(dp) :: x, y, z, coeff(225)

   integer :: i, j

   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   do j = 1, nfree
      do i = 1, nfree
         coeff((j - 1) * nfree + i) = 0.0d0
      end do
      coeff((j - 1) * nfree + j) = -1.0d0
   end do
   return
end subroutine bfunbl

subroutine cfunbl(nfree, x, y, z, coeff)
   use precision_basics, only: dp

   implicit none

   integer :: nfree
   real(dp) :: x, y, z, coeff(225)

   integer :: i, j

   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   do j = 1, nfree
      do i = 1, nfree
         coeff((j - 1) * nfree + i) = 0.0d0
      end do
      coeff((j - 1) * nfree + j) = -1.0d0
   end do
   return
end subroutine cfunbl

subroutine dfunbl(nfree, x, y, z, coeff)
   use precision_basics, only: dp

   implicit none

   integer :: nfree
   real(dp) :: x, y, z, coeff(225)

   integer :: i, j

   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   do j = 1, nfree
      do i = 1, nfree
         coeff((j - 1) * nfree + i) = 0.0d0
      end do
   end do
   return
end subroutine dfunbl

subroutine efunbl(nfree, x, y, z, coeff)
   use precision_basics, only: dp

   implicit none

   integer :: nfree
   real(dp) :: x, y, z, coeff(225)

   integer :: i, j

   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   do j = 1, nfree
      do i = 1, nfree
         coeff((j - 1) * nfree + i) = 0.0d0
      end do
   end do
   return
end subroutine efunbl

subroutine ffunbl(nfree, x, y, z, coeff)
   use precision_basics, only: dp

   implicit none

   integer :: nfree
   real(dp) :: x, y, z, coeff(225)

   integer :: i, j

   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   do j = 1, nfree
      do i = 1, nfree
         coeff((j - 1) * nfree + i) = 0.0d0
      end do
   end do
   return
end subroutine ffunbl

subroutine gfunbl(nfree, x, y, z, coeff)
   use precision_basics, only: dp

   implicit none

   integer :: nfree
   real(dp) :: x, y, z, coeff(225)

   integer :: i, j

   no_warning_unused_dummy_argument(x)
   no_warning_unused_dummy_argument(y)
   no_warning_unused_dummy_argument(z)
   do j = 1, nfree
      do i = 1, nfree
         coeff((j - 1) * nfree + i) = 0.0d0
      end do
   end do
   return
end subroutine gfunbl

! NOT THREAD-SAFE
subroutine amuxXXX(n, x, y, a, ja, ia)
   use m_saad, only: jasafe
   use precision, only: dp

   implicit none

   real(dp) :: x(n), y(n), a(*)
   integer n, ja(*), ia(*)
!-----------------------------------------------------------------------
!         A times a vector
!-----------------------------------------------------------------------
! multiplies a matrix by a vector using the dot product form
! Matrix A is stored in compressed sparse row storage.
!
! on entry:
!----------
! n     = row dimension of A
! x     = real array of length equal to the column dimension of
!         the A matrix.
! a, ja,
!    ia = input matrix in compressed sparse row format.
!
! on return:
!-----------
! y     = real array of length n, containing the product y=Ax
!
!-----------------------------------------------------------------------
! local variables
!
   real(dp) :: t
   integer i, k
!-----------------------------------------------------------------------

   if (jasafe /= 1) then
      !$OMP PARALLEL DO                                          &
      !$OMP PRIVATE(i,k,t)

      do i = 1, n
         !
         !     compute the inner product of row i with vector x
         !
         t = 0.0d0
         do k = ia(i), ia(i + 1) - 1
            t = t + a(k) * x(ja(k))
         end do
         !
         !     store result in y(i)
         !
         y(i) = t

      end do
      !$OMP END PARALLEL DO
   else
      do i = 1, n

         t = 0.0d0
         do k = ia(i), ia(i + 1) - 1
            t = t + a(k) * x(ja(k))
         end do

         y(i) = t

      end do
   end if

!
   return
end subroutine amuxXXX

! NOT THREAD-SAFE
subroutine atmux(n, x, y, a, ja, ia)
   use m_saad, only: jasafe
   use precision, only: dp

   implicit none

   real(dp) :: x(*), y(*), a(*)
   integer n, ia(*), ja(*)
!-----------------------------------------------------------------------
!         transp( A ) times a vector
!-----------------------------------------------------------------------
! multiplies the transpose of a matrix by a vector when the original
! matrix is stored in compressed sparse row storage. Can also be
! viewed as the product of a matrix by a vector when the original
! matrix is stored in the compressed sparse column format.
!-----------------------------------------------------------------------
!
! on entry:
!----------
! n     = row dimension of A
! x     = real array of length equal to the column dimension of
!         the A matrix.
! a, ja,
!    ia = input matrix in compressed sparse row format.
!
! on return:
!-----------
! y     = real array of length n, containing the product y=transp(A)*x
!
!-----------------------------------------------------------------------
!     local variables
!
   integer i, k
!-----------------------------------------------------------------------
!
!     zero out output vector
!
!
! loop over the rows
!

   if (jasafe /= 1) then
      !$OMP PARALLEL DO                                          &
      !$OMP PRIVATE(i,k)
      do i = 1, n
         y(i) = 0.0
         do k = ia(i), ia(i + 1) - 1
            y(ja(k)) = y(ja(k)) + x(i) * a(k)
         end do
      end do
      !$OMP END PARALLEL DO
   else
      do i = 1, n
         y(i) = 0.0
         do k = ia(i), ia(i + 1) - 1
            y(ja(k)) = y(ja(k)) + x(i) * a(k)
         end do
      end do
   end if

!
   return
end subroutine atmux

subroutine lusol(n, y, x, alu, jlu, ju, nau)
   implicit none

   double precision :: x(n), y(n), alu(nau)
   integer n, jlu(nau), ju(nau), nau
!-----------------------------------------------------------------------
!
! This routine solves the system (LU) x = y,
! given an LU decomposition of a matrix stored in (alu, jlu, ju)
! modified sparse row format
!
!-----------------------------------------------------------------------
! on entry:
! n   = dimension of system
! y   = the right-hand-side vector
! alu, jlu, ju
!     = the LU matrix as provided from the ILU routines.
!
! on return
! x   = solution of LU x = y.
!-----------------------------------------------------------------------
!
! Note: routine is in place: call lusol (n, x, x, alu, jlu, ju)
!       will solve the system with rhs x and overwrite the result on x .
!
!-----------------------------------------------------------------------
! local variables
!
   integer i, k
!
! forward solve
!

   do i = 1, n
      x(i) = y(i)
      do k = jlu(i), ju(i) - 1
         x(i) = x(i) - alu(k) * x(jlu(k))
      end do
   end do

!
!     backward solve.
!

   do i = n, 1, -1
      do k = ju(i), jlu(i + 1) - 1
         x(i) = x(i) - alu(k) * x(jlu(k))
      end do
      x(i) = alu(i) * x(i)
   end do

!
   return
end subroutine lusol

subroutine lutsol(n, y, x, alu, jlu, ju)
   implicit none

   double precision :: x(n), y(n), alu(*)
   integer n, jlu(*), ju(*)
!-----------------------------------------------------------------------
!
! This routine solves the system  Transp(LU) x = y,
! given an LU decomposition of a matrix stored in (alu, jlu, ju)
! modified sparse row format. Transp(M) is the transpose of M.
!-----------------------------------------------------------------------
! on entry:
! n   = dimension of system
! y   = the right-hand-side vector
! alu, jlu, ju
!     = the LU matrix as provided from the ILU routines.
!
! on return
! x   = solution of transp(LU) x = y.
!-----------------------------------------------------------------------
!
! Note: routine is in place: call lutsol (n, x, x, alu, jlu, ju)
!       will solve the system with rhs x and overwrite the result on x .
!
!-----------------------------------------------------------------------
! local variables
!
   integer i, k
!

! forward solve (with U^T)
!
   do i = 1, n
      x(i) = y(i) * alu(i)
      do k = ju(i), jlu(i + 1) - 1
         x(jlu(k)) = x(jlu(k)) - alu(k) * x(i)
      end do
   end do
!
!       backward solve (with L^T)
!
   do i = n, 1, -1
      do k = jlu(i), ju(i) - 1
         x(jlu(k)) = x(jlu(k)) - alu(k) * x(i)
      end do
   end do
!
   return
end subroutine lutsol

!> (re)allocate solver
!>   it is assumed that number of rows, number of non-zero entries, number of non-zero entries in preconditioner and size of work array are set
subroutine allocSolver(solver, ierror)
   use m_solver
   use unstruc_messages
   use m_alloc
   implicit none

   type(tsolver), intent(inout) :: solver !< solver
   integer, intent(inout) :: ierror !< error (1) or not (0)

   ierror = 1

!  check sizes
   if (solver%numrows <= 0 .or. &
       solver%numnonzeros <= 0 .or. &
       solver%numnonzerosprecond <= 0 .or. &
       solver%nwork <= 0) then
      goto 1234
   end if

   call realloc(solver%a, solver%numnonzeros, keepExisting=.false., fill=0d0)
   call realloc(solver%ia, solver%numrows + 1, keepExisting=.false., fill=0)
   call realloc(solver%ja, solver%numnonzeros, keepExisting=.false., fill=0)

   call realloc(solver%rhs, solver%numrows, keepExisting=.false., fill=0d0)

   call realloc(solver%alu, solver%numnonzerosprecond, keepExisting=.false., fill=0d0)
   call realloc(solver%ju, solver%numrows, keepExisting=.false., fill=0)
   call realloc(solver%jlu, solver%numnonzerosprecond, keepExisting=.false., fill=0)

   call realloc(solver%work, solver%nwork, keepExisting=.false., fill=0d0)
   call realloc(solver%jw, solver%nwork, keepExisting=.false., fill=0)

   ierror = 0
1234 continue

   if (ierror /= 0) then
      call mess(LEVEL_ERROR, 'alloc_solver: error')
      call deallocSolver(solver)
   end if

   return
end subroutine allocSolver

!> deallocate solver
subroutine deallocSolver(solver)
   use m_solver
   use m_alloc
   implicit none

   type(tsolver), intent(inout) :: solver !< solver

   if (allocated(solver%a)) deallocate (solver%a)
   if (allocated(solver%ia)) deallocate (solver%ia)
   if (allocated(solver%ja)) deallocate (solver%ja)

   if (allocated(solver%rhs)) deallocate (solver%rhs)

   if (allocated(solver%alu)) deallocate (solver%alu)
   if (allocated(solver%ju)) deallocate (solver%ju)
   if (allocated(solver%jlu)) deallocate (solver%jlu)

   if (allocated(solver%work)) deallocate (solver%work)
   if (allocated(solver%jw)) deallocate (solver%jw)

   solver%numrows = 0
   solver%numnonzeros = 0
   solver%numnonzerosprecond = 0
   solver%nwork = 0

   return
end subroutine deallocSolver

!> solve linear system
subroutine solveSystem(solver, sol, japrecond, iters, ierror)
   use m_solver
   implicit none

   type(tsolver), intent(in) :: solver !< solver
   double precision, dimension(solver%numrows), intent(inout) :: sol !< solution vector
   integer, intent(in) :: japrecond !< compute preconditioner (1) or not (0)
   integer, intent(out) :: iters !< number of iterations
   integer, intent(inout) :: ierror !< error (1) or not (0)

!   double precision, dimension(:), allocatable :: w
!   integer,          dimension(:), allocatable :: jw

   integer :: N

   N = solver%numrows

!   allocate(w(2*N))
!   allocate(jw(2*N))

   ierror = 1

   if (japrecond == 1) then
!     compute preconditioner
      call ilud(solver%numrows, solver%a, solver%ja, solver%ia, solver%alpha, solver%tol, solver%alu, solver%jlu, solver%ju, solver%numnonzerosprecond, solver%work, solver%jw, ierror, solver%numnonzeros)
      if (ierror /= 0) goto 1234
   end if

!  solve system
   call runrc2(solver%numrows, solver%rhs, sol, solver%ipar, solver%fpar, solver%work, solver%a, solver%ja, solver%ia, solver%alu, solver%jlu, solver%ju, iters, solver%eps, solver%jabcgstab, ierror, solver%numnonzerosprecond)
   if (ierror /= 0) goto 1234

   ierror = 0
1234 continue

!   if ( allocated(w) ) deallocate(w)
!   if ( allocated(jw) ) deallocate(jw)

   return
end subroutine solveSystem
