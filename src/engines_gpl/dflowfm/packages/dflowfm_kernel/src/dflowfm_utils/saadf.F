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

! matall
      
      
!----------------------------------------------------------------------c
!                          S P A R S K I T                             c
!----------------------------------------------------------------------c
!        BASIC LINEAR ALGEBRA FOR SPARSE MATRICES. BLASSM MODULE       c
!----------------------------------------------------------------------c
! amub   :   computes     C = A*B                                      c
! aplb   :   computes     C = A+B                                      c
! aplb1  :   computes     C = A+B  [Sorted version: A, B, C sorted]    c
! aplsb  :   computes     C = A + s B                                  c
! aplsb1 :   computes     C = A+sB  [Sorted version: A, B, C sorted]   c
! apmbt  :   Computes     C = A +/- transp(B)                          c
! aplsbt :   Computes     C = A + s * transp(B)                        c
! diamua :   Computes     C = Diag * A                                 c
! amudia :   Computes     C = A* Diag                                  c
! aplsca :   Computes     A:= A + s I    (s = scalar)                  c
! apldia :   Computes     C = A + Diag.                                c
!----------------------------------------------------------------------c
! Note: this module still incomplete.                                  c
!----------------------------------------------------------------------c
      subroutine amub (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *                  c,jc,ic,nzmax,iw,ierr)
      integer, intent(in) :: nrow, ncol, nzmax
      real*8, intent(inout) :: a(*), b(*), c(*)
      integer, intent(inout) :: ja(*),jb(*),jc(*),ia(nrow+1),ib(*),ic(*)
     * ,iw(ncol)
      integer :: len, ierr, j, ii, ka, jj, kb, jcol, jpos, job
      integer :: k
!-----------------------------------------------------------------------
! performs the matrix by matrix product C = A B
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow  = integer. The row dimension of A = row dimension of C
! ncol  = integer. The column dimension of B = column dimension of C
! job   = integer. Job indicator. When job = 0, only the structure
!                  (i.e. the arrays jc, ic) is computed and the
!                  real values are ignored.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format.
!
! b,
! jb,
! ib    =  Matrix B in compressed sparse row format.
!
! nzmax = integer. The  length of the arrays c and jc.
!         amub will stop if the result matrix C  has a number
!         of elements that exceeds exceeds nzmax. See ierr.
!
! on return:
!----------
! c,
! jc,
! ic    = resulting matrix C in compressed sparse row sparse format.
!
! ierr  = integer. serving as error message.
!         ierr = 0 means normal return,
!         ierr .gt. 0 means that amub stopped while computing the
!         i-th row  of C with i=ierr, because the number
!         of elements in C exceeds nzmax.
!
! work arrays:
!------------
! iw    = integer work array of length equal to the number of
!         columns in A.
! Note:
!-------
!   The row dimension of B is not needed. However there is no checking
!   on the condition that ncol(A) = nrow(B).
!
!-----------------------------------------------------------------------
      real*8 scal
      logical values
      values = (job .ne. 0)
      len = 0
      ic(1) = 1
      ierr = 0
!     initialize array iw.
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
!
      do 500 ii=1, nrow
!     row i
         do 200 ka=ia(ii), ia(ii+1)-1
            if (values) scal = a(ka)
            jj   = ja(ka)
            do 100 kb=ib(jj),ib(jj+1)-1
               jcol = jb(kb)
               jpos = iw(jcol)
               if (jpos .eq. 0) then
                  len = len+1
                  if (len .gt. nzmax) then
                     ierr = ii
                     return
                  endif
                  jc(len) = jcol
                  iw(jcol)= len
                  if (values) c(len)  = scal*b(kb)
               else
                  if (values) c(jpos) = c(jpos) + scal*b(kb)
               endif
 100        continue
 200     continue
         do 201 k=ic(ii), len
            iw(jc(k)) = 0
 201     continue
         ic(ii+1) = len+1
 500  continue
      return
!-------------end-of-amub-----------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine aplb (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      integer, intent(in) :: nrow, ncol, nzmax, job
      real*8, intent(inout) :: a(*), b(*), c(*)
      integer, intent(inout) :: ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1)
     * ,ic(nrow+1), iw(ncol)
      integer, intent(out) :: ierr
      integer :: ii, ka, jcol, len, j, kb, jpos, k
!-----------------------------------------------------------------------
! performs the matrix sum  C = A+B.
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A and B
! ncol  = integer. The column dimension of A and B.
! job   = integer. Job indicator. When job = 0, only the structure
!                  (i.e. the arrays jc, ic) is computed and the
!                  real values are ignored.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format.
!
! b,
! jb,
! ib  =  Matrix B in compressed sparse row format.
!
! nzmax  = integer. The  length of the arrays c and jc.
!         amub will stop if the result matrix C  has a number
!         of elements that exceeds exceeds nzmax. See ierr.
!
! on return:
!----------
! c,
! jc,
! ic  = resulting matrix C in compressed sparse row sparse format.
c  
! ierr   = integer. serving as error message.
!         ierr = 0 means normal return,
!         ierr .gt. 0 means that amub stopped while computing the
!         i-th row  of C with i=ierr, because the number
!         of elements in C exceeds nzmax.
!
! work arrays:
!------------
! iw  = integer work array of length equal to the number of
!         columns in A.
!
!-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0)
      ierr = 0
      len = 0
      ic(1) = 1
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
!
      do 500 ii=1, nrow
!     row i
         do 200 ka=ia(ii), ia(ii+1)-1
            len = len+1
            jcol    = ja(ka)
            if (len .gt. nzmax) goto 999
            jc(len) = jcol
            if (values) c(len)  = a(ka)
            iw(jcol)= len
 200     continue
!
         do 300 kb=ib(ii),ib(ii+1)-1
            jcol = jb(kb)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol
               if (values) c(len)  = b(kb)
               iw(jcol)= len
            else
               if (values) c(jpos) = c(jpos) + b(kb)
            endif
 300     continue
         do 301 k=ic(ii), len
       iw(jc(k)) = 0
 301     continue
         ic(ii+1) = len+1
 500  continue
      return
 999  ierr = ii
      return
!------------end of aplb -----------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine aplb1(nrow,ncol,job,a,ja,ia,b,jb,ib,c,jc,ic,nzmax,ierr)
      integer, intent(in) :: nrow, ncol, nzmax, job
      integer, intent(out) :: ierr
      real*8, intent(inout) :: a(*), b(*), c(*)
      integer, intent(inout):: ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1)
     * ,ic(nrow+1)
      integer :: i, ka, kb, kamax, kbmax, j2,kc, j1
!-----------------------------------------------------------------------
! performs the matrix sum  C = A+B for matrices in sorted CSR format.
! the difference with aplb  is that the resulting matrix is such that
! the elements of each row are sorted with increasing column indices in
! each row, provided the original matrices are sorted in the same way.
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A and B
! ncol  = integer. The column dimension of A and B.
! job   = integer. Job indicator. When job = 0, only the structure
!                  (i.e. the arrays jc, ic) is computed and the
!                  real values are ignored.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format with entries sorted
!
! b,
! jb,
! ib  =  Matrix B in compressed sparse row format with entries sorted
!        ascendly in each row
!
! nzmax  = integer. The  length of the arrays c and jc.
!         amub will stop if the result matrix C  has a number
!         of elements that exceeds exceeds nzmax. See ierr.
!
! on return:
!----------
! c,
! jc,
! ic  = resulting matrix C in compressed sparse row sparse format
!         with entries sorted ascendly in each row.
c  
! ierr   = integer. serving as error message.
!         ierr = 0 means normal return,
!         ierr .gt. 0 means that amub stopped while computing the
!         i-th row  of C with i=ierr, because the number
!         of elements in C exceeds nzmax.
!
! Notes:
!-------
!     this will not work if any of the two input matrices is not sorted
!-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0)
      ierr = 0
!     kc = 1
!     ic(1) = kc
!
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1
 5       continue
         if (ka .le. kamax) then
            j1 = ja(ka)
         else
            j1 = ncol+1
         endif
         if (kb .le. kbmax) then
            j2 = jb(kb)
         else
            j2 = ncol+1
         endif
!
!     three cases
!
         if (j1 .eq. j2) then
            if (values) c(kc) = a(ka)+b(kb)
            jc(kc) = j1
            ka = ka+1
            kb = kb+1
!           kc = kc+1
         else if (j1 .lt. j2) then
            jc(kc) = j1
            if (values) c(kc) = a(ka)
            ka = ka+1
!           kc = kc+1
         else if (j1 .gt. j2) then
            jc(kc) = j2
            if (values) c(kc) = b(kb)
            kb = kb+1
!           kc = kc+1
         endif
!        if (kc .gt. nzmax) goto 999
         if (ka .le. kamax .or. kb .le. kbmax) goto 5
!        ic(i+1) = kc
 6    continue
      return
 999  ierr = i
      return
!------------end-of-aplb1-----------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine aplsb (nrow,ncol,a,ja,ia,s,b,jb,ib,c,jc,ic,
     *     nzmax,ierr)
      integer, intent(in) :: nrow, ncol, nzmax
      integer, intent(out) :: ierr
      real*8, intent(inout) :: a(*), b(*), c(*), s
      integer, intent(inout) :: ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1)
     * ,ic(nrow+1)
      integer :: i, ka, kb, kamax, kbmax, j1, j2, kc
!-----------------------------------------------------------------------
! performs the operation C = A+s B for matrices in sorted CSR format.
! the difference with aplsb is that the resulting matrix is such that
! the elements of each row are sorted with increasing column indices in
! each row, provided the original matrices are sorted in the same way.
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A and B
! ncol  = integer. The column dimension of A and B.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format with entries sorted
!
! s   = real. scalar factor for B.
!
! b,
! jb,
! ib  =  Matrix B in compressed sparse row format with entries sorted
!        ascendly in each row
!
! nzmax  = integer. The  length of the arrays c and jc.
!         amub will stop if the result matrix C  has a number
!         of elements that exceeds exceeds nzmax. See ierr.
!
! on return:
!----------
! c,
! jc,
! ic  = resulting matrix C in compressed sparse row sparse format
!         with entries sorted ascendly in each row.
c  
! ierr   = integer. serving as error message.
!         ierr = 0 means normal return,
!         ierr .gt. 0 means that amub stopped while computing the
!         i-th row  of C with i=ierr, because the number
!         of elements in C exceeds nzmax.
!
! Notes:
!-------
!     this will not work if any of the two input matrices is not sorted
!-----------------------------------------------------------------------
      ierr = 0
!     kc = 1
!     ic(1) = kc
!
!     the following loop does a merge of two sparse rows + adds  them.
!
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1
 5       continue
!
!     this is a while  -- do loop --
!
         if (ka .le. kamax .or. kb .le. kbmax) then
!
            if (ka .le. kamax) then
               j1 = ja(ka)
            else
!     take j1 large enough  that always j2 .lt. j1
               j1 = ncol+1
            endif
            if (kb .le. kbmax) then
               j2 = jb(kb)
            else
!     similarly take j2 large enough  that always j1 .lt. j2
               j2 = ncol+1
            endif
!
!     three cases
!
            if (j1 .eq. j2) then
               c(kc) = a(ka)+s*b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
!              kc = kc+1
            else if (j1 .lt. j2) then
               jc(kc) = j1
               c(kc) = a(ka)
               ka = ka+1
!              kc = kc+1
            else if (j1 .gt. j2) then
               jc(kc) = j2
               c(kc) = s*b(kb)
               kb = kb+1
!              kc = kc+1
            endif
!           if (kc .gt. nzmax) goto 999
            goto 5
!
!     end while loop
!
         endif
!        ic(i+1) = kc
 6    continue
      return
 999  ierr = i
      return
!------------end-of-aplsb ---------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine aplsb1 (nrow,ncol,a,ja,ia,s,b,jb,ib,c,jc,ic,
     *     nzmax,ierr)
      integer, intent(in) :: nrow, ncol, nzmax
      integer, intent(out) :: ierr
      real*8, intent(inout) :: a(*), b(*), c(*), s
      integer, intent(inout) :: ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1)
     * ,ic(nrow+1)
      integer :: i, ka, kb, kamax, kbmax, j1, j2, kc
!-----------------------------------------------------------------------
! performs the operation C = A+s B for matrices in sorted CSR format.
! the difference with aplsb is that the resulting matrix is such that
! the elements of each row are sorted with increasing column indices in
! each row, provided the original matrices are sorted in the same way.
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A and B
! ncol  = integer. The column dimension of A and B.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format with entries sorted
!
! s   = real. scalar factor for B.
!
! b,
! jb,
! ib  =  Matrix B in compressed sparse row format with entries sorted
!        ascendly in each row
!
! nzmax  = integer. The  length of the arrays c and jc.
!         amub will stop if the result matrix C  has a number
!         of elements that exceeds exceeds nzmax. See ierr.
!
! on return:
!----------
! c,
! jc,
! ic  = resulting matrix C in compressed sparse row sparse format
!         with entries sorted ascendly in each row.
c  
! ierr   = integer. serving as error message.
!         ierr = 0 means normal return,
!         ierr .gt. 0 means that amub stopped while computing the
!         i-th row  of C with i=ierr, because the number
!         of elements in C exceeds nzmax.
!
! Notes:
!-------
!     this will not work if any of the two input matrices is not sorted
!-----------------------------------------------------------------------
      ierr = 0
!     kc = 1
!     ic(1) = kc
!
!     the following loop does a merge of two sparse rows + adds  them.
!
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1
 5       continue
!
!     this is a while  -- do loop --
!
         if (ka .le. kamax .or. kb .le. kbmax) then
!
            if (ka .le. kamax) then
               j1 = ja(ka)
            else
!     take j1 large enough  that always j2 .lt. j1
               j1 = ncol+1
            endif
            if (kb .le. kbmax) then
               j2 = jb(kb)
            else
!     similarly take j2 large enough  that always j1 .lt. j2
               j2 = ncol+1
            endif
!
!     three cases
!
            if (j1 .eq. j2) then
               c(kc) = a(ka)+s*b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
!              kc = kc+1
            else if (j1 .lt. j2) then
               jc(kc) = j1
               c(kc) = a(ka)
               ka = ka+1
!              kc = kc+1
            else if (j1 .gt. j2) then
               jc(kc) = j2
               c(kc) = s*b(kb)
               kb = kb+1
!              kc = kc+1
            endif
!           if (kc .gt. nzmax) goto 999
            goto 5
!
!     end while loop
!
         endif
!        ic(i+1) = kc
 6    continue
      return
 999  ierr = i
      return
!------------end-of-aplsb1 ---------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine apmbt (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      integer, intent(in) :: nrow, ncol, nzmax, job
      integer, intent(out) :: ierr
      real*8, intent(inout) :: a(*), b(*), c(*)
      integer, intent(inout) :: ja(*),jb(*),jc(*),ia(nrow+1),ib(ncol+1)
     * ,ic(*),iw(*)
      integer :: j, nnza, nnzb, ljob, ipos, k, ii, jcol, jpos, i, len
      integer :: ka
!-----------------------------------------------------------------------
! performs the matrix sum  C = A + transp(B) or C = A - transp(B)
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A and transp(B)
! ncol  = integer. The column dimension of A. Also the row
!                  dimension of B.
!
! job = integer. if job = -1, apmbt will compute C= A - transp(B)
!         (structure + values)
!         if (job .eq. 1)  it will compute C=A+transp(A)
!         (structure+ values)
!         if (job .eq. 0) it will compute the structure of
!         C= A+/-transp(B) only (ignoring all real values).
!         any other value of job will be treated as  job=1
! a,
! ja,
! ia    = Matrix A in compressed sparse row format.
!
! b,
! jb,
! ib  =  Matrix B in compressed sparse row format.
!
! nzmax  = integer. The  length of the arrays c, jc, and ic.
!         amub will stop if the result matrix C  has a number
!         of elements that exceeds exceeds nzmax. See ierr.
!
! on return:
!----------
! c,
! jc,
! ic  = resulting matrix C in compressed sparse row format.
c  
! ierr   = integer. serving as error message.
!         ierr = 0 means normal return.
!         ierr = -1 means that nzmax was .lt. either the number of
!         nonzero elements of A or the number of nonzero elements in B.
!         ierr .gt. 0 means that amub stopped while computing the
!         i-th row  of C with i=ierr, because the number
!         of elements in C exceeds nzmax.
!
! work arrays:
!------------
! iw  = integer work array of length at least max(ncol,nrow)
!
! Notes:
!------- It is important to note that here all of three arrays c, ic,
!        and jc are assumed to be of length nnz(c). This is because
!        the matrix is internally converted in coordinate format.
!
!-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0)
!
      ierr = 0
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
!
      nnza = ia(nrow+1)-1
      nnzb = ib(ncol+1)-1
      len = nnzb
      if (nzmax .lt. nnzb .or. nzmax .lt. nnza) then
         ierr = -1
         return
      endif
!
! trasnpose matrix b into c
!
      ljob = 0
      if (values) ljob = 1
      ipos = 1
!     call csrcsc (ncol,ljob,ipos,b,jb,ib,c,jc,ic)
!-----------------------------------------------------------------------
      if (job .eq. -1) then
         do 2 k=1,len
       c(k) = -c(k)
 2       continue
      endif
!
!--------------- main loop --------------------------------------------
!
      do 500 ii=1, nrow
         do 200 k = ic(ii),ic(ii+1)-1
            iw(jc(k)) = k
 200     continue
!-----------------------------------------------------------------------
         do 300 ka = ia(ii), ia(ii+1)-1
            jcol = ja(ka)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
!
!     if fill-in append in coordinate format to matrix.
!
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol

               ic(len) = ii
               if (values) c(len)  = a(ka)
            else
!     else do addition.
               if (values) c(jpos) = c(jpos) + a(ka)
            endif
 300     continue
         do 301 k=ic(ii), ic(ii+1)-1
       iw(jc(k)) = 0
 301     continue
 500  continue
!
!     convert first part of matrix (without fill-ins) into coo format
!
      ljob = 2
      if (values) ljob = 3
      do 501 i=1, nrow+1
         iw(i) = ic(i)
 501  continue
      call csrcoo (nrow,ljob,nnzb,c,jc,iw,nnzb,c,ic,jc,ierr)
!
!     convert the whole thing back to csr format.
!
      ljob = 0
      if (values) ljob = 1
      call coicsr (nrow,len,ljob,c,jc,ic,iw)
      return
 999  ierr = ii
      return
!--------end-of-apmbt---------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine aplsbt(nrow,ncol,a,ja,ia,s,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      integer, intent(in) :: nrow, ncol, nzmax
      integer, intent(out) :: ierr
      real*8, intent(inout) :: a(*), b(*), c(*), s
      integer, intent(inout) :: ja(*),jb(*),jc(*),ia(nrow+1),ib(ncol+1)
     * ,ic(*),iw(*)
      integer :: j, nnza, nnzb, len, k, ii, ka, jcol, ljob, ipos, jpos
      integer :: i
!-----------------------------------------------------------------------
! performs the matrix sum  C = A + transp(B).
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A and transp(B)
! ncol  = integer. The column dimension of A. Also the row
!                  dimension of B.
!
! a,
! ja,
! ia    = Matrix A in compressed sparse row format.
!
! s   = real. scalar factor for B.
!
!
! b,
! jb,
! ib  =  Matrix B in compressed sparse row format.
!
! nzmax  = integer. The  length of the arrays c, jc, and ic.
!         amub will stop if the result matrix C  has a number
!         of elements that exceeds exceeds nzmax. See ierr.
!
! on return:
!----------
! c,
! jc,
! ic  = resulting matrix C in compressed sparse row format.
c  
! ierr   = integer. serving as error message.
!         ierr = 0 means normal return.
!         ierr = -1 means that nzmax was .lt. either the number of
!         nonzero elements of A or the number of nonzero elements in B.
!         ierr .gt. 0 means that amub stopped while computing the
!         i-th row  of C with i=ierr, because the number
!         of elements in C exceeds nzmax.
!
! work arrays:
!------------
! iw  = integer work array of length at least max(nrow,ncol)
!
! Notes:
!------- It is important to note that here all of three arrays c, ic,
!        and jc are assumed to be of length nnz(c). This is because
!        the matrix is internally converted in coordinate format.
!
!-----------------------------------------------------------------------
      ierr = 0
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
!
      nnza = ia(nrow+1)-1
      nnzb = ib(ncol+1)-1
      len = nnzb
      if (nzmax .lt. nnzb .or. nzmax .lt. nnza) then
         ierr = -1
         return
      endif
!
!     transpose matrix b into c
!
      ljob = 1
      ipos = 1
!     call csrcsc (ncol,ljob,ipos,b,jb,ib,c,jc,ic)
      do 2 k=1,len
 2       c(k) = c(k)*s
!
!     main loop. add rows from ii = 1 to nrow.
!
         do 500 ii=1, nrow
!     iw is used as a system to recognize whether there
!     was a nonzero element in c.
            do 200 k = ic(ii),ic(ii+1)-1
               iw(jc(k)) = k
 200        continue
!
            do 300 ka = ia(ii), ia(ii+1)-1
               jcol = ja(ka)
               jpos = iw(jcol)
           if (jpos .eq. 0) then
!
!     if fill-in append in coordinate format to matrix.
!
              len = len+1
              if (len .gt. nzmax) goto 999
              jc(len) = jcol
              ic(len) = ii
              c(len)  = a(ka)
           else
!     else do addition.
              c(jpos) = c(jpos) + a(ka)
           endif
 300    continue
        do 301 k=ic(ii), ic(ii+1)-1
           iw(jc(k)) = 0
 301    continue
 500  continue
!
!     convert first part of matrix (without fill-ins) into coo format
!
      ljob = 3
      do 501 i=1, nrow+1
         iw(i) = ic(i)
 501  continue
      call csrcoo (nrow,ljob,nnzb,c,jc,iw,nnzb,c,ic,jc,ierr)
!
!     convert the whole thing back to csr format.
!
      ljob = 1
      call coicsr (nrow,len,ljob,c,jc,ic,iw)
      return
 999  ierr = ii
      return
!--------end-of-aplsbt--------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine diamua (nrow,job, a, ja, ia, diag, b, jb, ib)
      integer, intent(in) :: nrow, job
      real*8, intent(inout) :: a(*), b(*), diag(nrow)
      integer, intent(inout) :: ja(*),jb(*), ia(nrow+1),ib(nrow+1)
      real*8 :: scal
      integer :: ii, k1, k2, k
!-----------------------------------------------------------------------
! performs the matrix by matrix product B = Diag * A  (in place)
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A
!
! job   = integer. job indicator. Job=0 means get array b only
!         job = 1 means get b, and the integer arrays ib, jb.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format.
!
! diag = diagonal matrix stored as a vector dig(1:n)
!
! on return:
!----------
!
! b,
! jb,
! ib  = resulting matrix B in compressed sparse row sparse format.
c  
! Notes:
!-------
! 1)        The column dimension of A is not needed.
! 2)        algorithm in place (B can take the place of A).
!           in this case use job=0.
!-----------------------------------------------------------------
      do 1 ii=1,nrow
!
!     normalize each row
!
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         scal = diag(ii)
         do 2 k=k1, k2
            b(k) = a(k)*scal
 2       continue
 1    continue
!
      if (job .eq. 0) return
!
      do 3 ii=1, nrow+1
         ib(ii) = ia(ii)
 3    continue
      do 31 k=ia(1), ia(nrow+1) -1
         jb(k) = ja(k)
 31   continue
      return
!----------end-of-diamua------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine amudia (nrow,job, a, ja, ia, diag, b, jb, ib)
      integer, intent(in) :: nrow, job
      real*8, intent(inout) :: a(*), b(*), diag(nrow)
      integer, intent(inout) :: ja(*),jb(*), ia(nrow+1),ib(nrow+1)
      integer :: ii, k1, k2, k
!-----------------------------------------------------------------------
! performs the matrix by matrix product B = A * Diag  (in place)
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A
!
! job   = integer. job indicator. Job=0 means get array b only
!         job = 1 means get b, and the integer arrays ib, jb.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format.
!
! diag = diagonal matrix stored as a vector dig(1:n)
!
! on return:
!----------
!
! b,
! jb,
! ib  = resulting matrix B in compressed sparse row sparse format.
c  
! Notes:
!-------
! 1)        The column dimension of A is not needed.
! 2)        algorithm in place (B can take the place of A).
!-----------------------------------------------------------------
      do 1 ii=1,nrow
!
!     scale each element
!
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         do 2 k=k1, k2
            b(k) = a(k)*diag(ja(k))
 2       continue
 1    continue
!
      if (job .eq. 0) return
!
      do 3 ii=1, nrow+1
         ib(ii) = ia(ii)
 3    continue
      do 31 k=ia(1), ia(nrow+1) -1
         jb(k) = ja(k)
 31   continue
      return
!-----------------------------------------------------------------------
!-----------end-of-amudiag----------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine aplsca (nrow, a, ja, ia, scal,iw)
      integer, intent(in) :: nrow
      real*8, intent(inout) :: a(*), scal
      integer, intent(inout) :: ja(*), ia(nrow+1),iw(*)
      integer :: icount, j, ko, ii, k1, k2, k
!-----------------------------------------------------------------------
! Adds a scalar to the diagonal entries of a sparse matrix A :=A + s I
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A
!
! a,
! ja,
! ia    = Matrix A in compressed sparse row format.
!
! scal  = real. scalar to add to the diagonal entries.
!
! on return:
!----------
!
! a,
! ja,
! ia  = matrix A with diagonal elements shifted (or created).
c  
! iw    = integer work array of length n. On return iw will
!         contain  the positions of the diagonal entries in the
!         output matrix. (i.e., a(iw(k)), ja(iw(k)), k=1,...n,
!         are the values/column indices of the diagonal elements
!         of the output matrix. ).
!
! Notes:
!-------
!     The column dimension of A is not needed.
!     important: the matrix a may be expanded slightly to allow for
!     additions of nonzero elements to previously nonexisting diagonals.
!     The is no checking as to whether there is enough space appended
!     to the arrays a and ja. if not sure allow for n additional
!     elemnts.
!     coded by Y. Saad. Latest version July, 19, 1990
!-----------------------------------------------------------------------
      logical test
!
      call diapos (nrow,ja,ia,iw)
      icount = 0
      do 1 j=1, nrow
         if (iw(j) .eq. 0) then
            icount = icount+1
         else
            a(iw(j)) = a(iw(j)) + scal
         endif
 1    continue
!
!     if no diagonal elements to insert in data structure return.
!
      if (icount .eq. 0) return
!
! shift the nonzero elements if needed, to allow for created
! diagonal elements.
!
      ko = ia(nrow+1)+icount
!
!     copy rows backward
!
      do 5 ii=nrow, 1, -1
!
!     go through  row ii
!
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         ia(ii+1) = ko
         test = (iw(ii) .eq. 0)
         do 4 k = k2,k1,-1
            j = ja(k)
            if (test .and. (j .lt. ii)) then
               test = .false.
               ko = ko - 1
               a(ko) = scal
               ja(ko) = ii
               iw(ii) = ko
            endif
            ko = ko-1
            a(ko) = a(k)
            ja(ko) = j
 4       continue
!     diagonal element has not been added yet.
         if (test) then
            ko = ko-1
            a(ko) = scal
            ja(ko) = ii
            iw(ii) = ko
         endif
 5    continue
      ia(1) = ko
      return
!-----------------------------------------------------------------------
!----------end-of-aplsca------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine apldia (nrow, job, a, ja, ia, diag, b, jb, ib, iw)
      integer, intent(in) :: nrow, job
      real*8, intent(inout) :: a(*), b(*), diag(nrow)
      integer, intent(inout) :: ja(*),jb(*), ia(nrow+1),ib(nrow+1),iw(*)
      integer :: nnz, k, icount, j, ko, ii, k1, k2
!-----------------------------------------------------------------------
! Adds a diagonal matrix to a general sparse matrix:  B = A + Diag
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow   = integer. The row dimension of A
!
! job   = integer. job indicator. Job=0 means get array b only
!         (i.e. assume that a has already been copied into array b,
!         or that algorithm is used in place. ) For all practical
!         purposes enter job=0 for an in-place call and job=1 otherwise
!
!         Note: in case there are missing diagonal elements in A,
!         then the option job =0 will be ignored, since the algorithm
!         must modify the data structure (i.e. jb, ib) in this
!         situation.
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format.
!
! diag = diagonal matrix stored as a vector dig(1:n)
!
! on return:
!----------
!
! b,
! jb,
! ib  = resulting matrix B in compressed sparse row sparse format.
!
!
! iw    = integer work array of length n. On return iw will
!         contain  the positions of the diagonal entries in the
!         output matrix. (i.e., a(iw(k)), ja(iw(k)), k=1,...n,
!         are the values/column indices of the diagonal elements
!         of the output matrix. ).
!
! Notes:
!-------
! 1)        The column dimension of A is not needed.
! 2)        algorithm in place (b, jb, ib, can be the same as
!           a, ja, ia, on entry). See comments for parameter job.
!
! coded by Y. Saad. Latest version July, 19, 1990
!-----------------------------------------------------------------
      logical test
!
!     copy integer arrays into b's data structure if required
!
      if (job .ne. 0) then
         nnz = ia(nrow+1)-1
         do 2  k=1, nnz
            jb(k) = ja(k)
            b(k)  = a(k)
 2       continue
         do 3 k=1, nrow+1
            ib(k) = ia(k)
 3       continue
      endif
!
!     get positions of diagonal elements in data structure.
!
      call diapos (nrow,ja,ia,iw)
!
!     count number of holes in diagonal and add diag(*) elements to
!     valid diagonal entries.
!
      icount = 0
      do 1 j=1, nrow
         if (iw(j) .eq. 0) then
            icount = icount+1
         else
            b(iw(j)) = a(iw(j)) + diag(j)
         endif
 1    continue
!
!     if no diagonal elements to insert return
!
      if (icount .eq. 0) return
!
!     shift the nonzero elements if needed, to allow for created
!     diagonal elements.
!
      ko = ib(nrow+1)+icount
!
!     copy rows backward
!
      do 5 ii=nrow, 1, -1
!
!     go through  row ii
!
         k1 = ib(ii)
         k2 = ib(ii+1)-1
         ib(ii+1) = ko
         test = (iw(ii) .eq. 0)
         do 4 k = k2,k1,-1
            j = jb(k)
            if (test .and. (j .lt. ii)) then
               test = .false.
               ko = ko - 1
               b(ko) = diag(ii)
               jb(ko) = ii
               iw(ii) = ko
            endif
            ko = ko-1
            b(ko) = a(k)
            jb(ko) = j
 4       continue
!     diagonal element has not been added yet.
         if (test) then
            ko = ko-1
            b(ko) =  diag(ii)
            jb(ko) = ii
            iw(ii) = ko
         endif
 5    continue
      ib(1) = ko
      return
!-----------------------------------------------------------------------
!------------end-of-apldiag---------------------------------------------
      end
!----------------------------------------------------------------------c
!                          S P A R S K I T                             c
!----------------------------------------------------------------------c
!          BASIC MATRIX-VECTOR OPERATIONS - MATVEC MODULE              c
!         Matrix-vector Mulitiplications and Triang. Solves            c
!----------------------------------------------------------------------c
! contents: (as of Nov 18, 1991)                                       c
!----------                                                            c
! 1) Matrix-vector products:                                           c
!---------------------------                                           c
! amux  : A times a vector. Compressed Sparse Row (CSR) format.        c
! amuxms: A times a vector. Modified Compress Sparse Row format.       c
! atmux : Transp(A) times a vector. CSR format.                        c
! atmuxr: Transp(A) times a vector. CSR format. A rectangular.         c
! amuxe : A times a vector. Ellpack/Itpack (ELL) format.               c
! amuxd : A times a vector. Diagonal (DIA) format.                     c
! amuxj : A times a vector. Jagged Diagonal (JAD) format.              c
! vbrmv : Sparse matrix-full vector product, in VBR format             c
!                                                                      c
! 2) Triangular system solutions:                                      c
!-------------------------------                                       c
! lsol  : Unit Lower Triang. solve. Compressed Sparse Row (CSR) format.c
! ldsol : Lower Triang. solve.  Modified Sparse Row (MSR) format.      c
! lsolc : Unit Lower Triang. solve. Comp. Sparse Column (CSC) format.  c
! ldsolc: Lower Triang. solve. Modified Sparse Column (MSC) format.    c
! ldsoll: Lower Triang. solve with level scheduling. MSR format.       c
! usol  : Unit Upper Triang. solve. Compressed Sparse Row (CSR) format.c
! udsol : Upper Triang. solve.  Modified Sparse Row (MSR) format.      c
! usolc : Unit Upper Triang. solve. Comp. Sparse Column (CSC) format.  c
! udsolc: Upper Triang. solve.  Modified Sparse Column (MSC) format.   c
!----------------------------------------------------------------------c
! 1)     M A T R I X    B Y    V E C T O R     P R O D U C T S         c
!----------------------------------------------------------------------c

!-----------------------------------------------------------------------
      subroutine amuxms (n, x, y, a,ja)
      real*8  x(*), y(*), a(*)
      integer n, ja(*)
!-----------------------------------------------------------------------
!         A times a vector in MSR format
!-----------------------------------------------------------------------
! multiplies a matrix by a vector using the dot product form
! Matrix A is stored in Modified Sparse Row storage.
!
! on entry:
!----------
! n     = row dimension of A
! x     = real array of length equal to the column dimension of
!         the A matrix.
! a, ja,= input matrix in modified compressed sparse row format.
!
! on return:
!-----------
! y     = real array of length n, containing the product y=Ax
!
!-----------------------------------------------------------------------
! local variables
!
      integer i, k
!-----------------------------------------------------------------------
        do 10 i=1, n
        y(i) = a(i)*x(i)
 10     continue
      do 100 i = 1,n
!
!     compute the inner product of row i with vector x
!
         do 99 k=ja(i), ja(i+1)-1
            y(i) = y(i) + a(k) *x(ja(k))
 99      continue
 100  continue
!
      return
!---------end-of-amuxm--------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
      subroutine atmuxr (m, n, x, y, a, ja, ia)
      real*8 x(*), y(*), a(*)
      integer m, n, ia(*), ja(*)
!-----------------------------------------------------------------------
!         transp( A ) times a vector, A can be rectangular
!-----------------------------------------------------------------------
! See also atmux.  The essential difference is how the solution vector
! is initially zeroed.  If using this to multiply rectangular CSC
! matrices by a vector, m number of rows, n is number of columns.
!-----------------------------------------------------------------------
!
! on entry:
!----------
! m     = column dimension of A
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
      do 1 i=1,m
         y(i) = 0.0
 1    continue
!
! loop over the rows
!
      do 100 i = 1,n
         do 99 k=ia(i), ia(i+1)-1
            y(ja(k)) = y(ja(k)) + x(i)*a(k)
 99      continue
 100  continue
!
      return
!-------------end-of-atmuxr---------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine amuxe (n,x,y,na,ncol,a,ja)
      integer, intent(in) :: n, na, ncol
      real*8, intent(inout) :: x(n), y(n), a(na,*)
      integer, intent(inout) :: ja(na,*)
!-----------------------------------------------------------------------
!        A times a vector in Ellpack Itpack format (ELL)
!-----------------------------------------------------------------------
! multiplies a matrix by a vector when the original matrix is stored
! in the ellpack-itpack sparse format.
!-----------------------------------------------------------------------
!
! on entry:
!----------
! n     = row dimension of A
! x     = real array of length equal to the column dimension of
!         the A matrix.
! na    = integer. The first dimension of arrays a and ja
!         as declared by the calling program.
! ncol  = integer. The number of active columns in array a.
!         (i.e., the number of generalized diagonals in matrix.)
! a, ja = the real and integer arrays of the itpack format
!         (a(i,k),k=1,ncol contains the elements of row i in matrix
!          ja(i,k),k=1,ncol contains their column numbers)
!
! on return:
!-----------
! y     = real array of length n, containing the product y=y=A*x
!
!-----------------------------------------------------------------------
! local variables
!
      integer i, j
!-----------------------------------------------------------------------
      do 1 i=1, n
         y(i) = 0.0
 1    continue
      do 10 j=1,ncol
         do 25 i = 1,n
            y(i) = y(i)+a(i,j)*x(ja(i,j))
 25      continue
 10   continue
!
      return
!--------end-of-amuxe---------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine amuxd (n,x,y,diag,ndiag,idiag,ioff)
      integer n, ndiag, idiag, ioff(idiag)
      real*8 x(n), y(n), diag(ndiag,idiag)
!-----------------------------------------------------------------------
!        A times a vector in Diagonal storage format (DIA)
!-----------------------------------------------------------------------
! multiplies a matrix by a vector when the original matrix is stored
! in the diagonal storage format.
!-----------------------------------------------------------------------
!
! on entry:
!----------
! n     = row dimension of A
! x     = real array of length equal to the column dimension of
!         the A matrix.
! ndiag  = integer. The first dimension of array adiag as declared in
!         the calling program.
! idiag  = integer. The number of diagonals in the matrix.
! diag   = real array containing the diagonals stored of A.
! idiag  = number of diagonals in matrix.
! diag   = real array of size (ndiag x idiag) containing the diagonals
!
! ioff   = integer array of length idiag, containing the offsets of the
!        diagonals of the matrix:
!          diag(i,k) contains the element a(i,i+ioff(k)) of the matrix.
!
! on return:
!-----------
! y     = real array of length n, containing the product y=A*x
!
!-----------------------------------------------------------------------
! local variables
!
      integer j, k, io, i1, i2
!-----------------------------------------------------------------------
      do 1 j=1, n
         y(j) = 0.0d0
 1    continue 
      do 10 j=1, idiag
         io = ioff(j)
         i1 = max0(1,1-io)
         i2 = min0(n,n-io)
         do 9 k=i1, i2  
            y(k) = y(k)+diag(k,j)*x(k+io)
 9       continue
 10   continue
!
      return
!----------end-of-amuxd-------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine amuxj (n, x, y, jdiag, a, ja, ia)
      integer n, jdiag, ja(*), ia(*)
      real*8 x(n), y(n), a(*)
!-----------------------------------------------------------------------
!        A times a vector in Jagged-Diagonal storage format (JAD)
!-----------------------------------------------------------------------
! multiplies a matrix by a vector when the original matrix is stored
! in the jagged diagonal storage format.
!-----------------------------------------------------------------------
!
! on entry:
!----------
! n      = row dimension of A
! x      = real array of length equal to the column dimension of
!         the A matrix.
! jdiag  = integer. The number of jadded-diagonals in the data-structure.
! a      = real array containing the jadded diagonals of A stored
!          in succession (in decreasing lengths)
! j      = integer array containing the colum indices of the
!          corresponding elements in a.
! ia     = integer array containing the lengths of the  jagged diagonals
!
! on return:
!-----------
! y      = real array of length n, containing the product y=A*x
!
! Note:
!-------
! Permutation related to the JAD format is not performed.
! this can be done by:
!     call permvec (n,y,y,iperm)
! after the call to amuxj, where iperm is the permutation produced
! by csrjad.
!-----------------------------------------------------------------------
! local variables
!
      integer i, ii, k1, len, j
!-----------------------------------------------------------------------
      do 1 i=1, n
         y(i) = 0.0d0
 1    continue
      do 70 ii=1, jdiag
         k1 = ia(ii)-1
         len = ia(ii+1)-k1-1
         do 60 j=1,len
            y(j)= y(j)+a(k1+j)*x(ja(k1+j))
 60      continue
 70   continue
!
      return
!----------end-of-amuxj-------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine vbrmv(nr, nc, ia, ja, ka, a, kvstr, kvstc, x, b)
!-----------------------------------------------------------------------
      integer, intent(in) :: nr, nc
      integer, intent(inout) :: ia(nr+1), ja(*), ka(*), kvstr(nr+1)
     * , kvstc(*)
      real*8, intent(inout) ::  a(*), x(*), b(*)
!-----------------------------------------------------------------------
!     Sparse matrix-full vector product, in VBR format.
!-----------------------------------------------------------------------
!     On entry:
!--------------
!     nr, nc  = number of block rows and columns in matrix A
!     ia,ja,ka,a,kvstr,kvstc = matrix A in variable block row format
!     x       = multiplier vector in full format
!
!     On return:
!---------------
!     b = product of matrix A times vector x in full format
!
!     Algorithm:
!---------------
!     Perform multiplication by traversing a in order.
!
!-----------------------------------------------------------------------
!-----local variables
      integer n, i, j, ii, jj, k, istart, istop
      real*8  xjj
!---------------------------------
      n = kvstc(nc+1)-1
      do i = 1, n
         b(i) = 0.d0
      enddo
!---------------------------------
      k = 1
      do i = 1, nr
         istart = kvstr(i)
         istop  = kvstr(i+1)-1
         do j = ia(i), ia(i+1)-1
            do jj = kvstc(ja(j)), kvstc(ja(j)+1)-1
               xjj = x(jj)
               do ii = istart, istop
                  b(ii) = b(ii) + xjj*a(k)
                  k = k + 1
               enddo
            enddo
         enddo
      enddo
!---------------------------------
      return
      end
!-----------------------------------------------------------------------
!----------------------end-of-vbrmv-------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------c
! 2)     T R I A N G U L A R    S Y S T E M    S O L U T I O N S       c
!----------------------------------------------------------------------c
      subroutine lsol (n,x,y,al,jal,ial)
      integer n, jal(*),ial(n+1)
      real*8  x(n), y(n), al(*)
!-----------------------------------------------------------------------
!   solves    L x = y ; L = lower unit triang. /  CSR format
!-----------------------------------------------------------------------
! solves a unit lower triangular system by standard (sequential )
! forward elimination - matrix stored in CSR format.
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real array containg the right side.
!
! al,
! jal,
! ial,    = Lower triangular matrix stored in compressed sparse row
!          format.
!
! On return:
!-----------
c  x  = The solution of  L x  = y.
!--------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8  t
!-----------------------------------------------------------------------
      x(1) = y(1)
      do 150 k = 2, n
         t = y(k)
         do 100 j = ial(k), ial(k+1)-1
            t = t-al(j)*x(jal(j))
 100     continue
         x(k) = t
 150  continue
!
      return
!----------end-of-lsol--------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine ldsol (n,x,y,al,jal)
      integer n, jal(*)
      real*8 x(n), y(n), al(*)
!-----------------------------------------------------------------------
!     Solves L x = y    L = triangular. MSR format
!-----------------------------------------------------------------------
! solves a (non-unit) lower triangular system by standard (sequential)
! forward elimination - matrix stored in MSR format
! with diagonal elements already inverted (otherwise do inversion,
! al(1:n) = 1.0/al(1:n),  before calling ldsol).
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real array containg the right hand side.
!
! al,
! jal,   = Lower triangular matrix stored in Modified Sparse Row
!          format.
!
! On return:
!-----------
c  x = The solution of  L x = y .
!--------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8 t
!-----------------------------------------------------------------------
      x(1) = y(1)*al(1)
      do 150 k = 2, n
         t = y(k)
         do 100 j = jal(k), jal(k+1)-1
            t = t - al(j)*x(jal(j))
 100     continue
         x(k) = al(k)*t
 150  continue
      return
!----------end-of-ldsol-------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine lsolc (n,x,y,al,jal,ial)
      integer n, jal(*),ial(*)
      real*8  x(n), y(n), al(*)
!-----------------------------------------------------------------------
!       SOLVES     L x = y ;    where L = unit lower trang. CSC format
!-----------------------------------------------------------------------
! solves a unit lower triangular system by standard (sequential )
! forward elimination - matrix stored in CSC format.
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real*8 array containg the right side.
!
! al,
! jal,
! ial,    = Lower triangular matrix stored in compressed sparse column
!          format.
!
! On return:
!-----------
c  x  = The solution of  L x  = y.
!-----------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8 t
!-----------------------------------------------------------------------
      do 140 k=1,n
         x(k) = y(k)
 140  continue
      do 150 k = 1, n-1
         t = x(k)
         do 100 j = ial(k), ial(k+1)-1
            x(jal(j)) = x(jal(j)) - t*al(j)
 100     continue
 150  continue
!
      return
!----------end-of-lsolc-------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine ldsolc (n,x,y,al,jal)
      integer n, jal(*)
      real*8 x(n), y(n), al(*)
!-----------------------------------------------------------------------
!    Solves     L x = y ;    L = nonunit Low. Triang. MSC format
!-----------------------------------------------------------------------
! solves a (non-unit) lower triangular system by standard (sequential)
! forward elimination - matrix stored in Modified Sparse Column format
! with diagonal elements already inverted (otherwise do inversion,
! al(1:n) = 1.0/al(1:n),  before calling ldsol).
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real array containg the right hand side.
!
! al,
! jal,
! ial,    = Lower triangular matrix stored in Modified Sparse Column
!           format.
!
! On return:
!-----------
c  x = The solution of  L x = y .
!--------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8 t
!-----------------------------------------------------------------------
      do 140 k=1,n
         x(k) = y(k)
 140  continue
      do 150 k = 1, n
         x(k) = x(k)*al(k)
         t = x(k)
         do 100 j = jal(k), jal(k+1)-1
            x(jal(j)) = x(jal(j)) - t*al(j)
 100     continue
 150  continue
!
      return
!----------end-of-lsolc------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine ldsoll (n,x,y,al,jal,nlev,lev,ilev)
      integer, intent(in) :: n, nlev
      integer, intent(inout) :: jal(*), ilev(nlev+1), lev(n)
      real*8, intent(inout) :: x(n), y(n), al(*)
!-----------------------------------------------------------------------
!    Solves L x = y    L = triangular. Uses LEVEL SCHEDULING/MSR format
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real array containg the right hand side.
!
! al,
! jal,   = Lower triangular matrix stored in Modified Sparse Row
!          format.
! nlev   = number of levels in matrix
! lev    = integer array of length n, containing the permutation
!          that defines the levels in the level scheduling ordering.
! ilev   = pointer to beginning of levels in lev.
!          the numbers lev(i) to lev(i+1)-1 contain the row numbers
!          that belong to level number i, in the level shcheduling
!          ordering.
!
! On return:
!-----------
c  x = The solution of  L x = y .
!--------------------------------------------------------------------
      integer :: ii, jrow, i, k
      real*8 :: t
!
!     outer loop goes through the levels. (SEQUENTIAL loop)
!
      do 150 ii=1, nlev
!
!     next loop executes within the same level. PARALLEL loop
!
         do 100 i=ilev(ii), ilev(ii+1)-1
            jrow = lev(i)
!
! compute inner product of row jrow with x
!
            t = y(jrow)
            do 130 k=jal(jrow), jal(jrow+1)-1
               t = t - al(k)*x(jal(k))
 130        continue
            x(jrow) = t*al(jrow)
 100     continue
 150  continue
      return
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine usol (n,x,y,au,jau,iau)
      integer n, jau(*),iau(n+1)
      real*8  x(n), y(n), au(*)
!-----------------------------------------------------------------------
!             Solves   U x = y    U = unit upper triangular.
!-----------------------------------------------------------------------
! solves a unit upper triangular system by standard (sequential )
! backward elimination - matrix stored in CSR format.
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real array containg the right side.
!
! au,
! jau,
! iau,    = Lower triangular matrix stored in compressed sparse row
!          format.
!
! On return:
!-----------
c  x = The solution of  U x = y .
!--------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8  t
!-----------------------------------------------------------------------
      x(n) = y(n)
      do 150 k = n-1,1,-1
         t = y(k)
         do 100 j = iau(k), iau(k+1)-1
            t = t - au(j)*x(jau(j))
 100     continue
         x(k) = t
 150  continue
!
      return
!----------end-of-usol--------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine udsol (n,x,y,au,jau)
      integer n, jau(*)
      real*8  x(n), y(n),au(*)
!-----------------------------------------------------------------------
!             Solves   U x = y  ;   U = upper triangular in MSR format
!-----------------------------------------------------------------------
! solves a non-unit upper triangular matrix by standard (sequential )
! backward elimination - matrix stored in MSR format.
! with diagonal elements already inverted (otherwise do inversion,
! au(1:n) = 1.0/au(1:n),  before calling).
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real array containg the right side.
!
! au,
! jau,    = Lower triangular matrix stored in modified sparse row
!          format.
!
! On return:
!-----------
c  x = The solution of  U x = y .
!--------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8 t
!-----------------------------------------------------------------------
      x(n) = y(n)*au(n)
      do 150 k = n-1,1,-1
         t = y(k)
         do 100 j = jau(k), jau(k+1)-1
            t = t - au(j)*x(jau(j))
 100     continue
         x(k) = au(k)*t
 150  continue
!
      return
!----------end-of-udsol-------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine usolc (n,x,y,au,jau,iau)
      real*8  x(*), y(*), au(*)
      integer n, jau(*),iau(*)
!-----------------------------------------------------------------------
!       SOUVES     U x = y ;    where U = unit upper trang. CSC format
!-----------------------------------------------------------------------
! solves a unit upper triangular system by standard (sequential )
! forward elimination - matrix stored in CSC format.
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real*8 array containg the right side.
!
! au,
! jau,
! iau,    = Uower triangular matrix stored in compressed sparse column
!          format.
!
! On return:
!-----------
c  x  = The solution of  U x  = y.
!-----------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8 t
!-----------------------------------------------------------------------
      do 140 k=1,n
         x(k) = y(k)
 140  continue
      do 150 k = n,1,-1
         t = x(k)
         do 100 j = iau(k), iau(k+1)-1
            x(jau(j)) = x(jau(j)) - t*au(j)
 100     continue
 150  continue
!
      return
!----------end-of-usolc-------------------------------------------------
!-----------------------------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine udsolc (n,x,y,au,jau)
      integer n, jau(*)
      real*8 x(n), y(n), au(*)
!-----------------------------------------------------------------------
!    Solves     U x = y ;    U = nonunit Up. Triang. MSC format
!-----------------------------------------------------------------------
! solves a (non-unit) upper triangular system by standard (sequential)
! forward elimination - matrix stored in Modified Sparse Column format
! with diagonal elements already inverted (otherwise do inversion,
! auuuul(1:n) = 1.0/au(1:n),  before calling ldsol).
!-----------------------------------------------------------------------
!
! On entry:
!----------
! n      = integer. dimension of problem.
! y      = real*8 array containg the right hand side.
!
! au,
! jau,   = Upper triangular matrix stored in Modified Sparse Column
!          format.
!
! On return:
!-----------
c  x = The solution of  U x = y .
!--------------------------------------------------------------------
! local variables
!
      integer k, j
      real*8 t
!-----------------------------------------------------------------------
      do 140 k=1,n
         x(k) = y(k)
 140  continue
      do 150 k = n,1,-1
         x(k) = x(k)*au(k)
         t = x(k)
         do 100 j = jau(k), jau(k+1)-1
            x(jau(j)) = x(jau(j)) - t*au(j)
 100     continue
 150  continue
!
      return
!----------end-of-udsolc------------------------------------------------
!-----------------------------------------------------------------------
      end

!-----------------------------------------------------------------------
        subroutine errpr (n, y, y1,iout,msg)
        integer, intent(in) :: n, iout
        real*8, intent(in) :: y(*), y1(*)
        character*6, intent(in) :: msg

        real*8 :: t
        integer :: k

        t = 0.0d0
        do 1 k=1,n
           t = t+(y(k)-y1(k))**2
 1      continue
        t = sqrt(t)
        write (iout,*) ' 2-norm of difference in ',msg,' =', t
        return
        end
      subroutine  dcopy(n,dx,incx,dy,incy)
!
!     copies a vector, x, to a vector, y.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1)
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
   50 continue
      return
      end


      double precision function ddotORG(n,dx,incx,dy,incy) result(ddot)
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
!
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
         dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
!
      double precision function dasum(n,dx,incx)
!
!     takes the sum of the absolute values.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dtemp
      integer i,incx,m,mp1,n,nincx
!
      dasum = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dtemp = dtemp + dabs(dx(i))
   10 continue
      dasum = dtemp
      return
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,6)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dabs(dx(i))
   30 continue
      if( n .lt. 6 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        dtemp = dtemp + dabs(dx(i)) + dabs(dx(i + 1)) + dabs(dx(i + 2))
     *  + dabs(dx(i + 3)) + dabs(dx(i + 4)) + dabs(dx(i + 5))
   50 continue
   60 dasum = dtemp
      return
      end

      subroutine daxpyXXX(n,da,dx,incx,dy,incy)
!
!     constant times a vector plus a vector.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1),da
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
      double precision function dnrm2XXX ( n, dx, incx)
      integer, intent(in) :: n, incx
      double precision, intent(in) :: dx(*)
      double precision  :: cutlo, cuthi, hitest, sum, xmax,zero,one
      integer           :: next, nn, i, j
      data   zero, one /0.0d0, 1.0d0/
!
!     euclidean norm of the n-vector stored in dx() with storage
!     increment incx .
!     if    n .le. 0 return with result = 0.
!     if n .ge. 1 then incx must be .ge. 1
!
!           c.l.lawson, 1978 jan 08
!
!     four phase method     using two built-in constants that are
!     hopefully applicable to all machines.
!         cutlo = maximum of  dsqrt(u/eps)  over all known machines.
!         cuthi = minimum of  dsqrt(v)      over all known machines.
!     where
!         eps = smallest no. such that eps + 1. .gt. 1.
!         u   = smallest positive no.   (underflow limit)
!         v   = largest  no.            (overflow  limit)
!
!     brief outline of algorithm..
!
!     phase 1    scans zero components.
!     move to phase 2 when a component is nonzero and .le. cutlo
!     move to phase 3 when a component is .gt. cutlo
!     move to phase 4 when a component is .ge. cuthi/m
!     where m = n for x() real and m = 2*n for complex.
!
!     values for cutlo and cuthi..
!     from the environmental parameters listed in the imsl converter
!     document the limiting values are as follows..
!     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
!                   univac and dec at 2**(-103)
!                   thus cutlo = 2**(-51) = 4.44089e-16
!     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
!                   thus cuthi = 2**(63.5) = 1.30438e19
!     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
!                   thus cutlo = 2**(-33.5) = 8.23181d-11
!     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
!     data cutlo, cuthi / 8.232d-11,  1.304d19 /
!     data cutlo, cuthi / 4.441e-16,  1.304e19 /
      data cutlo, cuthi / 8.232d-11,  1.304d19 /
!
      if(n .gt. 0) go to 10
         dnrm2XXX  = zero
         go to 300
!
   10 assign 30 to next
      sum = zero
      nn = n * incx
!                                                 begin main loop
      i = 1
   20    go to next,(30, 50, 70, 110)
   30 if( dabs(dx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
!
!                        phase 1.  sum is zero
!
   50 if( dx(i) .eq. zero) go to 200
      if( dabs(dx(i)) .gt. cutlo) go to 85
!
!                                prepare for phase 2.
      assign 70 to next
      go to 105
!
!                                prepare for phase 4.
!
  100 i = j
      assign 110 to next
      sum = (sum / dx(i)) / dx(i)
  105 xmax = dabs(dx(i))
      go to 115
!
!                   phase 2.  sum is small.
!                             scale to avoid destructive underflow.
!
   70 if( dabs(dx(i)) .gt. cutlo ) go to 75
!
!                     common code for phases 2 and 4.
!                     in phase 4 sum is large.  scale to avoid overflow.
!
  110 if( dabs(dx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / dx(i))**2
         xmax = dabs(dx(i))
         go to 200
!
  115 sum = sum + (dx(i)/xmax)**2
      go to 200
!
!
!                  prepare for phase 3.
!
   75 sum = (sum * xmax) * xmax
!
!
!     for real or d.p. set hitest = cuthi/n
!     for complex      set hitest = cuthi/(2*n)
!
   85 hitest = cuthi/float( n )
!
!                   phase 3.  sum is mid-range.  no scaling.
!
      do 95 j =i,nn,incx
      if(dabs(dx(j)) .ge. hitest) go to 100
   95    sum = sum + dx(j)**2
      dnrm2XXX = dsqrt( sum )
      go to 300
!
  200 continue
      i = i + incx
      if ( i .le. nn ) go to 20
!
!              end of main loop.
!
!              compute square root and adjust for scaling.
!
      dnrm2XXX = xmax * dsqrt(sum)
  300 continue
      return
      end

      subroutine  dscalXXX(n,da,dx,incx)
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision da,dx(1)
      integer i,incx,m,mp1,n,nincx
!
      if(n.le.0)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end

      subroutine  dswapXXX (n,dx,incx,dy,incy)
!
!     interchanges two vectors.
!     uses unrolled loops for increments equal one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!       code for both increments equal to 1
!
!
!       clean-up loop
!
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end

      integer function idamaxXXX(n,dx,incx)
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dmax
      integer i,incx,ix,n
!
      idamaxXXX = 0
      if( n .lt. 1 ) return
      idamaxXXX = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamaxXXX = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
!
!        code for increment equal to 1
!
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamaxXXX = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
!
      subroutine  drotXXX (n,dx,incx,dy,incy,c,s)
!
!     applies a plane rotation.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1),dtemp,c,s
      integer i,incx,incy,ix,iy,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!       code for both increments equal to 1
!
   20 do 30 i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
   30 continue
      return
      end
!
      subroutine drotgXXX(da,db,c,s)
!
!     construct givens plane rotation.
!     jack dongarra, linpack, 3/11/78.
!
      double precision da,db,c,s,roe,scale,r,z
!
      roe = db
      if( dabs(da) .gt. dabs(db) ) roe = da
      scale = dabs(da) + dabs(db)
      if( scale .ne. 0.0d0 ) go to 10
!        c = 1.0d0
         s = 0.0d0
         r = 0.0d0
         go to 20
   10 r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
      r = dsign(1.0d0,roe)*r
!     c = da/r
      s = db/r
   20 z = 1.0d0
      if( dabs(da) .gt. dabs(db) ) z = s
!     if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c
      da = r
      db = z
      return
      end
!
      subroutine  ccopyXXX(n,cx,incx,cy,incy)
!
!     copies a vector, x, to a vector, y.
!     jack dongarra, linpack, 3/11/78.
!
      complex cx(1),cy(1)
      integer i,incx,incy,ix,iy,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        cy(iy) = cx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!        code for both increments equal to 1
!
   20 do 30 i = 1,n
        cy(i) = cx(i)
   30 continue
      return
      end
      subroutine  cscalXXX(n,ca,cx,incx)
!
!     scales a vector by a constant.
!     jack dongarra, linpack,  3/11/78.
!
      complex ca,cx(1)
      integer i,incx,n,nincx
!
      if(n.le.0)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      nincx = n*incx
      do 10 i = 1,nincx,incx
        cx(i) = ca*cx(i)
   10 continue
      return
!
!        code for increment equal to 1
!
   20 do 30 i = 1,n
        cx(i) = ca*cx(i)
   30 continue
      return
      end
!
      subroutine  csrotXXX (n,cx,incx,cy,incy,c,s)
!
!     applies a plane rotation, where the cos and sin (c and s) are real
!     and the vectors cx and cy are complex.
!     jack dongarra, linpack, 3/11/78.
!
      complex cx(1),cy(1),ctemp
      real c,s
      integer i,incx,incy,ix,iy,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        ctemp = c*cx(ix) + s*cy(iy)
        cy(iy) = c*cy(iy) - s*cx(ix)
        cx(ix) = ctemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!       code for both increments equal to 1
!
   20 do 30 i = 1,n
        ctemp = c*cx(i) + s*cy(i)
        cy(i) = c*cy(i) - s*cx(i)
        cx(i) = ctemp
   30 continue
      return
      end
      subroutine  cswapXXX (n,cx,incx,cy,incy)
!
!     interchanges two vectors.
!     jack dongarra, linpack, 3/11/78.
!
      complex cx(1),cy(1),ctemp
      integer i,incx,incy,ix,iy,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        ctemp = cx(ix)
        cx(ix) = cy(iy)
        cy(iy) = ctemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!       code for both increments equal to 1
   20 do 30 i = 1,n
        ctemp = cx(i)
        cx(i) = cy(i)
        cy(i) = ctemp
   30 continue
      return
      end
      subroutine  csscalXXX(n,sa,cx,incx)
!
!     scales a complex vector by a real constant.
!     jack dongarra, linpack, 3/11/78.
!
      complex cx(1)
      real sa
      integer i,incx,n,nincx
!
      if(n.le.0)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      nincx = n*incx
      do 10 i = 1,nincx,incx
        cx(i) = cmplx(sa*real(cx(i)),sa*aimag(cx(i)))
   10 continue
      return
!
!        code for increment equal to 1
!
   20 do 30 i = 1,n
        cx(i) = cmplx(sa*real(cx(i)),sa*aimag(cx(i)))
   30 continue
      return
      end

c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                    FORMAT CONVERSION MODULE                          c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c csrdns  : converts a row-stored sparse matrix into the dense format. c
c dnscsr  : converts a dense matrix to a sparse storage format.        c
c coocsr  : converts coordinate to  to csr format                      c
c coicsr  : in-place conversion of coordinate to csr format            c
c csrcoo  : converts compressed sparse row to coordinate.              c
c csrssr  : converts compressed sparse row to symmetric sparse row     c
c ssrcsr  : converts symmetric sparse row to compressed sparse row     c
c csrell  : converts compressed sparse row to ellpack format           c
c ellcsr  : converts ellpack format to compressed sparse row format    c
c csrmsr  : converts compressed sparse row format to modified sparse   c
c           row format                                                 c
c msrcsr  : converts modified sparse row format to compressed sparse   c
c           row format.                                                c
c csrcsc  : converts compressed sparse row format to compressed sparse c
c           column format (transposition)                              c
c csrcsc2 : rectangular version of csrcsc                              c
c csrlnk  : converts compressed sparse row to linked list format       c
c lnkcsr  : converts linked list format to compressed sparse row fmt   c
c csrdia  : converts a compressed sparse row format into a diagonal    c
c           format.                                                    c
c diacsr  : converts a diagonal format into a compressed sparse row    c
c           format.                                                    c
c bsrcsr  : converts a block-row sparse format into a compressed       c
c           sparse row format.                                         c
c csrbsr  : converts a compressed sparse row format into a block-row   c
c           sparse format.                                             c
c csrbnd  : converts a compressed sparse row format into a banded      c
c           format (linpack style).                                    c
c bndcsr  : converts a banded format (linpack style) into a compressed c
c           sparse row storage.                                        c
c csrssk  : converts the compressed sparse row format to the symmetric c
c           skyline format                                             c
c sskssr  : converts symmetric skyline format to symmetric  sparse row c
c           format.                                                    c
c csrjad  : converts the csr format into the jagged diagonal format    c
c jadcsr  : converts the jagged-diagonal format into the csr format    c
c csruss  : Compressed Sparse Row to Unsymmetric Sparse Skyline        c
c           format                                                     c
c usscsr  : Unsymmetric Sparse Skyline format to Compressed Sparse Row c
c csrsss  : Compressed Sparse Row to Symmetric Sparse Skyline format   c
c ssscsr  : Symmetric Sparse Skyline format to Compressed Sparse Row   c
c csrvbr  : Converts compressed sparse row to var block row format     c
c vbrcsr  : Converts var block row to compressed sparse row format     c
c csorted : Checks if matrix in CSR format is sorted by columns        c
c--------- miscalleneous additions not involving the csr format--------c
c cooell  : converts coordinate to Ellpack/Itpack format               c
c dcsort  : sorting routine used by crsjad                             c
c----------------------------------------------------------------------c
      subroutine csrdns(nrow,ncol,a,ja,ia,dns,ndns,ierr)
      integer, intent(in) :: nrow, ncol, ndns
      integer, intent(out) :: ierr
      real*8, intent(inout) :: dns(ndns,*), a(*)
      integer, intent(inout) :: ja(*),ia(*)
      integer :: i, j, k
c-----------------------------------------------------------------------
c Compressed Sparse Row    to    Dense
c-----------------------------------------------------------------------
c
c converts a row-stored sparse matrix into a densely stored one
c
c On entry:
c----------
c
c nrow   = row-dimension of a
c ncol   = column dimension of a
c a,
c ja,
c ia    = input matrix in compressed sparse row format.
c         (a=value array, ja=column array, ia=pointer array)
c dns   = array where to store dense matrix
c ndns   = first dimension of array dns
c
c on return:
c-----------
c dns   = the sparse matrix a, ja, ia has been stored in dns(ndns,*)
c
c ierr  = integer error indicator.
c         ierr .eq. 0  means normal return
c         ierr .eq. i  means that the code has stopped when processing
c         row number i, because it found a column number .gt. ncol.
c
c-----------------------------------------------------------------------
      ierr = 0
      do 1 i=1, nrow
         do 2 j=1,ncol
       dns(i,j) = 0.0d0
 2       continue
 1    continue
c
      do 4 i=1,nrow
         do 3 k=ia(i),ia(i+1)-1
            j = ja(k)
       if (j .gt. ncol) then
               ierr = i
               return
       endif
       dns(i,j) = a(k)
 3       continue 
 4    continue
      return
c---- end of csrdns ----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine dnscsr(nrow,ncol,nzmax,dns,ndns,a,ja,ia,ierr)
      integer, intent(in) :: nrow, ncol, ndns, nzmax
      integer, intent(out) :: ierr
      real*8, intent(inout) :: dns(ndns,*),a(*)
      integer, intent(inout) :: ia(*),ja(*)
      integer ::next, i, j
c-----------------------------------------------------------------------
c Dense     to    Compressed Row Sparse
c-----------------------------------------------------------------------
c
c converts a densely stored matrix into a row orientied
c compactly sparse matrix. ( reverse of csrdns )
c Note: this routine does not check whether an element
c is small. It considers that a(i,j) is zero if it is exactly
c equal to zero: see test below.
c-----------------------------------------------------------------------
c on entry:
c---------
c
c nrow   = row-dimension of a
c ncol   = column dimension of a
c nzmax = maximum number of nonzero elements allowed. This
c         should be set to be the lengths of the arrays a and ja.
c dns   = input nrow x ncol (dense) matrix.
c ndns   = first dimension of dns.
c
c on return:
c----------
c
c a, ja, ia = value, column, pointer  arrays for output matrix
c
c ierr   = integer error indicator:
c         ierr .eq. 0 means normal retur
c         ierr .eq. i means that the the code stopped while
c         processing row number i, because there was no space left in
c         a, and ja (as defined by parameter nzmax).
c-----------------------------------------------------------------------
      ierr = 0
      next = 1
      ia(1) = 1
      do 4 i=1,nrow
         do 3 j=1, ncol
            if (dns(i,j) .eq. 0.0d0) goto 3
            if (next .gt. nzmax) then
               ierr = i
               return
            endif
            ja(next) = j
            a(next) = dns(i,j)
            next = next+1
 3       continue 
         ia(i+1) = next
 4    continue
      return
c---- end of dnscsr ----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine coocsr(nrow,nnz,a,ir,jc,ao,jao,iao)
c-----------------------------------------------------------------------
      integer, intent(in) :: nrow, nnz
      real*8, intent(inout) :: a(*),ao(*)
      integer, intent(inout) :: ir(*),jc(*),jao(*),iao(*)
      integer :: k, j, k0, iad, i
      real*8 :: x
c-----------------------------------------------------------------------
c  Coordinate     to   Compressed Sparse Row
c-----------------------------------------------------------------------
c converts a matrix that is stored in coordinate format
c  a, ir, jc into a row general sparse ao, jao, iao format.
c
c on entry:
c---------
c nrow   = dimension of the matrix
c nnz = number of nonzero elements in matrix
c a,
c ir,
c jc    = matrix in coordinate format. a(k), ir(k), jc(k) store the nnz
c         nonzero elements of the matrix with a(k) = actual real value of
c    the elements, ir(k) = its row number and jc(k) = its column
c    number. The order of the elements is arbitrary.
c
c on return:
c-----------
c ir  is destroyed
c
c ao, jao, iao = matrix in general sparse matrix format with ao
c  continung the real values, jao containing the column indices,
c  and iao being the pointer to the beginning of the row,
c  in arrays ao, jao.
c
c Notes:
c------ This routine is NOT in place.  See coicsr
c
c------------------------------------------------------------------------
      do 1 k=1,nrow+1
         iao(k) = 0
 1    continue
c determine row-lengths.
      do 2 k=1, nnz
         iao(ir(k)) = iao(ir(k))+1
 2    continue
c starting position of each row..
      k = 1
      do 3 j=1,nrow+1
         k0 = iao(j)
         iao(j) = k
         k = k+k0
 3    continue
c go through the structure  once more. Fill in output matrix.
      do 4 k=1, nnz
         i = ir(k)
         j = jc(k)
         x = a(k)
         iad = iao(i)
         ao(iad) =  x
         jao(iad) = j
         iao(i) = iad+1
 4    continue
c shift back iao
      do 5 j=nrow,1,-1
         iao(j+1) = iao(j)
 5    continue
      iao(1) = 1
      return
c------------- end of coocsr -------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine coicsr (n,nnz,job,a,ja,ia,iwk)
      integer, intent(in) :: n, nnz, job
      integer, intent(inout) :: ia(nnz),ja(nnz),iwk(n+1)
      real*8, intent(inout) :: a(*)
c------------------------------------------------------------------------
c IN-PLACE coo-csr conversion routine.
c------------------------------------------------------------------------
c this subroutine converts a matrix stored in coordinate format into
c the csr format. The conversion is done in place in that the arrays
c a,ja,ia of the result are overwritten onto the original arrays.
c------------------------------------------------------------------------
c on entry:
c---------
c n   = integer. row dimension of A.
c nnz = integer. number of nonzero elements in A.
c job   = integer. Job indicator. when job=1, the real values in a are
c         filled. Otherwise a is not touched and the structure of the
c         array only (i.e. ja, ia)  is obtained.
c a   = real array of size nnz (number of nonzero elements in A)
c         containing the nonzero elements
c ja  = integer array of length nnz containing the column positions
c    of the corresponding elements in a.
c ia  = integer array of length nnz containing the row positions
c    of the corresponding elements in a.
c iwk = integer work array of length n+1
c on return:
c----------
c a
c ja
c ia  = contains the compressed sparse row data structure for the
c         resulting matrix.
c Note:
c-------
c         the entries of the output matrix are not sorted (the column
c         indices in each are not in increasing order) use coocsr
c         if you want them sorted.
c----------------------------------------------------------------------c
c  Coded by Y. Saad, Sep. 26 1989                                      c
c----------------------------------------------------------------------c
      real*8 :: t,tnext
      logical :: values
      integer :: i, j, k, init, ipos, inext, jnext
c-----------------------------------------------------------------------
      values = (job .eq. 1)
c find pointer array for resulting matrix.
      do 35 i=1,n+1
         iwk(i) = 0
 35   continue
      do 4 k=1,nnz
         i = ia(k)
         iwk(i+1) = iwk(i+1)+1
 4    continue
c------------------------------------------------------------------------
      iwk(1) = 1
      do 44 i=2,n
         iwk(i) = iwk(i-1) + iwk(i)
 44   continue
c
c     loop for a cycle in chasing process.
c
      init = 1
      k = 0
 5    if (values) t = a(init)
      i = ia(init)
      j = ja(init)
      ia(init) = -1
c------------------------------------------------------------------------
 6    k = k+1     
c     current row number is i.  determine  where to go.
      ipos = iwk(i)
c     save the chased element.
      if (values) tnext = a(ipos)
      inext = ia(ipos)
      jnext = ja(ipos)
c     then occupy its location.
      if (values) a(ipos)  = t
      ja(ipos) = j
c     update pointer information for next element to come in row i.
      iwk(i) = ipos+1
c     determine  next element to be chased,
      if (ia(ipos) .lt. 0) goto 65
      t = tnext
      i = inext
      j = jnext
      ia(ipos) = -1
      if (k .lt. nnz) goto 6
      goto 70
 65   init = init+1
      if (init .gt. nnz) goto 70
      if (ia(init) .lt. 0) goto 65
c     restart chasing --   
      goto 5
 70   do 80 i=1,n
         ia(i+1) = iwk(i)
 80   continue
      ia(1) = 1
      return
c----------------- end of coicsr ----------------------------------------
c------------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrcoo (nrow,job,nzmax,a,ja,ia,nnz,ao,ir,jc,ierr)
c-----------------------------------------------------------------------
      integer, intent(in) :: nrow, job, nzmax
      integer, intent(out) :: ierr
      real*8, intent(inout) :: a(*),ao(*)
      integer, intent(inout) :: ir(*),jc(*),ja(*),ia(nrow+1)
      integer :: nnz, k, i, k1, k2
c-----------------------------------------------------------------------
c  Compressed Sparse Row      to      Coordinate
c-----------------------------------------------------------------------
c converts a matrix that is stored in coordinate format
c  a, ir, jc into a row general sparse ao, jao, iao format.
c
c on entry:
c---------
c nrow   = dimension of the matrix.
c job   = integer serving as a job indicator.
c         if job = 1 fill in only the array ir, ignore jc, and ao.
c         if job = 2 fill in ir, and jc but not ao
c         if job = 3 fill in everything.
c         The reason why these options are provided is that on return
c         ao and jc are the same as a, ja. So when job = 3, a and ja are
c         simply copied into ao, jc.  When job=2, only jc and ir are
c         returned. With job=1 only the array ir is returned. Moreover,
c         the algorithm is in place:
c       call csrcoo (nrow,1,nzmax,a,ja,ia,nnz,a,ia,ja,ierr)
c         will write the output matrix in coordinate format on a, ja,ia.
c
c a,
c ja,
c ia    = matrix in compressed sparse row format.
c nzmax = length of space available in ao, ir, jc.
c         the code will stop immediatly if the number of
c         nonzero elements found in input matrix exceeds nzmax.
c
c on return:
c-----------
c ao, ir, jc = matrix in coordinate format.
c
c nnz        = number of nonzero elements in matrix.
c ierr       = integer error indicator.
c         ierr .eq. 0 means normal retur
c         ierr .eq. 1 means that the the code stopped
c         because there was no space in ao, ir, jc
c         (according to the value of  nzmax).
c
c NOTES: 1)This routine is PARTIALLY in place: csrcoo can be called with
c         ao being the same array as as a, and jc the same array as ja.
c         but ir CANNOT be the same as ia.
c         2) note the order in the output arrays,
c------------------------------------------------------------------------
      ierr = 0
      nnz = ia(nrow+1)-1
      if (nnz .gt. nzmax) then
         ierr = 1
         return
      endif
c------------------------------------------------------------------------
      goto (3,2,1) job
 1    do 10 k=1,nnz
         ao(k) = a(k)
 10   continue
 2    do 11 k=1,nnz
         jc(k) = ja(k)
 11   continue
c
c     copy backward to allow for in-place processing.
c
 3    do 13 i=nrow,1,-1
         k1 = ia(i+1)-1
         k2 = ia(i)
         do 12 k=k1,k2,-1
            ir(k) = i
 12      continue
 13   continue
      return
c------------- end-of-csrcoo -------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrssr (nrow,a,ja,ia,nzmax,ao,jao,iao,ierr)
      integer, intent(in) :: nrow, nzmax
      integer, intent(out) :: ierr
      real*8, intent(inout) :: a(*), ao(*)
      integer, intent(inout) :: ia(*), ja(*), iao(*), jao(*)
      real*8 :: t
      integer :: ko, i, kold, kdiag, k
c-----------------------------------------------------------------------
c Compressed Sparse Row     to     Symmetric Sparse Row
c-----------------------------------------------------------------------
c this subroutine extracts the lower triangular part of a matrix.
c It can used as a means for converting a symmetric matrix for
c which all the entries are stored in sparse format into one
c in which only the lower part is stored. The routine is in place in
c that the output matrix ao, jao, iao can be overwritten on
c the  input matrix  a, ja, ia if desired. Csrssr has been coded to
c put the diagonal elements of the matrix in the last position in
c each row (i.e. in position  ao(ia(i+1)-1   of ao and jao)
c-----------------------------------------------------------------------
c On entry
c-----------
c nrow  = dimension of the matrix a.
c a, ja,
c    ia = matrix stored in compressed row sparse format
c
c nzmax = length of arrays ao,  and jao.
c
c On return:
c-----------
c ao, jao,
c     iao = lower part of input matrix (a,ja,ia) stored in compressed sparse
c          row format format.
c
c ierr   = integer error indicator.
c          ierr .eq. 0  means normal return
c          ierr .eq. i  means that the code has stopped when processing
c          row number i, because there is not enough space in ao, jao
c          (according to the value of nzmax)
c
c-----------------------------------------------------------------------
      ierr = 0
      ko = 0
c-----------------------------------------------------------------------
      do  7 i=1, nrow
         kold = ko
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .gt. i) goto 71
            ko = ko+1
            if (ko .gt. nzmax) then
               ierr = i
               return
            endif
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
         if (kdiag .eq. 0 .or. kdiag .eq. ko) goto 72
c
c     exchange
c
         t = ao(kdiag)
         ao(kdiag) = ao(ko)
         ao(ko) = t
c
         k = jao(kdiag)
         jao(kdiag) = jao(ko)
         jao(ko) = k
 72      iao(i) = kold+1
 7    continue
c     redefine iao(n+1)
      iao(nrow+1) = ko+1
      return
c--------- end of csrssr -----------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine ssrcsr(job, value2, nrow, a, ja, ia, nzmax,
     &                  ao, jao, iao, indu, iwk, ierr)
c     .. Scalar Arguments ..
      integer            ierr, job, nrow, nzmax, value2
c     ..
c     .. Array Arguments ..
      integer            ia(nrow+1), iao(nrow+1), indu(nrow),
     &                   iwk(nrow+1), ja(*), jao(nzmax)
      real*8             a(*), ao(nzmax)
c     ..
c-----------------------------------------------------------------------
c     Symmetric Sparse Row to Compressed Sparse Row format
c-----------------------------------------------------------------------
c     This subroutine converts a given matrix in SSR format to regular
c     CSR format by computing Ao = A + A' - diag(A), where A' is A
c     transpose.
c
c     Typically this routine is used to expand the SSR matrix of
c     Harwell Boeing matrices, or to obtain a symmetrized graph of
c     unsymmetric matrices.
c
c     This routine is inplace, i.e., (Ao,jao,iao) may be same as
c     (a,ja,ia).
c
c     It is possible to input an arbitrary CSR matrix to this routine,
c     since there is no syntactical difference between CSR and SSR
c     format. It also removes duplicate entries and perform a partial
c     ordering. The output matrix has an order of lower half, main
c     diagonal and upper half after the partial ordering.
c-----------------------------------------------------------------------
c on entry:
c---------
c
c job   = options
c         0 -- duplicate entries are not removed. If the input matrix is
c              SSR (not an arbitary CSR) matrix, no duplicate entry should
c              arise from this routine.
c         1 -- eliminate duplicate entries, zero entries.
c         2 -- eliminate duplicate entries and perform partial ordering.
c         3 -- eliminate duplicate entries, sort the entries in the
c              increasing order of clumn indices.
c
c value2= will the values of A be copied?
c         0 -- only expand the graph (a, ao are not touched)
c         1 -- expand the matrix with the values.
c
c nrow  = column dimension of inout matrix
c a,
c ia,
c ja    = matrix in compressed sparse row format.
c
c nzmax = size of arrays ao and jao. SSRCSR will abort if the storage
c          provided in ao, jao is not sufficient to store A. See ierr.
c
c on return:
c----------
c ao, jao, iao
c       = output matrix in compressed sparse row format. The resulting
c         matrix is symmetric and is equal to A+A'-D. ao, jao, iao,
c         can be the same as a, ja, ia in the calling sequence.
c
c indu  = integer array of length nrow. INDU will contain pointers
c         to the beginning of upper traigular part if job > 1.
c         Otherwise it is also used as a work array (size nrow).
c
c iwk   = integer work space (size nrow+1).
c
c ierr  = integer. Serving as error message. If the length of the arrays
c         ao, jao exceeds nzmax, ierr returns the minimum value
c         needed for nzmax. otherwise ierr=0 (normal return).
c
c-----------------------------------------------------------------------
c     .. Local Scalars ..
      integer            i, ipos, j, k, kfirst, klast, ko, kosav, nnz
      real*8             tmp
c     ..
c     .. Executable Statements ..
      ierr = 0
      do 10 i = 1, nrow
         indu(i) = 0
         iwk(i) = 0
 10   continue
      iwk(nrow+1) = 0
c
c     .. compute number of elements in each row of (A'-D)
c     put result in iwk(i+1)  for row i.
c
      do 30 i = 1, nrow
         do 20 k = ia(i), ia(i+1) - 1
            j = ja(k)
            if (j.ne.i)
     &         iwk(j+1) = iwk(j+1) + 1
 20      continue
 30   continue
c
c     .. find addresses of first elements of ouput matrix. result in iwk
c
      iwk(1) = 1
      do 40 i = 1, nrow
         indu(i) = iwk(i) + ia(i+1) - ia(i)
         iwk(i+1) = iwk(i+1) + indu(i)
         indu(i) = indu(i) - 1
 40   continue
c.....Have we been given enough storage in ao, jao ?
      nnz = iwk(nrow+1) - 1
      if (nnz.gt.nzmax) then
         ierr = nnz
         return
      endif
c
c     .. copy the existing matrix (backwards).
c
      kosav = iwk(nrow+1)
      do 60 i = nrow, 1, -1
         klast = ia(i+1) - 1
         kfirst = ia(i)
         iao(i+1) = kosav
         kosav = iwk(i)
         ko = iwk(i) - kfirst
         iwk(i) = ko + klast + 1
         do 50 k = klast, kfirst, -1
            if (value2.ne.0)
     &         ao(k+ko) = a(k)
            jao(k+ko) = ja(k)
 50      continue
 60   continue
      iao(1) = 1
c
c     now copy (A'-D). Go through the structure of ao, jao, iao
c     that has already been copied. iwk(i) is the address
c     of the next free location in row i for ao, jao.
c
      do 80 i = 1, nrow
         do 70 k = iao(i), indu(i)
            j = jao(k)
            if (j.ne.i) then
               ipos = iwk(j)
               if (value2.ne.0)
     &            ao(ipos) = ao(k)
               jao(ipos) = i
               iwk(j) = ipos + 1
            endif
 70      continue
 80   continue
      if (job.le.0) return
c
c     .. eliminate duplicate entries --
c     array INDU is used as marker for existing indices, it is also the
c     location of the entry.
c     IWK is used to stored the old IAO array.
c     matrix is copied to squeeze out the space taken by the duplicated
c     entries.
c
      do 90 i = 1, nrow
         indu(i) = 0
         iwk(i) = iao(i)
 90   continue
      iwk(nrow+1) = iao(nrow+1)
      k = 1
      do 120 i = 1, nrow
         iao(i) = k
         ipos = iwk(i)
         klast = iwk(i+1)
 100     if (ipos.lt.klast) then
            j = jao(ipos)
            if (indu(j).eq.0) then
c     .. new entry ..
               if (value2.ne.0) then
                  if (ao(ipos) .ne. 0.0D0) then
                     indu(j) = k
                     jao(k) = jao(ipos)
                     ao(k) = ao(ipos)
                     k = k + 1
                  endif
               else
                  indu(j) = k
                  jao(k) = jao(ipos)
                  k = k + 1
               endif
            else if (value2.ne.0) then
c     .. duplicate entry ..
               ao(indu(j)) = ao(indu(j)) + ao(ipos)
            endif
            ipos = ipos + 1
            go to 100
         endif
c     .. remove marks before working on the next row ..
         do 110 ipos = iao(i), k - 1
            indu(jao(ipos)) = 0
 110     continue
 120  continue
      iao(nrow+1) = k
      if (job.le.1) return
c
c     .. partial ordering ..
c     split the matrix into strict upper/lower triangular
c     parts, INDU points to the the beginning of the strict upper part.
c
      do 140 i = 1, nrow
         klast = iao(i+1) - 1
         kfirst = iao(i)
 130     if (klast.gt.kfirst) then
            if (jao(klast).lt.i .and. jao(kfirst).ge.i) then
c     .. swap klast with kfirst ..
               j = jao(klast)
               jao(klast) = jao(kfirst)
               jao(kfirst) = j
               if (value2.ne.0) then
                  tmp = ao(klast)
                  ao(klast) = ao(kfirst)
                  ao(kfirst) = tmp
               endif
            endif
            if (jao(klast).ge.i)
     &         klast = klast - 1
            if (jao(kfirst).lt.i)
     &         kfirst = kfirst + 1
            go to 130
         endif
c
         if (jao(klast).lt.i) then
            indu(i) = klast + 1
         else
            indu(i) = klast
         endif
 140  continue
      if (job.le.2) return
c
c     .. order the entries according to column indices
c     bubble-sort is used
c
      do 190 i = 1, nrow
         do 160 ipos = iao(i), indu(i)-1
            do 150 j = indu(i)-1, ipos+1, -1
               k = j - 1
               if (jao(k).gt.jao(j)) then
                  ko = jao(k)
                  jao(k) = jao(j)
                  jao(j) = ko
                  if (value2.ne.0) then
                     tmp = ao(k)
                     ao(k) = ao(j)
                     ao(j) = tmp
                  endif
               endif
 150        continue
 160     continue
         do 180 ipos = indu(i), iao(i+1)-1
            do 170 j = iao(i+1)-1, ipos+1, -1
               k = j - 1
               if (jao(k).gt.jao(j)) then
                  ko = jao(k)
                  jao(k) = jao(j)
                  jao(j) = ko
                  if (value2.ne.0) then
                     tmp = ao(k)
                     ao(k) = ao(j)
                     ao(j) = tmp
                  endif
               endif
 170        continue
 180     continue
 190  continue
c
      return
c---- end of ssrcsr ----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine xssrcsr (nrow,a,ja,ia,nzmax,ao,jao,iao,indu,ierr)
      integer, intent(in) :: nrow, nzmax
      integer, intent(out) :: ierr
      integer, intent(inout) :: ia(nrow+1),iao(nrow+1),ja(*),jao(nzmax)
     * ,indu(nrow+1)
      real*8 , intent(inout) :: a(*),ao(nzmax)
      integer :: i, j, k, lenrow, nnz, ipos, kosav, klast, kfirst, ko
c-----------------------------------------------------------------------
c Symmetric Sparse Row   to    (regular) Compressed Sparse Row
c-----------------------------------------------------------------------
c this subroutine converts  a symmetric  matrix in which only the lower
c part is  stored in compressed sparse row format, i.e.,
c a matrix stored in symmetric sparse format, into a fully stored matrix
c i.e., a matrix where both the lower and upper parts are stored in
c compressed sparse row format. the algorithm is in place (i.e. result
c may be overwritten onto the input matrix a, ja, ia ----- ).
c the output matrix delivered by ssrcsr is such that each row starts with
c the elements of the lower part followed by those of the upper part.
c-----------------------------------------------------------------------
c on entry:
c---------
c  
c nrow  = row dimension of inout matrix
c a,
c ia,
c ja    = matrix in compressed sparse row format. This is assumed to be
c         a lower triangular matrix.
c
c nzmax  = size of arrays ao and jao. ssrcsr will abort if the storage
c     provided in a, ja is not sufficient to store A. See ierr.
c  
c on return:
c----------
c ao, iao,
c   jao = output matrix in compressed sparse row format. The resulting
c         matrix is symmetric and is equal to A+A**T - D, if
c         A is the original lower triangular matrix. ao, jao, iao,
c         can be the same as a, ja, ia in the calling sequence.
c
c indu  = integer array of length nrow+1. If the input matrix is such
c         that the last element in each row is its diagonal element then
c         on return, indu will contain the pointers to the diagonal
c         element in each row of the output matrix. Otherwise used as
c         work array.
c ierr  = integer. Serving as error message. If the length of the arrays
c         ao, jao exceeds nzmax, ierr returns the minimum value
c         needed for nzmax. otherwise ierr=0 (normal return).
c
c-----------------------------------------------------------------------
      ierr = 0
      do 1 i=1,nrow+1
         indu(i) = 0
 1    continue
c
c     compute  number of elements in each row of strict upper part.
c     put result in indu(i+1)  for row i.
c
      do 3 i=1, nrow
         do 2 k=ia(i),ia(i+1)-1
            j = ja(k)
            if (j .lt. i) indu(j+1) = indu(j+1)+1
 2       continue
 3    continue
c-----------
c     find addresses of first elements of ouput matrix. result in indu
c-----------
      indu(1) = 1
      do 4 i=1,nrow
         lenrow = ia(i+1)-ia(i)
         indu(i+1) = indu(i) + indu(i+1) + lenrow
 4    continue
c--------------------- enough storage in a, ja ? --------
      nnz = indu(nrow+1)-1
      if (nnz .gt. nzmax) then
         ierr = nnz
         return
      endif
c
c     now copy lower part (backwards).
c
      kosav = indu(nrow+1)
      do 6 i=nrow,1,-1
         klast = ia(i+1)-1
         kfirst = ia(i)
         iao(i+1) = kosav
         ko = indu(i)
         kosav = ko
         do 5 k = kfirst, klast
            ao(ko) = a(k)
            jao(ko) = ja(k)
       ko = ko+1
 5       continue
         indu(i) = ko
 6    continue
      iao(1) = 1
c
c     now copy upper part. Go through the structure of ao, jao, iao
c     that has already been copied (lower part). indu(i) is the address
c     of the next free location in row i for ao, jao.
c
      do 8 i=1,nrow
c     i-th row is now in ao, jao, iao structure -- lower half part
         do 9 k=iao(i), iao(i+1)-1
            j = jao(k)
            if (j .ge. i)  goto 8
            ipos = indu(j)
            ao(ipos) = ao(k)
            jao(ipos) = i
            indu(j) = indu(j) + 1
 9       continue
 8    continue
      return
c----- end of xssrcsr --------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrell (nrow,a,ja,ia,maxcol,coef,jcoef,ncoef,
     *                   ndiag,ierr)
      integer, intent(in) :: nrow, maxcol, ncoef
      integer, intent(out) :: ierr, ndiag
      integer, intent(inout) :: ia(nrow+1), ja(*), jcoef(ncoef,1)
      real*8 , intent(inout) :: a(*), coef(ncoef,1)
      integer :: i,j,k,k1,k2
c-----------------------------------------------------------------------
c Compressed Sparse Row     to    Ellpack - Itpack format
c-----------------------------------------------------------------------
c this subroutine converts  matrix stored in the general a, ja, ia
c format into the coef, jcoef itpack format.
c
c-----------------------------------------------------------------------
c on entry:
c----------
c nrow     = row dimension of the matrix A.
c
c a,
c ia,
c ja      = input matrix in compressed sparse row format.
c
c ncoef  = first dimension of arrays coef, and jcoef.
c
c maxcol = integer equal to the number of columns available in coef.
c
c on return:
c----------
c coef   = real array containing the values of the matrix A in
c         itpack-ellpack format.
c jcoef = integer array containing the column indices of coef(i,j)
c         in A.
c ndiag = number of active 'diagonals' found.
c
c ierr   = error message. 0 = correct return. If ierr .ne. 0 on
c    return this means that the number of diagonals found
c         (ndiag) exceeds maxcol.
c
c-----------------------------------------------------------------------
c first determine the length of each row of lower-part-of(A)
      ierr = 0
      ndiag = 0
      do 3 i=1, nrow
         k = ia(i+1)-ia(i)
         ndiag = max0(ndiag,k)
 3    continue
c----- check whether sufficient columns are available. -----------------
      if (ndiag .gt. maxcol) then
         ierr = 1
         return
      endif
c
c fill coef with zero elements and jcoef with row numbers.------------
c
      do 4 j=1,ndiag
         do 41 i=1,nrow
            coef(i,j) = 0.0d0
            jcoef(i,j) = i
 41      continue
 4    continue
c
c------- copy elements row by row.--------------------------------------
c
      do 6 i=1, nrow
         k1 = ia(i)
         k2 = ia(i+1)-1
         do 5 k=k1,k2
            coef(i,k-k1+1) = a(k)
            jcoef(i,k-k1+1) = ja(k)
 5       continue
 6    continue
      return
c--- end of csrell------------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine ellcsr(nrow,coef,jcoef,ncoef,ndiag,a,ja,ia,nzmax,ierr)
      integer, intent(in) :: nrow, nzmax, ncoef, ndiag
      integer, intent(out) :: ierr
      integer, intent(inout) :: ia(nrow+1), ja(*), jcoef(ncoef,1)
      real*8 , intent(inout) :: a(*), coef(ncoef,1)
      integer :: kpos, i, k
c-----------------------------------------------------------------------
c  Ellpack - Itpack format  to  Compressed Sparse Row
c-----------------------------------------------------------------------
c this subroutine converts a matrix stored in ellpack-itpack format
c coef-jcoef into the compressed sparse row format. It actually checks
c whether an entry in the input matrix is a nonzero element before
c putting it in the output matrix. The test does not account for small
c values but only for exact zeros.
c-----------------------------------------------------------------------
c on entry:
c----------
c
c nrow   = row dimension of the matrix A.
c coef   = array containing the values of the matrix A in ellpack format.
c jcoef = integer arraycontains the column indices of coef(i,j) in A.
c ncoef = first dimension of arrays coef, and jcoef.
c ndiag = number of active columns in coef, jcoef.
c
c ndiag = on entry the number of columns made available in coef.
c
c on return:
c----------
c a, ia,
c    ja = matrix in a, ia, ja format where.
c
c nzmax  = size of arrays a and ja. ellcsr will abort if the storage
c     provided in a, ja is not sufficient to store A. See ierr.
c
c ierr   = integer. serves are output error message.
c         ierr = 0 means normal return.
c         ierr = 1 means that there is not enough space in
c         a and ja to store output matrix.
c-----------------------------------------------------------------------
c first determine the length of each row of lower-part-of(A)
      ierr = 0
c-----check whether sufficient columns are available. -----------------
c
c------- copy elements row by row.--------------------------------------
      kpos = 1
      ia(1) = kpos
      do 6 i=1, nrow
         do 5 k=1,ndiag
            if (coef(i,k) .ne. 0.0d0) then
               if (kpos .gt. nzmax) then
                  ierr = kpos
                  return
               endif
               a(kpos) = coef(i,k)
               ja(kpos) = jcoef(i,k)
               kpos = kpos+1
       endif
 5       continue
         ia(i+1) = kpos
 6    continue 
      return
c--- end of ellcsr -----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrmsr (n,a,ja,ia,ao,jao,wk,iwk)
      integer, intent(in) :: n
      real*8 , intent(inout) :: a(*),ao(*),wk(n)
      integer, intent(inout) :: ia(n+1),ja(*),jao(*),iwk(n+1)
      integer :: icount, i, k, iptr, ii, j
c-----------------------------------------------------------------------
c Compressed Sparse Row   to      Modified - Sparse Row
c                                 Sparse row with separate main diagonal
c-----------------------------------------------------------------------
c converts a general sparse matrix a, ja, ia into
c a compressed matrix using a separated diagonal (referred to as
c the bell-labs format as it is used by bell labs semi conductor
c group. We refer to it here as the modified sparse row format.
c Note: this has been coded in such a way that one can overwrite
c the output matrix onto the input matrix if desired by a call of
c the form
c
c     call csrmsr (n, a, ja, ia, a, ja, wk,iwk)
c
c In case ao, jao, are different from a, ja, then one can
c use ao, jao as the work arrays in the calling sequence:
c
c     call csrmsr (n, a, ja, ia, ao, jao, ao,jao)
c
c-----------------------------------------------------------------------
c
c on entry :
c---------
c a, ja, ia = matrix in csr format. note that the
c       algorithm is in place: ao, jao can be the same
c            as a, ja, in which case it will be overwritten on it
c            upon return.
c  
c on return :
c-----------
c
c ao, jao  = sparse matrix in modified sparse row storage format:
c     +  ao(1:n) contains the diagonal of the matrix.
c     +  ao(n+2:nnz) contains the nondiagonal elements of the
c             matrix, stored rowwise.
c     +  jao(n+2:nnz) : their column indices
c     +  jao(1:n+1) contains the pointer array for the nondiagonal
c             elements in ao(n+1:nnz) and jao(n+2:nnz).
c             i.e., for i .le. n+1 jao(i) points to beginning of row i
c        in arrays ao, jao.
c         here nnz = number of nonzero elements+1
c work arrays:
c------------
c wk  = real work array of length n
c iwk   = integer work array of length n+1
c
c notes:
c-------
c        Algorithm is in place.  i.e. both:
c
c          call csrmsr (n, a, ja, ia, ao, jao, ao,jao)
c          (in which  ao, jao, are different from a, ja)
c           and
c          call csrmsr (n, a, ja, ia, a, ja, wk,iwk)
c          (in which  wk, jwk, are different from a, ja)
c        are OK.
c--------
c coded by Y. Saad Sep. 1989. Rechecked Feb 27, 1990.
c-----------------------------------------------------------------------
      icount = 0
c
c store away diagonal elements and count nonzero diagonal elements.
c
      do 1 i=1,n
         wk(i) = 0.0d0
         iwk(i+1) = ia(i+1)-ia(i)
         do 2 k=ia(i),ia(i+1)-1
            if (ja(k) .eq. i) then
               wk(i) = a(k)
               icount = icount + 1
               iwk(i+1) = iwk(i+1)-1
            endif
 2       continue
 1    continue
c
c compute total length
c
      iptr = n + ia(n+1) - icount
c
c     copy backwards (to avoid collisions)
c
      do 500 ii=n,1,-1
         do 100 k=ia(ii+1)-1,ia(ii),-1
            j = ja(k)
            if (j .ne. ii) then
               ao(iptr) = a(k)
               jao(iptr) = j
               iptr = iptr-1
            endif
 100     continue
 500  continue
c
c compute pointer values and copy wk(*)
c
      jao(1) = n+2
      do 600 i=1,n
         ao(i) = wk(i)
         jao(i+1) = jao(i)+iwk(i+1)
 600  continue
      return   
c------------ end of subroutine csrmsr ---------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine msrcsr (n,a,ja,ao,jao,iao,wk,iwk)
      integer, intent(in) :: n
      real*8 , intent(inout) :: a(*),ao(*),wk(n)
      integer, intent(inout) :: ja(*),jao(*),iao(n+1),iwk(n+1)
      integer :: i,j,k,ii,idiag,iptr
c-----------------------------------------------------------------------
c       Modified - Sparse Row  to   Compressed Sparse Row
c
c-----------------------------------------------------------------------
c converts a compressed matrix using a separated diagonal
c (modified sparse row format) in the Compressed Sparse Row
c format.
c does not check for zero elements in the diagonal.
c
c
c on entry :
c---------
c n          = row dimension of matrix
c a, ja      = sparse matrix in msr sparse storage format
c              see routine csrmsr for details on data structure
c
c on return :
c-----------
c
c ao,jao,iao = output matrix in csr format.
c
c work arrays:
c------------
c wk       = real work array of length n
c iwk      = integer work array of length n+1
c
c notes:
c   The original version of this was NOT in place, but has
c   been modified by adding the vector iwk to be in place.
c   The original version had ja instead of iwk everywhere in
c   loop 500.  Modified  Sun 29 May 1994 by R. Bramley (Indiana).
c
c-----------------------------------------------------------------------
      logical added
      do 1 i=1,n
         wk(i) = a(i)
         iwk(i) = ja(i)
 1    continue
      iwk(n+1) = ja(n+1)
      iao(1) = 1
      iptr = 1
c---------
      do 500 ii=1,n
         added = .false.
         idiag = iptr + (iwk(ii+1)-iwk(ii))
         do 100 k=iwk(ii),iwk(ii+1)-1
            j = ja(k)
            if (j .lt. ii) then
               ao(iptr) = a(k)
               jao(iptr) = j
               iptr = iptr+1
            elseif (added) then
               ao(iptr) = a(k)
               jao(iptr) = j
               iptr = iptr+1
            else
c add diag element - only reserve a position for it.
               idiag = iptr
               iptr = iptr+1
               added = .true.
c     then other element
               ao(iptr) = a(k)
               jao(iptr) = j
               iptr = iptr+1
            endif
 100     continue
         ao(idiag) = wk(ii)
         jao(idiag) = ii
         if (.not. added) iptr = iptr+1
         iao(ii+1) = iptr
 500  continue
      return
c------------ end of subroutine msrcsr ---------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrcsc (n,job,ipos,a,ja,ia,ao,jao,iao)
      integer, intent(in) :: n, job, ipos
      integer ia(n+1),iao(n+1),ja(*),jao(*)
      real*8  a(*),ao(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to      Compressed Sparse Column
c
c (transposition operation)   Not in place.
c-----------------------------------------------------------------------
c -- not in place --
c this subroutine transposes a matrix stored in a, ja, ia format.
c ---------------
c on entry:
c----------
c n   = dimension of A.
c job = integer to indicate whether to fill the values (job.eq.1) of the
c         matrix ao or only the pattern., i.e.,ia, and ja (job .ne.1)
c
c ipos  = starting position in ao, jao of the transposed matrix.
c         the iao array takes this into account (thus iao(1) is set to ipos.)
c         Note: this may be useful if one needs to append the data structure
c         of the transpose to that of A. In this case use for example
c                call csrcsc (n,1,ia(n+1),a,ja,ia,a,ja,ia(n+2))
c    for any other normal usage, enter ipos=1.
c a   = real array of length nnz (nnz=number of nonzero elements in input
c         matrix) containing the nonzero elements.
c ja  = integer array of length nnz containing the column positions
c    of the corresponding elements in a.
c ia  = integer of size n+1. ia(k) contains the position in a, ja of
c    the beginning of the k-th row.
c
c on return:
c ----------
c output arguments:
c ao  = real array of size nzz containing the "a" part of the transpose
c jao = integer array of size nnz containing the column indices.
c iao = integer array of size n+1 containing the "ia" index array of
c    the transpose.
c
c-----------------------------------------------------------------------
      call csrcsc2 (n,n,job,ipos,a,ja,ia,ao,jao,iao)
      end
c-----------------------------------------------------------------------
      subroutine csrcsc2 (n,n2,job,ipos,a,ja,ia,ao,jao,iao)
      integer, intent(in) :: n, job, n2, ipos
      integer, intent(inout) :: ia(n+1),iao(n2+1),ja(*),jao(*)
      real*8 , intent(inout) :: a(*),ao(*)
      integer :: i,j,k,next
c-----------------------------------------------------------------------
c Compressed Sparse Row     to      Compressed Sparse Column
c
c (transposition operation)   Not in place.
c-----------------------------------------------------------------------
c Rectangular version.  n is number of rows of CSR matrix,
c                       n2 (input) is number of columns of CSC matrix.
c-----------------------------------------------------------------------
c -- not in place --
c this subroutine transposes a matrix stored in a, ja, ia format.
c ---------------
c on entry:
c----------
c n   = number of rows of CSR matrix.
c n2    = number of columns of CSC matrix.
c job = integer to indicate whether to fill the values (job.eq.1) of the
c         matrix ao or only the pattern., i.e.,ia, and ja (job .ne.1)
c
c ipos  = starting position in ao, jao of the transposed matrix.
c         the iao array takes this into account (thus iao(1) is set to ipos.)
c         Note: this may be useful if one needs to append the data structure
c         of the transpose to that of A. In this case use for example
c                call csrcsc2 (n,n,1,ia(n+1),a,ja,ia,a,ja,ia(n+2))
c    for any other normal usage, enter ipos=1.
c a   = real array of length nnz (nnz=number of nonzero elements in input
c         matrix) containing the nonzero elements.
c ja  = integer array of length nnz containing the column positions
c    of the corresponding elements in a.
c ia  = integer of size n+1. ia(k) contains the position in a, ja of
c    the beginning of the k-th row.
c
c on return:
c ----------
c output arguments:
c ao  = real array of size nzz containing the "a" part of the transpose
c jao = integer array of size nnz containing the column indices.
c iao = integer array of size n+1 containing the "ia" index array of
c    the transpose.
c
c-----------------------------------------------------------------------
c----------------- compute lengths of rows of transp(A) ----------------
      do 1 i=1,n2+1
         iao(i) = 0
 1    continue
      do 3 i=1, n
         do 2 k=ia(i), ia(i+1)-1
            j = ja(k)+1
            iao(j) = iao(j)+1
 2       continue
 3    continue
c---------- compute pointers from lengths ------------------------------
      iao(1) = ipos
      do 4 i=1,n2
         iao(i+1) = iao(i) + iao(i+1)
 4    continue
c--------------- now do the actual copying -----------------------------
      do 6 i=1,n
         do 62 k=ia(i),ia(i+1)-1
            j = ja(k)
            next = iao(j)
            if (job .eq. 1)  ao(next) = a(k)
            jao(next) = i
            iao(j) = next+1
 62      continue
 6    continue
c-------------------------- reshift iao and leave ----------------------
      do 7 i=n2,1,-1
         iao(i+1) = iao(i)
 7    continue
      iao(1) = ipos
c--------------- end of csrcsc2 ----------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrlnk (n,a,ja,ia,link)
      real*8 , intent(inout) :: a(*)
      integer, intent(in) :: n
      integer, intent(inout) :: ja(*), ia(n+1), link(*)
c-----------------------------------------------------------------------
c      Compressed Sparse Row         to    Linked storage format.
c-----------------------------------------------------------------------
c this subroutine translates a matrix stored in compressed sparse
c row into one with a linked list storage format. Only the link
c array needs to be obtained since the arrays a, ja, and ia may
c be unchanged and  carry the same meaning for the output matrix.
c in  other words a, ja, ia, link   is the output linked list data
c structure with a, ja, unchanged from input, and ia possibly
c altered (in case therea re null rows in matrix). Details on
c the output array link are given below.
c-----------------------------------------------------------------------
c Coded by Y. Saad, Feb 21, 1991.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n   = integer equal to the dimension of A. 
c
c a   = real array of size nna containing the nonzero elements
c ja  = integer array of size nnz containing the column positions
c    of the corresponding elements in a.
c ia  = integer of size n+1 containing the pointers to the beginning
c         of each row. ia(k) contains the position in a, ja of the
c         beginning of the k-th row.
c
c on return:
c----------
c a, ja, are not changed.
c ia    may be changed if there are null rows.
c
c a     = nonzero elements.
c ja    = column positions.
c ia    = ia(i) points to the first element of row i in linked structure.
c link   = integer array of size containing the linked list information.
c         link(k) points to the next element of the row after element
c         a(k), ja(k). if link(k) = 0, then there is no next element,
c         i.e., a(k), jcol(k) is the last element of the current row.
c
c  Thus row number i can be accessed as follows:
c     next = ia(i)
c     while(next .ne. 0) do
c          value = a(next)      ! value a(i,j)
c          jcol  = ja(next)     ! column index j
c          next  = link(next)   ! address of next element in row
c     endwhile
c notes:
c ------ ia may be altered on return.
c-----------------------------------------------------------------------
c local variables
      integer i, k, istart, iend
c
c loop through all rows
c
      do 100 i =1, n
         istart = ia(i)
         iend = ia(i+1)-1
         if (iend .gt. istart) then
            do 99  k=istart, iend-1
               link(k) = k+1
 99         continue
            link(iend) = 0
         else
            ia(i) = 0
         endif
 100  continue
c
      return
c-------------end-of-csrlnk --------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine lnkcsr (n, a, jcol, istart, link, ao, jao, iao)
      real*8 a(*), ao(*)
      integer n, jcol(*), istart(n), link(*), jao(*), iao(*)
c-----------------------------------------------------------------------
c     Linked list storage format   to      Compressed Sparse Row  format
c-----------------------------------------------------------------------
c this subroutine translates a matrix stored in linked list storage
c format into the compressed sparse row format.
c-----------------------------------------------------------------------
c Coded by Y. Saad, Feb 21, 1991.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n   = integer equal to the dimension of A. 
c
c a   = real array of size nna containing the nonzero elements
c jcol   = integer array of size nnz containing the column positions
c    of the corresponding elements in a.
c istart= integer array of size n poiting to the beginning of the rows.
c         istart(i) contains the position of the first element of
c         row i in data structure. (a, jcol, link).
c         if a row is empty istart(i) must be zero.
c link   = integer array of size nnz containing the links in the linked
c         list data structure. link(k) points to the next element
c         of the row after element ao(k), jcol(k). if link(k) = 0,
c         then there is no next element, i.e., ao(k), jcol(k) is
c         the last element of the current row.
c
c on return:
c-----------
c ao, jao, iao = matrix stored in csr format:
c
c ao    = real array containing the values of the nonzero elements of
c         the matrix stored row-wise.
c jao = integer array of size nnz containing the column indices.
c iao = integer array of size n+1 containing the pointers array to the
c         beginning of each row. iao(i) is the address in ao,jao of
c         first element of row i.
c
c-----------------------------------------------------------------------
c first determine individial bandwidths and pointers.
c-----------------------------------------------------------------------
c local variables
      integer irow, ipos, next
c-----------------------------------------------------------------------
      ipos = 1
      iao(1) = ipos
c
c     loop through all rows
c
      do 100 irow =1, n
c
c     unroll i-th row.
c
         next = istart(irow)
 10      if (next .eq. 0) goto 99
         jao(ipos) = jcol(next)
         ao(ipos)  = a(next)
         ipos = ipos+1
         next = link(next)
         goto 10
 99      iao(irow+1) = ipos
 100  continue
c
      return
c-------------end-of-lnkcsr -------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrdia (n,idiag,job,a,ja,ia,ndiag,
     *                   diag,ioff,ao,jao,iao,ind)
      integer, intent(in) :: n, ndiag
      integer, intent(inout) :: idiag
      real*8 , intent(inout) :: diag(ndiag,idiag), a(*), ao(*)
      integer, intent(inout) :: ia(*), ind(*), ja(*), jao(*), iao(*)
     * , ioff(*)
      integer :: i,j,k,l,jmax,ii,job,job1,job2,ko,n2,idum
c-----------------------------------------------------------------------
c Compressed sparse row     to    diagonal format
c-----------------------------------------------------------------------
c this subroutine extracts  idiag diagonals  from the  input matrix a,
c a, ia, and puts the rest of  the matrix  in the  output matrix ao,
c jao, iao.  The diagonals to be extracted depend  on the  value of job
c (see below for details.)  In  the first  case, the  diagonals to be
c extracted are simply identified by  their offsets  provided in ioff
c by the caller.  In the second case, the  code internally determines
c the idiag most significant diagonals, i.e., those  diagonals of the
c matrix which  have  the  largest  number  of  nonzero elements, and
c extracts them.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = dimension of the matrix a.
c idiag = integer equal to the number of diagonals to be extracted.
c         Note: on return idiag may be modified.
c a, ja,          
c    ia = matrix stored in a, ja, ia, format
c job = integer. serves as a job indicator.  Job is better thought
c         of as a two-digit number job=xy. If the first (x) digit
c         is one on entry then the diagonals to be extracted are
c         internally determined. In this case csrdia exctracts the
c         idiag most important diagonals, i.e. those having the largest
c         number on nonzero elements. If the first digit is zero
c         then csrdia assumes that ioff(*) contains the offsets
c         of the diagonals to be extracted. there is no verification
c         that ioff(*) contains valid entries.
c         The second (y) digit of job determines whether or not
c         the remainder of the matrix is to be written on ao,jao,iao.
c         If it is zero  then ao, jao, iao is not filled, i.e.,
c         the diagonals are found  and put in array diag and the rest is
c         is discarded. if it is one, ao, jao, iao contains matrix
c         of the remaining elements.
c         Thus:
c         job= 0 means do not select diagonals internally (pick those
c                defined by ioff) and do not fill ao,jao,iao
c         job= 1 means do not select diagonals internally
c                      and fill ao,jao,iao
c         job=10 means  select diagonals internally
c                      and do not fill ao,jao,iao
c         job=11 means select diagonals internally
c                      and fill ao,jao,iao
c
c ndiag = integer equal to the first dimension of array diag.
c
c on return:
c-----------
c
c idiag = number of diagonals found. This may be smaller than its value
c         on entry.
c diag  = real array of size (ndiag x idiag) containing the diagonals
c         of A on return
c
c ioff  = integer array of length idiag, containing the offsets of the
c       diagonals to be extracted.
c ao, jao
c  iao  = remainder of the matrix in a, ja, ia format.
c work arrays:
c------------
c ind   = integer array of length 2*n-1 used as integer work space.
c         needed only when job.ge.10 i.e., in case the diagonals are to
c         be selected internally.
c
c Notes:
c-------
c    1) The algorithm is in place: ao, jao, iao can be overwritten on
c       a, ja, ia if desired
c    2) When the code is required to select the diagonals (job .ge. 10)
c       the selection of the diagonals is done from left to right
c       as a result if several diagonals have the same weight (number
c       of nonzero elemnts) the leftmost one is selected first.
c-----------------------------------------------------------------------
      job1 = job/10
      job2 = job-job1*10
      if (job1 .eq. 0) goto 50
      n2 = n+n-1
      call infdia(n,ja,ia,ind,idum)
c----------- determine diagonals to  accept.----------------------------
c-----------------------------------------------------------------------
      ii = 0
 4    ii=ii+1
      jmax = 0
      do 41 k=1, n2
         j = ind(k)
         if (j .le. jmax) goto 41
         i = k
         jmax = j
 41   continue
      if (jmax .le. 0) then
         ii = ii-1
         goto 42
      endif
      ioff(ii) = i-n
      ind(i) = - jmax
      if (ii .lt.  idiag) goto 4
 42   idiag = ii
c---------------- initialize diago to zero -----------------------------
 50   continue
      do 55 j=1,idiag
         do 54 i=1,n
            diag(i,j) = 0.0d0
 54      continue
 55   continue
c-----------------------------------------------------------------------
      ko = 1
c-----------------------------------------------------------------------
c extract diagonals and accumulate remaining matrix.
c-----------------------------------------------------------------------
      do 6 i=1, n
         do 51 k=ia(i),ia(i+1)-1
            j = ja(k)
            do 52 l=1,idiag
               if (j-i .ne. ioff(l)) goto 52
               diag(i,l) = a(k)
               goto 51
 52         continue
c--------------- append element not in any diagonal to ao,jao,iao -----
            if (job2 .eq. 0) goto 51
            ao(ko) = a(k)
            jao(ko) = j
            ko = ko+1
 51      continue
         if (job2 .ne. 0 ) ind(i+1) = ko
 6    continue
      if (job2 .eq. 0) return
c     finish with iao
      iao(1) = 1
      do 7 i=2,n+1
         iao(i) = ind(i)
 7    continue
      return
c----------- end of csrdia ---------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine diacsr (n,job,idiag,diag,ndiag,ioff,a,ja,ia)
      integer, intent(in) :: n, job, ndiag,idiag
      real*8 , intent(inout) :: diag(ndiag,idiag), a(*)
      integer ia(*), ja(*), ioff(*)
      real*8 :: t
      integer :: i,j,jj,ko
c-----------------------------------------------------------------------
c    diagonal format     to     compressed sparse row
c-----------------------------------------------------------------------
c this subroutine extract the idiag most important diagonals from the
c input matrix a, ja, ia, i.e, those diagonals of the matrix which have
c the largest number of nonzero elements. If requested (see job),
c the rest of the matrix is put in a the output matrix ao, jao, iao
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = integer. dimension of the matrix a.
c job = integer. job indicator with the following meaning.
c         if (job .eq. 0) then check for each entry in diag
c         whether this entry is zero. If it is then do not include
c         in the output matrix. Note that the test is a test for
c         an exact arithmetic zero. Be sure that the zeros are
c         actual zeros in double precision otherwise this would not
c         work.
c
c idiag = integer equal to the number of diagonals to be extracted.
c         Note: on return idiag may be modified.
c
c diag  = real array of size (ndiag x idiag) containing the diagonals
c         of A on return.
c
c ndiag = integer equal to the first dimension of array diag.
c
c ioff  = integer array of length idiag, containing the offsets of the
c       diagonals to be extracted.
c
c on return:
c-----------
c a,
c ja,          
c ia    = matrix stored in a, ja, ia, format
c
c Note:
c ----- the arrays a and ja should be of length n*idiag.
c
c-----------------------------------------------------------------------
      ia(1) = 1
      ko = 1
      do 80 i=1, n
         do 70 jj = 1, idiag
            j = i+ioff(jj)
            if (j .lt. 1 .or. j .gt. n) goto 70
            t = diag(i,jj)
            if (job .eq. 0 .and. t .eq. 0.0d0) goto 70
            a(ko) = t
            ja(ko) = j
            ko = ko+1
 70      continue
         ia(i+1) = ko
 80   continue
      return
c----------- end of diacsr ---------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine bsrcsr (job, n, m, na, a, ja, ia, ao, jao, iao)
      implicit none
      integer job, n, m, na, ia(*), ja(*), jao(*), iao(n+1)
      real*8 a(na,*), ao(*)
c-----------------------------------------------------------------------
c             Block Sparse Row  to Compressed Sparse Row.
c-----------------------------------------------------------------------
c NOTE: ** meanings of parameters may have changed wrt earlier versions
c FORMAT DEFINITION HAS CHANGED WRT TO EARLIER VERSIONS...
c-----------------------------------------------------------------------
c
c converts a  matrix stored in block-reduced   a, ja, ia  format to the
c general  sparse row a,  ja, ia  format.  A matrix   that has  a block
c structure is a matrix whose entries are blocks  of the same size m
c (e.g.  3 x 3).   Then it is often preferred  to work with the reduced
c graph of the matrix. Instead of storing one element at a time one can
c store a whole block at a time.  In this storage scheme  an entry is a
c square array holding the m**2 elements of a block.
c
c-----------------------------------------------------------------------
c on entry:
c----------
c job   = if job.eq.0 on entry, values are not copied (pattern only)
c
c n   = the block row dimension of the matrix.
c
c m     = the dimension of each block. Thus, the actual row dimension
c         of A is n x m.
c
c na  = first dimension of array a as declared in calling program.
c         This should be .ge. m**2.
c
c a   = real array containing the real entries of the matrix. Recall
c         that each entry is in fact an m x m block. These entries
c         are stored column-wise in locations a(1:m*m,k) for each k-th
c         entry. See details below.
c
c ja  = integer array of length n. ja(k) contains the column index
c         of the leading element, i.e., the element (1,1) of the block
c         that is held in the column a(*,k) of the value array.
c
c ia    = integer array of length n+1. ia(i) points to the beginning
c         of block row number i in the arrays a and ja.
c
c on return:
c-----------
c ao, jao,
c iao   = matrix stored in compressed sparse row format. The number of
c         rows in the new matrix is n x m.
c
c Notes: THIS CODE IS NOT IN PLACE.
c
c-----------------------------------------------------------------------
c BSR FORMAT.
c----------
c Each row of A contains the m x m block matrix unpacked column-
c wise (this allows the user to declare the array a as a(m,m,*) on entry
c if desired). The block rows are stored in sequence just as for the
c compressed sparse row format.
c
c-----------------------------------------------------------------------
c     example  with m = 2:
c                                                       1  2 3
c    +-------|--------|--------+                       +-------+
c    | 1   2 |  0   0 |  3   4 |     Block             | x 0 x | 1
c    | 5   6 |  0   0 |  7   8 |     Representation:   | 0 x x | 2
c    +-------+--------+--------+                       | x 0 0 | 3
c    | 0   0 |  9  10 | 11  12 |                       +-------+
c    | 0   0 | 13  14 | 15  16 |
c    +-------+--------+--------+
c    | 17 18 |  0   0 |  0   0 |
c    | 22 23 |  0   0 |  0   0 |
c    +-------+--------+--------+
c
c    For this matrix:     n    = 3
c                         m    = 2
c                         nnz  = 5
c-----------------------------------------------------------------------
c Data structure in Block Sparse Row format:
c-------------------------------------------
c Array A:
c-------------------------
c     1   3   9   11   17   <<--each m x m block is stored column-wise
c     5   7   13  15   22       in a  column of the array A.
c     2   4   10  12   18
c     6   8   14  16   23
c-------------------------
c JA  1   3   2    3    1   <<-- column indices for each block. Note that
c-------------------------       these indices are wrt block matrix.
c IA  1   3   5    6        <<-- pointers to beginning of each block row
c-------------------------       in arrays A and JA.
c-----------------------------------------------------------------------
c locals
c
      integer i, i1, i2, ij, ii, irow, j, jstart, k, krow, no
      logical val
c
      val = (job.ne.0)
      no = n * m
      irow = 1 
      krow = 1
      iao(irow) = 1
c-----------------------------------------------------------------------
      do 2 ii=1, n
c
c     recall: n is the block-row dimension
c
         i1 = ia(ii)
         i2 = ia(ii+1)-1
c
c     create m rows for each block row -- i.e., each k.
c
         do 23 i=1,m
            do 21 k=i1, i2
               jstart = m*(ja(k)-1)
               do 22  j=1,m
                  ij = (j-1)*m + i
                  if (val) ao(krow) = a(ij,k)
                  jao(krow) = jstart+j
                  krow = krow+1
 22            continue 
 21         continue
            irow = irow+1
            iao(irow) = krow
 23      continue
 2    continue
      return
c-------------end-of-bsrcsr --------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrbsr (job,nrow,m,na,a,ja,ia,ao,jao,iao,iw,ierr)
      implicit none
      integer job,ierr,nrow,m,na,ia(nrow+1),ja(*),jao(na),iao(*),iw(*)
      real*8 a(*),ao(na,*)
c-----------------------------------------------------------------------
c     Compressed Sparse Row  to    Block Sparse Row
c-----------------------------------------------------------------------
c
c This  subroutine converts a matrix stored  in a general compressed a,
c ja, ia format into a a block  sparse row format a(m,m,*),ja(*),ia(*).
c See routine  bsrcsr  for more  details on  data   structure for block
c matrices.
c
c NOTES: 1) the initial matrix does not have to have a block structure.
c zero padding is done for general sparse matrices.
c        2) For most practical purposes, na should be the same as m*m.
c
c-----------------------------------------------------------------------
c
c In what follows nr=1+(nrow-1)/m = block-row dimension of output matrix
c
c on entry:
c----------
c
c job   =  job indicator.
c          job =  0 -> only the pattern of output matrix is generated
c          job >  0 -> both pattern and values are generated.
c          job = -1 -> iao(1) will return the number of nonzero blocks,
c            in the output matrix. In this case jao(1:nr) is used as
c            workspace, ao is untouched, iao is untouched except iao(1)
c
c nrow   = integer, the actual row dimension of the matrix.
c
c m     = integer equal to the dimension of each block. m should be > 0.
c
c na  = first dimension of array ao as declared in calling program.
c         na should be .ge. m*m.
c
c a, ja,
c    ia = input matrix stored in compressed sparse row format.
c
c on return:
c-----------
c
c ao    = real  array containing the  values of the matrix. For details
c         on the format  see below. Each  row of  a contains the  m x m
c         block matrix  unpacked column-wise (this  allows the  user to
c         declare the  array a as ao(m,m,*) on  entry if desired).  The
c         block rows are stored in sequence  just as for the compressed
c         sparse row format. The block  dimension  of the output matrix
c         is  nr = 1 + (nrow-1) / m.
c
c jao   = integer array. containing the block-column indices of the
c         block-matrix. Each jao(k) is an integer between 1 and nr
c         containing the block column index of the block ao(*,k).
c
c iao   = integer array of length nr+1. iao(i) points to the beginning
c         of block row number i in the arrays ao and jao. When job=-1
c         iao(1) contains the number of nonzero blocks of the output
c         matrix and the rest of iao is unused. This is useful for
c         determining the lengths of ao and jao.
c
c ierr  = integer, error code.
c              0 -- normal termination
c              1 -- m is equal to zero
c              2 -- NA too small to hold the blocks (should be .ge. m**2)
c
c Work arrays:
c-------------
c iw    = integer work array of dimension  nr = 1 + (nrow-1) / m
c
c NOTES:
c-------
c     1) this code is not in place.
c     2) see routine bsrcsr for details on data sctructure for block
c        sparse row format.
c
c-----------------------------------------------------------------------
c     nr is the block-dimension of the output matrix.
c
      integer nr, m2, io, ko, ii, len, k, jpos, j, i, ij, jr, irow
      logical vals
c-----
      ierr = 0
      if (m*m .gt. na) ierr = 2
      if (m .eq. 0) ierr = 1
      if (ierr .ne. 0) return
c-----------------------------------------------------------------------
      vals = (job .gt. 0)
      nr = 1 + (nrow-1) / m
      m2 = m*m
      ko = 1
      io = 1
      iao(io) = 1
      len = 0
c
c     iw determines structure of block-row (nonzero indicator)
c
         do j=1, nr
            iw(j) = 0
         enddo
c
c     big loop -- leap by m rows each time.
c
      do ii=1, nrow, m
         irow = 0
c
c     go through next m rows -- make sure not to go beyond nrow.
c
         do while (ii+irow .le. nrow .and. irow .le. m-1)
            do k=ia(ii+irow),ia(ii+irow+1)-1
c
c     block column index = (scalar column index -1) / m + 1
c
               j = ja(k)-1
               jr = j/m + 1
               j = j - (jr-1)*m
               jpos = iw(jr)
               if (jpos .eq. 0) then
c
c     create a new block
c
                  iw(jr) = ko
                  jao(ko) = jr
                  if (vals) then
c
c     initialize new block to zero -- then copy nonzero element
c
                     do i=1, m2
                        ao(i,ko) = 0.0d0
                     enddo
                     ij = j*m + irow + 1
                     ao(ij,ko) = a(k)
                  endif
                  ko = ko+1
               else
c
c     copy column index and nonzero element
c
                  jao(jpos) = jr
                  ij = j*m + irow + 1
                  if (vals) ao(ij,jpos) = a(k)
               endif
            enddo
            irow = irow+1
         enddo
c
c     refresh iw
c
         do j = iao(io),ko-1
            iw(jao(j)) = 0
         enddo
         if (job .eq. -1) then
            len = len + ko-1
            ko = 1
         else
            io = io+1
            iao(io) = ko
         endif
      enddo
      if (job .eq. -1) iao(1) = len
c
      return
c--------------end-of-csrbsr--------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrbnd (n,a,ja,ia,job,abd,nabd,lowd,ml,mu,ierr)
      integer, intent(in) :: n, nabd, job
      integer, intent(out) :: ierr
      real*8 , intent(inout) :: a(*),abd(nabd,n)
      integer, intent(inout) :: ia(n+1),ja(*),lowd
      integer :: i,j,k,m,ii,mdiag,ml,mu
c-----------------------------------------------------------------------
c   Compressed Sparse Row  to  Banded (Linpack ) format.
c-----------------------------------------------------------------------
c this subroutine converts a general sparse matrix stored in
c compressed sparse row format into the banded format. for the
c banded format,the Linpack conventions are assumed (see below).
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = integer,the actual row dimension of the matrix.
c
c a,
c ja,
c ia    = input matrix stored in compressed sparse row format.
c
c job = integer. if job=1 then the values of the lower bandwith ml
c         and the upper bandwidth mu are determined internally.
c         otherwise it is assumed that the values of ml and mu
c         are the correct bandwidths on input. See ml and mu below.
c
c nabd  = integer. first dimension of array abd.
c
c lowd  = integer. this should be set to the row number in abd where
c         the lowest diagonal (leftmost) of A is located.
c         lowd should be  ( 1  .le.  lowd  .le. nabd).
c         if it is not known in advance what lowd should be
c         enter lowd = 0 and the default value lowd = ml+mu+1
c         will be chosen. Alternative: call routine getbwd from unary
c         first to detrermione ml and mu then define lowd accordingly.
c         (Note: the banded solvers in linpack use lowd=2*ml+mu+1. )
c
c ml  = integer. equal to the bandwidth of the strict lower part of A
c mu  = integer. equal to the bandwidth of the strict upper part of A
c         thus the total bandwidth of A is ml+mu+1.
c         if ml+mu+1 is found to be larger than lowd then an error
c         flag is raised (unless lowd = 0). see ierr.
c
c note:   ml and mu are assumed to have    the correct bandwidth values
c         as defined above if job is set to zero on entry.
c
c on return:
c-----------
c
c abd   = real array of dimension abd(nabd,n).
c         on return contains the values of the matrix stored in
c         banded form. The j-th column of abd contains the elements
c         of the j-th column of  the original matrix comprised in the
c         band ( i in (j-ml,j+mu) ) with the lowest diagonal at
c         the bottom row (row lowd). See details below for this format.
c
c ml  = integer. equal to the bandwidth of the strict lower part of A
c mu  = integer. equal to the bandwidth of the strict upper part of A
c         if job=1 on entry then these two values are internally computed.
c
c lowd  = integer. row number in abd where the lowest diagonal
c         (leftmost) of A is located on return. In case lowd = 0
c         on return, then it is defined to ml+mu+1 on return and the
c         lowd will contain this value on return. `
c
c ierr  = integer. used for error messages. On return:
c         ierr .eq. 0  :means normal return
c         ierr .eq. -1 : means invalid value for lowd. (either .lt. 0
c         or larger than nabd).
c         ierr .eq. -2 : means that lowd is not large enough and as
c         result the matrix cannot be stored in array abd.
c         lowd should be at least ml+mu+1, where ml and mu are as
c         provided on output.
c
c----------------------------------------------------------------------*
c Additional details on banded format.  (this closely follows the      *
c format used in linpack. may be useful for converting a matrix into   *
c this storage format in order to use the linpack  banded solvers).    *
c----------------------------------------------------------------------*
c             ---  band storage format  for matrix abd ---             *
c uses ml+mu+1 rows of abd(nabd,*) to store the diagonals of           *
c a in rows of abd starting from the lowest (sub)-diagonal  which  is  *
c stored in row number lowd of abd. the minimum number of rows needed  *
c in abd is ml+mu+1, i.e., the minimum value for lowd is ml+mu+1. the  *
c j-th  column  of  abd contains the elements of the j-th column of a, *
c from bottom to top: the element a(j+ml,j) is stored in  position     *
c abd(lowd,j), then a(j+ml-1,j) in position abd(lowd-1,j) and so on.   *
c Generally, the element a(j+k,j) of original matrix a is stored in    *
c position abd(lowd+k-ml,j), for k=ml,ml-1,..,0,-1, -mu.               *
c The first dimension nabd of abd must be .ge. lowd                    *
c                                                                      *
c     example [from linpack ]:   if the original matrix is             *
c                                                                      *
c              11 12 13  0  0  0                                       *
c              21 22 23 24  0  0                                       *
c               0 32 33 34 35  0     original banded matrix            *
c               0  0 43 44 45 46                                       *
c               0  0  0 54 55 56                                       *
c               0  0  0  0 65 66                                       *
c                                                                      *
c then  n = 6, ml = 1, mu = 2. lowd should be .ge. 4 (=ml+mu+1)  and   *
c if lowd = 5 for example, abd  should be:                             *
c                                                                      *
c untouched --> x  x  x  x  x  x                                       *
c               *  * 13 24 35 46                                       *
c               * 12 23 34 45 56    resulting abd matrix in banded     *
c              11 22 33 44 55 66    format                             *
c  row lowd--> 21 32 43 54 65  *                                       *
c                                                                      *
c * = not used                                                         *
c
*
c----------------------------------------------------------------------*
c first determine ml and mu.
c-----------------------------------------------------------------------
      ierr = 0
c-----------
      if (job .eq. 1) call getbwd(n,a,ja,ia,ml,mu)
      m = ml+mu+1
      if (lowd .eq. 0) lowd = m
      if (m .gt. lowd)  THEN 
         ierr = -2  
      ENDIF
      if (lowd .gt. nabd .or. lowd .lt. 0) ierr = -1
      if (ierr .lt. 0) return
c------------
      do 15  i=1,m
         ii = lowd -i+1
         do 10 j=1,n
       abd(ii,j) = 0.0d0
 10      continue
 15   continue
c---------------------------------------------------------------------  
      mdiag = lowd-ml
      do 30 i=1,n
         do 20 k=ia(i),ia(i+1)-1
            j = ja(k)
            abd(i-j+mdiag,j) = a(k)
 20      continue
 30   continue
      return
c------------- end of csrbnd -------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine bndcsr (n,abd,nabd,lowd,ml,mu,a,ja,ia,len,ierr)
      integer, intent(in) :: n, nabd
      integer, intent(out) :: ierr
      real*8,  intent(inout) :: a(*),abd(nabd,*)
      integer, intent(inout) :: ia(n+1),ja(*), len,lowd
      real*8 :: t
      integer :: i,j,irow,ml,mu,ko
c-----------------------------------------------------------------------
c Banded (Linpack ) format   to    Compressed Sparse Row  format.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = integer,the actual row dimension of the matrix.
c
c nabd  = first dimension of array abd.
c
c abd   = real array containing the values of the matrix stored in
c         banded form. The j-th column of abd contains the elements
c         of the j-th column of  the original matrix,comprised in the
c         band ( i in (j-ml,j+mu) ) with the lowest diagonal located
c         in row lowd (see below).
c
c lowd  = integer. this should be set to the row number in abd where
c         the lowest diagonal (leftmost) of A is located.
c         lowd should be s.t.  ( 1  .le.  lowd  .le. nabd).
c         The subroutines dgbco, ... of linpack use lowd=2*ml+mu+1.
c
c ml  = integer. equal to the bandwidth of the strict lower part of A
c mu  = integer. equal to the bandwidth of the strict upper part of A
c         thus the total bandwidth of A is ml+mu+1.
c         if ml+mu+1 is found to be larger than nabd then an error
c         message is set. see ierr.
c
c len   = integer. length of arrays a and ja. bndcsr will stop if the
c         length of the arrays a and ja is insufficient to store the
c         matrix. see ierr.
c
c on return:
c-----------
c a,
c ja,
c ia    = input matrix stored in compressed sparse row format.
c
c lowd  = if on entry lowd was zero then lowd is reset to the default
c         value ml+mu+l.
c
c ierr  = integer. used for error message output.
c         ierr .eq. 0 :means normal return
c         ierr .eq. -1 : means invalid value for lowd.
c    ierr .gt. 0 : means that there was not enough storage in a and ja
c         for storing the ourput matrix. The process ran out of space
c         (as indicated by len) while trying to fill row number ierr.
c         This should give an idea of much more storage might be required.
c         Moreover, the first irow-1 rows are correctly filled.
c
c notes:  the values in abd found to be equal to zero
c -----   (actual test: if (abd(...) .eq. 0.0d0) are removed.
c         The resulting may not be identical to a csr matrix
c         originally transformed to a bnd format.
c
c-----------------------------------------------------------------------
      ierr = 0
c-----------
      if (lowd .gt. nabd .or. lowd .le. 0) then
         ierr = -1
         return
      endif
c-----------
      ko = 1
      ia(1) = 1
      do 30 irow=1,n
c-----------------------------------------------------------------------
         i = lowd
          do  20 j=irow-ml,irow+mu
             if (j .le. 0 ) goto 19
             if (j .gt. n) goto 21
             t = abd(i,j)
             if (t .eq. 0.0d0) goto 19
             if (ko .gt. len) then
               ierr = irow
               return
            endif
            a(ko) = t
            ja(ko) = j
            ko = ko+1
 19         i = i-1
 20      continue
c     end for row irow
 21      ia(irow+1) = ko
 30   continue
      return
c------------- end of bndcsr -------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrssk (n,imod,a,ja,ia,asky,isky,nzmax,ierr)
      integer, intent(in) :: n, nzmax
      integer, intent(out) :: ierr
      real*8 , intent(inout) :: a(*), asky(nzmax)
      integer, intent(inout) :: imod, ia(n+1), isky(n+1), ja(*)
      integer :: i,j,k,ml,nnz,kend
c-----------------------------------------------------------------------
c      Compressed Sparse Row         to     Symmetric Skyline Format
c  or  Symmetric Sparse Row
c-----------------------------------------------------------------------
c this subroutine translates a compressed sparse row or a symmetric
c sparse row format into a symmetric skyline format.
c the input matrix can be in either compressed sparse row or the
c symmetric sparse row format. The output matrix is in a symmetric
c skyline format: a real array containing the (active portions) of the
c rows in  sequence and a pointer to the beginning of each row.
c
c This module is NOT  in place.
c-----------------------------------------------------------------------
c Coded by Y. Saad, Oct 5, 1989. Revised Feb. 18, 1991.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n   = integer equal to the dimension of A. 
c imod  = integer indicating the variant of skyline format wanted:
c         imod = 0 means the pointer isky points to the `zeroth'
c         element of the row, i.e., to the position of the diagonal
c         element of previous row (for i=1, isky(1)= 0)
c         imod = 1 means that itpr points to the beginning of the row.
c         imod = 2 means that isky points to the end of the row (diagonal
c                  element)
c
c a   = real array of size nna containing the nonzero elements
c ja  = integer array of size nnz containing the column positions
c    of the corresponding elements in a.
c ia  = integer of size n+1. ia(k) contains the position in a, ja of
c    the beginning of the k-th row.
c nzmax = integer. must be set to the number of available locations
c         in the output array asky.
c
c on return:
c----------
c
c asky    = real array containing the values of the matrix stored in skyline
c         format. asky contains the sequence of active rows from
c         i=1, to n, an active row being the row of elemnts of
c         the matrix contained between the leftmost nonzero element
c         and the diagonal element.
c isky   = integer array of size n+1 containing the pointer array to
c         each row. The meaning of isky depends on the input value of
c         imod (see above).
c ierr  =  integer.  Error message. If the length of the
c         output array asky exceeds nzmax. ierr returns the minimum value
c         needed for nzmax. otherwise ierr=0 (normal return).
c
c Notes:
c         1) This module is NOT  in place.
c         2) even when imod = 2, length of  isky is  n+1, not n.
c
c-----------------------------------------------------------------------
c first determine individial bandwidths and pointers.
c-----------------------------------------------------------------------
      ierr = 0
      isky(1) = 0
      do 3 i=1,n
         ml = 0
         do 31 k=ia(i),ia(i+1)-1
            ml = max(ml,i-ja(k)+1)
 31      continue
         isky(i+1) = isky(i)+ml
 3    continue
c
c     test if there is enough space  asky to do the copying.
c
      nnz = isky(n+1)
      if (nnz .gt. nzmax) then
         ierr = nnz
         return
      endif
c
c   fill asky with zeros.
c
      do 1 k=1, nnz
         asky(k) = 0.0d0
 1    continue
c
c     copy nonzero elements.
c
      do 4 i=1,n
         kend = isky(i+1)
         do 41 k=ia(i),ia(i+1)-1
            j = ja(k)
            if (j .le. i) asky(kend+j-i) = a(k)
 41      continue
 4    continue
c
c modify pointer according to imod if necessary.
c
      if (imod .eq. 0) return
      if (imod .eq. 1) then
         do 50 k=1, n+1
            isky(k) = isky(k)+1
 50      continue
      endif
      if (imod .eq. 2) then
         do 60 k=1, n
            isky(k) = isky(k+1)
 60      continue
      endif
c
      return
c------------- end of csrssk -------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine sskssr (n,imod,asky,isky,ao,jao,iao,nzmax,ierr)
      integer, intent(in) :: n, nzmax
      integer, intent(out) :: ierr
      real*8 , intent(inout) :: asky(*),ao(nzmax)
      integer, intent(inout) :: imod, isky(n+1),iao(n+1),jao(nzmax)
c-----------------------------------------------------------------------
c     Symmetric Skyline Format  to  Symmetric Sparse Row format.
c-----------------------------------------------------------------------
c  tests for exact zeros in skyline matrix (and ignores them in
c  output matrix).  In place routine (a, isky :: ao, iao)
c-----------------------------------------------------------------------
c this subroutine translates a  symmetric skyline format into a
c symmetric sparse row format. Each element is tested to see if it is
c a zero element. Only the actual nonzero elements are retained. Note
c that the test used is simple and does take into account the smallness
c of a value. the subroutine filter (see unary module) can be used
c for this purpose.
c-----------------------------------------------------------------------
c Coded by Y. Saad, Oct 5, 1989. Revised Feb 18, 1991./
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n   = integer equal to the dimension of A. 
c imod  = integer indicating the variant of skyline format used:
c         imod = 0 means the pointer iao points to the `zeroth'
c         element of the row, i.e., to the position of the diagonal
c         element of previous row (for i=1, iao(1)= 0)
c         imod = 1 means that itpr points to the beginning of the row.
c         imod = 2 means that iao points to the end of the row
c                  (diagonal element)
c asky  = real array containing the values of the matrix. asky contains
c         the sequence of active rows from i=1, to n, an active row
c         being the row of elemnts of the matrix contained between the
c         leftmost nonzero element and the diagonal element.
c isky   = integer array of size n+1 containing the pointer array to
c         each row. isky (k) contains the address of the beginning of the
c         k-th active row in the array asky.
c nzmax = integer. equal to the number of available locations in the
c         output array ao.
c
c on return:
c ----------
c ao  = real array of size nna containing the nonzero elements
c jao = integer array of size nnz containing the column positions
c    of the corresponding elements in a.
c iao = integer of size n+1. iao(k) contains the position in a, ja of
c    the beginning of the k-th row.
c ierr  = integer. Serving as error message. If the length of the
c         output arrays ao, jao exceeds nzmax then ierr returns
c         the row number where the algorithm stopped: rows
c         i, to ierr-1 have been processed succesfully.
c         ierr = 0 means normal return.
c         ierr = -1  : illegal value for imod
c Notes:
c-------
c This module is in place: ao and iao can be the same as asky, and isky.
c-----------------------------------------------------------------------
c local variables
      integer next, kend, kstart, i, j, k
      ierr = 0
c
c check for validity of imod
c
      if (imod.ne.0 .and. imod.ne.1 .and. imod .ne. 2) then
         ierr =-1
         return
      endif
c
c next  = pointer to next available position in output matrix
c kend  = pointer to end of current row in skyline matrix.
c
      next = 1
c
c set kend = start position -1 in  skyline matrix.
c
      kend = 0
      if (imod .eq. 1) kend = isky(1)-1
      if (imod .eq. 0) kend = isky(1)
c
c loop through all rows
c
      do 50 i=1,n
c
c save value of pointer to ith row in output matrix
c
         iao(i) = next
c
c get beginnning and end of skyline  row
c
         kstart = kend+1
         if (imod .eq. 0) kend = isky(i+1)
         if (imod .eq. 1) kend = isky(i+1)-1
         if (imod .eq. 2) kend = isky(i)
c
c copy element into output matrix unless it is a zero element.
c
         do 40 k=kstart,kend
            if (asky(k) .eq. 0.0d0) goto 40
            j = i-(kend-k)
            jao(next) = j
            ao(next)  = asky(k)
            next=next+1
            if (next .gt. nzmax+1) then
               ierr = i
               return
            endif
 40      continue
 50    continue
      iao(n+1) = next
      return
c-------------end-of-sskssr --------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrjad (nrow, a, ja, ia, idiag, iperm, ao, jao, iao)
      integer, intent(in) :: nrow
      integer, intent(out) :: idiag
      integer, intent(inout) :: ja(*), jao(*), ia(nrow+1), iperm(nrow)
     * , iao(nrow)
      real*8 , intent(inout) :: a(*), ao(*)
      integer :: i,j,k,k0,k1,jj,ilo,len
c-----------------------------------------------------------------------
c    Compressed Sparse Row  to   JAgged Diagonal storage.
c-----------------------------------------------------------------------
c this subroutine converts  matrix stored in the compressed sparse
c row format to the jagged diagonal format. The data structure
c for the JAD (Jagged Diagonal storage) is as follows. The rows of
c the matrix are (implicitly) permuted so that their lengths are in
c decreasing order. The real entries ao(*) and their column indices
c jao(*) are stored in succession. The number of such diagonals is idiag.
c the lengths of each of these diagonals is stored in iao(*).
c For more details see [E. Anderson and Y. Saad,
c ``Solving sparse triangular systems on parallel computers'' in
c Inter. J. of High Speed Computing, Vol 1, pp. 73-96 (1989).]
c or  [Y. Saad, ``Krylov Subspace Methods on Supercomputers''
c SIAM J. on  Stat. Scient. Comput., volume 10, pp. 1200-1232 (1989).]
c-----------------------------------------------------------------------
c on entry:
c----------
c nrow     = row dimension of the matrix A.
c
c a,
c ia,
c ja      = input matrix in compressed sparse row format.
c
c on return:
c----------
c
c idiag = integer. The number of jagged diagonals in the matrix.
c
c iperm = integer array of length nrow containing the permutation
c         of the rows that leads to a decreasing order of the
c         number of nonzero elements.
c
c ao    = real array containing the values of the matrix A in
c         jagged diagonal storage. The j-diagonals are stored
c         in ao in sequence.
c
c jao   = integer array containing the column indices of the
c         entries in ao.
c
c iao   = integer array containing pointers to the beginning
c         of each j-diagonal in ao, jao. iao is also used as
c         a work array and it should be of length n at least.
c
c-----------------------------------------------------------------------
c     ---- define initial iperm and get lengths of each row
c     ---- jao is used a work vector to store tehse lengths
c
      idiag = 0
      ilo = nrow
      do 10 j=1, nrow
         iperm(j) = j
         len = ia(j+1) - ia(j)
         ilo = min(ilo,len)
         idiag = max(idiag,len)
         jao(j) = len
 10   continue
c
c     call sorter to get permutation. use iao as work array.
c
      call dcsort (jao, nrow, iao, iperm, ilo, idiag)
c
c     define output data structure. first lengths of j-diagonals
c
      do 20 j=1, nrow
         iao(j) = 0
 20   continue
      do 40 k=1, nrow
         len = jao(iperm(k))
         do 30 i=1,len
            iao(i) = iao(i)+1
 30      continue
 40   continue
c
c     get the output matrix itself
c
      k1 = 1
      k0 = k1
      do 60 jj=1, idiag
         len = iao(jj)
         do 50 k=1,len
            i = ia(iperm(k))+jj-1
            ao(k1) = a(i)
            jao(k1) = ja(i)
            k1 = k1+1
 50      continue
         iao(jj) = k0
         k0 = k1
 60   continue
      iao(idiag+1) = k1
      return
c----------end-of-csrjad------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine jadcsr (nrow, idiag, a, ja, ia, iperm, ao, jao, iao)
      integer, intent(in) :: nrow, idiag
      integer, intent(inout) :: ja(*), jao(*), ia(idiag+1), iperm(nrow)
     * , iao(nrow+1)
      real*8 , intent(inout) :: a(*), ao(*)
      integer :: k1, jj, kpos, i, j, k, len
c-----------------------------------------------------------------------
c     Jagged Diagonal Storage   to     Compressed Sparse Row
c-----------------------------------------------------------------------
c this subroutine converts a matrix stored in the jagged diagonal format
c to the compressed sparse row format.
c-----------------------------------------------------------------------
c on entry:
c----------
c nrow     = integer. the row dimension of the matrix A.
c
c idiag   = integer. The  number of jagged diagonals in the data
c           structure a, ja, ia.
c
c a,
c ja,
c ia      = input matrix in jagged diagonal format.
c
c iperm   = permutation of the rows used to obtain the JAD ordering.
c
c on return:
c----------
c
c ao, jao,
c iao     = matrix in CSR format.
c-----------------------------------------------------------------------
c determine first the pointers for output matrix. Go through the
c structure once:
c
      do 137 j=1,nrow
         jao(j) = 0
 137  continue
c
c     compute the lengths of each row of output matrix -
c
      do 140 i=1, idiag
         len = ia(i+1)-ia(i)
         do 138 k=1,len
            jao(iperm(k)) = jao(iperm(k))+1
 138     continue
 140  continue
c
c     remember to permute
c
      kpos = 1
      iao(1) = 1
      do 141 i=1, nrow
         kpos = kpos+jao(i)
         iao(i+1) = kpos
 141  continue
c
c     copy elemnts one at a time.
c
      do 200 jj = 1, idiag
         k1 = ia(jj)-1
         len = ia(jj+1)-k1-1
         do 160 k=1,len
            kpos = iao(iperm(k))
            ao(kpos) = a(k1+k)
            jao(kpos) = ja(k1+k)
            iao(iperm(k)) = kpos+1
 160     continue
 200  continue
c
c     rewind pointers
c
      do 5 j=nrow,1,-1
         iao(j+1) = iao(j)
 5    continue
      iao(1) = 1
      return
c----------end-of-jadcsr------------------------------------------------
c-----------------------------------------------------------------------
      end
      subroutine dcsort(ival, n, icnt, index, ilo, ihi)
c-----------------------------------------------------------------------
c     Specifications for arguments:
c     ----------------------------
      integer n, ilo, ihi, ival(n), icnt(ilo:ihi), index(n)
c-----------------------------------------------------------------------
c    This routine computes a permutation which, when applied to the
c    input vector ival, sorts the integers in ival in descending
c    order.  The permutation is represented by the vector index.  The
c    permuted ival can be interpreted as follows:
c      ival(index(i-1)) .ge. ival(index(i)) .ge. ival(index(i+1))
c
c    A specialized sort, the distribution counting sort, is used
c    which takes advantage of the knowledge that
c        1)  The values are in the (small) range [ ilo, ihi ]
c        2)  Values are likely to be repeated often
c
c    contributed to SPARSKIT by Mike Heroux. (Cray Research)
c    ---------------------------------------
c-----------------------------------------------------------------------
c Usage:
c------
c     call dcsort( ival, n, icnt, index, ilo, ihi )
c
c Arguments:
c-----------
c    ival  integer array (input)
c          On entry, ia is an n dimensional array that contains
c          the values to be sorted.  ival is unchanged on exit.
c
c    n     integer (input)
c          On entry, n is the number of elements in ival and index.
c
c    icnt  integer (work)
c          On entry, is an integer work vector of length
c          (ihi - ilo + 1).
c
c    index integer array (output)
c          On exit, index is an n-length integer vector containing
c          the permutation which sorts the vector ival.
c
c    ilo   integer (input)
c          On entry, ilo is .le. to the minimum value in ival.
c
c    ihi   integer (input)
c          On entry, ihi is .ge. to the maximum value in ival.
c
c Remarks:
c---------
c         The permutation is NOT applied to the vector ival.
c
c----------------------------------------------------------------
c
c Local variables:
c    Other integer values are temporary indices.
c
c Author:
c--------
c    Michael Heroux
c    Sandra Carney
c       Mathematical Software Research Group
c       Cray Research, Inc.
c
c References:
c    Knuth, Donald E., "The Art of Computer Programming, Volume 3:
c    Sorting and Searching," Addison-Wesley, Reading, Massachusetts,
c    1973, pp. 78-79.
c
c Revision history:
c    05/09/90: Original implementation.  A variation of the
c              Distribution Counting Sort recommended by
c              Sandra Carney. (Mike Heroux)
c
c-----------------------------------------------------------------
c     ----------------------------------
c     Specifications for local variables
c     ----------------------------------
      integer i, j, ivalj
c
c     --------------------------
c     First executable statement
c     --------------------------
      do 10 i = ilo, ihi
        icnt(i) = 0
 10   continue
c
      do 20 i = 1, n
        icnt(ival(i)) = icnt(ival(i)) + 1
 20   continue
c
      do 30 i = ihi-1,ilo,-1
        icnt(i) = icnt(i) + icnt(i+1)
 30   continue
c
      do 40 j = n, 1, -1
        ivalj = ival(j)
        index(icnt(ivalj)) = j
        icnt(ivalj) = icnt(ivalj) - 1
 40   continue
      return
      end
c-------end-of-dcsort---------------------------------------------------
c-----------------------------------------------------------------------
      subroutine cooell(job,n,nnz,a,ja,ia,ao,jao,lda,ncmax,nc,ierr)
      implicit none
      integer job,n,nnz,lda,ncmax,nc,ierr
      integer ja(nnz),ia(nnz),jao(lda,ncmax)
      real*8  a(nnz),ao(lda,ncmax)
c-----------------------------------------------------------------------
c     COOrdinate format to ELLpack format
c-----------------------------------------------------------------------
c     On entry:
c     job     -- 0 if only pattern is to be processed(AO is not touched)
c     n       -- number of rows in the matrix
c     a,ja,ia -- input matix in COO format
c     lda     -- leading dimension of array AO and JAO
c     ncmax   -- size of the second dimension of array AO and JAO
c
c     On exit:
c     ao,jao  -- the matrix in ELL format
c     nc      -- maximum number of nonzeros per row
c     ierr    -- 0 if convertion succeeded
c                -1 if LDA < N
c                nc if NC > ncmax
c
c     NOTE: the last column of JAO is used as work space!!
c-----------------------------------------------------------------------
      integer i,j,k,ip
      real*8  zero
      logical copyval
      parameter (zero=0.0D0)
c     .. first executable statement ..
      copyval = (job.ne.0)
      if (lda .lt. n) then
         ierr = -1
         return
      endif
c     .. use the last column of JAO as workspace
c     .. initialize the work space
      do i = 1, n
         jao(i,ncmax) = 0
      enddo
      nc = 0
c     .. go through ia and ja to find out number nonzero per row
      do k = 1, nnz
         i = ia(k)
         jao(i,ncmax) = jao(i,ncmax) + 1
      enddo
c     .. maximum number of nonzero per row
      nc = 0
      do i = 1, n
         if (nc.lt.jao(i,ncmax)) nc = jao(i,ncmax)
         jao(i,ncmax) = 0
      enddo
c     .. if nc > ncmax retrun now
      if (nc.gt.ncmax) then
         ierr = nc
         return
      endif
c     .. go through ia and ja to copy the matrix to AO and JAO
      do k = 1, nnz
         i = ia(k)
         j = ja(k)
         jao(i,ncmax) = jao(i,ncmax) + 1
         ip = jao(i,ncmax)
         if (ip.gt.nc) nc = ip
         if (copyval) ao(i,ip) = a(k)
         jao(i,ip) = j
      enddo
c     .. fill the unspecified elements of AO and JAO with zero diagonals
      do i = 1, n
         do j = ia(i+1)-ia(i)+1, nc
            jao(i,j)=i
            if(copyval) ao(i,j) = zero
         enddo
      enddo
      ierr = 0
c
      return
      end
c-----end-of-cooell-----------------------------------------------------
c-----------------------------------------------------------------------
      subroutine xcooell(n,nnz,a,ja,ia,ac,jac,nac,ner,ncmax,ierr)
      integer, intent(in) :: n, nnz, nac, ner
      integer, intent(out) :: ierr
      real*8 , intent(inout) :: a(nnz), ac(nac,ner)
      integer, intent(inout) :: ja(nnz), ia(nnz), jac(nac,ner)
      integer ::ncmax, icount, k, ii, in, inn, is, innz
C-----------------------------------------------------------------------
C   coordinate format to ellpack format.
C-----------------------------------------------------------------------
C
C   DATE WRITTEN: June 4, 1989.
C
C   PURPOSE
C   -------
C  This subroutine takes a sparse matrix in coordinate format and
C  converts it into the Ellpack-Itpack storage.
C
C  Example:
C  -------
C       (   11   0   13    0     0     0  )
C       |   21  22    0   24     0     0  |
C       |    0  32   33    0    35     0  |
C   A = |    0   0   43   44     0    46  |
C       |   51   0    0   54    55     0  |
C       (   61  62    0    0    65    66  )
C
C   Coordinate storage scheme:
C
C    A  = (11,22,33,44,55,66,13,21,24,32,35,43,46,51,54,61,62,65)
C    IA = (1, 2, 3, 4, 5, 6, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6 )
C    JA = ( 1, 2, 3, 4, 5, 6, 3, 1, 4, 2, 5, 3, 6, 1, 4, 1, 2, 5)
C
C   Ellpack-Itpack storage scheme:
C
C       (   11  13    0    0   )          (   1   3   *    *  )
C       |   22  21   24    0   |          |   2   1   4    *  |
C  AC = |   33  32   35    0   |    JAC = |   3   2   5    *  |
C       |   44  43   46    0   |          |   4   3   6    *  |
C       |   55  51   54    0   |          |   5   1   4    *  |
C       (   66  61   62   65   )          (   6   1   2    5  )
C
C   Note: * means that you can store values from 1 to 6 (1 to n, where
C         n is the order of the matrix) in that position in the array.
C
C   Contributed by:
C   ---------------
C   Ernest E. Rothman
C   Cornell Thoery Center/Cornell National Supercomputer Facility
C   e-mail address: BITNET:   EER@CORNELLF.BITNET
C                   INTERNET: eer@cornellf.tn.cornell.edu
C
C   checked and modified  04/13/90 Y.Saad.
C
C   REFERENCES
C   ----------
C   Kincaid, D. R.; Oppe, T. C.; Respess, J. R.; Young, D. M. 1984.
C   ITPACKV 2C User's Guide, CNA-191. Center for Numerical Analysis,
C   University of Texas at Austin.
C
C   "Engineering and Scientific Subroutine Library; Guide and
C   Reference; Release 3 (SC23-0184-3). Pp. 79-86.
C
C-----------------------------------------------------------------------
C
C   INPUT PARAMETERS
C   ----------------
C  N       - Integer. The size of the square matrix.
C
C  NNZ     - Integer. Must be greater than or equal to the number of
C            nonzero elements in the sparse matrix. Dimension of A, IA
C            and JA.
C
C  NCA     - Integer. First dimension of output arrays ca and jac.
C
C  A(NNZ)  - Real array. (Double precision)
C            Stored entries of the sparse matrix A.
C            NNZ is the number of nonzeros.
C
C  IA(NNZ) - Integer array.
C            Pointers to specify rows for the stored nonzero entries
C            in A.
C
C  JA(NNZ) - Integer array.
C            Pointers to specify columns for the stored nonzero
C            entries in A.
C
C  NER     - Integer. Must be set greater than or equal to the maximum
C            number of nonzeros in any row of the sparse matrix.
C
C  OUTPUT PARAMETERS
C  -----------------
C  AC(NAC,*)  - Real array. (Double precision)
C               Stored entries of the sparse matrix A in compressed
C               storage mode.
C
C  JAC(NAC,*) - Integer array.
C               Contains the column numbers of the sparse matrix
C               elements stored in the corresponding positions in
C               array AC.
C
C  NCMAX   -  Integer. Equals the maximum number of nonzeros in any
C             row of the sparse matrix.
C
C  IERR    - Error parameter is returned as zero on successful
C             execution of the subroutin<e.
C             Error diagnostics are given by means of positive values
C             of this parameter as follows:
C
C             IERR = -1   -  NER is too small and should be set equal
C                            to NCMAX. The array AC may not be large
C                            enough to accomodate all the non-zeros of
C                            of the sparse matrix.
C             IERR =  1   -  The array AC has a zero column. (Warning)
C             IERR =  2   -  The array AC has a zero row.    (Warning)
C
C---------------------------------------------------------------------
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c   Initial error parameter to zero:
c
      ierr = 0
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c   Initial output arrays to zero:
c
      do 4 in = 1,ner
         do 4 innz =1,n
            jac(innz,in) = n
            ac(innz,in) = 0.0d0
 4    continue
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c   Assign nonzero elements of the sparse matrix (stored in the one
c   dimensional array A to the two dimensional array AC.
c   Also, assign the correct values with information about their
c   column indices to the two dimensional array KA. And at the same
c   time count the number of nonzeros in each row so that the
c   parameter NCMAX equals the maximum number of nonzeros in any row
c   of the sparse matrix.
c
      ncmax = 1
      do 10 is = 1,n
         k = 0
         do 30 ii = 1,nnz
            if(ia(ii).eq.is)then
               k = k + 1
               if (k .le. ner) then
                  ac(is,k) = a(ii)
                  jac(is,k) = ja(ii)
               endif
            endif
 30      continue
         if (k.ge.ncmax) ncmax = k
 10   continue
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c     Perform some simple error checks:
c
check maximum number of nonzeros in each row:
      if (ncmax.eq.ner) ierr = 0
      if (ncmax.gt.ner) then
         ierr = -1
         return
      endif
c
check if there are any zero columns in AC:
c
      do 45 in = 1,ncmax
         icount = 0
         do 44 inn =1,n
            if (ac(inn,in).ne.0.0d0) icount = 1
 44      continue
         if (icount.eq.0) then
            ierr = 1
            return
         endif
 45   continue
c
check if there are any zero rows in AC:
c
      do 55 inn = 1,n
         icount = 0
         do 54 in =1,ncmax
            if (ac(inn,in).ne.0.0d0) icount = 1
 54      continue
         if (icount.eq.0) then
            ierr = 2
            return
         endif
 55   continue
      return
c------------- end of xcooell -------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csruss (nrow,a,ja,ia,diag,al,jal,ial,au,jau,iau)
      real*8 a(*),al(*),diag(*),au(*)
      integer nrow,ja(*),ia(nrow+1),jal(*),ial(nrow+1),jau(*),
     *     iau(nrow+1)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to     Unsymmetric Sparse Skyline format
c-----------------------------------------------------------------------
c this subroutine converts a matrix stored in csr format into a nonsym.
c sparse skyline format. This latter format does not assume
c that the matrix has a symmetric pattern and consists of the following
c * the diagonal of A stored separately in diag(*);
c * The strict lower part of A is stored  in CSR format in al,jal,ial
c * The strict upper part is stored in CSC format in au,jau,iau.
c-----------------------------------------------------------------------
c On entry
c---------
c nrow  = dimension of the matrix a.
c a     = real array containing the nonzero values of the matrix
c         stored rowwise.
c ja    = column indices of the values in array a
c ia    = integer array of length n+1 containing the pointers to
c         beginning of each row in arrays a, ja.
c
c On return
c----------
c diag  = array containing the diagonal entries of A
c al,jal,ial = matrix in CSR format storing the strict lower
c              trangular part of A.
c au,jau,iau = matrix in CSC format storing the strict upper
c              triangular part of A.
c-----------------------------------------------------------------------
      integer i, j, k, kl, ku
c
c determine U's data structure first
c
      do 1 i=1,nrow+1
         iau(i) = 0
 1    continue
      do 3 i=1, nrow
         do 2 k=ia(i), ia(i+1)-1
            j = ja(k)
            if (j .gt. i) iau(j+1) = iau(j+1)+1
 2       continue
 3    continue
c
c     compute pointers from lengths
c
      iau(1) = 1
      do 4 i=1,nrow
         iau(i+1) = iau(i)+iau(i+1)
         ial(i+1) = ial(i)+ial(i+1)
 4    continue
c
c     now do the extractions. scan all rows.
c
      kl = 1
      ial(1) = kl
      do  7 i=1, nrow
c
c     scan all elements in a row
c
         do 71 k = ia(i), ia(i+1)-1
            j = ja(k)
c
c     if in upper part, store in row j (of transp(U) )
c
            if (j  .gt. i) then
               ku = iau(j)
               au(ku) = a(k)
               jau(ku) = i
               iau(j) = ku+1
            elseif (j  .eq. i) then
               diag(i) = a(k)
            elseif (j .lt. i) then
               al(kl) = a(k)
               jal(kl) = j
               kl = kl+1
            endif
 71      continue
         ial(i+1) = kl
 7    continue
c
c readjust iau
c
      do 8 i=nrow,1,-1
         iau(i+1) = iau(i)
 8    continue
      iau(1) = 1
c--------------- end-of-csruss -----------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine usscsr (nrow,a,ja,ia,diag,al,jal,ial,au,jau,iau)
      integer, intent(in) :: nrow
      real*8 , intent(inout) :: a(*),al(*),diag(*),au(*)
      integer, intent(inout) :: ja(*),ia(nrow+1),jal(*),ial(nrow+1)
     * ,jau(*),iau(nrow+1)
      integer :: i,j,k,ka,jak
c-----------------------------------------------------------------------
c Unsymmetric Sparse Skyline   format   to Compressed Sparse Row
c-----------------------------------------------------------------------
c this subroutine converts a matrix stored in nonsymmetric sparse
c skyline format into csr format. The sparse skyline format is
c described in routine csruss.
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c On entry
c-----------------------------------------------------------------------
c nrow  = dimension of the matrix a.
c diag  = array containing the diagonal entries of A
c al,jal,ial = matrix in CSR format storing the strict lower
c              trangular part of A.
c au,jau,iau = matrix in CSC format storing the strict upper
c              trangular part of A.
c On return
c ---------
c a     = real array containing the nonzero values of the matrix
c         stored rowwise.
c ja    = column indices of the values in array a
c ia    = integer array of length n+1 containing the pointers to
c         beginning of each row in arrays a, ja.
c
c-----------------------------------------------------------------------
c
c count elements in lower part + diagonal
c
      do 1 i=1, nrow
         ia(i+1) = ial(i+1)-ial(i)+1
 1    continue
c
c count elements in upper part
c
      do 3 i=1, nrow
         do 2 k=iau(i), iau(i+1)-1
            j = jau(k)
            ia(j+1) = ia(j+1)+1
 2       continue
 3    continue
c---------- compute pointers from lengths ------------------------------
      ia(1) = 1
      do 4 i=1,nrow
         ia(i+1) = ia(i)+ia(i+1)
 4    continue
c
c copy lower part + diagonal
c
      do 6 i=1, nrow
         ka = ia(i)
         do 5 k=ial(i), ial(i+1)-1
            a(ka) = al(k)
            ja(ka) = jal(k)
            ka = ka+1
 5       continue
         a(ka) = diag(i)
         ja(ka) = i
         ia(i) = ka+1
 6    continue
c
c     copy upper part
c
      do 8 i=1, nrow
         do 7 k=iau(i), iau(i+1)-1
c
c row number
c
            jak = jau(k)
c
c where element goes
c
            ka = ia(jak)
            a(ka) = au(k)
            ja(ka) = i
            ia(jak) = ka+1
 7       continue
 8    continue
c
c readjust ia
c
      do 9 i=nrow,1,-1
         ia(i+1) = ia(i)
 9    continue
      ia(1) = 1
c----------end-of-usscsr------------------------------------------------
      end
c-----------------------------------------------------------------------
!      subroutine csrsss (nrow,a,ja,ia,sorted,diag,al,jal,ial,au)
!      real*8 a(*),al(*),diag(*),au(*)
!      integer ja(*),ia(nrow+1),jal(*),ial(nrow+1)
!      logical sorted
!c-----------------------------------------------------------------------
!c Compressed Sparse Row     to     Symmetric Sparse Skyline   format
!c-----------------------------------------------------------------------
!c this subroutine converts a matrix stored in csr format into the
!c Symmetric sparse skyline   format. This latter format assumes that
!c that the matrix has a symmetric pattern. It consists of the following
!c * the diagonal of A stored separately in diag(*);
!c * The strict lower part of A is stored  in csr format in al,jal,ial
!c * The values only of strict upper part as stored in csc format in au.
!c-----------------------------------------------------------------------
!c On entry
!c-----------
!c nrow  = dimension of the matrix a.
!c a     = real array containing the nonzero values of the matrix
!c         stored rowwise.
!c ja    = column indices of the values in array a
!c ia    = integer array of length n+1 containing the pointers to
!c         beginning of each row in arrays a, ja.
!c sorted= a logical indicating whether or not the elements in a,ja,ia
!c         are sorted.
!c
!c On return
!c ---------
!c diag  = array containing the diagonal entries of A
!c al,jal,ial = matrix in csr format storing the strict lower
!c              trangular part of A.
!c au    = values of the strict upper trangular part of A, column wise.
!c-----------------------------------------------------------------------
!c
!c     extract lower part and diagonal.
!c
!      kl = 1
!      ial(1) = kl
!      do  7 i=1, nrow
!c
!c scan all elements in a row
!c
!         do 71 k = ia(i), ia(i+1)-1
!            jak = ja(k)
!            if (jak  .eq. i) then
!               diag(i) = a(k)
!            elseif (jak .lt. i) then
!               al(kl) = a(k)
!               jal(kl) = jak
!               kl = kl+1
!            endif
! 71      continue
!         ial(i+1) = kl
! 7    continue
!c
!c sort if not sorted
!c
!      if (.not. sorted) then
!c%%%%%---- incompatible arg list!
!         call csort (nrow, al, jal, ial, au, .true.)
!      endif
!c
!c copy u
!c
!      do  8 i=1, nrow
!c
!c scan all elements in a row
!c
!         do 81 k = ia(i), ia(i+1)-1
!            jak = ja(k)
!            if (jak  .gt. i) then
!               ku = ial(jak)
!               au(ku) = a(k)
!               ial(jak) = ku+1
!            endif
! 81      continue
! 8    continue
!c
!c readjust ial
!c
!      do 9 i=nrow,1,-1
!         ial(i+1) = ial(i)
! 9    continue
!      ial(1) = 1
!c--------------- end-of-csrsss -----------------------------------------
!c-----------------------------------------------------------------------
!      end
!c
      subroutine ssscsr (nrow,a,ja,ia,diag,al,jal,ial,au)
      integer, intent(in) :: nrow
      real*8 , intent(inout) :: a(*),al(*),diag(*),au(*)
      integer, intent(inout) :: ja(*),ia(nrow+1),jal(*),ial(nrow+1)
      integer :: i,j,k,ka,jak
c-----------------------------------------------------------------------
c Unsymmetric Sparse Skyline   format   to Compressed Sparse Row
c-----------------------------------------------------------------------
c this subroutine converts a matrix stored in nonsymmetric sparse
c skyline format into csr format. The sparse skyline format is
c described in routine csruss.
c-----------------------------------------------------------------------
c On entry
c---------
c diag  = array containing the diagonal entries of A
c al,jal,ial = matrix in csr format storing the strict lower
c              trangular part of A.
c au    = values of strict upper part.
c
c On return
c ---------
c nrow  = dimension of the matrix a.
c a     = real array containing the nonzero values of the matrix
c         stored rowwise.
c ja    = column indices of the values in array a
c ia    = integer array of length n+1 containing the pointers to
c         beginning of each row in arrays a, ja.
c
c-----------------------------------------------------------------------
c
c count elements in lower part + diagonal
c
      do 1 i=1, nrow
         ia(i+1) = ial(i+1)-ial(i)+1
 1    continue
c
c count elements in upper part
c
      do 3 i=1, nrow
         do 2 k=ial(i), ial(i+1)-1
            j = jal(k)
            ia(j+1) = ia(j+1)+1
 2       continue
 3    continue
c---------- compute pointers from lengths ------------------------------
      ia(1) = 1
      do 4 i=1,nrow
         ia(i+1) = ia(i)+ia(i+1)
 4    continue
c
c copy lower part + diagonal
c
      do 6 i=1, nrow
         ka = ia(i)
         do 5 k=ial(i), ial(i+1)-1
            a(ka) = al(k)
            ja(ka) = jal(k)
            ka = ka+1
 5       continue
         a(ka) = diag(i)
         ia(i) = ka+1
 6    continue
c
c     copy upper part
c
      do 8 i=1, nrow
         do 7 k=ial(i), ial(i+1)-1
c
c row number
c
            jak = jal(k)
c
c where element goes
c
            ka = ia(jak)
            a(ka) = au(k)
            ja(ka) = i
            ia(jak) = ka+1
 7       continue
 8    continue
c
c readjust ia
c
      do 9 i=nrow,1,-1
         ia(i+1) = ia(i)
 9    continue
      ia(1) = 1
c----------end-of-ssscsr------------------------------------------------
      end
c-----------------------------------------------------------------------
!      subroutine csrvbr(n,ia,ja,a,nr,nc,kvstr,kvstc,ib,jb,kb,
!     &     b, job, iwk, nkmax, nzmax, ierr )
!c-----------------------------------------------------------------------
!      integer n, ia(n+1), ja(*), nr, nc, ib(*), jb(nkmax-1), kb(nkmax)
!      integer kvstr(*), kvstc(*), job, iwk(*), nkmax, nzmax, ierr
!      real*8  a(*), b(nzmax)
!c-----------------------------------------------------------------------
!c     Converts compressed sparse row to variable block row format.
!c-----------------------------------------------------------------------
!c     On entry:
!c--------------
!c     n       = number of matrix rows
!c     ia,ja,a = input matrix in CSR format
!c
!c     job     = job indicator.
!c               If job=0, kvstr and kvstc are used as supplied.
!c               If job=1, kvstr and kvstc are determined by the code.
!c               If job=2, a conformal row/col partitioning is found and
!c               returned in both kvstr and kvstc.  In the latter two cases,
!c               an optimized algorithm can be used to perform the
!c               conversion because all blocks are full.
!c
!c     nkmax   = size of supplied jb and kb arrays
!c     nzmax   = size of supplied b array
!c
!c     If job=0 then the following are input:
!c     nr,nc   = matrix block row and block column dimension
!c     kvstr   = first row number for each block row
!c     kvstc   = first column number for each block column.
!c               (kvstr and kvstc may be the same array)
!c
!c     On return:
!c---------------
!c
!c     ib,jb,kb,b = output matrix in VBR format
!c
!c     ierr    = error message
!c               ierr = 0 means normal return
!c               ierr = 1 out of space in jb and/or kb arrays
!c               ierr = 2 out of space in b array
!c               ierr = 3 nonsquare matrix used with job=2
!c
!c     If job=1,2 then the following are output:
!c     nr,nc   = matrix block row and block column dimension
!c     kvstr   = first row number for each block row
!c     kvstc   = first column number for each block column
!c               If job=2, then kvstr and kvstc contain the same info.
!c
!c     Work space:
!c----------------
!c     iwk(1:ncol) = inverse kvstc array.  If job=1,2 then we also need:
!c     iwk(ncol+1:ncol+nr) = used to help determine sparsity of each block row.
!c     The workspace is not assumed to be initialized to zero, nor is it
!c     left that way.
!c
!c     Algorithms:
!c----------------
!c     There are two conversion codes in this routine.  The first assumes
!c     that all blocks are full (there is a nonzero in the CSR data
!c     structure for each entry in the block), and is used if the routine
!c     determines the block partitioning itself.  The second code makes
!c     no assumptions about the block partitioning, and is used if the
!c     caller provides the partitioning.  The second code is much less
!c     efficient than the first code.
!c
!c     In the first code, the CSR data structure is traversed sequentially
!c     and entries are placed into the VBR data structure with stride
!c     equal to the row dimension of the block row.  The columns of the
!c     CSR data structure are sorted first if necessary.
!c
!c     In the second code, the block sparsity pattern is first determined.
!c     This is done by traversing the CSR data structure and using an
!c     implied linked list to determine which blocks are nonzero.  Then
!c     the VBR data structure is filled by mapping each individual entry
!c     in the CSR data structure into the VBR data structure.  The columns
!c     of the CSR data structure are sorted first if necessary.
!c
!c-----------------------------------------------------------------------
!c     Local variables:
!c---------------------
!      integer ncol, nb, neqr, numc, a0, b0, b1, k0, i, ii, j, jj, jnew
!      logical sorted
!c
!c     ncol = number of scalar columns in matrix
!c     nb = number of blocks in conformal row/col partitioning
!c     neqr = number of rows in block row
!c     numc = number of nonzero columns in row
!c     a0 = index for entries in CSR a array
!c     b0 = index for entries in VBR b array
!c     b1 = temp
!c     k0 = index for entries in VBR kb array
!c     i  = loop index for block rows
!c     ii = loop index for scalar rows in block row
!c     j  = loop index for block columns
!c     jj = loop index for scalar columns in block column
!c     jnew = block column number
!c     sorted = used to indicate if matrix already sorted by columns
!c
!c-----------------------------------------------------------------------
!      ierr = 0
!c-----sort matrix by column indices
!      call csorted(n, ia, ja, sorted)
!      if (.not. sorted) then
!         call csort (n, a, ja, ia, b, .true.)
!      endif
!      if (job .eq. 1 .or. job .eq. 2) then
!c--------need to zero workspace; first find ncol
!         ncol = 0
!         do i = 2, n
!            ncol = max0(ncol, ja(ia(i)-1))
!         enddo
!         do i = 1, ncol
!            iwk(i) = 0
!         enddo
!         call csrkvstr(n, ia, ja, nr, kvstr)
!         call csrkvstc(n, ia, ja, nc, kvstc, iwk)
!      endif
!c-----check if want conformal partitioning
!      if (job .eq. 2) then
!         if (kvstr(nr+1) .ne. kvstc(nc+1)) then
!            ierr = 3
!            return
!         endif
!c        use iwk temporarily
!         call kvstmerge(nr, kvstr, nc, kvstc, nb, iwk)
!         nr = nb
!         nc = nb
!         do i = 1, nb+1
!            kvstr(i) = iwk(i)
!            kvstc(i) = iwk(i)
!         enddo
!      endif
!c-----------------------------------------------------------------------
!c     inverse kvst (scalar col number) = block col number
!c     stored in iwk(1:n)
!c-----------------------------------------------------------------------
!      do i = 1, nc
!         do j = kvstc(i), kvstc(i+1)-1
!            iwk(j) = i
!         enddo
!      enddo
!      ncol = kvstc(nc+1)-1
!c-----jump to conversion routine
!      if (job .eq. 0) goto 400
!c-----------------------------------------------------------------------
!c     Fast conversion for computed block partitioning
!c-----------------------------------------------------------------------
!      a0 = 1
!      b0 = 1
!      k0 = 1
!      kb(1) = 1
!c-----loop on block rows
!      do i = 1, nr
!         neqr = kvstr(i+1) - kvstr(i)
!         numc = ia(kvstr(i)+1) - ia(kvstr(i))
!         ib(i) = k0
!c--------loop on first row in block row to determine block sparsity
!         j = 0
!         do jj = ia(kvstr(i)), ia(kvstr(i)+1)-1
!            jnew = iwk(ja(jj))
!            if (jnew .ne. j) then
!c--------------check there is enough space in kb and jb arrays
!               if (k0+1 .gt. nkmax) then
!                  ierr = 1
!                  write (*,*) 'csrvbr: no space in kb for block row ', i
!                  return
!               endif
!c--------------set entries for this block
!               j = jnew
!               b0 = b0 + neqr * (kvstc(j+1) - kvstc(j))
!               kb(k0+1) = b0
!               jb(k0) = j
!               k0 = k0 + 1
!            endif
!         enddo
!c--------loop on scalar rows in block row
!         do ii = 0, neqr-1
!            b1 = kb(ib(i))+ii
!c-----------loop on elements in a scalar row
!            do jj = 1, numc
!c--------------check there is enough space in b array
!               if (b1 .gt. nzmax) then
!                  ierr = 2
!                  write (*,*) 'csrvbr: no space in b for block row ', i
!                  return
!               endif
!               b(b1) = a(a0)
!               b1 = b1 + neqr
!               a0 = a0 + 1
!            enddo
!         enddo
!      enddo
!      ib(nr+1) = k0
!      return
!c-----------------------------------------------------------------------
!c     Conversion for user supplied block partitioning
!c-----------------------------------------------------------------------
! 400  continue
!c-----initialize workspace for sparsity indicator
!      do i = ncol+1, ncol+nc
!         iwk(i) = 0
!      enddo
!      k0 = 1
!      kb(1) = 1
!c-----find sparsity of block rows
!      do i = 1, nr
!         neqr = kvstr(i+1) - kvstr(i)
!         numc = ia(kvstr(i)+1) - ia(kvstr(i))
!         ib(i) = k0
!c--------loop on all the elements in the block row to determine block sparsity
!         do jj = ia(kvstr(i)), ia(kvstr(i+1))-1
!            iwk(iwk(ja(jj))+ncol) = 1
!         enddo
!c--------use sparsity to set jb and kb arrays
!         do j = 1, nc
!            if (iwk(j+ncol) .ne. 0) then
!c--------------check there is enough space in kb and jb arrays
!               if (k0+1 .gt. nkmax) then
!                  ierr = 1
!                  write (*,*) 'csrvbr: no space in kb for block row ', i
!                  return
!               endif
!               kb(k0+1) = kb(k0) + neqr * (kvstc(j+1) - kvstc(j))
!               jb(k0) = j
!               k0 = k0 + 1
!               iwk(j+ncol) = 0
!            endif
!         enddo
!      enddo
!      ib(nr+1) = k0
!c-----Fill b with entries from a by traversing VBR data structure.
!      a0 = 1
!c-----loop on block rows
!      do i = 1, nr
!         neqr = kvstr(i+1) - kvstr(i)
!c--------loop on scalar rows in block row
!         do ii = 0, neqr-1
!            b0 = kb(ib(i)) + ii
!c-----------loop on block columns
!            do j = ib(i), ib(i+1)-1
!c--------------loop on scalar columns within block column
!               do jj = kvstc(jb(j)), kvstc(jb(j)+1)-1
!c-----------------check there is enough space in b array
!                  if (b0 .gt. nzmax) then
!                     ierr = 2
!                     write (*,*)'csrvbr: no space in b for blk row',i
!                     return
!                  endif
!                  if (a0 .ge. ia(kvstr(i)+ii+1)) then
!                     b(b0) = 0.d0
!                  else
!                     if (jj .eq. ja(a0)) then
!                        b(b0) = a(a0)
!                        a0 = a0 + 1
!                     else
!                        b(b0) = 0.d0
!                     endif
!                  endif
!                  b0 = b0 + neqr
!c--------------endloop on scalar columns
!               enddo
!c-----------endloop on block columns
!            enddo
! 2020       continue
!         enddo
!      enddo
!      return
!      end
c-----------------------------------------------------------------------
c----------------------------end-of-csrvbr------------------------------
c----------------------------------------------------------------------c
      subroutine vbrcsr(ia, ja, a, nr, kvstr, kvstc, ib, jb, kb,
     &   b, nzmax, ierr)
c-----------------------------------------------------------------------
      integer ia(*), ja(*), nr, ib(nr+1), jb(*), kb(*)
      integer kvstr(nr+1), kvstc(*), nzmax, ierr
      real*8  a(*), b(nzmax)
c-----------------------------------------------------------------------
c     Converts variable block row to compressed sparse row format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     nr      = number of block rows
c     kvstr   = first row number for each block row
c     kvstc   = first column number for each block column
c     ib,jb,kb,b = input matrix in VBR format
c     nzmax   = size of supplied ja and a arrays
c
c     On return:
c---------------
c     ia,ja,a = output matrix in CSR format
c
c     ierr    = error message
c               ierr = 0 means normal return
c               ierr = negative row number when out of space in
c                      ja and a arrays
c
c     Work space:
c----------------
c     None
c
c     Algorithm:
c---------------
c     The VBR data structure is traversed in the order that is required
c     to fill the CSR data structure.  In a given block row, consecutive
c     entries in the CSR data structure are entries in the VBR data
c     structure with stride equal to the row dimension of the block.
c     The VBR data structure is assumed to be sorted by block columns.
c
c-----------------------------------------------------------------------
c     Local variables:
c---------------------
      integer neqr, numc, a0, b0, i, ii, j, jj
c
c     neqr = number of rows in block row
c     numc = number of nonzero columns in row
c     a0 = index for entries in CSR a array
c     b0 = index for entries in VBR b array
c     i  = loop index for block rows
c     ii = loop index for scalar rows in block row
c     j  = loop index for block columns
c     jj = loop index for scalar columns in block column
c
c-----------------------------------------------------------------------
      ierr = 0
      a0 = 1
      b0 = 1
c-----loop on block rows
      do i = 1, nr
c--------set num of rows in block row, and num of nonzero cols in row
         neqr = kvstr(i+1) - kvstr(i)
         numc = ( kb(ib(i+1)) - kb(ib(i)) ) / neqr
c--------construct ja for a scalar row
         do j = ib(i), ib(i+1)-1
            do jj = kvstc(jb(j)), kvstc(jb(j)+1)-1
               ja(a0) = jj
               a0 = a0 + 1
            enddo
         enddo
c--------construct neqr-1 additional copies of ja for the block row
         do ii = 1, neqr-1
            do j = 1, numc
               ja(a0) = ja(a0-numc)
               a0 = a0 + 1
            enddo
         enddo
c--------reset a0 back to beginning of block row
         a0 = kb(ib(i))
c--------loop on scalar rows in block row
         do ii = 0, neqr-1
            ia(kvstr(i)+ii) = a0
            b0 = kb(ib(i)) + ii
c-----------loop on elements in a scalar row
            do jj = 1, numc
c--------------check there is enough space in a array
               if (a0 .gt. nzmax) then
                  ierr = -(kvstr(i)+ii)
                  write (*,*) 'vbrcsr: no space for row ', -ierr
                  return
               endif
               a(a0) = b(b0)
               a0 = a0 + 1
               b0 = b0 + neqr
            enddo
         enddo
c-----endloop on block rows
      enddo
      ia(kvstr(nr+1)) = a0
      return
      end
c-----------------------------------------------------------------------
c---------------------------end-of-vbrcsr-------------------------------
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      subroutine csorted(n, ia, ja, sorted)
c-----------------------------------------------------------------------
      integer n, ia(n+1), ja(*)
      logical sorted
c-----------------------------------------------------------------------
c     Checks if matrix in CSR format is sorted by columns.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     n       = number of rows in matrix
c     ia, ja  = sparsity structure of matrix in CSR format
c
c     On return:
c---------------
c     sorted  = indicates if matrix is sorted by columns
c
c-----------------------------------------------------------------------
c-----local variables
      integer i,j
c---------------------------------
      do i = 1, n
         do j = ia(i)+1, ia(i+1)-1
            if (ja(j-1) .ge. ja(j)) then
               sorted = .false.
               return
            endif
         enddo
      enddo
      sorted = .true.
      return
      end
c-----------------------------------------------------------------------
c------------------------end-of-csorted---------------------------------
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                     UNARY SUBROUTINES MODULE                         c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c submat : extracts a submatrix from a sparse matrix.                  c
c filter : filters elements from a matrix according to their magnitude.c
c filterm: same as above, but for the MSR format                       c
c csort  : sorts the elements in increasing order of columns           c
c clncsr : clean up the CSR format matrix, remove duplicate entry, etc c
c transp : in-place transposition routine (see also csrcsc in formats) c
c copmat : copy of a matrix into another matrix (both stored csr)      c
c msrcop : copies a matrix in MSR format into a matrix in MSR format   c
c getelm : returns a(i,j) for any (i,j) from a CSR-stored matrix.      c
c getdia : extracts a specified diagonal from a matrix.                c
c getl   : extracts lower triangular part                              c
c getu   : extracts upper triangular part                              c
c levels : gets the level scheduling structure for lower triangular    c
c          matrices.                                                   c
c amask  : extracts     C = A mask M                                   c
c rperm  : permutes the rows of a matrix (B = P A)                     c
c cperm  : permutes the columns of a matrix (B = A Q)                  c
c dperm  : permutes both the rows and columns of a matrix (B = P A Q ) c
c dperm1 : general extractiob routine (extracts arbitrary rows)        c
c dperm2 : general submatrix permutation/extraction routine            c
c dmperm : symmetric permutation of row and column (B=PAP') in MSR fmt c
c dvperm : permutes a real vector (in-place)                           c
c ivperm : permutes an integer vector (in-place)                       c
c retmx  : returns the max absolute value in each row of the matrix    c
c diapos : returns the positions of the diagonal elements in A.        c
c extbdg : extracts the main diagonal blocks of a matrix.              c
c getbwd : returns the bandwidth information on a matrix.              c
c blkfnd : finds the block-size of a matrix.                           c
c blkchk : checks whether a given integer is the block size of A.      c
c infdia : obtains information on the diagonals of A.                  c
c amubdg : gets number of nonzeros in each row of A*B (as well as NNZ) c
c aplbdg : gets number of nonzeros in each row of A+B (as well as NNZ) c
c rnrms  : computes the norms of the rows of A                         c
c cnrms  : computes the norms of the columns of A                      c
c roscal : scales the rows of a matrix by their norms.                 c
c coscal : scales the columns of a matrix by their norms.              c
c addblk : Adds a matrix B into a block of A.                          c
c get1up : Collects the first elements of each row of the upper        c
c          triangular portion of the matrix.                           c
c xtrows : extracts given rows from a matrix in CSR format.            c
c csrkvstr:  Finds block row partitioning of matrix in CSR format      c
c csrkvstc:  Finds block column partitioning of matrix in CSR format   c
c kvstmerge: Merges block partitionings, for conformal row/col pattern c
c----------------------------------------------------------------------c
      subroutine submat (n,job,i1,i2,j1,j2,a,ja,ia,nr,nc,ao,jao,iao)
      integer, intent(in) :: n,job
      integer, intent(inout) :: i1,i2,j1,j2,nr,nc,ia(*),ja(*),jao(*)
     * ,iao(*)
      real*8 , intent(inout) :: a(*),ao(*)
      integer :: i, j, k, ii, k1,k2,klen
c-----------------------------------------------------------------------
c extracts the submatrix A(i1:i2,j1:j2) and puts the result in
c matrix ao,iao,jao
c---- In place: ao,jao,iao may be the same as a,ja,ia.
c--------------
c on input
c---------
c n   = row dimension of the matrix
c i1,i2 = two integers with i2 .ge. i1 indicating the range of rows to be
c          extracted.
c j1,j2 = two integers with j2 .ge. j1 indicating the range of columns
c         to be extracted.
c         * There is no checking whether the input values for i1, i2, j1,
c           j2 are between 1 and n.
c a,
c ja,
c ia    = matrix in compressed sparse row format.
c
c job = job indicator: if job .ne. 1 then the real values in a are NOT
c         extracted, only the column indices (i.e. data structure) are.
c         otherwise values as well as column indices are extracted...
c
c on output
c--------------
c nr  = number of rows of submatrix
c nc  = number of columns of submatrix
c       * if either of nr or nc is nonpositive the code will quit.
c
c ao,
c jao,iao = extracted matrix in general sparse format with jao containing
c     the column indices,and iao being the pointer to the beginning
c     of the row,in arrays a,ja.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      nr = i2-i1+1
      nc = j2-j1+1
c
      if ( nr .le. 0 .or. nc .le. 0) return
c
      klen = 0
c
c     simple procedure. proceeds row-wise...
c
      do 100 i = 1,nr
         ii = i1+i-1
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         iao(i) = klen+1
c-----------------------------------------------------------------------
         do 60 k=k1,k2
            j = ja(k)
            if (j .ge. j1 .and. j .le. j2) then
               klen = klen+1
               if (job .eq. 1) ao(klen) = a(k)
               jao(klen) = j - j1+1
            endif
 60      continue
 100  continue
      iao(nr+1) = klen+1
      return
c------------end-of submat----------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine filter(n,job,drptol,a,ja,ia,b,jb,ib,len,ierr)
      real*8 a(*),b(*),drptol
      integer ja(*),jb(*),ia(*),ib(*),n,job,len,ierr
c-----------------------------------------------------------------------
c     This module removes any elements whose absolute value
c     is small from an input matrix A and puts the resulting
c     matrix in B.  The input parameter job selects a definition
c     of small.
c-----------------------------------------------------------------------
c on entry:
c---------
c  n   = integer. row dimension of matrix
c  job   = integer. used to determine strategy chosen by caller to
c         drop elements from matrix A.
c          job = 1
c              Elements whose absolute value is less than the
c              drop tolerance are removed.
c          job = 2
c              Elements whose absolute value is less than the
c              product of the drop tolerance and the Euclidean
c              norm of the row are removed.
c          job = 3
c              Elements whose absolute value is less that the
c              product of the drop tolerance and the largest
c              element in the row are removed.
c
c drptol = real. drop tolerance used for dropping strategy.
c a
c ja
c ia     = input matrix in compressed sparse format
c len  = integer. the amount of space available in arrays b and jb.
c
c on return:
c----------
c b
c jb
c ib    = resulting matrix in compressed sparse format.
c
c ierr   = integer. containing error message.
c         ierr .eq. 0 indicates normal return
c         ierr .gt. 0 indicates that there is'nt enough
c         space is a and ja to store the resulting matrix.
c         ierr then contains the row number where filter stopped.
c note:
c------ This module is in place. (b,jb,ib can ne the same as
c       a, ja, ia in which case the result will be overwritten).
c----------------------------------------------------------------------c
c           contributed by David Day,  Sep 19, 1989.                   c
c----------------------------------------------------------------------c
c local variables
      real*8 norm,loctol
      integer index,row,k,k1,k2
c
      index = 1
      do 10 row= 1,n
         k1 = ia(row)
         k2 = ia(row+1) - 1
         ib(row) = index
       goto (100,200,300) job
 100     norm = 1.0d0
         goto 400
 200     norm = 0.0d0
         do 22 k = k1,k2
            norm = norm + a(k) * a(k)
 22      continue
         norm = sqrt(norm)
         goto 400
 300     norm = 0.0d0
         do 23 k = k1,k2
            if( abs(a(k))  .gt. norm) then
               norm = abs(a(k))
            endif
 23      continue
 400     loctol = drptol * norm
       do 30 k = k1,k2
          if( abs(a(k)) .gt. loctol)then
               if (index .gt. len) then
               ierr = row
               return
            endif
            b(index) =  a(k)
            jb(index) = ja(k)
            index = index + 1
         endif
 30   continue
 10   continue
      ib(n+1) = index
      return
c--------------------end-of-filter -------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine filterm (n,job,drop,a,ja,b,jb,len,ierr)
      real*8 a(*),b(*),drop
      integer ja(*),jb(*),n,job,len,ierr
c-----------------------------------------------------------------------
c     This subroutine removes any elements whose absolute value
c     is small from an input matrix A. Same as filter but
c     uses the MSR format.
c-----------------------------------------------------------------------
c on entry:
c---------
c  n   = integer. row dimension of matrix
c  job   = integer. used to determine strategy chosen by caller to
c         drop elements from matrix A.
c          job = 1
c              Elements whose absolute value is less than the
c              drop tolerance are removed.
c          job = 2
c              Elements whose absolute value is less than the
c              product of the drop tolerance and the Euclidean
c              norm of the row are removed.
c          job = 3
c              Elements whose absolute value is less that the
c              product of the drop tolerance and the largest
c              element in the row are removed.
c
c drop = real. drop tolerance used for dropping strategy.
c a
c ja     = input matrix in Modifief Sparse Row format
c len  = integer. the amount of space in arrays b and jb.
c
c on return:
c----------
c
c b, jb = resulting matrix in Modifief Sparse Row format
c
c ierr   = integer. containing error message.
c         ierr .eq. 0 indicates normal return
c         ierr .gt. 0 indicates that there is'nt enough
c         space is a and ja to store the resulting matrix.
c         ierr then contains the row number where filter stopped.
c note:
c------ This module is in place. (b,jb can ne the same as
c       a, ja in which case the result will be overwritten).
c----------------------------------------------------------------------c
c           contributed by David Day,  Sep 19, 1989.                   c
c----------------------------------------------------------------------c
c local variables
c
      real*8 norm,loctol
      integer index,row,k,k1,k2
c
      index = n+2
      do 10 row= 1,n
         k1 = ja(row)
         k2 = ja(row+1) - 1
         jb(row) = index
       goto (100,200,300) job
 100     norm = 1.0d0
         goto 400
 200     norm = a(row)**2
         do 22 k = k1,k2
            norm = norm + a(k) * a(k)
 22      continue
         norm = sqrt(norm)
         goto 400
 300     norm = abs(a(row))
         do 23 k = k1,k2
            norm = max(abs(a(k)),norm)
 23      continue
 400     loctol = drop * norm
       do 30 k = k1,k2
          if( abs(a(k)) .gt. loctol)then
               if (index .gt. len) then
                  ierr = row
                  return
               endif
               b(index) =  a(k)
               jb(index) = ja(k)
               index = index + 1
            endif
 30      continue
 10   continue
      jb(n+1) = index
      return
c--------------------end-of-filterm-------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csort (n,a,ja,ia,iwork,values)
      logical values
      integer n, ja(*), ia(n+1), iwork(*)
      real*8 a(*)
c-----------------------------------------------------------------------
c This routine sorts the elements of  a matrix (stored in Compressed
c Sparse Row Format) in increasing order of their column indices within
c each row. It uses a form of bucket sort with a cost of O(nnz) where
c nnz = number of nonzero elements.
c requires an integer work array of length 2*nnz.
c-----------------------------------------------------------------------
c on entry:
c---------
c n     = the row dimension of the matrix
c a     = the matrix A in compressed sparse row format.
c ja    = the array of column indices of the elements in array a.
c ia    = the array of pointers to the rows.
c iwork = integer work array of length max ( n+1, 2*nnz )
c         where nnz = (ia(n+1)-ia(1))  ) .
c values= logical indicating whether or not the real values a(*) must
c         also be permuted. if (.not. values) then the array a is not
c         touched by csort and can be a dummy array.
c
c on return:
c----------
c the matrix stored in the structure a, ja, ia is permuted in such a
c way that the column indices are in increasing order within each row.
c iwork(1:nnz) contains the permutation used  to rearrange the elements.
c-----------------------------------------------------------------------
c Y. Saad - Feb. 1, 1991.
c-----------------------------------------------------------------------
c local variables
      integer i, k, j, ifirst, nnz, next, irow, ko
c
c count the number of elements in each column
c
      do 1 i=1,n+1
         iwork(i) = 0
 1    continue
      do 3 i=1, n
         do 2 k=ia(i), ia(i+1)-1
            j = ja(k)+1
            iwork(j) = iwork(j)+1
 2       continue
 3    continue
c
c compute pointers from lengths.
c
      iwork(1) = 1
      do 4 i=1,n
         iwork(i+1) = iwork(i) + iwork(i+1)
 4    continue
c
c get the positions of the nonzero elements in order of columns.
c
      ifirst = ia(1)
      nnz = ia(n+1)-ifirst
      do 5 i=1,n
         do 51 k=ia(i),ia(i+1)-1
            j = ja(k)
            next = iwork(j)
            iwork(nnz+next) = k
            iwork(j) = next+1
 51      continue
 5    continue
c
c convert to coordinate format
c
      do 6 i=1, n
         do 61 k=ia(i), ia(i+1)-1
            iwork(k) = i
 61      continue
 6    continue
c
c loop to find permutation: for each element find the correct
c position in (sorted) arrays a, ja. Record this in iwork.
c
      do 7 k=1, nnz
         ko = iwork(nnz+k)
         irow = iwork(ko)
         next = ia(irow)
c
c the current element should go in next position in row. iwork
c records this position.
c
         iwork(ko) = next
         ia(irow)  = next+1
 7       continue
c
c perform an in-place permutation of the  arrays.
c
         call ivperm (nnz, ja(ifirst), iwork)
         if (values) call dvperm (nnz, a(ifirst), iwork)
c
c reshift the pointers of the original matrix back.
c
      do 8 i=n,1,-1
         ia(i+1) = ia(i)
 8    continue
      ia(1) = ifirst
c
      return
c---------------end-of-csort--------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine clncsr(job,value2,nrow,a,ja,ia,indu,iwk)
c     .. Scalar Arguments ..
      integer job, nrow, value2
c     ..
c     .. Array Arguments ..
      integer ia(nrow+1),indu(nrow),iwk(nrow+1),ja(*)
      real*8  a(*)
c     ..
c
c     This routine performs two tasks to clean up a CSR matrix
c     -- remove duplicate/zero entries,
c     -- perform a partial ordering, new order lower triangular part,
c        main diagonal, upper triangular part.
c
c     On entry:
c
c     job   = options
c         0 -- nothing is done
c         1 -- eliminate duplicate entries, zero entries.
c         2 -- eliminate duplicate entries and perform partial ordering.
c         3 -- eliminate duplicate entries, sort the entries in the
c              increasing order of clumn indices.
c
c     value2  -- 0 the matrix is pattern only (a is not touched)
c                1 matrix has values too.
c     nrow    -- row dimension of the matrix
c     a,ja,ia -- input matrix in CSR format
c
c     On return:
c     a,ja,ia -- cleaned matrix
c     indu    -- pointers to the beginning of the upper triangular
c                portion if job > 1
c
c     Work space:
c     iwk     -- integer work space of size nrow+1
c
c     .. Local Scalars ..
      integer i,j,k,ko,ipos,kfirst,klast
      real*8  tmp
c     ..
c
      if (job.le.0) return
c
c     .. eliminate duplicate entries --
c     array INDU is used as marker for existing indices, it is also the
c     location of the entry.
c     IWK is used to stored the old IA array.
c     matrix is copied to squeeze out the space taken by the duplicated
c     entries.
c
      do 90 i = 1, nrow
         indu(i) = 0
         iwk(i) = ia(i)
 90   continue
      iwk(nrow+1) = ia(nrow+1)
      k = 1
      do 120 i = 1, nrow
         ia(i) = k
         ipos = iwk(i)
         klast = iwk(i+1)
 100     if (ipos.lt.klast) then
            j = ja(ipos)
            if (indu(j).eq.0) then
c     .. new entry ..
               if (value2.ne.0) then
                  if (a(ipos) .ne. 0.0D0) then
                     indu(j) = k
                     ja(k) = ja(ipos)
                     a(k) = a(ipos)
                     k = k + 1
                  endif
               else
                  indu(j) = k
                  ja(k) = ja(ipos)
                  k = k + 1
               endif
            else if (value2.ne.0) then
c     .. duplicate entry ..
               a(indu(j)) = a(indu(j)) + a(ipos)
            endif
            ipos = ipos + 1
            go to 100
         endif
c     .. remove marks before working on the next row ..
         do 110 ipos = ia(i), k - 1
            indu(ja(ipos)) = 0
 110     continue
 120  continue
      ia(nrow+1) = k
      if (job.le.1) return
c
c     .. partial ordering ..
c     split the matrix into strict upper/lower triangular
c     parts, INDU points to the the beginning of the upper part.
c
      do 140 i = 1, nrow
         klast = ia(i+1) - 1
         kfirst = ia(i)
 130     if (klast.gt.kfirst) then
            if (ja(klast).lt.i .and. ja(kfirst).ge.i) then
c     .. swap klast with kfirst ..
               j = ja(klast)
               ja(klast) = ja(kfirst)
               ja(kfirst) = j
               if (value2.ne.0) then
                  tmp = a(klast)
                  a(klast) = a(kfirst)
                  a(kfirst) = tmp
               endif
            endif
            if (ja(klast).ge.i)
     &         klast = klast - 1
            if (ja(kfirst).lt.i)
     &         kfirst = kfirst + 1
            go to 130
         endif
c
         if (ja(klast).lt.i) then
            indu(i) = klast + 1
         else
            indu(i) = klast
         endif
 140  continue
      if (job.le.2) return
c
c     .. order the entries according to column indices
c     burble-sort is used
c
      do 190 i = 1, nrow
         do 160 ipos = ia(i), indu(i)-1
            do 150 j = indu(i)-1, ipos+1, -1
               k = j - 1
               if (ja(k).gt.ja(j)) then
                  ko = ja(k)
                  ja(k) = ja(j)
                  ja(j) = ko
                  if (value2.ne.0) then
                     tmp = a(k)
                     a(k) = a(j)
                     a(j) = tmp
                  endif
               endif
 150        continue
 160     continue
         do 180 ipos = indu(i), ia(i+1)-1
            do 170 j = ia(i+1)-1, ipos+1, -1
               k = j - 1
               if (ja(k).gt.ja(j)) then
                  ko = ja(k)
                  ja(k) = ja(j)
                  ja(j) = ko
                  if (value2.ne.0) then
                     tmp = a(k)
                     a(k) = a(j)
                     a(j) = tmp
                  endif
               endif
 170        continue
 180     continue
 190  continue
      return
c---- end of clncsr ----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine copmat (nrow,a,ja,ia,ao,jao,iao,ipos,job)
      real*8 a(*),ao(*)
      integer nrow, ia(*),ja(*),jao(*),iao(*), ipos, job
c----------------------------------------------------------------------
c copies the matrix a, ja, ia, into the matrix ao, jao, iao.
c----------------------------------------------------------------------
c on entry:
c---------
c nrow   = row dimension of the matrix
c a,
c ja,
c ia    = input matrix in compressed sparse row format.
c ipos  = integer. indicates the position in the array ao, jao
c         where the first element should be copied. Thus
c         iao(1) = ipos on return.
c job   = job indicator. if (job .ne. 1) the values are not copies
c         (i.e., pattern only is copied in the form of arrays ja, ia).
c
c on return:
c----------
c ao,
c jao,
c iao   = output matrix containing the same data as a, ja, ia.
c-----------------------------------------------------------------------
c           Y. Saad, March 1990.
c-----------------------------------------------------------------------
c local variables
      integer kst, i, k
c
      kst    = ipos -ia(1)
      do 100 i = 1, nrow+1
         iao(i) = ia(i) + kst
 100  continue
c
      do 200 k=ia(1), ia(nrow+1)-1
         jao(kst+k)= ja(k)
 200  continue
c
      if (job .ne. 1) return
      do 201 k=ia(1), ia(nrow+1)-1
         ao(kst+k) = a(k)
 201  continue
c
      return
c--------end-of-copmat -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine msrcop (nrow,a,ja,ao,jao,job)
      real*8 a(*),ao(*)
      integer nrow, ja(*),jao(*), job
c----------------------------------------------------------------------
c copies the MSR matrix a, ja, into the MSR matrix ao, jao
c----------------------------------------------------------------------
c on entry:
c---------
c nrow   = row dimension of the matrix
c a,ja  = input matrix in Modified compressed sparse row format.
c job   = job indicator. Values are not copied if job .ne. 1
c
c on return:
c----------
c ao, jao   = output matrix containing the same data as a, ja.
c-----------------------------------------------------------------------
c           Y. Saad,
c-----------------------------------------------------------------------
c local variables
      integer i, k
c
      do 100 i = 1, nrow+1
         jao(i) = ja(i)
 100  continue
c
      do 200 k=ja(1), ja(nrow+1)-1
         jao(k)= ja(k)
 200  continue
c
      if (job .ne. 1) return
      do 201 k=ja(1), ja(nrow+1)-1
         ao(k) = a(k)
 201  continue
      do 202 k=1,nrow
         ao(k) = a(k)
 202  continue
c
      return
c--------end-of-msrcop -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      double precision function getelm (i,j,a,ja,ia,iadd,sorted)
c-----------------------------------------------------------------------
c     purpose:
c     --------
c     this function returns the element a(i,j) of a matrix a,
c     for any pair (i,j).  the matrix is assumed to be stored
c     in compressed sparse row (csr) format. getelm performs a
c     binary search in the case where it is known that the elements
c     are sorted so that the column indices are in increasing order.
c     also returns (in iadd) the address of the element a(i,j) in
c     arrays a and ja when the search is successsful (zero if not).
c-----
c     first contributed by noel nachtigal (mit).
c     recoded jan. 20, 1991, by y. saad [in particular
c     added handling of the non-sorted case + the iadd output]
c-----------------------------------------------------------------------
c     parameters:
c     -----------
c on entry:
c----------
c     i      = the row index of the element sought (input).
c     j      = the column index of the element sought (input).
c     a      = the matrix a in compressed sparse row format (input).
c     ja     = the array of column indices (input).
c     ia     = the array of pointers to the rows' data (input).
c     sorted = logical indicating whether the matrix is knonw to
c              have its column indices sorted in increasing order
c              (sorted=.true.) or not (sorted=.false.).
c              (input).
c on return:
c-----------
c     getelm = value of a(i,j).
c     iadd   = address of element a(i,j) in arrays a, ja if found,
c              zero if not found. (output)
c
c     note: the inputs i and j are not checked for validity.
c-----------------------------------------------------------------------
c     noel m. nachtigal october 28, 1990 -- youcef saad jan 20, 1991.
c-----------------------------------------------------------------------
      integer i, ia(*), iadd, j, ja(*)
      double precision a(*)
      logical sorted
c
c     local variables.
c
      integer ibeg, iend, imid, k
c
c     initialization
c
      iadd = 0
      getelm = 0.0
      ibeg = ia(i)
      iend = ia(i+1)-1
c
c     case where matrix is not necessarily sorted
c
      if (.not. sorted) then
c
c scan the row - exit as soon as a(i,j) is found
c
         do 5  k=ibeg, iend
            if (ja(k) .eq.  j) then
               iadd = k
               goto 20
            endif
 5       continue
c
c     end unsorted case. begin sorted case
c
      else
c
c     begin binary search.   compute the middle index.
c
 10      imid = ( ibeg + iend ) / 2
c
c     test if  found
c
         if (ja(imid).eq.j) then
            iadd = imid
            goto 20
         endif
         if (ibeg .ge. iend) goto 20
c
c     else     update the interval bounds.
c
         if (ja(imid).gt.j) then
            iend = imid -1
         else
            ibeg = imid +1
         endif
         goto 10
c
c     end both cases
c
      endif
c
 20   if (iadd .ne. 0) getelm = a(iadd)
c
      return
c--------end-of-getelm--------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine getdia (nrow,ncol,job,a,ja,ia,len,diag,idiag,ioff)
      real*8 diag(*),a(*)
      integer nrow, ncol, job, len, ioff, ia(*), ja(*), idiag(*)
c-----------------------------------------------------------------------
c this subroutine extracts a given diagonal from a matrix stored in csr
c format. the output matrix may be transformed with the diagonal removed
c from it if desired (as indicated by job.)
c-----------------------------------------------------------------------
c our definition of a diagonal of matrix is a vector of length nrow
c (always) which contains the elements in rows 1 to nrow of
c the matrix that are contained in the diagonal offset by ioff
c with respect to the main diagonal. if the diagonal element
c falls outside the matrix then it is defined as a zero entry.
c thus the proper definition of diag(*) with offset ioff is
c
c     diag(i) = a(i,ioff+i) i=1,2,...,nrow
c     with elements falling outside the matrix being defined as zero.
c
c-----------------------------------------------------------------------
c
c on entry:
c----------
c
c nrow   = integer. the row dimension of the matrix a.
c ncol   = integer. the column dimension of the matrix a.
c job   = integer. job indicator.  if job = 0 then
c         the matrix a, ja, ia, is not altered on return.
c         if job.ne.0  then getdia will remove the entries
c         collected in diag from the original matrix.
c         this is done in place.
c
c a,ja,
c    ia = matrix stored in compressed sparse row a,ja,ia,format
c ioff  = integer,containing the offset of the wanted diagonal
c       the diagonal extracted is the one corresponding to the
c       entries a(i,j) with j-i = ioff.
c       thus ioff = 0 means the main diagonal
c
c on return:
c-----------
c len   = number of nonzero elements found in diag.
c         (len .le. min(nrow,ncol-ioff)-max(1,1-ioff) + 1 )
c
c diag  = real*8 array of length nrow containing the wanted diagonal.
c       diag contains the diagonal (a(i,j),j-i = ioff ) as defined
c         above.
c
c idiag = integer array of  length len, containing the poisitions
c         in the original arrays a and ja of the diagonal elements
c         collected in diag. a zero entry in idiag(i) means that
c         there was no entry found in row i belonging to the diagonal.
c
c a, ja,
c    ia = if job .ne. 0 the matrix is unchanged. otherwise the nonzero
c         diagonal entries collected in diag are removed from the
c         matrix and therefore the arrays a, ja, ia will change.
c       (the matrix a, ja, ia will contain len fewer elements)
c
c----------------------------------------------------------------------c
c     Y. Saad, sep. 21 1989 - modified and retested Feb 17, 1996.      c
c----------------------------------------------------------------------c
c     local variables
      integer istart, max, iend, i, kold, k, kdiag, ko
c
      istart = max(0,-ioff)
      iend = min(nrow,ncol-ioff)
      len = 0
      do 1 i=1,nrow
         idiag(i) = 0
       diag(i) = 0.0d0
 1    continue
c
c     extract  diagonal elements
c
      do 6 i=istart+1, iend
         do 51 k= ia(i),ia(i+1) -1
            if (ja(k)-i .eq. ioff) then
               diag(i)= a(k)
               idiag(i) = k
               len = len+1
               goto 6
            endif
 51      continue
 6    continue
      if (job .eq. 0 .or. len .eq.0) return
c
c     remove diagonal elements and rewind structure
c
      ko = 0
      do  7 i=1, nrow
         kold = ko
         kdiag = idiag(i)
         do 71 k= ia(i), ia(i+1)-1
            if (k .ne. kdiag) then
               ko = ko+1
               a(ko) = a(k)
               ja(ko) = ja(k)
            endif
 71      continue
         ia(i) = kold+1
 7    continue
c
c     redefine ia(nrow+1)
c
      ia(nrow+1) = ko+1
      return
c------------end-of-getdia----------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine transp (nrow,ncol,a,ja,ia,iwk,ierr)
      integer, intent(in) :: nrow
      integer, intent(out) :: ierr
      integer, intent(inout) :: ia(*), ja(*), iwk(*), ncol
      real*8 , intent(inout) :: a(*)
c------------------------------------------------------------------------
c In-place transposition routine.
c------------------------------------------------------------------------
c this subroutine transposes a matrix stored in compressed sparse row
c format. the transposition is done in place in that the arrays a,ja,ia
c of the transpose are overwritten onto the original arrays.
c------------------------------------------------------------------------
c on entry:
c---------
c nrow   = integer. The row dimension of A.
c ncol   = integer. The column dimension of A.
c a   = real array of size nnz (number of nonzero elements in A).
c         containing the nonzero elements
c ja  = integer array of length nnz containing the column positions
c       of the corresponding elements in a.
c ia  = integer of size n+1, where n = max(nrow,ncol). On entry
c         ia(k) contains the position in a,ja of  the beginning of
c         the k-th row.
c
c iwk = integer work array of same length as ja.
c
c on return:
c----------
c
c ncol   = actual row dimension of the transpose of the input matrix.
c         Note that this may be .le. the input value for ncol, in
c         case some of the last columns of the input matrix are zero
c         columns. In the case where the actual number of rows found
c         in transp(A) exceeds the input value of ncol, transp will
c         return without completing the transposition. see ierr.
c a,
c ja,
c ia  = contains the transposed matrix in compressed sparse
c         row format. The row dimension of a, ja, ia is now ncol.
c
c ierr   = integer. error message. If the number of rows for the
c         transposed matrix exceeds the input value of ncol,
c         then ierr is  set to that number and transp quits.
c         Otherwise ierr is set to 0 (normal return).
c
c Note:
c----- 1) If you do not need the transposition to be done in place
c         it is preferrable to use the conversion routine csrcsc
c         (see conversion routines in formats).
c      2) the entries of the output matrix are not sorted (the column
c         indices in each are not in increasing order) use csrcsc
c         if you want them sorted.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c  modified Oct. 11, 1989.                                             c
c----------------------------------------------------------------------c
c local variables
      real*8  :: t, t1
      integer :: i,j,k,l,init,inext,jcol,nnz

      ierr = 0
      nnz = ia(nrow+1)-1
c
c     determine column dimension
c
      jcol = 0
      do 1 k=1, nnz
         jcol = max(jcol,ja(k))
 1    continue
      if (jcol .gt. ncol) then
         ierr = jcol
         return
      endif
c
c     convert to coordinate format. use iwk for row indices.
c
      ncol = jcol
c
      do 3 i=1,nrow
         do 2 k=ia(i),ia(i+1)-1
            iwk(k) = i
 2       continue
 3    continue
c     find pointer array for transpose.
      do 35 i=1,ncol+1
         ia(i) = 0
 35   continue
      do 4 k=1,nnz
         i = ja(k)
         ia(i+1) = ia(i+1)+1
 4    continue
      ia(1) = 1
c------------------------------------------------------------------------
      do 44 i=1,ncol
         ia(i+1) = ia(i) + ia(i+1)
 44   continue
c
c     loop for a cycle in chasing process.
c
      init = 1
      k = 0
 5    t = a(init)
      i = ja(init)
      j = iwk(init)
      iwk(init) = -1
c------------------------------------------------------------------------
 6    k = k+1
c     current row number is i.  determine  where to go.
      l = ia(i)
c     save the chased element.
      t1 = a(l)
      inext = ja(l)
c     then occupy its location.
      a(l)  = t
      ja(l) = j
c     update pointer information for next element to be put in row i.
      ia(i) = l+1
c     determine  next element to be chased
      if (iwk(l) .lt. 0) goto 65
      t = t1
      i = inext
      j = iwk(l)
      iwk(l) = -1
      if (k .lt. nnz) goto 6
      goto 70
 65   init = init+1
      if (init .gt. nnz) goto 70
      if (iwk(init) .lt. 0) goto 65
c     restart chasing --
      goto 5
 70   continue
      do 80 i=ncol,1,-1
         ia(i+1) = ia(i)
 80   continue
      ia(1) = 1
c
      return
c------------------end-of-transp ----------------------------------------
c------------------------------------------------------------------------
      end
c------------------------------------------------------------------------
      subroutine getl (n,a,ja,ia,ao,jao,iao)
      integer n, ia(*), ja(*), iao(*), jao(*)
      real*8 a(*), ao(*)
c------------------------------------------------------------------------
c this subroutine extracts the lower triangular part of a matrix
c and writes the result ao, jao, iao. The routine is in place in
c that ao, jao, iao can be the same as a, ja, ia if desired.
c-----------
c on input:
c
c n     = dimension of the matrix a.
c a, ja,
c    ia = matrix stored in compressed sparse row format.
c On return:
c ao, jao,
c    iao = lower triangular matrix (lower part of a)
c     stored in a, ja, ia, format
c note: the diagonal element is the last element in each row.
c i.e. in  a(ia(i+1)-1 )
c ao, jao, iao may be the same as a, ja, ia on entry -- in which case
c getl will overwrite the result on a, ja, ia.
c
c------------------------------------------------------------------------
c local variables
      real*8 t
      integer ko, kold, kdiag, k, i
c
c inititialize ko (pointer for output matrix)
c
      ko = 0
      do  7 i=1, n
         kold = ko
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .gt. i) goto 71
            ko = ko+1
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
         if (kdiag .eq. 0 .or. kdiag .eq. ko) goto 72
c
c     exchange
c
         t = ao(kdiag)
         ao(kdiag) = ao(ko)
         ao(ko) = t
c
         k = jao(kdiag)
         jao(kdiag) = jao(ko)
         jao(ko) = k
 72      iao(i) = kold+1
 7    continue
c     redefine iao(n+1)
      iao(n+1) = ko+1
      return
c----------end-of-getl -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine getu (n,a,ja,ia,ao,jao,iao)
      integer n, ia(*), ja(*), iao(*), jao(*)
      real*8 a(*), ao(*)
c------------------------------------------------------------------------
c this subroutine extracts the upper triangular part of a matrix
c and writes the result ao, jao, iao. The routine is in place in
c that ao, jao, iao can be the same as a, ja, ia if desired.
c-----------
c on input:
c
c n     = dimension of the matrix a.
c a, ja,
c    ia = matrix stored in a, ja, ia, format
c On return:
c ao, jao,
c    iao = upper triangular matrix (upper part of a)
c     stored in compressed sparse row format
c note: the diagonal element is the last element in each row.
c i.e. in  a(ia(i+1)-1 )
c ao, jao, iao may be the same as a, ja, ia on entry -- in which case
c getu will overwrite the result on a, ja, ia.
c
c------------------------------------------------------------------------
c local variables
      real*8 t
      integer ko, k, i, kdiag, kfirst
      ko = 0
      do  7 i=1, n
         kfirst = ko+1
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .lt. i) goto 71
            ko = ko+1
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
         if (kdiag .eq. 0 .or. kdiag .eq. kfirst) goto 72
c     exchange
         t = ao(kdiag)
         ao(kdiag) = ao(kfirst)
         ao(kfirst) = t
c
         k = jao(kdiag)
         jao(kdiag) = jao(kfirst)
         jao(kfirst) = k
 72      iao(i) = kfirst
 7    continue
c     redefine iao(n+1)
      iao(n+1) = ko+1
      return
c----------end-of-getu -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine levels (n, jal, ial, nlev, lev, ilev, levnum)
      integer, intent(in) :: n
      integer, intent(out) :: nlev
      integer, intent(inout) ::jal(*),ial(*), levnum(*), ilev(*), lev(*)
      integer :: i, j, levi
c-----------------------------------------------------------------------
c levels gets the level structure of a lower triangular matrix
c for level scheduling in the parallel solution of triangular systems
c strict lower matrices (e.g. unit) as well matrices with their main
c diagonal are accepted.
c-----------------------------------------------------------------------
c on entry:
c----------
c n        = integer. The row dimension of the matrix
c jal, ial =
c
c on return:
c-----------
c nlev     = integer. number of levels found
c lev      = integer array of length n containing the level
c            scheduling permutation.
c ilev     = integer array. pointer to beginning of levels in lev.
c            the numbers lev(i) to lev(i+1)-1 contain the row numbers
c            that belong to level number i, in the level scheduling
c            ordering. The equations of the same level can be solved
c            in parallel, once those of all the previous levels have
c            been solved.
c work arrays:
c-------------
c levnum   = integer array of length n (containing the level numbers
c            of each unknown on return)
c-----------------------------------------------------------------------
      do 10 i = 1, n
         levnum(i) = 0
 10   continue
c
c     compute level of each node --
c
      nlev = 0
      do 20 i = 1, n
         levi = 0
         do 15 j = ial(i), ial(i+1) - 1
            levi = max (levi, levnum(jal(j)))
 15      continue
         levi = levi+1
         levnum(i) = levi
         nlev = max(nlev,levi)
 20   continue
c-------------set data structure  --------------------------------------
      do 21 j=1, nlev+1
         ilev(j) = 0
 21   continue
c------count  number   of elements in each level -----------------------
      do 22 j=1, n
         i = levnum(j)+1
         ilev(i) = ilev(i)+1
 22   continue
c---- set up pointer for  each  level ----------------------------------
      ilev(1) = 1
      do 23 j=1, nlev
         ilev(j+1) = ilev(j)+ilev(j+1)
 23   continue
c-----determine elements of each level --------------------------------
      do 30 j=1,n
         i = levnum(j)
         lev(ilev(i)) = j
         ilev(i) = ilev(i)+1
 30   continue
c     reset pointers backwards
      do 35 j=nlev, 1, -1
         ilev(j+1) = ilev(j)
 35   continue
      ilev(1) = 1
      return
c----------end-of-levels------------------------------------------------
C-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine amask (nrow,ncol,a,ja,ia,jmask,imask,
     *                  c,jc,ic,iw,nzmax,ierr)
c---------------------------------------------------------------------
      integer, intent(in) :: nrow, ncol, nzmax
      integer, intent(out) :: ierr
      real*8 , intent(inout) :: a(*),c(*)
      integer, intent(inout) :: ia(nrow+1),ja(*),jc(*),ic(nrow+1)
     * ,jmask(*),imask(nrow+1)
      logical, intent(inout) :: iw(ncol)
      integer :: k, k1,k2,len,ii,j
c-----------------------------------------------------------------------
c This subroutine builds a sparse matrix from an input matrix by
c extracting only elements in positions defined by the mask jmask, imask
c-----------------------------------------------------------------------
c On entry:
c---------
c nrow  = integer. row dimension of input matrix
c ncol   = integer. Column dimension of input matrix.
c
c a,
c ja,
c ia  = matrix in Compressed Sparse Row format
c
c jmask,
c imask = matrix defining mask (pattern only) stored in compressed
c         sparse row format.
c
c nzmax = length of arrays c and jc. see ierr.
c
c On return:
c-----------
c
c a, ja, ia and jmask, imask are unchanged.
c
c c
c jc,
c ic  = the output matrix in Compressed Sparse Row format.
c
c ierr  = integer. serving as error message.c
c         ierr = 1  means normal return
c         ierr .gt. 1 means that amask stopped when processing
c         row number ierr, because there was not enough space in
c         c, jc according to the value of nzmax.
c
c work arrays:
c-------------
c iw  = logical work array of length ncol.
c
c note:
c------ the  algorithm is in place: c, jc, ic can be the same as
c a, ja, ia in which cas the code will overwrite the matrix c
c on a, ja, ia
c
c-----------------------------------------------------------------------
      ierr = 0
      len = 0
      do 1 j=1, ncol
         iw(j) = .false.
 1    continue
c     unpack the mask for row ii in iw
      do 100 ii=1, nrow
c     save pointer in order to be able to do things in place
         do 2 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .true.
 2       continue
c     add umasked elemnts of row ii
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         ic(ii) = len+1
         do 200 k=k1,k2
            j = ja(k)
            if (iw(j)) then
               len = len+1
               if (len .gt. nzmax) then
                  ierr = ii
                  return
               endif
               jc(len) = j
               c(len) = a(k)
            endif
 200     continue
c
         do 3 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .false.
 3       continue
 100  continue
      ic(nrow+1)=len+1
c
      return
c-----end-of-amask -----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine rperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer, intent(in) :: nrow, job
      integer, intent(inout) :: ja(*),ia(nrow+1),jao(*),iao(nrow+1)
     * ,perm(nrow)
      real*8 , intent(inout) :: a(*),ao(*)
c-----------------------------------------------------------------------
c this subroutine permutes the rows of a matrix in CSR format.
c rperm  computes B = P A  where P is a permutation matrix.
c the permutation P is defined through the array perm: for each j,
c perm(j) represents the destination row number of row number j.
c Youcef Saad -- recoded Jan 28, 1991.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = dimension of the matrix
c a, ja, ia = input matrix in csr format
c perm   = integer array of length nrow containing the permutation arrays
c       for the rows: perm(i) is the destination of row i in the
c         permuted matrix.
c         ---> a(i,j) in the original matrix becomes a(perm(i),j)
c         in the output  matrix.
c
c job = integer indicating the work to be done:
c        job = 1  permute a, ja, ia into ao, jao, iao
c                       (including the copying of real values ao and
c                       the array iao).
c        job .ne. 1 :  ignore real values.
c                     (in which case arrays a and ao are not needed nor
c                      used).
c
c------------
c on return:
c------------
c ao, jao, iao = input matrix in a, ja, ia format
c note :
c        if (job.ne.1)  then the arrays a and ao are not used.
c----------------------------------------------------------------------c
c           Y. Saad, May  2, 1990                                      c
c----------------------------------------------------------------------c
      logical :: values
      integer :: i, j, k, ii, ko
      values = (job .eq. 1)
c
c     determine pointers for output matix.
c
      do 50 j=1,nrow
         i = perm(j)
         iao(i+1) = ia(j+1) - ia(j)
 50   continue
c
c get pointers from lengths
c
      iao(1) = 1
      do 51 j=1,nrow
         iao(j+1)=iao(j+1)+iao(j)
 51   continue
c
c copying
c
      do 100 ii=1,nrow
c
c old row = ii  -- new row = iperm(ii) -- ko = new pointer
c
         ko = iao(perm(ii))
         do 60 k=ia(ii), ia(ii+1)-1
            jao(ko) = ja(k)
            if (values) ao(ko) = a(k)
            ko = ko+1
 60      continue
 100  continue
c
      return
c---------end-of-rperm -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine cperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(*), job
      real*8 a(*), ao(*)
c-----------------------------------------------------------------------
c this subroutine permutes the columns of a matrix a, ja, ia.
c the result is written in the output matrix  ao, jao, iao.
c cperm computes B = A P, where  P is a permutation matrix
c that maps column j into column perm(j), i.e., on return
c      a(i,j) becomes a(i,perm(j)) in new matrix
c Y. Saad, May 2, 1990 / modified Jan. 28, 1991.
c-----------------------------------------------------------------------
c on entry:
c----------
c nrow   = row dimension of the matrix
c
c a, ja, ia = input matrix in csr format.
c
c perm   = integer array of length ncol (number of columns of A
c         containing the permutation array  the columns:
c         a(i,j) in the original matrix becomes a(i,perm(j))
c         in the output matrix.
c
c job = integer indicating the work to be done:
c        job = 1  permute a, ja, ia into ao, jao, iao
c                       (including the copying of real values ao and
c                       the array iao).
c        job .ne. 1 :  ignore real values ao and ignore iao.
c
c------------
c on return:
c------------
c ao, jao, iao = input matrix in a, ja, ia format (array ao not needed)
c
c Notes:
c-------
c 1. if job=1 then ao, iao are not used.
c 2. This routine is in place: ja, jao can be the same.
c 3. If the matrix is initially sorted (by increasing column number)
c    then ao,jao,iao  may not be on return.
c
c----------------------------------------------------------------------c
c local parameters:
      integer k, i, nnz
c
      nnz = ia(nrow+1)-1
      do 100 k=1,nnz
         jao(k) = perm(ja(k))
 100  continue
c
c     done with ja array. return if no need to touch values.
c
      if (job .ne. 1) return
c
c else get new pointers -- and copy values too.
c
      do 1 i=1, nrow+1
         iao(i) = ia(i)
 1    continue
c
      do 2 k=1, nnz
         ao(k) = a(k)
 2    continue
c
      return
c---------end-of-cperm--------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine dperm (nrow,a,ja,ia,ao,jao,iao,perm,qperm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),
     +        qperm(*),job
      real*8 a(*),ao(*)
c-----------------------------------------------------------------------
c This routine permutes the rows and columns of a matrix stored in CSR
c format. i.e., it computes P A Q, where P, Q are permutation matrices.
c P maps row i into row perm(i) and Q maps column j into column qperm(j):
c      a(i,j)    becomes   a(perm(i),qperm(j)) in new matrix
c In the particular case where Q is the transpose of P (symmetric
c permutation of A) then qperm is not needed.
c note that qperm should be of length ncol (number of columns) but this
c is not checked.
c-----------------------------------------------------------------------
c Y. Saad, Sep. 21 1989 / recoded Jan. 28 1991.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = dimension of the matrix
c a, ja,
c    ia = input matrix in a, ja, ia format
c perm   = integer array of length n containing the permutation arrays
c       for the rows: perm(i) is the destination of row i in the
c         permuted matrix -- also the destination of column i in case
c         permutation is symmetric (job .le. 2)
c
c qperm  = same thing for the columns. This should be provided only
c         if job=3 or job=4, i.e., only in the case of a nonsymmetric
c       permutation of rows and columns. Otherwise qperm is a dummy
c
c job = integer indicating the work to be done:
c * job = 1,2 permutation is symmetric  Ao :== P * A * transp(P)
c        job = 1  permute a, ja, ia into ao, jao, iao
c        job = 2 permute matrix ignoring real values.
c * job = 3,4 permutation is non-symmetric  Ao :== P * A * Q
c        job = 3  permute a, ja, ia into ao, jao, iao
c        job = 4 permute matrix ignoring real values.
c
c on return:
c-----------
c ao, jao, iao = input matrix in a, ja, ia format
c
c in case job .eq. 2 or job .eq. 4, a and ao are never referred to
c and can be dummy arguments.
c Notes:
c-------
c  1) algorithm is in place
c  2) column indices may not be sorted on return even  though they may be
c     on entry.
c----------------------------------------------------------------------c
c local variables
      integer locjob, mod
c
c     locjob indicates whether or not real values must be copied.
c
      locjob = mod(job,2)
c
c permute rows first
c
      call rperm (nrow,a,ja,ia,ao,jao,iao,perm,locjob)
c
c then permute columns
c
      locjob = 0
c
      if (job .le. 2) then
         call cperm (nrow,ao,jao,iao,ao,jao,iao,perm,locjob)
      else
         call cperm (nrow,ao,jao,iao,ao,jao,iao,qperm,locjob)
      endif
c
      return
c-------end-of-dperm----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine dperm1 (i1,i2,a,ja,ia,b,jb,ib,perm,ipos,job)
      integer, intent(in) :: ipos, job
      integer, intent(inout) :: i1,i2,ja(*),ia(*),jb(*),ib(*),perm(*)
      real*8 , intent(inout) :: a(*),b(*)
c-----------------------------------------------------------------------
c     general submatrix extraction routine.
c-----------------------------------------------------------------------
c     extracts rows perm(i1), perm(i1+1), ..., perm(i2) (in this order)
c     from a matrix (doing nothing in the column indices.) The resulting
c     submatrix is constructed in b, jb, ib. A pointer ipos to the
c     beginning of arrays b,jb,is also allowed (i.e., nonzero elements
c     are accumulated starting in position ipos of b, jb).
c-----------------------------------------------------------------------
c Y. Saad,Sep. 21 1989 / recoded Jan. 28 1991 / modified for PSPARSLIB
c Sept. 1997..
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = dimension of the matrix
c a,ja,
c   ia  = input matrix in CSR format
c perm   = integer array of length n containing the indices of the rows
c         to be extracted.
c
c job   = job indicator. if (job .ne.1) values are not copied (i.e.,
c         only pattern is copied).
c
c on return:
c-----------
c b,ja,
c ib   = matrix in csr format. b(ipos:ipos+nnz-1),jb(ipos:ipos+nnz-1)
c     contain the value and column indices respectively of the nnz
c     nonzero elements of the permuted matrix. thus ib(1)=ipos.
c
c Notes:
c-------
c  algorithm is NOT in place
c-----------------------------------------------------------------------
c local variables
c
      integer ko,irow,k,i
      logical values
c-----------------------------------------------------------------------
      values = (job .eq. 1)
      ko = ipos
      ib(1) = ko
      do 900 i=i1,i2
         irow = perm(i)
         do 800 k=ia(irow),ia(irow+1)-1
            if (values) b(ko) = a(k)
            jb(ko) = ja(k)
            ko=ko+1
 800     continue
         ib(i-i1+2) = ko
 900  continue
      return
c--------end-of-dperm1--------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine dperm2 (i1,i2,a,ja,ia,b,jb,ib,cperm,rperm,istart,
     *        ipos,job)
      integer, intent(in) :: ipos, job, istart
      integer, intent(inout) :: i1,i2,ja(*),ia(*),jb(*),ib(*),cperm(*)
     * ,rperm(*)
      real*8 , intent(inout) :: a(*),b(*)
c-----------------------------------------------------------------------
c     general submatrix permutation/ extraction routine.
c-----------------------------------------------------------------------
c     extracts rows rperm(i1), rperm(i1+1), ..., rperm(i2) and does an
c     associated column permutation (using array cperm). The resulting
c     submatrix is constructed in b, jb, ib. For added flexibility, the
c     extracted elements are put in sequence starting from row 'istart'
c     of B. In addition a pointer ipos to the beginning of arrays b,jb,
c     is also allowed (i.e., nonzero elements are accumulated starting in
c     position ipos of b, jb). In most applications istart and ipos are
c     equal to one. However, the generality adds substantial flexiblity.
c     EXPLE: (1) to permute msr to msr (excluding diagonals)
c     call dperm2 (1,n,a,ja,ja,b,jb,jb,rperm,rperm,1,n+2)
c            (2) To extract rows 1 to 10: define rperm and cperm to be
c     identity permutations (rperm(i)=i, i=1,n) and then
c            call dperm2 (1,10,a,ja,ia,b,jb,ib,rperm,rperm,1,1)
c            (3) to achieve a symmetric permutation as defined by perm:
c            call dperm2 (1,10,a,ja,ia,b,jb,ib,perm,perm,1,1)
c            (4) to get a symmetric permutation of A and append the
c            resulting data structure to A's data structure (useful!)
c            call dperm2 (1,10,a,ja,ia,a,ja,ia(n+1),perm,perm,1,ia(n+1))
c-----------------------------------------------------------------------
c Y. Saad,Sep. 21 1989 / recoded Jan. 28 1991.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = dimension of the matrix
c i1,i2 = extract rows rperm(i1) to rperm(i2) of A, with i1<i2.
c
c a,ja,
c   ia  = input matrix in CSR format
c cperm = integer array of length n containing the permutation arrays
c       for the columns: cperm(i) is the destination of column j,
c         i.e., any column index ja(k) is transformed into cperm(ja(k))
c
c rperm  =  permutation array for the rows. rperm(i) = origin (in A) of
c          row i in B. This is the reverse permutation relative to the
c          ones used in routines cperm, dperm,....
c          rows rperm(i1), rperm(i1)+1, ... rperm(i2) are
c          extracted from A and stacked into B, starting in row istart
c          of B.
c istart= starting row for B where extracted matrix is to be added.
c         this is also only a pointer of the be beginning address for
c         ib , on return.
c ipos  = beginning position in arrays b and jb where to start copying
c         elements. Thus, ib(istart) = ipos.
c
c job   = job indicator. if (job .ne.1) values are not copied (i.e.,
c         only pattern is copied).
c
c on return:
c-----------
c b,ja,
c ib   = matrix in csr format. positions 1,2,...,istart-1 of ib
c     are not touched. b(ipos:ipos+nnz-1),jb(ipos:ipos+nnz-1)
c     contain the value and column indices respectively of the nnz
c     nonzero elements of the permuted matrix. thus ib(istart)=ipos.
c
c Notes:
c-------
c  1) algorithm is NOT in place
c  2) column indices may not be sorted on return even  though they
c     may be on entry.
c-----------------------------------------------------------------------
c local variables
c
      integer ko,irow,k, i
      logical values
c-----------------------------------------------------------------------
      values = (job .eq. 1)
      ko = ipos
      ib(istart) = ko
      do 900 i=i1,i2
         irow = rperm(i)
         do 800 k=ia(irow),ia(irow+1)-1
            if (values) b(ko) = a(k)
            jb(ko) = cperm(ja(k))
            ko=ko+1
 800     continue
         ib(istart+i-i1+1) = ko
 900  continue
      return
c--------end-of-dperm2--------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine dmperm (nrow,a,ja,ao,jao,perm,job)
      integer, intent(in) :: nrow
      integer, intent(inout) :: ja(*),jao(*),perm(nrow),job
      real*8 , intent(inout) :: a(*),ao(*)
c-----------------------------------------------------------------------
c This routine performs a symmetric permutation of the rows and
c columns of a matrix stored in MSR format. i.e., it computes
c B = P A transp(P), where P, is  a permutation matrix.
c P maps row i into row perm(i) and column j into column perm(j):
c      a(i,j)    becomes   a(perm(i),perm(j)) in new matrix
c (i.e.  ao(perm(i),perm(j)) = a(i,j) )
c calls dperm.
c-----------------------------------------------------------------------
c Y. Saad, Nov 15, 1991.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = dimension of the matrix
c a, ja = input matrix in MSR format.
c perm   = integer array of length n containing the permutation arrays
c       for the rows: perm(i) is the destination of row i in the
c         permuted matrix -- also the destination of column i in case
c         permutation is symmetric (job .le. 2)
c
c job = integer indicating the work to be done:
c        job = 1  permute a, ja, ia into ao, jao, iao
c        job = 2 permute matrix ignoring real values.
c
c on return:
c-----------
c ao, jao = output matrix in MSR.
c
c in case job .eq. 2 a and ao are never referred to and can be dummy
c arguments.
c
c Notes:
c-------
c  1) algorithm is NOT in place
c  2) column indices may not be sorted on return even  though they may be
c     on entry.
c----------------------------------------------------------------------c
c     local variables
c
      integer n1, n2, j
      n1 = nrow+1
      n2 = n1+1
c
      call dperm (nrow,a,ja,ja,ao(n2),jao(n2),jao,perm,perm,job)
c
      jao(1) = n2
      do 101 j=1, nrow
         ao(perm(j)) = a(j)
         jao(j+1) = jao(j+1)+n1
 101  continue
c
c done
c
      return
c-----------------------------------------------------------------------
c--------end-of-dmperm--------------------------------------------------
      end
c-----------------------------------------------------------------------
    
      subroutine permsimple(n, x, XH, perm, permselect)
      integer, intent(in) :: n, perm(n), permselect
      real*8 , intent(inout) :: x(n), XH(N)
      integer :: k
      
      if (permselect > 1) then 
         XH   = X
         DO K = 1,N
             X(PERM(K)) = XH(k)
         ENDDO
      endif
      END
    
      subroutine permsimpleINVERSE (n, x, XH, perm, permselect)
      integer, intent(in) :: n, perm(n), permselect
      real*8 , intent(inout) :: x(n), XH(N)
      integer :: k
      
      if (permselect > 1) then 
         XH   = X
         DO K = 1,N
            X(K) = XH(PERM(K))
         ENDDO
      endif
      END
      

      subroutine dvperm (n, x, perm)
      integer, intent(in) :: n
      integer, intent(inout) :: perm(n)
      real*8 , intent(inout) :: x(n)
c-----------------------------------------------------------------------
c this subroutine performs an in-place permutation of a real vector x
c according to the permutation array perm(*), i.e., on return,
c the vector x satisfies,
c
c     x(perm(j)) :== x(j), j=1,2,.., n
c
c-----------------------------------------------------------------------
c on entry:
c---------
c n   = length of vector x.
c perm   = integer array of length n containing the permutation  array.
c x   = input vector
c
c on return:
c----------
c x   = vector x permuted according to x(perm(*)) :=  x(*)
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables
      real*8  :: tmp, tmp1
      integer :: init, next, j, k, ii
c
      init      = 1
      tmp   = x(init)
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
c
c loop
c
 6    k = k+1
c
c save the chased element --
c
      tmp1    = x(ii)
      x(ii)     = tmp
      next    = perm(ii)
      if (next .lt. 0 ) goto 65
c
c test for end
c
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next
c
c end loop
c
      goto 6
c
c reinitilaize cycle --
c
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp   = x(init)
      ii = perm(init)
      perm(init)=-perm(init)
      goto 6
c
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue
c
      return
c-------------------end-of-dvperm---------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine ivperm (n, ix, perm)
      integer, intent(in) :: n
      integer, intent(inout) :: ix(n), perm(n)
c-----------------------------------------------------------------------
c this subroutine performs an in-place permutation of an integer vector
c ix according to the permutation array perm(*), i.e., on return,
c the vector x satisfies,
c
c     ix(perm(j)) :== ix(j), j=1,2,.., n
c
c-----------------------------------------------------------------------
c on entry:
c---------
c n   = length of vector x.
c perm   = integer array of length n containing the permutation  array.
c ix  = input vector
c
c on return:
c----------
c ix  = vector x permuted according to ix(perm(*)) :=  ix(*)
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables
      integer :: tmp, tmp1, j, init, ii, k, next
c
      init      = 1
      tmp   = ix(init)
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
c
c loop
c
 6    k = k+1
c
c save the chased element --
c
      tmp1    = ix(ii)
      ix(ii)     = tmp
      next    = perm(ii)
      if (next .lt. 0 ) goto 65
c
c test for end
c
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next
c
c end loop
c
      goto 6
c
c reinitilaize cycle --
c
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp   = ix(init)
      ii = perm(init)
      perm(init)=-perm(init)
      goto 6
c
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue
c
      return
c-------------------end-of-ivperm---------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine retmx (n,a,ja,ia,dd)
      real*8 a(*),dd(*)
      integer n,ia(*),ja(*)
c-----------------------------------------------------------------------
c returns in dd(*) the max absolute value of elements in row *.
c used for scaling purposes. superseded by rnrms  .
c
c on entry:
c n   = dimension of A
c a,ja,ia
c     = matrix stored in compressed sparse row format
c dd  = real*8 array of length n. On output,entry dd(i) contains
c       the element of row i that has the largest absolute value.
c       Moreover the sign of dd is modified such that it is the
c       same as that of the diagonal element in row i.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables
      integer k2, i, k1, k
      real*8 t, t1, t2
c
c initialize
c
      k2 = 1
      do 11 i=1,n
         k1 = k2
         k2 = ia(i+1) - 1
         t = 0.0d0
         do 101  k=k1,k2
            t1 = abs(a(k))
            if (t1 .gt. t) t = t1
            if (ja(k) .eq. i) then
               if (a(k) .ge. 0.0) then
                  t2 = a(k)
               else
                  t2 = - a(k)
               endif
            endif
 101     continue
         dd(i) =  t2*t
c     we do not invert diag
 11   continue
      return
c---------end of retmx -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine diapos  (n,ja,ia,idiag)
      integer, intent(in) :: n
      integer, intent(inout) :: ia(n+1), ja(*), idiag(n)
      integer :: i, k
c-----------------------------------------------------------------------
c this subroutine returns the positions of the diagonal elements of a
c sparse matrix a, ja, ia, in the array idiag.
c-----------------------------------------------------------------------
c on entry:
c----------
c
c n   = integer. row dimension of the matrix a.
c a,ja,
c    ia = matrix stored compressed sparse row format. a array skipped.
c
c on return:
c-----------
c idiag  = integer array of length n. The i-th entry of idiag
c          points to the diagonal element a(i,i) in the arrays
c          a, ja. (i.e., a(idiag(i)) = element A(i,i) of matrix A)
c          if no diagonal element is found the entry is set to 0.
c----------------------------------------------------------------------c
c           Y. Saad, March, 1990
c----------------------------------------------------------------------c
      do 1 i=1, n
         idiag(i) = 0
 1    continue
c
c     sweep through data structure.
c
      do  6 i=1,n
         do 51 k= ia(i),ia(i+1) -1
            if (ja(k) .eq. i) idiag(i) = k
 51      continue
 6    continue
c----------- -end-of-diapos---------------------------------------------
c-----------------------------------------------------------------------
      return
      end
c-----------------------------------------------------------------------
      subroutine dscaldg (n,a,ja,ia,diag,job)
      integer, intent(in) :: n, job
      real*8 , intent(inout) :: a(*), diag(*)
      integer, intent(inout) :: ia(*),ja(*)
      integer :: i,j, k1, k2,k
      real*8 :: t
c-----------------------------------------------------------------------
c scales rows by diag where diag is either given (job=0)
c or to be computed:
c  job = 1 ,scale row i by  by  +/- max |a(i,j) | and put inverse of
c       scaling factor in diag(i),where +/- is the sign of a(i,i).
c  job = 2 scale by 2-norm of each row..
c if diag(i) = 0,then diag(i) is replaced by one
c (no scaling)..
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      goto (12,11,10) job+1
 10   do 110 j=1,n
         k1= ia(j)
         k2 = ia(j+1)-1
         t = 0.0d0
         do 111 k = k1,k2
 111        t = t+a(k)*a(k)
 110        diag(j) = sqrt(t)
            goto 12
 11   continue
      call retmx (n,a,ja,ia,diag)
c------
 12   do 1 j=1,n
         if (diag(j) .ne. 0.0d0) then
            diag(j) = 1.0d0/diag(j)
         else
            diag(j) = 1.0d0
         endif
 1    continue
      do 2 i=1,n
         t = diag(i)
         do 21 k=ia(i),ia(i+1) -1
            a(k) = a(k)*t
 21      continue
 2    continue
      return
c--------end of dscaldg -----------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine extbdg (n,a,ja,ia,bdiag,nblk,ao,jao,iao)
      integer, intent(in) :: n, nblk
      real*8 , intent(inout) :: bdiag(*),a(*),ao(*)
      integer, intent(inout) :: ia(*),ja(*),jao(*),iao(*)
      integer :: j, k, jj, j1, j2, ko, kb, i, ltr, l, m
c-----------------------------------------------------------------------
c this subroutine extracts the main diagonal blocks of a
c matrix stored in compressed sparse row format and puts the result
c into the array bdiag and the remainder in ao,jao,iao.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = integer. The row dimension of the matrix a.
c a,
c ja,
c ia    = matrix stored in csr format
c nblk  = dimension of each diagonal block. The diagonal blocks are
c         stored in compressed format rowwise,i.e.,we store in
c       succession the i nonzeros of the i-th row after those of
c       row number i-1..
c
c on return:
c----------
c bdiag = real*8 array of size (n x nblk) containing the diagonal
c       blocks of A on return
c ao,
c jao,
C iao   = remainder of the matrix stored in csr format.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      m = 1 + (n-1)/nblk
c this version is sequential -- there is a more parallel version
c that goes through the structure twice ....
      ltr =  ((nblk-1)*nblk)/2
      l = m * ltr
      do 1 i=1,l
         bdiag(i) = 0.0d0
 1    continue
      ko = 0
      kb = 1
      iao(1) = 1
c-------------------------
      do 11 jj = 1,m
         j1 = (jj-1)*nblk+1
         j2 =  min0 (n,j1+nblk-1)
         do 12 j=j1,j2
            do 13 i=ia(j),ia(j+1) -1
               k = ja(i)
               if (k .lt. j1) then
                  ko = ko+1
                  ao(ko) = a(i)
                  jao(ko) = k
               else if (k .lt. j) then
c     kb = (jj-1)*ltr+((j-j1)*(j-j1-1))/2+k-j1+1
c     bdiag(kb) = a(i)
                  bdiag(kb+k-j1) = a(i)
               endif
 13         continue
            kb = kb + j-j1
            iao(j+1) = ko+1
 12      continue
 11   continue
      return
c---------end-of-extbdg-------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine getbwd(n,a,ja,ia,ml,mu)
      integer, intent(in) :: n
      real*8 , intent(inout) :: a(*)
      integer, intent(inout) :: ja(*),ia(n+1),ml,mu
      integer ::ldist,i,k
c-----------------------------------------------------------------------
c gets the bandwidth of lower part and upper part of A.
c does not assume that A is sorted.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = integer = the row dimension of the matrix
c a, ja,
c    ia = matrix in compressed sparse row format.
c
c on return:
c-----------
c ml  = integer. The bandwidth of the strict lower part of A
c mu  = integer. The bandwidth of the strict upper part of A
c
c Notes:
c ===== ml and mu are allowed to be negative or return. This may be
c       useful since it will tell us whether a band is confined
c       in the strict  upper/lower triangular part.
c       indeed the definitions of ml and mu are
c
c       ml = max ( (i-j)  s.t. a(i,j) .ne. 0  )
c       mu = max ( (j-i)  s.t. a(i,j) .ne. 0  )
c----------------------------------------------------------------------c
c Y. Saad, Sep. 21 1989                                                c
c----------------------------------------------------------------------c
      ml = - n
      mu = - n
      do 3 i=1,n
         do 31 k=ia(i),ia(i+1)-1
            ldist = i-ja(k)
            ml = max(ml,ldist)
            mu = max(mu,-ldist)
 31      continue
 3    continue
      return
c---------------end-of-getbwd ------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine blkfnd (nrow,ja,ia,nblk)
      integer, intent(in) :: nrow
      integer, intent(out) :: nblk
      integer, intent(inout) :: ia(nrow+1),ja(*)
      integer :: imsg, jf, jl, i1, i2, jfirst, jlast, jrow, len0, iblk
     * , len, i, irow, minlen
c-----------------------------------------------------------------------
c This routine attemptps to determine whether or not  the input
c matrix has a block structure and finds the blocks size
c if it does. A block matrix is one which is
c comprised of small square dense blocks. If there are zero
c elements within the square blocks and the original data structure
c takes these zeros into account then blkchk may fail to find the
c correct block size.
c-----------------------------------------------------------------------
c on entry
c---------
c nrow   = integer equal to the row dimension of the matrix.
c ja    = integer array containing the column indices of the entries
c         nonzero entries of the matrix stored by row.
c ia    = integer array of length nrow + 1 containing the pointers
c         beginning of each row in array ja.
c
c nblk  = integer containing the assumed value of nblk if job = 0
c
c on return
c----------
c nblk  = integer containing the value found for nblk when job = 1.
c         if imsg .ne. 0 this value is meaningless however.
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c-----------------------------------------------------------------------
c first part of code will find candidate block sizes.
c criterion used here is a simple one: scan rows and  determine groups
c of rows that have the same length and such that the first column
c number and the last column number are identical.
c-----------------------------------------------------------------------
      minlen = ia(2)-ia(1)
      irow   = 1
      do 1 i=2,nrow
         len = ia(i+1)-ia(i)
         if (len .lt. minlen) then
            minlen = len
            irow = i
         endif
 1    continue
c
c     ---- candidates are all dividers of minlen
c
      nblk = 1
      if (minlen .le. 1) return
c
      do 99 iblk = minlen, 1, -1
         if (mod(minlen,iblk) .ne. 0) goto 99
         len = ia(2) - ia(1)
         len0 = len
         jfirst = ja(1)
         jlast = ja(ia(2)-1)
         do 10 jrow = irow+1,irow+nblk-1
            i1 = ia(jrow)
            i2 = ia(jrow+1)-1
            len = i2+1-i1
            jf = ja(i1)
            jl = ja(i2)
            if (len .ne. len0 .or. jf .ne. jfirst .or.
     *           jl .ne. jlast) goto 99
 10      continue
c
c     check for this candidate ----
c
         call blkchk (nrow,ja,ia,iblk,imsg)
         if (imsg .eq. 0) then
c
c     block size found
c
            nblk = iblk
            return
         endif
 99   continue
c--------end-of-blkfnd -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine blkchk (nrow,ja,ia,nblk,imsg)
      integer, intent(in) :: nrow, nblk
      integer, intent(out) :: imsg
      integer, intent(inout) :: ia(nrow+1),ja(*)
      integer :: len, i, k, lena, jstart, j, ii, i1,j2, irow, nr
c-----------------------------------------------------------------------
c This routine checks whether the input matrix is a block
c matrix with block size of nblk. A block matrix is one which is
c comprised of small square dense blocks. If there are zero
c elements within the square blocks and the data structure
c takes them into account then blkchk may fail to find the
c correct block size.
c-----------------------------------------------------------------------
c on entry
c---------
c nrow   = integer equal to the row dimension of the matrix.
c ja    = integer array containing the column indices of the entries
c         nonzero entries of the matrix stored by row.
c ia    = integer array of length nrow + 1 containing the pointers
c         beginning of each row in array ja.
c
c nblk  = integer containing the value of nblk to be checked.
c
c on return
c----------
c
c imsg  = integer containing a message  with the following meaning.
c          imsg = 0 means that the output value of nblk is a correct
c                   block size. nblk .lt. 0 means nblk not correct
c                   block size.
c          imsg = -1 : nblk does not divide nrow
c          imsg = -2 : a starting element in a row is at wrong position
c             (j .ne. mult*nblk +1 )
c          imsg = -3 : nblk does divide a row length -
c          imsg = -4 : an element is isolated outside a block or
c             two rows in same group have different lengths
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c----------------------------------------------------------------------
c first part of code will find candidate block sizes.
c this is not guaranteed to work . so a check is done at the end
c the criterion used here is a simple one:
c scan rows and determine groups of rows that have the same length
c and such that the first column number and the last column number
c are identical.
c----------------------------------------------------------------------
      imsg = 0
      if (nblk .le. 1) return
      nr = nrow/nblk
      if (nr*nblk .ne. nrow) goto 101
c--   main loop ---------------------------------------------------------
      irow = 1
      do 20 ii=1, nr
c     i1= starting position for group of nblk rows in original matrix
         i1 = ia(irow)
         j2 = i1
c     lena = length of each row in that group  in the original matrix
         lena = ia(irow+1)-i1
c     len = length of each block-row in that group in the output matrix
         len = lena/nblk
         if (len* nblk .ne. lena) goto 103
c
c     for each row
c
         do 6 i = 1, nblk
            irow = irow + 1
            if (ia(irow)-ia(irow-1) .ne. lena ) goto 104
c
c     for each block
c
            do 7 k=0, len-1
               jstart = ja(i1+nblk*k)-1
               if ( (jstart/nblk)*nblk .ne. jstart) goto 102
c
c     for each column
c
               do 5 j=1, nblk
                  if (jstart+j .ne. ja(j2) )  goto 104
                  j2 = j2+1
 5             continue
 7          continue
 6       continue
 20   continue
c     went through all loops successfully:
      return
 101  imsg = -1
      return
 102  imsg = -2
      return
 103  imsg = -3
      return
 104  imsg = -4
c----------------end of chkblk -----------------------------------------
c-----------------------------------------------------------------------
      return
      end
c-----------------------------------------------------------------------
      subroutine infdia (n,ja,ia,ind,idiag)
      integer, intent(in) :: n
      integer, intent(out) :: idiag
      integer, intent(inout) :: ia(*), ind(*), ja(*)
      integer :: i,j,k,n2
c-----------------------------------------------------------------------
c     obtains information on the diagonals of A.
c-----------------------------------------------------------------------
c this subroutine finds the lengths of each of the 2*n-1 diagonals of A
c it also outputs the number of nonzero diagonals found.
c-----------------------------------------------------------------------
c on entry:
c----------
c n   = dimension of the matrix a.
c
c a,    ..... not needed here.
c ja,
c ia    = matrix stored in csr format
c
c on return:
c-----------
c
c idiag = integer. number of nonzero diagonals found.
c
c ind   = integer array of length at least 2*n-1. The k-th entry in
c         ind contains the number of nonzero elements in the diagonal
c         number k, the numbering beeing from the lowermost diagonal
c         (bottom-left). In other words ind(k) = length of diagonal
c         whose offset wrt the main diagonal is = - n + k.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      n2= n+n-1
      do 1 i=1,n2
         ind(i) = 0
 1    continue
      do 3 i=1, n
         do 2 k=ia(i),ia(i+1)-1
            j = ja(k)
            ind(n+j-i) = ind(n+j-i) +1
 2       continue
 3    continue
c     count the nonzero ones.
      idiag = 0
      do 41 k=1, n2
         if (ind(k) .ne. 0) idiag = idiag+1
 41   continue
      return
c done
c------end-of-infdia ---------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine amubdg (nrow,ncol,ncolb,ja,ia,jb,ib,ndegr,nnz,iw)
      integer, intent(in) :: nrow, ncol, ncolb
      integer, intent(out) :: nnz
      integer, intent(inout) :: ja(*),jb(*),ia(nrow+1),ib(ncol+1)
     * ,ndegr(nrow),iw(ncolb)
      integer :: jc, jr, j, last, k, ldg, ii
c-----------------------------------------------------------------------
c gets the number of nonzero elements in each row of A*B and the total
c number of nonzero elements in A*B.
c-----------------------------------------------------------------------
c on entry:
c --------
c
c nrow  = integer.  row dimension of matrix A
c ncol  = integer.  column dimension of matrix A = row dimension of
c                   matrix B.
c ncolb = integer. the colum dimension of the matrix B.
c
c ja, ia= row structure of input matrix A: ja = column indices of
c         the nonzero elements of A stored by rows.
c         ia = pointer to beginning of each row  in ja.
c
c jb, ib= row structure of input matrix B: jb = column indices of
c         the nonzero elements of A stored by rows.
c         ib = pointer to beginning of each row  in jb.
c
c on return:
c ---------
c ndegr  = integer array of length nrow containing the degrees (i.e.,
c         the number of nonzeros in  each row of the matrix A * B
c
c nnz   = total number of nonzero elements found in A * B
c
c work arrays:
c-------------
c iw  = integer work array of length ncolb.
c-----------------------------------------------------------------------
      do 1 k=1, ncolb
         iw(k) = 0
 1    continue

      do 2 k=1, nrow
         ndegr(k) = 0
 2    continue
c
c     method used: Transp(A) * A = sum [over i=1, nrow]  a(i)^T a(i)
c     where a(i) = i-th row of  A. We must be careful not to add  the
c     elements already accounted for.
c
c
      do 7 ii=1,nrow
c
c     for each row of A
c
         ldg = 0
c
c    end-of-linked list
c
         last = -1
         do 6 j = ia(ii),ia(ii+1)-1
c
c     row number to be added:
c
            jr = ja(j)
            do 5 k=ib(jr),ib(jr+1)-1
               jc = jb(k)
               if (iw(jc) .eq. 0) then
c
c     add one element to the linked list
c
                  ldg = ldg + 1
                  iw(jc) = last
                  last = jc
               endif
 5          continue
 6       continue
         ndegr(ii) = ldg
c
c     reset iw to zero
c
         do 61 k=1,ldg
            j = iw(last)
            iw(last) = 0
            last = j
 61      continue
c-----------------------------------------------------------------------
 7    continue
c
      nnz = 0
      do 8 ii=1, nrow
         nnz = nnz+ndegr(ii)
 8    continue
c
      return
c---------------end-of-amubdg ------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aplbdg (nrow,ncol,ja,ia,jb,ib,ndegr,nnz,iw)
      integer, intent(in) :: nrow, ncol
      integer, intent(out) :: nnz
      integer, intent(inout) :: ja(*),jb(*),ia(nrow+1),ib(nrow+1)
     * ,iw(ncol),ndegr(nrow)
      integer :: last, j, jr, jc, ldg, k, ii
c-----------------------------------------------------------------------
c gets the number of nonzero elements in each row of A+B and the total
c number of nonzero elements in A+B.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow   = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c
c b,
c jb,
c ib  =  Matrix B in compressed sparse row format.
c
c on return:
c----------
c ndegr  = integer array of length nrow containing the degrees (i.e.,
c         the number of nonzeros in  each row of the matrix A + B.
c
c nnz   = total number of nonzero elements found in A * B
c
c work arrays:
c------------
c iw  = integer work array of length equal to ncol.
c
c-----------------------------------------------------------------------
      do 1 k=1, ncol
         iw(k) = 0
 1    continue
c
      do 2 k=1, nrow
         ndegr(k) = 0
 2    continue
c
      do 7 ii=1,nrow
         ldg = 0
c
c    end-of-linked list
c
         last = -1
c
c     row of A
c
         do 5 j = ia(ii),ia(ii+1)-1
            jr = ja(j)
c
c     add element to the linked list
c
            ldg = ldg + 1
            iw(jr) = last
            last = jr
 5       continue
c
c     row of B
c
         do 6 j=ib(ii),ib(ii+1)-1
            jc = jb(j)
            if (iw(jc) .eq. 0) then
c
c     add one element to the linked list
c
               ldg = ldg + 1
               iw(jc) = last
               last = jc
            endif
 6       continue
c     done with row ii.
         ndegr(ii) = ldg
c
c     reset iw to zero
c
         do 61 k=1,ldg
            j = iw(last)
            iw(last) = 0
            last = j
 61      continue
c-----------------------------------------------------------------------
 7    continue
c
      nnz = 0
      do 8 ii=1, nrow
         nnz = nnz+ndegr(ii)
 8    continue
      return
c----------------end-of-aplbdg -----------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine rnrms   (nrow, nrm, a, ja, ia, diag)
      integer, intent(in) :: nrow, nrm
      real*8 , intent(inout) :: a(*), diag(nrow)
      integer, intent(inout) :: ja(*), ia(nrow+1)
      real*8 :: scal
      integer :: k, k1, k2, ii
c-----------------------------------------------------------------------
c gets the norms of each row of A. (choice of three norms)
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow   = integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c
c on return:
c----------
c
c diag = real vector of length nrow containing the norms
c
c-----------------------------------------------------------------
      do 1 ii=1,nrow
c
c     compute the norm if each element.
c
         scal = 0.0d0
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         if (nrm .eq. 0) then
            do 2 k=k1, k2
               scal = max(scal,abs(a(k) ) )
 2          continue
         elseif (nrm .eq. 1) then
            do 3 k=k1, k2
               scal = scal + abs(a(k) )
 3          continue
         else
            do 4 k=k1, k2
               scal = scal+a(k)**2
 4          continue
         endif
         if (nrm .eq. 2) scal = sqrt(scal)
         diag(ii) = scal
 1    continue
      return
c-----------------------------------------------------------------------
c-------------end-of-rnrms----------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine cnrms   (nrow, nrm, a, ja, ia, diag)
      integer, intent(in) :: nrow, nrm
      real*8,  intent(inout) :: a(*), diag(nrow)
      integer, intent(inout) :: ja(*), ia(nrow+1)
      integer :: k, ii, k1, k2, j
c-----------------------------------------------------------------------
c gets the norms of each column of A. (choice of three norms)
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow   = integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c
c on return:
c----------
c
c diag = real vector of length nrow containing the norms
c
c-----------------------------------------------------------------
      do 10 k=1, nrow
         diag(k) = 0.0d0
 10   continue
      do 1 ii=1,nrow
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         do 2 k=k1, k2
            j = ja(k)
c     update the norm of each column
            if (nrm .eq. 0) then
               diag(j) = max(diag(j),abs(a(k) ) )
            elseif (nrm .eq. 1) then
               diag(j) = diag(j) + abs(a(k) )
            else
               diag(j) = diag(j)+a(k)**2
            endif
 2       continue
 1    continue
      if (nrm .ne. 2) return
      do 3 k=1, nrow
         diag(k) = sqrt(diag(k))
 3    continue
      return
c-----------------------------------------------------------------------
c------------end-of-cnrms-----------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine roscal(nrow,job,nrm,a,ja,ia,diag,b,jb,ib,ierr)
      integer, intent(in) :: nrow, job, nrm
      integer, intent(out) :: ierr
      real*8 , intent(out) :: a(*), b(*), diag(nrow)
      integer, intent(inout) :: ja(*),jb(*),ia(nrow+1),ib(nrow+1)
      integer :: j
c-----------------------------------------------------------------------
c scales the rows of A such that their norms are one on return
c 3 choices of norms: 1-norm, 2-norm, max-norm.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow   = integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         job = 1 means get b, and the integer arrays ib, jb.
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the rows have been scaled, i.e., on return
c        we have B = Diag*A.
c
c b,
c jb,
c ib  = resulting matrix B in compressed sparse row sparse format.
c
c ierr  = error message. ierr=0     : Normal return
c                        ierr=i > 0 : Row number i is a zero row.
c Notes:
c-------
c 1)        The column dimension of A is not needed.
c 2)        algorithm in place (B can take the place of A).
c-----------------------------------------------------------------
      call rnrms (nrow,nrm,a,ja,ia,diag)
      ierr = 0
      do 1 j=1, nrow
         if (diag(j) .eq. 0.0d0) then
            ierr = j
            return
         else
            diag(j) = 1.0d0/diag(j)
         endif
 1    continue
      call diamua(nrow,job,a,ja,ia,diag,b,jb,ib)
      return
c-------end-of-roscal---------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine coscal(nrow,job,nrm,a,ja,ia,diag,b,jb,ib,ierr)
c-----------------------------------------------------------------------
      integer, intent(in) :: nrow, job
      integer, intent(out) :: ierr
      real*8 , intent(inout) :: a(*),b(*),diag(nrow)
      integer, intent(inout) :: ja(*),jb(*),ia(nrow+1),ib(nrow+1)
      integer :: j, nrm
c-----------------------------------------------------------------------
c scales the columns of A such that their norms are one on return
c result matrix written on b, or overwritten on A.
c 3 choices of norms: 1-norm, 2-norm, max-norm. in place.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow   = integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         job = 1 means get b, and the integer arrays ib, jb.
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the columns have been scaled, i.e., on return
c        we have B = A * Diag
c
c b,
c jb,
c ib  = resulting matrix B in compressed sparse row sparse format.
c
c ierr  = error message. ierr=0     : Normal return
c                        ierr=i > 0 : Column number i is a zero row.
c Notes:
c-------
c 1)     The column dimension of A is not needed.
c 2)     algorithm in place (B can take the place of A).
c-----------------------------------------------------------------
      call cnrms (nrow,nrm,a,ja,ia,diag)
      ierr = 0
      do 1 j=1, nrow
         if (diag(j) .eq. 0.0) then
            ierr = j
            return
         else
            diag(j) = 1.0d0/diag(j)
         endif
 1    continue
      call amudia (nrow,job,a,ja,ia,diag,b,jb,ib)
      return
c--------end-of-coscal--------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine addblk(nrowa, ncola, a, ja, ia, ipos, jpos, job,
     & nrowb, ncolb, b, jb, ib, nrowc, ncolc, c, jc, ic, nzmx, ierr)
c      implicit none
      integer nrowa, nrowb, nrowc, ncola, ncolb, ncolc, ipos, jpos
      integer nzmx, ierr, job
      integer ja(1:*), ia(1:*), jb(1:*), ib(1:*), jc(1:*), ic(1:*)
      real*8 a(1:*), b(1:*), c(1:*)
c-----------------------------------------------------------------------
c     This subroutine adds a matrix B into a submatrix of A whose
c     (1,1) element is located in the starting position (ipos, jpos).
c     The resulting matrix is allowed to be larger than A (and B),
c     and the resulting dimensions nrowc, ncolc will be redefined
c     accordingly upon return.
c     The input matrices are assumed to be sorted, i.e. in each row
c     the column indices appear in ascending order in the CSR format.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrowa    = number of rows in A.
c bcola    = number of columns in A.
c a,ja,ia  = Matrix A in compressed sparse row format with entries sorted
c nrowb    = number of rows in B.
c ncolb    = number of columns in B.
c b,jb,ib  = Matrix B in compressed sparse row format with entries sorted
c
c nzmax     = integer. The  length of the arrays c and jc. addblk will
c            stop if the number of nonzero elements in the matrix C
c            exceeds nzmax. See ierr.
c
c on return:
c----------
c nrowc    = number of rows in C.
c ncolc    = number of columns in C.
c c,jc,ic  = resulting matrix C in compressed sparse row sparse format
c            with entries sorted ascendly in each row.
c
c ierr      = integer. serving as error message.
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that addblk stopped while computing the
c         i-th row  of C with i=ierr, because the number
c         of elements in C exceeds nzmax.
c
c Notes:
c-------
c     this will not work if any of the two input matrices is not sorted
c-----------------------------------------------------------------------
      logical values
      integer i,j1,j2,ka,kb,kc,kamax,kbmax
      values = (job .ne. 0)
      ierr = 0
      nrowc = max(nrowa, nrowb+ipos-1)
      ncolc = max(ncola, ncolb+jpos-1)
      kc = 1
      kbmax = 0
      ic(1) = kc
c
      do 10 i=1, nrowc
         if (i.le.nrowa) then
            ka = ia(i)
            kamax = ia(i+1)-1
         else
            ka = ia(nrowa+1)
         end if
         if ((i.ge.ipos).and.((i-ipos).le.nrowb)) then
            kb = ib(i-ipos+1)
            kbmax = ib(i-ipos+2)-1
         else
            kb = ib(nrowb+1)
         end if
c
c     a do-while type loop -- goes through all the elements in a row.
c
 20      continue
         if (ka .le. kamax) then
            j1 = ja(ka)
         else
            j1 = ncolc+1
         endif
         if (kb .le. kbmax) then
            j2 = jb(kb) + jpos - 1
         else
            j2 = ncolc+1
         endif
c
c     if there are more elements to be added.
c
         if ((ka .le. kamax .or. kb .le. kbmax) .and.
     &        (j1 .le. ncolc .or. j2 .le. ncolc)) then
c
c     three cases
c
            if (j1 .eq. j2) then
               if (values) c(kc) = a(ka)+b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
               kc = kc+1
            else if (j1 .lt. j2) then
               jc(kc) = j1
               if (values) c(kc) = a(ka)
               ka = ka+1
               kc = kc+1
            else if (j1 .gt. j2) then
               jc(kc) = j2
               if (values) c(kc) = b(kb)
               kb = kb+1
               kc = kc+1
            endif
            if (kc .gt. nzmx) goto 999
            goto 20
         end if
         ic(i+1) = kc
 10   continue
      return
 999  ierr = i
      return
c---------end-of-addblk-------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine get1up (n,ja,ia,ju)
      integer  n, ja(*),ia(*),ju(*)
c----------------------------------------------------------------------
c obtains the first element of each row of the upper triangular part
c of a matrix. Assumes that the matrix is already sorted.
c-----------------------------------------------------------------------
c parameters
c input
c -----
c ja      = integer array containing the column indices of aij
c ia      = pointer array. ia(j) contains the position of the
c           beginning of row j in ja
c
c output
c ------
c ju      = integer array of length n. ju(i) is the address in ja
c           of the first element of the uper triangular part of
c           of A (including rthe diagonal. Thus if row i does have
c           a nonzero diagonal element then ju(i) will point to it.
c           This is a more general version of diapos.
c-----------------------------------------------------------------------
c local vAriables
      integer i, k
c
      do 5 i=1, n
         ju(i) = 0
         k = ia(i)
c
 1       continue
         if (ja(k) .ge. i) then
            ju(i) = k
            goto 5
         elseif (k .lt. ia(i+1) -1) then
            k=k+1
c
c go try next element in row
c
            goto 1
         endif
 5    continue
      return
c-----end-of-get1up-----------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine xtrows (i1,i2,a,ja,ia,ao,jao,iao,iperm,job)
      integer i1,i2,ja(*),ia(*),jao(*),iao(*),iperm(*),job
      real*8 a(*),ao(*)
c-----------------------------------------------------------------------
c this subroutine extracts given rows from a matrix in CSR format.
c Specifically, rows number iperm(i1), iperm(i1+1), ...., iperm(i2)
c are extracted and put in the output matrix ao, jao, iao, in CSR
c format.  NOT in place.
c Youcef Saad -- coded Feb 15, 1992.
c-----------------------------------------------------------------------
c on entry:
c----------
c i1,i2   = two integers indicating the rows to be extracted.
c           xtrows will extract rows iperm(i1), iperm(i1+1),..,iperm(i2),
c           from original matrix and stack them in output matrix
c           ao, jao, iao in csr format
c
c a, ja, ia = input matrix in csr format
c
c iperm  = integer array of length nrow containing the reverse permutation
c         array for the rows. row number iperm(j) in permuted matrix PA
c         used to be row number j in unpermuted matrix.
c         ---> a(i,j) in the permuted matrix was a(iperm(i),j)
c         in the inout matrix.
c
c job = integer indicating the work to be done:
c        job .ne. 1 : get structure only of output matrix,,
c               i.e., ignore real values. (in which case arrays a
c               and ao are not used nor accessed).
c        job = 1  get complete data structure of output matrix.
c               (i.e., including arrays ao and iao).
c------------
c on return:
c------------
c ao, jao, iao = input matrix in a, ja, ia format
c note :
c        if (job.ne.1)  then the arrays a and ao are not used.
c----------------------------------------------------------------------c
c           Y. Saad, revised May  2, 1990                              c
c----------------------------------------------------------------------c
      logical :: values
      integer :: ii, j, k, ko
      values = (job .eq. 1)
c
c copying
c
      ko = 1
      iao(1) = ko
      do 100 j=i1,i2
c
c ii=iperm(j) is the index of old row to be copied.
c
         ii = iperm(j)
         do 60 k=ia(ii), ia(ii+1)-1
            jao(ko) = ja(k)
            if (values) ao(ko) = a(k)
            ko = ko+1
 60      continue
         iao(j-i1+2) = ko
 100  continue
c
      return
c---------end-of-xtrows-------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrkvstr(n, ia, ja, nr, kvstr)
c-----------------------------------------------------------------------
      integer n, ia(n+1), ja(*), nr, kvstr(*)
c-----------------------------------------------------------------------
c     Finds block row partitioning of matrix in CSR format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     n       = number of matrix scalar rows
c     ia,ja   = input matrix sparsity structure in CSR format
c
c     On return:
c---------------
c     nr      = number of block rows
c     kvstr   = first row number for each block row
c
c     Notes:
c-----------
c     Assumes that the matrix is sorted by columns.
c     This routine does not need any workspace.
c
c-----------------------------------------------------------------------
c     local variables
      integer i, j, jdiff
c-----------------------------------------------------------------------
      nr = 1
      kvstr(1) = 1
c---------------------------------
      do i = 2, n
         jdiff = ia(i+1)-ia(i)
         if (jdiff .eq. ia(i)-ia(i-1)) then
            do j = ia(i), ia(i+1)-1
               if (ja(j) .ne. ja(j-jdiff)) then
                  nr = nr + 1
                  kvstr(nr) = i
                  goto 299
               endif
            enddo
 299        continue
         else
 300        nr = nr + 1
            kvstr(nr) = i
         endif
      enddo
      kvstr(nr+1) = n+1
c---------------------------------
      return
      end
c-----------------------------------------------------------------------
c------------------------end-of-csrkvstr--------------------------------
      subroutine csrkvstc(n, ia, ja, nc, kvstc, iwk)
c-----------------------------------------------------------------------
      integer n, ia(n+1), ja(*), nc, kvstc(*), iwk(*)
c-----------------------------------------------------------------------
c     Finds block column partitioning of matrix in CSR format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     n       = number of matrix scalar rows
c     ia,ja   = input matrix sparsity structure in CSR format
c
c     On return:
c---------------
c     nc      = number of block columns
c     kvstc   = first column number for each block column
c
c     Work space:
c----------------
c     iwk(*) of size equal to the number of scalar columns plus one.
c        Assumed initialized to 0, and left initialized on return.
c
c     Notes:
c-----------
c     Assumes that the matrix is sorted by columns.
c
c-----------------------------------------------------------------------
c     local variables
      integer i, j, k, ncol
c
c-----------------------------------------------------------------------
c-----use ncol to find maximum scalar column number
      ncol = 0
c-----mark the beginning position of the blocks in iwk
      do i = 1, n
         if (ia(i) .lt. ia(i+1)) then
            j = ja(ia(i))
            iwk(j) = 1
            do k = ia(i)+1, ia(i+1)-1
               j = ja(k)
               if (ja(k-1).ne.j-1) then
                  iwk(j) = 1
                  iwk(ja(k-1)+1) = 1
               endif
            enddo
            iwk(j+1) = 1
            ncol = max0(ncol, j)
         endif
      enddo
c---------------------------------
      nc = 1
      kvstc(1) = 1
      do i = 2, ncol+1
         if (iwk(i).ne.0) then
            nc = nc + 1
            kvstc(nc) = i
            iwk(i) = 0
         endif
      enddo
      nc = nc - 1
c---------------------------------
      return
      end
c-----------------------------------------------------------------------
c------------------------end-of-csrkvstc--------------------------------
c-----------------------------------------------------------------------
      subroutine kvstmerge(nr, kvstr, nc, kvstc, n, kvst)
c-----------------------------------------------------------------------
      integer nr, kvstr(nr+1), nc, kvstc(nc+1), n, kvst(*)
c-----------------------------------------------------------------------
c     Merges block partitionings, for conformal row/col pattern.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     nr,nc   = matrix block row and block column dimension
c     kvstr   = first row number for each block row
c     kvstc   = first column number for each block column
c
c     On return:
c---------------
c     n       = conformal row/col matrix block dimension
c     kvst    = conformal row/col block partitioning
c
c     Notes:
c-----------
c     If matrix is not square, this routine returns without warning.
c
c-----------------------------------------------------------------------
c-----local variables
      integer i,j
c---------------------------------
      if (kvstr(nr+1) .ne. kvstc(nc+1)) return
      i = 1
      j = 1
      n = 1
  200 if (i .gt. nr+1) then
         kvst(n) = kvstc(j)
         j = j + 1
      elseif (j .gt. nc+1) then
         kvst(n) = kvstr(i)
         i = i + 1
      elseif (kvstc(j) .eq. kvstr(i)) then
         kvst(n) = kvstc(j)
         j = j + 1
         i = i + 1
      elseif (kvstc(j) .lt. kvstr(i)) then
         kvst(n) = kvstc(j)
         j = j + 1
      else
         kvst(n) = kvstr(i)
         i = i + 1
      endif
      n = n + 1
      if (i.le.nr+1 .or. j.le.nc+1) goto 200
      n = n - 2
c---------------------------------
      return
c------------------------end-of-kvstmerge-------------------------------
      end
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c          REORDERING ROUTINES -- COLORING BASED ROUTINES              c 
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c multic  : greedy algorithm for multicoloring                         c
c indset0 : greedy algorithm for independent set ordering              c
c indset1 : independent set ordering using minimal degree traversal    c
c indset2 : independent set ordering with local minimization           c
c indset3 : independent set ordering by vertex cover algorithm         c
c HeapSort, FixHeap, HeapInsert, interchange, MoveBack, FiHeapM,       c
c           FixHeapM, HeapInsertM,indsetr,rndperm, are utility         c
c           routines for sorting, generating random permutations, etc. c
c----------------------------------------------------------------------c
      subroutine multic (n,ja,ia,ncol,kolrs,il,iord,maxcol,ierr)
      integer, intent(in) :: n, maxcol
      integer, intent(out) :: ierr
      integer, intent(inout) :: ja(*),ia(n+1),kolrs(n),iord(n)
     * ,il(maxcol+1)
c-----------------------------------------------------------------------
c     multicoloring ordering -- greedy algorithm -- 
c     determines the coloring permutation and sets up
c     corresponding data structures for it.
c-----------------------------------------------------------------------
c on entry
c -------- 
c n     = row and column dimention of matrix
c ja    = column indices of nonzero elements of matrix, stored rowwise.
c ia    = pointer to beginning of each row in ja.
c maxcol= maximum number of colors allowed -- the size of il is
c         maxcol+1 at least. Note: the number of colors does not
c         exceed the maximum degree of each node +1.
c iord  = en entry iord gives the order of traversal of the nodes
c         in the multicoloring algorithm. If there is no preference 
c         then set iord(j)=j for j=1,...,n
c
c on return
c --------- 
c ncol  = number of colours found 
c kolrs = integer array containing the color number assigned to each node 
c il    = integer array containing the pointers to the
c         beginning of each color set. In the permuted matrix
c         the rows /columns il(kol) to il(kol+1)-1 have the same color.
c iord  = permutation array corresponding to the multicolor ordering.
c         row number i will become row nbumber iord(i) in permuted 
c         matrix. (iord = destination permutation array).
c ierr  = integer. Error message. normal return ierr = 0. If ierr .eq.1
c         then the array il was overfilled. 
c 
c-----------------------------------------------------------------------
c
      integer kol, i, j, k, mycol, icol, mcol, ii, ncol
c
      ierr = 0
      do 1 j=1, n
         kolrs(j) = 0
 1    continue 
      do 11 j=1, maxcol
         il(j) = 0
 11   continue
c     
      ncol = 0
c     
c     scan all nodes 
c     
      do 4 ii=1, n
         i = iord(ii) 
c     
c     look at adjacent nodes to determine colors already assigned
c     
         mcol = 0
         do 2 k=ia(i), ia(i+1)-1
            j = ja(k)
            icol = kolrs(j)
            if (icol .ne. 0) then
               mcol = max(mcol,icol) 
c     
c     il used as temporary to record already assigned colors.
c     
               il(icol) = 1 
            endif
 2       continue
c     
c     taken colors determined. scan il until a slot opens up.
c     
         mycol = 1
 3       if (il(mycol) .eq. 1) then
            mycol = mycol+1 
            if (mycol .gt. maxcol) goto 99
            if (mycol .le. mcol) goto 3
         endif
c     
c     reset il to zero for next nodes
c     
         do 35 j=1, mcol
            il(j) = 0
 35      continue
c     
c     assign color and update number of colors so far
c     
         kolrs(i) = mycol
         ncol = max(ncol,mycol)
 4    continue
c     
c     every node has now been colored. Count nodes of each color
c     
      do 6 j=1, n
         kol = kolrs(j)+1
         il(kol) = il(kol)+1
 6    continue
c     
c     set pointers il
c     
      il(1) = 1
      do 7 j=1, ncol
         il(j+1) = il(j)+il(j+1)
 7    continue
c     
c     set iord
c     
      do 8 j=1, n
         kol = kolrs(j) 
         iord(j) = il(kol)
         il(kol) = il(kol)+1
 8    continue
c     
c     shift il back 
c     
      do 9 j=ncol,1,-1
         il(j+1) = il(j)
 9    continue
      il(1) = 1
c     
      return
 99   ierr = 1
      return
c----end-of-multic------------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------

      subroutine indset0 (n,ja,ia,nset,iord,riord,sym,iptr)
      integer, intent(in) :: n, iptr
      integer, intent(inout) :: nset, ja(*),ia(*),riord(*),iord(*)
      logical, intent(in) :: sym
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering
c----------------------------------------------------------------------
c parameters:
c ----------
c n      = row dimension of matrix
c ja, ia = matrix pattern in CRS format
c nset   = (output) number of elements in the independent set
c iord   = permutation array corresponding to the independent set 
c          ordering. Row number i will become row number iord(i) in 
c          permuted matrix.
c riord  = reverse permutation array. Row number i in the permutated 
c          matrix is row number riord(i) in original matrix. 
c----------------------------------------------------------------------
c notes: works for CSR, MSR, and CSC formats but assumes that the
c matrix has a symmetric structure. 
c---------------------------------------------------------------------- 
c local variables
c 
      integer j, k1, k2, nod, k, mat, ipos, nummat
      do 1 j=1, n
         iord(j) = 0
 1    continue
      nummat = 1
      if (.not. sym) nummat = 2
c     
c     iord used as a marker
c     
      nset = 0
      do 12  nod=1, n
         if (iord(nod) .ne. 0) goto 12 
         nset = nset+1
         iord(nod) = 1
c     
c     visit all neighbors of current nod 
c     
         ipos = 0
         do 45 mat=1, nummat
            do 4 k=ia(ipos+nod), ia(ipos+nod+1)-1 
               j = ja(k)
               if (j .ne. nod) iord(j) = 2
 4          continue
            ipos = iptr-1
 45      continue
 12   continue
c     
c     get permutation
c     
      k1 = 0 
      k2 = nset 
      do 6 j=1,n
         if (iord(j) .eq. 1) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
         riord(k) = j         
         iord(j) = k
 6    continue
      return
c---------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------
      subroutine indset1 (n,ja,ia,nset,iord,riord,iw,sym,iptr)
      integer, intent(in) :: n, iptr
      integer, intent(inout) :: nset, ja(*),ia(*),riord(*),iord(*),iw(*)
      logical, intent(in) :: sym
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- with intial 
c order of traversal given by that of min degree. 
c----------------------------------------------------------------------
c parameters:
c ----------
c n      = row dimension of matrix
c ja, ia = matrix pattern in CRS format
c nset   = (output) number of elements in the independent set
c iord   = permutation array corresponding to the independent set 
c          ordering. Row number i will become row number iord(i) in 
c          permuted matrix.
c riord  = reverse permutation array. Row number i in the permutated 
c          matrix is row number riord(i) in original matrix. 
c----------------------------------------------------------------------
c notes: works for CSR, MSR, and CSC formats but assumes that the
c matrix has a symmetric structure. 
c---------------------------------------------------------------------- 
c local variables
      integer j,k1,k2,nummat,nod,k,ipos, mat, ii, imat, iptrm1
c
c     nummat is the number of matrices to loop through (A in symmetric
c     pattern case (nummat=1) or A,and transp(A) otherwise (mummat=2) 
c
      if (sym) then
         nummat = 1
      else
         nummat = 2
      endif
      iptrm1 = iptr-1
c     
c     initialize arrays
c
      do 1 j=1,n
         iord(j) = j 
         riord(j) = j 
         iw(j) = 0 
 1    continue
c     
c     initialize degrees of all nodes
c     
      ipos = 0
      do 100 imat =1,nummat
         do 15 j=1,n
            iw(j) = iw(j) + ia(ipos+j+1)-ia(ipos+j) 
 15      continue
         ipos = iptrm1
 100  continue 
c     
c     call heapsort -- sorts nodes in increasing degree. 
c     
      call HeapSort (iw,iord,riord,n,n) 
c     
c     weights no longer needed -- use iw to store order of traversal.
c     
      do 16 j=1, n
         iw(n-j+1) = iord(j)
         iord(j) = 0
 16   continue
c     
c     iord used as a marker
c     
      nset = 0
      do 12  ii = 1, n
         nod = iw(ii) 
         if (iord(nod) .ne. 0) goto 12 
         nset = nset+1
         iord(nod) = 1
c     
c     visit all neighbors of current nod 
c     
         ipos = 0
         do 45 mat=1, nummat
            do 4 k=ia(ipos+nod), ia(ipos+nod+1)-1 
               j = ja(k)
               if (j .ne. nod) iord(j) = 2
 4          continue
            ipos = iptrm1 
 45      continue
 12   continue
c     
c     get permutation
c     
      k1 = 0 
      k2 = nset 
      do 6 j=1,n
         if (iord(j) .eq. 1) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
         riord(k) = j         
         iord(j) = k
 6    continue
      return
c----------------------------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine indset2(n,ja,ia,nset,iord,riord,iw,sym,iptr)
      integer, intent(in) :: n
      integer, intent(inout) ::nset, iptr,ja(*),ia(*),riord(n),iord(n)
     * ,iw(n)
      logical, intent(in) :: sym
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- local minimization
c using heap strategy -- 
c----------------------------------------------------------------------
c This version for BOTH unsymmetric and symmetric patterns
c----------------------------------------------------------------------
c on entry
c -------- 
c n     = row and column dimension of matrix
c ja    = column indices of nonzero elements of matrix,stored rowwise.
c ia    = pointer to beginning of each row in ja.
c sym   = logical indicating whether the matrix has a symmetric pattern.
c         If not the transpose must also be provided -- appended to the
c         ja, ia structure -- see description of iptr next.
c iptr  = in case the matrix has an unsymmetric pattern,the transpose
c         is assumed to be stored in the same arrays ia,ja. iptr is the
c         location in ia of the pointer to the first row of transp(A).
c         more generally, ia(iptr),...,ia(iptr+n) are the pointers to 
c         the beginnings of rows 1, 2, ...., n+1 (row n+1 is fictitious)
c         of the transpose of A in the array ja. For example,when using 
c         the msr format,one can write:
c          iptr = ja(n+1)
c          ipos = iptr+n+2                ! get the transpose of A:
c          call csrcsc (n,0,ipos,a,ja,ja,a,ja,ja(iptr))    ! and then:
c          call indset(n,ja,ja,nset,iord,riord,iwk,.false.,iptr) 
c
c iw    = work space of length n.
c
c on return: 
c---------- 
c nset  = integer. The number of unknowns in the independent set. 
c iord  = permutation array corresponding to the new ordering. The 
c         first nset unknowns correspond to the independent set.
c riord = reverse permutation array.  
c----------------------------------------------------------------------
c local variables --
c 
      integer j,k1,k2,nummat,nod,k,ipos,i,last,lastlast,jold,jnew,
     *     jo,jn, ichild, imat, iptrm1
c
c     nummat is the number of matrices to loop through (A in symmetric
c     pattern case (nummat=1) or A,and transp(A) otherwise (mummat=2) 
c
      if (sym) then
         nummat = 1
      else
         nummat = 2
      endif
      iptrm1 = iptr-1
c
c     initialize arrays
c
      do 1 j=1,n
         iord(j) = j
         riord(j) = j
         iw(j) = 0 
 1    continue
c
c     initialize degrees of all nodes
c
      ipos = 0
      do 100 imat =1,nummat
         do 15 j=1,n
            iw(j) = iw(j) + ia(ipos+j+1)-ia(ipos+j) 
 15      continue
 100     ipos = iptrm1
c     
c start by constructing a heap
c 
      do 2 i=n/2,1,-1 
         j = i
         call FixHeap (iw,iord,riord,j,j,n) 
 2    continue
c     
c main loop -- remove nodes one by one. 
c 
      last = n
      nset = 0
 3    continue
      lastlast = last
      nod = iord(1) 
c     
c     move first element to end
c 
      call moveback (iw,iord,riord,last) 
      last = last -1 
      nset = nset + 1 
c
c     scan all neighbors of accepted node -- move them to back -- 
c     
      ipos = 0
      do 101 imat =1,nummat      
         do 5 k=ia(ipos+nod),ia(ipos+nod+1)-1
            jold = ja(k)
            jnew = riord(jold)
            if (jold .eq. nod .or. jnew .gt. last) goto 5 
            iw(jnew) = -1
            call HeapInsert (iw,iord,riord,jnew,ichild,jnew) 
            call moveback (iw,iord,riord,last) 
            last = last -1 
 5       continue 
         ipos = iptrm1
 101  continue
c
c update the degree of each edge
c 
         do 6 k=last+1,lastlast-1
            jold = iord(k) 
c     
c     scan the neighbors of current node
c     
         ipos = 0
         do 102 imat =1,nummat
            do 61 i=ia(ipos+jold),ia(ipos+jold+1)-1 
               jo = ja(i) 
               jn = riord(jo) 
c
c     consider this node only if it has not been moved          
c     
               if (jn .gt. last) goto 61
c     update degree of this neighbor 
               iw(jn) = iw(jn)-1
c     and fix the heap accordingly
               call HeapInsert (iw,iord,riord,jn,ichild,jn)
 61         continue
            ipos = iptrm1
 102     continue
 6    continue
c
c     stopping test -- end main "while"loop 
c
      if (last .gt. 1) goto 3
      nset = nset + last
c
c     rescan all nodes one more time to determine the permutations 
c
      k1 = 0 
      k2 = nset
      do 7 j=n,1,-1
         if (iw(j) .ge. 0) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
         riord(k) = iord(j) 
 7    continue
      do j=1,n
         iord(riord(j)) = j
      enddo
      return
c----------------------------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine indset3(n,ja,ia,nset,iord,riord,iw,sym,iptr)
      integer, intent(in) :: n
      integer, intent(inout) :: nset,iptr,ja(*),ia(*),riord(n),iord(n)
     * ,iw(n) 
      logical, intent(in) :: sym 
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- local minimization
c using heap strategy -- VERTEX COVER ALGORITHM --  
c ASSUMES MSR FORMAT (no diagonal element) -- ADD A SWITCH FOR CSR -- 
c----------------------------------------------------------------------
c This version for BOTH unsymmetric and symmetric patterns
c----------------------------------------------------------------------
c on entry
c -------- 
c n     = row and column dimension of matrix
c ja    = column indices of nonzero elements of matrix,stored rowwise.
c ia    = pointer to beginning of each row in ja.
c sym   = logical indicating whether the matrix has a symmetric pattern.
c         If not the transpose must also be provided -- appended to the
c         ja, ia structure -- see description of iptr next.
c iptr  = in case the matrix has an unsymmetric pattern,the transpose
c         is assumed to be stored in the same arrays ia,ja. iptr is the
c         location in ia of the pointer to the first row of transp(A).
c         more generally, ia(iptr),...,ia(iptr+n) are the pointers to 
c         the beginnings of rows 1, 2, ...., n+1 (row n+1 is fictitious)
c         of the transpose of A in the array ja. For example,when using 
c         the msr format,one can write:
c          iptr = ja(n+1)
c          ipos = iptr+n+2                ! get the transpose of A:
c          call csrcsc (n,0,ipos,a,ja,ja,a,ja,ja(iptr))    ! and then:
c          call indset(n,ja,ja,nset,iord,riord,iwk,.false.,iptr) 
c
c iw    = work space of length n.
c
c on return: 
c---------- 
c nset  = integer. The number of unknowns in the independent set. 
c iord  = permutation array corresponding to the new ordering. The 
c         first nset unknowns correspond to the independent set.
c riord = reverse permutation array.  
c----------------------------------------------------------------------
c local variables --
c 
      integer :: j,nummat,nod,k,ipos,i,lastnset,jold,jnew, nnz, ideg
      integer :: iptrm1, imat
c
c     nummat is the number of matrices to loop through (A in symmetric
c     pattern case (nummat=1) or A,and transp(A) otherwise (mummat=2) 
c
      if (sym) then
         nummat = 1
      else
         nummat = 2
      endif
      iptrm1 = iptr-1
c
c     initialize arrays
c
      do 1 j=1,n
         riord(j) = j
         iord(j) = j
         iw(j) = 0 
 1    continue
c
c     initialize degrees of all nodes
c
      nnz = 0
      ipos = 0
      do 100 imat =1,nummat
         do 15 j=1,n
            ideg = ia(ipos+j+1)-ia(ipos+j) 
            iw(j) = iw(j) + ideg 
            nnz = nnz + ideg
 15      continue
 100     ipos = iptrm1
c
c     number of edges
c     
         if (sym) nnz = 2*nnz
c     
c start by constructing a Max heap
c 
      do 2 i=n/2,1,-1 
         j = i
         call FixHeapM (iw,riord,iord,j,j,n) 
 2    continue
      nset = n
c----------------------------------------------------------------------     
c main loop -- remove nodes one by one. 
c----------------------------------------------------------------------
 3    continue
      lastnset = nset
      nod = riord(1) 
c     
c     move first element to end
c 
      call movebackM (iw,riord,iord,nset) 
      nnz = nnz - iw(nset) 
      nset = nset -1 
c
c     scan all neighbors of accepted node -- 
c     
      ipos = 0
      do 101 imat =1,nummat      
         do 5 k=ia(ipos+nod),ia(ipos+nod+1)-1
            jold = ja(k)
            jnew = iord(jold)
            if (jold .eq. nod .or. jnew .gt. nset) goto 5 
            iw(jnew) = iw(jnew) - 1
            nnz = nnz-1 
            call FixHeapM (iw,riord,iord,jnew,jnew,nset) 
 5       continue 
         ipos = iptrm1
 101  continue
c      
      if (nnz .gt. 0) goto 3
      return
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine HeapSort (a,ind,rind,n,ncut)
      integer, intent(in) :: n, ncut
      integer, intent(inout) :: a(*),ind(n),rind(n)
c----------------------------------------------------------------------  
c integer version -- min heap sorts decreasinly. 
c---------------------------------------------------------------------- 
c sorts inger keys in array a increasingly and permutes the companion
c array ind rind accrodingly. 
c n    = size of array
c ncut = integer indicating when to cut the process.the process is
c        stopped after ncut outer steps of the heap-sort algorithm.
c        The first ncut values are sorted and they are the smallest
c        ncut values of the array.
c----------------------------------------------------------------------
c local variables 
c
      integer i,last, j,jlast
c   
c    Heap sort algorithm ---
c    
c    build heap 
       do 1 i=n/2,1,-1 
          j = i
          call FixHeap (a,ind,rind,j,j,n) 
 1    continue
c     
c   done -- now remove keys one by one 
c
      jlast = max(2,n-ncut+1)
      do 2 last=n,jlast,-1 
         call moveback (a,ind,rind,last) 
 2    continue
      return
      end 
c----------------------------------------------------------------------
      subroutine FixHeap (a,ind,rind,jkey,vacant,last)
      integer a(*),ind(*),rind(*),jkey,vacant,last
c----------------------------------------------------------------------
c     inserts a key (key and companion index) at the vacant position 
c     in a (min) heap -
c arguments
c     a(1:last)    = real array
c     ind(1:last)  = integer array -- permutation of initial data
c     rind(1:last) = integer array -- reverse permutation 
c     jkey         = position of key to be inserted. a(jkey) 
c                    will be inserted into the heap
c     vacant       = vacant where a key is to be inserted
c     last         = number of elements in the heap.
c----------------------------------------------------------------------
c local variables 
c
      integer child,lchild,rchild,xkey, ikey
      xkey = a(jkey) 
      ikey = ind(jkey) 
      lchild = 2*vacant
 1    continue 
      rchild = lchild+1  
      child = lchild  
      if (rchild .le. last .and. a(rchild) .lt. a(child))
     *     child = rchild 
      if (xkey .le. a(child) .or. child .gt. last) goto 2 
      a(vacant) = a(child) 
      ind(vacant) = ind(child) 
      rind(ind(vacant)) = vacant 
      vacant = child 
      lchild = 2*vacant 
      if (lchild .le.  last) goto 1
 2    continue 
      a(vacant) = xkey 
      ind(vacant) = ikey 
      rind(ikey) = vacant 
      return
c----------------------------------------------------------------------      
      end
c----------------------------------------------------------------------      
      subroutine HeapInsert (a,ind,rind,jkey,child,node) 
      integer a(*),ind(*),rind(*),jkey,child,node
c----------------------------------------------------------------------
c inserts a key to a heap from `node'. Checks values up
c only -- i.e.,assumes that the subtree (if any) whose root
c is node is such that the keys are all inferior to those
c to ge inserted. 
c 
c child is where the key ended up.
c---------------------------------------------------------------------- 
c---- local variables 
      integer parent,xkey,ikey
      xkey = a(jkey) 
      ikey = ind(jkey) 
c      node = node + 1 
      a(node) = xkey 
      ind(node) = ikey 
      rind(ikey) = node 
      if (node .le. 1) return
      child=node 
 1    parent = child/2  
      if (a(parent) .le. a(child)) goto 2
      call interchange(a,ind,rind,child,parent) 
      child = parent 
      if (child .gt. 1) goto 1 
 2    continue
      return
      end 
c-----------------------------------------------------------------------
      subroutine interchange (a,ind,rind,i,j)
      integer a(*),ind(*),rind(*),i,j 
      integer tmp,itmp
      tmp = a(i)  
      itmp = ind(i) 
c
      a(i) = a(j) 
      ind(i) = ind(j) 
c
      a(j) = tmp
      ind(j) = itmp 
      rind(ind(j)) = j
      rind(ind(i)) = i
c
      return
      end 
c----------------------------------------------------------------------
      subroutine moveback (a,ind,rind,last) 
      integer a(*),ind(*),rind(*),last 
c moves the front key to the back and inserts the last
c one back in from the top -- 
c 
c local variables 
c
      integer vacant,xmin, imin
c
         vacant = 1 
         xmin = a(vacant)
         imin = ind(vacant) 
         call FixHeap(a,ind,rind,last,vacant,last-1) 
         a(last) = xmin 
        ind(last) = imin 
         rind(ind(last)) = last
c
         return
         end 
c----------------------------------------------------------------------
      subroutine FixHeapM (a,ind,rind,jkey,vacant,last)
      integer a(*),ind(*),rind(*),jkey,vacant,last
c----      
c     inserts a key (key and companion index) at the vacant position 
c     in a heap -  THIS IS A MAX HEAP VERSION
c arguments
c     a(1:last)    = real array
c     ind(1:last)  = integer array -- permutation of initial data
c     rind(1:last) = integer array -- reverse permutation 
c     jkey         = position of key to be inserted. a(jkey) 
c                    will be inserted into the heap
c     vacant       = vacant where a key is to be inserted
c     last         = number of elements in the heap.
c----     
c local variables 
c
      integer child,lchild,rchild,xkey, ikey
      xkey = a(jkey) 
      ikey = ind(jkey) 
      lchild = 2*vacant
 1    continue 
      rchild = lchild+1  
      child = lchild  
      if (rchild .le. last .and. a(rchild) .gt. a(child))
     *     child = rchild 
      if (xkey .ge. a(child) .or. child .gt. last) goto 2
      a(vacant) = a(child) 
      ind(vacant) = ind(child) 
      rind(ind(vacant)) = vacant 
      vacant = child 
      lchild = 2*vacant 
      if (lchild .le.  last) goto 1
 2    continue 
      a(vacant) = xkey 
      ind(vacant) = ikey 
      rind(ikey) = vacant 
      return
      end
c      
      subroutine HeapInsertM (a,ind,rind,jkey,child,node) 
      integer a(*),ind(*),rind(*),jkey,child,node
c----------------------------------------------------------------------
c inserts a key to a heap from `node'. Checks values up
c only -- i.e.,assumes that the subtree (if any) whose root
c is node is such that the keys are all inferior to those
c to ge inserted. 
c 
c child is where the key ended up.
c---------------------------------------------------------------------- 
c---- local variables 
      integer parent,xkey,ikey
      xkey = a(jkey) 
      ikey = ind(jkey) 
c      node = node + 1 
      a(node) = xkey 
      ind(node) = ikey 
      rind(ikey) = node 
      if (node .le. 1) return
      child=node 
 1    parent = child/2  
      if (a(parent) .ge. a(child)) goto 2
      call interchange(a,ind,rind,child,parent) 
      child = parent 
      if (child .gt. 1) goto 1 
 2    continue
      return
      end 
c----------------------------------------------------------------------
      subroutine movebackM (a,ind,rind,last) 
      integer a(*),ind(*),rind(*),last
c----------------------------------------------------------------------
c moves the front key to the back and inserts the last
c one back in from the top --  MAX HEAP VERSION 
c----------------------------------------------------------------------
c 
c local variables 
c
      integer vacant,xmin, imin
c
      vacant = 1 
      xmin = a(vacant)
      imin = ind(vacant) 
      call FixHeapM(a,ind,rind,last,vacant,last-1) 
      a(last) = xmin 
      ind(last) = imin 
      rind(ind(last)) = last
c----------------------------------------------------------------------
      return
      end 
c----------------------------------------------------------------------
      subroutine indsetr (n,ja,ia,nset,iord,riord,sym,iptr)
      integer, intent(in) :: n
      integer, intent(inout) :: nset, ja(*),ia(*),riord(*),iord(*)
      logical, intent(in) :: sym
      integer :: ipos, iptr, ii, nummat
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- RANDOM TRAVERSAL -- 
c----------------------------------------------------------------------
c parameters:
c ----------
c n      = row dimension of matrix
c ja, ia = matrix pattern in CRS format
c nset   = (output) number of elements in the independent set
c iord   = permutation array corresponding to the independent set 
c          ordering. Row number i will become row number iord(i) in 
c          permuted matrix.
c riord  = reverse permutation array. Row number i in the permutated 
c          matrix is row number riord(i) in original matrix. 
c----------------------------------------------------------------------
c notes: works for CSR, MSR, and CSC formats but assumes that the
c matrix has a symmetric structure. 
c---------------------------------------------------------------------- 
c local variables
c 
      integer j, k1, k2, nod, k, mat, iseed
      do 1 j=1, n
         iord(j) = 0
 1    continue
c
c generate random permutation
c 
      iseed = 0 
      call rndperm(n, riord, iseed)
      write (8,'(10i6)') (riord(j),j=1,n) 
c
      nummat = 1
      if (.not. sym) nummat = 2
c     
c iord used as a marker
c 
      nset = 0
      do 12  ii=1, n
         nod = riord(ii) 
         if (iord(nod) .ne. 0) goto 12 
         nset = nset+1
         iord(nod) = 1
c
c visit all neighbors of current nod 
c     
         ipos = 0
         do 45 mat=1, nummat
            do 4 k=ia(ipos+nod), ia(ipos+nod+1)-1 
               j = ja(k)
               if (j .ne. nod) iord(j) = 2
 4          continue
            ipos = iptr-1
 45      continue
 12   continue
c
c get permutation
c     
      k1 = 0 
      k2 = nset 
      do 6 j=1,n
         if (iord(j) .eq. 1) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
            riord(k) = j         
            iord(j) = k
 6    continue
      return
c----------------------------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine rndperm(n,iord,iseed) 
      integer n, iseed, iord(n) 
c----------------------------------------------------------------------
c this subroutine will generate a pseudo random permutation of the
c n integers 1,2, ...,n.
c iseed is the initial seed. any integer.
c----------------------------------------------------------------------
c local
c
      integer i, j, itmp
      integer, external :: irand
c----------------------------------------------------------------------
      do j=1, n
         iord(j) = j
      enddo
c
      do i=1, n
         j = mod(irand(0),n) + 1
         itmp = iord(i) 
         iord(i) = iord(j) 
         iord(j) = itmp
      enddo
c----------------------------------------------------------------------
      return
c----------------------------------------------------------------------
      end

      subroutine amub_countonly(nrow,ncol,a,ja,ia,b,jb,ib,iw,len)
      integer, intent(in) :: nrow, ncol
      integer, intent(out) :: len
      real*8 , intent(inout) :: a(*), b(*)
      integer, intent(inout) :: ja(*),jb(*),ia(nrow+1),ib(*),iw(ncol)
      integer :: jj, k, kb, jcol, jpos, j, ii, ka
!-----------------------------------------------------------------------
! computes number of nonzeros in matrix product C = A B
!-----------------------------------------------------------------------
! on entry:
! ---------
! nrow  = integer. The row dimension of A = row dimension of C
! ncol  = integer. The column dimension of B = column dimension of C
!
! a,
! ja,
! ia   = Matrix A in compressed sparse row format.
!
! b,
! jb,
! ib    =  Matrix B in compressed sparse row format.
!
! on return:
!----------
! len
!
! work arrays:
!------------
! iw    = integer work array of length equal to the number of
!         columns in A.
! Note:
!-------
!   The row dimension of B is not needed. However there is no checking
!   on the condition that ncol(A) = nrow(B).
!
!-----------------------------------------------------------------------
      len = 0
!     initialize array iw.
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
!
      do 500 ii=1, nrow
!     row i
         do 200 ka=ia(ii), ia(ii+1)-1
            jj   = ja(ka)
            do 100 kb=ib(jj),ib(jj+1)-1
               jcol = jb(kb)
               jpos = iw(jcol)
               if (jpos .eq. 0) then
                  len = len+1
                  iw(jcol)= len
               endif
 100        continue
 200     continue
         do 201 k=1,ncol
            iw(k) = 0
 201     continue
 500  continue
      return
!-------------end-of-amub-----------------------------------------------
!-----------------------------------------------------------------------
      end