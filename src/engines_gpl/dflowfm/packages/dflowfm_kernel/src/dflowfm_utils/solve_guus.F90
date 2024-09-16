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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

 subroutine inireduce()
    use m_reduce
    use m_flowparameters, only: Noderivedtypes
    use m_flowgeom
    use m_partitioninfo
    use m_flowparameters, only: icgsolver, ipre
    use m_alloc
    use system_utils, only: ARCH
    use m_qnerror

    ! subroutine to intialise the following variables:
    ! noactive
    ! nbrstk
    implicit none
    integer m, ierr
    integer idmn, k

    lintot = lnx; nodtot = ndx

    allocate (ij(nodtot), stat=ierr)
    call aerr('ij(nodtot)', ierr, nodtot)

    allocate (nodbr2(nodtot), stat=ierr); nodbr2 = 0
    call aerr('nodbr2(nodtot)', ierr, nodtot)

    allocate (nbrstk(nodtot), stat=ierr); nbrstk = 0
    call aerr('nbrstk(nodtot)', ierr, nodtot)

    allocate (nodstk(nodtot), stat=ierr); nodstk = 0
    call aerr('nodstk(nodtot)', ierr, nodtot)

    if (allocated(noel)) deallocate (noel)
    allocate (noel(nodtot), stat=ierr); noel = 0
    call aerr('noel(nodtot)', ierr, nodtot)

    if (allocated(noel0)) deallocate (noel0)
    allocate (noel0(nodtot), stat=ierr); noel0 = 0
    call aerr('noel0(nodtot)', ierr, nodtot)

    if (allocated(ia)) deallocate (ia)
    allocate (ia(nodtot), stat=ierr)
    call aerr('ia(nodtot)', ierr, nodtot)

    if (allocated(row)) deallocate (row)
    allocate (row(nodtot), stat=ierr)
    call aerr('row(nodtot)', ierr, nodtot)

    if (allocated(lv2)) deallocate (lv2)
    allocate (lv2(lintot), stat=ierr); lv2 = 0
    call aerr('lv2(lintot)', ierr, lintot)

    if (allocated(jagauss)) deallocate (jagauss)
    allocate (jagauss(nodtot), stat=ierr); jagauss = 1
    call aerr('jagauss(nodtot)', ierr, nodtot)

! check solver applicability
    if (jampi == 0) then
       if (icgsolver == 6) then
          icgsolver = 4 ! lets prevent hk geting tired
       end if
       if (icgsolver > 4 .and. icgsolver /= 6 .and. icgsolver /= 10 .and. icgsolver /= 44 .and. icgsolver /= 8) then
          write (6, *) 'icgsolver=', icgsolver
          call qnerror('inireduce: inappropriate Krylov solver', ' ', ' ')
          icgsolver = 1
       end if
    else
       if (icgsolver == 4) then ! as a service to the user too
          if (ARCH == 'linux') then ! on linux
             icgsolver = 6
          else ! on windows
             icgsolver = 7
          end if
       else if (icgsolver < 4 .or. icgsolver == 44) then
          write (6, *) 'icgsolver=', icgsolver
          call qnerror('inireduce: inappropriate Krylov solver', ' ', ' ')
       end if
    end if

    if (icgsolver /= 4) then
       Noderivedtypes = 0
    end if

! set preconditioner
    if (icgsolver == 7) then
       ipre = 4
    else if (icgsolver == 88) then ! DEBUG
       icgsolver = 7 ! DEBUG
       ipre = 0 ! DEBUG
    else
       ipre = 0
    end if

    if (jampi == 1) then
!   do not eliminate ghost- and sendnodes
       do idmn = 0, ndomains - 1
          do m = 1, nghostlist_s(ndomains - 1)
             ndn = ighostlist_s(m)
             if (ndn > nodtot) then
                call qnerror('ighost: ndn > nodtot', ' ', ' ')
             end if
             jagauss(ndn) = 0
          end do

          do m = 1, numsend_s
             ndn = isendlist_s(m)
             if (ndn > nodtot) then
                call qnerror('isend: ndn > nodtot', ' ', ' ')
             end if
             jagauss(ndn) = 0
          end do
       end do
    end if

! if (maxdge > 0) then
!
!    do L = Lnxi+1, lnx
!       k2 = ln(2,L)
!       jagauss(k2) = 0   ! no gauss on open bnd
!    enddo
!
! endif

    do k = 1, ndx
       if (nd(k)%lnx == 0) then ! isolated points, may be caused by thindams (dry points in d3d etc)
          jagauss(k) = 0
       end if
    end do

    ij%l = 0
    do nbr = 1, lintot
       ! m=linev(nbr,1)
       m = nbr
       nodl = ln(1, nbr)
       nodr = ln(2, nbr)
       if (m > 0) call ijtrue(nodl, nodr)
       if (nodl /= 0) nbrstk(nodl) = nodl
       if (nodr /= 0) nbrstk(nodr) = nodr
    end do
    noactive = 0
    do ndn = 1, nodtot
       if (nbrstk(ndn) == ndn) then
          noactive = noactive + 1
       end if
    end do

 end subroutine inireduce

 subroutine pointonstack
    use m_reduce
    use m_alloc
    !
    ! a point to be eliminated is found and placed on the elimination stack
    !
    implicit none
    integer i, j, n, m, ierr

    nogauss = nogauss + 1
    noel(nogauss) = ndn
    nbrstk(ndn) = 0
    row(ndn)%l = nodbr2(ndn)
    ! GD: memory leak?
    allocate (row(ndn)%j(nodbr2(ndn)), row(ndn)%a(nodbr2(ndn)), stat=ierr)
    call aerr('row(ndn)%j(nodbr2(ndn)),row(ndn)%a(nodbr2(ndn))', ierr, nodbr2(ndn))
    m = 0

    do n = 1, ij(ndn)%l
       if (ij(ndn)%b(n)) then
          m = m + 1
          row(ndn)%j(m) = ij(ndn)%j(n)
       end if
    end do

    do n = 1, nodbr2(ndn)
       j = row(ndn)%j(n)
       call ijfalse(ndn, j)
    end do

    do m = 1, nodbr2(ndn)
       i = row(ndn)%j(m)
       do n = m + 1, nodbr2(ndn)
          j = row(ndn)%j(n)
          call ijtrue(i, j)
       end do
    end do

    do n = 1, nodbr2(ndn)
       j = row(ndn)%j(n)
       if (nodbr2(j) < mindgr) mindgr = nodbr2(j)
    end do
    return
 end subroutine pointonstack

 !> Computes minimum degree and fills stack with nodes of the minimum degree
 !
 subroutine mindegree
    use m_reduce
    use m_partitioninfo
    implicit none
    integer n
    mindgr = nodtot + 10
    do n = 1, nodtot

       if (nbrstk(n) /= 0 .and. jagauss(n) == 1) then
          ndn = nbrstk(n)
          if (nodbr2(ndn) < mindgr) then
             mindgr = nodbr2(ndn)
          end if
       end if
    end do
    return
 end subroutine mindegree

 !> Put new fill in on stack of Gauss-CG solver
 subroutine ijtrue(i, j)
    use m_reduce
    use m_alloc
    implicit none
    integer, intent(in) :: i !< First node number
    integer, intent(in) :: j !< Second node number
    integer :: n, k1, k2, ierr
    integer :: cursize, minsize

    if (allocated(intbuf)) then
       cursize = size(intbuf)
    else
       cursize = 0
    end if
    minsize = max(2000, cursize, ij(i)%l, ij(j)%l)

    if (cursize < minsize) then
       ! Exponential growth to prevent 1000s of buffer realloc calls
       minsize = ceiling(1.2 * minsize)
       call realloc(intbuf, minsize, stat=ierr, keepExisting=.false.)
       call realloc(logbuf, minsize, stat=ierr, keepExisting=.false.)
    end if

    k1 = ij(i)%l
    do n = 1, ij(i)%l
       k2 = ij(i)%j(n)
       if (ij(i)%j(n) == j) then
          if (ij(i)%b(n)) return
          write (*, '('' error in ij stack'')')
          call error('ijtrue', ' ', ' ')
          stop
       end if
    end do
    if (ij(i)%l /= 0) then
       logbuf(1:ij(i)%l) = ij(i)%b(1:ij(i)%l)
       intbuf(1:ij(i)%l) = ij(i)%j(1:ij(i)%l)
       deallocate (ij(i)%j, ij(i)%b, stat=ierr)
       call aerr('ij(i)%j,ij(i)%b)', ierr, -2 * ij(i)%l)

       nodbr2(i) = nodbr2(i) + 1; ij(i)%l = ij(i)%l + 1

       allocate (ij(i)%j(ij(i)%l), ij(i)%b(ij(i)%l), stat=ierr)
       call aerr('ij(i)%j(ij(i)%l),ij(i)%b(ij(i)%l)', ierr, ij(i)%l)

       ij(i)%b(1:ij(i)%l - 1) = logbuf(1:ij(i)%l - 1)
       ij(i)%j(1:ij(i)%l - 1) = intbuf(1:ij(i)%l - 1)
       ij(i)%b(ij(i)%l) = .true.
       ij(i)%j(ij(i)%l) = j
    else
       allocate (ij(i)%b(1), ij(i)%j(1), stat=ierr)
       call aerr('ij(i)%b(1),ij(i)%j(1)', ierr, 1)

       nodbr2(i) = 1; ij(i)%l = 1
       ij(i)%b(1) = .true.; ij(i)%j(1) = j
    end if
    if (ij(j)%l /= 0) then
       logbuf(1:ij(j)%l) = ij(j)%b(1:ij(j)%l)
       intbuf(1:ij(j)%l) = ij(j)%j(1:ij(j)%l)
       deallocate (ij(j)%j, ij(j)%b, stat=ierr)
       call aerr('ij(j)%j,ij(j)%b', ierr, -2 * ij(j)%l)
       nodbr2(j) = nodbr2(j) + 1; ij(j)%l = ij(j)%l + 1

       allocate (ij(j)%j(ij(j)%l), ij(j)%b(ij(j)%l), stat=ierr)
       call aerr('ij(j)%j(ij(j)%l),ij(j)%b(ij(j)%l)', ierr, 2 * ij(j)%l)

       ij(j)%b(1:ij(j)%l - 1) = logbuf(1:ij(j)%l - 1)
       ij(j)%j(1:ij(j)%l - 1) = intbuf(1:ij(j)%l - 1)
       ij(j)%b(ij(j)%l) = .true.
       ij(j)%j(ij(j)%l) = i
    else
       allocate (ij(j)%b(1), ij(j)%j(1), stat=ierr)
       call aerr('ij(j)%b(1),ij(j)%j(1)', ierr, 1)
       nodbr2(j) = 1; ij(j)%l = 1
       ij(j)%b(1) = .true.; ij(j)%j(1) = i
    end if
 end subroutine ijtrue

 subroutine ijfalse(i, j)
    use m_reduce
    implicit none
    integer i, j, n
    do n = 1, ij(j)%l
       if (ij(j)%j(n) == i) then
          if (ij(j)%b(n)) then
             ij(j)%b(n) = .false.
             nodbr2(j) = nodbr2(j) - 1
             return
          else
             write (*, '('' error in ijfalse'')')
             stop
          end if
       end if
    end do
    write (*, '('' connection does not exist, error in ijfalse'')')
    stop
 end subroutine ijfalse

 !>  make the matrix and packed matrix array administration
 !>    ia(i)%l : number of non-zero lower-diagonal entries for row i (Matrix is assumed symmetrical)
 !>    ia(i)%j : array with the non-zero lower-diagonal column indices
 !>    ia(i)%a : for row i, array with pointers to the packed matrix array data for matrix elements
 !>
 !>              A(i,j) = Apacked(idx)
 !>              where
 !>              j   = ia(i)%j(k),
 !>              idx = ia(i)%a(k), for 1<=k<=ia(i)%l
 !>
 !>    lv2(L)  : pointer to the lower-diagonal off-diagonal entry of link L in the packed matrix array
 subroutine cijadres
    use m_reduce
    use m_flowgeom
    use m_alloc

    implicit none
    integer j, n, iadres, jj, iad2, ierr
    integer, external :: ijadr
    iadres = 0
    do n = 1, nodtot
       ia(n)%l = 0
       do jj = 1, ij(n)%l
          j = ij(n)%j(jj)
          if (j < n) ia(n)%l = ia(n)%l + 1
       end do
       if (ia(n)%l > 0) then
          iad2 = 0
! GD: memory leak
!     if(allocated(ia(n)%j)) deallocate(ia(n)%j)
!     if(allocated(ia(n)%a)) deallocate(ia(n)%a)

          allocate (ia(n)%j(ia(n)%l), ia(n)%a(ia(n)%l), stat=ierr)
          call aerr('ia(n)%j(ia(n)%l),ia(n)%a(ia(n)%l)', ierr, 2 * ia(n)%l)
          do jj = 1, ij(n)%l
             j = ij(n)%j(jj)
             if (j < n) then
                iad2 = iad2 + 1
                iadres = iadres + 1
                ia(n)%j(iad2) = j
                ia(n)%a(iad2) = iadres
             end if
          end do
       end if
    end do
    do n = 1, lintot
       nodl = ln(1, n)
       nodr = ln(2, n)
       ! if (linev(n,1)>0)
       lv2(n) = ijadr(nodl, nodr)
    end do
    ijstck = iadres
 end subroutine cijadres

!> get the packed matrix array index for matrix element (i,j)
!>
!>   A(i,j) = Apacked(ijadr(i,j))
 integer function ijadr(i, j)
    use m_reduce

    implicit none
    integer i, j, k, m, n
    m = max(i, j)
    n = min(i, j)
    do k = 1, ia(m)%l
       if (ia(m)%j(k) == n) then
          ijadr = ia(m)%a(k)
          return
       end if
    end do
    ijadr = 0
    return
 end function ijadr

 subroutine inigauss
    !
    ! This subroutine initialises the adresses of the fill in coefficients during Gauss elimination
    !
    use m_reduce
    use m_alloc

    implicit none
    integer i, j, m, n, ijij, nn, ierr
    integer, external :: ijadr
    do n = 1, nogauss
       ndn = noel(n)
       ijij = 0
       do m = 1, nodbr2(ndn)
          i = row(ndn)%j(m)
          do nn = m + 1, nodbr2(ndn)
             j = row(ndn)%j(nn)
             ijij = ijij + 1
          end do
       end do
       allocate (row(ndn)%f(ijij), stat=ierr)
       call aerr('row(ndn)%f(ijij)', ierr, ijij)
       ijij = 0
       do m = 1, nodbr2(ndn)
          i = row(ndn)%j(m)
          row(ndn)%a(m) = ijadr(ndn, i)
          do nn = m + 1, nodbr2(ndn)
             j = row(ndn)%j(nn)
             ijij = ijij + 1
             row(ndn)%f(ijij) = ijadr(i, j)
          end do
       end do
    end do

! determine ccc array size
    npmax = 0
    do n = 1, nogauss
       ndn = noel(n)
       npmax = max(npmax, nodbr2(ndn))
    end do

    return
 end subroutine inigauss

 subroutine inicg
    use m_reduce
    use m_alloc

    implicit none
    integer i, j, n, m, ierr
    integer, external :: ijadr
    nocg = 0
    do i = 1, nodtot
       ndn = nbrstk(i)
       if (ndn > 0) then
          allocate (row(ndn)%j(nodbr2(ndn)), row(ndn)%a(nodbr2(ndn)), stat=ierr)
          call aerr('inicg', ierr, 2 * nodbr2(ndn))
          row(ndn)%l = nodbr2(ndn)
          nocg = nocg + 1; noel(nogauss + nocg) = ndn; 
          m = 0
          do n = 1, ij(ndn)%l
             if (ij(ndn)%b(n)) then
                m = m + 1
                row(ndn)%j(m) = ij(ndn)%j(n)
             end if
          end do
          do j = 1, row(ndn)%l
             row(ndn)%a(j) = ijadr(ndn, row(ndn)%j(j))
          end do
       end if
    end do
 end subroutine inicg

 subroutine solve_matrix(s1, ndx, itsol)
    use Timers
    use m_flowparameters, only: noderivedtypes, icgsolver, ipre
    use m_reduce
    use m_flowtimes, only: handle_sol
    use m_partitioninfo, only: my_rank, ndomains
    use m_timer
    use m_qnerror
    use m_solve_jacobi

#ifdef HAVE_PETSC
    use m_petsc
#endif

    implicit none

    integer :: ndx
    double precision :: s1(ndx)
    integer :: itsol
    integer :: ierror ! error (1) or not (0)

    ierror = 0

    if (jatimer == 1) call starttimer(ITOTALSOL)

    call timstrt('Conjugate Gradient solver', handle_sol)

    if (jatimer == 1) then
       call starttimer(IGAUSSEL)
    end if

    if (Noderivedtypes >= 2) then
       call gauss_eliminationjipjan()
    else
       call gauss_elimination()
    end if

    if (jatimer == 1) then
       call stoptimer(IGAUSSEL)
    end if

    if (jatimer == 1) call starttimer(ICG)

    if (icgsolver == 1) then
       call conjugategradient_omp(s1, ndx, ipre) ! ipre = 0,1    ! faster, solution depends on thread sequence
    else if (icgsolver == 2) then
       call conjugategradient_omp_threadsafe(s1, ndx, ipre) ! ipre = 0,1    ! slower, solution always identical
    else if (icgsolver == 3) then
       call conjugategradient(s1, ndx, ipre) ! ipre = 0,1,2  ! no omp
    else if (icgsolver == 4 .or. icgsolver == 44) then
       if (nocg > 10 .or. Noderivedtypes == 5) then
          if (icgsolver == 4) then ! thread-safe
             call conjugategradientSAAD(ddr, s1, ndx, nocgiter, 1, 1, ierror) ! Saad, always using omp and ILUD preconditioner
          else
             call conjugategradientSAAD(ddr, s1, ndx, nocgiter, 1, 0, ierror) ! Saad, always using omp and ILUD preconditioner
          end if
          if (ierror == 1) goto 1234
       else
!      do *not* use Saad solver when nocg < 3
          call conjugategradient(s1, ndx, ipre) ! ipre = 0,1,2  ! no omp
       end if
    else if (icgsolver == 5) then
!    call conjugategradientSAAD_global(s1,ndx,nocgiter)
    else if (icgsolver == 6) then
#ifdef HAVE_PETSC
       call conjugategradientPETSC(s1, ndx, nocgiter, 1, ipre) ! 1:always compute preconditioner
       if (nocgiter == -999) then
          ierror = 1
          goto 1234
       end if
#else
       call qnerror('No PETSC solver available', ' ', ' ')
#endif
    else if (icgsolver == 7) then
       call conjugategradient_MPI(s1, ndx, ipre, nocgiter, ierror) ! parallel cg, ipre=0,1: "global" preconditioning, ipre=3,4: block preconditioning
       if (ierror == 1) goto 1234
    else if (icgsolver == 8) then
#ifdef HAVE_PARMS
       nocgiter = 999
       call conjugategradient_parms(s1, ndx, nocgiter)
#else
       call qnerror('No pARMS solver available', ' ', ' ')
#endif
    else if (icgsolver == 9 .or. icgsolver > 90) then
       call testsolver(Ndx, s1, nocgiter, ierror)
    else if (icgsolver == 10) then
       call solve_jacobi(s1, ndx, nocgiter)
    else
       call qnerror('no valid solver', ' ', ' ')
    end if

    ierror = 0

! error handling
1234 continue

    if (jatimer == 1) then
       call stoptimer(ICG)
       numcgits = numcgits + nocgiter
    end if

    if (jatimer == 1) call starttimer(IGAUSSSU)

    if (Noderivedtypes >= 1) then
       call gauss_substitutionjipjan(s1, ndx)
    else
       call gauss_substitution(s1, ndx)
    end if

    if (jatimer == 1) call stoptimer(IGAUSSSU)

    if (jatimer == 1) call stoptimer(ITOTALSOL)

    call timstop(handle_sol)

    itsol = nocgiter

    if (ierror /= 0) then

       call writematrix_matlab()

    end if

 end subroutine solve_matrix

 subroutine conjugategradientSAAD(righthandside, s1, ndx, its, jaini, jadosafe, ierror)
    use m_reduce
! use unstruc_messages
    use M_SAAD
    use m_flowgeom, only: kfs
    use MessageHandling
    use m_flowparameters, only: Noderivedtypes
    use m_partitioninfo, only: my_rank
    use m_netw, only: xzw, yzw
    use unstruc_model, only: md_ident
    use m_qnerror
    use m_calls_saad
    
    implicit none
    integer :: ndx, its
    double precision :: s1(ndx)
    double precision, dimension(Ndx), intent(in) :: righthandside !< right-hand side, all cells
    integer, intent(in) :: jaini !< compute preconditioner and permutation (1) or not (0), or initialization only (-1), or ILU solve only (2)
    integer, intent(in) :: jadosafe !< thread-safe (1) or not (0), will set jasafe module variable
    integer, intent(out) :: ierror !< error (1) or not (0)

    integer :: j, jj, n, ntot, na, nietnul, m
    integer :: minp, k

    double precision :: res

    logical, save :: jaoutput = .false.

! ddr (rechterlid), bbr (diag) , ccr (off diag), s1, row, row()%j, row()%a [AvD]

    ierror = 0

    if (nocg <= 0) return

! set jasafe module variable
    jasafe = jadosafe

    if (jaini == -1 .or. jaini == 1) then

       if (Noderivedtypes <= 2) then

          do n = nogauss + 1, nogauss + nocg
             ndn = noel(n) ! guus index
             nietnul = 0
             ntot = row(ndn)%l
             do j = 1, ntot
                jj = row(ndn)%a(j)
                if (ccr(jj) /= 0d0) then
                   i = row(ndn)%j(j)
                   if (kfs(i) == 1) then
                      nietnul = nietnul + 1
                   end if
                end if
             end do
             if (nietnul == 0 .and. jaini == 1) then ! for non-parallel solver only
                noel(n) = -noel(n)
             end if
          end do

       else

          do n = nogauss + 1, nogauss + nocg
             ndn = noel(n) ! guus index
             nietnul = 0
             do m = L1row(ndn), L2row(ndn)
                if (ccr(iarow(m)) /= 0) then
                   if (kfs(jrow(m)) > 0) then
                      nietnul = nietnul + 1
                   end if
                end if
             end do
             if (nietnul == 0 .and. jaini == 1) then ! for non-parallel solver only
                noel(n) = -noel(n)
             end if
          end do

       end if

       ! make matrix L
       nn = 0
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n) ! guus index
          if (ndn > 0) then
             nn = nn + 1
             ngs(ndn) = nn ! guus to saad
          end if
       end do

       nn = 0
       na = 0 ! saad matrix non zero counter
       iao(1) = 1 !

       if (Noderivedtypes <= 3) then

          do n = nogauss + 1, nogauss + nocg
             ndn = noel(n) ! guus index
             if (ndn > 0) then
                nn = nn + 1 ! n - nogauss   ! saad index

                sol(nn) = s1(ndn)
!            rhs(nn)   = ddr(ndn)  ! input argument
                rhs(nn) = righthandside(ndn)

                na = na + 1 ! saad matrix non zero counter
                ao(na) = bbr(ndn)
                jao(na) = nn ! saad row

                ntot = row(ndn)%l
                do j = 1, ntot
                   jj = row(ndn)%a(j)
                   if (ccr(jj) /= 0d0) then
                      i = row(ndn)%j(j)
                      if (kfs(i) == 1) then
                         na = na + 1
                         ao(na) = ccr(jj)
                         jao(na) = ngs(i) ! saad row
                      else ! internal boundary: add to right-hand side
!                      rhs(nn) = rhs(nn) - ccr(jj)*s1(i)
                      end if
                   end if
                end do
                iao(nn + 1) = na + 1 ! saad column, nr of non zeros plus 1
             end if
          end do

       else

          do n = nogauss + 1, nogauss + nocg
             ndn = noel(n) ! guus index
             if (ndn > 0) then
                nn = nn + 1 ! n - nogauss   ! saad index

                sol(nn) = s1(ndn)
!            rhs(nn)   = ddr(ndn)  ! input argument
                rhs(nn) = righthandside(ndn)

                na = na + 1 ! saad matrix non zero counter
                ao(na) = bbr(ndn)
                jao(na) = nn ! saad row

                do m = L1row(ndn), L2row(ndn)
                   jj = iarow(m)
                   if (ccr(jj) /= 0) then
                      i = jrow(m)
                      if (kfs(i) == 1) then
                         na = na + 1
                         ao(na) = ccr(jj)
                         jao(na) = ngs(i) ! saad row
                      else ! internal boundary: add to right-hand side
!                      rhs(nn) = rhs(nn) - ccr(jj)*s1(i)
                      end if
                   end if
                end do
                iao(nn + 1) = na + 1 ! saad column, nr of non zeros plus 1

             end if
          end do

       end if

    else ! always make rhs (eliminate ghostcells)

       if (Noderivedtypes <= 3) then

          do n = nogauss + 1, nogauss + nocg
             nn = n - nogauss ! saad index
             ndn = noel(n) ! guus

             if (ndn > 0) then

                rhs(nn) = righthandside(ndn)
                ntot = row(ndn)%l
                do j = 1, ntot
                   jj = row(ndn)%a(j)
                   if (ccr(jj) /= 0d0) then
                      i = row(ndn)%j(j)
                      if (kfs(i) == 1) then
                      else ! internal boundary: add to right-hand side
!                       rhs(nn) = rhs(nn) - ccr(jj)*s1(i)
                      end if
                   end if
                end do
             end if
          end do
          na = iao(nn + 1) - 1 ! total number of non-zeros

       else

          do n = nogauss + 1, nogauss + nocg
             nn = n - nogauss ! saad index
             ndn = noel(n) ! guus

             if (ndn > 0) then

                rhs(nn) = righthandside(ndn)

                do m = L1row(ndn), L2row(ndn)
                   jj = iarow(m)
                   if (ccr(jj) /= 0) then
                      i = jrow(m)
                      if (kfs(i) == 1) then
                      else ! internal boundary: add to right-hand side
!                      rhs(nn) = rhs(nn) - ccr(jj)*s1(i)
                      end if
                   end if
                end do
             end if
          end do
          na = iao(nn + 1) - 1 ! total number of non-zeros
       end if
    end if

    if (jaoutput) then
       !  BEGIN DEBUG
       call newfil(minp, 'system_'//trim(md_ident)//'.m')
       write (minp, "('Numrows = ', I0, ';')") nn
       write (minp, "('%startpointers')")
       write (minp, "('ia = [')", advance="no")
       write (minp, "(I0, ' ')", advance="no") (iao(i), i=1, nn + 1)
       write (minp, "('];')")
       write (minp, "('%rowindices')")
       write (minp, "('ja = [')", advance="no")
       write (minp, "(I0, ' ')", advance="no") (jao(i), i=1, iao(nn + 1) - 1)
       write (minp, "('];')")
       write (minp, "('%matrix elements')")
       write (minp, "('aa = [')", advance="no")
       write (minp, "(E15.5)", advance="no") (ao(i), i=1, iao(nn + 1) - 1)
       write (minp, "('];')")
       write (minp, "('%right-hand side')")
       write (minp, "('rhs = [')", advance="no")
       write (minp, "(E15.5)", advance="no") (rhs(i), i=1, nn)
       write (minp, "('];')")
       write (minp, "('%x-coordinates')")
       write (minp, "('x= [')", advance="no")
       nn = 0
       do n = nogauss + 1, nogauss + nocg
          k = noel(n)
          if (k > 0) then
             nn = nn + 1
             write (minp, "(E15.5)", advance="no") xzw(k)
          end if
       end do
       write (minp, "('];')")
       write (minp, "('%y-coordinates')")
       write (minp, "('y= [')", advance="no")
       do n = nogauss + 1, nogauss + nocg
          k = noel(n)
          if (k > 0) then
             write (minp, "(E15.5)", advance="no") yzw(k)
          end if
       end do
       write (minp, "('];')")

       call doclose(minp)
       jaoutput = .false.
       !  END DEBUG
    end if

    if (nn > 0) then
       call cgsaad(its, na, nn, jaini, 1, ierror, res) ! nocg ipv nn
       !write (6,*) res

!    check error
       if (ierror /= 0) then
!      all is not lost, check residual
          if (res <= 1d4 * epscg) then ! allow a larger error
             call mess(LEVEL_INFO, 'conjugategradientSAAD: non-fatal error')
          else
             call qnerror('conjugategradientSAAD: error', ' ', ' ')
             call mess(LEVEL_WARN, 'conjugategradientSAAD: error')
          end if

          !if ( jampi.eq.0 ) then
          !   call newfil(matr, 'saadmatrix.m')
          !else
          !   call newfil(matr, 'saadmatrix_' // sdmn // '.m')
          !end if
          !
          !nocg = nn
          !write(matr,'(a)') 'data = [ ...'
          !do i=1,nocg
          !   do j=iao(i),iao(i+1)-1
          !      dum = ao(j)
          !      if ( abs(dum).lt.1d-99 ) dum = 0d0
          !      write(matr,'(2I10,E20.5)') i, jao(j), dum
          !   end do
          !end do
          !write(matr,'(a)') '];'
          !
          !write(matr,'("A=sparse(data(:,1),data(:,2),data(:,3));")')
          !
          !write(matr,'(a)') 'b=[ ...'
          !do i=1,nocg
          !   write(matr,'(E17.5)') rhs(i)
          !end do
          !write(matr,'(a)') '];'
          !
          !write(matr,'(a)') 'sol=[ ...'
          !do i=1,nocg
          !   write(matr,'(E17.5)') sol(i)
          !end do
          !write(matr,'(a)') '];'
          !
          !call doclose(matr)
       else
!      write(message,*) 'Solver converged in ', its,' iterations'
!      call mess(LEVEL_INFO, message)
       end if
    else
!   nothing to do
    end if

    if (jaini /= -1) then
       nn = 0
       do n = nogauss + 1, nogauss + nocg

          ! nn  = n - nogauss  ! saad index
          ndn = noel(n) ! guus index
          if (ndn > 0) then
             nn = nn + 1
             s1(ndn) = sol(nn) ! en hier zet men thee en over
          else
             ndn = -ndn
             noel(n) = ndn
             s1(ndn) = righthandside(ndn) / bbr(ndn)
          end if
       end do
    end if

 end subroutine conjugategradientSAAD

 subroutine conjugategradient(s1, ndx, ipre)
    use m_reduce
    use unstruc_messages

    implicit none
    integer :: ndx, ipre
    double precision :: s1(ndx)

    integer :: i, j, jj, n, ntot
    double precision :: rkzki, rkzki0, pkapki, alfak, betak
    double precision :: eps

! ddr (rechterlid), bbr (diag) , ccr (off diag), s1, row, row()%j, row()%a [AvD]
    if (nocg <= 0) return

    nocgiter = 0
    eps = 0d0
    rkzki = 0d0

    ! make matrix L
    if (ipre == 2) then ! make L
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          bbl(ndn) = bbr(ndn)
          ntot = row(ndn)%l
          do j = 1, ntot
             i = row(ndn)%j(j)
             if (i < ndn) then
                jj = row(ndn)%a(j)
                bbl(ndn) = bbl(ndn) - ccr(jj) * ccr(jj) / bbl(i)
             end if
          end do
       end do
    end if

    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rk(ndn) = ddr(ndn) - bbr(ndn) * s1(ndn)
       ntot = row(ndn)%l
       do j = 1, ntot
          i = row(ndn)%j(j)
          jj = row(ndn)%a(j)
          rk(ndn) = rk(ndn) - ccr(jj) * s1(i)
       end do
       if (abs(rk(ndn)) > eps) eps = abs(rk(ndn))
       pk(ndn) = rk(ndn) / bbr(ndn)
       zkr(ndn) = pk(ndn)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do
    if (eps < epscg) then
       return
    end if

10  continue

    pkapki = 0d0
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       apk(ndn) = bbr(ndn) * pk(ndn)
       ntot = row(ndn)%l
       do j = 1, ntot
          i = row(ndn)%j(j)
          jj = row(ndn)%a(j)
          apk(ndn) = apk(ndn) + ccr(jj) * pk(i)
       end do
       pkapki = pkapki + pk(ndn) * apk(ndn)
    end do
    alfak = rkzki / pkapki
    eps = 0d0
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       s1(ndn) = s1(ndn) + alfak * pk(ndn)
       rk(ndn) = rk(ndn) - alfak * apk(ndn)
       if (abs(rk(ndn)) > eps) eps = abs(rk(ndn))
    end do
    if (ipre == 0) then ! row scaling
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn) / bbr(ndn)
          ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do
    else ! GS,  incomplete Choleski
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn)
          ntot = row(ndn)%l
          do j = 1, ntot
             i = row(ndn)%j(j)
             if (i < ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do
          if (ipre == 1) then
             zkr(ndn) = zkr(ndn) / bbr(ndn) ! GS
          else
             zkr(ndn) = zkr(ndn) / bbl(ndn) ! Chol
          end if
          ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do
       do n = nogauss + nocg, nogauss + 1, -1
          ndn = noel(n)
          if (ipre < 2) then
             zkr(ndn) = bbr(ndn) * zkr(ndn)
          else
             zkr(ndn) = bbl(ndn) * zkr(ndn)
          end if
          ntot = row(ndn)%l
          do j = 1, ntot
             i = row(ndn)%j(j)
             jj = row(ndn)%a(j)
             if (i > ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do
          if (ipre == 1) then
             zkr(ndn) = zkr(ndn) / bbr(ndn) ! Gs
          else
             zkr(ndn) = zkr(ndn) / bbl(ndn) ! Chol
          end if
          ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do
    end if
    if (eps < epscg) then
       return
    end if
    nocgiter = nocgiter + 1
    if (nocgiter > 1000) then
       call mess(LEVEL_ERROR, ' no convergence for CG method, nr of iterations, eps : ', nocgiter, eps)
    end if

    rkzki0 = rkzki
    rkzki = 0d0

    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do
    betak = rkzki / rkzki0
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       pk(ndn) = zkr(ndn) + betak * pk(ndn)
    end do
    goto 10
 end subroutine conjugategradient

 subroutine conjugategradient_omp_threadsafe(s1, ndx, ipre)
    use m_reduce
    use unstruc_messages

    implicit none
    integer :: ndx
    double precision :: s1(ndx)

    integer i, ipre, j, jj, n
    double precision :: rkzki, rkzki0, pkapki, alfak, betak
    double precision :: eps

    if (nocg <= 0) return

    nocgiter = 0
    eps = 0d0
    rkzki = 0d0

    !$xxOMP PARALLEL DO                                           &
    !$xxOMP PRIVATE(n,ndn,j)                                      &
    !$xxOMP REDUCTION(+:rkzki)                                    &
    !$xxOMP REDUCTION(MAX:eps)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rk(ndn) = ddr(ndn) - bbr(ndn) * s1(ndn)
       do j = 1, row(ndn)%l
          rk(ndn) = rk(ndn) - ccr(row(ndn)%a(j)) * s1(row(ndn)%j(j))
       end do
       eps = max(eps, abs(rk(ndn))) ! if (abs(rk(ndn)).gt.eps) eps=abs(rk(ndn)) abs abs
       pk(ndn) = rk(ndn) / bbr(ndn)
       zkr(ndn) = pk(ndn)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do
    !$xxOMP END PARALLEL DO

    if (eps < epscg) then
       return
    end if

10  continue

    pkapki = 0d0
!$xxOMP PARALLEL DO                                           &
!$xxOMP PRIVATE(n,ndn,j)                                      &
!$xxOMP REDUCTION(+:pkapki)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       apk(ndn) = bbr(ndn) * pk(ndn)
       do j = 1, row(ndn)%l
          apk(ndn) = apk(ndn) + ccr(row(ndn)%a(j)) * pk(row(ndn)%j(j))
       end do
       pkapki = pkapki + pk(ndn) * apk(ndn)
    end do
!$xxOMP END PARALLEL DO

    alfak = rkzki / pkapki

    !$OMP PARALLEL DO                                           &
    !$OMP PRIVATE(n,ndn)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       s1(ndn) = s1(ndn) + alfak * pk(ndn)
       rk(ndn) = rk(ndn) - alfak * apk(ndn)
    end do
    !$OMP END PARALLEL DO

    eps = 0d0

    if (ipre == 0) then
       !$OMP PARALLEL DO                                  &
       !$OMP PRIVATE(n,ndn)                               &
       !$OMP REDUCTION(MAX:eps)
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn) / bbr(ndn)
          eps = max(eps, abs(zkr(ndn))) ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do
       !$OMP END PARALLEL DO
    else

       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn)
          do j = 1, row(ndn)%l ! ntot
             i = row(ndn)%j(j)
             if (i < ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do

          zkr(ndn) = zkr(ndn) / bbr(ndn)

          eps = max(eps, abs(zkr(ndn)))
       end do

       do n = nogauss + nocg, nogauss + 1, -1
          ndn = noel(n)
          zkr(ndn) = bbr(ndn) * zkr(ndn)
          do j = 1, row(ndn)%l
             i = row(ndn)%j(j)
             jj = row(ndn)%a(j)
             if (i > ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do

          !if (ieee_is_nan(bbr(ndn) ) ) then
          !   write(*,*) 'before ', ndn, bbr(ndn), zkr(ndn)
          !endif

          zkr(ndn) = zkr(ndn) / bbr(ndn)

          !if (ieee_is_nan(zkr(ndn) ) ) then
          !    write(*,*) 'after ', ndn, bbr(ndn), zkr(ndn)
          !endif

          if (abs(zkr(ndn)) > eps) eps = abs(zkr(ndn))
       end do
    end if

    !if (eps.lt.epscg) then
    !  return
    !endif

    if (eps < epscg) then
!   write(message,*) 'Solver converged in ', nocgiter,' iterations'
!   call mess(LEVEL_INFO, message)
       return
    end if

    nocgiter = nocgiter + 1
    if (nocgiter > 1000) then
       call mess(LEVEL_ERROR, ' no convergence for CG method, nr of iterations, eps : ', nocgiter, eps)
    end if

    rkzki0 = rkzki
    rkzki = 0d0
    !$xOMP PARALLEL DO                              &
    !$xOMP PRIVATE(n,ndn)                           &
    !$xOMP REDUCTION(+:rkzki)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do
    !$xOMP END PARALLEL DO       ! todo omp onbegrijpelijk dat dit verschil geeft

    betak = rkzki / rkzki0

    !$OMP PARALLEL DO                              &
    !$OMP PRIVATE(n,ndn)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       pk(ndn) = zkr(ndn) + betak * pk(ndn)
    end do
    !$OMP END PARALLEL DO

    goto 10
 end subroutine conjugategradient_omp_threadsafe

 subroutine conjugategradient_omp(s1, ndx, ipre)
    use m_reduce
    use unstruc_messages

    implicit none
    integer :: ndx
    double precision :: s1(ndx)

    integer i, ipre, j, jj, n
    double precision :: rkzki, rkzki0, pkapki, alfak, betak
    double precision :: eps

    if (nocg <= 0) return

    nocgiter = 0
    eps = 0d0
    rkzki = 0d0

    !$OMP PARALLEL DO                                           &
    !$OMP PRIVATE(n,ndn,j)                                      &
    !$OMP REDUCTION(+:rkzki)                                    &
    !$OMP REDUCTION(MAX:eps)

    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rk(ndn) = ddr(ndn) - bbr(ndn) * s1(ndn)
       do j = 1, row(ndn)%l
          rk(ndn) = rk(ndn) - ccr(row(ndn)%a(j)) * s1(row(ndn)%j(j))
       end do
       eps = max(eps, abs(rk(ndn))) ! if (abs(rk(ndn)).gt.eps) eps=abs(rk(ndn)) abs abs
       pk(ndn) = rk(ndn) / bbr(ndn)
       zkr(ndn) = pk(ndn)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do
    !$OMP END PARALLEL DO

    if (eps < epscg) then
       return
    end if

10  continue

    pkapki = 0d0
!$OMP PARALLEL DO                                           &
!$OMP PRIVATE(n,ndn,j)                                      &
!$OMP REDUCTION(+:pkapki)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       apk(ndn) = bbr(ndn) * pk(ndn)
       do j = 1, row(ndn)%l
          apk(ndn) = apk(ndn) + ccr(row(ndn)%a(j)) * pk(row(ndn)%j(j))
       end do
       pkapki = pkapki + pk(ndn) * apk(ndn)
    end do
!$OMP END PARALLEL DO

    alfak = rkzki / pkapki

    !$OMP PARALLEL DO                                           &
    !$OMP PRIVATE(n,ndn)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       s1(ndn) = s1(ndn) + alfak * pk(ndn)
       rk(ndn) = rk(ndn) - alfak * apk(ndn)
    end do
    !$OMP END PARALLEL DO

    eps = 0d0

    if (ipre == 0) then
       !$OMP PARALLEL DO                                  &
       !$OMP PRIVATE(n,ndn)                               &
       !$OMP REDUCTION(MAX:eps)
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn) / bbr(ndn)
          eps = max(eps, abs(zkr(ndn))) ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do
       !$OMP END PARALLEL DO
    else

       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn)
          do j = 1, row(ndn)%l ! ntot
             i = row(ndn)%j(j)
             if (i < ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do

          zkr(ndn) = zkr(ndn) / bbr(ndn)

          eps = max(eps, abs(zkr(ndn)))
       end do

       do n = nogauss + nocg, nogauss + 1, -1
          ndn = noel(n)
          zkr(ndn) = bbr(ndn) * zkr(ndn)
          do j = 1, row(ndn)%l
             i = row(ndn)%j(j)
             jj = row(ndn)%a(j)
             if (i > ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do

          !if (ieee_is_nan(bbr(ndn) ) ) then
          !   write(*,*) 'before ', ndn, bbr(ndn), zkr(ndn)
          !endif

          zkr(ndn) = zkr(ndn) / bbr(ndn)

          !if (ieee_is_nan(zkr(ndn) ) ) then
          !    write(*,*) 'after ', ndn, bbr(ndn), zkr(ndn)
          !endif

          if (abs(zkr(ndn)) > eps) eps = abs(zkr(ndn))
       end do
    end if

    if (eps < epscg) then
       return
    end if
    nocgiter = nocgiter + 1
    if (nocgiter > 1000) then
       call mess(LEVEL_ERROR, ' no convergence for CG method, nr of iterations, eps : ', nocgiter, eps)
    end if

    rkzki0 = rkzki
    rkzki = 0d0
    !$OMP PARALLEL DO                              &
    !$OMP PRIVATE(n,ndn)                           &
    !$OMP REDUCTION(+:rkzki)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do
    !$OMP END PARALLEL DO       ! todo omp onbegrijpelijk dat dit verschil geeft

    betak = rkzki / rkzki0

    !$OMP PARALLEL DO                              &
    !$OMP PRIVATE(n,ndn)
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       pk(ndn) = zkr(ndn) + betak * pk(ndn)
    end do
    !$OMP END PARALLEL DO

    goto 10
 end subroutine conjugategradient_omp

 subroutine gauss_substitution(s1, ndx)
    use m_reduce
    implicit none

    integer :: ndx
    double precision :: s1(ndx)

    integer m, n, np
    do n = nogauss, 1, -1
       ndn = noel(n)
       d0(ndn) = ddr(ndn)
       np = row(ndn)%l
       do m = 1, np
          d0(ndn) = d0(ndn) - ccr(row(ndn)%a(m)) * s1(row(ndn)%j(m))
       end do
       s1(ndn) = d0(ndn)
    end do

 end subroutine gauss_substitution

 subroutine gauss_substitutionjipjan(s1, ndx)
    use m_reduce
    implicit none

    integer :: ndx
    double precision :: s1(ndx)

    integer m, n
    do n = nogauss, 1, -1

       ndn = noel(n)
       d0(ndn) = ddr(ndn)
       do m = L1row(ndn), L2row(ndn)
          d0(ndn) = d0(ndn) - ccr(iarow(m)) * s1(jrow(m))
       end do
       ! JRE
       s1(ndn) = d0(ndn)
    end do

 end subroutine gauss_substitutionjipjan

 subroutine jipjanini()
    use m_reduce
    use m_flowparameters, only: Noderivedtypes
    implicit none

    integer m, n, np, Ltot, j, iftot, k

    Ltot = 0; iftot = 0
    do n = nogauss0, 1, -1
       k = 0
       ndn = noel0(n)
       np = row(ndn)%l
       do m = 1, np
          Ltot = Ltot + 1
          do j = m + 1, np
             iftot = iftot + 1
          end do
       end do
    end do

    if (Noderivedtypes >= 3) then
       do n = nogauss + 1, nogauss + nocg
          ndn = noel0(n)
          np = row(ndn)%l
          do m = 1, np
             Ltot = Ltot + 1
          end do
       end do
    end if

    if (allocated(L1row)) then
       deallocate (L1row, L2row, iarow, jrow, ifrow, ift)
    end if
    if (.not. allocated(L1row)) then
       allocate (L1row(nodtot), L2row(nodtot), iarow(Ltot), jrow(Ltot)); L1row = 0; L2row = 0; iarow = 0; jrow = 0
       allocate (ifrow(iftot), ift(nodtot)); ifrow = 0; ift = 0
    end if

    Ltot = 0
    do n = nogauss0, 1, -1
       ndn = noel0(n)
       np = row(ndn)%l
       L1row(ndn) = Ltot + 1
       L2row(ndn) = Ltot + np
       do m = 1, np
          Ltot = Ltot + 1
          iarow(Ltot) = row(ndn)%a(m)
          jrow(Ltot) = row(ndn)%j(m)
       end do
    end do

    if (Noderivedtypes >= 3) then
       do n = nogauss + 1, nogauss + nocg
          ndn = noel0(n)
          np = row(ndn)%l
          L1row(ndn) = Ltot + 1
          L2row(ndn) = Ltot + np
          do m = 1, np
             Ltot = Ltot + 1
             iarow(Ltot) = row(ndn)%a(m)
             jrow(Ltot) = row(ndn)%j(m)
          end do
       end do
    end if

    iftot = 0
    do n = 1, nogauss0
       ndn = noel0(n)
       ift(ndn) = iftot
       np = row(ndn)%l
       k = 0
       do m = 1, np
          do j = m + 1, np
             k = k + 1
             iftot = iftot + 1
             ifrow(iftot) = row(ndn)%f(k)
          end do
       end do
    end do

    if (Noderivedtypes >= 5) then
       do n = 1, nodtot
          if (allocated(row(n)%a)) deallocate (row(n)%a)
          if (allocated(row(n)%j)) deallocate (row(n)%j)
          if (allocated(row(n)%f)) deallocate (row(n)%f)
       end do
       deallocate (row)
    end if

 end subroutine jipjanini

 subroutine gauss_elimination
    use m_reduce
    implicit none
    !double precision :: ccc(500)
    integer j, k, m, n, np, m1, nodm1, m1m2
    ccc = 0d0
    do n = 1, nogauss
       ndn = noel(n)
       np = row(ndn)%l
       ddr(ndn) = ddr(ndn) / bbr(ndn)
       do m = 1, np
          ccc(m) = ccr(row(ndn)%a(m)) / bbr(ndn)
       end do
       k = 0
       do m = 1, np
          m1 = row(ndn)%j(m)
          nodm1 = row(ndn)%a(m)
          bbr(m1) = bbr(m1) - ccc(m) * ccr(nodm1)
          ddr(m1) = ddr(m1) - ddr(ndn) * ccr(nodm1)
          do j = m + 1, np
             k = k + 1
             m1m2 = row(ndn)%f(k)
             ccr(m1m2) = ccr(m1m2) - ccr(nodm1) * ccc(j)
          end do
          ccr(nodm1) = ccc(m)
       end do
    end do
    return

 end subroutine gauss_elimination

 subroutine gauss_eliminationjipjan
    use m_reduce
    implicit none
    !double precision :: ccc(500)
    integer j, k, m, n, m1, nodm1, m1m2, mm, jj

    ccc = 0d0
    do n = 1, nogauss
       ndn = noel(n)
       ddr(ndn) = ddr(ndn) / bbr(ndn)

       do m = L1row(ndn), L2row(ndn)
          ccc(m - L1row(ndn) + 1) = ccr(iarow(m)) / bbr(ndn)
       end do

       k = 0
       do mm = L1row(ndn), L2row(ndn)
          m = mm - L1row(ndn) + 1
          m1 = jrow(mm)
          nodm1 = iarow(mm)
          bbr(m1) = bbr(m1) - ccc(m) * ccr(nodm1)
          ddr(m1) = ddr(m1) - ddr(ndn) * ccr(nodm1)

          do jj = mm + 1, L2row(ndn)
             j = jj - L1row(ndn) + 1
             k = k + 1
             m1m2 = ifrow(ift(ndn) + k)
             ccr(m1m2) = ccr(m1m2) - ccr(nodm1) * ccc(j)

          end do
          ccr(nodm1) = ccc(m)

       end do
    end do
    return

 end subroutine gauss_eliminationjipjan

 subroutine pack_matrix()
    use m_reduce
    use m_flowgeom
    use m_flow
    use m_partitioninfo

    implicit none
    integer nn

    call setkfs()

    if (jampi == 1) then
       call partition_setkfs() ! exclude all water-level ghostnodes from solution vector
    end if

    nowet = 0
    nocg = 0
    nogauss = 0
    noexpl = 0

    do nn = 1, nogauss0
       ndn = noel0(nn)
       if (kfs(ndn) == 1) then
          nowet = nowet + 1
          noel(nowet) = ndn
          nogauss = nogauss + 1
       end if
    end do

    do nn = nogauss0 + 1, nogauss0 + nocg0
       ndn = noel0(nn)
       if (kfs(ndn) == 1) then
          nowet = nowet + 1
          noel(nowet) = ndn
          nocg = nocg + 1
       end if
    end do

    if (ivariableteta == 2) then
       do nn = 1, nogauss0 + nocg0
          ndn = noel0(nn)
          if (kfs(ndn) == 2) then
             nowet = nowet + 1
             noel(nowet) = ndn
             noexpl = noexpl + 1
          end if
       end do
    end if

    return
 end

 subroutine reducept(Ndx, Lnx)
    ! this subroutine finds an elimination order for Gaussian elimination based upon minimum degree algorithm
    use m_reduce
    use unstruc_messages
    use m_flowparameters, only: icgsolver, ipre, Noderivedtypes
    use m_partitioninfo
    use m_readyy
    use m_calls_saad
    
    implicit none

    integer :: Ndx, Lnx

    integer :: nn ! integers used for counting
    integer :: minold ! minimum degree values
    integer :: ierror

    !  ARS 11272, check on the maximum number of nodes and do not allow maxdge larger than that

    nodtot = ndx
    lintot = lnx

    if (nodtot <= maxdge) maxdge = nodtot

    !
    call readyy('ini Gauss/CG', 0d0)
    call inireduce()
    call readyy('ini Gauss/CG', .35d0)
    call mindegree()
    call readyy('ini Gauss/CG', .40d0)
    nogauss = 0
    nocg = 0
    nn = 0
    do
       if (mindgr > maxdge) exit
       nn = nn + 1
       ndn = nn
       if (jagauss(nn) == 1) then
          if (nbrstk(ndn) /= 0 .and. nodbr2(ndn) == mindgr) then
             minold = mindgr
             call pointonstack()
             if (minold > mindgr) nn = 0
          end if
       else
!      write (*,*)
       end if
       if (nn == nodtot) then
          !  if (nn==noactive) then

          call mindegree()
          nn = 0
       end if
    end do
    call readyy('ini Gauss/CG', .75d0)
    call cijadres()
    call readyy('ini Gauss/CG', .80d0)
    call inigauss()
    call readyy('ini Gauss/CG', .90d0)
    if (nogauss < noactive) call inicg()
    call readyy('ini Gauss/CG', .95d0)

    deallocate (ij)
    deallocate (nbrstk, nodstk, nodbr2)
    if (allocated(intbuf)) deallocate (intbuf)
    if (allocated(logbuf)) deallocate (logbuf)
    if (allocated(jagauss)) deallocate (jagauss)

    call allocate_arrays()

    ! lv2 = lv2

    nogauss0 = nogauss
    nocg0 = nocg
    noel0 = noel

#ifndef HAVE_PETSC
    if (icgsolver == 6) then
       icgsolver = 7
    end if
#endif

    if (Noderivedtypes >= 1) then
       call jipjanini()
    end if

    call mess(LEVEL_INFO, 'nogauss , nocg : ', nogauss, nocg)
    call readyy('ini Gauss/CG', -1d0)

    if (icgsolver == 4 .or. icgsolver == 44 .or. icgsolver == 5 .or. (icgsolver == 7 .and. (ipre == 3 .or. ipre == 4))) then
       if (icgsolver /= 7) then
          call inisaad(epscg, maxmatvecs, 1d0) ! 1d0: with MILU preconditioner
          !call inisaad(epscg,1d0)   ! 1d0: with MILU preconditioner
       else ! use Saad as preconditioner
          call inisaad(epscg, maxmatvecs, 0.d0) ! 0.0d0: as ILU/MILU preconditioner, for robustness sake
          !call inisaad(epscg,0.d0) ! 0.0d0: as ILU/MILU preconditioner, for robustness sake
       end if
    else if (icgsolver == 6) then
#ifdef HAVE_PETSC
       call ini_petsc(Ndx, ierror)
       call preparePETSCsolver(0)
#endif
    else if (icgsolver == 10) then
#ifdef HAVE_PETSC
       call ini_petsc(Ndx, ierror)
       call preparePETSCsolver(1)
#endif
    else if (icgsolver == 8) then
#ifdef HAVE_PARMS
       call iniparms(ierror)
#endif
    else if (icgsolver == 9 .or. icgsolver > 90) then
       call initestsolver()
    end if

 end subroutine reducept

 subroutine allocate_arrays
    use m_reduce
    use m_flow, only: nonlin
    use m_alloc
    implicit none

    integer allocerr

    if (allocated(ccr)) deallocate (ccr)
    allocate (ccr(0:ijstck), STAT=allocErr); ccr = 0
    call aerr('ccr(0:ijstck)', allocerr, ijstck)

    if (allocated(bbl)) deallocate (bbl)
    allocate (bbl(0:ijstck), STAT=allocErr); bbl = 0
    call aerr('bbl(0:ijstck)', allocerr, ijstck)

    if (nonlin > 0) then
       if (allocated(ccrsav)) deallocate (ccrsav)
       allocate (ccrsav(0:ijstck), STAT=allocErr); ccrsav = 0
       call aerr('ccrsav(0:ijstck)', allocerr, ijstck)
    end if

    if (allocated(bbr)) deallocate (bbr, ddr)
    allocate (bbr(nodtot), ddr(nodtot), STAT=allocErr); bbr = 0; ddr = 0
    call aerr('bbr(nodtot), ddr(nodtot)', allocErr, 2 * nodtot)

    if (allocated(d0)) deallocate (d0, zkr, pk, apk, rk)
    allocate (d0(nodtot), zkr(nodtot), pk(0:nodtot), apk(nodtot), rk(nodtot), STAT=allocErr)
    call aerr('d0(nodtot), zkr(nodtot),pk(0:nodtot), apk(nodtot), rk(nodtot)', allocErr, 5 * nodtot)

    if (allocated(ccc)) deallocate (ccc)
    allocate (ccc(npmax), STAT=allocErr)
    call aerr('ccc(npmax)', allocErr, npmax)

    pk = 0

    d0 = 0
    zkr = 0
    apk = 0
    rk = 0

    ccc = 0d0

    return
 end subroutine allocate_arrays

 subroutine explicit()
    use m_flowtimes
    use m_reduce
    use m_flowgeom
    use m_flow

    implicit none

    integer :: n, nn

    do n = nogauss + nocg + 1, nowet ! alle expliciete punten
       nn = noel(n)
       if (kfs(nn) == 2) then
          s1(nn) = s0(nn)
!       do k = 1, nd(nn)%lnx
!          ff    = dts/a1(nn)
!          l     = nd(nn)%ln(k)
!          il    = abs(l)

!          if ( hu(L) > 0 ) then
!             if (l>0) then
!                s1(nn) = s1(nn) + ff*(1-teta(il))*ru(il)*au(il) ! or q(il) when called after subroutine u1q1
!             else
!                s1(nn) = s1(nn) - ff*(1-teta(il))*ru(il)*au(il)
!             endif
!          endif
          !  enddo
       else

       end if
    end do

 end subroutine explicit

 subroutine conjugategradient_MPI(s1, ndx, ipre, nocgiter_loc, ierror)
    use m_reduce
! use unstruc_messages
! BEGIN MPI
#ifdef HAVE_MPI
    use mpi
#endif

    use m_partitioninfo
! END MPI
    use m_timer
    use MessageHandling
    use m_flowparameters, only: jalogsolverconvergence

    implicit none
    integer :: ndx, ipre
    double precision :: s1(ndx)
    integer, intent(out) :: nocgiter_loc
    integer, intent(out) :: ierror !< error (1) or not (0)
    character(len=100) :: message

#ifdef HAVE_MPI
    integer :: nopreconditioner

    integer :: i, j, jj, n, ntot
    double precision :: rkzki, rkzki0, pkapki, alfak, betak
    double precision :: eps

    ! BEGIN MPI
    double precision :: eps_tmp, rkzki_tmp, pkapki_tmp
    ! END MPI

    ierror = 0

#endif

    nocgiter_loc = 0

    ! ddr (rechterlid), bbr (diag) , ccr (off diag), s1, row, row()%j, row()%a [AvD]

#ifdef HAVE_MPI

    eps = 0d0
    rkzki = 0d0

    if (nocg > 0) then
       if (ipre == 2) then
          write (6, *) "ipre=2 not supported"
          stop
       else if (ipre == 3 .or. ipre == 4) then ! Saad preconditioner
          !   compute Saad matrix, preconditioner and permutation only
          call conjugategradientSAAD(ddr, s1, ndx, nopreconditioner, -1, 1, ierror) ! ddr and s1 not used
          if (ierror /= 0) goto 1234
       end if

    else
       rk = 0d0
    end if

! BEGIN MPI
    if (jatimer == 1) call starttimer(IMPICOMM)
    !call update_ghost(s1, ierror)
    call update_ghosts(ITYPE_S, 1, nodtot, s1, ierror)
    if (jatimer == 1) call stoptimer(IMPICOMM)
    if (ierror /= 0) goto 1234
    ! END MPI

    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rk(ndn) = ddr(ndn) - bbr(ndn) * s1(ndn)
       ntot = row(ndn)%l
       do j = 1, ntot
          i = row(ndn)%j(j)
          jj = row(ndn)%a(j)
          rk(ndn) = rk(ndn) - ccr(jj) * s1(i)
       end do
       if (abs(rk(ndn)) > eps) eps = abs(rk(ndn))
!   pk(ndn)=rk(ndn)/bbr(ndn)
!   zkr(ndn)=pk(ndn)
!   rkzki=rkzki+rk(ndn)*zkr(ndn)
    end do

    ! preconditioning
    if (ipre == 3 .or. ipre == 4) then ! Saad preconditioner
!  BEGIN MPI
       if (jatimer == 1) call starttimer(IMPICOMM)
       !call update_ghost(rk,ierror)
       call update_ghosts(ITYPE_S, 1, nodtot, rk, ierror)
       if (jatimer == 1) call stoptimer(IMPICOMM)
       if (ierror /= 0) goto 1234
! END MPI
       if (nocg > 0) then
          if (ipre == 3) then
             call conjugategradientSAAD(rk, zkr, ndx, nopreconditioner, 0, 1, ierror) ! do not (re)compute preconditioner and permutation
             if (ierror /= 0) goto 1234
          else if (ipre == 4) then
             call conjugategradientSAAD(rk, zkr, ndx, nopreconditioner, 2, 1, ierror) ! do not (re)compute preconditioner and permutation
             if (ierror /= 0) goto 1234
          end if
       else
          zkr = 0d0
       end if
    else
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn) / bbr(ndn)
       end do
    end if

    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       pk(ndn) = zkr(ndn)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do

    ! BEGIN MPI
    if (jatimer == 1) call starttimer(IMPICOMM)
    call mpi_allreduce(rkzki, rkzki_tmp, 1, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
    if (jatimer == 1) call stoptimer(IMPICOMM)
    if (ierror /= 0) goto 1234
    rkzki = rkzki_tmp
    if (jatimer == 1) call starttimer(IMPICOMM)
    call mpi_allreduce(eps, eps_tmp, 1, mpi_double_precision, mpi_max, DFM_COMM_DFMWORLD, ierror)
    if (jatimer == 1) call stoptimer(IMPICOMM)
    if (ierror /= 0) goto 1234
    eps = eps_tmp
    ! END MPI

    if (eps < epscg) then
       if (my_rank == 0) then
          if (jalogsolverconvergence == 1) then
             write (message, *) 'Solver converged in ', nocgiter_loc, ' iterations'
             call mess(LEVEL_INFO, message)
          end if
       end if
       return
    end if

10  continue

    ! BEGIN MPI
    if (jatimer == 1) call starttimer(IMPICOMM)
    !call update_ghost(pk(1:nodtot),ierror)
    call update_ghosts(ITYPE_S, 1, nodtot, pk(1:nodtot), ierror)
    if (jatimer == 1) call stoptimer(IMPICOMM)
    if (ierror /= 0) goto 1234
    ! END MPI

    ! compute A pk and pk' A Pk
    pkapki = 0d0
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       apk(ndn) = bbr(ndn) * pk(ndn)
       ntot = row(ndn)%l
       do j = 1, ntot
          i = row(ndn)%j(j)
          jj = row(ndn)%a(j)
          apk(ndn) = apk(ndn) + ccr(jj) * pk(i)
       end do
       pkapki = pkapki + pk(ndn) * apk(ndn)
    end do

    ! BEGIN MPI
    if (jatimer == 1) call starttimer(IMPICOMM)
    call mpi_allreduce(pkapki, pkapki_tmp, 1, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
    if (jatimer == 1) call stoptimer(IMPICOMM)
    if (ierror /= 0) goto 1234
    pkapki = pkapki_tmp
    ! END MPI

    alfak = rkzki / pkapki
    eps = 0d0
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       s1(ndn) = s1(ndn) + alfak * pk(ndn)
       rk(ndn) = rk(ndn) - alfak * apk(ndn)
       if (abs(rk(ndn)) > eps) eps = abs(rk(ndn))
    end do

    ! BEGIN MPI
    if (jatimer == 1) call starttimer(IMPICOMM)
    call mpi_allreduce(eps, eps_tmp, 1, mpi_double_precision, mpi_max, DFM_COMM_DFMWORLD, ierror)
    if (jatimer == 1) call stoptimer(IMPICOMM)
    if (ierror /= 0) goto 1234
    eps = eps_tmp
    ! END MPI

    if (ipre == 0) then ! row scaling
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn) / bbr(ndn)
          ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do
    else if (ipre == 1 .or. ipre == 2) then ! GS,  incomplete Choleski

! BEGIN MPI
       if (jatimer == 1) call starttimer(IMPICOMM)
       !call update_ghost(rk,ierror)
       call update_ghosts(ITYPE_S, 1, nodtot, rk, ierror)
       if (jatimer == 1) call stoptimer(IMPICOMM)
       if (ierror /= 0) goto 1234
       zkr = rk
! END MPI

       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n)
          zkr(ndn) = rk(ndn)
          ntot = row(ndn)%l
          do j = 1, ntot
             i = row(ndn)%j(j)
             if (i < ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do
          if (ipre == 1) then
             zkr(ndn) = zkr(ndn) / bbr(ndn) ! GS
          else
             zkr(ndn) = zkr(ndn) / bbl(ndn) ! Chol
          end if
          ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do

       ! BEGIN MPI
       if (jatimer == 1) call starttimer(IMPICOMM)
       !call update_ghost(zkr,ierror)
       call update_ghosts(ITYPE_S, 1, nodtot, zkr, ierror)
       if (jatimer == 1) call stoptimer(IMPICOMM)
       if (ierror /= 0) goto 1234
       ! END MPI

       do n = nogauss + nocg, nogauss + 1, -1
          ndn = noel(n)
          if (ipre < 2) then
             zkr(ndn) = bbr(ndn) * zkr(ndn)
          else
             zkr(ndn) = bbl(ndn) * zkr(ndn)
          end if
          ntot = row(ndn)%l
          do j = 1, ntot
             i = row(ndn)%j(j)
             jj = row(ndn)%a(j)
             if (i > ndn) then
                jj = row(ndn)%a(j)
                zkr(ndn) = zkr(ndn) - ccr(jj) * zkr(i)
             end if
          end do
          if (ipre == 1) then
             zkr(ndn) = zkr(ndn) / bbr(ndn) ! Gs
          else
             zkr(ndn) = zkr(ndn) / bbl(ndn) ! Chol
          end if
          ! if (abs(zkr(ndn)).gt.eps) eps=abs(zkr(ndn))
       end do
    else if (ipre == 3 .or. ipre == 4) then ! Saad preconditioning
!  BEGIN MPI
       if (jatimer == 1) call starttimer(IMPICOMM)
       !call update_ghost(rk,ierror)
       call update_ghosts(ITYPE_S, 1, nodtot, rk, ierror)
       if (jatimer == 1) call stoptimer(IMPICOMM)
       if (ierror /= 0) goto 1234
!  END MPI
       if (nocg > 0) then
          if (ipre == 3) then
             call conjugategradientSAAD(rk, zkr, ndx, nopreconditioner, 0, 1, ierror) ! do not (re)compute preconditioner and permutation
             if (ierror /= 0) goto 1234
          else if (ipre == 4) then
             call conjugategradientSAAD(rk, zkr, ndx, nopreconditioner, 2, 1, ierror) ! do not (re)compute preconditioner and permutation
             if (ierror /= 0) goto 1234
          end if
       else
          zkr = 0d0
       end if
    end if

    if (eps < epscg) then
       if (my_rank == 0) then
          if (jalogsolverconvergence == 1) then
             write (message, *) 'Solver converged in ', nocgiter_loc, ' iterations'
             call mess(LEVEL_INFO, message)
          end if
       end if
       return
    end if

    nocgiter_loc = nocgiter_loc + 1

    ! MPI
! if ( my_rank.eq.0 .and. 100*(nocgiter_loc/10).eq.nocgiter_loc ) write(6,*) nocgiter_loc, eps
!   if ( my_rank.eq.0 ) write(6,*) nocgiter_loc, eps, alfak
    ! END MPI

    if (nocgiter_loc > 1000) then
       call mess(LEVEL_ERROR, ' no convergence for CG method, nr of iterations, eps : ', nocgiter_loc, eps)
    end if

    rkzki0 = rkzki
    rkzki = 0d0

    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       rkzki = rkzki + rk(ndn) * zkr(ndn)
    end do

    ! BEGIN MPI
    if (jatimer == 1) call starttimer(IMPICOMM)
    call mpi_allreduce(rkzki, rkzki_tmp, 1, mpi_double_precision, mpi_sum, DFM_COMM_DFMWORLD, ierror)
    if (jatimer == 1) call stoptimer(IMPICOMM)
    if (ierror /= 0) goto 1234
    rkzki = rkzki_tmp
    ! END MPI

    betak = rkzki / rkzki0
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       pk(ndn) = zkr(ndn) + betak * pk(ndn)
    end do
    goto 10

#endif

    ierror = 0

! error handling
1234 continue

    if (ierror /= 0) call mess(LEVEL_ERROR, 'conjugategradient_MPI error')

 end subroutine conjugategradient_MPI

 subroutine writematrix_matlab()
    use m_reduce
    use m_partitioninfo
    implicit none
    integer :: mout, n, i, jj, j, ntot

    if (jampi == 0) then
       call newfil(mout, 'matrix.m')
    else
       call newfil(mout, 'matrix_'//sdmn//'.m')
    end if

    write (mout, *) 'N = ', maxval(noel(nogauss + 1:nogauss + nocg))

    write (mout, '(A)') 'bbl=['
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       ntot = row(ndn)%l
       write (mout, *) ndn, ndn, bbl(ndn)
       do j = 1, ntot
          i = row(ndn)%j(j)
          jj = row(ndn)%a(j)
          if (i < ndn) write (mout, *) ndn, i, ccr(jj)
       end do
    end do
    write (mout, '(A)') ';]'

    write (mout, '(A)') 'bbr=['
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       ntot = row(ndn)%l
       write (mout, *) ndn, ndn, bbr(ndn)
       do j = 1, ntot
          i = row(ndn)%j(j)
          jj = row(ndn)%a(j)
          write (mout, *) ndn, i, ccr(jj)
       end do
    end do
    write (mout, '(A)') ';]'

    write (mout, '(A)') 'ddr=['
    do n = nogauss + 1, nogauss + nocg
       ndn = noel(n)
       ntot = row(ndn)%l
       write (mout, *) ndn, ndn, ddr(ndn)
    end do
    write (mout, '(A)') ';]'

    call doclose(mout)
 end subroutine writematrix_matlab

 subroutine initestsolver()
    use m_reduce
    use m_flowgeom, only: Ndx, Lnx, ln, xu, yu, xz, yz
    use m_partitioninfo
    use messagehandling, only: mess, LEVEL_ERROR
    use m_plotdots
    use m_calls_saad
    implicit none

    integer, dimension(:), allocatable :: imask

    integer :: i, ip1, k1, k2, L
    integer :: iglev1, iglev2
    integer :: n
    integer :: jawritten

    if (my_rank == 0) then
       continue
    end if

!   check if maxghostlev_s = minghostlev_s + 1
    if (maxghostlev_s /= minghostlev_s + 1) then
       call mess(LEVEL_ERROR, 'initestsolver: maxghostlev_s.ne.minghostlev_s+1')
    end if

    if (allocated(s1_ghost)) deallocate (s1_ghost)
    allocate (s1_ghost(numghost_s))
    s1_ghost = 0d0

    allocate (imask(Ndx))

    call inisaad(epscg, maxmatvecs, 1d0)

!   mark ghostcells in ighostlist_s
    imask = 0
    do n = 1, nghostlist_s(ndomains - 1)
       imask(ighostlist_s(n)) = 1
    end do

!   make interface administration

!   count interface ghost cells and links
    nbndint = 0
    do L = 1, Lnx
       do i = 1, 2
          ip1 = i + 1; if (ip1 > 2) ip1 = ip1 - 2
          k1 = ln(i, L)
          k2 = ln(ip1, L)

          if (ighosttype_s == IGHOSTTYPE_CELLBASED) then
             iglev1 = ighostlev_cellbased(k1)
             iglev2 = ighostlev_cellbased(k2)
          else if (ighosttype_s == IGHOSTTYPE_NODEBASED) then
             iglev1 = ighostlev_nodebased(k1)
             iglev2 = ighostlev_nodebased(k2)
          else
             iglev1 = ighostlev(k1)
             iglev2 = ighostlev(k2)
          end if

          if (iglev1 == minghostlev_s .and. iglev2 == maxghostlev_s) then
!            check if both nodes are in ighostlist_s
             if (imask(k1) /= 1 .or. imask(k2) /= 1) then
                call mess(LEVEL_ERROR, 'initestsolver: ghostcell error')
             end if

             nbndint = nbndint + 1
          end if
       end do
    end do

!   allocate
    if (allocated(kbndint)) deallocate (kbndint)
    allocate (kbndint(3, nbndint))
    kbndint = 0
    if (allocated(zbndint)) deallocate (zbndint)
    allocate (zbndint(2, nbndint))
    zbndint = 0d0

!   store interface ghost cells and links
    nbndint = 0
    do L = 1, Lnx
       do i = 1, 2
          ip1 = i + 1; if (ip1 > 2) ip1 = ip1 - 2
          k1 = ln(i, L)
          k2 = ln(ip1, L)

          if (ighosttype_s == IGHOSTTYPE_CELLBASED) then
             iglev1 = ighostlev_cellbased(k1)
             iglev2 = ighostlev_cellbased(k2)
          else if (ighosttype_s == IGHOSTTYPE_NODEBASED) then
             iglev1 = ighostlev_nodebased(k1)
             iglev2 = ighostlev_nodebased(k2)
          else
             iglev1 = ighostlev(k1)
             iglev2 = ighostlev(k2)
          end if

          if (iglev1 == minghostlev_s .and. iglev2 == maxghostlev_s) then
             nbndint = nbndint + 1
             kbndint(1, nbndint) = k2
             kbndint(2, nbndint) = k1
             kbndint(3, nbndint) = L
          end if
       end do
    end do

!   check if nodes on both sides of interface links are found
    do n = 1, nbndint
       k1 = kbndint(1, n)
       k2 = kbndint(2, n)
       L = kbndint(3, n)
       if (k1 == 0 .or. k2 == 0 .or. L == 0) then
          call mess(LEVEL_ERROR, 'initestsolver: interface error')
       end if
    end do

!   BEGIN DEBUG
    numdots = 0
    do n = 1, nbndint
       k1 = kbndint(1, n)
       k2 = kbndint(2, n)
       L = kbndint(3, n)
       call adddot(xz(k1), yz(k1), 1d0)
       call adddot(xz(k2), yz(k2), 2d0)
       call adddot(xu(L), yu(L), 3d0)
    end do
    call write_dots('Lbndint_'//sdmn//'.xyz', jawritten)
!   END DEBUG

    if (allocated(imask)) deallocate (imask)

    return
 end subroutine initestsolver

 subroutine testsolver(Ndx, s1, itsol, ierror)
    use m_partitioninfo
    use m_reduce
    use m_saad
    use m_flowgeom, only: kfs
    use unstruc_messages
    use m_timer
    use network_data, only: xzw
    use m_flowparameters, only: jalogsolverconvergence
    use mpi
    use m_calls_saad
    implicit none

    integer, intent(in) :: ndx
    double precision, dimension(Ndx), intent(inout) :: s1
    integer, intent(out) :: itsol
    integer, intent(out) :: ierror

    character(len=128) :: message

!    double precision, dimension(:), allocatable      :: bdum, cdum, ddum

    double precision :: maxdiff
    double precision :: res ! residual
    double precision :: dum
    double precision :: beta, val
    
    integer, parameter :: MAXITER = 100
    integer :: iter, its
    integer :: jj, n, na
    integer :: iout
    integer :: ki, kb, L

    integer, parameter :: javerbose = 1

    double precision, dimension(:), allocatable :: bbr_sav, ddr_sav
    double precision, dimension(1) :: res_global
    double precision :: res_global0, tolDD
    if (nocg <= 0) return

    ierror = 0
    itsol = 0

!   set beta
    beta = sbeta
    tol = prectol
    !beta=0.5d0

!   save original matrix and rhs entries at the interface
    allocate (bbr_sav(nbndint))
    allocate (ddr_sav(nbndint))

    do n = 1, nbndint
       ki = kbndint(2, n)
       bbr_sav(n) = bbr(ki)
       ddr_sav(n) = ddr(ki)
    end do

    !ccrsav = ccr
!   insert interface conditions to set matrix
    do n = 1, nbndint
       kb = kbndint(1, n) ! boundary flownode, will be eliminated for the system
       ki = kbndint(2, n)
       L = kbndint(3, n) ! flowlink
       jj = Lv2(L) ! row number in the system

       if (kfs(kb) > 0 .or. kfs(ki) /= 1) then ! make sure kb is the boundary flownode
          call mess(LEVEL_ERROR, 'testsolver: kfs error')
       end if

       !beta = abs(u1(L))*dts * Dxi(L)   ! CFL number in normal direcion
       !beta = 0.5d0
       bbr(ki) = bbr(ki) - ccr(jj) * (0.5d0 - beta) / (0.5d0 + beta)
    end do

!   initialize Saad solver and compute preconditioner, get row numbering
    call conjugategradientSAAD(ddr, s1, ndx, itsol, -1, 1, ierror)

!   overwrite maximum number op iterations
    ipar(6) = Nsubiters

!   overwrite tolerance
    fpar(2) = epscg

    iout = 6
    if (my_rank == 0 .and. javerbose == 1) then
       write (iout, "(A8, A15, A8, A15)") 'iter', 'maxdiff', 'matvecs', 'subres'
    end if

    nocgiter = 0

!   Schwarz iterations
    do iter = 1, MAXITER
!      flownode-to-rownumber
       !do n=nogauss+1,nogauss+nocg
       !   nn  = n - nogauss  ! saad index
       !   ndn = noel(n)      ! guus
       !   if (ndn > 0) then
       !      ngs(ndn) = nn
       !   end if
       !end do

!      insert interface conditions
       do n = 1, nbndint
          ki = kbndint(2, n)
          ddr(ki) = ddr_sav(n)
       end do

       do n = 1, nbndint
          kb = kbndint(1, n) ! boundary flownode, will be eliminated for the system
          ki = kbndint(2, n)
          !L  = kbndint(3,n) ! flowlink
          jj = Lv2(L) ! row number in the system

          if (kfs(kb) > 0 .or. kfs(ki) /= 1) then ! make sure kb is the boundary flownode
             call mess(LEVEL_ERROR, 'testsolver: kfs error')
          end if
          !
          !beta = abs(u1(L))*dts * Dxi(L)   ! CFL number in normal direcion
          !!beta = 0.5d0
          !beta = 10d0
          val = (0.5d0 - beta) * s1(ki) + (0.5d0 + beta) * s1(kb)
          ddr(ki) = ddr(ki) - ccr(jj) * val / (0.5d0 + beta) !s1(\kb)
          !  bbr(ki) = bbr_sav(n) - ccr(jj)*(0.5d0-beta)/(0.5d0+beta)

          !zbndint(1,n) = beta
          zbndint(2, n) = val
       end do

       nn = nocg ! number of rows
       na = iao(nn + 1) - 1 ! total number of non-zeros

!      solve system,
!      do not reinitialize Saad solver and do not compute preconditioner again
       nn = 0
       do n = nogauss + 1, nogauss + nocg
          ndn = noel(n) ! guus index
          if (ndn > 0) then
             nn = nn + 1 ! n - nogauss   ! saad index

             sol(nn) = s1(ndn)
!            rhs(nn)   = ddr(ndn)  ! input argument
             rhs(nn) = ddr(ndn)
          end if
       end do

       ipar(6) = Nsubiters
       call cgsaad(its, na, nn, 0, jabicgstab, ierror, res)
       nn = 0
       do n = nogauss + 1, nogauss + nocg

          ! nn  = n - nogauss  ! saad index
          ndn = noel(n) ! guus index
          if (ndn > 0) then
             nn = nn + 1
             s1(ndn) = sol(nn) ! en hier zet men thee en over
          else
             ndn = -ndn
             noel(n) = ndn
             s1(ndn) = ddr(ndn) / bbr(ndn)
          end if
       end do
       !call cgsaad(its,na,nn,0,ierror,res)

!      BEGIN DEBUG
       !call writematrix_matlab()

       !call conjugategradientSAAD(ddr,s1,ndx,its,1,1,ierror)

!         reset ccr
       !ccr = ccrsav
!      END DEBUG

!!      copy solution vector to s1
!       nn = 0
!       do n   = nogauss+1, nogauss+nocg
!          ndn = noel(n)      ! guus index
!          if (ndn > 0) then
!             nn = nn + 1
!             s1(ndn) = sol(nn)  ! en hier zet men thee en over
!          else
!             ndn = -ndn
!             noel(n) = ndn
!             s1(ndn) = ddr(ndn) / bbr(ndn)
!          endif
!       enddo
!      set interface ghost values
       do n = 1, nbndint
          kb = kbndint(1, n)
          ki = kbndint(2, n)

          !beta = zbndint(1,n)
          val = zbndint(2, n)
          s1(kb) = (val - (0.5d0 - beta) * s1(ki)) / (0.5d0 + beta)
       end do
!      compute global residual
       res_global = fpar(3)**2
       call reduce_sum(1, res_global)
       !call mpi_allreduce(res_s2, res_global,1, mpi_double_precision, mpi_sum,DFM_COMM_DFMWORLD, ierror)
       res_global = sqrt(res_global)
       if (iter == 1) then
          res_global0 = res_global(1)
          tolDD = max(epscg * res_global0, epscg)
       end if

       if (res_global(1) < tolDD) then !.and. res.lt.epscg ) then
          exit
       end if

!      store s1 in ghost cells before update
       do i = 1, nghostlist_s(ndomains - 1)
          s1_ghost(i) = s1(ighostlist_s(i))
       end do

!      update ghost cells
       if (jatimer == 1) call starttimer(IMPICOMM)
       call update_ghosts(ITYPE_S, 1, Ndx, s1, ierror)
       if (jatimer == 1) call stoptimer(IMPICOMM)

!       maxdiff = 0d0
!!      compute maximum difference
!       do i=1,nghostlist_s(ndomains-1)
!          diff = s1_ghost(i)-s1(ighostlist_s(i))
!          if ( abs(diff).gt.1d-6 .and. iter.ge.MAXITER-1 .and. my_rank.eq.0 ) then
!             k = ighostlist_s(i)
!             write(6,*) my_rank, i, k, iglobal_s(k), diff
!          end if
!          maxdiff = max(maxdiff, abs(diff))
!       end do
!!      compute err_ex
!       err_ex = maxval(abs(xzw-s1))
!       if (my_rank==0) then
!       diff1(iter) = maxdiff
!       err_1(iter) = err_ex
!       endif
!!      compute maximum redisual
!       maxresloc = 0d0
!       do n=nogauss+1,nogauss+nocg
!          ndn = noel(n)      ! guus
!          if (ndn > 0) then
!              resloc = ddr(ndn) - bbr(ndn)*s1(ndn)
!
!              ntot=row(ndn)%l
!              do j=1,ntot
!                 i  = row(ndn)%j(j)
!                 jj = row(ndn)%a(j)
!                 resloc = resloc - ccr(jj)*s1(i)
!              enddo
!
!              maxresloc = max(maxresloc,abs(resloc))
!          end if
!       end do
!
!       call reduce_double3_max(maxdiff,res,maxresloc)

       dum = dble(its)
       call reduce_double3_max(maxdiff, res, dum)
       its = int(dum)

       if (my_rank == 0 .and. javerbose == 1) then
          write (6, *) 'iter=', iter, 'maxdiff=', maxdiff, 'its=', its, 'res=', res, 'ini_res=', fpar(3), 'global_res=', res_global(1)
          !write(6,*) 'iter=', iter, 'res_global=', res_global, 'its=', its, 'sub res=', res, 'coupling res=', fpar(3)
       end if

       nocgiter = nocgiter + its

       !if ( maxdiff.lt.epsdiff .and. res.lt.epscg ) then
       !   exit
       !end if

       !if (err_ex .lt. 1d-8) then
       !   exit
       !endif

       !if ( iter.eq.1 ) then
       !   fpar(2) = max(epscg,stoptol*maxdiff)
       !else
       !   fpar(2) = min(fpar(2),max(epscg,stoptol*maxdiff))
       !end if
       !if ( iter.eq.1 ) then
       !   fpar(2) = max(epscg,stoptol*res_global(1))
       !else
       !   fpar(2) = min(fpar(2),max(epscg,stoptol*res_global(1)))
       !end if
       !ipar(6) = maxmatvecs
!       write(6,*) "number of matvecs=", ipar(7)
!
!!      BEGIN DEBUG
       !fpar(2) = epscg
!!      END DEBUG

    end do

    if (my_rank == 0) then
       if (jalogsolverconvergence == 1) then
          write (message, "('Solver converged in ', I0, ' (', I0, ') iterations')") nocgiter, iter
          call mess(LEVEL_INFO, message)
       end if
    end if

1234 continue

    if (allocated(bbr_sav)) deallocate (bbr_sav)
    if (allocated(ddr_sav)) deallocate (ddr_sav)

    return
 end subroutine testsolver
