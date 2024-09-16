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

! filter

#include "blasfm.h"

!> initialize filter
subroutine ini_filter(jafilter, filterorder, jacheckmonitor, ierr)
   use m_flowgeom, only: Lnx, ln, ln2lne, nd, lne2ln, dx, wu, ba, ban, lncn, xu, yu, csu, snu
   use network_data, only: lne, nmk, kn, nod, nb, kc
   use m_flow, only: Lnkx, kmx
   use m_filter
   use m_solver
   use m_alloc
   use unstruc_messages
   use unstruc_model, only: md_netfile
   use dfm_error
   use m_alloc
   use m_partitioninfo, only: jampi
   use m_qnerror
   use m_writematrix
   use m_makenetnodescoding
   implicit none

   integer, intent(in) :: jafilter !< explicit (1), implicit (2), or no filter (0)
   integer, intent(in) :: filterorder !< filter order
   integer, intent(inout) :: jacheckmonitor !< compute and output "checkerboard" monitor (1) or not (0)
   integer, intent(out) :: ierr !< error (1) or not (0)

!   integer,          dimension(:), allocatable :: iLvec  !< vector Laplacian in CRS format, startpointers
!   integer,          dimension(:), allocatable :: jLvec  !< vector Laplacian in CRS format, row numbers
!   double precision, dimension(:), allocatable :: ALvec  !< vector Laplacian in CRS format, matrix entries

!   integer, dimension(:), allocatable :: num

!   integer, dimension(:), allocatable :: dum
   integer, dimension(:), allocatable :: iwork

!   integer, parameter                 :: LENFILNAM = 128
!
!   character(len=LENFILNAM)           :: FNAM

   double precision :: dfacDiv, dfacCurl, dfac, val

   integer :: kk, k
   integer :: L, L2, Lf, Lf2, LL2
   integer :: nn, n
   integer :: numtot

   integer :: istart, iend
   integer :: i, ipoint, j

   integer :: len

   integer :: ierror

!  clean up pre-existing filter
   call dealloc_filter()

   if (jafilter /= 0 .or. jacheckmonitor == 1) then
      if (kmx > 1) then
         call realloc(checkmonitor, kmx, keepExisting=.false., fill=0d0)
         if (jampi == 1) then
            call realloc(workin, kmx + 1, keepExisting=.false., fill=0d0)
            call realloc(workout, kmx + 1, keepExisting=.false., fill=0d0)
         end if
         jacheckmonitor = 1
      else
         jacheckmonitor = 0
      end if
   end if

   if (jafilter == 0) then
      itype = 0
      order = 1 ! safety

      return
   end if

   itype = jafilter
   order = filterorder

   ierr = 1
   !
   ! Filter to suppress checkerboarding is also available for z-layers (so that ERROR message has been switched off)
   !
   !  check for sigma-layers
   ! if ( layertype.ne.1 ) then
   ! call mess(LEVEL_ERROR, 'filter: only sigma layers supported')

   ! goto 1234
   ! end if

   !
   ! Filter to suppress checkerboarding is also available for z-layers (so that ERROR message has been switched off)
   !
   !  check parallelization
   ! if ( jampi.eq.1 .and. itype.ne.1 ) then
   ! call mess(LEVEL_ERROR, 'filter: only explicit filter supported in parallel simulations')
   ! end if

!  check explicit filter
   if (itype == 1 .and. order == 1) then
      call mess(LEVEL_ERROR, 'filter: first-order, explicit filter not supported')
   end if

!  construct vector Laplacian
!  boundary conditions: u.n = 0, n.du/ds = 0

!  set node mask: 1 internal 2D, other elsewhere
   kc = 1
   call MAKENETNODESCODING()

!  get upper bound for number of non-zero entries per row
   call realloc(num, Lnx, keepExisting=.false., fill=0)

!  loop over flowlinks
   do Lf = 1, Lnx
!     Div-part: loop over left,right neighboring cell
      do kk = 1, 2
!        get cell number
         k = ln(kk, Lf)
         if (k == 0) cycle

!        add to upper bound
         num(Lf) = num(Lf) + nd(k)%lnx
      end do

!     Curl-part: get netlink
      L = abs(ln2lne(Lf))
!     loop over left,right netnode
      do nn = 1, 2
!        get netnode number
         n = kn(nn, L)

!        add to upper bound
         num(Lf) = num(Lf) + nmk(n)
      end do
   end do

!  allocate and construct startpointers with upper bound
   call realloc(iLvec, Lnx + 1, keepExisting=.false.)
   iLvec(1) = 1
   do Lf = 1, Lnx
      iLvec(Lf + 1) = iLvec(Lf) + num(Lf)
   end do

!  allocate CRS with upper bound
   numtot = iLvec(Lnx + 1) - 1
   call realloc(jLvec, numtot, fill=0, keepExisting=.false.)
   call realloc(ALvec, numtot, fill=0d0, keepExisting=.false.)

!  construct row numbers and fill matrix
   num = 0
!  loop over edges
   do Lf = 1, Lnx
!     get start- and endpointer
      istart = iLvec(Lf)
      iend = iLvec(Lf + 1) - 1

!     Div-part: loop over left, right neighboring cell
      dfacDiv = 1d0 / Dx(Lf)
      do kk = 1, 2
!        account for orientation
         dfacDiv = -dfacDiv

         k = ln(kk, Lf)

         if (k == 0) cycle

         dfac = dfacDiv / ba(k)

!        loop over edges of cell that are flowlinks
         do LL2 = 1, nd(k)%lnx
            Lf2 = abs(nd(k)%ln(LL2))
            if (Lf2 == 0) then
               call mess(LEVEL_ERROR, 'ini_filter: zero link number')
               goto 1234
            end if

            if (k == ln(1, Lf2)) then
               val = dfac * wu(Lf2)
            else if (k == ln(2, Lf2)) then
               val = -dfac * wu(Lf2)
            else
               call mess(LEVEL_ERROR, 'ini-filter: error in div')
            end if

!           add element on row
            call add_rowelem(jLvec(istart), ALvec(istart), iend - istart + 1, Lf2, val, num(Lf))
         end do
      end do

!     Curl-part: loop over left, right netnode
      dfacCurl = -1d0 / wu(Lf)
      do nn = 1, 2
!        account for orientation
         dfacCurl = -dfacCurl

!        get netnode number
         n = lncn(nn, Lf)

!        boundary condition: curl u = 0
         if (nb(n) /= 1) cycle

         if (n == 0) then
            call mess(LEVEL_ERROR, 'ini_filter: zero node number')
            goto 1234
         end if

         dfac = dfacCurl / (ban(n))

!        loop over attached flowlinks
         do LL2 = 1, nmk(n)
!           get netlink number
            L2 = nod(n)%lin(LL2)
!           get flowlink number
            Lf2 = lne2ln(L2)

            if (Lf2 <= 0) cycle ! boundary

            if (n == lncn(2, Lf2)) then
               val = dfac * Dx(Lf2)
            else if (n == lncn(1, Lf2)) then
               val = -dfac * Dx(Lf2)
            else
               call mess(LEVEL_ERROR, 'ini_filter: error in curl')
            end if

!           add row element
            call add_rowelem(jLvec(istart), ALvec(istart), iend - istart + 1, Lf2, val, num(Lf))
         end do
      end do
   end do

!  remove zeros
   ipoint = 1
   istart = iLvec(1)
   do Lf = 1, Lnx
      do i = istart, iLvec(Lf + 1) - 1
         if (jLvec(i) /= 0) then
            jLvec(ipoint) = jLvec(i)
            ALvec(ipoint) = ALvec(i)
            ipoint = ipoint + 1
         else
            exit
         end if
      end do
      istart = iLvec(Lf + 1)
      iLvec(Lf + 1) = ipoint
   end do

   N = iLvec(Lnx + 1) - 1
   call realloc(jLvec, N, keepExisting=.true.)
   call realloc(ALvec, N, keepExisting=.true.)

!  prepare solver
   if (filterorder == 1) then

   else if (filterorder == 2 .or. filterorder == 3) then
      allocate (iwork(Lnx))
      call amub_countonly(Lnx, Lnx, ALvec, jLvec, iLvec, ALvec, jLvec, iLvec, iwork, N)
   else
!     not supported
      call mess(LEVEL_ERROR, 'filter: order not supported')
      goto 1234
   end if

!  default solver settings
   call SolverSettings(solver_filter, Lnx, N)

!  allocate solver arrays
   call allocSolver(solver_filter, ierror)
   if (ierror /= 0) goto 1234

   if (filterorder == 1) then
      solver_filter%ia = iLvec
      solver_filter%ja = jLvec
   else if (filterorder == 2 .or. filterorder == 3) then
!     allocate Lvec2
      call realloc(ALvec2, N, keepExisting=.false., fill=0d0)

!     compute biharmonic operator
      call amub(Lnx, Lnx, 1, ALvec, jLvec, iLvec, ALvec, jLvec, iLvec, ALvec2, solver_filter%ja, solver_filter%ia, N, iwork, ierror)
      if (ierror /= 0) goto 1234
   end if

!  safety: check if diagonal entry exists
   Lp: do Lf = 1, Lnx
      do j = solver_filter%ia(Lf), solver_filter%ia(Lf + 1) - 1
         if (j == Lf) then
!           diagonal element found
            exit Lp
         end if
      end do

!     no diagonal element found
      call mess(LEVEL_ERROR, 'filterfuru: diagonal entry not filled')
      goto 1234

   end do Lp

!  allocate other arrays
   call realloc(sol, Lnx, keepExisting=.false., fill=0d0)
   call realloc(ustar, Lnkx, keepExisting=.false., fill=0d0)
   call realloc(eps, (/kmx, Lnx/), keepExisting=.false., fill=0d0)
   call realloc(Deltax, Lnx, keepExisting=.false., fill=0d0)

!  get typical mesh width
   call get_Deltax()

   if (itype == 1) then
      call realloc(dtmaxeps, Lnx, keepExisting=.false., fill=0d0)

!     get maximum time step divided by filter coefficient
      call get_dtmaxeps()
   end if

   if (jadebug == 1) then

!     get netfile basename
      len = index(md_netfile, '_net') - 1

      if (len < 1 .or. len + 6 > LENFILNAM) then
         call qnerror('write domains: net filename error', ' ', ' ')
         goto 1234
      end if

      FNAM = md_netfile(1:len)//'_flt.m'
      call realloc(num, Lnx + 1)
      call realloc(dum, Lnx)
      dum = 1
      do Lf = 1, Lnx + 1
         num(Lf) = Lf
      end do

      call writematrix(FNAM, Lnx, iLvec, jLvec, ALvec, 'L', 0)
      call writematrix(FNAM, Lnx, solver_filter%ia, solver_filter%ja, ALvec2, 'L2', 1)
      call writematrix(FNAM, Lnx, num, dum, xu, 'xu', 1)
      call writematrix(FNAM, Lnx, num, dum, yu, 'yu', 1)
      call writematrix(FNAM, Lnx, num, dum, csu, 'csu', 1)
      call writematrix(FNAM, Lnx, num, dum, snu, 'snu', 1)
   end if

   ierr = 0
1234 continue

!  deallocate
!   if ( allocated(num) ) deallocate(num)
!   if ( allocated(dum) ) deallocate(dum)
!   if ( allocated(iLvec) ) deallocate(iLvec)
!   if ( allocated(jLvec) ) deallocate(jLvec)
!   if ( allocated(ALvec) ) deallocate(ALvec)
   if (allocated(iwork)) deallocate (iwork)

   return
end subroutine ini_filter

! clean-up filter
subroutine dealloc_filter
   use m_filter
   implicit none

   call deallocSolver(solver_filter)

   if (allocated(ALvec2)) deallocate (ALvec2)
   if (allocated(iLvec)) deallocate (iLvec)
   if (allocated(jLvec)) deallocate (jLvec)
   if (allocated(ALvec)) deallocate (ALvec)
   if (allocated(sol)) deallocate (sol)
   if (allocated(ustar)) deallocate (ustar)
   if (allocated(eps)) deallocate (eps)
   if (allocated(Deltax)) deallocate (Deltax)

   if (allocated(dtmaxeps)) deallocate (dtmaxeps)
   if (allocated(checkmonitor)) deallocate (checkmonitor)
   if (allocated(workin)) deallocate (workin)
   if (allocated(workout)) deallocate (workout)

   if (allocated(num)) deallocate (num)
   if (allocated(dum)) deallocate (dum)

   return
end subroutine dealloc_filter

!> add element to row
subroutine add_rowelem(jA, A, N, j, val, num)
   use unstruc_messages
   implicit none

   integer, intent(in) :: N !< array length
   integer, dimension(N), intent(inout) :: jA !< rownumber array
   double precision, dimension(N), intent(inout) :: A !< matrix values
   integer, intent(in) :: j !< rownumber to be inserted
   double precision, intent(in) :: val !< value to be inserted
   integer, intent(inout) :: num !< number of nonzero entries

   integer :: i, i2

   do i = 1, N
      if (jA(i) == 0) then
!        no non-zero array members remaining
         jA(i) = j
         num = num + 1
         A(i) = val
         exit
      else if (jA(i) == j) then
!        entry already exists
         A(i) = A(i) + val
         exit
      else if (jA(i) > j) then
!        insert entry
         do i2 = N, i + 1, -1
            jA(i2) = jA(i2 - 1)
            A(i2) = A(i2 - 1)
         end do
         jA(i) = j
         A(i) = val
         num = num + 1
         exit
      else if (i == N) then
!        check for suffucient array size
         call mess(LEVEL_ERROR, 'insert_rownumber: error')
      else
!        proceed to next array member
      end if
   end do

   return
end subroutine add_rowelem

!> compute filter predictor (sigma only)
!>  (I-Delta t F) u^* = u^n
subroutine comp_filter_predictor()
   use m_filter
   use m_flowgeom, only: Lnx
   use m_flow, only: kmx, u0, plotlin, adve
   use m_flowtimes, only: Dts
   use m_physcoef, only: ag
   use unstruc_messages
   use m_saad, only: jasafe ! for amux
   use m_partitioninfo, only: jampi, update_ghosts, ITYPE_U, reduce_int1_max, my_rank
   use m_timer
   use m_get_Lbot_Ltop
   use m_writematrix
   implicit none

   double precision :: fac, dsign
   double precision :: dt

   integer :: klay, LL, L
   integer :: Lb, Lt
   integer :: i, j
   integer :: it, Nt

   integer :: iters

   integer :: japrecond

   integer :: jasafe_store

   integer :: ierror ! error (1) or not (0)

   integer :: lunfil

   double precision, parameter :: facmax = 0.9d0 ! safety factor for maximum allowed sub time step

   if (itype == 0) return

   call starttimer(IFILT)

   ierror = 1

!  store jasafe
   jasafe_store = jasafe

!  force thread safe
   jasafe = 1

   dsign = 1d0 ! sign of Lvec2 in matrix
   if (itype == 1) then
      dsign = -dsign
   end if

   if (jadebug == 1) then
      open (newunit=lunfil, file='debug.txt')
   end if

   japrecond = 1

!  loop over layers

   call starttimer(IFILT_OTHER)
   solver_filter%A = 0d0
   ustar = 0d0
   call stoptimer(IFILT_OTHER)

!  get filter coefficient
   call starttimer(IFILT_COEF)
   call get_filter_coeff()
   call stoptimer(IFILT_COEF)

   do klay = 1, kmx
      call starttimer(IFILT_OTHER)
!     compute number of sub time steps and sub time step
      Nt = 1
      dt = dts

      if (itype == 1) then
!        compute sub time step
         dt = huge(1d0)
         do LL = 1, Lnx
            dt = min(dt, dtmaxeps(LL) / max(eps(klay, LL), 1d-10))
         end do

         dt = facmax * dt

         if (dt < dts) then
!           get number of sub time steps
            Nt = 1 + floor(dts / dt)
!            write(6,"('(', I4, ') ', I4, ':', I4)") my_rank, klay, Nt
            dt = dts / Nt
         else
            dt = dts
         end if

         if (jampi == 1) then
            call reduce_int1_max(Nt)
            dt = dts / Nt
         end if
      end if

      call stoptimer(IFILT_OTHER)

!     construct matrix
      call starttimer(IFILT_MAT)

      if (itype == 1) then
         do LL = 1, Lnx
            call getLbotLtop(LL, Lb, Lt)
!           get 3D link index (sigma only)
            L = Lb + klay - 1

!           fill right-hand side
            if (L >= Lb .and. L <= Lt) solver_filter%rhs(LL) = u0(L)
         end do
      else
         do LL = 1, Lnx
            call getLbotLtop(LL, Lb, Lt)
!           get 3D link index (sigma only)
            L = Lb + klay - 1
            if (L >= Lb .and. L <= Lt) then
!               fill right-hand side
               if (itype == 3) then
                  solver_filter%rhs(LL) = u0(L) - adve(L) * Dt
               else
                  solver_filter%rhs(LL) = u0(L)
               end if

               if (order == 1) then
                  fac = -eps(klay, LL) * Dt * dsign
               else if (order == 2) then
                  fac = eps(klay, LL) * Dt * dsign
               else
                  fac = eps(klay, LL) * Dt * dsign
               end if
               plotlin(L) = eps(klay, LL)

!               BEGIN DEBUG
               if (itype == 1) then
                  plotlin(L) = dts / (dtmaxeps(LL) / max(eps(klay, LL), 1d-10))
               else
                  plotlin(L) = 1d0
               end if
!               END DEBUG

!               loop over columns
               do i = solver_filter%ia(LL), solver_filter%ia(LL + 1) - 1
!                  get column number
                  j = solver_filter%ja(i)

!                  add scaled biharmonic operator
                  if (order == 1) then
                     solver_filter%A(i) = fac * ALvec(i)
                  else if (order == 2 .or. order == 3) then
                     solver_filter%A(i) = fac * ALvec2(i)
                  end if

!                  add diagonal entry
                  if (j == LL) then
                     solver_filter%A(i) = solver_filter%A(i) + 1d0
                  end if
               end do
            end if

         end do
      end if

      call stoptimer(IFILT_MAT)

      if (jadebug == 1) then
         call writematrix(FNAM, Lnx, num, dum, solver_filter%rhs, 'rhs', 1)
         call writematrix(FNAM, Lnx, solver_filter%iA, solver_filter%jA, solver_filter%A, 'Fmat', 1)
         jadebug = 0
      end if

      call starttimer(IFILT_SOLV)

      if (itype == 1) then
!        explicit filter
!        sub time steps
         do it = 1, Nt
!            call amux(Lnx, solver_filter%rhs, sol, solver_filter%A, solver_filter%jA, solver_filter%iA)

!           compute sol = Lvec2 u
            call amux(Lnx, solver_filter%rhs, sol, ALvec2, solver_filter%jA, solver_filter%iA)

!           compute u - eps*Dt*Lvec2 u
            do LL = 1, Lnx
               sol(LL) = solver_filter%rhs(LL) - eps(klay, LL) * Dt * sol(LL)
            end do

            if (jampi == 1) then
               call update_ghosts(ITYPE_U, 1, Lnx, sol, ierror)
            end if

            if (it < Nt) then
               solver_filter%rhs = sol
            end if
         end do
      else if (itype == 2 .or. itype == 3) then
!        solve system for r (reuse preconditioner)
         call solveSystem(solver_filter, sol, japrecond, iters, ierror)
         japrecond = 0
         if (ierror /= 0) goto 1234
      else
!        unsupported option
         goto 1234
      end if

      call stoptimer(IFILT_SOLV)

      call starttimer(IFILT_COPYBACK)

!     copy layer data back to 3D arrays
      do LL = 1, Lnx
         call getLbotLtop(LL, Lb, Lt)
!        get 3D link index (sigma only)
         L = Lb + klay - 1

!        fill layer data
         if (L >= Lb .and. L <= Lt) ustar(L) = sol(LL)
      end do

      call stoptimer(IFILT_COPYBACK)
   end do

   ierror = 0

1234 continue

!  restore jasafe
   jasafe = jasafe_store

   if (ierror /= 0) then
      call mess(LEVEL_ERROR, 'filter_furu: error')
      continue
   end if

   if (jadebug == 1) then
      close (lunfil)
   end if

   call stoptimer(IFILT)

   return
end subroutine comp_filter_predictor

!> get maximum filter time step mulitplied with filter coefficient
subroutine get_dtmaxeps()
   use m_flowgeom, only: Lnx
   use m_filter
   implicit none

   double precision :: diag, offdiag

   integer :: i, j
   integer :: L

   integer :: ierror ! error (1) or not (0)

   ierror = 1

   dtmaxeps = huge(1d0)

   if (order == 2 .or. order == 3) then
      do L = 1, Lnx
         diag = 0d0
         offdiag = 0d0

         do i = solver_filter%ia(L), solver_filter%ia(L + 1) - 1
!           get column index
            j = solver_filter%ja(i)

            if (j == L) then
!              get diagonal entry
               diag = -ALvec2(i)
            else
!              sum off diagonal entries
               offdiag = offdiag + abs(ALvec2(i))
            end if
         end do

         if (offdiag > 0d0) then
!           update maximum time step
            dtmaxeps(L) = min(dtmaxeps(L), 1d0 / offdiag)
            dtmaxeps(L) = min(dtmaxeps(L), 2d0 / (-diag + offdiag))
         else
!           error
!            goto 1234
         end if
      end do
   end if

   ierror = 0
1234 continue

   return
end subroutine get_dtmaxeps

!> determine typical mesh width
subroutine get_Deltax()
   use m_flowgeom, only: Dx, csu, snu, Lnx
   use m_filter
   implicit none

   double precision :: dinpr

   integer :: L, L1
   integer :: j

   double precision, parameter :: dtol = 1d-8

   do L = 1, Lnx
      Deltax(L) = Dx(L)

!     get other links in stencil
      do j = solver_filter%ia(L), solver_filter%ia(L + 1) - 1
         L1 = solver_filter%ja(j)

!        exclude self
         if (L1 == L) cycle

!        account for orientation
         dinpr = abs(csu(L) * csu(L1) + snu(L) * snu(L1))

!        update typical mesh width
         if (dinpr > dtol) then
            Deltax(L) = min(Deltax(L), Dx(L1) / dinpr)
         end if
      end do
   end do

   return
end subroutine get_Deltax

!> compute "checkerboard" mode monitor
subroutine comp_checkmonitor()
   use m_flowgeom, only: Lnx, Dx, wu, ba, ln
   use m_flow, only: qw, kmx, Lbot, Ltop
   use m_turbulence, only: ln0
   use m_filter
   use m_partitioninfo
   use m_get_Lbot_Ltop
   implicit none

   double precision :: area
   integer :: kk1, kk2, k1, k2
   integer :: Ll, L, Lb, Lt
   integer :: klay
   integer :: jaghost, idmn_link

   checkmonitor = 0d0
   area = 0d0

   do LL = 1, Lnx
!     get neighboring 2D cells
      kk1 = ln(1, LL)
      kk2 = ln(2, LL)

      if (jampi == 1) then
!        determine if link is ghost or not
         call link_ghostdata(my_rank, idomain(kk1), idomain(kk2), jaghost, idmn_link)
         if (jaghost == 1) then
!           exclude ghost links
            cycle
         end if
      end if

      call getLbotLtop(LL, Lb, Lt)
!     Implementation of filter for both sigma and z layers:
      do L = Lbot(LL), Ltop(LL)
         klay = L - Lbot(LL) + 1

!        get neighboring 3D cells
         k1 = ln0(1, L)
         k2 = ln0(2, L)

!        add to monitor
         checkmonitor(klay) = checkmonitor(klay) + abs(qw(k2) / ba(kk2) - qw(k1) / ba(kk1)) * 0.5d0 * wu(LL)
      end do
      area = area + 0.5d0 * Dx(LL) * wu(LL)
   end do

   if (jampi == 1) then
      workin(1:kmx) = checkmonitor(1:kmx)
      workin(kmx + 1) = area
      call reduce_double_sum(kmx + 1, workin, workout)
      checkmonitor(1:kmx) = workout(1:kmx)
      area = workout(kmx + 1)
   end if

   do klay = 1, kmx
      checkmonitor(klay) = checkmonitor(klay) / area
   end do

   return
end subroutine comp_checkmonitor

!> get filter coefficient (sigma only)
subroutine get_filter_coeff()
   use m_flowgeom, only: Lnx, ln, nd, acL, wcx1, wcx2, wcy1, wcy2, csu, snu
   use m_flow, only: qa, vol1, kmx, vicLu, hu, Lbot, Ltop
   use m_filter, only: iLvec, jLvec, ALvec, eps, order, Deltax
   use m_get_Lbot_Ltop

   double precision, dimension(kmx) :: eps1 ! first-order filter coefficient
   double precision :: eps2 ! second-order filter coefficient
   double precision :: eps3 ! third-order filter coefficient

   double precision :: dsign
   double precision :: Q
   double precision :: wcx, wcy, w, alpha
   double precision :: volu
   double precision :: vicouv !< typical viscosity

   integer :: klay
   integer :: LL
   integer :: Lb, Lt, L
   integer :: LL1, iLL1
   integer :: Lb1, Lt1, L1
   integer :: kk
   integer :: k1, k2
   integer :: i, ik, iL, j

   integer :: ierror
!
   double precision, parameter :: dtol = 0.01d0

   ierror = 1

   do LL = 1, Lnx
!     compute first-order filter coefficient
      eps1 = 0d0

      call getLbotLtop(LL, Lb, Lt)

!     loop over left/right neighboring cell
      do ik = 1, 2
         kk = ln(ik, LL)

         alpha = acL(LL)
         if (ik == 2) then
            alpha = 1d0 - acL(LL)
         end if

!        loop over links in cell
         do iL = 1, nd(kk)%lnx
            iLL1 = nd(kk)%ln(iL)
            LL1 = abs(iLL1)

!           exclude self
            if (LL1 == LL) then
               cycle
            end if

!           check if link is dry
            if (hu(LL1) == 0) then
               cycle
            end if
!
!           safety
            if (abs(csu(LL) * csu(LL1) + snu(LL) * snu(LL1)) < dtol) then
               cycle
            end if

            call getLbotLtop(LL1, Lb1, Lt1)

!           get orientation of link
            dsign = 1d0 ! outward
            wcx = wcx1(LL1)
            wcy = wcy1(LL1)
            if (ln(2, LL1) == kk) then
               dsign = -1d0 ! inward
               wcx = wcx2(LL1)
               wcy = wcy2(LL1)
            end if

!           compare with vector Laplacian
            do i = iLvec(LL), ILvec(LL + 1) - 1
               j = jLvec(i)
               if (j == LL1) exit
            end do

            if (j /= LL1) then ! safety
               goto 1234
            end if

!           loop over water column
!           Implementation of filter for both sigma and z layers:
            do L = Lbot(LL), Ltop(LL)
               klay = L - Lbot(LL) + 1

!              get advection volume
               k1 = ln(1, L)
               k2 = ln(2, L)

               volu = acL(LL) * vol1(k1) + (1d0 - acL(LL)) * vol1(k2)

               if (volu > 0d0) then

!                 get 3D link number (sigma only)
                  L1 = Lb1 + klay - 1

!                 check if link is active
!!                  if ( L1.gt.Lt1 ) then
!!                     exit
!!                  end if

!                 get outward positive flux
                  Q = qa(L1) * dsign

!                 outflowing only
                  if (Q <= 0d0) then
                     cycle
                  else
                     continue
                  end if

!                 compute weight of this link (L1) in advection of link L
                  w = (wcx * csu(LL) + wcy * snu(LL)) * alpha / volu

                  w = w * Q

                  if (abs(ALvec(i)) > 1d-10) then
                     eps1(klay) = max(eps1(klay), w / ALvec(i))
                  else
                     continue
                  end if
               end if

            end do
         end do
      end do

!    Implementation of filter for both sigma and z layers:
      do L = Lbot(LL), Ltop(LL)
         klay = L - Lbot(LL) + 1

         vicouv = vicLu(L)

!       compute third-order filter coefficient from first-order filter coefficient
         eps3 = 0.25 * Deltax(LL)**2 * eps1(klay)

!       compute second-order filter coefficient
         eps2 = max(eps3, 0.25 * Deltax(LL)**2 * vicouv)

         if (order == 1) then
            eps(klay, LL) = eps1(klay)
         else if (order == 2) then
            eps(klay, LL) = eps2
         else if (order == 3) then
            eps(klay, LL) = eps3
         end if
      end do
   end do

   ierror = 0
1234 continue

   return
end subroutine get_filter_coeff
