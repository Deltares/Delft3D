!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_s1nod
   use m_update_matrix, only: update_matrix

   implicit none

   private

   public :: s1nod

contains

!> nodes in continuity eq
   subroutine s1nod()
      use precision, only: dp
      use precision_basics, only: comparereal
      use m_plotdots, only: adddot
      use messagehandling, only: setmessage, level_warn
      use m_flow, only: a1, bb, nonlin, a1m, dd, s1, vol0, vol1, s1m, s0, nbndz, kbndz, zbndz, zbndz0, u0, epshs, hs, ag, kmx, fu, ru, lbot, ltop, epshu, hu, nbndu, kbndu, jacheckmatrix
      use m_flowgeom, only: ndx, ndx2d, xz, yz, nd, lnx1d, kfs, dx, dxi, bl, ndxi
      use m_flowtimes, only: dti, refdat, time1, alfsmo, dts
      use m_reduce, only: bbr, ddr, ccr, lv2
      use m_partitioninfo, only: jampi, idomain, my_rank, qnerror, jaoverlap
      use m_sobekdfm, only: nbnd1d2d, compute_1d2d_boundaries
      use unstruc_channel_flow, only: network
      use time_module, only: seconds_to_datetimestring
      use m_branch, only: t_branch
      use iso_c_utils, only: MAXSTRINGLEN
      use m_water_level_boundary, only: correct_water_level_boundary
      use m_boundary_condition_type, only: BOUNDARY_WATER_LEVEL, BOUNDARY_WATER_LEVEL_NEUMANN, &
                                           BOUNDARY_VELOCITY_RIEMANN, BOUNDARY_WATER_LEVEL_OUTFLOW, &
                                           BOUNDARY_DISCHARGE_HEAD

      integer :: n
      integer :: kb, k2, L, k, LL, itpbn
      integer :: ibr
      real(kind=dp) :: dtiba, hh, water_level_boundary, dtgh
      real(kind=dp) :: sqrtgfh, cffu, rowsum, fuL, ruL, huL, hep
      integer :: i, ierr
      character(len=2) :: dim_text
      real(kind=dp), parameter :: HBMIN = 1.0e-3_dp
      real(kind=dp), pointer, dimension(:) :: gridPointsChainages
      type(t_branch), pointer, dimension(:) :: branch
      logical :: domainCheck
      character(len=MAXSTRINGLEN) :: msgbufpar ! can not use msgbuf, as each OpenMP thread must have it's own

      !bbr = bb + dti*a1     !m2/s
      !ddr = dd + dti*a1*s1  !m3/s

! BEGIN DEBUG
      !if ( jampi.eq.1 ) then
      !   call reduce_dt()
      !   call update_ghosts(ITYPE_Sall, Ndx, s1, ierr)
      !   call update_ghosts(ITYPE_Sall, Ndx, a1, ierr)
      !   call update_ghosts(ITYPE_Sall, Ndx, vol0, ierr)
      !   call update_ghosts(ITYPE_Sall, Ndx, vol1, ierr)
      !   call update_ghosts(ITYPE_U, 1,    Lnx, fu,   ierr)
      !   call update_ghosts(ITYPE_U, 1,    Lnx, ru,   ierr)
      !end if
! END DEBUG

 !! remove entries for 1d2d nodes in bb, dd and ccr
      !  do n  = 1, nbnd1d2d                                   ! 1D2D boundaries
      !   kb      = kbnd1d2d(1,n)
      !   k2      = kbnd1d2d(2,n)
      !   L       = kbnd1d2d(3,n)
      !   bb(k2) = bb(k2) -bb(kb)
      !   dd(k2) = dd(k2) + dd(kb)
      !   bb(kb) = 0d0
      !   dd(kb) = 0d0
      !   ccr(lv2(L)) = 0d0
      ! end do

      !$OMP PARALLEL DO           &
      !$OMP PRIVATE(n,dtiba,domaincheck,dim_text,L,i,LL,ibr,branch,gridPointsChainages,k)
      do n = 1, ndx ! Waterlevels, = s1ini
         dtiba = dti * a1(n)
         bbr(n) = bb(n) + dtiba ! need it also for kfs.ne.1 at the boundaries (for parallel runs, see partition_setkfs)
         if (nonlin >= 2) then ! pressurised
            bbr(n) = bbr(n) - dti * a1m(n)
         end if

         domaincheck = .true.
         if (jampi == 1) then
            domaincheck = (idomain(n) == my_rank)
         end if

         ! Check for zero (0d0) value on diagonal, to print a warning before a resulting Saad crash.
         if (comparereal(bbr(n), 0.0_dp) == 0 .and. domainCheck) then
            if (n <= ndx2d) then
               dim_text = '2D'
            else
               dim_text = '1D'
            end if
            write (msgbufpar, '(a, i0, a)') 'The surface area of '//dim_text//'-node with node number ''', n, ''' is equal to 0'
            call setMessage(LEVEL_WARN, msgbufpar)
            call SetMessage(-1, 'This might lead to a SAAD error in the solve process')
            write (msgbufpar, '(a)') 'Current time is: '
            call seconds_to_datetimestring(msgbufpar(18:), refdat, time1)
            call setMessage(-1, msgbufpar)
            write (msgbufpar, '(a,f10.2,a,f10.2,a)') 'The location of the node is at (', xz(n), ',', yz(n), ')'
            call setMessage(-1, msgbufpar)
            L = -1
            if (n > ndx2d .and. network%loaded) then
               do i = 1, nd(n)%lnx
                  if (abs(nd(n)%ln(i)) <= lnx1d) then
                     L = abs(nd(n)%ln(i))
                     exit
                  end if
               end do
               if (L /= -1) then
                  ibr = network%adm%lin2ibr(L)
                  LL = network%adm%lin2local(L)

                  branch => network%brs%branch
                  gridPointsChainages => branch(ibr)%gridPointsChainages
                  ! UNST-4031: Dangerous code: this assumes that flow links and flow nodes
                  ! on a single branch are perfectly sorted, which is not required in input
                  ! networks. The k value below may not be correct.
                  if (nd(n)%ln(i) < 0) then
                     ! gridpoint at start of link internal gridpoint is equal to
                     k = LL
                  else
                     k = LL + 1
                  end if
                  write (msgbufpar, '(a, f9.2)') 'The gridpoint lies at branch with id '''//trim(branch(ibr)%id)//''' at chainage: ', gridPointsChainages(k)
                  call setMessage(-1, msgbufpar)
               end if
            end if
            call adddot(xz(n), yz(n), colournumber=247)
         end if

         if (kfs(n) == 1) then ! only for implicit points
            if (nonlin > 0) then
               ddr(n) = dd(n) + dtiba * s1(n) !
               ddr(n) = ddr(n) + dti * (vol0(n) - vol1(n))
               if (nonlin >= 2) then ! pressurised
                  ddr(n) = ddr(n) - s1m(n) * a1m(n) * dti
               end if
            else
               ddr(n) = dd(n) + dtiba * s0(n) ! Use s0 for the linear solver. Normally s0 = s1, however in
               ! iterative couplings this might not be the case (e.g. 1d2d
               ! SOBEK D-FlowFM coupling
            end if
         end if ! then also setback s1 !
      end do
      !$OMP END PARALLEL DO

      ! compute right-hand sides
      do n = 1, nbndz ! overrides for waterlevel boundaries
         kb = kbndz(1, n)
         k2 = kbndz(2, n)
         L = kbndz(3, n)
         itpbn = kbndz(4, n)
!    bbr(kb) = 1d0
         if (itpbn == BOUNDARY_WATER_LEVEL) then ! waterlevelbnd
            water_level_boundary = zbndz(n)
            if (alfsmo < 1.0_dp) then
               water_level_boundary = alfsmo * water_level_boundary + (1.0_dp - alfsmo) * zbndz0(n)
            end if
         else if (itpbn == BOUNDARY_WATER_LEVEL_NEUMANN) then ! neumannbnd, positive specified slope leads to inflow
            !water_level_boundary   = s1(k2) + zbndz(n)*dx(L)
            water_level_boundary = -zbndz(n) * dx(L) * ccr(Lv2(L)) ! right-hand side
         else if (itpbn == BOUNDARY_VELOCITY_RIEMANN) then ! Riemannbnd
!       hh   = max(epshs, 0.5d0*( hs(kb) + hs(k2) ) )
!       water_level_boundary   = 2d0*zbndz(n) - zbndz0(n) - sqrt(hh/ag)*u1(L)
            water_level_boundary = 2.0_dp * zbndz(n) - zbndz0(n)
         else if (itpbn == BOUNDARY_WATER_LEVEL_OUTFLOW) then ! outflowbnd
            if (u0(L) > 0.0_dp) then
               water_level_boundary = s1(k2)
            else
               hh = max(epshs, 0.5_dp * (hs(kb) + hs(k2)))
               dtgh = dts * (sqrt(ag * hh))
               water_level_boundary = s1(kb) - dtgh * (dxi(L) * (s1(kb) - s1(k2)) - zbndz(n)) ! verder testen
            end if
         else if (itpbn == BOUNDARY_DISCHARGE_HEAD) then ! qhbnd
            water_level_boundary = zbndz(n)
            if (alfsmo < 1.0_dp) then
               water_level_boundary = alfsmo * water_level_boundary + (1.0_dp - alfsmo) * zbndz0(n)
            end if
         end if

!   set matrix entries
         if (itpbn == BOUNDARY_WATER_LEVEL_NEUMANN) then
!      Neumann boundary condition
            if (ccr(Lv2(L)) == 0.0_dp) then ! internal cell is wet, but boundary face is inactive (see setkfs)
               ccr(Lv2(L)) = -bbr(kb)
               bbr(k2) = bbr(k2) + bbr(kb)
               ddr(k2) = ddr(k2) + ccr(Lv2(L)) * zbndz(n) * dx(L)
            end if

            bbr(kb) = -ccr(Lv2(L))
            ddr(kb) = -zbndz(n) * dx(L) * ccr(Lv2(L)) ! double for safety
         else if (itpbn == BOUNDARY_VELOCITY_RIEMANN) then
!      Riemann boundary condition (note: ccr= -Au theta fu)
            water_level_boundary = max(water_level_boundary, bl(kb) + HBMIN)
            if (ccr(Lv2(L)) == 0.0_dp) then ! internal cell is wet, but boundary face is inactive (see setkfs)
               ddr(kb) = bbr(kb) * water_level_boundary ! u(L)=0 assumed
            else
               hh = max(epshs, 0.5_dp * (hs(kb) + hs(k2)))
               sqrtgfh = sqrt(ag / hh)
               if (kmx == 0) then
                  fuL = fu(L)
                  ruL = ru(L)
               else
                  fuL = 0.0_dp
                  ruL = 0.0_dp
                  huL = 0.0_dp
                  do LL = Lbot(L), Ltop(L)
                     hep = max(epshu, hu(L) - hu(L - 1))
                     fuL = fuL + fu(LL) * hep
                     ruL = ruL + ru(LL) * hep
                     huL = huL + hep
                  end do
                  ful = fuL / huL
                  ruL = ruL / huL
               end if
               cffu = ccr(Lv2(L)) / fuL
               bbr(kb) = -cffu * (fuL + sqrtgfh)
               ddr(kb) = -cffu * (sqrtgfh * water_level_boundary - ruL)
            end if
         else
!      Dirichlet boundary condition

            call correct_water_level_boundary(water_level_boundary, kb)

            water_level_boundary = max(water_level_boundary, bl(kb) + HBMIN)

            ddr(kb) = bbr(kb) * water_level_boundary
            ddr(k2) = ddr(k2) - ccr(Lv2(L)) * water_level_boundary ! met link(L) in s1ini
            ccr(Lv2(L)) = 0.0_dp
         end if
      end do

      do n = 1, nbndu ! velocity boundaries
         kb = kbndu(1, n)
         k2 = kbndu(2, n)
         L = kbndu(3, n)
         !   bbr(kb) = 1d0
         !   ddr(kb) = s1(k2)
         !     SPvdP: apply Neumann conditions to water level at velocity boundaries
         ccr(Lv2(L)) = -bbr(k2) ! some non-zero value
         bbr(k2) = bbr(k2) - ccr(Lv2(L))
         bbr(kb) = -ccr(Lv2(L)) ! should not be zero
         ddr(kb) = 0.0_dp
      end do

      if (nbnd1d2d > 0) then
         call compute_1d2d_boundaries()
      end if

      jacheckmatrix = 0
      if (jacheckmatrix > 0) then
         do k = 1, ndxi
            rowsum = 0
            do LL = 1, size(nd(n)%ln)
               L = abs(nd(n)%ln(LL))
               rowsum = rowsum + abs(ccr(Lv2(L)))
            end do
            if (bbr(k) <= rowsum) then
               call qnerror('checkmatrix = nocheck', ' ', ' ')
            end if
         end do
      end if

! update overlapping ghost-parts of matrix
      if (jampi == 1 .and. jaoverlap == 1) then
         call update_matrix(ierr)
      end if

   end subroutine s1nod

end module m_s1nod
