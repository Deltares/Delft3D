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

module m_transport_sub
   use m_update_constituents, only: update_constituents
   use m_apply_tracer_bc, only: apply_tracer_bc
   use m_setequilibriumsedimentbnds, only: setequilibriumsedimentbnds

   implicit none

   private

   public :: transport

contains

   !> transport for now, advect salinity and add
   !! high order limited terms to uqcx, uqcy
   subroutine transport()
      use precision, only: dp
      use m_density, only: set_potential_density, set_pressure_dependent_density
      use m_getverticallyaveraged
      use m_flowgeom, only: ln, ndxi, lnxi, ndx, lnx, ba, mxban, nban, banf, ban, xz
      use m_flow, only: apply_thermobaricity, jasal, maxitverticalforestersal, jatem, maxitverticalforestertem, limtyptm, &
                        limtypsed, iadvec, limtypmom, nbnds, kbnds, q1, kmxd, zbnds, salmax, kbndz, nbndu, kbndu, nbndsd, kbndsd, &
                        kmxl, nbndtm, kbndtm, zbndtm, nbndz, kbanz, kbanu, zbndsd, dvolbot, sam0tot, sam1tot, &
                        vol1, eps10, saminbnd, samoutbnd, qsho, samerr, kmxn, rhowat, jabaroctimeint, jarhoxu, &
                        rho0, potential_density, in_situ_density, rho, jacreep, lbot, ltop, rhou, kbot, kmx, kplotordepthaveraged, sa1, ndkx, ktop, zws
      use Timers, only: timstrt, timstop
      use m_sediment, only: jased, sedi, sed, dmorfac, tmorfspinup, jamorf, stm_included, jaceneqtr, blinc, ws, sed, sdupq, rhosed, rhobulkrhosed, grainlay, mxgr
      use m_netw, only: zk
      use m_flowtimes, only: keepstbndonoutflow, time1, tstart_user, dts, handle_extra
      use m_flowparameters, only: jadiagnostictransport
      use m_transport, only: numconst, constituents, isalt, itemp
      use m_laterals, only: average_concentrations_for_laterals, apply_transport_is_used
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_Lbot_Ltop, only: getlbotltop
      use m_get_equilibrium_transport_rates

      integer :: L, k, k1, k2, kb, n

      real(kind=dp) :: qb, wsemx, dgrlay, dtvi, hsk, dmorfax
      integer :: j, ki, jastep, cell_index_2d, cell_index_3d, kk
      integer :: LL, Lb, Lt, kt, km

      real(kind=dp) :: flx(mxgr) !< sed erosion flux (kg/s)                 , dimension = mxgr
      real(kind=dp) :: seq(mxgr) !< sed equilibrium transport rate (kg/m/s) , dimension = mxgr
      real(kind=dp) :: wse(mxgr) !< effective fall velocity (m/s)           , dimension = mxgr, ws*crefa=wse*seq

      real(kind=dp) :: valtop

      call timstrt('Transport', handle_extra(52)) ! transport

      if (jasal == 0) then
         ! limtypsa = 0
         maxitverticalforestersal = 0
      end if
      if (jatem == 0) then
         limtypTM = 0; maxitverticalforestertem = 0
      end if
      if (jased == 0) then
         limtypsed = 0
      end if
      if (iadvec == 0) then
         limtypmom = 0
      end if

      ! It is not allowed to change the selection of limiters for sal/tem/sed as defined in the input file. Therefore, the next line is now a comment (and will be removed in future)
      ! limtyp = max(Limtypsa, limtyptm, limtypsed)                   ! check if limiter need be applied

      if (jasal > 0) then

         do k = 1, nbnds ! set 1D or 3D sal boundary conditions
            LL = kbndS(3, k)
            call getLbotLtop(LL, Lb, Lt)
            kb = 0
            do L = Lb, Lt
               kb = ln(1, L); ki = ln(2, L)
               if (q1(L) >= 0 .or. keepstbndonoutflow == 1) then
                  kk = kmxd * (k - 1) + L - Lb + 1
                  constituents(isalt, kb) = zbnds(kk) ! inflow
                  salmax = max(salmax, constituents(isalt, kb))
               else
                  constituents(isalt, kb) = constituents(isalt, ki) ! outflow
               end if
            end do

            if (kb > 0) then
               valtop = constituents(isalt, kb)
               do L = Lt + 1, Lb + kmxL(LL) - 1
                  kb = ln(1, L)
                  constituents(isalt, kb) = valtop
               end do
            end if
         end do

      end if

      if (jatem > 0) then
         do k = 1, nbndtm ! set 1D or 3D temp boundary conditions
            LL = kbndTM(3, k)
            call getLbotLtop(LL, Lb, Lt)
            kb = 0
            do L = Lb, Lt
               kb = ln(1, L); ki = ln(2, L)
               if (q1(L) >= 0 .or. keepstbndonoutflow == 1) then
                  kk = kmxd * (k - 1) + L - Lb + 1
                  constituents(itemp, kb) = zbndTM(kk) ! inflow
               else
                  constituents(itemp, kb) = constituents(itemp, ki) ! outflow
               end if
            end do

            if (kb > 0) then
               valtop = constituents(itemp, kb)
               do L = Lt + 1, Lb + kmxL(LL) - 1
                  kb = ln(1, L)
                  constituents(itemp, kb) = valtop
               end do
            end if

         end do
      end if

      if (jased > 0 .and. jased < 4) then

         if (nbndz > 0) then
            call setequilibriumsedimentbnds(nbndz, 6, kbndz, kbanz, 0)
         end if
         if (nbndu > 0) then
            call setequilibriumsedimentbnds(nbndu, 6, kbndu, kbanu, 1)
         end if

         do k = 1, nbndsd ! set prescribed sediment boundary conditions
            kb = kbndSd(1, k)
            ki = kbndSd(2, k)
            L = kbndSd(3, k)
            do j = 1, mxgr
               if (q1(L) > 0) then
                  sed(j, kb) = zbndsd(k) ! inflow ,  todo, check vectormax over grainsizes if boundaryprescribed, else
               end if
            end do
         end do

         if (dmorfac > 0 .and. time1 >= tstart_user + TMorfspinup) then
            jamorf = 1
         end if
         dvolbot = 0.0_dp

      end if

      if (jadiagnostictransport == 0) then ! if jadiagnostictransport = 1 then update of constituents is skipped (all constituents are then "frozen")
         call apply_tracer_bc()
         call update_constituents(0) ! do all constituents
      end if

      if (jasal > 0) then !  compute salt error

         sam0tot = sam1tot
         sam1tot = 0.0_dp

         !$OMP PARALLEL DO &
         !$OMP PRIVATE(cell_index_3d,kb,kt,km,k) &
         !$OMP REDUCTION(+:sam1tot)
         do cell_index_2d = 1, ndxi
            call getkbotktop(cell_index_2d, kb, kt)
            if (kt < kb) cycle
            if (vol1(kb) < eps10) cycle
            km = kt - kb + 1

            do cell_index_3d = kb, kt
               sam1tot = sam1tot + constituents(isalt, cell_index_3d) * vol1(cell_index_3d)
            end do
         end do
         !$OMP END PARALLEL DO

         saminbnd = 0.0_dp; samoutbnd = 0.0_dp

         do LL = lnxi + 1, 0 !  lnx                                ! copy on outflow
            call getLbotLtop(LL, Lb, Lt)
            if (Lt < Lb) then
               cycle
            end if
            do L = Lb, Lt
               kb = ln(1, L); ki = ln(2, L)
               if (q1(L) > 0) then
                  saminbnd = saminbnd + q1(L) * constituents(isalt, kb) * dts ! mass in
               else
                  samoutbnd = samoutbnd - (q1(L) * constituents(isalt, ki) + qsho(L)) * dts ! mass out
               end if
            end do
         end do
         samerr = sam1tot - sam0tot !  - saminbnd + samoutbnd
      end if

      !$OMP PARALLEL DO &
      !$OMP PRIVATE(cell_index_2d)
      do cell_index_2d = 1, ndx
         call set_potential_density(potential_density, cell_index_2d)
      end do
      !$OMP END PARALLEL DO

      if (apply_thermobaricity) then
         !$OMP PARALLEL DO &
         !$OMP PRIVATE(cell_index_2d)
         do cell_index_2d = 1, ndx
            ! calculate the in-situ density (function of salinity, temperature and pressure)
            call set_pressure_dependent_density(in_situ_density, cell_index_2d)
         end do
         !$OMP END PARALLEL DO
      end if

      if (stm_included) then
         !$OMP PARALLEL DO &
         !$OMP PRIVATE(cell_index_2d,kb,kt,k)
         do cell_index_2d = 1, ndx
            call getkbotktop(cell_index_2d, kb, kt)
            do cell_index_3d = kt + 1, kb + kmxn(cell_index_2d) - 1
               rhowat(cell_index_3d) = rhowat(kt) ! UNST-5170
            end do
         end do
         !$OMP END PARALLEL DO
      end if

      ! propagate rho
      if (jabaroctimeint == 5) then ! rho advection
         dts = 0.5_dp * dts
         if (jarhoxu > 0) then
            rho0 = rho
         end if
         call update_constituents(1) ! do rho only
         dts = 2.0_dp * dts
      end if

      if (jarhoxu > 0 .and. jacreep == 1) then
         do LL = 1, lnx
            do L = Lbot(LL), Ltop(LL)
               k1 = ln(1, L); k2 = ln(2, L)
               rhou(L) = 0.5_dp * (rho(k1) + rho(k2))
            end do
         end do
      end if

      if (jased > 0 .and. jased < 4) then

         dmorfax = max(1.0_dp, dmorfac)

         if (jaceneqtr == 1) then ! original cell centre equilibriumtransport approach

            if (dmorfac > 0.0_dp) then
               blinc = 0.0_dp
            end if

            jastep = 1 ! 1 = first hor. transport, then limiting

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(k,flx,seq,wse,hsk,dtvi,wsemx,j,qb,dgrlay,kb) &
            !$OMP REDUCTION(+:dvolbot)
            do k = 1, ndxi
               kb = kbot(k)
               if (vol1(kb) > 0.0_dp) then

                  flx = 0.0_dp
                  if (kmx == 0) then
                     call getequilibriumtransportrates(k, seq, wse, mxgr, hsk) ! get per flowcell and store in small array seq
                  else
                     wse = ws
                     seq(1) = 0.0_dp
                  end if

                  dtvi = dts / vol1(kb)
                  wsemx = 0.45_dp * vol1(kb) / (ba(k) * dts)
                  do j = 1, mxgr

                     if (Wse(j) > wsemx) then
                        Wse(j) = wsemx
                     end if
                     qb = Wse(j) * ba(k) ! (m3/s)

                     if (jastep == 0) then
                        flx(j) = qb * (seq(j) - sed(j, kb)) ! (m3/s).(kg/m3) = kg/s   , positive = erosion
                        sed(j, kb) = sed(j, kb) + dtvi * (sdupq(j, kb) + flx(j)) ! horizontal + vertical transport
                     else
                        sed(j, kb) = sed(j, kb) + dtvi * (sdupq(j, kb)) ! horizontal transport
                        flx(j) = qb * (seq(j) - sed(j, kb)) ! (m3/s).(kg/m3) = kg/s   , positive = erosion
                        sed(j, kb) = sed(j, kb) + dtvi * (+flx(j)) ! vertical transport
                     end if

                     dgrlay = -dts * dmorfax * flx(j) / (rhosed(j) * ba(k) * rhobulkrhosed) ! (s)*( )* (kg/s) * (m3 / kg) / m2 = (m)

                     if (jamorf == 1) then
                        grainlay(j, k) = grainlay(j, k) + dgrlay
                        blinc(k) = blinc(k) + dgrlay
                        dvolbot = dvolbot + dgrlay * ba(k)
                     end if

                  end do

               else

                  sed(:, k) = 0.0_dp

               end if
            end do
            !$OMP END PARALLEL DO

         else

            sedi = 0.0_dp

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(kk,flx, seq, wse, hsk,n,k,dtvi,wsemx,j,qb,dgrlay,kb) &
            !$OMP REDUCTION(+:dvolbot)

            do kk = 1, mxban

               flx = 0.0_dp

               call getequilibriumtransportrates(kk, seq, wse, mxgr, hsk) ! get per netnode and store in small array seq

               n = nban(1, kk) ! net node
               k = nban(2, kk) ! flow node
               kb = kbot(k)

               if (vol1(kb) > 0 .and. hsk > 0) then

                  dtvi = dts / vol1(kb) ! (s/m3)
                  wsemx = 0.45_dp * vol1(kb) / (ba(k) * dts) ! (m/s) was 0.45

                  do j = 1, mxgr
                     if (Wse(j) > wsemx) then
                        Wse(j) = wsemx
                     end if
                     qb = Wse(j) * banf(kk) ! (m3/s)
                     flx(j) = qb * (seq(j) - sed(j, kb)) ! (m3/s).(kg/m3) = kg/s   , positive = erosion

                     !  if (zk(n) > skmx(n) ) then                           ! no flux if net point above max surrouding waterlevels
                     !     flx(j) = max( 0.0_dp, flx(j) )
                     !  endif

                     sedi(j, k) = sedi(j, k) + dtvi * flx(j) ! vertical transport (s/m3)*(kg/s) = (kg/m3)

                     dgrlay = -dts * dmorfax * flx(j) / (rhosed(j) * ban(n) * rhobulkrhosed) ! (s)*( )* (kg/s) * (m3 / kg) * (1/m2) = m

                     if (jamorf == 1) then
                        grainlay(j, n) = grainlay(j, n) + dgrlay
                        zk(n) = zk(n) + dgrlay
                        dvolbot = dvolbot + banf(kk) * dgrlay
                     end if
                  end do
               end if
            end do
            !$OMP END PARALLEL DO

            !$OMP PARALLEL DO &
            !$OMP PRIVATE(k,j,kb)
            do k = 1, ndxi
               kb = kbot(k)
               do j = 1, mxgr
                  sed(j, kb) = max(0.0_dp, sed(j, kb) + sedi(j, k))
               end do
            end do
            !$OMP END PARALLEL DO

         end if ! jacenterfluxes

      end if ! jased

      do LL = lnxi + 1, lnx ! copy on outflow
         call getLbotLtop(LL, Lb, Lt)
         if (Lt < Lb) then
            cycle
         end if
         do L = Lb, Lt
            if (q1(L) < 0) then
               kb = ln(1, L); ki = ln(2, L)
               if (jasal > 0 .and. keepstbndonoutflow == 0) then
                  constituents(isalt, kb) = constituents(isalt, ki)
               end if
               if (jatem > 0 .and. keepstbndonoutflow == 0) then
                  constituents(itemp, kb) = constituents(itemp, ki)
               end if
               if (jased > 0) then
                  do j = 1, mxgr
                     sed(j, kb) = sed(j, ki)
                  end do
               end if
            end if
         end do
      end do

      if (kplotordepthaveraged == 2) then
         if (jasal > 0) then
            call getverticallyaveraged(sa1, ndkx)
         end if
      end if

      do k = 1, 0 !  ndxi ! for test selectiveZ.mdu
         if (xz(k) > 270) then
            do cell_index_3d = kbot(k), ktop(k)
               if (zws(cell_index_3d) < -5.0_dp) then
                  constituents(isalt, cell_index_3d) = 30.0_dp
               else
                  constituents(isalt, cell_index_3d) = 0.0_dp
               end if
            end do
         end if
      end do

      if (numconst > 0 .and. apply_transport_is_used) then
         call average_concentrations_for_laterals(numconst, kmx, kmxn, vol1, constituents, dts)
      end if

      call timstop(handle_extra(52)) ! transport

   end subroutine transport

end module m_transport_sub
