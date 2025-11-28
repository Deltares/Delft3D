!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_flow_allocflow

   implicit none

   private

   public :: flow_allocflow

contains

   !> initialise flow model time independent parameters
   subroutine flow_allocflow()
      use precision, only: dp, sp
      use m_netw, only: kn
      use m_flowgeom, only: ndx, ln, lnx, lnx1d, ln2lne, bl, bob, kcu, lncn, ucnx, ucny, ndx2d, ndxi, lnxi
      use m_flow, only: s0, s00, s1, hs, a0, a1, cfs, negativedepths, negativedepths_cum, noiterations, noiterations_cum, &
                        limitingTimestepEstimation, limitingTimestepEstimation_cum, flowCourantNumber, kbot, ktop, ktop0, kmxn, Lbot, Ltop, &
                        kmxL, ustb, ustw, laydefnr, laytyp, laymx, nlaybn, nrlayn, jamapflowanalysis, mxlaydefs, kmx, kbotc, kmxc, &
                        layertype, LAYTP_SIGMA, LAYTP_DENS_SIGMA, LAYTP_Z, LAYTP_POLYGON_MIXED, &
                        numvertdis, mxlays, sdkx, dkx, zlaybot, iStrchType, zlaytop, Floorlevtoplay, dztop, dztopuniabovez, &
                        sini, sigmagrowthfactor, numtopsig, janumtopsiguniform, mxlayz, kmxx, zslay, &
                        dzslay, strch_user, laycof, strch_exponent, indlaynod, wflaynod, ndkx, jazlayeratubybob, lnkx, ln0, ucx, squ, sqi, dvyc, &
                        uqcx, uqcy, vol0, ucyq, vol1, ucy, qin, ucxq, vih, dvxc, vol1_f, sqa, volerror, sq, ucmag, jatrt, ucx_mor, ucy_mor, &
                        uc1d, u1du, japure1d, alpha_mom_1d, alpha_ene_1d, q1d, au1d, wu1d, sar1d, volu1d, freeboard, hsonground, volonground, &
                        qcur1d2d, vtot1d2d, qcurlat, vtotlat, s1gradient, squ2d, squcor, icorio, hus, ucz, rho, rhomean, rhowat, jatem, jasal, &
                        jacreep, baroclinic_force_prev, baroclinic_pressures, integrated_baroclinic_pressures, rhosww, qw, zws, ww1, zws0, keepzlayeringatbed, kmxd, &
                        workx, work1, work0, worky, jasecflow, spirint, zwsbtol, czusf, czssf, spircrv, ht_xy, spirfy, spirucm, ht_xx, spirfx, spirsrc, spiratx, &
                        spiraty, jabarrieradvection, struclink, ducxdx, ducydy, ducxdy, ducydx, dsadx, dsady, dsall, dteml, jatidep, jaselfal, tidep, &
                        limtypmom, limtypsa, tidef, s1init, jaselfalcorrectwlwithini, turkin0, tureps0, vicwws, turkin1, vicwwu, tureps1, tke_min, eps_min, &
                        turkinws, turepsws, sqcu, tqcu, eqcu, epsz0, z0ucur, z0urou, taus, taubxu, taubu, cfuhi, frcu, ifrcutp, u0, u1, q1, qa, map_fixed_weir_energy_loss, &
                        v, ucxu, ucyu, hu, huvli, au, au_nostrucs, viu, viclu, suu, advi, adve, plotlin, frcu_bkp, frcu_mor, jacali, ifrctypuni, jafrculin, &
                        frculin, u_to_umain, q1_main, cfclval, cftrt, jamap_chezy_elements, czs, jamap_chezy_links, jarhoxu, rhou, fu, czu, bb, ru, dd, sa1, &
                        salini, sam0, sam1, same, tem1, temini, background_air_temperature, background_humidity, background_cloudiness, soiltempthick, &
                        jahisheatflux, qtotmap, jamapheatflux, qevamap, qfrevamap, qconmap, qfrconmap, qsunmap, qlongmap, ustbc, idensform, jarichardsononoutput, &
                        q1waq, qwwaq, itstep, sqwave, infiltrationmodel, dfm_hyd_noinfilt, infilt, dfm_hyd_infilt_const, infiltcap, infiltcapuni, &
                        jagrw, pgrw, bgrw, sgrw1, sgrw0, h_aquiferuni, bgrwuni, janudge, zcs, use_density
      use m_flowtimes, only: dtcell, time_wetground, ja_timestep_auto, ja_timestep_nostruct, ti_waq
      use m_missing, only: dmiss
      use unstruc_model, only: md_netfile, md_vertplizfile
      use m_netw, only: numk, numl
      use m_alloc, only: aerr, realloc
      use m_sediment, only: stm_included, jased, sed, grainlay, mxgr, sdupq, jaceneqtr, blinc, sedi
      use m_ship, only: nshiptxy, zsp0, zspc, zspc0, v0ship, v1ship, qinship, shl, shb, shd, stuw, stuwmx, roer, fstuw, froer, roermx
      use m_sferic, only: jsferic
      use m_partitioninfo, only: jampi, reduce_double_min
      use m_integralstats, only: is_numndvals, is_maxvalsnd, is_sumvalsnd, is_valnamesnd
      use unstruc_channel_flow, only: network
      use m_bedform, only: bfm_included, bfmpar
      use m_fm_erosed, only: ucxq_mor, ucyq_mor, hs_mor
      use m_hydrology, only: jadhyd, alloc_hydrology, init_hydrology
      use m_qnerror, only: qnerror
      use m_get_zlayer_indices, only: getzlayerindices
      use m_get_zlayer_indices_bobL, only: getzlayerindicesbobL
      use m_filez, only: oldfil
      use m_wind, only: jarain, jaevap, jaqext, ja_computed_airdensity, cloudiness, rain, evap, air_temperature, heatsrc, heatsrc0, &
                        air_pressure, dew_point_temperature, relative_humidity, solar_radiation, net_solar_radiation, tbed, qext, qextreal, vextcum, cdwcof
      use m_nudge, only: nudge_temperature, nudge_salinity, nudge_time, nudge_rate
      use m_polygonlayering, only: polygonlayering
      use m_turbulence, only: potential_density, in_situ_density, difwws, rich, richs, drhodz
      use m_density_parameters, only: apply_thermobaricity
      use m_add_baroclinic_pressure, only: rhointerfaces
      use m_set_kbot_ktop, only: set_kbot_ktop
      use m_alloc, only: realloc

      integer :: ierr, n, k, mxn, j, kk, LL, L, k1, k2, k3, n1, n2, n3, n4, kb1, kb2, numkmin, numkmax, kbc1, kbc2
      integer :: nlayb, nrlay, nlayb1, nrlay1, nlayb2, nrlay2, Lb, Lt, mx, ltn, mpol, Lt1, Lt2, Ldn
      integer :: laybed, laytop, nrlayL, Lf, kuni, kb
      integer :: nlayb1L, nrlay1L, nlayb2L, nrlay2L
      integer :: ndx1d

      real(kind=dp) :: zmn, zmx, dzm
      real(kind=dp) :: gf, w1, w2, w3, zbt, zbb, dzb, gfi, gfk
      logical :: jawel

      if (ndx == 0) return

      call ilowercase(md_netfile) ! INTERACTOR!

      ! node related
      call realloc(s0, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('s0(ndx)', ierr, ndx)
      call realloc(s1, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('s1(ndx)', ierr, ndx)
      call realloc(a0, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('a0(ndx)', ierr, ndx)
      call realloc(a1, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('a1(ndx)', ierr, ndx)
      call realloc(hs, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('hs(ndx)', ierr, ndx)
      call realloc(s00, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('s00(ndx)', ierr, ndx)
      call realloc(cfs, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('cfs(ndx)', ierr, ndx)

      if (jamapFlowAnalysis > 0) then
         call realloc(negativeDepths, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('negativeDepths(ndx)', ierr, ndx)
         call realloc(negativeDepths_cum, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('negativeDepths_cum(ndx)', ierr, ndx)
         call realloc(noIterations, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('noIterations(ndx)', ierr, ndx)
         call realloc(noIterations_cum, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('noIterations_cum(ndx)', ierr, ndx)
         call realloc(limitingTimestepEstimation, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('limitingTimestepEstimation(ndx)', ierr, ndx)
         call realloc(limitingTimestepEstimation_cum, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('limitingTimestepEstimation_cum(ndx)', ierr, ndx)
         call realloc(flowCourantNumber, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('flowCourantNumber(ndx)', ierr, ndx)
      end if

      call realloc(kbot, ndx, stat=ierr, keepexisting=.false.)
      call aerr('kbot(ndx)', ierr, ndx)
      call realloc(ktop, ndx, stat=ierr, keepexisting=.false.)
      call aerr('ktop(ndx)', ierr, ndx)
      call realloc(ktop0, ndx, stat=ierr, keepexisting=.false.)
      call aerr('ktop0(ndx)', ierr, ndx)
      call realloc(kmxn, ndx, stat=ierr, keepexisting=.false.)
      call aerr('kmxn(ndx)', ierr, ndx)
      call realloc(Lbot, lnx, stat=ierr, keepexisting=.false.)
      call aerr('Lbot(lnx)', ierr, lnx)
      call realloc(Ltop, lnx, stat=ierr, keepexisting=.false.)
      call aerr('Ltop(lnx)', ierr, lnx)
      call realloc(kmxL, lnx, stat=ierr, keepexisting=.false.)
      call aerr('kmxL(lnx)', ierr, lnx)

      call realloc(ustb, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ustb(lnx)', ierr, lnx)
      call realloc(ustw, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ustw(lnx)', ierr, lnx)

      call realloc(laydefnr, ndx, stat=ierr, keepexisting=.false.)
      call aerr('laydefnr(ndx)', ierr, ndx)
      call realloc(laytyp, mxlaydefs, stat=ierr, keepexisting=.false.)
      call aerr('laytyp(mxlaydefs)', ierr, mxlaydefs)
      call realloc(laymx, mxlaydefs, stat=ierr, keepexisting=.false.)
      call aerr('laymx(mxlaydefs)', ierr, mxlaydefs)

      if (layertype /= LAYTP_SIGMA) then
         call realloc(nlaybn, ndx, stat=ierr, fill=0, keepexisting=.false.)
         call aerr('nlaybn(ndx)', ierr, ndx)
         call realloc(nrlayn, ndx, stat=ierr, fill=0, keepexisting=.false.)
         call aerr('nrlayn(ndx)', ierr, ndx)
      end if

      do k = 1, Ndx
         kbot(k) = k
         ktop(k) = k
         kmxn(k) = 1
      end do
      do L = 1, Lnx
         Lbot(L) = L
         Ltop(L) = L
         kmxL(L) = 1
      end do

      if (kmx > 0) then

         numkmin = int(1e8_dp)
         numkmax = -numkmin
         do Lf = Lnx1D + 1, Lnx ! we only need netnode nrs in 2D, todo: trim to numkmin
            L = ln2lne(Lf)
            if (kn(3, L) == 2) then
               numkmin = min(numkmin, kn(1, L), kn(2, L))
               numkmax = max(numkmax, kn(1, L), kn(2, L))
            end if
         end do

         numkmax = numk
         call realloc(kbotc, numkmax, stat=ierr, keepexisting=.false.)
         call aerr('kbotc(numkmax)', ierr, numkmax)
         call realloc(kmxc, numkmax, stat=ierr, keepexisting=.false.)
         call aerr('kmxc(numkmax)', ierr, numkmax)

         kbot(:) = 1
         ktop(:) = 1
         kmxn(:) = 1
         Lbot(:) = 1
         Ltop(:) = 1
         kmxL(:) = 1
         kbotc(:) = 1
         kmxc(:) = 1

         mxlays = kmx
         numvertdis = 3
         mxlaydefs = numvertdis
         mx = 0
         laydefnr = 1

         if (layertype == LAYTP_POLYGON_MIXED) then
            inquire (file=md_vertplizfile, exist=jawel)
            if (jawel) then
               call oldfil(mpol, md_vertplizfile)
            else
               call qnerror('vertical_layering.pliz not found, switch back to sigma', ' ', ' ')
               layertype = LAYTP_SIGMA
            end if
         end if

         if (layertype == LAYTP_SIGMA .or. layertype == LAYTP_DENS_SIGMA) then ! pure and density controlled sigma-layers
            mxlaydefs = 1
            laytyp(1) = 1
            laymx(1) = kmx
            if (layertype == LAYTP_DENS_SIGMA) then
               call realloc(sdkx, ndx, stat=ierr, keepexisting=.false.)
               call aerr('sdkx(ndx)', ierr, ndx)
               call realloc(dkx, ndx, stat=ierr, keepexisting=.false.)
               call aerr('dkx(ndx)', ierr, ndx)
            end if
         else if (layertype == LAYTP_Z) then ! all z
            mxlaydefs = 1
            laytyp(1) = 2

            if (zlaybot == dmiss) then
               zmn = bl(1)
               do n = 2, ndx
                  zmn = min(bl(n), zmn)
               end do
            else
               zmn = zlaybot
            end if

            if (jampi == 1) then
               call reduce_double_min(zmn)
            end if

            if (iStrchType >= 0) then
               if (zlaytop == dmiss) then
                  zmx = sini
               else
                  zmx = zlaytop
               end if
            else
               if (Floorlevtoplay == dmiss) then
                  zmx = sini
               else
                  if (dztop == dmiss) then
                     zmx = Floorlevtoplay
                  else
                     zmx = Floorlevtoplay + dztop
                  end if
               end if
            end if

            if (dztopuniabovez == dmiss) then
               zbt = zmn
            else
               zbt = max(zmn, dztopuniabovez)
            end if

            if (dztop == dmiss) then
               dzm = (zmx - zbt) / mxlayz
            else
               dzm = dztop
               mxlayz = (zmx - zbt) / dzm
               if (numtopsig > 0 .and. janumtopsiguniform == 1) then
                  mxlayz = max(mxlayz, numtopsig)
               end if
            end if

            kuni = mxlayz
            mx = kuni
            if (zbt > zmn) then ! count extra layers needed to fill out till bottom
               zbb = zbt
               dzb = dzm
               do while (zbb > zmn .and. mx < kmxx - 1)
                  dzb = dzb * sigmagrowthfactor
                  zbb = zbb - dzb
                  mx = mx + 1
               end do
            end if

            dzm = max(dzm, 1.0e-2_dp)
            mxlayz = mx
            kmx = mx ! repair code
            laymx(1) = mx
         else if (layertype == LAYTP_POLYGON_MIXED) then ! polygon defined z-layers
            call polygonlayering(mpol)
         end if
         do k = 1, mxlaydefs
            mx = max(mx, laymx(k))
         end do

         call realloc(zslay, uindex=[mx, mxlaydefs], lindex=[0, 1], stat=ierr, keepexisting=.false.)
         call realloc(dzslay, uindex=[mx, mxlaydefs], lindex=[0, 1], stat=ierr, fill=0.0_dp, keepexisting=.false.)

         if (iStrchType == STRCH_USER) then
            do j = 1, mxlaydefs
               mx = laymx(j)
               do k = 1, mx
                  dzslay(k, j) = laycof(k) / 100.0_dp
               end do
            end do

         elseif (iStrchType == STRCH_EXPONENT) then
            gfi = 1.0_dp / laycof(2)
            gf = laycof(3)
            do j = 1, mxlaydefs
               mx = laymx(j)
               k1 = laycof(1) * mx
               gfk = gfi**k1
               if (gfk == 1.0_dp) then
                  gfi = 1.0_dp
                  dzslay(1, j) = 1.0_dp / mx
               else
                  dzslay(1, j) = (1.0_dp - gfi) / (1.0_dp - gfk) * laycof(1)
               end if
               do k = 2, k1
                  dzslay(k, j) = dzslay(k - 1, j) * gfi
               end do
               gfk = gf**(kmx - k1)
               if (k1 < kmx) then
                  if (gfk == 1.0_dp) then
                     gf = 1.0_dp
                     dzslay(k1 + 1, j) = 1.0_dp / mx
                  else
                     dzslay(k1 + 1, j) = (1.0_dp - gf) / (1.0_dp - gfk) * (1.0_dp - laycof(1))
                  end if
                  do k = k1 + 2, mx
                     dzslay(k, j) = dzslay(k - 1, j) * gf
                  end do
               end if
            end do
         else
            do j = 1, mxlaydefs
               mx = laymx(j)
               do k = 1, mx
                  dzslay(k, j) = 1.0_dp / mx
               end do
            end do
         end if

         do j = 1, mxlaydefs
            mx = laymx(j)
            if (laytyp(j) == 1) then

               zslay(0, j) = 0.0_dp
               do k = 1, mx
                  zslay(k, j) = zslay(k - 1, j) + dzslay(k, j)
               end do

            else if (laytyp(j) == 2) then

               call realloc(zslay, uindex=[mx, mxlaydefs], lindex=[0, 1], stat=ierr, keepexisting=.false.) ! nr of layer distributions

               if (iStrchType >= 0) then
                  zslay(0, j) = zmn
                  do k = 1, mx
                     zslay(k, j) = zslay(k - 1, j) + dzslay(k, j) * (zmx - zmn)
                  end do
               else
                  zslay(0, j) = zmn
                  zslay(mx, j) = zmx
                  do k = mx - 1, mx - kuni, -1
                     zslay(k, j) = zslay(k + 1, j) - dzm
                  end do

                  dzb = dzm
                  do k = mx - kuni - 1, 1, -1
                     dzb = dzb * sigmagrowthfactor
                     zslay(k, j) = zslay(k + 1, j) - dzb
                  end do
               end if
            end if
         end do

         kk = Ndx
         do n = 1, ndx

            kbot(n) = 0
            kk = kk + 1 ! ghost cell for everyone

            Ldn = laydefnr(n)
            if (Ldn >= 1) then
               if (laytyp(Ldn) == 1) then
                  mx = laymx(Ldn)
                  kmxn(n) = mx
               else if (laytyp(Ldn) == 2) then
                  call getzlayerindices(n, nlayb, nrlay)
                  kmxn(n) = nrlay
               end if
            end if

         end do

         kk = Ndx
         do n = 1, ndx ! Count ndkx + set kbot array

            kbot(n) = 0
            kk = kk + 1

            Ldn = laydefnr(n)
            if (Ldn == 0) then
               k1 = indlaynod(1, n)
               k2 = indlaynod(2, n)
               k3 = indlaynod(3, n)
               w1 = wflaynod(1, n)
               w2 = wflaynod(2, n)
               w3 = wflaynod(3, n)
               kmxn(n) = max(1, nint(w1 * kmxn(k1) + w2 * kmxn(k2) + w3 * kmxn(k3)))
            end if

            do k = 1, kmxn(n)
               kk = kk + 1
               if (k == 1) then
                  kbot(n) = kk
               end if
            end do
         end do
         ndkx = kk

         LL = Lnx ! Stapelen vanaf grondlaag
         do L = 1, lnx
            n1 = ln(1, L)
            n2 = ln(2, L)
            kmxL(L) = min(kmxn(n1), kmxn(n2))

            if (jaZlayeratubybob == 1 .and. kmxL(L) > numtopsig) then
               call getzlayerindicesbobL(n1, nlayb1, nrlay1, min(bob(1, L), bob(2, L)))
               call getzlayerindicesbobL(n2, nlayb2, nrlay2, min(bob(1, L), bob(2, L)))
               kmxL(L) = min(nrlay1, nrlay2)
            end if

            if (abs(kcu(L)) == 2) then
               n3 = lncn(1, L)
               n4 = lncn(2, L)
               kmxc(n3) = max(kmxc(n3), kmxL(L))
               kmxc(n4) = max(kmxc(n4), kmxL(L))
            end if

            do k = 0, kmxL(L)
               LL = LL + 1
            end do
         end do
         Lnkx = LL

         call realloc(ln, [2, Lnkx])
         call realloc(lncn, [2, Lnkx])

         LL = Lnx ! Stapelen vanaf grondlaag

         kk = numk ! setup cornerpoint admin
         do n = numkmin, numkmax
            do k = 0, kmxc(n)
               kk = kk + 1
               if (k == 1) then
                  kbotc(n) = kk
               end if
            end do
         end do
         call realloc(ucnx, kk)
         call realloc(ucny, kk)

         do L = 1, lnx
            n1 = ln(1, L)
            n2 = ln(2, L)
            n3 = lncn(1, L)
            n4 = lncn(2, L)

            Lt1 = 0
            Lt2 = 0
            if (laydefnr(n1) > 0 .and. laydefnr(n2) > 0) then
               Lt1 = laytyp(laydefnr(n1))
               Lt2 = laytyp(laydefnr(n2))
            end if

            if (Lt1 == 2 .and. Lt2 == 2) then

               call getzlayerindices(n1, nlayb1, nrlay1) ! connection to be made at bedcell of highest adjacent cell
               call getzlayerindices(n2, nlayb2, nrlay2)
               kb1 = max(0, nlayb2 - nlayb1)
               kb2 = max(0, nlayb1 - nlayb2)

               if (jaZlayeratubybob == 1 .and. kmxL(L) > numtopsig) then ! min(nrlay1,nrlay2) > numtopsig ) then ! connection to be made at first cell above local bob
                  ! but not if you are in numtopsig country
                  call getzlayerindicesbobL(n1, nlayb1L, nrlay1L, min(bob(1, L), bob(2, L)))
                  call getzlayerindicesbobL(n2, nlayb2L, nrlay2L, min(bob(1, L), bob(2, L)))
                  kb1 = nlayb1L - nlayb1
                  kb2 = nlayb2L - nlayb2
               end if

               laybed = max(nlayb1, nlayb2)
               laytop = min(nlayb1 + nrlay1, nlayb2 + nrlay2) ! should be identical for n1,n2,n3,n4
               nrlayL = laytop - laybed + 1

               kbc1 = kmxc(n3) - nrlayL
               kbc2 = kmxc(n4) - nrlayL

            else
               kb1 = 0
               kb2 = 0 ! linking starts at kbot(n1) + kb1 on left and at kbot(n2) + kb2 on right
               kbc1 = 0
               kbc2 = 0
            end if

            do k = 0, kmxL(L) ! 1 extra below bedlayer k = 1
               LL = LL + 1
               if (k == 1) then
                  Lbot(L) = LL
               end if
               if (k > 0) then
                  ln(1, LL) = kbot(n1) + kb1
                  ln(2, LL) = kbot(n2) + kb2
                  kb1 = kb1 + 1
                  kb1 = min(kb1, kmxn(n1))
                  kb2 = kb2 + 1
                  kb2 = min(kb2, kmxn(n2))

                  if (abs(kcu(L)) == 2) then
                     lncn(1, LL) = kbotc(n3) + kbc1
                     lncn(2, LL) = kbotc(n4) + kbc2
                     kbc1 = kbc1 + 1
                     kbc1 = min(kbc1, kmxc(n3))
                     kbc2 = kbc2 + 1
                     kbc2 = min(kbc2, kmxc(n4))
                  end if

               end if
            end do
         end do

         call realloc(ln0, [2, Lnkx])
         ln0 = ln

         do LL = 1, lnx ! only checking
            Lb = Lbot(LL)
            Lt = Lb + kmxL(LL) - 1
            n1 = ln(1, LL)
            n2 = ln(2, LL)
            do L = Lb, Lt
               k1 = ln(1, L)
               k2 = ln(2, L)
               if (k1 > kbot(n1) + kmxn(n1) - 1) then
                  ln(1, L) = k1
               end if
               if (k2 > kbot(n2) + kmxn(n2) - 1) then
                  ln(2, L) = k2
               end if
            end do
         end do

      else
         ndkx = ndx
         Lnkx = Lnx
      end if

      call realloc(ucx, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ucx(ndkx)', ierr, ndkx)
      call realloc(ucy, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ucy(ndkx)', ierr, ndkx)
      call realloc(uqcx, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('uqcx(ndkx)', ierr, ndkx)
      call realloc(uqcy, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('uqcy(ndkx)', ierr, ndkx)
      call realloc(ucxq, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ucxq(ndkx)', ierr, ndkx)
      call realloc(ucyq, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ucyq(ndkx)', ierr, ndkx)
      call realloc(ucmag, ndkx, stat=ierr, keepexisting=.false.)
      call aerr('ucmag(ndkx)', ierr, ndkx)
      call realloc(qin, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('qin(ndkx)', ierr, ndkx)
      call realloc(vih, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('vih(ndkx)', ierr, ndkx)
      call realloc(dvxc, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('dvxc(ndkx)', ierr, ndkx)
      call realloc(dvyc, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('dvyc(ndkx)', ierr, ndkx)
      call realloc(squ, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('squ(ndkx)', ierr, ndkx)
      call realloc(sqi, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('sqi(ndkx)', ierr, ndkx)
      call realloc(sq, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('sq(ndkx)', ierr, ndkx)
      call realloc(sqa, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('sqa(ndkx)', ierr, ndkx)
      call realloc(vol0, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('vol0(ndkx)', ierr, ndkx)
      call realloc(vol1, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('vol1(ndkx)', ierr, ndkx)
      call realloc(vol1_f, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('vol1_f(ndkx)', ierr, ndkx)
      call realloc(volerror, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('volerror(ndkx)', ierr, ndkx)

      if (stm_included .or. bfm_included .or. bfmpar%lfbedfrmrou .or. jatrt > 0) then
         allocate (ucxq_mor(1:ndkx), ucyq_mor(1:ndkx), hs_mor(1:ndkx), stat=ierr, source=0.0_dp)
         call realloc(ucx_mor, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ucx_mor(ndkx)', ierr, ndkx)
         call realloc(ucy_mor, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ucy_mor(ndkx)', ierr, ndkx)
      end if

      if (lnxi > 0 .and. kmx == 0) then
         call realloc(uc1D, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('uc1D(ndx)', ierr, ndx)
         call realloc(u1Du, lnx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('u1Du(lnx)', ierr, lnx)
         if (japure1D >= 3) then
            call realloc(alpha_mom_1D, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('alpha_mom_1D', ierr, ndx)
            call realloc(alpha_ene_1D, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('alpha_ene_1D', ierr, ndx)
            call realloc(q1D, [2, lnx], stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('q1D(2,lnx)', ierr, lnx)
            call realloc(au1D, [2, lnx], stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('au1D(2,lnx)', ierr, lnx)
            call realloc(wu1D, [2, lnx], stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('wu1D(2,lnx)', ierr, lnx)
            call realloc(sar1D, [2, lnx], stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('sar1D(2,lnx)', ierr, lnx)
            call realloc(volu1D, lnx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('volu1D(lnx)', ierr, lnx)
         end if
      end if

      if (kmx > 0) then
         call realloc(dtcell, ndkx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('dtcell(ndkx)', ierr, ndkx)
      else
         call realloc(dtcell, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('dtcell(ndx)', ierr, ndx)
      end if

      ! for 1D only
      if (network%loaded) then
         ndx1d = ndxi - ndx2d
         if (ndx1d > 0) then
            call realloc(time_wetground, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('time_wetground(ndx)', ierr, ndx)

            call realloc(freeboard, ndx1d, keepExisting=.false., fill=dmiss, stat=ierr)
            call aerr('freeboard(ndxi-ndx2d)', ierr, ndx1d)

            call realloc(hsOnGround, ndx1d, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('hsOnGround(ndxi-ndx2d)', ierr, ndx1d)

            call realloc(volOnGround, ndx1d, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('volOnGround(ndxi-ndx2d)', ierr, ndx1d)

            call realloc(qCur1d2d, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('qCur1d2d(ndx)', ierr, ndx)

            call realloc(vTot1d2d, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('vTot1d2d(ndx)', ierr, ndx)

            call realloc(qCurLat, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('qCurLat(ndx)', ierr, ndx)

            call realloc(vTotLat, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
            call aerr('vTotLat(ndx)', ierr, ndx)
         end if
         if (lnx1d > 0) then
            call realloc(s1Gradient, lnx, keepExisting=.false., fill=dmiss, stat=ierr)
            call aerr('s1Gradient', ierr, lnx)
         end if
      end if

      if (kmx > 0 .and. (ja_timestep_auto == 3 .or. ja_timestep_auto == 4)) then
         call realloc(squ2D, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('squ2D(ndkx)', ierr, ndkx)
      end if

      if (ja_timestep_auto == 1 .and. ja_timestep_nostruct > 0) then
         call realloc(squcor, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('squcor(ndx)', ierr, ndx)
      end if

      if (icorio == 7 .or. icorio == 8 .or. icorio == 27 .or. icorio == 28) then
         call realloc(hus, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('hus(ndkx)', ierr, ndkx)
      end if
      if (kmx > 0) then
         call realloc(ucz, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ucz(ndkx)', ierr, ndkx)
      end if

      call realloc(potential_density, ndkx, stat=ierr, fill=rhomean, keepexisting=.false.)
      call aerr('potential_density(ndkx)', ierr, ndkx)

      if (apply_thermobaricity) then
         call realloc(in_situ_density, ndkx, stat=ierr, fill=rhomean, keepexisting=.false.)
         call aerr('in_situ_density(ndkx)', ierr, ndkx)
         rho => in_situ_density
      else
         rho => potential_density
      end if

      if (stm_included) then
         call realloc(rhowat, ndkx, stat=ierr, fill=rhomean, keepexisting=.false.)
         call aerr('rhowat(ndkx)', ierr, ndkx)
      end if

      if (use_density() .or. stm_included) then
         call realloc(baroclinic_force_prev, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('baroclinic_force_prev(lnkx)', ierr, lnkx)
         call realloc(baroclinic_pressures, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('baroclinic_pressures(ndkx)', ierr, ndkx)
         call realloc(integrated_baroclinic_pressures, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('integrated_baroclinic_pressures(ndkx)', ierr, ndkx)

         if (rhointerfaces == 1) then
            call realloc(rhosww, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('rhosww(ndkx)', ierr, ndkx)
         end if
      end if

      if (kmx > 0) then
         call realloc(zws, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('zws(ndkx)', ierr, ndkx)
         call realloc(zws0, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('zws0(ndkx)', ierr, ndkx)
         call realloc(ww1, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ww1(ndkx)', ierr, ndkx)
         call realloc(qw, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('qw(ndkx)', ierr, ndkx)

         do n1 = 1, ndx
            Ldn = laydefnr(n1)
            Ltn = laytyp(Ldn)
            kb = kbot(n1)
            zws(kb - 1) = bl(n1) - zwsbtol
            if (Ltn == 2 .and. keepzlayeringatbed == 1 .and. kmxn(n1) > numtopsig) then
               call getzlayerindices(n1, nlayb1, nrlay1)
               zws(kb - 1) = zslay(nlayb1 - 1, Ldn)
            end if
         end do

      end if

      kmxd = max(1, kmx)

      call realloc(workx, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('workx(ndkx)', ierr, ndkx)
      call realloc(worky, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('worky(ndkx)', ierr, ndkx)
      call realloc(work0, uindex=[max(kmx, 1), max(ndx, lnx)], lindex=[0, 1], stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('work0 (0:max(kmx,1),max(ndx,lnx))', ierr, max(kmx + 1, 1) * max(ndx, lnx))
      call realloc(work1, [max(kmx, 1), max(ndx, lnx)], stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('work1 (max(kmx,1),max(ndx,lnx))', ierr, max(kmx, 1) * max(ndx, lnx))

      if (jasecflow > 0) then ! Secondary Flow
         call realloc(spirint, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('spirint(ndx)', ierr, ndx)
         call realloc(czusf, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('czusf(lnx)', ierr, lnx)
         call realloc(czssf, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('czssf(ndx)', ierr, ndx)

         if (kmx == 0) then
            call realloc(spircrv, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spircrv(ndx)', ierr, ndx)
            call realloc(spirint, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spirint(ndx)', ierr, ndx)
            call realloc(spirsrc, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spirsrc(ndx)', ierr, ndx)
            call realloc(spirfx, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spirfx(ndx)', ierr, ndx)
            call realloc(spirfy, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spirfy(ndx)', ierr, ndx)
            call realloc(spirucm, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spirucm(ndx)', ierr, ndx)
            call realloc(ht_xx, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('ht_xx(ndx)', ierr, ndx)
            call realloc(ht_xy, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('ht_xy(ndx)', ierr, ndx)
         else
            call realloc(spiratx, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spiratx(ndx)', ierr, ndx)
            call realloc(spiraty, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('spiraty(ndx)', ierr, ndx)
         end if
      end if

      if (jadhyd == 1) then
         call alloc_hydrology() ! allocate the hydrology module (for spatial input reading in flow_flowinit())
      end if

      if (jabarrieradvection == 3) then
         call realloc(struclink, lnx, stat=ierr, fill=0, keepexisting=.false.)
         call aerr('struclink(lnx)', ierr, lnx)
      end if

      if (limtypmom == 6) then
         call realloc(ducxdx, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ducxdx(ndkx)', ierr, ndkx)
         call realloc(ducxdy, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ducxdy(ndkx)', ierr, ndkx)
         call realloc(ducydx, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ducydx(ndkx)', ierr, ndkx)
         call realloc(ducydy, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('ducydy(ndkx)', ierr, ndkx)
      end if

      if (limtypsa == 6) then
         call realloc(dsadx, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('dsadx(ndkx)', ierr, ndkx)
         call realloc(dsady, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('dsady(ndkx)', ierr, ndkx)
      end if

      ! Anti-creep
      if (jacreep == 1 .and. (use_density() .and. jased < 4)) then
         if (kmx >= 2) then
            call realloc(dsalL, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('dsalL(lnkx)', ierr, lnkx)
            call realloc(dtemL, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('dtemL(lnkx)', ierr, lnkx)
         end if
      end if

      if (jsferic == 0) then
         jatidep = 0
         jaselfal = 0
      else if (jatidep > 0 .or. jaselfal > 0) then
         if (jaselfal > 0) then ! also store SAL potential
            call realloc(tidep, [2, ndx], stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('tidep(2,ndx)', ierr, 2 * ndx)
            if (jaSELFALcorrectWLwithIni == 1) then
               call realloc(s1init, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('s1init(ndx)', ierr, ndx)
            end if
         else
            call realloc(tidep, [1, ndx], stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('tidep(1,ndx)', ierr, ndx)
         end if
         call realloc(tidef, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('tidef(lnx)', ierr, lnx)
      end if

      if (kmx > 0) then ! turbulence arrays

         call realloc(turkin0, lnkx, stat=ierr, fill=tke_min, keepexisting=.false.)
         call aerr('turkin0(lnkx)', ierr, lnkx)
         call realloc(turkin1, lnkx, stat=ierr, fill=tke_min, keepexisting=.false.)
         call aerr('turkin1(lnkx)', ierr, lnkx)
         call realloc(tureps0, lnkx, stat=ierr, fill=eps_min, keepexisting=.false.)
         call aerr('tureps0(lnkx)', ierr, lnkx)
         call realloc(tureps1, lnkx, stat=ierr, fill=eps_min, keepexisting=.false.)
         call aerr('tureps1(lnkx)', ierr, lnkx)
         call realloc(vicwwu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('vicwwu(lnkx)', ierr, lnkx)
         call realloc(vicwws, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('vicwws(ndkx)', ierr, ndkx)
         call realloc(difwws, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('difwws(ndkx)', ierr, ndkx)
         call realloc(drhodz, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('drhodz(ndkx)', ierr, ndkx)

         call realloc(turkinws, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('turkinws(ndkx)', ierr, ndkx)
         call realloc(turepsws, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('turepsws(ndkx)', ierr, ndkx)

         call realloc(sqcu, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sqcu(ndkx)', ierr, ndkx)
         call realloc(tqcu, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('tqcu(ndkx)', ierr, ndkx)
         call realloc(eqcu, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('eqcu(ndkx)', ierr, ndkx)
      end if

      call realloc(z0ucur, lnx, stat=ierr, fill=epsz0, keepExisting=.false.)
      call aerr('z0ucur(lnx)', ierr, lnx)
      call realloc(z0urou, lnx, stat=ierr, fill=epsz0, keepExisting=.false.)
      call aerr('z0urou(lnx)', ierr, lnx)
      call realloc(taus, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('taus    (ndx)', ierr, ndx)
      call realloc(taubxu, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.) ! Always needs to be allocated, even if jawave == 0, used in gettau()
      call aerr('taubxu(lnx)', ierr, lnx)
      call realloc(taubu, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('taubu(lnx)', ierr, lnx)

      ! link related
      call realloc(cfuhi, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('cfuhi(lnx)', ierr, lnx)
      call realloc(frcu, lnx, stat=ierr, fill=dmiss, keepexisting=.false.)
      call aerr('frcu(lnx)', ierr, lnx)
      if (jacali == 1) then
         call realloc(frcu_bkp, lnx, stat=ierr, fill=dmiss, keepexisting=.false.)
         call aerr('frcu_bkp(lnx)', ierr, lnx)
      end if
      call realloc(frcu_mor, lnx, stat=ierr, fill=dmiss, keepexisting=.false.)
      call aerr('frcu_mor(lnx)', ierr, lnx)
      call realloc(ifrcutp, lnx, stat=ierr, fill=abs(ifrctypuni), keepexisting=.false.)
      call aerr('ifrcutp(lnx)', ierr, lnx)

      call realloc(u0, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('u0(lnkx)', ierr, lnkx)
      call realloc(u1, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('u1(lnkx)', ierr, lnkx)
      call realloc(q1, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('q1(lnkx)', ierr, lnkx)
      call realloc(qa, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('qa(lnkx)', ierr, lnkx)
      call realloc(map_fixed_weir_energy_loss, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('map_fixed_weir_energy_loss(lnkx)', ierr, lnkx)
      call realloc(v, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('v(lnkx)', ierr, lnkx)
      call realloc(ucxu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ucxu(lnkx)', ierr, lnkx)
      call realloc(ucyu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ucyu(lnkx)', ierr, lnkx)
      call realloc(hu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('hu(lnkx)', ierr, lnkx)
      call realloc(huvli, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('huvli(lnkx)', ierr, lnkx)
      call realloc(au, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('au(lnkx)', ierr, lnkx)
      call realloc(au_nostrucs, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('au_nostrucs(lnkx)', ierr, lnkx)
      call realloc(viu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('viu(lnkx)', ierr, lnkx)
      call realloc(vicLu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('vicLu(lnkx)', ierr, lnkx)
      call realloc(suu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('suu(lnkx)', ierr, lnkx)
      call realloc(advi, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('advi(lnkx)', ierr, lnkx)
      call realloc(adve, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('adve(lnkx)', ierr, lnkx)
      call realloc(plotlin, max(lnkx, ndkx), stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('plotlin(max(lnkx,ndkx))', ierr, lnkx)

      if (jafrculin > 0) then
         call realloc(frculin, lnx, stat=ierr, fill=dmiss, keepexisting=.false.)
         call aerr('frculin(lnx)', ierr, lnx)
      end if

      if (network%loaded .or. stm_included) then
         call realloc(u_to_umain, lnkx, stat=ierr, fill=1.0_dp, keepexisting=.false.)
         call aerr('u_to_umain(lnkx)', ierr, lnkx)
         call realloc(q1_main, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('q1_main(lnkx)', ierr, lnkx)
      end if

      if (jacali == 1) then
         call realloc(cfclval, numl, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('cfclval(numl)', ierr, numl)
      end if

      if (jatrt == 1) then
         call realloc(cftrt, [numl, 3], stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('cftrt(numl,3)', ierr, numl)
      end if

      if (jamap_chezy_elements > 0) then
         call realloc(czs, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('czs(ndx)', ierr, ndx)
      end if
      if (jamap_chezy_links > 0) then
         call realloc(czu, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('czu(lnx)', ierr, lnx)
      end if

      if (jarhoxu > 0 .or. jased > 0) then
         call realloc(rhou, lnkx, stat=ierr, fill=rhomean, keepexisting=.false.)
         call aerr('rhou(lnkx)', ierr, lnkx)
      end if

      ! m_dzstats
      if (is_numndvals > 0) then
         call realloc(is_maxvalsnd, [is_numndvals, ndx], keepExisting=.false., fill=0.0_dp)
         call realloc(is_sumvalsnd, [is_numndvals, ndx], keepExisting=.false., fill=0.0_dp)
         call realloc(is_valnamesnd, is_numndvals, keepExisting=.false., fill='')
      end if

      ! solving related
      call realloc(fu, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('fu(lnkx)', ierr, lnkx)
      call realloc(ru, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('ru(lnkx)', ierr, lnkx)
      call realloc(bb, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('bb(ndx)', ierr, ndx)
      call realloc(dd, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
      call aerr('dd(ndx)', ierr, ndx)

      if (jasal > 0 .or. kmx > 0) then
         call realloc(sa1, ndkx, stat=ierr, fill=salini, keepexisting=.false.)
         call aerr('sa1(ndkx)', ierr, ndkx)
         call realloc(sam0, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sam0(ndkx)', ierr, ndkx)
         call realloc(sam1, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sam1(ndkx)', ierr, ndkx)
         call realloc(same, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('same(ndkx)', ierr, ndkx)
      end if

      if (ja_computed_airdensity == 1) then
         call realloc(air_pressure, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('air_pressure(ndx)', ierr, ndx)

         call realloc(air_temperature, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('air_temperature(ndx)', ierr, ndx)

         call realloc(dew_point_temperature, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('dew_point_temperature(ndx)', ierr, ndx)

         call realloc(cloudiness, ndx, stat=ierr, fill=BACKGROUND_CLOUDINESS, keepexisting=.false.)
         call aerr('cloudiness(ndx)', ierr, ndx)
      end if

      if (jatem > 0) then
         call realloc(tem1, ndkx, stat=ierr, fill=temini, keepexisting=.false.)
         call aerr('tem1(ndkx)', ierr, ndkx)
         call realloc(heatsrc, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('heatsrc(ndkx)', ierr, ndkx)
         call realloc(heatsrc0, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('heatsrc0(ndkx)', ierr, ndkx)

         if (jatem > 1) then ! also heat modelling involved
            call realloc(air_temperature, ndx, stat=ierr, fill=BACKGROUND_AIR_TEMPERATURE, keepexisting=.false.)
            call aerr('air_temperature(ndx)', ierr, ndx)

            call realloc(relative_humidity, ndx, stat=ierr, fill=BACKGROUND_HUMIDITY, keepexisting=.false.)
            call aerr('relative_humidity(ndx)', ierr, ndx)

            call realloc(cloudiness, ndx, stat=ierr, fill=BACKGROUND_CLOUDINESS, keepexisting=.false.)
            call aerr('cloudiness(ndx)', ierr, ndx)

            call realloc(dew_point_temperature, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('dew_point_temperature(ndx)', ierr, ndx)

            call realloc(solar_radiation, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('solar_radiation(ndx)', ierr, ndx)

            call realloc(net_solar_radiation, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('net_solar_radiation(ndx)', ierr, ndx)

            if (Soiltempthick > 0) then
               call realloc(tbed, ndx, stat=ierr, fill=temini, keepexisting=.false.)
               call aerr('tbed(ndx)', ierr, ndx)
            end if
         end if

         if ((jamapheatflux > 0 .or. jahisheatflux > 0) .and. jatem > 1) then
            call realloc(qtotmap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('qtotmap(ndx)', ierr, ndx)
         end if

         if (jatem == 5) then ! save cd coeff if heat modelling also involved
            call realloc(cdwcof, lnx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('cdwcof(lnx)', ierr, lnx)

            if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! his or map output
               call realloc(qtotmap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('qtotmap(ndx)', ierr, ndx)
               call realloc(Qsunmap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('Qsunmap(ndx)', ierr, ndx)
               call realloc(Qevamap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('Qevamap(ndx)', ierr, ndx)
               call realloc(Qconmap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('Qconmap(ndx)', ierr, ndx)
               call realloc(Qlongmap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('Qlongmap(ndx)', ierr, ndx)
               call realloc(Qfrevamap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('Qfrevamap(ndx)', ierr, ndx)
               call realloc(Qfrconmap, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
               call aerr('Qfrconmap(ndx)', ierr, ndx)
            end if
         end if
      end if

      if (jased > 0 .and. jased < 4) then
         call realloc(sed, [mxgr, ndkx], stat=ierr, keepexisting=.false.)
         call aerr('sed(mxgr,ndkx)', ierr, ndkx * mxgr)

         call realloc(sdupq, [mxgr, ndkx], stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sdupq(mxgr,ndkx)', ierr, ndkx * mxgr)

         if (jaceneqtr == 1) then ! cell centre equilibrium transport concentration
            mxn = ndx
            call realloc(blinc, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('blinc(ndx)', ierr, ndx)
         else ! cell corner equilibrium transport concentration
            mxn = numk
            call realloc(sedi, [mxgr, ndx], stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('sedi(mxgr,ndx)', ierr, ndx * mxgr)
         end if
         call realloc(grainlay, [mxgr, mxn], stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('grainlay(mxgr,mxn)', ierr, mxgr * mxn)

         if (kmx > 0 .and. jased > 0 .and. jased < 4) then
            call realloc(ustbc, mxn, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('ustbc(mxn)', ierr, mxn)
         end if
      end if

      if (idensform > 0 .and. jaRichardsononoutput > 0) then
         call realloc(rich, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('rich(lnkx)', ierr, lnkx)
         call realloc(richs, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('richs(ndkx)', ierr, ndkx)
      else
         jaRichardsononoutput = 0
      end if

      if (ti_waq > 0) then
         call realloc(q1waq, lnkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('q1waq(lnkx)', ierr, lnkx)
         if (kmx > 0) then
            call realloc(qwwaq, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
            call aerr('qwwaq(ndkx)', ierr, ndkx)
         end if
      end if

      if (itstep == 4) then ! explicit time-step
         call realloc(sqwave, ndkx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sqwave(ndx)', ierr, ndx)
      end if

      if (infiltrationmodel /= DFM_HYD_NOINFILT) then
         call realloc(infilt, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('infilt(ndx)', ierr, ndx)

         if (infiltrationmodel == DFM_HYD_INFILT_CONST) then
            call realloc(infiltcap, ndx, stat=ierr, fill=infiltcapuni, keepexisting=.false.)
            call aerr('infiltcap(ndx)', ierr, ndx)
         end if
      end if

      if (jagrw > 0) then
         call realloc(sgrw0, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sgrw0(ndx)', ierr, ndx)
         call realloc(sgrw1, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sgrw1(ndx)', ierr, ndx)
         call realloc(pgrw, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('pgrw(ndx)', ierr, ndx)
         call realloc(bgrw, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('bgrw(ndx)', ierr, ndx)

         if (h_aquiferuni > 0.0_dp) then
            bgrw = bl - h_aquiferuni
         else
            bgrw = bgrwuni
         end if

      end if

      if (jarain > 0) then
         call realloc(rain, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('rain(ndx)', ierr, ndx)
      end if

      if (jaevap > 0) then
         call realloc(evap, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('evap(ndx)', ierr, ndx)
      end if

      if (jaQext > 0) then
         call realloc(qext, ndkx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('qext(ndkx)', ierr, ndkx)
         call realloc(qextreal, ndkx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('qextreal(ndkx)', ierr, ndkx)
         call realloc(vextcum, ndkx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('vextcum(ndkx)', ierr, ndkx)
      end if

      if (nshiptxy > 0) then
         call realloc(zsp0, numk, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('zsp0(numk)', ierr, numk)
         call realloc(zspc, numk, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('zspc(numk)', ierr, numk)
         call realloc(zspc0, numk, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('zspc0(numk)', ierr, numk)
         call realloc(v0ship, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('v0ship(ndx)', ierr, ndx)
         call realloc(v1ship, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('v1ship(ndx)', ierr, ndx)
         call realloc(qinship, ndx, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('qinship(ndx)', ierr, ndx)

         call realloc(shL, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('shL(2)', ierr, 2)
         call realloc(shB, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('shB(2)', ierr, 2)
         call realloc(shd, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('shd(2)', ierr, 2)
         call realloc(stuw, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('stuw(2)', ierr, 2)
         call realloc(fstuw, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('fstuw(2)', ierr, 2)
         call realloc(stuwmx, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('stuwmx(2)', ierr, 2)
         call realloc(roer, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('roer(2)', ierr, 2)
         call realloc(froer, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('froer(2)', ierr, 2)
         call realloc(roermx, 2, stat=ierr, fill=0.0_dp, keepExisting=.false.)
         call aerr('roermx(2)', ierr, 2)
      end if

      if (janudge == 1) then
         call realloc(nudge_temperature, Ndkx, fill=DMISS)
         call realloc(nudge_salinity, Ndkx, fill=DMISS)
         call realloc(zcs, Ndkx)
         call realloc(nudge_time, Ndx, fill=DMISS)
         call realloc(nudge_rate, Ndx, fill=DMISS)
      end if

      call set_kbot_ktop(jazws0=1)
   end subroutine flow_allocflow
end module m_flow_allocflow
