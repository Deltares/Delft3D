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

!
!

! fill observation stations array
module m_fill_valobs

   implicit none

   private

   public :: fill_valobs

contains

   subroutine fill_valobs()
      use precision, only: dp
      use m_linkstocentercartcomp, only: linkstocentercartcomp
      use m_flow, only: kmx, realloc, ndkx, jawave, no_waves, jahistaucurrent, jahisvelocity, jahisvelvec, ucmag, jaeulervel, &
                        flowwithoutwaves, workx, taus, worky, jawaveswartdelwaq, jased, dmiss, jahistur, javiusp, viclu, viusp, &
                        vicouv, s1, nshiptxy, zsp, wave_surfbeat, ucx, ucy, zws, hs, epshu, ucz, jasal, jatem, jahisrho, &
                        potential_density, apply_thermobaricity, in_situ_density, squ, sqi, iturbulencemodel, vicwws, difwws, &
                        drhodz, brunt_vaisala_coefficient, idensform, jarichardsononoutput, richs, hu, vicwwu, turkin1, tureps1, &
                        rich, jahisrain, jahis_airdensity, infiltrationmodel, dfm_hyd_infilt_const, dfm_hyd_infilt_horton, &
                        jahisinfilt, infiltcap, infilt, jahisheatflux, qsunmap, qevamap, qconmap, qlongmap, qfrevamap, qfrconmap, qtotmap, &
                        use_density
      use m_flowtimes, only: handle_extra
      use m_transport, only: constituents, isalt, itemp, itra1, ised1
      use m_flowgeom, only: ndx, lnx, bl, nd, ln, wcl, bob, ba
      use m_observations_data, only : valobs, numobs, nummovobs, kobs, lobs, ipnt_s1, ipnt_hs, ipnt_bl, ipnt_cmx, cmxobs, ipnt_wx, ipnt_wy, ipnt_patm, ipnt_waver, ipnt_waveh, ipnt_wavet, ipnt_waved, ipnt_wavel, ipnt_waveu, ipnt_taux, ipnt_tauy, ival_sbcx1, ival_sbcxn, ipnt_sbcx1, ival_sbcy1, ival_sbcyn, ipnt_sbcy1, ival_sscx1, ival_sscxn, ipnt_sscx1, ival_sscy1, ival_sscyn, ipnt_sscy1, ival_sbwx1, ival_sbwxn, ipnt_sbwx1, ival_sbwy1, ival_sbwyn, ipnt_sbwy1, ival_sswx1, ival_sswxn, ipnt_sswx1, ival_sswy1, ival_sswyn, ipnt_sswy1, ipnt_taub, ival_bodsed1, ival_bodsedn, ipnt_bodsed1, ipnt_dpsed, ival_msed1, ival_msedn, ipnt_msed1, ival_lyrfrac1, ival_lyrfracn, ipnt_lyrfrac1, ipnt_poros, ipnt_thlyr, ival_frac1, ival_fracn, ipnt_frac1, ipnt_mudfrac, ipnt_sandfrac, ival_mfluff1, ival_mfluffn, ipnt_mfluff1, ival_fixfac1, ival_fixfacn, ipnt_fixfac1, ival_hidexp1, ival_hidexpn, ipnt_hidexp1, ival_sour1, ival_sourn, ipnt_sour1, ival_sink1, ival_sinkn, ipnt_sink1, ival_wqb1, ival_wqbn, ipnt_wqb1, ipnt_ucxq, ipnt_ucyq, ipnt_zcs, ipnt_ucx, ipnt_ucy, ipnt_ucxst, ipnt_ucyst, ipnt_ucz, ipnt_sa1, ipnt_tem1, ipnt_viu, ipnt_rhop, ipnt_rho, ipnt_umag, ipnt_qmag, ival_tra1, ival_tran, ipnt_tra1, ival_hwq1, ival_hwqn, ipnt_hwq1, ival_wqb3d1, ival_wqb3dn, ipnt_wqb3d1, ival_sf1, ival_sfn, ipnt_sf1, ival_ws1, ival_wsn, ipnt_ws1, ipnt_sed, ipnt_smx, smxobs, ipnt_zws, ipnt_vicwws, ipnt_difwws, ipnt_bruv, ipnt_richs, ival_seddif1, ival_seddifn, ipnt_seddif1, ipnt_zwu, ipnt_vicwwu, ipnt_tkin, ipnt_teps, ipnt_rich, ipnt_rain, ipnt_airdensity, ipnt_infiltcap, ipnt_infiltact, ipnt_wind, ipnt_tair, ipnt_rhum, ipnt_clou, ipnt_qsun, ipnt_qeva, ipnt_qcon, ipnt_qlon, ipnt_qfre, ipnt_qfrc, ipnt_qtot
      use m_sediment, only: jahissigwav, stm_included, stmpar, ustokes, hwav, twav, phiwav, rlabda, uorb, sedtra, fp, mtd, sed
      use Timers, only: timon, timstrt, timstop
      use m_gettaus, only: gettaus
      use m_gettauswave, only: gettauswave
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_Lbot_Ltop, only: getlbotltop
      use m_get_Lbot_Ltop_max, only: getlbotltopmax
      use m_get_layer_indices, only: getlayerindices
      use m_get_layer_indices_l_max, only: getlayerindiceslmax
      use m_reconstruct_ucz, only: reconstructucz
      use m_get_ucx_ucy_eul_mag, only: getucxucyeulmag
      use m_get_link1, only: getlink1
      use m_fm_wq_processes, only: kbx, wqbot, waqoutputs
      use m_xbeach_data, only: R
      use fm_statistical_output, only: model_is_3d
      use m_links_to_centers, only: links_to_centers
      use m_wind, only: wx, wy, jawind, air_pressure_available, air_pressure, jarain, rain, air_density, air_temperature, relative_humidity, cloudiness

      implicit none

      integer :: i, ii, j, kk, k, kb, kt, klay, L, LL, Lb, Lt, LLL, k1, k2, k3, n, nlayb, nrlay, nlaybL, nrlayLx
      integer :: link_id_nearest
      integer :: kmx_const, kk_const, nlyrs
      real(kind=dp) :: wavfac
      real(kind=dp) :: dens
      real(kind=dp) :: ux, uy, um
      real(kind=dp), allocatable :: wa(:, :)
      real(kind=dp), allocatable :: frac(:, :)
      real(kind=dp), allocatable :: poros(:)
      real(kind=dp), allocatable :: ueux(:)
      real(kind=dp), allocatable :: ueuy(:)
      real(kind=dp), allocatable :: vius(:) !< Flowlink-averaged horizontal viscosity (viu) at s-point

      kmx_const = kmx
      nlyrs = 0

      if (timon) call timstrt("fill_valobs", handle_extra(55))
      !
      if (.not. allocated(ueux)) then
         call realloc(ueux, ndkx, keepExisting=.false., fill=0.0_dp)
         call realloc(ueuy, ndkx, keepExisting=.false., fill=0.0_dp)
      end if
      !
      if (jawave > NO_WAVES) then
         if (jahissigwav == 0) then
            wavfac = 1.0_dp
         else
            wavfac = sqrt(2.0_dp)
         end if
         if (allocated(wa)) then
            deallocate (wa)
         end if
         allocate (wa(1:2, 1:max(kmx, 1)))
      end if

      ! get velocities here (and not at velocity writing)
      if (jahistaucurrent > 0 .or. jahisvelocity > 0 .or. jahisvelvec > 0) then
         call getucxucyeulmag(ndkx, ueux, ueuy, ucmag, jaeulervel, jahisvelocity)
      end if

      if (jahistaucurrent > 0) then
         if ((jawave == NO_WAVES .or. flowWithoutWaves)) then
            ! fill taus
            call gettaus(1, 1)

            ! get vector comps
            if (kmx == 0) then

               do k = 1, ndx
                  workx(k) = taus(k) * ueux(k) / max(ucmag(k), 1.0e-4_dp)
                  worky(k) = taus(k) * ueuy(k) / max(ucmag(k), 1.0e-4_dp)
               end do
            else
               do k = 1, ndx
                  call getkbotktop(k, kb, kt)
                  ux = ueux(kb); uy = ueuy(kb)
                  um = max(hypot(ux, uy), 1.0e-4_dp)
                  workx(k) = taus(k) * ux / um
                  worky(k) = taus(k) * uy / um
               end do
            end if
         else
            call gettauswave(jawaveswartdelwaq)
         end if
      end if
      !
      if (stm_included .and. jased > 0) then
         if (stmpar%morlyr%settings%iunderlyr == 2) then
            if (allocated(frac)) then
               deallocate (frac)
            end if
            allocate (frac(stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr))
            frac = dmiss
            if (allocated(poros)) then
               deallocate (poros)
            end if
            allocate (poros(1:stmpar%morlyr%settings%nlyr))
            poros = dmiss
         end if
      end if

      if (jahistur > 0) then
         if (.not. allocated(vius)) then
            allocate (vius(ndkx))
            ! Set initial value of horizontal viscosity to user-defined value
            if (javiusp == 1) then ! Spatially varying horizontal eddy viscosity
               if (model_is_3D()) then
                  do LL = 1, lnx
                     call getLbotLtopmax(LL, Lb, Lt)
                     do L = Lb, Lt
                        vicLu(L) = viusp(LL)
                     end do
                  end do
               else
                  vicLu(:) = viusp(:)
               end if
            else
               vicLu(:) = vicouv
            end if
         end if
         call links_to_centers(vius, vicLu)
      end if

      valobs = DMISS
      do i = 1, numobs + nummovobs
         k = max(kobs(i), 1)
         link_id_nearest = lobs(i)
         if (kobs(i) > 0) then ! rely on reduce_kobs to have selected the right global flow nodes

            if (model_is_3D()) then
               call getkbotktop(k, kb, kt)
               call getlayerindices(k, nlayb, nrlay)
               call reconstructucz(k)
            else
               kb = k
               kt = k
               nlayb = 1
            end if

            if (jawave > NO_WAVES .and. .not. flowWithoutWaves) then
               wa = 0.0_dp
               call linkstocentercartcomp(k, ustokes, wa) ! wa now 2*1 value or 2*1 vertical slice
            end if

!        store values in valobs work array
            valobs(i, :) = dmiss ! Intended to have dmiss on inactive layers for output.
            ! It is taken care of in subroutine reduce_valobs for parallel computation.
            valobs(i, IPNT_S1) = s1(k)
            if (nshiptxy > 0) then
               if (allocated(zsp)) then
                  valobs(i, IPNT_S1) = valobs(i, IPNT_S1) + zsp(k)
               end if
            end if

            valobs(i, IPNT_HS) = s1(k) - bl(k)

            valobs(i, IPNT_BL) = bl(k)

            valobs(i, IPNT_CMX) = cmxobs(i)
            if (jawind > 0) then
               valobs(i, IPNT_wx) = 0.0_dp
               valobs(i, IPNT_wy) = 0.0_dp
               do LL = 1, nd(k)%lnx
                  LLL = abs(nd(k)%ln(LL))
                  k1 = ln(1, LLL); k2 = ln(2, LLL)
                  k3 = 1; if (nd(k)%ln(LL) > 0) k3 = 2
                  valobs(i, IPNT_wx) = valobs(i, IPNT_wx) + wx(LLL) * wcL(k3, LLL)
                  valobs(i, IPNT_wy) = valobs(i, IPNT_wy) + wy(LLL) * wcL(k3, LLL)
               end do
            end if
            if (air_pressure_available .and. allocated(air_pressure)) then
               valobs(i, IPNT_PATM) = air_pressure(k)
            end if

            if (jawave == WAVE_SURFBEAT .and. allocated(R)) then
               valobs(i, IPNT_WAVER) = R(k)
            end if

            call collect_ice_values(valobs, i, k)

            if (jawave > NO_WAVES .and. allocated(hwav)) then
               valobs(i, IPNT_WAVEH) = hwav(k) * wavfac
               valobs(i, IPNT_WAVET) = twav(k)
               if (.not. flowWithoutWaves) then
                  valobs(i, IPNT_WAVED) = modulo(270.0_dp - phiwav(k), 360.0_dp) ! Direction from
                  valobs(i, IPNT_WAVEL) = rlabda(k)
                  valobs(i, IPNT_WAVEU) = uorb(k)
               end if
            end if

            if (jahistaucurrent > 0) then
               valobs(i, IPNT_TAUX) = workx(k)
               valobs(i, IPNT_TAUY) = worky(k)
            end if

            if (stm_included .and. jased > 0) then
               do j = IVAL_SBCX1, IVAL_SBCXN
                  ii = j - IVAL_SBCX1 + 1
                  valobs(i, IPNT_SBCX1 + ii - 1) = sedtra%sbcx(k, ii)
               end do
               do j = IVAL_SBCY1, IVAL_SBCYN
                  ii = j - IVAL_SBCY1 + 1
                  valobs(i, IPNT_SBCY1 + ii - 1) = sedtra%sbcy(k, ii)
               end do
               do j = IVAL_SSCX1, IVAL_SSCXN
                  ii = j - IVAL_SSCX1 + 1
                  valobs(i, IPNT_SSCX1 + ii - 1) = sedtra%sscx(k, ii)
               end do
               do j = IVAL_SSCY1, IVAL_SSCYN
                  ii = j - IVAL_SSCY1 + 1
                  valobs(i, IPNT_SSCY1 + ii - 1) = sedtra%sscy(k, ii)
               end do
               if (jawave > NO_WAVES .and. .not. flowWithoutWaves) then
                  do j = IVAL_SBWX1, IVAL_SBWXN
                     ii = j - IVAL_SBWX1 + 1
                     valobs(i, IPNT_SBWX1 + ii - 1) = sedtra%sbwx(k, ii)
                  end do
                  do j = IVAL_SBWY1, IVAL_SBWYN
                     ii = j - IVAL_SBWY1 + 1
                     valobs(i, IPNT_SBWY1 + ii - 1) = sedtra%sbwy(k, ii)
                  end do
                  do j = IVAL_SSWX1, IVAL_SSWXN
                     ii = j - IVAL_SSWX1 + 1
                     valobs(i, IPNT_SSWX1 + ii - 1) = sedtra%sswx(k, ii)
                  end do
                  do j = IVAL_SSWY1, IVAL_SSWYN
                     ii = j - IVAL_SSWY1 + 1
                     valobs(i, IPNT_SSWY1 + ii - 1) = sedtra%sswy(k, ii)
                  end do
               end if
               !
               valobs(i, IPNT_TAUB) = sedtra%taub(k) ! contains tausmax or Soulsby-Clarke shear stresses
               ! bed composition
               if (stmpar%morlyr%settings%iunderlyr == 1) then
                  do j = IVAL_BODSED1, IVAL_BODSEDN
                     ii = j - IVAL_BODSED1 + 1
                     valobs(i, IPNT_BODSED1 + ii - 1) = stmpar%morlyr%state%bodsed(ii, k)
                  end do
                  valobs(i, IPNT_DPSED) = stmpar%morlyr%state%dpsed(k)
               elseif (stmpar%morlyr%settings%iunderlyr == 2) then
                  nlyrs = stmpar%morlyr%settings%nlyr
                  do l = 1, stmpar%lsedtot
                     if (stmpar%morlyr%settings%iporosity == 0) then
                        dens = stmpar%sedpar%cdryb(l)
                     else
                        dens = stmpar%sedpar%rhosol(l)
                     end if
                     do n = 1, stmpar%morlyr%settings%nlyr
                        if (stmpar%morlyr%state%thlyr(n, k) > 0.0_fp) then ! lyrfrac
                           frac(l, n) = stmpar%morlyr%state%msed(l, n, k) / (dens * stmpar%morlyr%state%svfrac(n, k) * &
                                                                             stmpar%morlyr%state%thlyr(n, k))
                        else
                           frac(l, n) = 0.0_dp
                        end if
                     end do
                  end do
                  !
                  if (stmpar%morlyr%settings%iporosity > 0) then
                     poros = 1.0_dp - stmpar%morlyr%state%svfrac(:, k)
                  end if
                  !
                  do klay = 1, nlyrs
                     do j = IVAL_MSED1, IVAL_MSEDN
                        ii = j - IVAL_MSED1 + 1
                        valobs(i, IPNT_MSED1 + (ii - 1) * nlyrs + klay - 1) = stmpar%morlyr%state%msed(ii, klay, k)
                     end do
                     !
                     do j = IVAL_LYRFRAC1, IVAL_LYRFRACN
                        ii = j - IVAL_LYRFRAC1 + 1
                        valobs(i, IPNT_LYRFRAC1 + (ii - 1) * nlyrs + klay - 1) = frac(ii, klay)
                     end do
                     !
                     valobs(i, IPNT_POROS + klay - 1) = poros(klay)
                     valobs(i, IPNT_THLYR + klay - 1) = stmpar%morlyr%state%thlyr(klay, k)
                  end do
               end if
               !
               do j = IVAL_FRAC1, IVAL_FRACN
                  ii = j - IVAL_FRAC1 + 1
                  valobs(i, IPNT_FRAC1 + ii - 1) = sedtra%frac(k, ii)
               end do
               valobs(i, IPNT_MUDFRAC) = sedtra%mudfrac(k)
               valobs(i, IPNT_SANDFRAC) = sedtra%sandfrac(k)
               !
               if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
                  do j = IVAL_MFLUFF1, IVAL_MFLUFFN
                     ii = j - IVAL_MFLUFF1 + 1
                     valobs(i, IPNT_MFLUFF1 + ii - 1) = stmpar%morpar%flufflyr%mfluff(ii, k)
                  end do
               end if
               !
               do j = IVAL_FIXFAC1, IVAL_FIXFACN
                  ii = j - IVAL_FIXFAC1 + 1
                  valobs(i, IPNT_FIXFAC1 + ii - 1) = sedtra%fixfac(k, ii)
               end do
               !
               do j = IVAL_HIDEXP1, IVAL_HIDEXPN
                  ii = j - IVAL_HIDEXP1 + 1
                  valobs(i, IPNT_HIDEXP1 + ii - 1) = sedtra%hidexp(k, ii)
               end do
               !
               if (stmpar%lsedsus > 0) then
                  do j = IVAL_SOUR1, IVAL_SOURN
                     ii = j - IVAL_SOUR1 + 1
                     valobs(i, IPNT_SOUR1 + ii - 1) = sedtra%sourse(k, ii)
                  end do
                  do j = IVAL_SINK1, IVAL_SINKN
                     ii = j - IVAL_SINK1 + 1
                     valobs(i, IPNT_SINK1 + ii - 1) = sedtra%sinkse(k, ii)
                  end do
               end if
            end if
            !
            if (IVAL_WQB1 > 0) then
               do j = IVAL_WQB1, IVAL_WQBN
                  ii = j - IVAL_WQB1 + 1
                  valobs(i, IPNT_WQB1 + ii - 1) = wqbot(ii, kb)
               end do
            end if

            if (model_is_3D()) then
               valobs(i, IPNT_UCXQ) = ucx(k)
               valobs(i, IPNT_UCYQ) = ucy(k)
            end if

            do kk = kb, kt
               klay = kk - kb + nlayb

               if (model_is_3D()) then
                  valobs(i, IPNT_ZCS + klay - 1) = 0.5_dp * (zws(kk) + zws(kk - 1))
               end if

               if (jahisvelocity > 0 .or. jahisvelvec > 0) then
                  valobs(i, IPNT_UCX + klay - 1) = ueux(kk)
                  valobs(i, IPNT_UCY + klay - 1) = ueuy(kk)
               end if

               if (jawave > NO_WAVES .and. .not. flowWithoutWaves) then
                  if (hs(k) > epshu) then
                     if (kmx == 0) then
                        kk_const = 1
                     else
                        kk_const = klay
                     end if
                     valobs(i, IPNT_UCXST + klay - 1) = wa(1, kk_const)
                     valobs(i, IPNT_UCYST + klay - 1) = wa(2, kk_const)
                  end if
               end if

               if (model_is_3D()) then
                  valobs(i, IPNT_UCZ + klay - 1) = ucz(kk)
               end if
               if (jasal > 0) then
                  valobs(i, IPNT_SA1 + klay - 1) = constituents(isalt, kk)
               end if
               if (jatem > 0) then
                  valobs(i, IPNT_TEM1 + klay - 1) = constituents(itemp, kk)
               end if
               if (jahistur > 0) then
                  valobs(i, IPNT_VIU + klay - 1) = vius(kk)
               end if
               if (use_density() .and. jahisrho > 0) then
                  valobs(i, IPNT_RHOP + klay - 1) = potential_density(kk)
                  if (apply_thermobaricity) then
                     valobs(i, IPNT_RHO + klay - 1) = in_situ_density(kk)
                  end if
               end if
               if (jahisvelocity > 0) then
                  valobs(i, IPNT_UMAG + klay - 1) = ucmag(kk)
               end if
               valobs(i, IPNT_QMAG + klay - 1) = 0.5_dp * (squ(kk) + sqi(kk))

               if (kmx == 0) then
                  kmx_const = 1 ! to make numbering below work
               end if

               if (IVAL_TRA1 > 0) then
                  do j = IVAL_TRA1, IVAL_TRAN
                     ii = j - IVAL_TRA1 + 1
                     valobs(i, IPNT_TRA1 + (ii - 1) * kmx_const + klay - 1) = constituents(ITRA1 + ii - 1, kk)
                  end do
               end if

               if (IVAL_HWQ1 > 0) then
                  do j = IVAL_HWQ1, IVAL_HWQN
                     ii = j - IVAL_HWQ1 + 1
                     valobs(i, IPNT_HWQ1 + (ii - 1) * kmx_const + klay - 1) = waqoutputs(ii, kk - kbx + 1)
                  end do
               end if

               if (IVAL_WQB3D1 > 0) then
                  do j = IVAL_WQB3D1, IVAL_WQB3DN
                     ii = j - IVAL_WQB3D1 + 1
                     valobs(i, IPNT_WQB3D1 + (ii - 1) * kmx_const + klay - 1) = wqbot(ii, kk)
                  end do
               end if

               if (IVAL_SF1 > 0) then
                  do j = IVAL_SF1, IVAL_SFN
                     ii = j - IVAL_SF1 + 1
                     valobs(i, IPNT_SF1 + (ii - 1) * kmx_const + klay - 1) = constituents(ISED1 + ii - 1, kk)
                  end do
               end if

               if (kmx == 0 .and. IVAL_WS1 > 0) then
                  do j = IVAL_WS1, IVAL_WSN
                     ii = j - IVAL_WS1 + 1
                     valobs(i, IPNT_WS1 + (ii - 1) * kmx_const + klay - 1) = mtd%ws(kk, ii) ! 1:lsedsus
                  end do
               end if

               if (jased > 0 .and. .not. stm_included) then
                  valobs(i, IPNT_SED + klay - 1) = sed(1, kk)
               end if
               valobs(i, IPNT_CMX) = max(valobs(i, IPNT_UCX), sqrt(ucx(kk)**2 + ucy(kk)**2))
            end do
            valobs(i, IPNT_SMX) = max(smxobs(i), s1(k))

            if (model_is_3D()) then
               call getkbotktop(k, kb, kt)
               call getlayerindices(k, nlayb, nrlay)
               do kk = kb - 1, kt
                  klay = kk - kb + nlayb + 1
                  valobs(i, IPNT_ZWS + klay - 1) = zws(kk)
                  if (iturbulencemodel >= 2 .and. jahistur > 0) then
                     valobs(i, IPNT_VICWWS + klay - 1) = vicwws(kk)
                     valobs(i, IPNT_DIFWWS + klay - 1) = difwws(kk)
                  end if
                  if (use_density() .and. jahisrho > 0) then
                     if (zws(kt) - zws(kb - 1) > epshu .and. kk > kb - 1 .and. kk < kt) then
                        valobs(i, IPNT_BRUV + klay - 1) = drhodz(kk) * brunt_vaisala_coefficient
                     end if
                  end if
                  if (idensform > 0 .and. jaRichardsononoutput > 0) then
                     valobs(i, IPNT_RICHS + klay - 1) = richs(kk)
                  end if

                  if (IVAL_WS1 > 0) then
                     do j = IVAL_WS1, IVAL_WSN
                        ii = j - IVAL_WS1 + 1
                        valobs(i, IPNT_WS1 + (ii - 1) * (kmx + 1) + klay - 1) = mtd%ws(kb + klay - 2, ii)
                     end do
                  end if
                  if (IVAL_SEDDIF1 > 0) then
                     do j = IVAL_SEDDIF1, IVAL_SEDDIFN
                        ii = j - IVAL_SEDDIF1 + 1
                        valobs(i, IPNT_SEDDIF1 + (ii - 1) * (kmx + 1) + klay - 1) = mtd%seddif(ii, kb + klay - 2)
                     end do
                  end if
               end do

               if (link_id_nearest > 0) then
                  call getLbotLtop(link_id_nearest, Lb, Lt)
                  call getlayerindicesLmax(link_id_nearest, nlaybL, nrlayLx)
                  do L = Lb - 1, Lt
                     klay = L - Lb + nlaybL + 1
                     valobs(i, IPNT_ZWU + klay - 1) = min(bob(1, link_id_nearest), bob(2, link_id_nearest)) + hu(L)
                     if (iturbulencemodel >= 2 .and. jahistur > 0) then
                        valobs(i, IPNT_VICWWU + klay - 1) = vicwwu(L)
                     end if
                     if (iturbulencemodel >= 3 .and. jahistur > 0) then
                        valobs(i, IPNT_TKIN + klay - 1) = turkin1(L)
                        valobs(i, IPNT_TEPS + klay - 1) = tureps1(L)
                     end if
                     if (idensform > 0 .and. jaRichardsononoutput > 0) then
                        valobs(i, IPNT_RICH + klay - 1) = rich(L)
                     end if
                  end do
               end if
            end if

!        Rainfall
            if (jarain > 0 .and. jahisrain > 0) then
               valobs(i, IPNT_RAIN) = rain(k)
            end if

            if (allocated(air_density) .and. jahis_airdensity > 0) then
               valobs(i, IPNT_AIRDENSITY) = air_density(k)
            end if

!        Infiltration
            if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
               valobs(i, IPNT_INFILTCAP) = infiltcap(k) * 1.0e3_dp * 3600.0_dp ! m/s -> mm/hr
               if (ba(k) > 0.0_dp) then
                  valobs(i, IPNT_INFILTACT) = infilt(k) / ba(k) * 1.0e3_dp * 3600.0_dp ! m/s -> mm/hr
               else
                  valobs(i, IPNT_INFILTACT) = 0.0_dp
               end if
            end if

!        Heatflux
            if (jatem > 0 .and. jahisheatflux > 0) then
               call getlink1(k, LL)
               if (jawind > 0) then
                  valobs(i, IPNT_WIND) = sqrt(wx(LL) * wx(LL) + wy(LL) * wy(LL))
               end if

               if (jatem > 1) then ! also heat modelling involved
                  valobs(i, IPNT_TAIR) = air_temperature(k)
               end if

               if (jatem == 5 .and. allocated(relative_humidity) .and. allocated(cloudiness)) then
                  valobs(i, IPNT_RHUM) = relative_humidity(k)
                  valobs(i, IPNT_CLOU) = cloudiness(k)
               end if

               if (jatem == 5) then
                  valobs(i, IPNT_QSUN) = Qsunmap(k)
                  valobs(i, IPNT_QEVA) = Qevamap(k)
                  valobs(i, IPNT_QCON) = Qconmap(k)
                  valobs(i, IPNT_QLON) = Qlongmap(k)
                  valobs(i, IPNT_QFRE) = Qfrevamap(k)
                  valobs(i, IPNT_QFRC) = Qfrconmap(k)
               end if

               if (jatem > 1) then
                  valobs(i, IPNT_QTOT) = Qtotmap(k)
               end if
            end if
         else
            valobs(i, :) = DMISS
         end if
      end do

!  No need to copy empty layers from top anymore, they have been filled with dmiss

      if (allocated(wa)) then
         deallocate (wa)
      end if

      if (timon) call timstop(handle_extra(55))
   end subroutine fill_valobs

   !> Support routine to collect the values of the ice quantities at the observation stations
   subroutine collect_ice_values(valobs, i, k)
      use precision, only: dp
      use m_fm_icecover, only: ja_icecover, ICECOVER_NONE, fm_is_allocated_ice
      use m_fm_icecover, only: ice_s1, ice_zmin, ice_zmax, ice_area_fraction, ice_thickness, ice_pressure, ice_temperature, snow_thickness, snow_temperature
      use m_observations_data, only: IPNT_ICE_S1, IPNT_ICE_ZMIN, IPNT_ICE_ZMAX, &
                                     IPNT_ICE_AREA_FRACTION, IPNT_ICE_THICKNESS, IPNT_ICE_PRESSURE, IPNT_ICE_TEMPERATURE, &
                                     IPNT_SNOW_THICKNESS, IPNT_SNOW_TEMPERATURE

      real(kind=dp), dimension(:, :), intent(inout) :: valobs !< values at observations stations
      integer, intent(in) :: i !< index of the observation station
      integer, intent(in) :: k !< face index associated with the observation station

      if (ja_icecover == ICECOVER_NONE .or. .not. fm_is_allocated_ice()) return

      call conditional_assign(valobs, i, IPNT_ICE_S1, ice_s1, k)
      call conditional_assign(valobs, i, IPNT_ICE_ZMIN, ice_zmin, k)
      call conditional_assign(valobs, i, IPNT_ICE_ZMAX, ice_zmax, k)
      call conditional_assign(valobs, i, IPNT_ICE_AREA_FRACTION, ice_area_fraction, k)
      call conditional_assign(valobs, i, IPNT_ICE_THICKNESS, ice_thickness, k)
      call conditional_assign(valobs, i, IPNT_ICE_PRESSURE, ice_pressure, k)
      call conditional_assign(valobs, i, IPNT_ICE_TEMPERATURE, ice_temperature, k)
      call conditional_assign(valobs, i, IPNT_SNOW_THICKNESS, snow_thickness, k)
      call conditional_assign(valobs, i, IPNT_SNOW_TEMPERATURE, snow_temperature, k)
   end subroutine collect_ice_values

   !> Support routine to conditionally assign values to the target variable
   subroutine conditional_assign(valobs, i, ipnt, array, k)
      use precision, only: dp, fp

      real(kind=dp), dimension(:, :), intent(inout) :: valobs !< values at observations stations
      integer, intent(in) :: i !< index of the observation station
      integer, intent(in) :: ipnt !< pointer index in valobs
      real(kind=fp), dimension(:), intent(in) :: array !< array from which to assign value
      integer, intent(in) :: k !< face index associated with the observation station

      if (ipnt > 0) then
         valobs(i, ipnt) = real(array(k), dp)
      end if
   end subroutine conditional_assign

end module m_fill_valobs
