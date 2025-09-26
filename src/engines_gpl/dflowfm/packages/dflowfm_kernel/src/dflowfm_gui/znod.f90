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

module m_znod
   use m_getfetch, only: getfetch

   implicit none

contains

   real(kind=dp) function znod(kk) ! get various values at flow nodes
      use precision, only: dp
      use m_getktoplot
      use m_flow
      use m_flowgeom
      use m_flowtimes ! for volerr
      use m_sediment
      use m_fm_erosed, only: ucxq_mor, ucyq_mor
      use m_xbeach_data
      use m_transportdata
      use m_observations_data
      use m_flowparameters, only: ispirparopt
      use m_wind, only: air_pressure_available, air_pressure, rain, relative_humidity, tbed, air_temperature, cloudiness, solar_radiation
      use unstruc_display_data, only: grwhydopt
      use m_drawthis
      use m_get_equilibrium_transport_rates
      use m_get_tau
      use m_nudge, only: nudge_rate
      use m_waves, only: waveparopt, hwav, rlabda, twav, uorb, fwav_mag, ust_mag, sxwav, numoptwav, sywav, sbxwav, sbywav, ustx_cc, usty_cc, phiwav, fetch
      use m_waveconst

      implicit none

      integer :: kk, k, nodval, L
      real(kind=dp) :: seq(mxgr), wse(mxgr), hsk, dum, czc, taucurc, ustw2, U10, FetchL, FetchD, rkk, shs
      integer :: jawaveswartdelwaq_local
      real(kind=dp), external :: sinhsafei

      nodval = ndraw(28)
      znod = DMISS
      if (kk < 1) then
         return
      end if

      k = kk
      if (kmx > 0) then
         if (kplotordepthaveraged == 1) then
            call getktoplot(kk, k)
            if (k < 0) return
         end if
      end if

      !if ( jampi.eq.1 ) then
      !   if ( idomain(k).ne.my_rank ) return
      !end if

      if (nodval == 2) then
         znod = s1(kk)
      else if (nodval == 3) then
         znod = bl(kk)
      else if (nodval == 4) then
         znod = ba(kk)
      else if (nodval == 5) then
         znod = a1(kk)
      else if (nodval == 6) then
         znod = vol1(k)
      else if (nodval == 7) then
         znod = s1(kk) - bl(kk)
      else if (nodval == 8) then
         znod = sqrt(ucx(k) * ucx(k) + ucy(k) * ucy(k))
         if (stm_included) then
            znod = sqrt(ucxq_mor(k) * ucxq_mor(k) + ucyq_mor(k) * ucyq_mor(k))
         end if
      else if (nodval == 9) then
         znod = ucx(k)
         if (stm_included) then
            znod = ucxq_mor(k)
         end if
      else if (nodval == 10) then
         znod = ucy(k)
         if (stm_included) then
            znod = ucyq_mor(k)
         end if
      else if (nodval == 11) then
         if (jasal > 0) znod = constituents(isalt, k)
      else if (nodval == 12) then
         if (jatem > 0) then
            if (jafahrenheit == 0) then
               znod = constituents(itemp, k)
            else
               znod = 32.0_dp + (9.0_dp / 5.0_dp) * constituents(itemp, k)
            end if
         end if
      else if (nodval == 13) then
         if (jased > 0 .and. .not. stm_included) then
            znod = sed(jgrtek, k)
         else if (jagrw > 0) then
            znod = sgrw1(kk)
         end if
      else if (nodval == 14) then
         if (hs(kk) > 0) then
            znod = sqrt(ucx(k) * ucx(k) + ucy(k) * ucy(k)) / sqrt(ag * hs(kk)) ! Froude
         else
            znod = 0.0_dp
         end if
      else if (nodval == 15) then
         znod = kk
      else if (nodval == 16) then
         znod = nd(kk)%lnx
      else if (nodval == 17) then
         znod = kcs(kk) !  voldhu(kk) - vol1(kk)
      else if (nodval == 18) then
         znod = squ(k)
      else if (nodval == 19) then
         znod = sqi(k)
      else if (nodval == 20) then
         znod = sqi(k) - squ(k)
      else if (nodval == 21) then
         znod = qw(k) / a1(kk)
      else if (nodval == 22) then
         if (jased > 0) then
            call getequilibriumtransportrates(kk, seq, wse, mxgr, hsk)
            znod = seq(jgrtek)
         end if
      else if (nodval == 23) then
         znod = qin(k)
      else if (nodval == 24) then
         if (mxgr > 1 .and. jaceneqtr == 1) znod = grainlay(jgrtek, kk)
      else if (nodval == 25 .and. kmx > 0) then
         znod = ktop(kk) - kbot(kk) + 1
      else if (nodval == 26) then
         if (squ(k) > 0.0_dp .and. vol1(k) > 0.0_dp) then
            znod = vol1(k) / squ(k)
         end if
      else if (nodval == 27) then
         if (kmx > 1) znod = vicwws(k)
         ! 28 = substi/cg
      else if (nodval == 29) then
         if (allocated(tidep)) then
            if (ubound(tidep, 1) == 2) then
               znod = tidep(2, kk)
            else
               znod = tidep(1, kk)
            end if
         end if
!     znod = plotlin(kk)
      else if (nodval == 30) then
         znod = dt_max
         do k = kbot(kk), ktop(kk)
            znod = min(znod, vol1(k) / max(squ(k), eps10))
         end do
      else if (nodval == 31) then
         if (air_pressure_available) znod = air_pressure(kk)
      else if (nodval == 32) then
         if (numlimdt(kk) > 0) znod = numlimdt(kk)
      else if (nodval == 33) then
         ZNOD = (ucx(k) * ucx(k) + ucy(k) * ucy(k)) / (2.0_dp * ag)
         znod = u1(min(k, lnx)) * u1(min(k, lnx)) / (2.0_dp * ag)
         znod = znod + s1(kk)

         plotlin(kk) = znod

      else if (nodval == 34) then
         znod = volerror(k)
      else if (nodval == 35) then

         znod = rho(k) ! sam0(k) !  kktop(kk) - kbot(kk) + 1

      else if (nodval == 36) then

         znod = dt_max
         do k = kbot(kk), ktop(kk)
            if (squ(k) > eps10) then
               znod = min(znod, cflmx * vol1(k) / squ(k))
            end if
         end do

      else if (nodval == 37) then

         if (Soiltempthick > 0 .and. jatem > 0) then
            znod = tbed(kk)
         else
            znod = same(k)
         end if

      else if (nodval == 38) then

         znod = zws(k) - zws(k - 1)

      else if (nodval == 39) then

         if (flowWithoutWaves) then
            jawaveswartdelwaq_local = WAVE_WAQ_SHEAR_STRESS_HYD
         else
            jawaveswartdelwaq_local = jawaveswartdelwaq
         end if
         call get_tau(kk, znod, czc, jawaveswartdelwaq_local)

      else if (nodval == 40) then

         znod = rain(kk)

      else if (nodval == 41 .and. jatem > 0) then
         znod = relative_humidity(kk)
      else if (nodval == 42 .and. jatem > 0) then
         znod = air_temperature(kk)
      else if (nodval == 43 .and. jatem > 0) then
         znod = cloudiness(kk)
      else if (nodval == 44 .and. jatem > 0 .and. allocated(solar_radiation)) then
         znod = solar_radiation(kk)
      else if (nodval == 45 .and. NUMCONST > 0) then
         if (iconst_cur > 0 .and. iconst_cur <= NUMCONST) then
            znod = constituents(iconst_cur, k)
         end if
      else if (nodval == 46) then
         if (allocated(FrcInternalTides2D)) then
            znod = FrcInternalTides2D(kk)
         else
            znod = turkinws(k)
         end if
      else if (nodval == 47 .and. (jagrw > 0 .or. jadhyd > 0)) then
         select case (grwhydopt)
         case (1) ! Ground water pressure
            if (jagrw > 0) then
               if (infiltrationmodel == 1) then
                  znod = sgrw1(k)
               else
                  znod = pgrw(kk)
               end if
            end if
         case (4) ! Infiltration capacity
            if (infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) then
               znod = infiltcap(kk) * 1.0e3_dp * 3600.0_dp ! m/s -> mm/hr
            end if
         case (6) ! Interception layer thickness
            if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
               znod = InterceptThickness(kk)
            end if
         case (7) ! Interception layer water depth       (m)
            if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
               znod = InterceptHs(kk)
            end if
         case (8) ! Potential evaporation            (mm/hr)
            if (jadhyd == 1) then
               znod = PotEvap(kk) * 1.0e3_dp * 3600.0_dp ! m/s -> mm/hr
            end if
         case (9) ! Actual evaporation open water    (mm/hr)
            if (jadhyd == 1) then
               znod = ActEvap(kk) * 1.0e3_dp * 3600.0_dp ! m/s -> mm/hr
            end if
         end select

      else if (nodval == 48) then
         if (nonlin >= 2) then
            znod = a1m(kk)
         else if (jaPure1D > 0) then ! visualise
            znod = uc1d(kk)
         else if (kmx > 0) then
            znod = kmxn(kk)
         end if
      else if (nodval == 49) then
         if (nshiptxy > 0) then
            znod = zsp(kk)
         end if
      else if (nodval == 50) then
         if (janudge > 0) then
            znod = 0.0_dp
            if (nudge_rate(kk) > 0.0_dp) then
               znod = 1.0_dp / nudge_rate(kk)
            end if
         else if (nshiptxy > 0) then
            znod = s1(kk) + zsp(kk)
         end if

      else if (nodval == numoptwav .and. jawave > NO_WAVES .and. .not. flowWithoutWaves) then
         if (jawave == WAVE_FETCH_HURDLE .or. jawave == WAVE_FETCH_YOUNG) then
            select case (waveparopt)
            case (1)
               znod = Hwav(kk)
            case (2)
               znod = Rlabda(kk)
            case (3)
               znod = Twav(kk)
            case (4)
               znod = Uorb(kk)
            case (5)
               call get_tau(kk, taucurc, czc, ustw2, jawaveswartdelwaq)
               znod = sqrt(ustw2) !ustw
            case (6)
               call get_tau(kk, taucurc, czc, ustw2, jawaveswartdelwaq)
               znod = sqrt(taucurc / rhomean) !ustw+c
            case (7)
               call get_tau(kk, taucurc, czc, ustw2, jawaveswartdelwaq)
               znod = taucurc ! taus to Delwaq
            case (8)
               znod = dmiss ! Ustokes
            case (9)
               call getfetch(kk, U10, FetchL, FetchD)
               znod = FetchL
            case (10)
               call getfetch(kk, U10, FetchL, FetchD)
               znod = FetchD
            end select

         else
            select case (waveparopt)
            case (1)
               znod = Hwav(kk)
            case (2)
               if (jawave /= WAVE_SURFBEAT) then
                  !   znod = Twav(kk)
                  !elseif (windmodel.eq.0) then
                  znod = Trep
                  !else
                  !   znod = tt1(itheta_view,kk)
               end if
            case (3)
               znod = taus(kk)
            case (4)
               znod = fwav_mag(k)
            case (5)
               znod = ust_mag(k)
            case (6)
               if (twav(kk) > 0.0_dp) then
                  call wavenr(hs(kk), twav(kk), rkk, ag)
                  znod = rkk
               end if
            case (7)
               if (twav(kk) > 0.0_dp) then
                  call wavenr(hs(kk), twav(kk), rkk, ag)
                  shs = sinhsafei(rkk * hs(kk))
                  znod = shs
               else
                  znod = dmiss
               end if
            case (8)
               znod = hypot(sxwav(kk), sywav(kk))
            case (9)
               znod = hypot(sbxwav(kk), sbywav(kk))
            case (10)
               if (jawave == WAVE_SURFBEAT) then
                  znod = ustx_cc(kk)
               end if
            case (11)
               if (jawave == WAVE_SURFBEAT) then
                  znod = usty_cc(kk)
               end if
            case (12)
               znod = ee1(itheta_view, kk)
            case (13)
               znod = rr(itheta_view, kk)
            case (14)
               if (jawave == WAVE_SURFBEAT) then
                  znod = uorb(kk)
               end if
            case (15)
               if (jawave == WAVE_SURFBEAT) then
                  znod = D(kk)
               end if
            case (16)
               if (jawave == WAVE_SURFBEAT) then
                  znod = DR(kk)
               end if
            case (17)
               if (jawave == WAVE_SURFBEAT) then
                  znod = R(kk)
               end if
            case (18)
               if (jawave == WAVE_SURFBEAT) then
                  znod = Sxx(kk)
               end if
            case (19)
               if (jawave == WAVE_SURFBEAT) then
                  znod = Syy(kk)
               end if
            case (20)
               if (jawave == WAVE_SURFBEAT) then
                  znod = Sxy(kk)
               end if
            case (21)
               if (jawave == WAVE_SURFBEAT) then
                  znod = kwav(kk)
               end if
            case (22)
               znod = mod(270.0_dp - phiwav(kk), 360.0_dp)

            case (23)
               if (jawave == WAVE_SURFBEAT) then
                  znod = dhsdx(kk)
               end if
            case (24)
               if (jawave == WAVE_SURFBEAT) then
                  znod = dhsdy(kk)
               end if
            case (25)
               znod = dmiss
               !if (jawave .eq. 4) then
               !   if (jawind>0 .and. jawsource>0) then
               !      znod = wsorE(itheta_view, kk)
               !   endif
               !end if
            case (26)
               if (jawave == WAVE_SURFBEAT) then
                  znod = sigt(itheta_view, kk)
               end if
            case (27)
               if (jawave == WAVE_SURFBEAT) then
                  !if (windmodel.eq.0) then
                  znod = cgwav(kk)
                  !else
                  !   znod = cgwavt(itheta_view,kk)
                  !end if
               end if
            case (28)
               if (jawave == WAVE_FETCH_HURDLE .or. jawave == WAVE_FETCH_YOUNG) then
                  znod = fetch(1, kk)
               end if
            case (29)
               znod = dmiss
               !if (jawave.eq.4) then
               !   znod = dtheta * egradcg(itheta_view,kk)
               !endif
            case (30)
               znod = dmiss
               !if (jawave.eq.4) then
               !   znod = SwT(kk)
               !endif
            case (31)
               znod = dmiss
               !if(jawave.eq.4) then
               !   znod = SwE(kk)
               !endif
            case (32)
               if (jawave == WAVE_SURFBEAT) then
                  znod = horadvec(itheta_view, kk)
               end if
            case (33)
               znod = dmiss
               !if(jawave.eq.4) then
               !   znod = horadvec2(itheta_view,kk)
               !endif
            case (34)
               znod = dmiss
               !if(jawave.eq.4) then
               !   znod = ma(itheta_view,kk)
               !endif
            end select

         end if

      else if (nodval == numoptsf .and. jasecflow > 0) then
         select case (ispirparopt)
         case (1)
            if (jasecflow > 0) then
               znod = spircrv(kk)
            else
               znod = bz(kk)
            end if
         case (2)
            znod = spirint(kk)
         case (3)
            znod = sqrt(spirfx(kk) * spirfx(kk) + spirfy(kk) * spirfy(kk))
         end select
      else if (nodval == numoptsed .and. stm_included) then
         select case (sedparopt)
         case (1)
            znod = mtd%blchg(kk)
         case (2)
            dum = 0.0_dp
            do l = 1, stmpar%lsedsus
               dum = dum + sedtra%sourse(kk, l)
            end do
            znod = dum
         case (3)
            dum = 0.0_dp
            do l = 1, stmpar%lsedsus
               dum = dum + sedtra%sinkse(kk, l)
            end do
            znod = dum
         case (4)
            dum = 0.0_dp
            do l = 1, stmpar%lsedtot
               dum = dum + hypot(sedtra%sxtot(kk, l), sedtra%sytot(kk, l))
            end do
            znod = dum
         end select
      end if
   end function znod

end module m_znod
