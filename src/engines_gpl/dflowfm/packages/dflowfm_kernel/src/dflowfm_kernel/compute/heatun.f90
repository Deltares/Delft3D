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

module m_heatun
   use m_qsun_nominal, only: qsun_nominal

   implicit none

contains

   subroutine heatun(n, timhr, qsno)
      use precision, only: dp, comparereal, fp
      use physicalconsts, only: stf
      use m_physcoef, only: ag, rhomean, backgroundsalinity, dalton, epshstem, stanton, sfr, Soiltempthick, &
                            BACKGROUND_AIR_PRESSURE, BACKGROUND_HUMIDITY, BACKGROUND_CLOUDINESS, Secchidepth2, surftempsmofac, &
                            jadelvappos, zab
      use m_heatfluxes, only: em, albedo, cpa, tkelvn, jaSecchisp, Secchisp, jamapheatflux, rcpi, &
                              fwind, Qtotmap, Qsunmap, Qevamap, Qconmap, Qlongmap, Qfrevamap, Qfrconmap, Qsunav, Qlongav, Qconav, &
                              Qevaav, Qfrconav, Qfrevaav
      use m_flow, only: kmx, hs, solar_radiation_factor, zws, ucx, ucy, ktop
      use m_flowparameters, only: jahisheatflux, jatem, ja_solar_radiation_factor
      use m_missing, only: dmiss
      use m_flowgeom, only: ba, nd, ln, yz, xz
      use m_sferic, only: jsferic
      use m_flowtimes, only: dts
      use m_transport, only: constituents, itemp, isalt
      use m_fm_icecover, only: ja_icecover, ice_af, ice_albedo, ice_h, ice_t, snow_albedo, snow_h, snow_t, &
                               qh_air2ice, qh_ice2wat, ICECOVER_NONE, ICECOVER_SEMTNER, preprocess_icecover
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_link1, only: getlink1
      use m_wind, only: japatm, jaevap, longwave_available, relativewind, tair, wx, wy, rhum, clou, patm, heatsrc0, qrad, &
                        solar_radiation, solrad_available, tbed, rhoair, longwave, evap, cdwcof, airdensity, ja_airdensity, ja_computed_airdensity

      real(kind=dp), intent(in) :: timhr, qsno
      integer, intent(in) :: n

      integer :: k, kb, kt, k2, L, LL, j, j2, ncols
      real(kind=dp) :: qsn, qsun, qsnom, presn, tairn, twatn, twatK, rhumn, cloun, windn, air_density
      real(kind=dp) :: ce, ch, qwmx, qahu, tl, Qcon, Qeva, Qlong, pvtamx, pvtwmx, pvtahu, delvap
      real(kind=dp) :: dexp, zlo, zup, explo, expup, ratio, rcpiba, qheat, atot

      real(kind=dp) :: w(20), Qfree, b, gred, wfree, Qfrcon, Qfreva, rhoa0, rhoa10

      real(kind=dp) :: pr2 = 0.49_dp, xnuair = 16.0e-06_dp, cfree = 0.14_dp

      real(kind=dp) :: rdry = 287.05e-02_dp, rvap = 461.495e-02_dp, evafac = 1.0_dp

      real(kind=dp) :: hlc, arn, wxL, wyL, bak2, twatb

      real(kind=dp) :: qsunsoil, qwatsoil, watsoiltransfer, rdtsdz

      real(kind=dp) :: afrac !< area fraction of ice cover (-)
      real(kind=dp) :: Qlong_ice !< coefficient for long wave radiation of ice (J m-2 s-1 K-4)
      real(kind=dp) :: tsurf !< surface temperature ... temperature of water, ice or snow depending on their presence (degC)
      real(kind=dp) :: salinity !< water salinity (ppt)

      real(kind=dp), parameter :: MIN_THICK = 0.001_fp !< threshold thickness for ice/snow to overrule the underlying layer (m)

      if (ja_icecover /= ICECOVER_NONE) then
         afrac = 1.0_dp - ice_af(n)
      else
         afrac = 1.0_dp
      end if

      presn = 1.0e-2_dp * BACKGROUND_AIR_PRESSURE ! Air pressure (mbar)
      rhumn = 1.0e-2_dp * BACKGROUND_HUMIDITY ! ( )
      cloun = 1.0e-2_dp * BACKGROUND_CLOUDINESS ! ( )
      ce = Dalton ! Dalton  number = 1.50e-3 (Gill, 1982)           evaporative flux
      ch = Stanton ! Stanton number = 1.45e-3 (Friehe&Schmitt, 1976) convective heat flux
      qsun = 0.0_dp
      qsnom = qsno
      call getlink1(n, L)
      if (relativewind > 0.0_dp) then
         wxL = wx(L) - relativewind * ucx(ktop(n))
         wyL = wy(L) - relativewind * ucy(ktop(n))
      else
         wxL = wx(L)
         wyL = wy(L)
      end if
      windn = sqrt(wxL * wxL + wyL * wyL)

      call getkbotktop(n, kb, kt)

      twatn = constituents(itemp, kt)
      if (surftempsmofac > 0.0_dp) then
         arn = ba(n)
         twatn = twatn * arn
         do LL = 1, nd(n)%lnx
            L = abs(nd(n)%ln(LL))
            k2 = ln(1, L) + ln(2, L) - n
            if (hs(k2) > epshstem) then
               bak2 = surftempsmofac * ba(k2)
               twatn = twatn + constituents(itemp, ktop(k2)) * bak2
               arn = arn + bak2
            end if
         end do
         if (arn > 0.0_dp) then
            twatn = twatn / arn
         end if
      end if

      tairn = tair(n)

      if (jatem == 3) then ! excess

         hlc = 4.48_dp + 0.049_dp * twatn + fwind * (3.5_dp + 2.05_dp * windn) * (1.12_dp + 0.018_dp * twatn + 0.00158_dp * twatn**2)

         qheat = -hlc * (twatn - tairn)
         rcpiba = rcpi * ba(n)
         heatsrc0(kt) = heatsrc0(kt) + qheat * rcpiba * afrac ! fill heat source array

         if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! todo, only at mapintervals
            Qtotmap(n) = qheat
         end if

      else if (jatem == 5) then

         ! Set TSURF either to TWATN or to ice_t(n) or to snow_t(n) and change albedo parameter in case of ice and/or snow
         !
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (snow_h(n) > MIN_THICK) then
               !
               ! ice and snow
               albedo = snow_albedo
               tsurf = snow_t(n)
            elseif (ice_h(n) > MIN_THICK) then
               !
               ! ice but no snow
               albedo = ice_albedo
               tsurf = ice_t(n)
            else
               !
               ! no ice and no snow, but ice_modelling switched on
               tsurf = twatn
            end if
         else
            !
            ! ice_modelling switched off
            tsurf = twatn
         end if

         rhumn = min(1.0_dp, max(0.0_dp, 1.0e-2_dp * rhum(n)))
         cloun = min(1.0_dp, max(0.0_dp, 1.0e-2_dp * clou(n)))

         if (japatm > 0) then
            presn = 1d-2 * patm(n)
         end if

         ! Solar radiation restricted by presence of clouds and reflection of water surface (albedo)
         if (solrad_available) then
            qsun = qrad(n) * (1.0_dp - albedo)
         else ! Calculate solar radiation from cloud coverage specified in file
            if (jsferic == 1) then
               call qsun_nominal(xz(n), yz(n), timhr, qsnom)
            end if
            if (qsnom > 0.0_dp) then
               qsun = qsnom * (1.0_dp - 0.40_dp * cloun - 0.38_dp * cloun * cloun) * (1.0_dp - albedo)
            else
               qsun = 0.0_dp
            end if
         end if

         if (ja_solar_radiation_factor > 0) then
            if (comparereal(solar_radiation_factor(n), dmiss) /= 0) then
               qsun = qsun * solar_radiation_factor(n)
            end if
         end if
         solar_radiation(n) = qsun ! solar_radiation is passed on to fm_wq_processes

         rcpiba = rcpi * ba(n)
         qsn = qsun * rcpiba

         if (qsn > 0.0_dp) then

            if (kmx > 0) then ! distribute incoming radiation over water column
               ! zab  = (min(0.5_dp*hs(n) ,Secchidepth)) / 1.7_dp

               if (Secchidepth2 > 0.0_dp) then
                  j2 = 2
               else
                  j2 = 1
               end if

               do j = j2, 1, -1

                  if (j == 1 .and. jaSecchisp > 0) then
                     zab(1) = Secchisp(n) / 1.7_dp
                  end if

                  zlo = 0.0_dp
                  explo = 1.0_dp

                  do k = kt, kb, -1
                     zup = zlo
                     expup = explo
                     zlo = zws(kt) - zws(k - 1)
                     ratio = zlo / zab(j)
                     if (ratio > 4.0_dp) then !  .or. k.eq.kb) then
                        explo = 0.0_dp
                     else
                        explo = exp(-ratio)
                     end if
                     dexp = expup - explo
                     if (dexp > 0.0_dp) then
                        heatsrc0(k) = heatsrc0(k) + sfr(j) * qsn * dexp * afrac
                     else
                        exit
                     end if
                  end do
               end do

            else
               heatsrc0(n) = heatsrc0(n) + qsn * afrac
            end if

         end if

         if (kmx > 0 .and. Soiltempthick > 0.0_dp) then
            if (qsn > 0.0_dp) then
               qsunsoil = qsun * explo
            else
               qsunsoil = 0.0_dp
            end if
            watsoiltransfer = 1.0_dp / (0.5_dp * Soiltempthick) ! thermalcond sand = 0.15 -> 4 for dry -> saturated, [W/mK]
            twatb = constituents(itemp, kb)
            qwatsoil = watsoiltransfer * (twatb - tbed(n))
            heatsrc0(kb) = heatsrc0(kb) - rcpiba * qwatsoil * afrac
            rdtsdz = rcpi * dts / Soiltempthick
            tbed(n) = (tbed(n) + rdtsdz * (qsunsoil + watsoiltransfer * twatb)) / (1.0_dp + watsoiltransfer * rdtsdz)
         end if

         ! PVTWMX = PVapour at TWater and MaX relative humidity
         ! PVTAMX = PVapour at TAir   and MaX relative humidity
         pvtamx = saturation_pressure(tairn) ! saturation pressure of water vapour in air remote (ewl)
         pvtwmx = saturation_pressure(tsurf) ! and near water surface (ew); eq.(A.12):

         pvtahu = rhumn * pvtamx ! vapour pressure in air remote (eal)

         qwmx = (0.62_dp * pvtwmx) / (presn - 0.38_dp * pvtwmx) ! specific humidity of air remote and
         qahu = (0.62_dp * pvtahu) / (presn - 0.38_dp * pvtahu) ! saturated air near water surface; eq.(A.9)+(A.10):

         tl = 2.5e6_dp - 2.3e3_dp * tsurf ! latent heat tl; eq.(A.19.b): (J/kg)

         if (Stanton < 0.0_dp) then ! if specified negative, use windspeed dependent Cd coeff
            ch = abs(Stanton) * cdwcof(L)
         end if
         if (Dalton < 0.0_dp) then ! if specified negative, use windspeed dependent Cd coeff
            ce = abs(Dalton) * cdwcof(L)
         end if

         delvap = qwmx - qahu ! D3D, both positive and negative evaporation, cannot be correct
         if (jadelvappos == 1) then
            delvap = max(0.0_dp, delvap) ! DPM, DFM This must be positive, otherwise heat is pumped into water
         end if ! causing air to cool down below prescribed temperature, immedia. and

         ! change parameters for ice modelling
         !
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (ice_h(n) > MIN_THICK) then
               ! in case of ice (and snow) overrule the Stanton number (convective heat flux)
               ch = 0.00232_dp
            end if
         end if

         if (ja_airdensity > 0 .or. ja_computed_airdensity > 0) then
            air_density = airdensity(n)
         else
            air_density = rhoair
         end if

         Qeva = -ce * air_density * windn * delvap * tl ! heat loss of water by evaporation eq.(A.19.a); Dalton number is ce:

         Qcon = -ch * air_density * cpa * windn * (tsurf - tairn) ! heat loss of water by convection eq.(A.23); Stanton number is ch:

         twatK = tsurf + tkelvn
         if (longwave_available) then
            Qlong = em * (longwave(n) - stf * (twatK**4)) ! difference between prescribed long wave downward flux and calculated upward flux
         else
            Qlong = -em * stf * (twatK**4) * (0.39_dp - 0.05_dp * sqrt(pvtahu)) ! heat loss by effective infrared back radiation hl, restricted by
            Qlong = Qlong * (1.0_dp - 0.6_dp * cloun**2) !  presence of clouds and water vapour in air; eq.(A.22):
         end if

         Qfree = 0.0_dp; Qfrcon = 0.0_dp; Qfreva = 0.0_dp ! Contribution by free convection:
         rhoa0 = ((presn - pvtwmx) / rdry + pvtwmx / rvap) / (Tsurf + Tkelvn)
         rhoa10 = ((presn - pvtahu) / rdry + pvtahu / rvap) / (Tairn + Tkelvn)
         gred = 2.0_dp * ag * (rhoa10 - rhoa0) / (rhoa0 + rhoa10)
         if (gred > 0.0_dp) then ! Ri= (gred/DZ)/ (du/dz)2, Ri>0.25 stable
            wfree = gred * xnuair / pr2
            wfree = cfree * wfree**0.33333333_dp
            Qfrcon = min(0.0_dp, -air_density * cpa * wfree * (tsurf - tairn) * evafac) ! Free convective sensible heat loss:
            Qfreva = min(0.0_dp, -wfree * (qwmx - qahu) * tl * evafac * (rhoa0 + rhoa10) * 0.5_dp) ! Free convective latent/evaporation heat loss:
            Qfree = Qfrcon + Qfreva
         end if

         qheat = Qeva + Qcon + Qlong + Qfree ! net heat flux [W/m^2] into water, solar radiation excluded:
         if (jaevap > 0) then
            evap(n) = (Qeva + Qfreva) / (tL * rhomean) * afrac ! (J/sm2)/(J/kg)/kg/m3) = (m/s)
         end if

         heatsrc0(kt) = heatsrc0(kt) + qheat * rcpiba * afrac ! fill heat source array

         ! In case of ice preprocessing of ice quantities
         !
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (ice_h(n) > MIN_THICK .or. (twatn < 0.1_fp .and. tair(n) < 0.0_fp)) then
               !
               ! Compute Qlong_ice (NB. Delft3D-FLOW definition is used, with opposite sign, so that
               ! algorithm in preprocess_icecover remains identical to the one for Delft3D-FLOW
               Qlong_ice = em * stf * (0.39_dp - 0.05_dp * sqrt(pvtahu)) * (1.0_dp - 0.6_dp * cloun**2)
               !
               qh_air2ice(n) = qsun + qheat
               !
               if (isalt > 0) then
                  if (kmx == 0) then
                     salinity = constituents(isalt, n)
                  else
                     salinity = constituents(isalt, kt)
                  end if
               else
                  salinity = backgroundsalinity
               end if
               call preprocess_icecover(n, Qlong_ice, twatn, salinity, windn)
            end if
            !
            if (ice_h(n) > MIN_THICK) then
               !
               ! recompute heatsrc0 because of presence of ice
               !
               if (kmx > 0) then
                  heatsrc0(kt) = qh_ice2wat(n) * afrac
               else
                  heatsrc0(n) = qh_ice2wat(n) * afrac
               end if
            end if
         end if

         if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! todo, only at mapintervals
            Qsunmap(n) = Qsun
            Qevamap(n) = Qeva
            Qconmap(n) = Qcon
            Qlongmap(n) = Qlong
            Qfrevamap(n) = Qfreva
            Qfrconmap(n) = Qfrcon
            Qtotmap(n) = Qsun + qheat
         end if

         !if (ti_xls > 0) then

         Atot = 0.0_dp ! these 2 lines outside loop
         w = 0.0_dp ! array of spatially averaged output

         b = ba(n) ! Spatially averaged time series output :
         atot = atot + b ! Total area
         w(1) = timhr / 24.0_dp ! Time in days
         w(2) = w(2) + b * tairn ! tair
         w(3) = w(3) + b * constituents(itemp, kt) ! Twatn, SST
         if (soiltempthick > 0.0_dp) then
            w(4) = w(4) + b * tbed(n) ! tbed
         end if
         w(5) = w(5) + b * (qsun + qheat) ! qtot
         w(6) = w(6) + b * Qsun ! Qsun
         w(7) = w(7) + b * Qlong ! QLw
         w(8) = w(8) + b * Qcon ! Qcon
         w(9) = w(9) + b * Qeva ! Qeva
         w(10) = w(10) + b * Qfrcon ! Qfreecon
         w(11) = w(11) + b * Qfreva ! Qfree
         w(12) = w(12) + b * windn ! wind
         w(13) = w(13) + b * rhumn ! rhum
         w(14) = w(14) + b * cloun ! clou
         w(15) = w(15) + b * presn ! pres

         ncols = 15
         if (Atot > 0.0_dp) then
            w(2:ncols) = w(2:ncols) / Atot
         end if
         Qsunav = w(6); Qlongav = w(7); Qconav = w(8); Qevaav = w(9); Qfrconav = w(10); Qfrevaav = w(11)
      end if

   contains

      !> function to compute saturation pressure of water vapour at a specified temperature
      real(kind=dp) function saturation_pressure(temp)
         use precision, only: dp
         real(kind=dp) :: temp !< temperature (degC)

         saturation_pressure = 10.0_dp**((0.7859_dp + 0.03477_dp * temp) / (1.0_dp + 0.00412_dp * temp))
         ! 6.1121_dp*exp( (18.678_dp - (temp/234.5_dp))*(temp/(257.14_dp+temp) ) )  ! Buck
      end function saturation_pressure

   end subroutine heatun

end module m_heatun
