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

module m_heatun

   implicit none

contains

   subroutine heatun(n, time_in_hours, nominal_solar_radiation)
      use precision, only: dp, comparereal, fp
      use physicalconsts, only: stf, celsius_to_kelvin, kelvin_to_celsius
      use m_physcoef, only: ag, rhomean, backgroundsalinity, backgroundwatertemperature, dalton, epshstem, stanton, sfr, &
                            soiltempthick, BACKGROUND_AIR_PRESSURE, BACKGROUND_HUMIDITY, BACKGROUND_CLOUDINESS, secchidepth2, surftempsmofac, &
                            jadelvappos, zab
      use m_heatfluxes, only: em, albedo, cpa, jaSecchisp, Secchisp, jamapheatflux, rcpi, fwind, qtotmap, qsunmap, qevamap, &
                              qconmap, qlongmap, qfrevamap, qfrconmap, qsunav, qlongav, qconav, qevaav, qfrconav, qfrevaav
      use m_flow, only: kmx, hs, solar_radiation_factor, zws, ucx, ucy, ktop
      use m_flowparameters, only: jahisheatflux, temperature_model, TEMPERATURE_MODEL_EXCESS, TEMPERATURE_MODEL_COMPOSITE, &
                                  ja_solar_radiation_factor
      use m_missing, only: dmiss
      use m_flowgeom, only: ba, nd, ln, yz, xz
      use m_sferic, only: jsferic
      use m_flowtimes, only: dts
      use m_transport, only: constituents, itemp, isalt
      use m_fm_icecover, only: ja_icecover, ice_area_fraction, ice_albedo, ice_thickness, ice_temperature, snow_albedo, &
                               snow_thickness, snow_temperature, qh_air2ice, qh_ice2wat, ICECOVER_NONE, ICECOVER_SEMTNER, preprocess_icecover
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_link1, only: getlink1
      use m_wind, only: air_pressure_available, jaevap, long_wave_radiation_available, relativewind, air_temperature, wx, wy, &
                        relative_humidity, cloudiness, air_pressure, heatsrc0, solar_radiation, solar_radiation_available, net_solar_radiation, &
                        net_solar_radiation_available, tbed, rhoair, long_wave_radiation, evap, cdwcof, air_density, ja_airdensity, &
                        ja_computed_airdensity
      use m_qsun_nominal, only: calculate_nominal_solar_radiation

      integer, intent(in) :: n
      real(kind=dp), intent(in) :: time_in_hours !< Current model time in hours
      real(kind=dp), intent(in) :: nominal_solar_radiation

      integer :: cell_index_3D, k_bot, k_top, k2, L, LL, j, j2, ncols
      real(kind=dp) :: solar_radiation_flux, net_solar_radiation_in_cell, nominal_solar_radiation_in_cell, air_pressure_in_cell, air_temperature_in_cell, &
                       water_temperature_in_cell, water_surface_temperature_kelvin, relative_humidity_in_cell, cloudiness_in_cell, wind_speed_in_cell, air_density_in_cell
      real(kind=dp) :: evaporative_heat_flux_coefficient, convective_heat_flux_coefficient, specific_humidity_surface_saturation, specific_humidity_air_surface, &
                       latent_heat_vaporization, convective_heat_flux, evaporative_heat_flux, longwave_radiation_flux, saturation_vapor_pressure_at_air_temperature, &
                       saturation_vapor_pressure_at_surface_temperature, vapor_pressure_air_humidity, vapor_pressure_difference
      real(kind=dp) :: dexp, zlo, zup, explo, expup, ratio, heat_capacity_water_cell_area, total_heat_flux, total_area
      real(kind=dp) :: weighted_sums(20), free_convection_heat_flux, cell_area_weight, buoyancy_parameter, free_convection_velocity, free_convective_sensible_heat_flux, &
                       free_convective_latent_heat_flux, air_density_surface, air_density_10m
      real(kind=dp) :: pr2 = 0.49_dp, xnuair = 16.0e-06_dp, cfree = 0.14_dp
      real(kind=dp) :: rdry = 287.05e-02_dp, rvap = 461.495e-02_dp, evafac = 1.0_dp
      real(kind=dp) :: heat_transfer_coefficient, cell_area, wxL, wyL, bak2, bottom_water_temperature
      real(kind=dp) :: solar_radiation_soil_heat_flux, soil_to_water_heat_flux, soil_water_heat_transfer_coefficient, rdtsdz

      real(kind=dp) :: ice_free_area_fraction !< area fraction of ice cover (-)
      real(kind=dp) :: qlong_ice !< coefficient for long wave radiation of ice (J m-2 s-1 K-4)
      real(kind=dp) :: surface_temperature !< surface temperature ... temperature of water, ice or snow depending on their presence (degC)
      real(kind=dp) :: surface_albedo !< local surface albedo (may differ from albedo when ice/snow is present)
      real(kind=dp) :: salinity !< water salinity (ppt)

      real(kind=dp), parameter :: MIN_THICK = 0.001_fp !< threshold thickness for ice/snow to overrule the underlying layer (m)

      if (ja_icecover /= ICECOVER_NONE) then
         ice_free_area_fraction = 1.0_dp - ice_area_fraction(n)
      else
         ice_free_area_fraction = 1.0_dp
      end if

      air_pressure_in_cell = 1.0e-2_dp * BACKGROUND_AIR_PRESSURE ! Air pressure (mbar)
      relative_humidity_in_cell = 1.0e-2_dp * BACKGROUND_HUMIDITY ! ( )
      cloudiness_in_cell = 1.0e-2_dp * BACKGROUND_CLOUDINESS ! ( )
      evaporative_heat_flux_coefficient = dalton ! Dalton  number = 1.50e-3 (Gill, 1982)           evaporative flux
      convective_heat_flux_coefficient = stanton ! Stanton number = 1.45e-3 (Friehe&Schmitt, 1976) convective heat flux
      net_solar_radiation_in_cell = 0.0_dp
      nominal_solar_radiation_in_cell = nominal_solar_radiation
      call getlink1(n, L)
      if (relativewind > 0.0_dp) then
         wxL = wx(L) - relativewind * ucx(ktop(n))
         wyL = wy(L) - relativewind * ucy(ktop(n))
      else
         wxL = wx(L)
         wyL = wy(L)
      end if
      wind_speed_in_cell = sqrt(wxL * wxL + wyL * wyL)

      call getkbotktop(n, k_bot, k_top)

      water_temperature_in_cell = constituents(itemp, k_top)
      if (surftempsmofac > 0.0_dp) then
         cell_area = ba(n)
         water_temperature_in_cell = water_temperature_in_cell * cell_area
         do ll = 1, nd(n)%lnx
            l = abs(nd(n)%ln(ll))
            k2 = ln(1, l) + ln(2, l) - n
            if (hs(k2) > epshstem) then
               bak2 = surftempsmofac * ba(k2)
               water_temperature_in_cell = water_temperature_in_cell + constituents(itemp, ktop(k2)) * bak2
               cell_area = cell_area + bak2
            end if
         end do
         if (cell_area > 0.0_dp) then
            water_temperature_in_cell = water_temperature_in_cell / cell_area
         end if
      end if

      air_temperature_in_cell = air_temperature(n)

      if (temperature_model == TEMPERATURE_MODEL_EXCESS) then

         heat_transfer_coefficient = 4.48_dp + 0.049_dp * water_temperature_in_cell + fwind * (3.5_dp + 2.05_dp * wind_speed_in_cell) * (1.12_dp + 0.018_dp * water_temperature_in_cell + 0.00158_dp * water_temperature_in_cell**2)

         total_heat_flux = -heat_transfer_coefficient * (water_temperature_in_cell - backgroundwatertemperature)
         heat_capacity_water_cell_area = rcpi * ba(n)
         heatsrc0(k_top) = heatsrc0(k_top) + total_heat_flux * heat_capacity_water_cell_area * ice_free_area_fraction ! fill heat source array

         if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! todo, only at mapintervals
            qtotmap(n) = total_heat_flux
         end if

      else if (temperature_model == TEMPERATURE_MODEL_COMPOSITE) then

         ! Set surface_temperature either to water_temperature_in_cell or to ice_temperature(n) or to snow_temperature(n)
         ! and use a local surface_albedo so we do not overwrite the module variable `albedo`.
         surface_albedo = albedo
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (snow_thickness(n) > MIN_THICK) then
               ! ice and snow
               surface_albedo = snow_albedo
               surface_temperature = kelvin_to_celsius(snow_temperature(n))
            elseif (ice_thickness(n) > MIN_THICK) then
               ! ice but no snow
               surface_albedo = ice_albedo
               surface_temperature = kelvin_to_celsius(ice_temperature(n))
            else
               ! no ice and no snow, but ice_modelling switched on
               surface_temperature = water_temperature_in_cell
            end if
         else
            ! ice_modelling switched off
            surface_temperature = water_temperature_in_cell
         end if

         relative_humidity_in_cell = min(1.0_dp, max(0.0_dp, 0.01_dp * relative_humidity(n)))
         cloudiness_in_cell = min(1.0_dp, max(0.0_dp, 0.01_dp * cloudiness(n)))

         if (air_pressure_available) then
            air_pressure_in_cell = 0.01_dp * air_pressure(n)
         end if

         ! Solar radiation restricted by presence of clouds and/or reflection of water surface (albedo)
         if (net_solar_radiation_available) then
            net_solar_radiation_in_cell = solar_radiation(n)
         else if (solar_radiation_available) then
            net_solar_radiation_in_cell = solar_radiation(n) * (1.0_dp - surface_albedo)
         else ! Calculate solar radiation from cloud coverage specified in file
            if (jsferic == 1) then
               nominal_solar_radiation_in_cell = calculate_nominal_solar_radiation(xz(n), yz(n), time_in_hours)
            end if
            if (nominal_solar_radiation_in_cell > 0.0_dp) then
               net_solar_radiation_in_cell = nominal_solar_radiation_in_cell * (1.0_dp - 0.40_dp * cloudiness_in_cell - 0.38_dp * cloudiness_in_cell * cloudiness_in_cell) * (1.0_dp - surface_albedo)
            else
               net_solar_radiation_in_cell = 0.0_dp
            end if
         end if

         if (ja_solar_radiation_factor > 0) then
            if (comparereal(solar_radiation_factor(n), dmiss) /= 0) then
               net_solar_radiation_in_cell = net_solar_radiation_in_cell * solar_radiation_factor(n)
            end if
         end if
         net_solar_radiation(n) = net_solar_radiation_in_cell ! net_solar_radiation is passed on to fm_wq_processes

         heat_capacity_water_cell_area = rcpi * ba(n)
         solar_radiation_flux = net_solar_radiation_in_cell * heat_capacity_water_cell_area

         if (solar_radiation_flux > 0.0_dp) then

            if (kmx > 0) then ! distribute incoming radiation over water column
               if (secchidepth2 > 0.0_dp) then
                  j2 = 2
               else
                  j2 = 1
               end if

               do j = j2, 1, -1

                  if (j == 1 .and. jasecchisp > 0) then
                     zab(1) = secchisp(n) / 1.7_dp
                  end if

                  zlo = 0.0_dp
                  explo = 1.0_dp

                  do cell_index_3D = k_top, k_bot, -1
                     zup = zlo
                     expup = explo
                     zlo = zws(k_top) - zws(cell_index_3D - 1)
                     ratio = zlo / zab(j)
                     if (ratio > 4.0_dp) then !  .or. cell_index_3D.eq.k_bot) then
                        explo = 0.0_dp
                     else
                        explo = exp(-ratio)
                     end if
                     dexp = expup - explo
                     if (dexp > 0.0_dp) then
                        heatsrc0(cell_index_3D) = heatsrc0(cell_index_3D) + sfr(j) * solar_radiation_flux * dexp * ice_free_area_fraction
                     else
                        exit
                     end if
                  end do
               end do

            else
               heatsrc0(n) = heatsrc0(n) + solar_radiation_flux * ice_free_area_fraction
            end if

         end if

         if (kmx > 0 .and. soiltempthick > 0.0_dp) then
            if (solar_radiation_flux > 0.0_dp) then
               solar_radiation_soil_heat_flux = net_solar_radiation_in_cell * explo
            else
               solar_radiation_soil_heat_flux = 0.0_dp
            end if
            soil_water_heat_transfer_coefficient = 1.0_dp / (0.5_dp * soiltempthick) ! thermalcond sand = 0.15 -> 4 for dry -> saturated, [weighted_sums/mK]
            bottom_water_temperature = constituents(itemp, k_bot)
            soil_to_water_heat_flux = soil_water_heat_transfer_coefficient * (bottom_water_temperature - tbed(n))
            heatsrc0(k_bot) = heatsrc0(k_bot) - heat_capacity_water_cell_area * soil_to_water_heat_flux * ice_free_area_fraction
            rdtsdz = rcpi * dts / soiltempthick
            tbed(n) = (tbed(n) + rdtsdz * (solar_radiation_soil_heat_flux + soil_water_heat_transfer_coefficient * bottom_water_temperature)) / (1.0_dp + soil_water_heat_transfer_coefficient * rdtsdz)
         end if

         saturation_vapor_pressure_at_air_temperature = compute_saturation_pressure(air_temperature_in_cell)
         saturation_vapor_pressure_at_surface_temperature = compute_saturation_pressure(surface_temperature) ! eq.(A.12):

         vapor_pressure_air_humidity = relative_humidity_in_cell * saturation_vapor_pressure_at_air_temperature

         specific_humidity_surface_saturation = (0.62_dp * saturation_vapor_pressure_at_surface_temperature) / (air_pressure_in_cell - 0.38_dp * saturation_vapor_pressure_at_surface_temperature)
         specific_humidity_air_surface = (0.62_dp * vapor_pressure_air_humidity) / (air_pressure_in_cell - 0.38_dp * vapor_pressure_air_humidity) ! saturated air near water surface; eq.(A.9)+(A.10):

         latent_heat_vaporization = 2.5e6_dp - 2.3e3_dp * surface_temperature ! eq.(A.19.b): (J/kg)

         if (Stanton < 0.0_dp) then ! if specified negative, use windspeed dependent Cd coeff
            convective_heat_flux_coefficient = abs(Stanton) * cdwcof(L)
         end if
         if (Dalton < 0.0_dp) then ! if specified negative, use windspeed dependent Cd coeff
            evaporative_heat_flux_coefficient = abs(Dalton) * cdwcof(L)
         end if

         vapor_pressure_difference = specific_humidity_surface_saturation - specific_humidity_air_surface ! D3D, both positive and negative evaporation, cannot be correct
         if (jadelvappos == 1) then
            vapor_pressure_difference = max(0.0_dp, vapor_pressure_difference) ! DPM, DFM This must be positive, otherwise heat is pumped into water
         end if ! causing air to cool down below prescribed temperature, immedia. and

         ! change parameters for ice modelling
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (ice_thickness(n) > MIN_THICK) then
               ! in case of ice (and snow) overrule the Stanton number (convective heat flux)
               convective_heat_flux_coefficient = 0.00232_dp
            end if
         end if

         if (ja_airdensity > 0 .or. ja_computed_airdensity > 0) then
            air_density_in_cell = air_density(n)
         else
            air_density_in_cell = rhoair
         end if

         evaporative_heat_flux = -evaporative_heat_flux_coefficient * air_density_in_cell * wind_speed_in_cell * vapor_pressure_difference * latent_heat_vaporization ! heat loss of water by evaporation eq.(A.19.a); Dalton number is evaporative_heat_flux_coefficient:

         convective_heat_flux = -convective_heat_flux_coefficient * air_density_in_cell * cpa * wind_speed_in_cell * (surface_temperature - air_temperature_in_cell) ! heat loss of water by convection eq.(A.23); Stanton number is convective_heat_flux_coefficient:

         water_surface_temperature_kelvin = celsius_to_kelvin(surface_temperature)
         if (long_wave_radiation_available) then
            longwave_radiation_flux = em * (long_wave_radiation(n) - stf * (water_surface_temperature_kelvin**4)) ! difference between prescribed long wave downward flux and calculated upward flux
         else
            longwave_radiation_flux = -em * stf * (water_surface_temperature_kelvin**4) * (0.39_dp - 0.05_dp * sqrt(vapor_pressure_air_humidity)) ! heat loss by effective infrared back radiation hl, restricted by
            longwave_radiation_flux = longwave_radiation_flux * (1.0_dp - 0.6_dp * cloudiness_in_cell**2) !  presence of clouds and water vapour in air; eq.(A.22):
         end if

         free_convection_heat_flux = 0.0_dp
         free_convective_sensible_heat_flux = 0.0_dp
         free_convective_latent_heat_flux = 0.0_dp ! Contribution by free convection:
         air_density_surface = ((air_pressure_in_cell - saturation_vapor_pressure_at_surface_temperature) / rdry + saturation_vapor_pressure_at_surface_temperature / rvap) / celsius_to_kelvin(surface_temperature)
         air_density_10m = ((air_pressure_in_cell - vapor_pressure_air_humidity) / rdry + vapor_pressure_air_humidity / rvap) / celsius_to_kelvin(air_temperature_in_cell)
         buoyancy_parameter = 2.0_dp * ag * (air_density_10m - air_density_surface) / (air_density_surface + air_density_10m)
         if (buoyancy_parameter > 0.0_dp) then ! Ri= (buoyancy_parameter/DZ)/ (du/dz)2, Ri>0.25 stable
            free_convection_velocity = buoyancy_parameter * xnuair / pr2
            free_convection_velocity = cfree * free_convection_velocity**0.33333333_dp
            free_convective_sensible_heat_flux = min(0.0_dp, -air_density_in_cell * cpa * free_convection_velocity * (surface_temperature - air_temperature_in_cell) * evafac) ! Free convective sensible heat loss:
            free_convective_latent_heat_flux = min(0.0_dp, -free_convection_velocity * (specific_humidity_surface_saturation - specific_humidity_air_surface) * latent_heat_vaporization * evafac * (air_density_surface + air_density_10m) * 0.5_dp) ! Free convective latent/evaporation heat loss:
            free_convection_heat_flux = free_convective_sensible_heat_flux + free_convective_latent_heat_flux
         end if

         total_heat_flux = evaporative_heat_flux + convective_heat_flux + longwave_radiation_flux + free_convection_heat_flux ! net heat flux [W/m^2] into water, solar radiation excluded:
         if (jaevap > 0) then
            evap(n) = (evaporative_heat_flux + free_convective_latent_heat_flux) / (latent_heat_vaporization * rhomean) * ice_free_area_fraction ! (J/sm2)/(J/kg)/kg/m3) = (m/s)
         end if

         heatsrc0(k_top) = heatsrc0(k_top) + total_heat_flux * heat_capacity_water_cell_area * ice_free_area_fraction ! fill heat source array

         ! In case of ice preprocessing of ice quantities
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (ice_thickness(n) > MIN_THICK .or. (water_temperature_in_cell < 0.1_fp .and. air_temperature(n) < 0.0_fp)) then

               ! Compute Qlong_ice (NB. Delft3D-FLOW definition is used, with opposite sign, so that
               ! algorithm in preprocess_icecover remains identical to the one for Delft3D-FLOW
               qlong_ice = em * stf * (0.39_dp - 0.05_dp * sqrt(vapor_pressure_air_humidity)) * (1.0_dp - 0.6_dp * cloudiness_in_cell**2)

               qh_air2ice(n) = net_solar_radiation_in_cell + total_heat_flux

               if (isalt > 0) then
                  if (kmx == 0) then
                     salinity = constituents(isalt, n)
                  else
                     salinity = constituents(isalt, k_top)
                  end if
               else
                  salinity = backgroundsalinity
               end if
               call preprocess_icecover(n, Qlong_ice, water_temperature_in_cell, salinity, wind_speed_in_cell)
            end if

            if (ice_thickness(n) > MIN_THICK) then
               ! recompute heatsrc0 because of presence of ice
               if (kmx > 0) then
                  heatsrc0(k_top) = qh_ice2wat(n) * ice_free_area_fraction
               else
                  heatsrc0(n) = qh_ice2wat(n) * ice_free_area_fraction
               end if
            end if
         end if

         if (jamapheatflux > 0 .or. jahisheatflux > 0) then ! todo, only at mapintervals
            qsunmap(n) = net_solar_radiation_in_cell
            qevamap(n) = evaporative_heat_flux
            qconmap(n) = convective_heat_flux
            qlongmap(n) = longwave_radiation_flux
            qfrevamap(n) = free_convective_latent_heat_flux
            qfrconmap(n) = free_convective_sensible_heat_flux
            qtotmap(n) = net_solar_radiation_in_cell + total_heat_flux
         end if

         total_area = 0.0_dp ! these 2 lines outside loop
         weighted_sums = 0.0_dp ! array of spatially averaged output

         cell_area_weight = ba(n) ! Spatially averaged time series output :
         total_area = total_area + cell_area_weight ! Total area
         weighted_sums(1) = time_in_hours / 24.0_dp ! Time in days
         weighted_sums(2) = weighted_sums(2) + cell_area_weight * air_temperature_in_cell
         weighted_sums(3) = weighted_sums(3) + cell_area_weight * constituents(itemp, k_top) ! sea surface temperature
         if (soiltempthick > 0.0_dp) then
            weighted_sums(4) = weighted_sums(4) + cell_area_weight * tbed(n)
         end if
         weighted_sums(5) = weighted_sums(5) + cell_area_weight * (net_solar_radiation_in_cell + total_heat_flux)
         weighted_sums(6) = weighted_sums(6) + cell_area_weight * net_solar_radiation_in_cell
         weighted_sums(7) = weighted_sums(7) + cell_area_weight * longwave_radiation_flux
         weighted_sums(8) = weighted_sums(8) + cell_area_weight * convective_heat_flux
         weighted_sums(9) = weighted_sums(9) + cell_area_weight * evaporative_heat_flux
         weighted_sums(10) = weighted_sums(10) + cell_area_weight * free_convective_sensible_heat_flux
         weighted_sums(11) = weighted_sums(11) + cell_area_weight * free_convective_latent_heat_flux
         weighted_sums(12) = weighted_sums(12) + cell_area_weight * wind_speed_in_cell
         weighted_sums(13) = weighted_sums(13) + cell_area_weight * relative_humidity_in_cell
         weighted_sums(14) = weighted_sums(14) + cell_area_weight * cloudiness_in_cell
         weighted_sums(15) = weighted_sums(15) + cell_area_weight * air_pressure_in_cell

         ncols = 15
         if (total_area > 0.0_dp) then
            weighted_sums(2:ncols) = weighted_sums(2:ncols) / total_area
         end if
         qsunav = weighted_sums(6)
         qlongav = weighted_sums(7)
         qconav = weighted_sums(8)
         qevaav = weighted_sums(9)
         qfrconav = weighted_sums(10)
         qfrevaav = weighted_sums(11)
      end if

   contains

      !> Computes the saturation pressure of water vapor at a specified temperature (degrees Celsius)
      pure function compute_saturation_pressure(temperature) result(saturation_pressure)
         use precision, only: dp

         real(kind=dp), intent(in) :: temperature !< Temperature (degrees Celsius)
         real(kind=dp) :: saturation_pressure !< Saturation pressure (hPa)

         saturation_pressure = 10.0_dp**((0.7859_dp + 0.03477_dp * temperature) / (1.0_dp + 0.00412_dp * temperature))
      end function compute_saturation_pressure

   end subroutine heatun

end module m_heatun
