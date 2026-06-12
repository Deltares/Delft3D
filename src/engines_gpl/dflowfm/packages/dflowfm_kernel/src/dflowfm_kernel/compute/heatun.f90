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
      use m_physcoef, only: ag, rhomean, backgroundsalinity, backgroundwatertemperature, dalton, epshstem, stanton, secchi_depth, &
                            soiltempthick, BACKGROUND_AIR_PRESSURE, BACKGROUND_HUMIDITY, BACKGROUND_CLOUDINESS, surftempsmofac, &
                            jadelvappos, free_convection_coefficient, secchi_radiation_fraction, diffuse_attenuation_coefficient, &
                            POOLE_ATKINS_PARAMETER
      use m_heatfluxes, only: EMMISIVITY_FACTOR, albedo, SPECIFIC_HEAT_AIR, secchi_depth_is_spatially_varying, spatial_secchi_depth, rcpi, fwind, qtotmap, qsunmap, qevamap, &
                              qconmap, qlongmap, qfrevamap, qfrconmap, qsunav, qlongav, qconav, qevaav, qfrconav, qfrevaav, &
                              PRANDTL_NUMBER_SQUARED, KINEMATIC_VISCOSITY_AIR, GAS_CONSTANT_DRY_AIR, GAS_CONSTANT_WATER_VAPOR, MIN_ICE_SNOW_THICKNESS
      use m_flow, only: kmx, hs, solar_radiation_factor, zws, ucx, ucy, ktop
      use m_flowparameters, only: his_write_settings, map_write_settings, temperature_model, TEMPERATURE_MODEL_EXCESS, TEMPERATURE_MODEL_COMPOSITE, &
                                  ja_solar_radiation_factor, air_water_interaction_model, AIR_WATER_INTERACTION_MODEL_MOST, itempforcingtyp
      use m_missing, only: dmiss
      use m_flowgeom, only: ba, nd, ln, yz, xz
      use m_sferic, only: jsferic
      use m_flowtimes, only: dts
      use m_transport, only: constituents, itemp, isalt
      use m_fm_icecover, only: ja_icecover, ice_area_fraction, ice_albedo, ice_thickness, ice_temperature, snow_albedo, &
                               snow_thickness, snow_temperature, qh_air2ice, qh_ice2wat, ICECOVER_NONE, ICECOVER_SEMTNER, preprocess_icecover
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_link1, only: getlink1
      use m_wind, only: air_pressure_available, jaevap, long_wave_radiation_available, sensible_heat_flux_available, latent_heat_flux_available, relativewind, air_temperature, wx, wy, &
                        relative_humidity, cloudiness, air_pressure, heatsrc0, solar_radiation, solar_radiation_available, net_solar_radiation, &
                        net_solar_radiation_available, tbed, rhoair, long_wave_radiation, sensible_heat_flux, latent_heat_flux, evap, cdwcof, air_density, ja_airdensity, &
                        ja_computed_airdensity
      use m_qsun_nominal, only: calculate_nominal_solar_radiation

      integer, intent(in) :: n !< Cell index
      real(kind=dp), intent(in) :: time_in_hours !< Current model time in hours
      real(kind=dp), intent(in) :: nominal_solar_radiation !< Nominal solar radiation (W/m2) at current time and location, used when solar radiation is not available in file but needs to be calculated from cloud coverage

      real(kind=dp), dimension(20) :: weighted_sums

      real(kind=dp) :: net_solar_radiation_in_cell, air_pressure_in_cell, air_temperature_in_cell, &
                       water_temperature_in_cell, relative_humidity_in_cell, cloudiness_in_cell, wind_speed_in_cell, air_density_in_cell
      real(kind=dp) :: specific_humidity_surface_saturation, specific_humidity_air_surface, latent_heat_vaporization, &
                 longwave_radiation_flux, saturation_vapor_pressure_at_surface_temperature, vapor_pressure_air_humidity, &
                       forced_sensible_heat_flux, forced_latent_heat_flux
      real(kind=dp) :: heat_capacity_water_cell_area, total_heat_flux, total_area, &
                       cell_area_weight, free_convective_sensible_heat_flux, free_convective_latent_heat_flux
      real(kind=dp) :: heat_transfer_coefficient, cell_area, wxL, wyL, bak2
      real(kind=dp) :: ice_free_area_fraction !< area fraction of ice cover (-)
      real(kind=dp) :: surface_temperature !< surface temperature ... temperature of water, ice or snow depending on their presence (degC)
      logical :: include_radiation_fluxes
      integer :: k_bot, k_top, k2, L, LL, ncols

      if (ja_icecover /= ICECOVER_NONE) then
         ice_free_area_fraction = 1.0_dp - ice_area_fraction(n)
      else
         ice_free_area_fraction = 1.0_dp
      end if

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
      heat_capacity_water_cell_area = rcpi * ba(n)

      if (temperature_model == TEMPERATURE_MODEL_EXCESS) then

         heat_transfer_coefficient = 4.48_dp + 0.049_dp * water_temperature_in_cell + fwind * (3.5_dp + 2.05_dp * wind_speed_in_cell) * (1.12_dp + 0.018_dp * water_temperature_in_cell + 0.00158_dp * water_temperature_in_cell**2)

         total_heat_flux = -heat_transfer_coefficient * (water_temperature_in_cell - backgroundwatertemperature)
         heatsrc0(k_top) = heatsrc0(k_top) + total_heat_flux * heat_capacity_water_cell_area * ice_free_area_fraction

         if (map_write_settings%heatflux > 0 .or. his_write_settings%heatflux > 0) then ! todo, only at mapintervals
            qtotmap(n) = total_heat_flux
         end if

      else if (temperature_model == TEMPERATURE_MODEL_COMPOSITE .or. air_water_interaction_model == AIR_WATER_INTERACTION_MODEL_MOST) then
         ! Set surface temperature from water/ice/snow state required by downstream flux routines.
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (snow_thickness(n) > MIN_ICE_SNOW_THICKNESS) then
               surface_temperature = kelvin_to_celsius(snow_temperature(n))
            elseif (ice_thickness(n) > MIN_ICE_SNOW_THICKNESS) then
               surface_temperature = kelvin_to_celsius(ice_temperature(n))
            else
               surface_temperature = water_temperature_in_cell
            end if
         else
            surface_temperature = water_temperature_in_cell
         end if

         net_solar_radiation_in_cell = 0.0_dp
         longwave_radiation_flux = 0.0_dp
         cloudiness_in_cell = min(1.0_dp, max(0.0_dp, 0.01_dp * BACKGROUND_CLOUDINESS))
         include_radiation_fluxes = net_solar_radiation_available .or. solar_radiation_available .or. &
                                   long_wave_radiation_available .or. (itempforcingtyp >= 1 .and. itempforcingtyp <= 4)
         if (include_radiation_fluxes) then
            call compute_longwave_radiation_flux(n, k_top, k_bot, time_in_hours, nominal_solar_radiation, &
                                                 water_temperature_in_cell, air_temperature_in_cell, surface_temperature, &
                                                 cloudiness_in_cell, net_solar_radiation_in_cell, heat_capacity_water_cell_area, &
                                                 longwave_radiation_flux, ice_free_area_fraction)
         end if

         if (air_water_interaction_model == AIR_WATER_INTERACTION_MODEL_MOST) then
             forced_sensible_heat_flux = sensible_heat_flux(n)
             forced_latent_heat_flux = latent_heat_flux(n)
             free_convective_sensible_heat_flux = 0.0_dp
             free_convective_latent_heat_flux = 0.0_dp
         else
             if (ja_airdensity > 0 .or. ja_computed_airdensity > 0) then
                air_density_in_cell = air_density(n)
             else
                air_density_in_cell = rhoair
             end if

             call compute_forced_heat_fluxes(n, L, surface_temperature, air_temperature_in_cell, wind_speed_in_cell, air_density_in_cell, &
                                             relative_humidity_in_cell, air_pressure_in_cell, &
                                             saturation_vapor_pressure_at_surface_temperature, vapor_pressure_air_humidity, &
                                             specific_humidity_surface_saturation, specific_humidity_air_surface, &
                                             latent_heat_vaporization, forced_sensible_heat_flux, forced_latent_heat_flux)

             call compute_free_convective_fluxes( air_pressure_in_cell, saturation_vapor_pressure_at_surface_temperature, &
                                                  vapor_pressure_air_humidity, surface_temperature, air_temperature_in_cell, &
                                                  specific_humidity_surface_saturation, specific_humidity_air_surface, &
                                                  latent_heat_vaporization, air_density_in_cell, &
                                                  free_convective_sensible_heat_flux, free_convective_latent_heat_flux)
         end if
         total_heat_flux = forced_latent_heat_flux + forced_sensible_heat_flux + longwave_radiation_flux + free_convective_sensible_heat_flux + free_convective_latent_heat_flux

         if (jaevap > 0) then
            evap(n) = (forced_latent_heat_flux + free_convective_latent_heat_flux) / (latent_heat_vaporization * rhomean) * ice_free_area_fraction
         end if

         heatsrc0(k_top) = heatsrc0(k_top) + total_heat_flux * heat_capacity_water_cell_area * ice_free_area_fraction

         ! In case of ice preprocessing of ice quantities
         call apply_ice_effects(n, net_solar_radiation_in_cell, total_heat_flux, vapor_pressure_air_humidity, &
                                 cloudiness_in_cell, water_temperature_in_cell, wind_speed_in_cell, &
                                 ice_free_area_fraction, k_top)

         if (map_write_settings%heatflux > 0 .or. his_write_settings%heatflux > 0) then ! todo, only at mapintervals
            qsunmap(n) = net_solar_radiation_in_cell
            qevamap(n) = forced_latent_heat_flux
            qconmap(n) = forced_sensible_heat_flux
            qlongmap(n) = longwave_radiation_flux
            qfrevamap(n) = free_convective_latent_heat_flux
            qfrconmap(n) = free_convective_sensible_heat_flux
            qtotmap(n) = net_solar_radiation_in_cell + total_heat_flux
         end if

         total_area = 0.0_dp
         weighted_sums = 0.0_dp ! array of spatially averaged output

         cell_area_weight = ba(n) ! Spatially averaged time series output
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
         weighted_sums(8) = weighted_sums(8) + cell_area_weight * forced_sensible_heat_flux
         weighted_sums(9) = weighted_sums(9) + cell_area_weight * forced_latent_heat_flux
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

      !> Computes surface state, cloudiness, net solar radiation and longwave radiation, and applies solar heating terms.
      subroutine compute_longwave_radiation_flux(n, k_top, k_bot, time_in_hours_in, nominal_solar_radiation_in, &
                                                 water_temperature_in_cell, air_temp, surface_temp, cloudiness_in_cell, &
                                                 net_solar_radiation_in_cell, heat_capacity_water_cell_area, longwave_radiation_flux, &
                                                 ice_free_area_fraction)
         integer, intent(in) :: n
         integer, intent(in) :: k_top, k_bot
         real(kind=dp), intent(in) :: time_in_hours_in, nominal_solar_radiation_in
         real(kind=dp), intent(in) :: water_temperature_in_cell, air_temp, surface_temp, ice_free_area_fraction
         real(kind=dp), intent(out) :: cloudiness_in_cell, net_solar_radiation_in_cell, heat_capacity_water_cell_area, longwave_radiation_flux

         real(kind=dp), dimension(2) :: diffuse_attenuation_coefficient_in_cell
         real(kind=dp) :: surf_temp_kelvin, sat_vapor_pressure_air, vapor_press_air, nominal_solar_radiation_in_cell
         real(kind=dp) :: relative_humidity_in_cell, surface_albedo
         real(kind=dp) :: solar_radiation_flux, dexp, zlo, zup, explo, expup, ratio
         real(kind=dp) :: solar_radiation_soil_heat_flux, soil_to_water_heat_flux, soil_water_heat_transfer_coefficient, rdtsdz
         real(kind=dp) :: bottom_water_temperature
         logical :: has_user_solar, has_cloudiness
         integer :: cell_index_3D, j, j2

         ! Set surface temperature and local albedo depending on water/ice/snow presence.
         surface_albedo = albedo
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (snow_thickness(n) > MIN_ICE_SNOW_THICKNESS) then
               surface_albedo = snow_albedo
            elseif (ice_thickness(n) > MIN_ICE_SNOW_THICKNESS) then
               surface_albedo = ice_albedo
            end if
         end if

         has_user_solar = net_solar_radiation_available .or. solar_radiation_available
         has_cloudiness = (itempforcingtyp >= 1 .and. itempforcingtyp <= 4)

         if (has_cloudiness) then
            cloudiness_in_cell = min(1.0_dp, max(0.0_dp, 0.01_dp * cloudiness(n)))
         else
            cloudiness_in_cell = min(1.0_dp, max(0.0_dp, 0.01_dp * BACKGROUND_CLOUDINESS))
         end if

         ! Solar radiation restricted by presence of clouds and/or reflection of water surface (albedo)
         nominal_solar_radiation_in_cell = nominal_solar_radiation_in
         if (has_user_solar) then
            if (net_solar_radiation_available) then
               net_solar_radiation_in_cell = solar_radiation(n)
            else if (solar_radiation_available) then
               net_solar_radiation_in_cell = solar_radiation(n) * (1.0_dp - surface_albedo)
            end if
         else
            ! Calculate solar radiation from cloud coverage specified in file
            if (jsferic == 1) then
               nominal_solar_radiation_in_cell = calculate_nominal_solar_radiation(xz(n), yz(n), time_in_hours_in)
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
         if (allocated(net_solar_radiation)) then
            net_solar_radiation(n) = net_solar_radiation_in_cell ! net_solar_radiation is passed on to fm_wq_processes
         end if

         heat_capacity_water_cell_area = rcpi * ba(n)
         solar_radiation_flux = net_solar_radiation_in_cell * heat_capacity_water_cell_area

         if (solar_radiation_flux > 0.0_dp) then
            if (kmx > 0) then ! distribute incoming radiation over water column
               diffuse_attenuation_coefficient_in_cell(1) = diffuse_attenuation_coefficient(1)
               diffuse_attenuation_coefficient_in_cell(2) = diffuse_attenuation_coefficient(2)

               if (secchi_depth(2) > 0.0_dp) then
                  j2 = 2
               else
                  j2 = 1
               end if

               do j = j2, 1, -1

                  if (j == 1 .and. secchi_depth_is_spatially_varying) then
                     diffuse_attenuation_coefficient_in_cell(1) = spatial_secchi_depth(n) / POOLE_ATKINS_PARAMETER
                  end if

                  zlo = 0.0_dp
                  explo = 1.0_dp

                  do cell_index_3D = k_top, k_bot, -1
                     zup = zlo
                     expup = explo
                     zlo = zws(k_top) - zws(cell_index_3D - 1)
                     ratio = zlo / diffuse_attenuation_coefficient_in_cell(j)
                     if (ratio > 4.0_dp) then
                        explo = 0.0_dp
                     else
                        explo = exp(-ratio)
                     end if
                     dexp = expup - explo
                     if (dexp > 0.0_dp) then
                        heatsrc0(cell_index_3D) = heatsrc0(cell_index_3D) + secchi_radiation_fraction(j) * solar_radiation_flux * dexp * ice_free_area_fraction
                     else
                        exit
                     end if
                  end do
               end do

            else
               heatsrc0(n) = heatsrc0(n) + solar_radiation_flux * ice_free_area_fraction
               explo = 0.0_dp
            end if
         else
            explo = 0.0_dp
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

         relative_humidity_in_cell = min(1.0_dp, max(0.0_dp, 0.01_dp * relative_humidity(n)))
         sat_vapor_pressure_air = compute_saturation_pressure(air_temp)
         vapor_press_air = relative_humidity_in_cell * sat_vapor_pressure_air

         surf_temp_kelvin = celsius_to_kelvin(surface_temp)
         if (long_wave_radiation_available) then
            longwave_radiation_flux = EMMISIVITY_FACTOR * (long_wave_radiation(n) - stf * (surf_temp_kelvin**4))
         else
            longwave_radiation_flux = -EMMISIVITY_FACTOR * stf * (surf_temp_kelvin**4) * (0.39_dp - 0.05_dp * sqrt(vapor_press_air))
            longwave_radiation_flux = longwave_radiation_flux * (1.0_dp - 0.6_dp * cloudiness_in_cell**2)
         end if
      end subroutine compute_longwave_radiation_flux

      !> Computes forced sensible and latent heat fluxes and all required pre-calculations.
      subroutine compute_forced_heat_fluxes(n, L, surface_temp, air_temp, wind_speed, air_dens, relative_humidity_in_cell, air_pressure_in_cell, &
                          sat_vap_press_surface, vapor_press_air, specific_hum_surface_sat, specific_hum_air, &
                                            latent_heat_vap, forced_sensible_flux, forced_latent_flux)
         integer, intent(in) :: n                             !< Cell index
         integer, intent(in) :: L                             !< Link index
         real(kind=dp), intent(in) :: surface_temp            !< Surface temperature (degC)
         real(kind=dp), intent(in) :: air_temp                !< Air temperature (degC)
         real(kind=dp), intent(in) :: wind_speed              !< Wind speed (m/s)
         real(kind=dp), intent(in) :: air_dens                !< Air density (kg/m3)
         real(kind=dp), intent(out) :: relative_humidity_in_cell !< Relative humidity (-)
         real(kind=dp), intent(out) :: air_pressure_in_cell   !< Air pressure (hPa)
         real(kind=dp), intent(out) :: sat_vap_press_surface  !< Saturation vapor pressure at surface temperature (hPa)
         real(kind=dp), intent(out) :: vapor_press_air        !< Vapor pressure of air humidity (hPa)
         real(kind=dp), intent(out) :: specific_hum_surface_sat !< Specific humidity at surface saturation (-)
         real(kind=dp), intent(out) :: specific_hum_air       !< Specific humidity of air (-)
         real(kind=dp), intent(out) :: latent_heat_vap        !< Latent heat of vaporization (J/kg)
         real(kind=dp), intent(out) :: forced_sensible_flux   !< Forced sensible heat flux (W/m2)
         real(kind=dp), intent(out) :: forced_latent_flux     !< Forced latent heat flux (W/m2)
         real(kind=dp) :: sat_vap_press_air, vapor_press_diff
         real(kind=dp) :: convective_heat_flux_coefficient, evaporative_heat_flux_coefficient

         relative_humidity_in_cell = min(1.0_dp, max(0.0_dp, 0.01_dp * relative_humidity(n)))

         air_pressure_in_cell = 1.0e-2_dp * BACKGROUND_AIR_PRESSURE
         if (air_pressure_available) then
            air_pressure_in_cell = 0.01_dp * air_pressure(n)
         end if

         sat_vap_press_air = compute_saturation_pressure(air_temp)
         sat_vap_press_surface = compute_saturation_pressure(surface_temp)
         vapor_press_air = relative_humidity_in_cell * sat_vap_press_air

         specific_hum_surface_sat = (0.62_dp * sat_vap_press_surface) / (air_pressure_in_cell - 0.38_dp * sat_vap_press_surface)
         specific_hum_air = (0.62_dp * vapor_press_air) / (air_pressure_in_cell - 0.38_dp * vapor_press_air)
         latent_heat_vap = 2.5e6_dp - 2.3e3_dp * surface_temp

         convective_heat_flux_coefficient = stanton
         evaporative_heat_flux_coefficient = dalton
         if (stanton < 0.0_dp) then ! if specified negative, use windspeed dependent Cd coeff
            convective_heat_flux_coefficient = abs(stanton) * cdwcof(L)
         end if
         if (dalton < 0.0_dp) then ! if specified negative, use windspeed dependent Cd coeff
            evaporative_heat_flux_coefficient = abs(dalton) * cdwcof(L)
         end if

         vapor_press_diff = specific_hum_surface_sat - specific_hum_air
         if (jadelvappos == 1) then
            vapor_press_diff = max(0.0_dp, vapor_press_diff)
         end if

         ! change parameters for ice modelling
         if (ja_icecover == ICECOVER_SEMTNER) then
            if (ice_thickness(n) > MIN_ICE_SNOW_THICKNESS) then
               ! in case of ice (and snow) overrule the Stanton number (convective heat flux)
               convective_heat_flux_coefficient = 0.00232_dp
            end if
         end if

         if (latent_heat_flux_available) then
            forced_latent_flux = latent_heat_flux(n)
         else
            forced_latent_flux = -evaporative_heat_flux_coefficient * air_dens * wind_speed * vapor_press_diff * latent_heat_vap
         end if

         if (sensible_heat_flux_available) then
            forced_sensible_flux = sensible_heat_flux(n)
         else
            forced_sensible_flux = -convective_heat_flux_coefficient * air_dens * SPECIFIC_HEAT_AIR * wind_speed * (surface_temp - air_temp)
         end if
      end subroutine compute_forced_heat_fluxes

      !> Computes the free convective sensible and latent heat fluxes due to buoyancy-driven air-sea exchange
      subroutine compute_free_convective_fluxes(air_press, sat_vap_press_surface, vapor_press_air, &
                                                 surface_temp, air_temp, specific_hum_surface_sat, &
                                                 specific_hum_air, latent_heat_vap, air_dens, &
                                                 free_conv_sensible, free_conv_latent)
         real(kind=dp), intent(in) :: air_press                  !< Air pressure (hPa)
         real(kind=dp), intent(in) :: sat_vap_press_surface      !< Saturation vapor pressure at surface temperature (hPa)
         real(kind=dp), intent(in) :: vapor_press_air            !< Vapor pressure of air humidity (hPa)
         real(kind=dp), intent(in) :: surface_temp               !< Surface temperature (degC)
         real(kind=dp), intent(in) :: air_temp                   !< Air temperature at 10 m (degC)
         real(kind=dp), intent(in) :: specific_hum_surface_sat   !< Specific humidity at surface saturation (-)
         real(kind=dp), intent(in) :: specific_hum_air           !< Specific humidity of air (-)
         real(kind=dp), intent(in) :: latent_heat_vap            !< Latent heat of vaporization (J/kg)
         real(kind=dp), intent(in) :: air_dens                   !< Air density (kg/m3)
         real(kind=dp), intent(out) :: free_conv_sensible        !< Free convective sensible heat flux (W/m2)
         real(kind=dp), intent(out) :: free_conv_latent          !< Free convective latent heat flux (W/m2)
         real(kind=dp) :: air_density_surface, air_density_10m, buoyancy_parameter, free_convection_velocity

         free_conv_sensible = 0.0_dp
         free_conv_latent = 0.0_dp
         air_density_surface = ((air_press - sat_vap_press_surface) / GAS_CONSTANT_DRY_AIR + sat_vap_press_surface / GAS_CONSTANT_WATER_VAPOR) / celsius_to_kelvin(surface_temp)
         air_density_10m = ((air_press - vapor_press_air) / GAS_CONSTANT_DRY_AIR + vapor_press_air / GAS_CONSTANT_WATER_VAPOR) / celsius_to_kelvin(air_temp)
         buoyancy_parameter = 2.0_dp * ag * (air_density_10m - air_density_surface) / (air_density_surface + air_density_10m)
         if (buoyancy_parameter > 0.0_dp) then
            free_convection_velocity = buoyancy_parameter * KINEMATIC_VISCOSITY_AIR / PRANDTL_NUMBER_SQUARED
            free_convection_velocity = free_convection_coefficient * free_convection_velocity**(1.0_dp / 3.0_dp)
            if (.not. sensible_heat_flux_available) then
               free_conv_sensible = min(0.0_dp, -air_dens * SPECIFIC_HEAT_AIR * free_convection_velocity * (surface_temp - air_temp))
            end if
            if (.not. latent_heat_flux_available) then
               free_conv_latent = min(0.0_dp, -free_convection_velocity * (specific_hum_surface_sat - specific_hum_air) * latent_heat_vap * (air_density_surface + air_density_10m) * 0.5_dp)
            end if
         end if
      end subroutine compute_free_convective_fluxes

      !> Applies ice/snow effects: preprocesses ice cover and updates the heat source term accordingly
      subroutine apply_ice_effects(n, net_solar_rad, total_heat_flux, vapor_press_air, &
                                    cloudiness, water_temp, wind_speed, ice_free_area, k_top)
         integer, intent(in) :: n                    !< Cell index
         real(kind=dp), intent(in) :: net_solar_rad  !< Net solar radiation at cell (W/m2)
         real(kind=dp), intent(in) :: total_heat_flux !< Total non-solar heat flux (W/m2)
         real(kind=dp), intent(in) :: vapor_press_air !< Vapor pressure of air humidity (hPa)
         real(kind=dp), intent(in) :: cloudiness      !< Cloudiness (-)
         real(kind=dp), intent(in) :: water_temp      !< Water surface temperature (degC)
         real(kind=dp), intent(in) :: wind_speed      !< Wind speed (m/s)
         real(kind=dp), intent(in) :: ice_free_area   !< Ice-free area fraction (-)
         integer, intent(in) :: k_top                 !< Index of top layer in 3D grid
         real(kind=dp) :: qlong_ice !< Coefficient for long wave radiation of ice (J m-2 s-1 K-4)
         real(kind=dp) :: salinity   !< Water salinity (ppt)

         if (ja_icecover == ICECOVER_SEMTNER) then
            if (ice_thickness(n) > MIN_ICE_SNOW_THICKNESS .or. (water_temp < 0.1_fp .and. air_temperature(n) < 0.0_fp)) then

               ! Compute Qlong_ice (NB. Delft3D-FLOW definition is used, with opposite sign, so that
               ! algorithm in preprocess_icecover remains identical to the one for Delft3D-FLOW
               qlong_ice = EMMISIVITY_FACTOR * stf * (0.39_dp - 0.05_dp * sqrt(vapor_press_air)) * (1.0_dp - 0.6_dp * cloudiness**2)

               qh_air2ice(n) = net_solar_rad + total_heat_flux

               if (isalt > 0) then
                  if (kmx == 0) then
                     salinity = constituents(isalt, n)
                  else
                     salinity = constituents(isalt, k_top)
                  end if
               else
                  salinity = backgroundsalinity
               end if
               call preprocess_icecover(n, qlong_ice, water_temp, salinity, wind_speed)
            end if

            if (ice_thickness(n) > MIN_ICE_SNOW_THICKNESS) then
               ! recompute heatsrc0 because of presence of ice
               if (kmx > 0) then
                  heatsrc0(k_top) = qh_ice2wat(n) * ice_free_area
               else
                  heatsrc0(n) = qh_ice2wat(n) * ice_free_area
               end if
            end if
         end if
      end subroutine apply_ice_effects

   end subroutine heatun

end module m_heatun
