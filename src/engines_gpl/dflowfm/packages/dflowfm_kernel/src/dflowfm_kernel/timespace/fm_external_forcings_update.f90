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

submodule(fm_external_forcings) fm_external_forcings_update
   use timers, only: timstrt, timstop
   use m_flowtimes
   use m_flowgeom
   use m_flow
   use m_meteo
   use m_calbedform
   use m_bedform
   use dfm_error
   use m_calibration, only: calibration_backup_frcu
   use unstruc_channel_flow
   use time_class
   use m_longculverts
   use m_nearfield, only: nearfield_mode, NEARFIELD_UPDATED, addNearfieldData
   use m_airdensity, only: get_airdensity
   use dfm_error
   use m_laterals, only: numlatsg
   use m_physcoef, only: BACKGROUND_AIR_PRESSURE
   implicit none

   integer, parameter :: HUMIDITY_AIRTEMPERATURE_CLOUDINESS = 1
   integer, parameter :: HUMIDITY_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION = 2
   integer, parameter :: DEWPOINT_AIRTEMPERATURE_CLOUDINESS = 3
   integer, parameter :: DEWPOINT_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION = 4
   integer, parameter :: DEWPOINT = 5

   integer :: ierr !< error flag
   logical :: l_set_frcu_mor = .false.
   logical :: first_time_wind

   logical, external :: flow_initwaveforcings_runtime, flow_trachy_needs_update
   character(len=255) :: tmpstr
   type(c_time) :: ecTime !< Time in EC-module

   ! variables for processing the pump with levels, SOBEK style
   logical :: success_copy

contains

   !> set field oriented boundary conditions
   module subroutine set_external_forcings(time_in_seconds, initialization, iresult)
      use m_set_frcu_mor
      use m_physcoef, only: BACKGROUND_AIR_PRESSURE
      double precision, intent(in) :: time_in_seconds !< Time in seconds
      logical, intent(in) :: initialization !< initialization phase
      integer, intent(out) :: iresult !< Integer error status: DFM_NOERR==0 if succesful.

      call timstrt('External forcings', handle_ext)

      success = .true.

      if (allocated(patm)) then
         ! Set the initial value to PavBnd (if provided by user) or BACKGROUND_AIR_PRESSURE with each update.
         ! An initial/reference value is required since .spw files may contain pressure drops/differences.
         ! patm may later be overridden by spatially varying air pressure values.
         if (PavBnd > 0) then
            patm(:) = PavBnd
         else
            patm(:) = BACKGROUND_AIR_PRESSURE
         end if
      end if

      call retrieve_icecover(time_in_seconds)

      if (ja_airdensity > 0) then
         call get_timespace_value_by_item_array_consider_success_value(item_airdensity, airdensity, time_in_seconds)
      end if
      if (ja_computed_airdensity == 1) then
         call get_timespace_value_by_item_array_consider_success_value(item_atmosphericpressure, patm, time_in_seconds)
         call get_timespace_value_by_item_array_consider_success_value(item_airtemperature, tair, time_in_seconds)
         call get_timespace_value_by_item_array_consider_success_value(item_humidity, rhum, time_in_seconds)
         call get_airdensity(patm, tair, rhum, airdensity, iresult)
      end if

      if (update_wind_stress_each_time_step == 0) then ! Update wind in set_external_forcing (each user timestep)
         call calculate_wind_stresses(time_in_seconds, iresult)
         if (iresult /= DFM_NOERR) then
            return
         end if
      end if

      if (jatem > 1) then
         call set_temperature_models(time_in_seconds)
      end if

      if (ja_friction_coefficient_time_dependent > 0) then
         call get_timespace_value_by_item_and_array(item_frcu, frcu, time_in_seconds)
      end if

      call ecTime%set4(time_in_seconds, irefdate, tzone, ecSupportTimeUnitConversionFactor(tunit))

      call set_wave_parameters(initialization)

      call retrieve_rainfall(time_in_seconds)

      if (ncdamsg > 0) then
         call get_timespace_value_by_item_array_consider_success_value(item_damlevel, zcdam, time_in_seconds)
      end if

      if (ncgensg > 0) then
         call get_timespace_value_by_item_array_consider_success_value(item_generalstructure, zcgen, time_in_seconds)
         call update_zcgen_widths_and_heights() ! TODO: replace by Jan's LineStructure from channel_flow
      end if

      if (npumpsg > 0) then
         call get_timespace_value_by_item_array_consider_success_value(item_pump, qpump, time_in_seconds)
      end if

      call update_network_data(time_in_seconds)

      if (nlongculverts > 0) then
         call get_timespace_value_by_item_and_consider_success_value(item_longculvert_valve_relative_opening, time_in_seconds)
      end if

      if (nvalv > 0) then
         call get_timespace_value_by_item_and_consider_success_value(item_valve1D, time_in_seconds)
      end if

      if (jatidep > 0 .or. jaselfal > 0) then
         call flow_settidepotential(time_in_seconds / 60d0)
      end if

      if (numlatsg > 0) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, irefdate, tzone, tunit, time_in_seconds) ! 'lateral(_)discharge'
      end if

      !Pump with levels, outside OpenMP region
      if (nPumpsWithLevels > 0) then
         call update_pumps_with_levels()
      end if

      if (numsrc > 0) then
         ! qstss must be an argument when calling ec_gettimespacevalue.
         ! It might be reallocated after initialization (when coupled to Cosumo).
         success = success .and. ec_gettimespacevalue(ecInstancePtr, item_discharge_salinity_temperature_sorsin, irefdate, tzone, tunit, time_in_seconds, qstss)
      end if

      if (jasubsupl > 0) then
         call update_subsidence_and_uplift_data(time_in_seconds)
      end if

      if (nearfield_mode == NEARFIELD_UPDATED) then
         call addNearfieldData()
      end if

      call timstop(handle_ext)

      if (.not. success) then
         iresult = DFM_EXTFORCERROR
         call print_error_message(time_in_seconds)
         return
      end if

      if (jatem > 1 .and. jaheat_eachstep == 0) then
         call heatu(time_in_seconds / 3600d0)
      end if

      if (bfm_included .and. .not. initialization) then
         if (bfmpar%lfbedfrm) then
            call fm_calbf() ! JRE+BJ to check: see with which timestep we update this?
         end if
      end if

      if (bfmpar%lfbedfrmrou .and. .not. initialization) then ! .true. if van rijn 2004 or trachy contains ripple roughness
         call fm_calksc()
      end if

      if ((jacali == 1) .and. initialization) then
         ! Make backup of roughness factor after initialisation of frcu
         call calibration_backup_frcu()
      end if

      if (jatrt == 1) then
         if (flow_trachy_needs_update(time1)) then
            call flow_trachyupdate() ! perform a trachy update step
            l_set_frcu_mor = .true.
         end if
      end if

      if (jacali == 1) then
         ! update calibration definitions and factors on links
         call calibration_update()
         l_set_frcu_mor = .true.
      end if

      if (stm_included) then
         if ((jased > 0) .and. l_set_frcu_mor) then
            call set_frcu_mor(1) !otherwise frcu_mor is set in getprof_1d()
            call set_frcu_mor(2)
         end if
      end if

      ! Update nudging temperature (and salinity)
      if (item_nudge_tem /= ec_undef_int .and. janudge > 0) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, item_nudge_tem, irefdate, tzone, tunit, time_in_seconds)
      end if

      iresult = DFM_NOERR

   end subroutine set_external_forcings

!> get_timespace_value_by_item_and_array_and_consider_success_value
   subroutine get_timespace_value_by_item_array_consider_success_value(item, array, time_in_seconds)

      integer, intent(in) :: item !< Item for getting values
      double precision, intent(inout) :: array(:) !< Array that stores the values
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      success = success .and. ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

   end subroutine get_timespace_value_by_item_array_consider_success_value

!> set_temperature_models
   subroutine set_temperature_models(time_in_seconds)
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      logical :: foundtempforcing

      ! Update arrays rhum, tair and clou in a single method call.
      ! Nothing happens in case quantity 'humidity_airtemperature_cloudiness' has never been added through ec_addtimespacerelation.
      select case (itempforcingtyp)
      case (HUMIDITY_AIRTEMPERATURE_CLOUDINESS)
         call get_timespace_value_by_name_and_consider_success_value('humidity_airtemperature_cloudiness', time_in_seconds)
      case (HUMIDITY_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION)
         call get_timespace_value_by_name_and_consider_success_value('humidity_airtemperature_cloudiness_solarradiation', time_in_seconds)
      case (DEWPOINT_AIRTEMPERATURE_CLOUDINESS)
         call get_timespace_value_by_name_and_consider_success_value('dewpoint_airtemperature_cloudiness', time_in_seconds)
      case (DEWPOINT_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION)
         call get_timespace_value_by_name_and_consider_success_value('dewpoint_airtemperature_cloudiness_solarradiation', time_in_seconds)
      case (DEWPOINT)
         call get_timespace_value_by_name_and_consider_success_value('dewpoint', time_in_seconds)
      end select

      foundtempforcing = (itempforcingtyp >= 1 .and. itempforcingtyp <= 4)

      if (btempforcingtypH) then
         call get_timespace_value_by_item_and_consider_success_value(item_humidity, time_in_seconds)
         foundtempforcing = .true.
      end if
      if (btempforcingtypA) then
         call get_timespace_value_by_item_and_consider_success_value(item_airtemperature, time_in_seconds)
         foundtempforcing = .true.
      end if
      if (btempforcingtypS) then
         call get_timespace_value_by_item_and_consider_success_value(item_solarradiation, time_in_seconds)
         foundtempforcing = .true.
      end if
      if (btempforcingtypC) then
         call get_timespace_value_by_item_and_consider_success_value(item_cloudiness, time_in_seconds)
         foundtempforcing = .true.
      end if
      if (btempforcingtypL) then
         call get_timespace_value_by_item_and_consider_success_value(item_longwaveradiation, time_in_seconds)
         foundtempforcing = .true.
      end if

      if (.not. foundtempforcing) then
         call mess(LEVEL_WARN, &
                   'No humidity, airtemperature, cloudiness and solar radiation forcing found, setting temperature model [physics:Temperature] = 1 (Only transport)')
         jatem = 1
      end if

   end subroutine set_temperature_models

!> get_timespace_value_by_name_and_consider_success_value
   subroutine get_timespace_value_by_name_and_consider_success_value(name, time_in_seconds)
      character(*), intent(in) :: name
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      success = success .and. ec_gettimespacevalue(ecInstancePtr, name, time_in_seconds)

   end subroutine get_timespace_value_by_name_and_consider_success_value

!> get_timespace_value_by_item_and_consider_success_value
   subroutine get_timespace_value_by_item_and_consider_success_value(item, time_in_seconds)

      integer, intent(in) :: item
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      success = success .and. ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

   end subroutine get_timespace_value_by_item_and_consider_success_value

   !> get_timespace_value_by_item_and_array
   subroutine get_timespace_value_by_item_and_array(item, array, time_in_seconds)

      integer, intent(in) :: item !< Item for getting values
      double precision, intent(inout) :: array(:) !< Array that stores the values
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

   end subroutine get_timespace_value_by_item_and_array

!> get_timespace_value_by_item
   subroutine get_timespace_value_by_item(item, time_in_seconds)

      integer, intent(in) :: item !< Item for getting values
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

   end subroutine get_timespace_value_by_item

!> set_wave_parameters
   subroutine set_wave_parameters(initialization)
      use ieee_arithmetic, only: ieee_is_nan

      logical, intent(in) :: initialization !< initialization phase

      logical :: all_wave_variables !< flag indicating whether _all_ wave variables should be mirrored at the boundary

      integer :: k

      if (jawave == 3 .or. jawave == 6 .or. jawave == 7) then

         if (.not. initialization) then
            !
            if (jawave == 7 .and. waveforcing == 1) then
               !
               call set_parameters_for_radiation_stress_driven_forces()
               !
            elseif (jawave == 7 .and. waveforcing == 2) then
               !
               call set_parameters_for_dissipation_driven_forces()
               !
            elseif (jawave == 7 .and. waveforcing == 3) then
               !
               call set_parameters_for_3d_dissipation_driven_forces()
            else
               !
               call set_all_wave_parameters()

               ! NB: choose whether to keep if(.not. initialization) hidden in initialize_wave_parameters or in set_wave_parameters

               if (.not. success) then
                  !
                  ! success = .false. : Most commonly, WAVE data has not been written to the com-file yet:
                  ! - Print a warning
                  ! - Continue with the calculation
                  ! - Just try it the next timestep again
                  ! - success must be set to .true., otherwise the calculation is aborted
                  !
                  message = dumpECMessageStack(LEVEL_WARN, callback_msg)
                  success = .true.
               end if
            end if
         end if
         !
         ! Now do the check on success for non-com file situations, and error when variable is missing
         !
         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'set_external_forcings:: Offline wave coupling with waveforcing=', waveforcing, '. &
               & Error reading data from nc file.'
            call warn_flush() ! ECMessage stack is not very informative
            message = dumpECMessageStack(LEVEL_ERROR, callback_msg)
         end if

         if (jawave == 7) then
            ! If wave model and flow model do not cover each other exactly, NaN values can propagate in the flow model.
            ! Correct for this by setting values to zero
            do k = 1, ndx
               if (isnan(hwavcom(k))) then ! one check should be enough, everything is collocated
                  hwavcom(k) = 0d0
                  twavcom(k) = 0d0
                  sxwav(k) = 0d0
                  sywav(k) = 0d0
                  sbxwav(k) = 0d0
                  sbywav(k) = 0d0
                  dsurf(k) = 0d0
                  dwcap(k) = 0d0
                  mxwav(k) = 0d0
                  mywav(k) = 0d0
                  phiwav(k) = 270d0
               end if
            end do
            phiwav = convert_wave_direction_from_nautical_to_cartesian(phiwav)
         end if

         ! SWAN data used via module m_waves
         !    Data from FLOW 2 SWAN: s1 (water level), bl (bottom level), ucx (vel. x), ucy (vel. y), FlowElem_xcc, FlowElem_ycc, wx, wy
         !          NOTE: all variables defined @ cell circumcentre of unstructured grid
         !                different from Delft3D. There all variables are defined on the velocity points.
         !    Data from SWAN 2 FLOW:  wavefx, wavefy, hrms (or 0.5*sqrt(2)*hm0), rtp, tp/tps/rtp, phi (= wavedirmean), Uorb, wlen
         !          NOTE:
         !                not necessary are; tmean (Tm01), urms, wavedirpeak
         !
         ! For badly converged SWAN sums, dwcap and dsurf can be NaN. Put these to 0d0,
         ! as they cause saad errors as a result of NaNs in the turbulence model
         if (.not. flowwithoutwaves) then
            if (allocated(dsurf) .and. allocated(dwcap)) then
               if (any(ieee_is_nan(dsurf)) .or. any(ieee_is_nan(dwcap))) then
                  write (msgbuf, '(a)') 'Surface dissipation fields from SWAN contain NaN values, which have been converted to 0d0. &
                                       & Check the correctness of the wave results before running the coupling.'
                  call warn_flush() ! No error, just warning and continue
                  !
                  where (ieee_is_nan(dsurf))
                     dsurf = 0d0
                  end where
                  !
                  where (ieee_is_nan(dwcap))
                     dwcap = 0d0
                  end where
               end if
            end if

            all_wave_variables = .not. (jawave == 7 .and. waveforcing /= 3)
            call select_wave_variables_subgroup(all_wave_variables)

            ! In MPI case, partition ghost cells are filled properly already, open boundaries are not
            !
            ! velocity boundaries
            if (nbndu > 0) then
               call fill_open_boundary_cells_with_inner_values(nbndu, kbndu)
            end if
            !
            ! waterlevel boundaries
            if (nbndz > 0) then
               call fill_open_boundary_cells_with_inner_values(nbndz, kbndz)
            end if
            !
            !  normal-velocity boundaries
            if (nbndn > 0) then
               call fill_open_boundary_cells_with_inner_values(nbndn, kbndn)
            end if
            !
            !  tangential-velocity boundaries
            if (nbndt > 0) then
               call fill_open_boundary_cells_with_inner_values(nbndt, kbndt)
            end if
         end if

         if (jawave > 0) then
            ! this call  is needed for bedform updates with van Rijn 2007 (cal_bf, cal_ksc below)
            ! These subroutines need uorb, rlabda
            call compute_wave_parameters()
         end if

      end if

   end subroutine set_wave_parameters

   subroutine get_values_and_consider_jawave6(item)

      integer, intent(in) :: item

      success_copy = success
      success = success .and. ecGetValues(ecInstancePtr, item, ecTime)
      if (jawave == 6) success = success_copy

   end subroutine get_values_and_consider_jawave6

!> set wave parameters for jawave==3 (online wave coupling) and jawave==6 (SWAN data for D-WAQ)
   subroutine set_all_wave_parameters()
      ! This part must be skipped during initialization
      if (jawave == 3) then
         ! Finally the delayed external forcings can be initialized
         success = flow_initwaveforcings_runtime()
      end if

      if (allocated(hwavcom)) then
         success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
      end if
      if (allocated(twavcom)) then
         success = success .and. ecGetValues(ecInstancePtr, item_tp, ecTime)
      end if
      if (allocated(phiwav)) then
         call get_values_and_consider_jawave6(item_dir)
      end if
      if (allocated(sxwav)) then
         call get_values_and_consider_jawave6(item_fx)
      end if
      if (allocated(sywav)) then
         call get_values_and_consider_jawave6(item_fy)
      end if
      if (allocated(sbxwav)) then
         call get_values_and_consider_jawave6(item_wsbu)
      end if
      if (allocated(sbywav)) then
         call get_values_and_consider_jawave6(item_wsbv)
      end if
      if (allocated(mxwav)) then
         call get_values_and_consider_jawave6(item_mx)
      end if
      if (allocated(mywav)) then
         call get_values_and_consider_jawave6(item_my)
      end if
      if (allocated(uorbwav)) then
         call get_values_and_consider_jawave6(item_ubot)
      end if
      if (allocated(dsurf)) then
         call get_values_and_consider_jawave6(item_dissurf)
      end if
      if (allocated(dwcap)) then
         call get_values_and_consider_jawave6(item_diswcap)
      end if

   end subroutine set_all_wave_parameters

!> set wave parameters for jawave == 7 (offline wave coupling) and waveforcing == 1 (wave forces via radiation stress)
   subroutine set_parameters_for_radiation_stress_driven_forces()

      twav(:) = 0d0
      success = success .and. ecGetValues(ecInstancePtr, item_dir, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_tp, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_fx, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_fy, ecTime)
      mxwav(:) = 0d0
      mywav(:) = 0d0
      uorbwav(:) = 0d0

   end subroutine set_parameters_for_radiation_stress_driven_forces
   !> set wave parameters for jawave == 7 (offline wave coupling) and waveforcing == 2 (wave forces via total dissipation)
   subroutine set_parameters_for_dissipation_driven_forces()

      twav(:) = 0d0
      success = success .and. ecGetValues(ecInstancePtr, item_dir, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_tp, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_dir, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_distot, ecTime)
      sxwav(:) = 0d0
      sywav(:) = 0d0
      mxwav(:) = 0d0
      mywav(:) = 0d0
      uorbwav(:) = 0d0

   end subroutine set_parameters_for_dissipation_driven_forces

   !> set wave parameters for jawave == 7 (offline wave coupling) and waveforcing == 3 (wave forces via 3D dissipation distribution)
   subroutine set_parameters_for_3d_dissipation_driven_forces()

      twav(:) = 0d0
      success = success .and. ecGetValues(ecInstancePtr, item_tp, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_dir, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_fx, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_fy, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_dissurf, ecTime)
      success = success .and. ecGetValues(ecInstancePtr, item_diswcap, ecTime)
      sbxwav(:) = 0d0
      sbywav(:) = 0d0
      mxwav(:) = 0d0
      mywav(:) = 0d0
      uorbwav(:) = 0d0

   end subroutine set_parameters_for_3d_dissipation_driven_forces

!> convert wave direction [degrees] from nautical to cartesian meteorological convention
   elemental function convert_wave_direction_from_nautical_to_cartesian(nautical_wave_direction) result(cartesian_wave_direction)

      double precision, intent(in) :: nautical_wave_direction !< wave direction [degrees] in nautical  convention
      double precision :: cartesian_wave_direction !< wave direction [degrees] in cartesian convention

      double precision, parameter :: MAX_RANGE_IN_DEGREES = 360d0
      double precision, parameter :: CONVERSION_PARAMETER_IN_DEGREES = 270d0

      cartesian_wave_direction = modulo(CONVERSION_PARAMETER_IN_DEGREES - nautical_wave_direction, MAX_RANGE_IN_DEGREES)

   end function convert_wave_direction_from_nautical_to_cartesian

!> retrieve icecover
   subroutine retrieve_icecover(time_in_seconds)
      use m_fm_icecover, only: ja_icecover, ice_af, ice_h, ICECOVER_EXT
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      if (ja_icecover == ICECOVER_EXT) then
         ice_af = 0.d0
         ice_h = 0.d0
         if (item_sea_ice_area_fraction /= ec_undef_int) then
            call get_timespace_value_by_item_and_consider_success_value(item_sea_ice_area_fraction, time_in_seconds)
         end if
         if (item_sea_ice_thickness /= ec_undef_int) then
            call get_timespace_value_by_item_and_consider_success_value(item_sea_ice_thickness, time_in_seconds)
         end if
      end if

   end subroutine retrieve_icecover

!> retrieve_rainfall
   subroutine retrieve_rainfall(time_in_seconds)
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      ! Retrieve rainfall for ext-file quantity 'rainfall'.
      if (jarain > 0) then
         if (item_rainfall /= ec_undef_int) then
            success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall', time_in_seconds)
         end if
         if (item_rainfall_rate /= ec_undef_int) then
            success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall_rate', time_in_seconds)
         end if
      end if

   end subroutine retrieve_rainfall

!> update_network_data
   subroutine update_network_data(time_in_seconds)
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      logical :: success_previous

      success_previous = success

      if (network%sts%numPumps > 0) then
         call get_timespace_value_by_item(item_pump_capacity, time_in_seconds)
      end if

      if (network%sts%numCulverts > 0) then
         call get_timespace_value_by_item(item_culvert_valveOpeningHeight, time_in_seconds)
      end if

      if (network%sts%numWeirs > 0) then
         call get_timespace_value_by_item(item_weir_crestLevel, time_in_seconds)
      end if

      if (network%sts%numOrifices > 0) then
         call get_timespace_value_by_item(item_orifice_crestLevel, time_in_seconds)
         call get_timespace_value_by_item(item_orifice_gateLowerEdgeLevel, time_in_seconds)
      end if

      if (network%sts%numGeneralStructures > 0) then
         call get_timespace_value_by_item(item_general_structure_crestLevel, time_in_seconds)
         call get_timespace_value_by_item(item_general_structure_gateLowerEdgeLevel, time_in_seconds)
         call get_timespace_value_by_item(item_general_structure_crestWidth, time_in_seconds)
         call get_timespace_value_by_item(item_general_structure_gateOpeningWidth, time_in_seconds)
      end if

   end subroutine update_network_data

!> update_subsidence_and_uplift_data
   subroutine update_subsidence_and_uplift_data(time_in_seconds)
      double precision, intent(in) :: time_in_seconds !< Time in seconds

      if (.not. sdu_first) then
         ! preserve the previous 'bedrock_surface_elevation' for computing the subsidence/uplift rate
         subsupl_tp = subsupl
      end if
      if (item_subsiduplift /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'bedrock_surface_elevation', time_in_seconds)
      end if
      if (sdu_first) then
         ! preserve the first 'bedrock_surface_elevation' field as the initial field
         subsupl_tp = subsupl
         subsupl_t0 = subsupl
         sdu_first = .false.
      end if

   end subroutine update_subsidence_and_uplift_data

end submodule fm_external_forcings_update
