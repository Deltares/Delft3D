!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 
module m_external_forcings

public :: set_external_forcings

contains
    
!> set field oriented boundary conditions
subroutine set_external_forcings(time_in_seconds, initialization, iresult)
   use timers,                 only : timstrt, timstop
   use m_flowtimes
   use m_flowgeom
   use m_flow
   use m_meteo
   use m_calbedform
   use m_bedform
   use dfm_error
   use m_calibration,          only: calibration_backup_frcu
   use unstruc_channel_flow
   use time_class
   use m_longculverts
   use m_nearfield,            only : nearfield_mode, NEARFIELD_UPDATED, addNearfieldData

   implicit none

   double precision, intent(in)    :: time_in_seconds  !< Time in seconds
   logical,          intent(in)    :: initialization   !< initialization phase
   integer,          intent(out)   :: iresult          !< Integer error status: DFM_NOERR==0 if succesful.

   integer, parameter              :: HUMIDITY_AIRTEMPERATURE_CLOUDINESS = 1
   integer, parameter              :: HUMIDITY_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION = 2
   integer, parameter              :: DEWPOINT_AIRTEMPERATURE_CLOUDINESS = 3
   integer, parameter              :: DEWPOINT_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION = 4
   integer, parameter              :: DEWPOINT = 5
  
   double precision, parameter     :: SEA_LEVEL_PRESSURE = 101325d0

   integer                         :: link, i, first, last
   logical                         :: l_set_frcu_mor = .false.
   logical                         :: first_time_wind

   logical, external               :: flow_initwaveforcings_runtime, flow_trachy_needs_update
   character(len=255)              :: tmpstr
   type(c_time)                    :: ecTime         !< Time in EC-module

   ! variables for processing the pump with levels, SOBEK style
   logical                         :: success_copy

   call timstrt('External forcings', handle_ext)

   success = .true.

   if (allocated(patm)) then
      ! To prevent any pressure jumps at the boundary, set (initial) patm in interior to PavBnd.
      ! May of course be overridden later by spatially varying patm values.
      patm = PavBnd
   end if

   if (ja_airdensity > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_airdensity, airdensity)
   end if

   if (jawind == 1 .or. japatm > 0) then
      call set_wind_data()
   end if

   if (jawind > 0) then
      if (jawindspeedfac > 0) then
         where (windspeedfac /= dmiss) 
            wx = wx * windspeedfac
            wy = wy * windspeedfac
         end where
      end if
      call setwindstress()
   end if

    if (jatem > 1) then
       call set_temperature_models()
    end if

    if (ja_friction_coefficient_time_dependent > 0) then
       call set_friction_coefficient()
   end if
        
   call ecTime%set4(time_in_seconds, irefdate, tzone, ecSupportTimeUnitConversionFactor(tunit))
   
   call set_wave_parameters()

   call retrive_rainfall()

   if (ncdamsg > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_damlevel, zcdam)
   end if

   if (ncgensg > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_generalstructure, zcgen)
      call update_zcgen_widths_and_heights() ! TODO: replace by Jan's LineStructure from channel_flow
   end if

   if (npumpsg > 0) then
      call get_timespace_value_by_item_array_consider_success_value(item_pump, qpump)
   end if
   
   call update_network_data()

   if (nlongculverts > 0) then
       call get_timespace_value_by_item_and_consider_success_value(item_longculvert_valve_relative_opening)
   end if

   if (nvalv > 0) then
       call get_timespace_value_by_item_and_consider_success_value(item_valve1D)
   end if

   if (jatidep > 0 .or. jaselfal > 0) then
      call flow_settidepotential(time_in_seconds/60d0)
   end if

   if (numlatsg > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, irefdate, tzone, tunit, time_in_seconds) ! 'lateral(_)discharge'
   end if

   !Pump with levels, outside OpenMP region
   if (nPumpsWithLevels > 0) then
      call update_pumps_with_levels()
   end if

   if (numsrc > 0) then
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_discharge_salinity_temperature_sorsin, irefdate, tzone, tunit, time_in_seconds)
   end if

   if (jasubsupl > 0) then
     call update_subsidence_and_uplift_data()
   end if

   if (nearfield_mode == NEARFIELD_UPDATED) then
      call addNearfieldData()
   end if

   call timstop(handle_ext)

   if (.not. success) then
      call print_error_message()
      return
   end if

   if (jatem > 1 .and. jaheat_eachstep == 0) then
      call heatu(time_in_seconds/3600d0)
   end if

   if (bfm_included .and. .not. initialization) then
      if (bfmpar%lfbedfrm) then
          call fm_calbf()            ! JRE+BJ to check: see with which timestep we update this?
       end if
   end if

   if (bfmpar%lfbedfrmrou .and. .not. initialization)  then     ! .true. if van rijn 2004 or trachy contains ripple roughness
      call fm_calksc()
   end if

   if ((jacali == 1) .and. initialization) then
      ! Make backup of roughness factor after initialisation of frcu
      call calibration_backup_frcu()
   end if

   if (jatrt == 1) then
       if (flow_trachy_needs_update(time1)) then
           call flow_trachyupdate()                            ! perform a trachy update step
           l_set_frcu_mor = .true.
       end if
   end if

   if (jacali == 1) then
       ! update calibration definitions and factors on links
       call calibration_update()
       l_set_frcu_mor = .true.
   end if

   if (stm_included) then
       if ((jased>0) .and. l_set_frcu_mor) then
           call set_frcu_mor(1)     !otherwise frcu_mor is set in getprof_1d()
           call set_frcu_mor(2)
       end if
   end if

   ! Update nudging temperature (and salinity)
   if (item_nudge_tem /= ec_undef_int .and. janudge > 0 ) then 
      success = success .and. ec_gettimespacevalue(ecInstancePtr, item_nudge_tem, irefdate, tzone, tunit, time_in_seconds)
   end if

   iresult = DFM_NOERR
   
contains

!> set_wind_data data
subroutine set_wind_data()

    integer  :: ec_item_id

    call initialize_array_with_zero(wx)
    call initialize_array_with_zero(wy)
    call initialize_array_with_zero(wdsu_x)
    call initialize_array_with_zero(wdsu_y)
    call initialize_array_with_zero(wcharnock)
    call initialize_array_with_zero(ec_pwxwy_x)
    call initialize_array_with_zero(ec_pwxwy_y)

    first_time_wind = (id_last_wind < 0)
    if (first_time_wind) then
        first = 1
        last  = get_ec_number_of_items()
    else
        first = id_first_wind
        last  = id_last_wind
    end if
    do i = first, last
        ec_item_id = get_ec_item_id(i)
        ! Retrieve wind's x- and y-component for ext-file quantity 'windxy'.
        if (ec_item_id == item_windxy_x .and. item_windxy_y /= ec_undef_int) then
            call get_timespace_value_by_item(item_windxy_x)
        ! Retrieve wind's p-, x- and y-component for ext-file quantity 'airpressure_windx_windy'.
        else if (ec_item_id == item_apwxwy_p .and. item_apwxwy_x /= ec_undef_int .and. item_apwxwy_y /= ec_undef_int) then
            if (item_apwxwy_c /= ec_undef_int) then
               call get_timespace_value_by_name('airpressure_windx_windy_charnock')
            else
               call get_timespace_value_by_name('airpressure_windx_windy')
            end if
        ! Retrieve wind's charnock-component for ext-file quantity 'charnock'.
        else if (ec_item_id == item_charnock) then
            call get_timespace_value_by_item(item_charnock)
        ! Retrieve wind's x-component for ext-file quantity 'windx'.
        else if (ec_item_id == item_windx) then
            call get_timespace_value_by_item(item_windx)
        ! Retrieve wind's y-component for ext-file quantity 'windy'.
        else if (ec_item_id == item_windy) then
            call get_timespace_value_by_item(item_windy)
        ! Retrieve wind's p-component for ext-file quantity 'atmosphericpressure'.
        else if (ec_item_id == item_atmosphericpressure) then
            call get_timespace_value_by_item(item_atmosphericpressure)
        else
            cycle  ! avoid updating id_first_wind and id_last_wind
        end if
        if (.not. success) then
            call print_error_message()
            return
        end if
        if (first_time_wind) then
            id_first_wind = min(i, id_first_wind)
            id_last_wind  = max(i, id_last_wind)
        end if
    end do

    if (jawindstressgiven > 0) then 
        call get_timespace_value_by_item_and_array(item_stressx, wdsu_x)
        call get_timespace_value_by_item_and_array(item_stressy, wdsu_y)
    end if   
   
    if (allocated(ec_pwxwy_x) .and. allocated( ec_pwxwy_y)) then
        if (jawindstressgiven == 1) then 
            call perform_additional_spatial_interpolation(wdsu_x, wdsu_y)
        else
            call perform_additional_spatial_interpolation(wx, wy)
        end if
        if (allocated(ec_pwxwy_c)) then
            do link  = 1, lnx
               wcharnock(link) = wcharnock(link) + 0.5d0*( ec_pwxwy_c(ln(1,link)) + ec_pwxwy_c(ln(2,link)) )
            end do
        end if
    end if
    if (allocated(ec_charnock)) then
        do link  = 1, lnx
            wcharnock(link) = wcharnock(link) + 0.5d0*( ec_charnock(ln(1,link)) + ec_charnock(ln(2,link)) )
        end do
    end if
        
    if (item_atmosphericpressure /= ec_undef_int) then
        where (patm == dmiss)
            patm = SEA_LEVEL_PRESSURE
        end where
    end if

    if (jawave == 1 .or. jawave == 2 .and. .not. flowWithoutWaves) then
        call tauwavefetch(time_in_seconds)
    end if
      
end subroutine set_wind_data

!> initialize_array_with_zero
subroutine initialize_array_with_zero(array)

    double precision, allocatable, intent(inout) :: array(:)

    if (allocated(array)) then
         array(:) = 0.d0
    end if
    
end subroutine initialize_array_with_zero

!> ec_number_of_items
integer function get_ec_number_of_items()

    get_ec_number_of_items = ecInstancePtr%nItems

end function get_ec_number_of_items

!> get_ec_item_id
integer function get_ec_item_id(i)

    integer, intent(in) :: i
    
    get_ec_item_id = ecInstancePtr%ecItemsPtr(i)%ptr%id

end function get_ec_item_id

!> get_timespace_value_by_item
subroutine get_timespace_value_by_item(item)

    integer, intent(in) :: item

    success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

end subroutine get_timespace_value_by_item

!> get_timespace_value_by_name
subroutine get_timespace_value_by_name(name)

    character(*), intent(in) :: name

    success = ec_gettimespacevalue(ecInstancePtr, name, time_in_seconds)
    
end subroutine get_timespace_value_by_name

!> get_timespace_value_by_item_and_array
subroutine get_timespace_value_by_item_and_array(item, array)

    integer, intent(in) :: item
    double precision, intent(inout) :: array(:)

    success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

end subroutine get_timespace_value_by_item_and_array


!> get_timespace_value_by_item_and_array_and_consider_success_value
subroutine get_timespace_value_by_item_array_consider_success_value(item, array)

    integer, intent(in) :: item
    double precision, intent(inout) :: array(:)

    success = success .and. ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

end subroutine get_timespace_value_by_item_array_consider_success_value

subroutine perform_additional_spatial_interpolation(array_x, array_y)

    double precision, intent(inout) :: array_x(:)
    double precision, intent(inout) :: array_y(:)
    
    do link  = 1, lnx
        array_x(link) = array_x(link) + 0.5d0*( ec_pwxwy_x(ln(1,link)) + ec_pwxwy_x(ln(2,link)) )
        array_y(link) = array_y(link) + 0.5d0*( ec_pwxwy_y(ln(1,link)) + ec_pwxwy_y(ln(2,link)) )
    end do

end subroutine perform_additional_spatial_interpolation
               

!> set_temperature_models
subroutine set_temperature_models()

    logical :: foundtempforcing

    ! Update arrays rhum, tair and clou in a single method call.
    ! Nothing happens in case quantity 'humidity_airtemperature_cloudiness' has never been added through ec_addtimespacerelation.
    select case (itempforcingtyp)
    case (HUMIDITY_AIRTEMPERATURE_CLOUDINESS)
        call get_timespace_value_by_name_and_consider_success_value('humidity_airtemperature_cloudiness')
    case (HUMIDITY_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION)
        call get_timespace_value_by_name_and_consider_success_value('humidity_airtemperature_cloudiness_solarradiation')
    case (DEWPOINT_AIRTEMPERATURE_CLOUDINESS)
        call get_timespace_value_by_name_and_consider_success_value('dewpoint_airtemperature_cloudiness')
    case (DEWPOINT_AIRTEMPERATURE_CLOUDINESS_SOLARRADIATION)
        call get_timespace_value_by_name_and_consider_success_value('dewpoint_airtemperature_cloudiness_solarradiation')
    case (DEWPOINT)
        call get_timespace_value_by_name_and_consider_success_value('dewpoint')
    end select

    foundtempforcing = (itempforcingtyp >= 1 .and. itempforcingtyp <= 4)
    
    if (btempforcingtypH) then
        call get_timespace_value_by_item_and_consider_success_value(item_humidity)
        foundtempforcing = .true.
    end if
    if (btempforcingtypA) then
        call get_timespace_value_by_item_and_consider_success_value(item_airtemperature)
        foundtempforcing = .true.
    end if
    if (btempforcingtypS) then
        call get_timespace_value_by_item_and_consider_success_value(item_solarradiation)
        foundtempforcing = .true.
    end if
    if (btempforcingtypC) then
        call get_timespace_value_by_item_and_consider_success_value(item_cloudiness)
        foundtempforcing = .true.
    end if
    if (btempforcingtypL) then
        call get_timespace_value_by_item_and_consider_success_value(item_longwaveradiation)
        foundtempforcing = .true.
    end if

    if (.not. foundtempforcing ) then
        call mess(LEVEL_WARN,&
    'No humidity, airtemperature, cloudiness and solar radiation forcing found, setting temperature model [physics:Temperature] = 1 (Only transport)')
        jatem = 1
    end if
    	   
end subroutine set_temperature_models

!> set friction coeffciient values at this time moment
subroutine set_friction_coefficient()

   call get_timespace_value_by_item_and_array(item_frcu, frcu)

end subroutine set_friction_coefficient

!> get_timespace_value_by_name_and_consider_success_value
subroutine get_timespace_value_by_name_and_consider_success_value(name)

    character(*), intent(in) :: name
    
    success = success .and. ec_gettimespacevalue(ecInstancePtr, name, time_in_seconds)
    
end subroutine get_timespace_value_by_name_and_consider_success_value

!> get_timespace_value_by_item_and_consider_success_value
subroutine get_timespace_value_by_item_and_consider_success_value(item)

    integer, intent(in) :: item

    success = success .and. ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

end subroutine get_timespace_value_by_item_and_consider_success_value

!> set_wave_parameters
subroutine set_wave_parameters()

   if (jawave == 3 .or. jawave == 6) then
      !
      ! This part must be skipped during initialization
      if (.not. initialization) then
         if (jawave == 3) then
            ! Finally the delayed external forcings can be initialized
            success = flow_initwaveforcings_runtime()
         end if
         if (allocated (hwavcom) ) then
            success = success .and. ecGetValues(ecInstancePtr, item_hrms, ecTime)
         end if
         if (allocated (twav) ) then
            success = success .and. ecGetValues(ecInstancePtr, item_tp, ecTime)
         end if
         if (allocated (phiwav) ) then
             call get_values_and_consider_jawave6(item_dir)
         end if
         if (allocated (sxwav) ) then
            call get_values_and_consider_jawave6(item_fx)
         end if
         if (allocated (sywav) ) then
             call get_values_and_consider_jawave6(item_fy)
         end if
         if (allocated (sbxwav) ) then
            call get_values_and_consider_jawave6(item_wsbu)
         end if
         if (allocated (sbywav) ) then
            call get_values_and_consider_jawave6(item_wsbv)
         end if
         if (allocated (mxwav) ) then
            call get_values_and_consider_jawave6(item_mx)
         end if
         if (allocated (mywav) ) then
            call get_values_and_consider_jawave6(item_my)
         end if
         if (allocated (dsurf) ) then
            call get_values_and_consider_jawave6(item_dissurf)
         end if
         if (allocated (dwcap) ) then
            call get_values_and_consider_jawave6(item_diswcap)
         end if
         if (allocated (uorbwav) ) then
            call get_values_and_consider_jawave6(item_ubot)
         end if
      end if
      if (.not. success) then
         !
         ! success = .false. : Most commonly, WAVE data has not been written to the com-file yet:
         ! - Print a warning
         ! - Continue with the calculation
         ! - Just try it the next timestep again
         ! - success must be set to .true., otherwise the calculation is aborted
         !
         message = dumpECMessageStack(LEVEL_WARN,callback_msg)
         success = .true.
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
         if (any(isnan(dsurf)) .or. any(isnan(dwcap))) then
            write (msgbuf, '(a)') 'Surface dissipation fields from SWAN contain NaN values, which have been converted to 0d0. &
                                 & Check the correctness of the wave results before running the coupling.'
            call warn_flush() ! No error, just warning and continue
            !
            where (isnan(dsurf))
               dsurf = 0d0
            end where
            !
            where (isnan(dwcap))
               dwcap = 0d0
            end where
         end if
         
         ! In MPI case, partition ghost cells are filled properly already, open boundaires are not
         call fill_open_boundary_cells_with_innner_values(nbndu, kbndu)
         !
         ! waterlevels
         call fill_open_boundary_cells_with_innner_values(nbndz, kbndz)
         !
         !  normal-velocity boundaries
         call fill_open_boundary_cells_with_innner_values(nbndn, kbndn)
         !
         !  tangential-velocity boundaries
         call fill_open_boundary_cells_with_innner_values(nbndt, kbndt)
      end if

      if (jawave>0) then
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
            

!> fill_open_boundary_cells_with_innner_values
subroutine fill_open_boundary_cells_with_innner_values(number_of_points, references)

    integer, intent(in) :: number_of_points
    integer, intent(in) :: references(:,:)
    
    integer             :: point, kb, ki 

    do point = 1, number_of_points
        kb   = references(1,point)
        ki   = references(2,point)
        hwavcom(kb) = hwavcom(ki)
        twav(kb)    = twav(ki)
        phiwav(kb)  = phiwav(ki)
        uorbwav(kb) = uorbwav(ki)
        sbxwav(kb)  = sbxwav(ki)
        sbywav(kb)  = sbywav(ki)
        sxwav(kb)   = sxwav(ki)
        sywav(kb)   = sywav(ki)
        dsurf(kb)   = dsurf(ki)
        dwcap(kb)   = dwcap(ki)
        mxwav(kb)   = mxwav(ki)
        mywav(kb)   = mywav(ki)
    end do
         
end subroutine fill_open_boundary_cells_with_innner_values

!> retrive_rainfall
subroutine retrive_rainfall()

   ! Retrieve rainfall for ext-file quantity 'rainfall'.
   if (jarain > 0) then
      if (item_rainfall /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall', time_in_seconds)
      end if
      if (item_rainfall_rate /= ec_undef_int) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, 'rainfall_rate', time_in_seconds)
      end if
   end if

end subroutine retrive_rainfall

!> update_network_data
subroutine update_network_data()

   logical :: success_previous
    
   success_previous = success
    
   if (network%sts%numPumps > 0) then
      call get_timespace_value_by_item(item_pump_capacity)
   end if

   if (network%sts%numCulverts > 0) then
      call get_timespace_value_by_item(item_culvert_valveOpeningHeight)
   end if

   if (network%sts%numWeirs > 0) then
      call get_timespace_value_by_item(item_weir_crestLevel)
   end if

   if (network%sts%numOrifices > 0) then
      call get_timespace_value_by_item(item_orifice_crestLevel)
      call get_timespace_value_by_item(item_orifice_gateLowerEdgeLevel)
   end if

   if (network%sts%numGeneralStructures > 0) then
      call get_timespace_value_by_item(item_general_structure_crestLevel)
      call get_timespace_value_by_item(item_general_structure_gateLowerEdgeLevel)
      call get_timespace_value_by_item(item_general_structure_crestWidth)
      call get_timespace_value_by_item(item_general_structure_gateOpeningWidth)
   end if
   
end subroutine update_network_data

!> update_subsidence_and_uplift_data
subroutine update_subsidence_and_uplift_data()

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
         sdu_first  = .false.
      end if
    
end subroutine update_subsidence_and_uplift_data

!> print_error_message
subroutine print_error_message()

   iresult = DFM_EXTFORCERROR
   write(tmpstr,'(f22.11)') time_in_seconds
   call mess(LEVEL_WARN, 'Error while updating meteo/structure forcing at time=' // trim(tmpstr))
   tmpstr = dumpECMessageStack(LEVEL_WARN,callback_msg)
   
end subroutine print_error_message
   
end subroutine set_external_forcings

end module m_external_forcings
