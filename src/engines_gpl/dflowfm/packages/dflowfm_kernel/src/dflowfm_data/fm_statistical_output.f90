!> Module for the statistical output with FM specific data and functions
module fm_statistical_output
   use m_output_config
   use m_statistical_output
   use messagehandling
   implicit none
   
private
   
   type(t_output_quantity_config_set), public :: out_quan_conf_his
   type(t_output_quantity_config_set), public :: out_quan_conf_map
   type(t_output_quantity_config_set), public :: out_quan_conf_clm

   type(t_output_variable_set), allocatable, public :: out_variable_set_his
   type(t_output_variable_set), allocatable, public :: out_variable_set_map
   type(t_output_variable_set), allocatable, public :: out_variable_set_clm
   
   double precision, dimension(:), allocatable, target, public :: constit_crs_obs_data !< constituent data on observation cross sections to be written
   
   public default_fm_statistical_output, aggregate_constit_crs_obs_data

   contains
   
   !> Aggregate constituent observation crossection data from crs()% value arrays into statistical_output data array.
   subroutine aggregate_constit_crs_obs_data(data_pointer)
   use m_monitoring_crosssections
   use m_transport, only: ISED1, NUMCONST_MDU, ISEDN
   use m_sediment, only: sedtot2sedsus, stmpar
   double precision, pointer, dimension(:), intent(inout) :: data_pointer  !< pointer to constit_crs_obs_data

   integer :: i, IP, num, l
   double precision :: rhol
   
   if (ncrs > 0) then
      if (.not. allocated(constit_crs_obs_data)) then
         allocate(constit_crs_obs_data(NUMCONST_MDU*ncrs))
      endif
      if (.not. associated(data_pointer))then
         data_pointer => constit_crs_obs_data
      endif
      
      do i=1,ncrs
         IP = IPNT_HUA
         do num = 1,NUMCONST_MDU
            IP = IP + 1
            if (num >= ISED1 .and. num <= ISEDN) then
               l = sedtot2sedsus(num-ISED1+1)
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               constit_crs_obs_data((i-1)*NUMCONST_MDU+num) = crs(i)%sumvalcur(IP)/rhol
            else
               constit_crs_obs_data((i-1)*NUMCONST_MDU+num) = crs(i)%sumvalcur(IP)
            endif
         end do
      enddo
   endif

   end subroutine

   !> set all possible statistical quantity items
   subroutine default_fm_statistical_output()
      use m_ug_nc_attribute, only: ug_nc_attribute
      use netcdf_utils
      use m_missing

      out_quan_conf_his%count = 0
      out_quan_conf_map%count = 0
      out_quan_conf_clm%count = 0

      !
      ! HIS: Mass balances
      !
      call addoutval(out_quan_conf_his, IDX_HIS_VOLTOT,                                             &
                     'Wrihis_balance', 'total_volume', '', '', 'TJ', UNC_LOC_WB, 'Write mass balance totals to his file')                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_STOR,                                               &
                     'Wrihis_balance', 'storage', '', '', 'TJ', UNC_LOC_WB)                
      call addoutval(out_quan_conf_his, IDX_HIS_VOLERR,                                             &
                     'Wrihis_balance', 'volume_error', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_BNDIN,                                             &
                     'Wrihis_balance', 'boundaries_in', '', '', 'TJ', UNC_LOC_WB)                                                       
      call addoutval(out_quan_conf_his, IDX_HIS_BNDOUT,                                            &
                     'Wrihis_balance', 'boundaries_out', '', '', 'TJ', UNC_LOC_WB)                                                       
      call addoutval(out_quan_conf_his, IDX_HIS_BNDTOT,                                            &
                     'Wrihis_balance', 'boundaries_total', '', '', 'TJ', UNC_LOC_WB)                                                          
      call addoutval(out_quan_conf_his, IDX_HIS_EXCHIN,                                             &
                     'Wrihis_balance', 'exchange_with_1D_in', '', '', 'TJ', UNC_LOC_WB)                                                       
      call addoutval(out_quan_conf_his, IDX_HIS_EXCHOUT,                                            &
                     'Wrihis_balance', 'exchange_with_1D_out', '', '', 'TJ', UNC_LOC_WB)                                                       
      call addoutval(out_quan_conf_his, IDX_HIS_EXCHTOT,                                            &
                     'Wrihis_balance', 'exchange_with_1D_total', '', '', 'TJ', UNC_LOC_WB)                                                          
      call addoutval(out_quan_conf_his, IDX_HIS_PRECIP_TOTAL,                                       &
                     'Wrihis_balance', 'precipitation_total', '', '', 'TJ', UNC_LOC_WB)                                                       
      call addoutval(out_quan_conf_his, IDX_HIS_EVAP,                                               &
                     'Wrihis_balance', 'evaporation', '', '', 'TJ', UNC_LOC_WB)                                  
      call addoutval(out_quan_conf_his, IDX_HIS_SOUR,                                               &
                     'Wrihis_balance', 'source_sink', '', '', 'TJ', UNC_LOC_WB)                                        
      call addoutval(out_quan_conf_his, IDX_HIS_InternalTidesDissipation,                           &
                     'Wrihis_balance', 'InternalTidesDissipation', '', '', 'TJ', UNC_LOC_WB)                                                             
      call addoutval(out_quan_conf_his, IDX_HIS_GravInput,                                          &
                     'Wrihis_balance', 'Gravitational_Input', '', '', 'TJ', UNC_LOC_WB)                                                       
      call addoutval(out_quan_conf_his, IDX_HIS_SalInput,                                           &
                     'Wrihis_balance', 'SAL_Input', '', '', 'TJ', UNC_LOC_WB)                                  
      call addoutval(out_quan_conf_his, IDX_HIS_SalInput2,                                          &
                     'Wrihis_balance', 'SAL_Input_2', '', '', 'TJ', UNC_LOC_WB)
      call addoutval(out_quan_conf_his, IDX_HIS_GRWIN,                                              &
                     'Wrihis_balance', 'groundwater_in', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_GRWOUT,                                             &
                     'Wrihis_balance', 'groundwater_out', '', '', 'TJ', UNC_LOC_WB)                                                    
      call addoutval(out_quan_conf_his, IDX_HIS_GRWTOT,                                             &
                     'Wrihis_balance', 'groundwater_total', '', '', 'TJ', UNC_LOC_WB)                                                    
      call addoutval(out_quan_conf_his, IDX_HIS_LATIN,                                              &
                     'Wrihis_balance', 'laterals_in', '', '', 'TJ', UNC_LOC_WB)
      call addoutval(out_quan_conf_his, IDX_HIS_LATOUT,                                             &
                     'Wrihis_balance', 'laterals_out', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_LATTOT,                                             &
                     'Wrihis_balance', 'laterals_total', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_LATIN1D,                                            &
                     'Wrihis_balance', 'laterals_in_1D', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_LATOUT1D,                                           &
                     'Wrihis_balance', 'laterals_out_1D', '', '', 'TJ', UNC_LOC_WB)                                                    
      call addoutval(out_quan_conf_his, IDX_HIS_LATTOT1D,                                           &
                     'Wrihis_balance', 'laterals_total_1D', '', '', 'TJ', UNC_LOC_WB)                                                    
      call addoutval(out_quan_conf_his, IDX_HIS_LATIN2D,                                            &
                     'Wrihis_balance', 'laterals_in_2D', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_LATOUT2D,                                           &
                     'Wrihis_balance', 'laterals_out_2D', '', '', 'TJ', UNC_LOC_WB)                                                    
      call addoutval(out_quan_conf_his, IDX_HIS_LATTOT2D,                                           &
                     'Wrihis_balance', 'laterals_total_2D', '', '', 'TJ', UNC_LOC_WB)                                                    
      call addoutval(out_quan_conf_his, IDX_HIS_EXTIN,                                              &
                     'Wrihis_balance', 'Qext_in', '', '', 'TJ', UNC_LOC_WB)                                     
      call addoutval(out_quan_conf_his, IDX_HIS_EXTOUT,                                             &
                     'Wrihis_balance', 'Qext_out', '', '', 'TJ', UNC_LOC_WB)                                        
      call addoutval(out_quan_conf_his, IDX_HIS_EXTTOT,                                             &
                     'Wrihis_balance', 'Qext_total', '', '', 'TJ', UNC_LOC_WB)                                              
      call addoutval(out_quan_conf_his, IDX_HIS_EXTIN1D,                                            &
                     'Wrihis_balance', 'Qext_in_1D', '', '', 'TJ', UNC_LOC_WB)                                        
      call addoutval(out_quan_conf_his, IDX_HIS_EXTOUT1D,                                           &
                     'Wrihis_balance', 'Qext_out_1D', '', '', 'TJ', UNC_LOC_WB)                                     
      call addoutval(out_quan_conf_his, IDX_HIS_EXTTOT1D,                                           &
                     'Wrihis_balance', 'Qext_total_1D', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_EXTIN2D,                                            &
                     'Wrihis_balance', 'Qext_in_2D', '', '', 'TJ', UNC_LOC_WB)                                                    
      call addoutval(out_quan_conf_his, IDX_HIS_EXTOUT2D,                                           &
                     'Wrihis_balance', 'Qext_out_2D', '', '', 'TJ', UNC_LOC_WB)                                           
      call addoutval(out_quan_conf_his, IDX_HIS_EXTTOT2D,                                           &
                     'Wrihis_balance', 'Qext_total_2D', '', '', 'TJ', UNC_LOC_WB)                                                 
      call addoutval(out_quan_conf_his, IDX_HIS_ICEPT,                                              &
                     'Wrihis_balance', 'total_volume_interception', '', '', 'TJ', UNC_LOC_WB)                                                             
      call addoutval(out_quan_conf_his, IDX_HIS_EVAP_ICEPT,                                         &
                     'Wrihis_balance', 'evaporation_interception', '', '', 'TJ', UNC_LOC_WB)                                                             
      call addoutval(out_quan_conf_his, IDX_HIS_PRECIP_GROUND,                                      &
                     'Wrihis_balance', 'precipitation_on_ground', '', '', 'TJ', UNC_LOC_WB)                                                          
  
      !
      ! HIS: source sinks
      !
      call addoutval(out_quan_conf_his, IDX_HIS_SOURCE_SINK_PRESCRIBED_DISCHARGE,                   &
                     'Wrihis_sourcesink', 'source_sink_prescribed_discharge', '', '',                     &
                     'm3 s-1', UNC_LOC_SOSI, 'Write sources-sinks statistics to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_SOURCE_SINK_PRESCRIBED_SALINITY_INCREMENT,          &
                     'Wrihis_sourcesink', 'source_sink_prescribed_salinity_increment', '', '',                     &
                     '1e-3', UNC_LOC_SOSI   )
      call addoutval(out_quan_conf_his, IDX_HIS_SOURCE_SINK_PRESCRIBED_TEMPERATURE_INCREMENT,       &
                     'Wrihis_sourcesink', 'source_sink_prescribed_temperature_increment', '', '',                     &
                     'degC', UNC_LOC_SOSI   )
      call addoutval(out_quan_conf_his, IDX_HIS_SOURCE_SINK_CURRENT_DISCHARGE,                      &
                     'Wrihis_sourcesink', 'source_sink_current_discharge', '', '',                     &
                     'm3 s-1', UNC_LOC_SOSI   )
      call addoutval(out_quan_conf_his, IDX_HIS_SOURCE_SINK_CUMULATIVE_VOLUME,                      &
                     'Wrihis_sourcesink', 'source_sink_cumulative_volume', '', '',                     &
                     'm3', UNC_LOC_SOSI   )
      call addoutval(out_quan_conf_his, IDX_HIS_SOURCE_SINK_DISCHARGE_AVERAGE ,                     &
                     'Wrihis_sourcesink', 'source_sink_discharge_average' , '', '',                     &
                     'm3 s-1', UNC_LOC_SOSI   )

      !
      ! HIS: hydraulic structures
      !
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE,                        &
                     'Wrihis_structure_gen', 'general_structure_discharge', 'Total discharge through general structure', '',                     &
                     'm3 s-1', UNC_LOC_GENSTRU, 'Write general structure parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_CREST_LEVEL,                      &
                     'Wrihis_structure_gen', 'general_structure_crest_level', 'Crest level of general structure', '',                     &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_GATE_LOWER_EDGE_LEVEL,            &
                     'Wrihis_structure_gen', 'general_structure_gate_lower_edge_level', 'Gate lower edge level of general structure', '',                     &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_WIDTH,               &
                     'Wrihis_structure_gen', 'general_structure_gate_opening_width', 'Gate opening width of general structure', '',                     &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_S1UP,                             &
                     'Wrihis_structure_gen', 'general_structure_s1up', 'Water level upstream of general structure', 'sea_surface_height',   &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_S1DN,                             &
                     'Wrihis_structure_gen', 'general_structure_s1dn', 'Water level downstream of general structure', 'sea_surface_height',   &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_HEAD,                             &
                     'Wrihis_structure_gen', 'general_structure_head', 'Head difference across general structure', '',                     &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA,                        &
                     'Wrihis_structure_gen', 'general_structure_flow_area', 'Flow area at general structure', '',                     &
                     'm2', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY,                         &
                     'Wrihis_structure_gen', 'general_structure_velocity', 'Velocity through general structure', '',                     &
                     'm s-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_CREST_WIDTH,                      &
                     'Wrihis_structure_gen', 'general_structure_crest_width', 'Crest width of general structure', '',                     &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_THROUGH_GATE_OPENING,   &
                     'Wrihis_structure_gen', 'general_structure_discharge_through_gate_opening', 'Discharge through gate opening of general structure', '',                     &
                     'm3 s-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_OVER_GATE,              &
                     'Wrihis_structure_gen', 'general_structure_discharge_over_gate', 'Discharge over gate of general structure', '',                     &
                     'm3 s-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_UNDER_GATE,             &
                     'Wrihis_structure_gen', 'general_structure_discharge_under_gate', 'Discharge under gate of general structure', '',                     &
                     'm3 s-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_HEIGHT,              &
                     'Wrihis_structure_gen', 'general_structure_gate_opening_height', 'Gate opening height of general structure', '',                     &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_GATE_UPPER_EDGE_LEVEL,            &
                     'Wrihis_structure_gen', 'general_structure_gate_upper_edge_level', 'Gate upper edge level of general structure', '',                     &
                     'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY_THROUGH_GATE_OPENING,    &
                     'Wrihis_structure_gen', 'general_structure_velocity_through_gate_opening', 'Velocity through gate opening of general structure', '',                     &
                     'm s-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY_OVER_GATE,               &
                     'Wrihis_structure_gen', 'general_structure_velocity_over_gate', 'Velocity over gate of general structure', '',                     &
                     'm s-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY_UNDER_GATE,              &
                     'Wrihis_structure_gen', 'general_structure_velocity_under_gate', 'Flow area in gate opening of general structure', '',                     &
                     'm s-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_IN_GATE_OPENING,        &
                     'Wrihis_structure_gen', 'general_structure_flow_area_in_gate_opening', 'Flow area in gate opening of general structure', '',                     &
                     'm2', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_OVER_GATE,              &
                     'Wrihis_structure_gen', 'general_structure_flow_area_over_gate', 'Flow area over gate of general structure', '',                     &
                     'm2', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_UNDER_GATE,             &
                     'Wrihis_structure_gen', 'general_structure_flow_area_under_gate', 'Flow area under gate of general structure', '',                     &
                     'm2', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_STATE,                            &
                     'Wrihis_structure_gen', 'general_structure_state', 'Flow state at general structure', '',                     &
                     '-', UNC_LOC_GENSTRU)

      allocate(out_quan_conf_his%statout(IDX_HIS_GENERAL_STRUCTURE_STATE)%additional_attributes(4))
      out_quan_conf_his%statout(IDX_HIS_GENERAL_STRUCTURE_STATE)%num_additional_attributes = 4
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_GENERAL_STRUCTURE_STATE)%additional_attributes(1), 'flag_values', (/ 0, 1, 2, 3, 4 /))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_GENERAL_STRUCTURE_STATE)%additional_attributes(2), 'flag_meanings', 'no_flow weir_free weir_submerged gate_free gate_submerged')
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_GENERAL_STRUCTURE_STATE)%additional_attributes(3), 'valid_range', (/ 0, 4 /))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_GENERAL_STRUCTURE_STATE)%additional_attributes(4), '_FillValue', int(dmiss))

      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_S1_ON_CREST,                      &
                     'Wrihis_structure_gen', 'general_structure_s1_on_crest', 'Water level on crest of general structure',          &
                     '', 'm', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_GENERAL_STRUCTURE_FORCE_DIFFERENCE,                 &
                     'Wrihis_structure_gen', 'general_structure_force_difference', 'Force difference per unit at general structure',     &
                     '', 'N m-1', UNC_LOC_GENSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_CDAM_DISCHARGE,                                     &
                     'Wrihis_structure_dam', 'cdam_discharge', 'controllable dam discharge',                         &
                     '', 'm3 s-1', UNC_LOC_DAM, 'Write dam parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_CDAM_CREST_LEVEL,                                   &
                     'Wrihis_structure_dam', 'cdam_crest_level', 'controllable dam crest level',                       &
                     '', 'm', UNC_LOC_DAM)
      call addoutval(out_quan_conf_his, IDX_HIS_CDAM_S1UP,                                          &
                     'Wrihis_structure_dam', 'cdam_s1up', 'controllable dam water level up',                    &
                     'sea_surface_height', 'm', UNC_LOC_DAM)
      call addoutval(out_quan_conf_his, IDX_HIS_CDAM_S1DN,                                          &
                     'Wrihis_structure_dam', 'cdam_s1dn', 'controllable dam water level down',                  &
                     'sea_surface_height', 'm', UNC_LOC_DAM)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_STRUCTURE_DISCHARGE,                           &
                     'Wrihis_structure_pump', 'pump_structure_discharge', 'Discharge through pump',                             &
                     '', 'm3 s-1', UNC_LOC_PUMP, 'Write pump parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_CAPACITY,                                      &
                     'Wrihis_structure_pump', 'pump_capacity', 'Capacity of pump',                                   &
                     '', 'm3 s-1', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_DISCHARGE_DIR,                                 &
                     'Wrihis_structure_pump', 'pump_discharge_dir', 'Discharge of pump w.r.t. pump orientation',          &
                     '', 'm3 s-1', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_S1UP,                                          &
                     'Wrihis_structure_pump', 'pump_s1up', 'Water level upstream of pump',                       &
                     'sea_surface_height', 'm', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_S1DN,                                          &
                     'Wrihis_structure_pump', 'pump_s1dn', 'Water level downstream of pump',                     &
                     'sea_surface_height', 'm', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_STRUCTURE_HEAD,                                &
                     'Wrihis_structure_pump', 'pump_structure_head', 'Head difference across pump structure',              &
                     '', 'm', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_ACTUAL_STAGE,                                  &
                     'Wrihis_structure_pump', 'pump_actual_stage', 'Actual stage of pump',                               &
                     '', '-', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_HEAD,                                          &
                     'Wrihis_structure_pump', 'pump_head', 'Head difference in pumping direction',               &
                     '', 'm', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_REDUCTION_FACTOR,                              &
                     'Wrihis_structure_pump', 'pump_reduction_factor', 'Reduction factor of pump',                           &
                     '', '-', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_S1_DELIVERY_SIDE,                              &
                     'Wrihis_structure_pump', 'pump_s1_delivery_side', 'Water level at delivery side of pump',               &
                     'sea_surface_height', 'm', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_PUMP_S1_SUCTION_SIDE,                               &
                     'Wrihis_structure_pump', 'pump_s1_suction_side', 'Water level at suction side of pump',                &
                     'sea_surface_height', 'm', UNC_LOC_PUMP)
      call addoutval(out_quan_conf_his, IDX_HIS_GATE_DISCHARGE,                                     &
                     'Wrihis_structure_gate', 'gate_discharge', 'gate discharge',                                     &
                     '', 'm3 s-1', UNC_LOC_GATE, 'Write gate parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_GATE_LOWER_EDGE_LEVEL,                              &
                     'Wrihis_structure_gate', 'gate_lower_edge_level', 'gate lower edge level',                              &
                     '', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATE_S1UP,                                          &
                     'Wrihis_structure_gate', 'gate_s1up', 'gate water level up',                                &
                     'sea_surface_height', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATE_S1DN,                                          &
                     'Wrihis_structure_gate', 'gate_s1dn', 'gate water level down',                              &
                     'sea_surface_height', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_DISCHARGE,                                  &
                     'Wrihis_structure_gate', 'gategen_discharge', 'gate discharge (via general structure)',             &
                     '', 'm3 s-1', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_CREST_LEVEL,                                &
                     'Wrihis_structure_gate', 'gategen_crest_level', 'gate crest level (via general structure)',           &
                     '', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_CREST_WIDTH,                                &
                     'Wrihis_structure_gate', 'gategen_crest_width', 'gate crest width (via general structure)',           &
                     '', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_GATE_LOWER_EDGE_LEVEL,                      &
                     'Wrihis_structure_gate', 'gategen_gate_lower_edge_level', 'gate lower edge level (via general structure)',      &
                     '', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_FLOW_THROUGH_HEIGHT,                        &
                     'Wrihis_structure_gate', 'gategen_flow_through_height', 'gate flow through height (via general structure)',   &
                     '', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_GATE_OPENING_WIDTH,                         &
                     'Wrihis_structure_gate', 'gategen_gate_opening_width', 'gate opening width (via general structure)',         &
                     '', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_S1UP,                                       &
                     'Wrihis_structure_gate', 'gategen_s1up', 'gate water level up (via general structure)',        &
                     'sea_surface_height', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_GATEGEN_S1DN,                                       &
                     'Wrihis_structure_gate', 'gategen_s1dn', 'gate water level down (via general structure)',      &
                     'sea_surface_height', 'm', UNC_LOC_GATE)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_DISCHARGE,                                  &
                     'Wrihis_structure_weir', 'weirgen_discharge', 'Discharge through weir',                             &
                     '', 'm3 s-1', UNC_LOC_WEIRGEN, 'Write weir parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_CREST_LEVEL,                                &
                     'Wrihis_structure_weir', 'weirgen_crest_level', 'Crest level of weir',                                &
                     '', 'm', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_CREST_WIDTH,                                &
                     'Wrihis_structure_weir', 'weirgen_crest_width', 'Crest width of weir',                                &
                     '', 'm', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_S1UP,                                       &
                     'Wrihis_structure_weir', 'weirgen_s1up', 'Water level upstream of weir',                       &
                     'sea_surface_height', 'm', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_S1DN,                                       &
                     'Wrihis_structure_weir', 'weirgen_s1dn', 'Water level downstream of weir',                     &
                     'sea_surface_height', 'm', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_STRUCTURE_HEAD,                             &
                     'Wrihis_structure_weir', 'weirgen_structure_head', 'Head difference across weir',                        &
                     '', 'm', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_VELOCITY,                                   &
                     'Wrihis_structure_weir', 'weirgen_velocity', 'Velocity through weir',                              &
                     '', 'm s-1', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_FLOW_AREA,                                  &
                     'Wrihis_structure_weir', 'weirgen_flow_area', 'Flow area at weir',                                  &
                     '', 'm2', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_STATE,                                      &
                     'Wrihis_structure_weir', 'weirgen_state', 'Flow state at weir',                                 &
                     '', '-', UNC_LOC_WEIRGEN)
      
      allocate(out_quan_conf_his%statout(IDX_HIS_WEIRGEN_STATE)%additional_attributes(4))
      out_quan_conf_his%statout(IDX_HIS_WEIRGEN_STATE)%num_additional_attributes = 4
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_WEIRGEN_STATE)%additional_attributes(1), 'flag_values', (/ 0, 1, 2 /))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_WEIRGEN_STATE)%additional_attributes(2), 'flag_meanings', 'no_flow weir_free weir_submerged')
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_WEIRGEN_STATE)%additional_attributes(3), 'valid_range', (/ 0, 2 /))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_WEIRGEN_STATE)%additional_attributes(4), '_FillValue', int(dmiss))
      
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_FORCE_DIFFERENCE,                           &
                     'Wrihis_structure_weir', 'weirgen_force_difference', 'Force difference per unit width at weir', '',                      &
                     'N m-1', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_WEIRGEN_S1_ON_CREST,                                &
                     'Wrihis_structure_weir', 'weirgen_s1_on_crest', 'Water level on crest of weir', '',                      &
                     'm', UNC_LOC_WEIRGEN)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_DISCHARGE,                                  &
                     'Wrihis_structure_orifice', 'orifice_discharge', 'Discharge through orifice', '',                      &
                     'm3 s-1', UNC_LOC_ORIFICE, 'Write orifice parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_CREST_LEVEL,                                &
                     'Wrihis_structure_orifice', 'orifice_crest_level', 'Crest level of orifice', '',                      &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_CREST_WIDTH,                                &
                     'Wrihis_structure_orifice', 'orifice_crest_width', 'Crest width of orifice', '',                      &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_GATE_LOWER_EDGE_LEVEL,                      &
                     'Wrihis_structure_orifice', 'orifice_gate_lower_edge_level', 'Gate lower edge level of orifice', '',                      &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_S1UP,                                       &
                     'Wrihis_structure_orifice', 'orifice_s1up', 'Water level upstream of orifice', 'sea_surface_height',    &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_S1DN,                                       &
                     'Wrihis_structure_orifice', 'orifice_s1dn', 'Water level downstream of orifice', 'sea_surface_height',    &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_GATE_OPENING_HEIGHT,                        &
                     'Wrihis_structure_orifice', 'orifice_gate_opening_height', 'Gate opening height of orifice', '',                      &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_HEAD,                                       &
                     'Wrihis_structure_orifice', 'orifice_head', 'Head difference across orifice', '',                      &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_FLOW_AREA,                                  &
                     'Wrihis_structure_orifice', 'orifice_flow_area', 'Flow area at orifice', '',                      &
                     'm2', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_STATE,                                      &
                     'Wrihis_structure_orifice', 'orifice_state', 'Flow state at orifice', '',                      &
                     '-', UNC_LOC_ORIFICE)
      
      allocate(out_quan_conf_his%statout(IDX_HIS_ORIFICE_STATE)%additional_attributes(4))
      out_quan_conf_his%statout(IDX_HIS_ORIFICE_STATE)%num_additional_attributes = 4
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_ORIFICE_STATE)%additional_attributes(1), 'flag_values', (/ 0, 1, 2, 3, 4/))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_ORIFICE_STATE)%additional_attributes(2), 'flag_meanings', 'no_flow weir_free weir_submerged gate_free gate_submerged')
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_ORIFICE_STATE)%additional_attributes(3), 'valid_range', (/ 0, 4 /))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_ORIFICE_STATE)%additional_attributes(4), '_FillValue', int(dmiss))
      
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_S1_ON_CREST,                                &
                     'Wrihis_structure_orifice', 'orifice_s1_on_crest', 'Water level on crest of orifice', '',                     &
                     'm', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_VELOCITY,                                   &
                     'Wrihis_structure_orifice', 'orifice_velocity', 'Velocity through orifice', '',                     &
                     'm s-1', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_ORIFICE_FORCE_DIFFERENCE,                           &
                     'Wrihis_structure_orifice', 'orifice_force_difference', 'Force difference per unit width at orifice', '',                     &
                     'N m-1', UNC_LOC_ORIFICE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_DISCHARGE,                                   &
                     'Wrihis_structure_bridge', 'bridge_discharge', 'Discharge through bridge', '',                     &
                     'm3 s-1', UNC_LOC_BRIDGE, 'Write bridge parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_S1UP,                                        &
                     'Wrihis_structure_bridge', 'bridge_s1up', 'Water level upstream of bridge', 'sea_surface_height',   &
                     'm', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_S1DN,                                        &
                     'Wrihis_structure_bridge', 'bridge_s1dn', 'Water level downstream of bridge', 'sea_surface_height',   &
                     'm', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_HEAD,                                        &
                     'Wrihis_structure_bridge', 'bridge_head', 'Head difference across bridge', '',                     &
                     'm', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_FLOW_AREA,                                   &
                     'Wrihis_structure_bridge', 'bridge_flow_area', 'Flow area at bridge', '',                     &
                     'm2', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_VELOCITY,                                    &
                     'Wrihis_structure_bridge', 'bridge_velocity', 'Velocity through bridge', '',                     &
                     'm s-1', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_BLUP,                                        &
                     'Wrihis_structure_bridge', 'bridge_blup', 'Bed level at upstream of bridge', 'altitude',             &
                     'm', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_BLDN,                                        &
                     'Wrihis_structure_bridge', 'bridge_bldn', 'Bed level at downstream of bridge', 'altitude',             &
                     'm', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_BRIDGE_BL_ACTUAL,                                   &
                     'Wrihis_structure_bridge', 'bridge_bl_actual', 'Actual bed level of bridge (crest)', 'altitude',             &
                     'm', UNC_LOC_BRIDGE)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_DISCHARGE,                                  &
                     'Wrihis_structure_culvert', 'culvert_discharge', 'Discharge through culvert', '',                     &
                     'm3 s-1', UNC_LOC_CULVERT, 'Write culvert parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_CREST_LEVEL,                                &
                     'Wrihis_structure_culvert', 'culvert_crest_level', 'Crest level of culvert', '',                     &
                     'm', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_GATE_LOWER_EDGE_LEVEL,                      &
                     'Wrihis_structure_culvert', 'culvert_gate_lower_edge_level', 'Gate lower edge level of culvert', '',                     &
                     'm', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_S1UP,                                       &
                     'Wrihis_structure_culvert', 'culvert_s1up', 'Water level upstream of culvert', 'sea_surface_height',   &
                     'm', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_S1DN,                                       &
                     'Wrihis_structure_culvert', 'culvert_s1dn', 'Water level downstream of culvert', 'sea_surface_height',   &
                     'm', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_GATE_OPENING_HEIGHT,                        &
                     'Wrihis_structure_culvert', 'culvert_gate_opening_height', 'Gate opening height of culvert', '',                     &
                     'm', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_HEAD,                                       &
                     'Wrihis_structure_culvert', 'culvert_head', 'Head difference across culvert', '',                     &
                     'm', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_FLOW_AREA,                                  &
                     'Wrihis_structure_culvert', 'culvert_flow_area', 'Flow area at culvert', '',                     &
                     'm2', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_VELOCITY,                                   &
                     'Wrihis_structure_culvert', 'culvert_velocity', 'Velocity through culvert', '',                     &
                     'm s-1', UNC_LOC_CULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_CULVERT_STATE,                                      &
                     'Wrihis_structure_culvert', 'culvert_state', 'Flow state at culvert', '',                     &
                     '-', UNC_LOC_CULVERT)
      
      allocate(out_quan_conf_his%statout(IDX_HIS_CULVERT_STATE)%additional_attributes(4))
      out_quan_conf_his%statout(IDX_HIS_CULVERT_STATE)%num_additional_attributes = 4
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_CULVERT_STATE)%additional_attributes(1), 'flag_values', (/ 0, 1, 2/))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_CULVERT_STATE)%additional_attributes(2), 'flag_meanings', 'no_flow culvert_free culvert_submerged')
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_CULVERT_STATE)%additional_attributes(3), 'valid_range', (/ 0, 2 /))
      call ncu_add_att(out_quan_conf_his%statout(IDX_HIS_CULVERT_STATE)%additional_attributes(4), '_FillValue', int(dmiss))
      
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_S1UP,                                      &
                     'Wrihis_structure_damBreak', 'dambreak_s1up', 'Water level upstream of dambreak', 'sea_surface_height',   &
                     'm', UNC_LOC_DAMBREAK, 'Write dam break parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_S1DN,                                      &
                     'Wrihis_structure_damBreak', 'dambreak_s1dn', 'Water level downstream of dambreak', 'sea_surface_height',   &
                     'm', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_DISCHARGE,                                 &
                     'Wrihis_structure_damBreak', 'dambreak_discharge', 'Discharge through dambreak', '',                     &
                     'm3 s-1', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_CUMULATIVE_DISCHARGE,                      &
                     'Wrihis_structure_damBreak', 'dambreak_cumulative_discharge', 'Cumulative Discharge through dambreak', '',                     &
                     'm3 s-1', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_VELOCITY,                                  &
                     'Wrihis_structure_damBreak', 'dambreak_velocity', 'Velocity through dambreak', '',                     &
                     'm s-1', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_HEAD,                                      &
                     'Wrihis_structure_damBreak', 'dambreak_head', 'Head difference across dambreak', '',                     &
                     'm', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_FLOW_AREA,                                 &
                     'Wrihis_structure_damBreak', 'dambreak_flow_area', 'Flow area at dambreak', '',                     &
                     'm2', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_CREST_LEVEL,                               &
                     'Wrihis_structure_damBreak', 'dambreak_crest_level', 'Crest level of dambreak', '',                     &
                     'm', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_CREST_WIDTH,                               &
                     'Wrihis_structure_damBreak', 'dambreak_crest_width', 'Crest width of dambreak', '',                     &
                     'm', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_BREACH_WIDTH_TIME_DERIVATIVE,              &
                     'Wrihis_structure_damBreak', 'dambreak_breach_width_time_derivative', 'Breach width time derivative of dambreak', '',                     &
                     'm s-1', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_DAMBREAK_WATER_LEVEL_JUMP,                          &
                     'Wrihis_structure_damBreak', 'dambreak_water_level_jump', 'Breach water level jump of dambreak', '',                     &
                     'm', UNC_LOC_DAMBREAK)
      call addoutval(out_quan_conf_his, IDX_HIS_UNIWEIR_DISCHARGE,                                  &
                     'Wrihis_structure_uniWeir', 'uniweir_discharge', 'Discharge through uniweir', '',                     &
                     'm3 s-1', UNC_LOC_UNIWEIR, 'Write universal weir parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_UNIWEIR_CREST_LEVEL,                                &
                     'Wrihis_structure_uniWeir', 'uniweir_crest_level', 'Crest level of uniweir', '',                     &
                     'm', UNC_LOC_UNIWEIR)
      call addoutval(out_quan_conf_his, IDX_HIS_UNIWEIR_S1UP,                                       &
                     'Wrihis_structure_uniWeir', 'uniweir_s1up', 'Water level upstream of uniweir', 'sea_surface_height',   &
                     'm', UNC_LOC_UNIWEIR)
      call addoutval(out_quan_conf_his, IDX_HIS_UNIWEIR_S1DN,                                       &
                     'Wrihis_structure_uniWeir', 'uniweir_s1dn', 'Water level downstream of uniweir', 'sea_surface_height',   &
                     'm', UNC_LOC_UNIWEIR)
      call addoutval(out_quan_conf_his, IDX_HIS_UNIWEIR_HEAD,                                       &
                     'Wrihis_structure_uniWeir', 'uniweir_head', 'Head difference across uniweir', '',                     &
                     'm', UNC_LOC_UNIWEIR)
      call addoutval(out_quan_conf_his, IDX_HIS_UNIWEIR_FLOW_AREA,                                  &
                     'Wrihis_structure_uniWeir', 'uniweir_flow_area', 'Flow area at uniweir', '',                     &
                     'm2', UNC_LOC_UNIWEIR)
      call addoutval(out_quan_conf_his, IDX_HIS_UNIWEIR_VELOCITY,                                   &
                     'Wrihis_structure_uniWeir', 'uniweir_velocity', 'Velocity through uniweir', '',                     &
                     'm s-1', UNC_LOC_UNIWEIR)
      call addoutval(out_quan_conf_his, IDX_HIS_CMPSTRU_DISCHARGE,                                  &
                     'Wrihis_structure_compound', 'cmpstru_discharge', 'Discharge through cmpstru', '',                     &
                     'm3 s-1', UNC_LOC_CMPSTRU, 'Write compound structure parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_CMPSTRU_S1UP,                                       &
                     'Wrihis_structure_compound', 'cmpstru_s1up', 'Water level upstream of cmpstru', 'sea_surface_height',   &
                     'm', UNC_LOC_CMPSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_CMPSTRU_S1DN,                                       &
                     'Wrihis_structure_compound', 'cmpstru_s1dn', 'Water level downstream of cmpstru', 'sea_surface_height',   &
                     'm', UNC_LOC_CMPSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_CMPSTRU_HEAD,                                       &
                     'Wrihis_structure_compound', 'cmpstru_head', 'Head difference across cmpstru', '',                     &
                     'm', UNC_LOC_CMPSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_CMPSTRU_FLOW_AREA,                                  &
                     'Wrihis_structure_compound', 'cmpstru_flow_area', 'Flow area at cmpstru', '',                     &
                     'm2', UNC_LOC_CMPSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_CMPSTRU_VELOCITY,                                   &
                     'Wrihis_structure_compound', 'cmpstru_velocity', 'Velocity through cmpstru', '',                     &
                     'm s-1', UNC_LOC_CMPSTRU)
      call addoutval(out_quan_conf_his, IDX_HIS_LONGCULVERT_DISCHARGE,                              &
                     'Wrihis_structure_longculvert', 'longculvert_discharge', 'Discharge through longculvert', '',                     &
                     'm3 s-1', UNC_LOC_LONGCULVERT, 'Write long culvert parameters to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_LONGCULVERT_S1UP,                                   &
                     'Wrihis_structure_longculvert', 'longculvert_s1up', 'Water level upstream of longculvert', 'sea_surface_height',   &
                     'm', UNC_LOC_LONGCULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_LONGCULVERT_S1DN,                                   &
                     'Wrihis_structure_longculvert', 'longculvert_s1dn', 'Water level downstream of longculvert', 'sea_surface_height',   &
                     'm', UNC_LOC_LONGCULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_LONGCULVERT_HEAD,                                   &
                     'Wrihis_structure_longculvert', 'longculvert_head', 'Head difference across longculvert', '',                     &
                     'm', UNC_LOC_LONGCULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_LONGCULVERT_FLOW_AREA,                              &
                     'Wrihis_structure_longculvert', 'longculvert_flow_area', 'Flow area at longculvert', '',                     &
                     'm2', UNC_LOC_LONGCULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_LONGCULVERT_VELOCITY,                               &
                     'Wrihis_structure_longculvert', 'longculvert_velocity', 'Velocity through longculvert', '',                     &
                     'm s-1', UNC_LOC_LONGCULVERT)
      call addoutval(out_quan_conf_his, IDX_HIS_LONGCULVERT_VALVE_RELATIVE_OPENING,                         &
                     'Wrihis_structure_longculvert', 'longculvert_valve_relative_opening', 'Valve relative opening in long culvert', '',                     &
                     '1', UNC_LOC_LONGCULVERT)

      !
      ! HIS: Output on observation stations
      !
      
      ! Basic flow quantities
      call addoutval(out_quan_conf_his, IDX_HIS_WATERLEVEL,                                         &
                     'Wrihis_waterlevel_s1', 'waterlevel', 'water level', 'sea_surface_height',   &
                     'm', UNC_LOC_STATION, 'Write water level to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_BEDLEVEL,                                           &
                     'Wrihis_bedlevel', 'bedlevel', 'bottom level', '',                     &
                     'm', UNC_LOC_STATION, 'Write bed level to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_WATERDEPTH,                                         &
                     'Wrihis_waterdepth', 'waterdepth', 'water depth', '',                     &
                     'm', UNC_LOC_STATION, 'Write water depth to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_X_VELOCITY,                                         &
                     'Wrihis_velocity_vector', 'x_velocity', 'x-component of flow element center velocity vector',               &
                     'sea_water_x_velocity', 'm s-1', UNC_LOC_STATION, 'Write velocity vectors to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_Y_VELOCITY,                                         &
                     'Wrihis_velocity_vector', 'y_velocity', 'y-component of flow element center velocity vector',               &
                     'sea_water_x_velocity', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_Z_VELOCITY,                                         &
                     'Wrihis_velocity_vector', 'z_velocity', 'vertical/upward component of flow element center velocity vector', &
                     'upward_sea_water_velocity', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_DEPTH_AVERAGED_X_VELOCITY,                          &
                     'Wrihis_velocity_vector', 'depth_averaged_x_velocity', 'flow element center depth-averaged velocity vector, x-component',  &
                     'sea_water_depth-averaged_x_velocity', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_DEPTH_AVERAGED_Y_VELOCITY,                          &
                     'Wrihis_velocity_vector', 'depth_averaged_y_velocity', 'flow element center depth-averaged velocity vector, y-component',  &
                     'sea_water_depth-averaged_y_velocity', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_VELOCITY_MAGNITUDE,                                 &
                     'Wrihis_velocity', 'velocity_magnitude',                              &
                     'velocity magnitude',                                                                          &
                     'sea_water_speed', 'm s-1', UNC_LOC_STATION, 'Write velocity magnitude to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN,                        &
                     'Wrihis_velocity', 'velocity_magnitude',                              &
                     'Eulerian velocity magnitude',                                                                 &
                     'sea_water_eulerian_speed', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_DISCHARGE_MAGNITUDE,                                &
                     'Wrihis_discharge', 'discharge_magnitude',                             &
                     'average discharge magnitude',                                                                 &
                     'water_volume_transport_in_river_channel', 'm3 s-1', UNC_LOC_STATION,          &
                     'Write discharge magnitude to his file')

      ! Turbulence model
      call addoutval(out_quan_conf_his, IDX_HIS_TKE,                                                &
                     'Wrihis_turbulence', 'tke', 'turbulent kinetic energy', '',                     &
                     'm2 s-2', UNC_LOC_STATION, 'Write k, eps and vicww to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_VICWW,                                              &
                     'Wrihis_turbulence', 'vicww' , 'turbulent vertical eddy viscosity', '',                     &
                     'm2 s-1''', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_EPS,                                                &
                     'Wrihis_turbulence', 'eps', 'turbulent energy dissipation', '',                     &
                     'm2 s-3', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_TAU,                                                &
                     'Wrihis_turbulence', 'tau', 'turbulent time scale', '',                     &
                     's-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_RICH,                                               &
                     'Richardsononoutput', 'rich', 'Richardson Nr',                                                                               &
                     '', '-', UNC_LOC_STATION)

      ! Gravity + buoyancy
      call addoutval(out_quan_conf_his, IDX_HIS_SALINITY,                                           &
                     'Wrihis_salinity', 'salinity', '', 'sea_water_salinity',   &
                     '1e-3', UNC_LOC_STATION, 'Write salinity to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_TEMPERATURE,                                        &
                     'Wrihis_temperature', 'temperature', '', 'sea_water_temperature',&
                     'degC', UNC_LOC_STATION, 'Write temperature to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_POTENTIAL_DENSITY,                                  &
                     'Wrihis_density', 'potential_density', 'potential_density', '',                     &
                     'kg m-3', UNC_LOC_STATION, 'Write density to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_DENSITY,                                            &
                     'Wrihis_density', 'density', 'density', '',                     &
                     'kg m-3', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_BRUNT_VAISALA_N2,                                   &
                     'Wrihis_density', 'Brunt_Vaisala_N2', 'Brunt_Vaisala_N2', '',                     &
                     '1/s2', UNC_LOC_STATION)

      ! Wave model
      call addoutval(out_quan_conf_his, IDX_HIS_HWAV,                                               &
                     'Wrihis_waves', 'hwav', 'Significant wave height',                                          &
                     'sea_surface_wave_significant_wave_height', 'm', UNC_LOC_STATION,              &
                     'Write wave data to his file')
      ! TODO: hwav sig vs. rms
      call addoutval(out_quan_conf_his, IDX_HIS_TWAV,                                               &
                     'Wrihis_waves', 'twav', 'Wave period',                                                      &
                     'sea_surface_wave_period', 's', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_PHIWAV,                                             &
                     'Wrihis_waves', 'phiwav', 'Wave from direction',                                              &
                     'sea_surface_wave_from_direction', 'deg from N', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_RLABDA,                                             &
                     'Wrihis_waves', 'rlabda', 'Wave length',                                                      &
                     'sea_surface_wave_length', 'm', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_UORB,                                               &
                     'Wrihis_waves', 'uorb', 'Orbital velocity',                                                 &
                     'sea_surface_wave_orbital_velocity', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_USTOKES,                                            &
                     'Wrihis_waves', 'ustokes', 'Stokes drift, x-component',                                        &
                     'sea_surface_wave_stokes_drift_x', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_VSTOKES,                                            &
                     'Wrihis_waves', 'vstokes', 'Stokes drift, y-component',                                        &
                     'sea_surface_wave_stokes_drift_y', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_TAUSX,                                              &
                     'Wrihis_taucurrent', 'tausx',                                           &
                     'Mean bottom shear stress vector, x-component',                                                &
                     'Mean bottom shear stress vector, x-component', 'Pa', UNC_LOC_STATION,         &
                     'Write mean bed shear stress to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_TAUSY,                                              &
                     'Wrihis_taucurrent', 'tausy',                                           &
                     'Mean bottom shear stress vector, y-component',                                                &
                     'Mean bottom shear stress vector, y-component', 'Pa', UNC_LOC_STATION)

      ! Meteo
      call addoutval(out_quan_conf_his, IDX_HIS_WINDX,                                              &
                     'Wrihis_wind', 'windx', 'velocity of air on flow element center, x-component', 'eastward_wind',        &
                     'm s-1', UNC_LOC_STATION, 'Write wind velocities to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_WINDX_SFERIC,                                       &
                     'Wrihis_wind', 'windx', 'velocity of air on flow element center, x-component', 'x_wind',               &
                     'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_WINDY,                                              &
                     'Wrihis_wind', 'windy', 'velocity of air on flow element center, x-component', 'northward_wind',       &
                     'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_WINDY_SFERIC,                                       &
                     'Wrihis_wind', 'windy', 'velocity of air on flow element center, x-component', 'y_wind',               &
                     'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_RAIN,                                               &
                     'Wrihis_rain', 'rain', 'precipitation depth per time unit', 'lwe_precipitation_rate', &
                     'mm day-1', UNC_LOC_STATION, 'Write precipitation to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_INFILTRATION_CAP,                                   &
                     'Wrihis_infiltration', 'infiltration_cap', 'Infiltration capacity', '',                     &
                     'mm hr-1', UNC_LOC_STATION, 'Write infiltration to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL,                   &
                     'Wrihis_infiltration', 'infiltration_actual', 'Actual infiltration rate', '',                     &
                     'mm hr-1', UNC_LOC_STATION)

      ! Heat flux model
      call addoutval(out_quan_conf_his, IDX_HIS_WIND,                                               &
                     'Wrihis_heat_fluxes', 'wind', 'windspeed', '',                     &
                     'm s-1', UNC_LOC_STATION, 'Write heat fluxes to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_TAIR,                                               &
                     'Wrihis_heat_fluxes', 'Tair', 'air temperature', '',                     &
                     'degC', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_RHUM,                                               &
                     'Wrihis_heat_fluxes', 'relative humidity', '', '',                     &
                     '', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_CLOU,                                               &
                     'Wrihis_heat_fluxes', 'clou'  , 'cloudiness', '',                     &
                     ' ', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_QSUN,                                               &
                     'Wrihis_heat_fluxes', 'Qsun'  , 'solar influx', '',                     &
                     'W m-2', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_QEVA,                                               &
                     'Wrihis_heat_fluxes', 'Qeva'  , 'evaporative heat flux', '',                     &
                     'W m-2', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_QCON,                                               &
                     'Wrihis_heat_fluxes', 'Qcon'  , 'sensible heat flux', '',                     &
                     'W m-2', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_QLONG,                                              &
                     'Wrihis_heat_fluxes', 'Qlong' , 'long wave back radiation', '',                     &
                     'W m-2', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_QFREVA,                                             &
                     'Wrihis_heat_fluxes', 'Qfreva', 'free convection evaporative heat flux', '',                     &
                     'W m-2', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_QFRCON,                                             &
                     'Wrihis_heat_fluxes', 'Qfrcon', 'free convection sensible heat flux', '',                     &
                     'W m-2', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_QTOT,                                               &
                     'Wrihis_heat_fluxes', 'Qtot'  , 'total heat flux', '',                     &
                     'W m-2', UNC_LOC_STATION)

      ! Sediment model
      call addoutval(out_quan_conf_his, IDX_HIS_SED,                                                &
                     'Wrihis_sediment', 'sed', 'Sediment concentration',                                           &
                     '', 'kg m-3', UNC_LOC_STATION, 'Write sediment transport to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_WS,                                                 &
                     'Wrihis_sediment', 'ws', 'Sediment settling velocity',                                       &
                     '', 'm s-1', UNC_LOC_STATION)
      call addoutval(out_quan_conf_his, IDX_HIS_SEDDIF,                                             &
                     'Wrihis_sediment', 'seddif', 'Sediment vertical diffusion',                                      &
                     '', 'm2 s-1', UNC_LOC_STATION)

      !
      ! HIS: Variables on observation cross sections
      !
      call addoutval(out_quan_conf_his, IDX_HIS_CONSTITUENTS,                                       &
                     'Wrihis_constituents', 'constituents', '',                                                                 &
                     '', '-', UNC_LOC_STATION, 'Write tracers to his file')

      !
      ! HIS: Lateral discharges
      !
      call addoutval(out_quan_conf_his, IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS,         &
                     'Wrihis_lateral', 'lateral_prescribed_discharge_instantaneous',      &
                     'Prescribed discharge through lateral at current time step (instantaneous)',                   &
                     '', 'm3 s-1', UNC_LOC_LATERAL, 'Write lateral data to his file')
      call addoutval(out_quan_conf_his, IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE,               &
                     'Wrihis_lateral', 'lateral_prescribed_discharge_average',            &
                     'Prescribed discharge through lateral, average over the last history time interval',           &
                     '', 'm3 s-1', UNC_LOC_LATERAL)
      call addoutval(out_quan_conf_his, IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS,           &
                     'Wrihis_lateral', 'lateral_realized_discharge_instantaneous',        &
                     'Realized discharge through lateral at current time step (instantaneous)',                     &
                     '', 'm3 s-1', UNC_LOC_LATERAL)
      call addoutval(out_quan_conf_his, IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE,                 &
                     'Wrihis_lateral', 'lateral_realized_discharge_average',              &
                     'Realized discharge through lateral, average over the last history time interval',             &
                     '', 'm3 s-1', UNC_LOC_LATERAL)

      !
      ! MAP:
      !
      call addoutval(out_quan_conf_map, IDX_MAP_S0,                                                 &
                     'Wrimap_waterlevel_s0', 's0', 'Water level on previous timestep',                                                            &
                     'sea_surface_height', 'm', UNC_LOC_S, 'Write water levels for previous time step to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_S1,                                                 &
                     'Wrimap_waterlevel_s1', 's1', 'Water level',                                                                                 &
                     'sea_surface_height', 'm', UNC_LOC_S, 'Write water levels to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_POTEVAP,                                            &
                     'Wrimap_evaporation', 'potevap', 'Potential evaporation rate at pressure points',                                               &
                     'water_potential_evaporation_flux', 'm s-1', UNC_LOC_S, 'Write evaporation to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_ACTEVAP,                                            &
                     'Wrimap_evaporation', 'actevap', 'Actual evaporation rate at pressure points',                                                  &
                     'lwe_water_evaporation_rate', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_PRESCREVAP,                                         &
                     'Wrimap_evaporation', 'prescrevap', 'Prescribed evaporation rate at pressure points',                                              &
                     '', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_VOL1,                                               &
                     'Wrimap_volume1', 'vol1', 'volume of water in grid cell',                                                                &
                     '', 'm3', UNC_LOC_S, 'Write volumes to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_WATERDEPTH,                                         &
                     'Wrimap_waterdepth', 'waterdepth', 'Water depth at pressure points',                                                              &
                     'sea_floor_depth_below_sea_surface', 'm', UNC_LOC_S, 'Write water depths to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_HU,                                                 &
                     'Wrimap_waterdepth_hu', 'hu', 'water depth at velocity points',                                                              &
                     'sea_floor_depth_below_sea_surface', 'm', UNC_LOC_U, 'Write water depths on u-points to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_NEGDPT,                                             &
                     'Wrimap_flow_analysis', 'negdpt', 'Number of times negative depth was calculated',                                               &
                     '', '1', UNC_LOC_S, 'Write flow analysis data to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_NEGDPT_CUM,                                         &
                     'Wrimap_flow_analysis', 'negdpt_cum', 'Cumulative number of times negative depth was calculated',                                    &
                     '', '1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_NOITER,                                             &
                     'Wrimap_flow_analysis', 'noiter', 'Number of times no nonlinear convergence was caused',                                         &
                     '', '1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_NOITER_CUM,                                         &
                     'Wrimap_flow_analysis', 'noiter_cum', 'Cumulative number of times no nonlinear convergence was caused',                              &
                     '', '1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_LIMTSTEP,                                           &
                     'Wrimap_flow_analysis', 'limtstep', 'Number of times a node was limiting for the computational time step',                         &
                     '', '1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_LIMTSTEP_CUM,                                       &
                     'Wrimap_flow_analysis', 'limtstep_cum', 'Cumulative number of times a node was limiting for the computational time step',              &
                     '', '1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_COURANT,                                            &
                     'Wrimap_flow_analysis', 'courant', 'Courant number',                                                                              &
                     '', '1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_AU,                                                 &
                     'Wrimap_flowarea_au', 'au', 'normal flow area between two neighbouring grid cells',                                        &
                     '', 'm2', UNC_LOC_U, 'Write flow areas au to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_U1,                                                 &
                     'Wrimap_velocity_component_u1', 'u1', 'Velocity at velocity point, n-component',                                                     &
                     '', 'm s-1', UNC_LOC_U, 'Write velocity component to map file')
      
      allocate(out_quan_conf_map%statout(IDX_MAP_U1)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_U1)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_U1)%additional_attributes(1), 'comment', &
                       'Positive direction is from first to second neighbouring face (flow element).')
      
      call addoutval(out_quan_conf_map, IDX_MAP_U0, 'Wrimap_velocity_component_u0', &
                     'u0', 'Velocity at velocity pointat previous time step, n-component ', '', 'm s-1', UNC_LOC_U, &
                     'Write velocity component for previous time step to map file')
            
      allocate(out_quan_conf_map%statout(IDX_MAP_U0)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_U0)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_U0)%additional_attributes(1), 'comment', &
                       'Positive direction is from first to second neighbouring face (flow element).')
      
      call addoutval(out_quan_conf_map, IDX_MAP_UCXQ_EULERIAN,                                      &
                     'Wrimap_velocity_vector', 'ucxq', 'Flow element center eulerian velocity vector based on discharge, x-component',  &
                     'ucxq_eulerian_velocity', 'm s-1', UNC_LOC_S, 'Write cell-center velocity vectors to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_UCYQ_EULERIAN,                                      &
                     'Wrimap_velocity_vector', 'ucyq', 'Flow element center eulerian velocity vector based on discharge, y-component',  &
                     'ucyq_eulerian_velocity', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_UCXQ,                                               &
                     'Wrimap_velocity_vector', 'ucxq', 'Flow element center velocity vector based on discharge, x-component',           &
                     'ucxq_velocity', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_UCYQ,                                               &
                     'Wrimap_velocity_vector', 'ucyq', 'Flow element center velocity vector based on discharge, y-component',           &
                     'ucyq_velocity', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_UCMAG,                                              &
                     'Wrimap_velocity_magnitude', 'ucmag', 'Flow element center velocity magnitude',                                        &
                     'sea_water_speed', 'm s-1', UNC_LOC_S, 'Write cell-center velocity vector magnitude to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_UCMAG_EULER,                                        &
                     'Wrimap_velocity_magnitude', 'ucmag', 'Flow element center eulerian velocity magnitude',                               &
                     'sea_water_eulerian_speed', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_UCMAGA_GLM,                                         &
                     'Wrimap_velocity_magnitude', 'ucmaga', 'Flow element center depth-averaged GLM velocity magnitude',                     &
                     'sea_water_speed', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_UCMAGA,                                             &
                     'Wrimap_velocity_magnitude', 'ucmaga', 'Flow element center depth-averaged velocity magnitude',                         &
                     'sea_water_speed', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WW1,                                                &
                     'Wrimap_upward_velocity_component', 'ww1', 'Upward velocity on vertical interface, n-component',                            &
                     'upward_sea_water_velocity', 'm s-1', UNC_LOC_W, 'Write upward velocity component on cell interfaces')          
      call addoutval(out_quan_conf_map, IDX_MAP_RHO,                                                &
                     'Wrimap_density_rho', 'rho', 'Flow element center mass density',                                              &
                     'sea_water_density', 'kg m-3', UNC_LOC_S3D, 'Write flow density to map file')         
      call addoutval(out_quan_conf_map, IDX_MAP_VIU,                                                &
                     'Wrimap_horizontal_viscosity_viu', 'viu', 'Horizontal eddy viscosity',                                                     &
                     '', 'm2 s-1', UNC_Loc_U, 'Write horizontal viscosity to map file')         
      call addoutval(out_quan_conf_map, IDX_MAP_DIU,                                                &
                     'Wrimap_horizontal_diffusivity_diu', 'diu', 'Horizontal eddy diffusivity',                                                   &
                     '', 'm2 s-1', UNC_Loc_U, 'Write horizontal diffusivity to map file')          
      call addoutval(out_quan_conf_map, IDX_MAP_Q1,                                                 &
                     'Wrimap_flow_flux_q1', 'q1', 'Discharge through flow link at current time',                                   &
                     'discharge', 'm3 s-1', UNC_LOC_U, 'Write flow flux to map file')
            
      allocate(out_quan_conf_map%statout(IDX_MAP_Q1)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_Q1)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_Q1)%additional_attributes(1), 'comment', &
                       'Positive direction is from first to second neighbouring face (flow element).')
      
      call addoutval(out_quan_conf_map, IDX_MAP_Q1_MAIN,                                            &
                     'Wrimap_flow_flux_q1_main', 'q1_main', 'Main channel discharge through flow link at current time',                      &
                     '', 'm3 s-1', UNC_LOC_U, 'Write flow flux in main channel to map file')
            
      allocate(out_quan_conf_map%statout(IDX_MAP_Q1_MAIN)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_Q1_MAIN)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_Q1_MAIN)%additional_attributes(1), 'comment', &
                       'Positive direction is from first to second neighbouring face (flow element).')
      
      call addoutval(out_quan_conf_map, IDX_MAP_FIXED_WEIR_ENERGY_LOSS,                             &
                     'Wrimap_fixed_weir_energy_loss', 'fixed weir energy loss', 'Fixed weir energy loss',                                                        &
                     '', 'm', UNC_LOC_U, 'Write fixed weir energy loss to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_SPIRCRV,                                            &
                     'Wrimap_spiral_flow', 'spircrv', 'Flow streamline curvature',                                                     &
                     'streamline_curvature', '1/m', UNC_LOC_S, 'Write spiral flow to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_SPIRINT,                                            &
                     'Wrimap_spiral_flow', 'spirint', 'Spiral flow intensity',                                                         &
                     'spiral_intensity', 'm/s', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_NUMLIMDT,                                           &
                     'Wrimap_numlimdt', 'Numlimdt', 'Number of times flow element was Courant limiting',                             &
                     '', '1', UNC_LOC_S, 'Write the number times a cell was Courant limiting to map file. (Consider using Wrimap_flow_analysis instead.)')
      call addoutval(out_quan_conf_map, IDX_MAP_TAUSX,                                              &
                     'Wrimap_taucurrent', 'tausx', 'Total bed shear stress vector, x-component',                                    &
                     '', 'N m-2', UNC_LOC_S, 'Write the shear stress to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_TAUSY,                                              &
                     'Wrimap_taucurrent', 'tausy', 'Total bed shear stress vector, y-component',                                    &
                     '', 'N m-2', UNC_LOC_S) 
      call addoutval(out_quan_conf_map, IDX_MAP_TAUS,                                               &
                     'Wrimap_taucurrent', 'taus', 'Total bed shear stress magnitude',                                              &
                     '', 'N m-2', UNC_LOC_S) 
      call addoutval(out_quan_conf_map, IDX_MAP_TAUSMAX,                                            &
                     'Wrimap_taucurrent', 'tausmax', 'Bed shear stress magnitude for morphology',                                     &
                     '', 'N m-2', UNC_LOC_S) 
      call addoutval(out_quan_conf_map, IDX_MAP_Z0UCUR,                                             &
                     'Wrimap_z0', 'z0ucur', 'Current related roughness height',                                              &
                     '', 'm', UNC_LOC_U) 
      call addoutval(out_quan_conf_map, IDX_MAP_Z0UROU,                                             &
                     'Wrimap_salinity', 'z0urou', 'Current-wave related roughness height',                                         &
                     '', 'm', UNC_LOC_U, 'Write salinity to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_SA1,                                                &
                     'Wrimap_chezy', 'sa1', 'Salinity in flow element',                                                      &
                     'sea_water_salinity', '1e-3', UNC_LOC_S, 'Write the chezy values in flow elements to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_CZS,                                                &
                     'Wrimap_chezy_on_flow_links', 'czs', 'Chezy roughness in flow element center',                                        &
                     '', 'm0.5s-1', UNC_LOC_S, 'Write the chezy values on flow links to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_CZU,                                                &
                     'Wrimap_input_roughness', 'czu', 'Chezy roughness on flow links',                                                 &
                     '', 'm0.5s-1', UNC_LOC_U, 'Write the input roughness on flow links to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_CFU,                                                &
                     'Wrimap_input_roughness', 'cfu', 'Input roughness on flow links',                                                 &
                     '', '-', UNC_LOC_U) 
      call addoutval(out_quan_conf_map, IDX_MAP_CFUTYP,                                             &
                     'Wrimap_temperature', 'cfutyp', 'Input roughness type on flow links',                                            &
                     '', '-', UNC_LOC_U) 
      call addoutval(out_quan_conf_map, IDX_MAP_TEM1,                                               &
                     'Wrimap_constituents', 'tem1', 'Temperature in flow element',                                                   &
                     'sea_water_temperature', 'degC', UNC_LOC_S, 'Write constituents to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_CONST,                                              &
                     'Wrimap_sediment', 'const', '',                                                                              &
                     '', '-', UNC_LOC_S, 'Write sediment fractions to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_MORS,                                               &
                     'Wrimap_turbulence', 'mors', '',                                                                              &
                     '', '-', UNC_LOC_S, 'Write vicww, k and eps to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_TURKIN1,                                            &
                     'Wrimap_turbulence', 'turkin1', 'turbulent kinetic energy',                                                      &
                     'specific_turbulent_kinetic_energy_of_sea_water', 'm2 s-2', UNC_LOC_WU) 
      call addoutval(out_quan_conf_map, IDX_MAP_VICWWU,                                             &
                     'Wrimap_turbulence', 'vicwwu', 'turbulent vertical eddy viscosity',                                             &
                     'eddy_viscosity', 'm2 s-1', UNC_LOC_WU) 
      call addoutval(out_quan_conf_map, IDX_MAP_TUREPS1,                                            &
                     'Wrimap_turbulence', 'tureps1', 'turbulent energy dissipation',                                                  &
                     'specific_turbulent_kinetic_energy_dissipation_in_sea_water', 'm2 s-3', UNC_LOC_WU) 
      call addoutval(out_quan_conf_map, IDX_MAP_TUREPS1_3,                                          &
                     'Wrimap_turbulence', 'tureps1', 'turbulent energy dissipation',                                                  &
                     'specific_turbulent_kinetic_energy_dissipation_in_sea_water', 'm2 s-3', UNC_LOC_WU)
      call addoutval(out_quan_conf_map, IDX_MAP_TUREPS1_4,                                          &
                     'Wrimap_turbulence', 'tureps1', 'turbulent time scale',                                                          &
                     '', 's-1', UNC_LOC_WU)
      call addoutval(out_quan_conf_map, IDX_MAP_CFRT_0,                                             &
                     'Wrimap_trachytopes', 'cfrt', 'Chezy roughness from trachytopes',                                              &
                     '', '', UNC_LOC_L)
            
      allocate(out_quan_conf_map%statout(IDX_MAP_CFRT_0)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_CFRT_0)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_CFRT_0)%additional_attributes(1), 'non_si_units', 'm0.5s-1')

      call addoutval(out_quan_conf_map, IDX_MAP_CFRT_1,                                             &
                     'Wrimap_trachytopes', 'cfrt', 'Manning roughness from trachytopes',                                            &
                     '', '', UNC_LOC_L, 'Write trachytope roughnesses to map file')
            
      allocate(out_quan_conf_map%statout(IDX_MAP_CFRT_1)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_CFRT_1)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_CFRT_1)%additional_attributes(1), 'non_si_units', 'sm-0.333')

      call addoutval(out_quan_conf_map, IDX_MAP_CFRT_2,                                             &
                     'Wrimap_trachytopes', 'cfrt', 'White-Colebrook roughness from trachytopes',                                    &
                     '', '', UNC_LOC_L)
            
      allocate(out_quan_conf_map%statout(IDX_MAP_CFRT_2)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_CFRT_2)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_CFRT_2)%additional_attributes(1), 'non_si_units', 'm')

      call addoutval(out_quan_conf_map, IDX_MAP_CFRT,                                               &
                     'Wrimap_trachytopes', 'cfrt', 'Roughness from trachytopes',                                                    &
                     '', '', UNC_LOC_L)
      
      allocate(out_quan_conf_map%statout(IDX_MAP_CFRT)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_CFRT)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_CFRT)%additional_attributes(1), 'non_si_units', ' ')
      
      call addoutval(out_quan_conf_map, IDX_MAP_CFCL,                                               &
                     'Wrimap_calibration', 'cfcl', 'Calibration factor for roughness',                                              &
                     '', '-', UNC_LOC_L, 'Write roughness calibration factors to map file')
      
      allocate(out_quan_conf_map%statout(IDX_MAP_CFCL)%additional_attributes(1))
      out_quan_conf_map%statout(IDX_MAP_CFCL)%num_additional_attributes = 1
      call ncu_add_att(out_quan_conf_map%statout(IDX_MAP_CFCL)%additional_attributes(1), 'non_si_units', 'm0.5s-1')

      call addoutval(out_quan_conf_map, IDX_MAP_RAINFALL_RATE,                       &
                     'Wrimap_rain', 'rainfall_rate', 'Rainfall rate',                                                            &
                     'rainfall_rate', 'm s-1', UNC_LOC_S, 'Write rainfall rates to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_INTERCEPTION_WATERDEPTH,             &
                     'Wrimap_interception', 'interception_waterdepth', 'Waterdepth in interception layer',                                         &
                     '', 'm', UNC_LOC_S, 'Write interception to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_PATM,                                &
                     'Wrimap_wind', 'Patm', 'Atmospheric pressure near surface',                                        &
                     'surface_air_pressure', 'N m-2', UNC_LOC_S, 'Write wind velocities to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_WINDX,                               &
                     'Wrimap_wind', 'windx', 'velocity of air on flow element center, x-component',                      &
                     'x_wind', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDY,                               &
                     'Wrimap_wind', 'windy', 'velocity of air on flow element center, y-component',                      &
                     'y_wind', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDXU,                              &
                     'Wrimap_wind', 'windxu', 'velocity of air on flow links, x-component',                               &
                     'x_wind', 'm s-1', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDYU,                              &
                     'Wrimap_wind', 'windyu', 'velocity of air on flow links, y-component',                               &
                     'y_wind', 'm s-1', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDX_SFERIC,                        &
                     'Wrimap_wind', 'windx', 'velocity of air on flow element center, x-component',                      &
                     'eastward_wind', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDY_SFERIC,                        &
                     'Wrimap_wind', 'windy', 'velocity of air on flow element center, y-component',                      &
                     'northward_wind', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDXU_SFERIC,                       &
                     'Wrimap_wind', 'windxu', 'velocity of air on flow links, x-component',                               &
                     'eastward_wind', 'm s-1', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDYU_SFERIC,                       &
                     'Wrimap_wind', 'windyu', 'velocity of air on flow links, y-component',                               &
                     'northward_wind', 'm s-1', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDSTRESSX,                         &
                     'Wrimap_windstress', 'windstressx', 'wind stress on flow element center, x-component',                          &
                     'surface_downward_x_stress', 'N m-2', UNC_LOC_S, 'Write wind stress to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_WINDSTRESSY,                         &
                     'Wrimap_windstress', 'windstressy', 'wind stress on flow element center, y-component',                          &
                     'surface_downward_y_stress', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDSTRESSX_SFERIC,                  &
                     'Wrimap_windstress', 'windstressx', 'wind stress on flow element center, x-component',                          &
                     'surface_downward_eastward_stress', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WINDSTRESSY_SFERIC,                  &
                     'Wrimap_windstress', 'windstressy', 'wind stress on flow element center, y-component',                          &
                     'surface_downward_northward_stress', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_TAIR ,                               &
                     'Wrimap_heat_fluxes', 'Tair' , 'surface_temperature'      ,                                                &
                     'Air temperature near surface', 'degC', UNC_LOC_S, 'Write heat fluxes to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_RHUM ,                               &
                     'Wrimap_heat_fluxes', 'Rhum' , 'surface_specific_humidity',                                                &
                     'Relative humidity near surface', '', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_CLOU ,                               &
                     'Wrimap_heat_fluxes', 'Clou' , 'cloud_area_fraction'      ,                                                &
                     'Cloudiness', '1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_QSUN  ,                              &
                     'Wrimap_heat_fluxes', 'Qsun'  , 'surface_net_downward_shortwave_flux',                                      &
                     'Solar influx', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_QEVA  ,                              &
                     'Wrimap_heat_fluxes', 'Qeva'  , 'surface_downward_latent_heat_flux',                                        &
                     'Evaporative heat flux', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_QCON  ,                              &
                     'Wrimap_heat_fluxes', 'Qcon'  , 'surface_downward_sensible_heat_flux',                                      &
                     'Sensible heat flux', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_QLONG ,                              &
                     'Wrimap_heat_fluxes', 'Qlong' , 'surface_net_downward_longwave_flux',                                       &
                     'Long wave back radiation', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_QFREVA,                              &
                     'Wrimap_heat_fluxes', 'Qfreva', 'downward_latent_heat_flux_in_sea_water_due_to_convection',                 &
                     'Free convection evaporative heat flux', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_QFRCON,                              &
                     'Wrimap_heat_fluxes', 'Qfrcon', 'surface_downward_sensible_heat_flux_due_to_convection',                    &
                     'Free convection sensible heat flux', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_QTOT  ,                              &
                     'Wrimap_heat_fluxes', 'Qtot'  , 'surface_downward_heat_flux_in_sea_water',                                  &
                     'Total heat flux', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_TIDALPOTENTIAL,                      &
                     'Wrimap_tidal_potential', 'TidalPotential', 'TidalPotential',                                                           &
                     'Tidal Potential generated by celestial forces in flow element center', 'm2 s-2', UNC_LOC_S, &
                     'Write tidal potential to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_SALPOTENTIAL,                        &
                     'Wrimap_sal_potential', 'SALPotential', 'SALPotential',                                                             &
                     'Self-attraction and loading Potential in flow element center', 'm2 s-2', UNC_LOC_S, &
                     'Write self attraction and loading potential to map file') 
      call addoutval(out_quan_conf_map, IDX_MAP_INTERNAL_TIDES_DISSIPATION,          &
                     'Wrimap_internal_tides_dissipation', 'internal_tides_dissipation', 'internal_tides_dissipation',                                               &
                     'internal tides dissipation in flow element center', 'J s-1 m-2', UNC_LOC_S, &
                     'Write internal tides dissipation to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_TNUDGE,                              &
                     'Wrimap_nudging', 'Tnudge', 'nudging_time',                                                             &
                     'Nudging relaxing time', 's', UNC_LOC_S)   
      call addoutval(out_quan_conf_map, IDX_MAP_NUDGE_TEM,                           &
                     'Wrimap_nudging', 'nudge_tem', 'nudging_tem',                                                              &
                     'Nudging temperature', 'degC', UNC_LOC_S3D)  
      call addoutval(out_quan_conf_map, IDX_MAP_NUDGE_SAL,                           &
                     'Wrimap_nudging', 'nudge_sal', 'nudging_sal',                                                              &
                     'Nudging salinity', '1e-3', UNC_LOC_S3D)  
      call addoutval(out_quan_conf_map, IDX_MAP_NUDGE_DTEM,                          &
                     'Wrimap_nudging', 'nudge_Dtem', 'nudging_Dtem',                                                             &
                     'Difference of nudging temperature with temperature', 'degC', UNC_LOC_S3D)  
      call addoutval(out_quan_conf_map, IDX_MAP_NUDGE_DSAL,                          &
                     'Wrimap_nudging', 'nudge_Dsal', 'nudging_Dsal',                                                             &
                     'Difference of nudging salinity with salinity', '1e-3', UNC_LOC_S3D)  
      call addoutval(out_quan_conf_map, IDX_MAP_HWAV,                                &
                     'Wrimap_waves', 'hwav', 'RMS wave height',                                                          &
                     'sea_surface_wave_rms_height', 'm', UNC_LOC_S, 'Write wave information to map file')
      call addoutval(out_quan_conf_map, IDX_MAP_HWAV_SIG,                            &
                     'Wrimap_waves', 'hwav', 'Significant wave height',                                                  &
                     'sea_surface_wave_significant_wave_height', 'm', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_TP,                                  &
                     'Wrimap_waves', 'tp', 'Peak wave period',                                                         &
                     '', 's', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_DIR,                                 &
                     'Wrimap_waves', 'dir', 'Mean direction of wave propagation relative to ksi-dir. ccw',              &
                     '', 'deg', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SXWAV,                               &
                     'Wrimap_waves', 'sxwav', 'Surface layer wave forcing term, x-component',                             &
                     'sea_surface_x_wave_force_surface', 'N m-2', UNC_LOC_S)  
      call addoutval(out_quan_conf_map, IDX_MAP_SYWAV,                               &
                     'Wrimap_waves', 'sywav', 'Surface layer wave forcing term, y-component',                             &
                     'sea_surface_y_wave_force_surface', 'N m-2', UNC_LOC_S)  
      call addoutval(out_quan_conf_map, IDX_MAP_SYBWAV,                              &
                     'Wrimap_waves', 'sybwav', 'Bottom layer wave forcing term, y-component',                              &
                     'sea_surface_y_wave_force_bottom', 'N m-2', UNC_LOC_S)  
      call addoutval(out_quan_conf_map, IDX_MAP_MX,                                  &
                     'Wrimap_waves', 'mx', 'Wave-induced volume flux in x-direction',                                  &
                     '', 'm3 s-1 m-1', UNC_LOC_S)    
      call addoutval(out_quan_conf_map, IDX_MAP_MY,                                  &
                     'Wrimap_waves', 'my', 'Wave-induced volume flux in y-direction',                                  &
                     '', 'm3 s-1 m-1', UNC_LOC_S)    
      call addoutval(out_quan_conf_map, IDX_MAP_DISSURF,                             &
                     'Wrimap_waves', 'dissurf', 'Wave energy dissipation rate at the free surface',                         &
                     '', 'w m-2', UNC_LOC_S)  
      call addoutval(out_quan_conf_map, IDX_MAP_DISWCAP,                             &
                     'Wrimap_waves', 'diswcap', 'Wave energy dissipation rate due to white capping',                        &
                     '', 'w m-2', UNC_LOC_S)  
      call addoutval(out_quan_conf_map, IDX_MAP_UORB,                                &
                     'Wrimap_waves', 'uorb', 'Wave orbital velocity',                                                    &
                     'sea_surface_wave_orbital_velocity', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_E,                                   &
                     'Wrimap_waves', 'E', 'Wave energy per square meter',                                             &
                     'sea_surface_bulk_wave_energy', 'J m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_R,                                   &
                     'Wrimap_waves', 'R', 'Roller energy per square meter',                                           &
                     'sea_surface_bulk_roller_energy', 'J m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_DR,                                  &
                     'Wrimap_waves', 'DR', 'Roller energy dissipation per square meter',                               &
                     'sea_surface_bulk_roller_dissipation', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_D,                                   &
                     'Wrimap_waves', 'D', 'Wave breaking energy dissipation per square meter',                        &
                     'sea_surface_wave_breaking_dissipation', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_DF,                                  &
                     'Wrimap_waves', 'Df', 'Wave bottom energy dissipation per square meter',                          &
                     'sea_surface_wave_bottom_dissipation', 'W m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SXX,                                 &
                     'Wrimap_waves', 'Sxx', 'Radiation stress, x-component',                                            &
                     '', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SYY,                                 &
                     'Wrimap_waves', 'Syy', 'Radiation stress, y-component',                                            &
                     '', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SXY,                                 &
                     'Wrimap_waves', 'Sxy', 'Radiation stress, xy-component',                                           &
                     'sea_surface_wave_radiation_stress_NE', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_CWAV,                                &
                     'Wrimap_waves', 'cwav', 'Sea_surface_wave_phase_celerity',                                          &
                     'sea_surface_wave_phase_celerity', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_CGWAV,                               &
                     'Wrimap_waves', 'cgwav', 'Sea_surface_wave_group_celerity',                                          &
                     'sea_surface_wave_group_celerity', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SIGMWAV,                             &
                     'Wrimap_waves', 'sigmwav', 'Sea_surface_wave_mean_frequency',                                          &
                     'sea_surface_wave_mean_frequency', 'rad s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_KWAV,                                &
                     'Wrimap_waves', 'kwav', 'Sea_surface_wave_wavenumber',                                              &
                     'sea_surface_wave_wavenumber', 'rad m-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_NWAV,                                &
                     'Wrimap_waves', 'nwav', 'Sea_surface_wave_ratio_group_phase_speed',                                 &
                     'sea_surface_wave_cg_over_c', '-', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_CTHETA,                              &
                     'Wrimap_waves', 'ctheta', 'Sea_surface_wave_refraction_celerity',                                     &
                     'sea_surface_wave_refraction_celerity', 'rad s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_L1,                                  &
                     'Wrimap_waves', 'L1', 'Sea_surface_wave_wavelength',                                              &
                     'sea_surface_wave_wavelength', 'm', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SWE,                                 &
                     'Wrimap_waves', 'SwE', 'wind source term on wave energy',                                          &
                     'source_term_wind_on_E', 'J m-2 s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SWT,                                 &
                     'Wrimap_waves', 'SwT', 'wind source term on wave period',                                          &
                     'source_term_wind_on_T', 's s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_SXBWAV,                              &
                     'Wrimap_waves', 'sxbwav', 'Water body wave forcing term, x-component',                                &
                     'sea_surface_x_wave_force_bottom', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_UST_CC,                              &
                     'Wrimap_waves', 'ust_cc', 'Stokes drift, x-component',                                                &
                     'sea_surface_x_stokes_drift', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_VST_CC,                              &
                     'Wrimap_waves', 'vst_cc', 'Stokes drift, y-component',                                                &
                     'sea_surface_y_stokes_drift', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_USTOKES,                             &
                     'Wrimap_waves', 'ustokes', 'Stokes drift, n-component',                                                &
                     '', 'm s-1', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_VSTOKES,                             &
                     'Wrimap_waves', 'vstokes', 'Stokes drift, t-component',                                                &
                     '', 'm s-1', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_THETAMEAN,                           &
                     'Wrimap_waves', 'thetamean', 'Wave from direction',                                                      &
                     'sea_surface_wave_from_direction', 'deg from N', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_TWAV,                                &
                     'Wrimap_waves', 'twav', 'Wave period',                                                              &
                     'sea_surface_wave_period', 's', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_FX,                                  &
                     'Wrimap_waves', 'Fx', 'Wave force, x-component',                                                  &
                     'sea_surface_x_wave_force', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_FY,                                  &
                     'Wrimap_waves', 'Fy', 'Wave force, y-component',                                                  &
                     'sea_surface_y_wave_force', 'N m-2', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WAVFU,                               &
                     'Wrimap_waves', 'wavfu', 'Wave force at velocity point, n-component',                                &
                     '', 'N m-2', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_WAVFV,                               &
                     'Wrimap_waves', 'wavfv', 'Wave force at velocity point, t-component',                                &
                     '', 'N m-2', UNC_LOC_U)
      call addoutval(out_quan_conf_map, IDX_MAP_DTCELL,                              &
                     'Wrimap_DTcell', 'dtcell', 'Time step per cell based on CFL',                                          &
                     '', 's', UNC_LOC_S, 'Write time step per cell based on CFL')
      call addoutval(out_quan_conf_map, IDX_MAP_TIME_WATER_ON_GROUND,                &
                     'Wrimap_time_water_on_ground', 'time_water_on_ground', 'Cumulative time water above ground level',                                 &
                     '', 's', UNC_LOC_S, 'Write cumulative time when water is above ground level to map file, only for 1D nodes')
      call addoutval(out_quan_conf_map, IDX_MAP_FREEBOARD,                           &
                     'Wrimap_freeboard', 'freeboard', 'Freeboard',                                                                &
                     '', 'm', UNC_LOC_S, 'Write freeboard to map file, only for 1D nodes')
      call addoutval(out_quan_conf_map, IDX_MAP_WATERDEPTH_ON_GROUND,                &
                     'Wrimap_waterdepth_on_ground', 'waterdepth_on_ground', 'Waterdepth above ground level',                                            &
                     '', 'm', UNC_LOC_S, 'Write waterdepth that is above ground level to map file, only for 1D nodes')
      call addoutval(out_quan_conf_map, IDX_MAP_VOLUME_ON_GROUND,                    &
                     'Wrimap_volume_on_ground', 'volume_on_ground', 'Volume above ground level',                                                &
                     '', 'm3', UNC_LOC_S, 'Write volume that is above ground level to map file, only for 1D nodes')
      call addoutval(out_quan_conf_map, IDX_MAP_CURRENT_TOTAL_NET_INFLOW_1D2D,       &
                     'Wrimap_total_net_inflow_1d2d', 'current_total_net_inflow_1d2d', 'Current total net inflow via all connected 1d2d links at each 1D node',    &
                     '', 'm3 s-1', UNC_LOC_S, 'Write current total 1d2d net inflow (discharge) and cumulative total 1d2d net inflow (volume) to map file, only for 1D nodes')
      call addoutval(out_quan_conf_map, IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_1D2D,    &
                     'Wrimap_total_net_inflow_1d2d', 'cumulative_total_net_inflow_1d2d', 'Cumulative total net inflow via all connected 1d2d links at each 1D node', &
                     '', 'm3', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_CURRENT_TOTAL_NET_INFLOW_LATERAL,    &
                     'Wrimap_total_net_inflow_lateral', 'current_total_net_inflow_lateral', 'Current total net inflow via all laterals at each 1D node',                &
                     '', 'm3 s-1', UNC_LOC_S, 'Write current total lateral net inflow (discharge) and cumulative total net lateral inflow (volume) to map file, only for 1D nodes')
      call addoutval(out_quan_conf_map, IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_LATERAL, &
                     'Wrimap_total_net_inflow_lateral', 'cumulative_total_net_inflow_lateral', 'Cumulative total net inflow via all laterals at each 1D node',             &
                     '', 'm3', UNC_LOC_S)
      call addoutval(out_quan_conf_map, IDX_MAP_WATER_LEVEL_GRADIENT,                &
                     'Wrimap_water_level_gradient', 'water_level_gradient', 'Water level gradient at each 1D flow link',                                &
                     '', '1', UNC_LOC_U, 'Write water level gradient to map file, only on 1D links')
      call addoutval(out_quan_conf_map, IDX_MAP_QIN,                                 &
                     'Wrimap_Qin', 'qin', 'Sum of all water influx',                                                  &
                     '', 'm3 s-1', UNC_LOC_S, 'Write sum of all influxes to map file')
      call addoutval(out_quan_conf_clm, IDX_CLS_S1,                                  &
                     'WriClass_Waterlevel', 's1', 'Water level',                                                              &
                     'sea_surface_height', 'm', UNC_LOC_S, 'Write waterlevel to class map file')
      call addoutval(out_quan_conf_clm, IDX_CLS_WATERDEPTH,                          &
                     'WriClass_Waterdepth', 'waterdepth', 'Water depth at pressure points',                                           &
                     'sea_floor_depth_below_sea_surface', 'm', UNC_LOC_S)
      call addoutval(out_quan_conf_clm, IDX_CLS_UCMAG,                               &
                     'WriClass_Velocity', 'ucmag', 'Flow element center velocity magnitude',                                   &
                     'sea_water_speed', 'm s-1', UNC_LOC_S, 'Write center velocity to class map file')
      call addoutval(out_quan_conf_clm, IDX_CLS_UCMAG_EULER,                         &
                     'WriClass_Velocity', 'ucmag', 'Flow element center Eulerian velocity magnitude',                          &
                     'sea_water_eulerian_speed', 'm s-1', UNC_LOC_S)
      call addoutval(out_quan_conf_clm, IDX_CLS_UCDIR,                               &
                     'WriClass_Velocity', 'ucdir', 'Flow element center velocity direction',                                   &
                     'sea_water_velocity_to_direction', 'degree', UNC_LOC_S)
      call addoutval(out_quan_conf_clm, IDX_CLS_UCDIR_EULER,                         &
                     'WriClass_Velocity', 'ucdir', 'Flow element center Eulerian velocity direction',                          &
                     'sea_water_eulerian_velocity_to_direction', 'degree', UNC_LOC_S)
   end subroutine default_fm_statistical_output
   
         
   subroutine flow_init_statistical_output_his(output_config,output_set)
   use m_flow
   use m_flowexternalforcings
   use m_structures
   use m_observations
   use m_sediment, only: stm_included
   use m_statistical_callback
   use m_transport, only: numconst, isalt, itemp, numconst_mdu
   use m_longculverts, only: nlongculverts
   USE m_monitoring_crosssections, only: ncrs
   USE, INTRINSIC :: ISO_C_BINDING
   
      type(t_output_variable_set),    intent(inout)   :: output_set    !> output set that items need to be added to
      type(t_output_quantity_config_set), intent(in)  :: output_config !> output config for which an output set is needed.
      double precision, pointer, dimension(:) :: temp_pointer
      procedure(process_data_double_interface),  pointer :: function_pointer => NULL()
      
      integer :: i, ntot
      
      ntot = numobs + nummovobs
    
      !
      ! Mass balance variables
      !
      if (jahisbal > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VOLTOT                     ),voltot(IDX_VOLTOT                      :IDX_VOLTOT                         )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_STOR                       ),voltot(IDX_HIS_STOR                    :IDX_HIS_STOR                       )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VOLERR                     ),voltot(IDX_HIS_VOLERR                  :IDX_HIS_VOLERR                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDIN                      ),voltot(IDX_HIS_BNDIN                   :IDX_HIS_BNDIN                      )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDOUT                     ),voltot(IDX_HIS_BNDOUT                  :IDX_HIS_BNDOUT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BNDTOT                     ),voltot(IDX_HIS_BNDTOT                  :IDX_HIS_BNDTOT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHIN                     ),voltot(IDX_HIS_EXCHIN                  :IDX_HIS_EXCHIN                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHOUT                    ),voltot(IDX_HIS_EXCHOUT                 :IDX_HIS_EXCHOUT                    )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXCHTOT                    ),voltot(IDX_HIS_EXCHTOT                 :IDX_HIS_EXCHTOT                    )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PRECIP_TOTAL               ),voltot(IDX_HIS_PRECIP_TOTAL            :IDX_HIS_PRECIP_TOTAL               )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EVAP                       ),voltot(IDX_HIS_EVAP                    :IDX_HIS_EVAP                       )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOUR                       ),voltot(IDX_HIS_SOUR                    :IDX_HIS_SOUR                       )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INTERNALTIDESDISSIPATION   ),voltot(IDX_HIS_INTERNALTIDESDISSIPATION:IDX_HIS_INTERNALTIDESDISSIPATION   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GravInput                  ),voltot(IDX_HIS_GravInput               :IDX_HIS_GravInput                  )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SalInput                   ),voltot(IDX_HIS_SalInput                :IDX_HIS_SalInput                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SalInput2                  ),voltot(IDX_HIS_SalInput2               :IDX_HIS_SalInput2                  )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWIN                      ),voltot(IDX_HIS_GRWIN                   :IDX_HIS_GRWIN                      )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWOUT                     ),voltot(IDX_HIS_GRWOUT                  :IDX_HIS_GRWOUT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GRWTOT                     ),voltot(IDX_HIS_GRWTOT                  :IDX_HIS_GRWTOT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN                      ),voltot(IDX_HIS_LATIN                   :IDX_HIS_LATIN                      )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT                     ),voltot(IDX_HIS_LATOUT                  :IDX_HIS_LATOUT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT                     ),voltot(IDX_HIS_LATTOT                  :IDX_HIS_LATTOT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN1D                    ),voltot(IDX_HIS_LATIN1D                 :IDX_HIS_LATIN1D                    )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT1D                   ),voltot(IDX_HIS_LATOUT1D                :IDX_HIS_LATOUT1D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT1D                   ),voltot(IDX_HIS_LATTOT1D                :IDX_HIS_LATTOT1D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATIN2D                    ),voltot(IDX_HIS_LATIN2D                 :IDX_HIS_LATIN2D                    )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATOUT2D                   ),voltot(IDX_HIS_LATOUT2D                :IDX_HIS_LATOUT2D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATTOT2D                   ),voltot(IDX_HIS_LATTOT2D                :IDX_HIS_LATTOT2D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN                      ),voltot(IDX_HIS_EXTIN                   :IDX_HIS_EXTIN                      )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT                     ),voltot(IDX_HIS_EXTOUT                  :IDX_HIS_EXTOUT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT                     ),voltot(IDX_HIS_EXTTOT                  :IDX_HIS_EXTTOT                     )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN1D                    ),voltot(IDX_HIS_EXTIN1D                 :IDX_HIS_EXTIN1D                    )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT1D                   ),voltot(IDX_HIS_EXTOUT1D                :IDX_HIS_EXTOUT1D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT1D                   ),voltot(IDX_HIS_EXTTOT1D                :IDX_HIS_EXTTOT1D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTIN2D                    ),voltot(IDX_HIS_EXTIN2D                 :IDX_HIS_EXTIN2D                    )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTOUT2D                   ),voltot(IDX_HIS_EXTOUT2D                :IDX_HIS_EXTOUT2D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EXTTOT2D                   ),voltot(IDX_HIS_EXTTOT2D                :IDX_HIS_EXTTOT2D                   )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ICEPT                      ),voltot(IDX_HIS_ICEPT                   :IDX_HIS_ICEPT                      )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EVAP_ICEPT                 ),voltot(IDX_HIS_EVAP_ICEPT              :IDX_HIS_EVAP_ICEPT                 )                                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PRECIP_GROUND              ),voltot(IDX_HIS_PRECIP_GROUND           :IDX_HIS_PRECIP_GROUND              )                                  )
      endif

      !
      ! Source-sink variables
      !
      if (jahissourcesink > 0 .and. numsrc > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_DISCHARGE),               qstss(1:(numconst+1)*numsrc:(numconst+1)))
         i = 1
         if (isalt > 0) then
            i = i + 1
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_SALINITY_INCREMENT),   qstss(i:(numconst+1)*numsrc:(numconst+i)))
         endif
         if (itemp > 0) then
            i = i + 1
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_PRESCRIBED_TEMPERATURE_INCREMENT),qstss(i:(numconst+1)*numsrc:(numconst+i)))
         endif
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_CURRENT_DISCHARGE),qsrc)
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_CUMULATIVE_VOLUME),vsrccum)
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SOURCE_SINK_DISCHARGE_AVERAGE),qsrcavg)
      endif

      !
      ! Hydraulic structures variables
      !
      if (jahiscgen > 0 .and. ngenstru > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE            ),valgenstru(IVAL_DIS   ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_CREST_LEVEL          ),valgenstru(IVAL_CRESTL,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_LOWER_EDGE_LEVEL),valgenstru(IVAL_EDGEL ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_WIDTH   ),valgenstru(IVAL_OPENW ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1UP                 ),valgenstru(IVAL_S1UP  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1DN                 ),valgenstru(IVAL_S1DN  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_HEAD                 ),valgenstru(IVAL_HEAD  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA            ),valgenstru(IVAL_AREA  ,1:ngenstru))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY             ),valgenstru(IVAL_VEL   ,1:ngenstru))
      endif
      if (network%sts%numGeneralStructures > 0) then ! write extra fields for new general structure
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_CREST_WIDTH                   ),valgenstru(IVAL_CRESTW   ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_THROUGH_GATE_OPENING),valgenstru(IVAL_DIS_OPEN ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_OVER_GATE           ),valgenstru(IVAL_DIS_OVER ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_UNDER_GATE          ),valgenstru(IVAL_DIS_UNDER,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_HEIGHT           ),valgenstru(IVAL_OPENH    ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_GATE_UPPER_EDGE_LEVEL         ),valgenstru(IVAL_UPPL     ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_THROUGH_GATE_OPENING ),valgenstru(IVAL_VEL_OPEN ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_OVER_GATE            ),valgenstru(IVAL_VEL_OVER ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_UNDER_GATE           ),valgenstru(IVAL_VEL_UNDER,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_IN_GATE_OPENING     ),valgenstru(IVAL_AREA_OPEN ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_OVER_GATE           ),valgenstru(IVAL_AREA_OVER ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_UNDER_GATE          ),valgenstru(IVAL_AREA_UNDER,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_STATE                         ),valgenstru(IVAL_STATE     ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_S1_ON_CREST                   ),valgenstru(IVAL_S1ONCREST ,1:network%sts%numGeneralStructures))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GENERAL_STRUCTURE_FORCE_DIFFERENCE              ),valgenstru(IVAL_FORCEDIF  ,1:network%sts%numGeneralStructures))
      endif
      if (jahiscdam > 0 .and. ncdamsg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_DISCHARGE           ), valcdam(2,:)                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_CREST_LEVEL         ), zcdam                      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_S1UP                ), valcdam(3,:)                  )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CDAM_S1DN                ), valcdam(4,:)                  )
      endif
      if (jahispump > 0 .and. npumpsg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_STRUCTURE_DISCHARGE),valgenstru(IVAL_DIS       ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_CAPACITY           ),valgenstru(IVAL_PP_CAP    ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_DISCHARGE_DIR      ),valgenstru(IVAL_PP_DISDIR ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1UP               ),valgenstru(IVAL_S1UP      ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1DN               ),valgenstru(IVAL_S1DN      ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_STRUCTURE_HEAD     ),valgenstru(IVAL_PP_HEAD   ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_ACTUAL_STAGE       ),valgenstru(IVAL_PP_STAG   ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_HEAD               ),valgenstru(IVAL_PP_HEAD   ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_REDUCTION_FACTOR   ),valgenstru(IVAL_PP_RED    ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1_DELIVERY_SIDE   ),valgenstru(IVAL_PP_S1DEL  ,1:npumpsg))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PUMP_S1_SUCTION_SIDE    ),valgenstru(IVAL_PP_S1SUC  ,1:npumpsg))
      endif                                                                                          
      if (jahisgate > 0 .and. ngatesg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_DISCHARGE       ),valgate(2,:)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_LOWER_EDGE_LEVEL),zgate                                          )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_S1UP            ),valgate(3,:)                                   )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATE_S1DN            ),valgate(4,:)                                   )
      endif
      if (jahisgate > 0 .and. ngategen > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_DISCHARGE            ),valgategen(IVAL_DIS,:)                                 )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_CREST_LEVEL          ),valgategen(IVAL_GATE_SILLH,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_CREST_WIDTH          ),valgategen(IVAL_WIDTH,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_GATE_LOWER_EDGE_LEVEL),valgategen(IVAL_GATE_EDGEL,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_FLOW_THROUGH_HEIGHT  ),valgategen(IVAL_GATE_FLOWH,:)                           )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_GATE_OPENING_WIDTH   ),valgategen(IVAL_GATE_OPENW,:)                                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_S1UP                 ),valgategen(IVAL_S1UP,:)                                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_GATEGEN_S1DN                 ),valgategen(IVAL_S1DN,:)    )
      endif
      if (jahisweir > 0 .and. nweirgen > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_DISCHARGE       ),valweirgen(IVAL_DIS       ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_CREST_LEVEL     ),valweirgen(IVAL_CRESTL    ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_CREST_WIDTH     ),valweirgen(IVAL_CRESTW    ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1UP            ),valweirgen(IVAL_S1UP      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1DN            ),valweirgen(IVAL_S1DN      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_STRUCTURE_HEAD  ),valweirgen(IVAL_HEAD      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_VELOCITY        ),valweirgen(IVAL_VEL       ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_FLOW_AREA       ),valweirgen(IVAL_AREA      ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_STATE           ),valweirgen(IVAL_STATE     ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_FORCE_DIFFERENCE),valweirgen(IVAL_FORCEDIF  ,1:nweirgen))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WEIRGEN_S1_ON_CREST     ),valweirgen(IVAL_S1ONCREST ,1:nweirgen))
      endif
      if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_DISCHARGE            ),valorifgen(IVAL_DIS       ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_CREST_LEVEL          ),valorifgen(IVAL_CRESTL    ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_CREST_WIDTH          ),valorifgen(IVAL_CRESTW    ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_GATE_LOWER_EDGE_LEVEL),valorifgen(IVAL_S1UP      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1UP                 ),valorifgen(IVAL_S1DN      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1DN                 ),valorifgen(IVAL_HEAD      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_GATE_OPENING_HEIGHT  ),valorifgen(IVAL_VEL       ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_HEAD                 ),valorifgen(IVAL_AREA      ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_FLOW_AREA            ),valorifgen(IVAL_STATE     ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_STATE                ),valorifgen(IVAL_FORCEDIF  ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_S1_ON_CREST          ),valorifgen(IVAL_S1ONCREST ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_VELOCITY             ),valorifgen(IVAL_S1ONCREST ,1:network%sts%numOrifices))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_ORIFICE_FORCE_DIFFERENCE     ),valorifgen(IVAL_S1ONCREST ,1:network%sts%numOrifices))
      endif
      if (jahisbridge > 0 .and. network%sts%numBridges > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_DISCHARGE ),valbridge(IVAL_DIS,     1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_S1UP      ),valbridge(IVAL_S1UP,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_S1DN      ),valbridge(IVAL_S1DN,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_HEAD      ),valbridge(IVAL_HEAD,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_FLOW_AREA ),valbridge(IVAL_AREA,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_VELOCITY  ),valbridge(IVAL_VEL,     1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BLUP      ),valbridge(IVAL_BLUP,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BLDN      ),valbridge(IVAL_BLDN,    1:network%sts%numBridges)        )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRIDGE_BL_ACTUAL ),valbridge(IVAL_BLACTUAL,1:network%sts%numBridges)        )
      endif
      if (jahisculv > 0 .and. network%sts%numCulverts > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_DISCHARGE            ),valculvert(IVAL_DIS        ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_CREST_LEVEL          ),valculvert(IVAL_CL_CRESTL  ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_GATE_LOWER_EDGE_LEVEL),valculvert(IVAL_CL_EDGEL   ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_S1UP                 ),valculvert(IVAL_S1UP       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_S1DN                 ),valculvert(IVAL_S1DN       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_GATE_OPENING_HEIGHT  ),valculvert(IVAL_CL_OPENH   ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_HEAD                 ),valculvert(IVAL_HEAD       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_FLOW_AREA            ),valculvert(IVAL_AREA       ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_VELOCITY             ),valculvert(IVAL_VEL        ,1:network%sts%numCulverts)            )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CULVERT_STATE                ),valculvert(IVAL_CL_STATE   ,1:network%sts%numCulverts)            )
      endif
      if (jahisdambreak > 0 .and. ndambreak > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_S1UP                        ),valdambreak(IVAL_S1UP,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_S1DN                        ),valdambreak(IVAL_S1DN,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_DISCHARGE                   ),valdambreak(IVAL_DIS,1:ndambreak)       )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CUMULATIVE_DISCHARGE        ),valdambreak(IVAL_DB_DISCUM,1:ndambreak) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_VELOCITY                    ),valdambreak(IVAL_VEL,1:ndambreak)       )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_HEAD                        ),valdambreak(IVAL_HEAD,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_FLOW_AREA                   ),valdambreak(IVAL_AREA,1:ndambreak)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CREST_LEVEL                 ),valdambreak(IVAL_DB_CRESTH,1:ndambreak) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_CREST_WIDTH                 ),valdambreak(IVAL_DB_CRESTW,1:ndambreak) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_BREACH_WIDTH_TIME_DERIVATIVE),valdambreak(IVAL_DB_TIMEDIV,1:ndambreak))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DAMBREAK_WATER_LEVEL_JUMP            ),valdambreak(IVAL_DB_JUMP,1:ndambreak)   )
      endif
      if (jahisuniweir > 0 .and. network%sts%numuniweirs > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_DISCHARGE  ),valuniweir(IVAL_DIS,1:network%sts%numuniweirs)      )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_CREST_LEVEL),valuniweir(IVAL_UW_CRESTL,1:network%sts%numuniweirs))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_S1UP       ),valuniweir(IVAL_S1UP,1:network%sts%numuniweirs)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_S1DN       ),valuniweir(IVAL_S1DN,1:network%sts%numuniweirs)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_HEAD       ),valuniweir(IVAL_HEAD,1:network%sts%numuniweirs)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_FLOW_AREA  ),valuniweir(IVAL_AREA,1:network%sts%numuniweirs)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UNIWEIR_VELOCITY   ),valuniweir(IVAL_VEL,1:network%sts%numuniweirs)      )
      endif
      if (jahiscmpstru > 0 .and. network%cmps%count > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_DISCHARGE),valcmpstru(IVAL_DIS,1:network%cmps%count) )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_S1UP     ),valcmpstru(IVAL_S1UP,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_S1DN     ),valcmpstru(IVAL_S1DN,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_HEAD     ),valcmpstru(IVAL_HEAD,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_FLOW_AREA),valcmpstru(IVAL_AREA,1:network%cmps%count))
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CMPSTRU_VELOCITY ),valcmpstru(IVAL_VEL,1:network%cmps%count) )
      endif
      if (jahislongculv > 0 .and. nlongculverts > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_DISCHARGE             ),vallongculvert(IVAL_DIS,1:nlongculverts)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_S1UP                  ),vallongculvert(IVAL_S1UP,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_S1DN                  ),vallongculvert(IVAL_S1DN,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_HEAD                  ),vallongculvert(IVAL_HEAD,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_FLOW_AREA             ),vallongculvert(IVAL_AREA,1:nlongculverts)    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_VELOCITY              ),vallongculvert(IVAL_VEL,1:nlongculverts)     )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LONGCULVERT_VALVE_RELATIVE_OPENING),vallongculvert(IVAL_LC_VALVE,1:nlongculverts))
      endif

      !
      ! Output on observation stations
      !

      ! Basic flow quantities
      if (ntot > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WATERLEVEL),valobs(IPNT_S1,:)                               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BEDLEVEL)  ,valobs(IPNT_BL,:)                             )
         if (jahiswatdep > 0) then
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WATERDEPTH),valobs(IPNT_HS,:)                               )
         endif
      endif
      if( jahisvelvec > 0 ) then
         if (numobs+nummovobs > 0) then
            if ( kmx>0 ) then
               call c_f_pointer (c_loc(valobs(IPNT_UCX:IPNT_UCX+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_X_VELOCITY),temp_pointer)
               
               call c_f_pointer (c_loc(valobs(IPNT_UCY:IPNT_UCY+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Y_VELOCITY),temp_pointer)
               
               call c_f_pointer (c_loc(valobs(IPNT_UCZ:IPNT_UCZ+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Z_VELOCITY),temp_pointer)

               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DEPTH_AVERAGED_X_VELOCITY),valobs(IPNT_UCXQ,:)             )
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DEPTH_AVERAGED_Y_VELOCITY),valobs(IPNT_UCYQ,:)             )
            else
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_X_VELOCITY),valobs(IPNT_UCX,:)                             )
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Y_VELOCITY),valobs(IPNT_UCY,:)                             )
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_Z_VELOCITY),valobs(IPNT_UCZ,:)                            )
            endif
         endif
      endif
      if (jahisvelocity > 0) then
         if (jaeulervel==0) then
            if(kmx>0) then
               call c_f_pointer (c_loc(valobs(IPNT_UMAG:IPNT_UMAG+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE),temp_pointer)
            else
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE),valobs(IPNT_UMAG,:)                                        )
            endif
         else
            if(kmx>0) then
               call c_f_pointer (c_loc(valobs(IPNT_UMAG:IPNT_UMAG+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN),temp_pointer)
            else
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN),valobs(IPNT_UMAG,:)                                        )
            endif
         endif
      endif
      if (jahisdischarge > 0) then
         if(kmx>0) then
            call c_f_pointer (c_loc(valobs(IPNT_QMAG:IPNT_QMAG+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DISCHARGE_MAGNITUDE),temp_pointer)
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DISCHARGE_MAGNITUDE),valobs(IPNT_QMAG,:)                                       )
         endif
      endif
      
      ! Turbulence model
      if ( kmx.gt.0 ) then
         if (iturbulencemodel >= 3 .and. jahistur > 0) then
            call c_f_pointer (c_loc(valobs(IPNT_TKIN:IPNT_TKIN+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TKE      ),temp_pointer                            )
         endif
         if (iturbulencemodel == 3 .and. jahistur >0) then
            call c_f_pointer (c_loc(valobs(IDX_HIS_EPS:IDX_HIS_EPS+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_EPS      ),temp_pointer                     )
         endif
         if (iturbulencemodel > 1) then
            call c_f_pointer (c_loc(valobs(IPNT_VICWW:IPNT_VICWW+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VICWW    ),temp_pointer                         )
         endif
         if (iturbulencemodel == 4 .and. jahistur > 0) then
            call c_f_pointer (c_loc(valobs(IPNT_TEPS:IPNT_TEPS+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAU     ),temp_pointer                      )
         endif
      endif
      if (idensform > 0 .and. jaRichardsononoutput > 0 .and. kmx > 0) then
         call c_f_pointer (c_loc(valobs(IPNT_RICH:IPNT_RICH+kmx,1:ntot)), temp_pointer, [kmx*ntot])
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RICH),temp_pointer)
      endif
      
      ! Gravity + buoyancy
      if (jasal > 0 .and. jahissal > 0) then
         if (kmx>0) then
            call c_f_pointer (c_loc(valobs(IPNT_SA1:IPNT_SA1+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SALINITY   ),temp_pointer                                )
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SALINITY   ),valobs(IPNT_SA1,:)                                )
         endif
      endif

      if (jatem > 0 .and. jahistem > 0) then
         if (kmx > 0) then
            call c_f_pointer (c_loc(valobs(IPNT_TEM1:IPNT_TEM1+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TEMPERATURE),temp_pointer)
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TEMPERATURE),valobs(IPNT_TEM1,:))
         endif
      end if

      if( (jasal > 0 .or. jatem > 0 .or. jased > 0 )  .and. jahisrho > 0) then
         if (kmx>0) then
            call c_f_pointer (c_loc(valobs(IPNT_RHOP:IPNT_RHOP+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_POTENTIAL_DENSITY   ),temp_pointer                                )
            
            call c_f_pointer (c_loc(valobs(IPNT_BRUV:IPNT_BRUV+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_BRUNT_VAISALA_N2),temp_pointer                              )
            if (idensform > 10) then
               call c_f_pointer (c_loc(valobs(IPNT_RHO:IPNT_RHO+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_DENSITY),temp_pointer                                )
            endif
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_POTENTIAL_DENSITY),valobs(IPNT_RHOP,:)                              )
         endif
      endif

      ! Wave model
      if (jawave>0 .and. jahiswav > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_HWAV    ),valobs(IPNT_WAVEH,:)                                    )
         !call add_stat_output_item(output_set, output_config%statout(IDX_HIS_HWAV_SIG),valobs(IPNT_HS,:)                                    )
         ! TODO: hwav sig vs. rms
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TWAV    ),valobs(IPNT_WAVET,:)                                    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_PHIWAV  ),valobs(IPNT_WAVED,:)                                    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RLABDA  ),valobs(IPNT_WAVEL,:)                              )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_UORB    ),valobs(IPNT_WAVEU,:)                              )
         if ( kmx>0 .and. .not. flowwithoutwaves) then
            call c_f_pointer (c_loc(valobs(IPNT_UCXST:IPNT_UCXST+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_USTOKES),temp_pointer)
            
            call c_f_pointer (c_loc(valobs(IPNT_UCYST:IPNT_UCYST+kmx,1:ntot)), temp_pointer, [kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VSTOKES),temp_pointer)
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_USTOKES),valobs(IPNT_UCXST,:)                                 )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_VSTOKES),valobs(IPNT_UCYST,:)                                 )
         endif
      endif
      if (jahistaucurrent>0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAUSX),valobs(IPNT_TAUX,:)                                                    )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAUSY),valobs(IPNT_TAUY,:)                                                    )
      endif

      ! Meteo
      if (jawind > 0 .and. jahiswind > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDX         ),valobs(IPNT_wx,:)                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDX_SFERIC  ),valobs(IPNT_wx,:)                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDY         ),valobs(IPNT_wy,:)                )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WINDY_SFERIC  ),valobs(IPNT_wy,:)                )
      endif

      if (jarain > 0 .and. jahisrain > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RAIN            ),valobs(IPNT_rain,:)                      )
      endif

      if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INFILTRATION_CAP)                 ,valobs(IPNT_infiltcap,:)                                          )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL) ,valobs(IPNT_infiltact,:)            )
      endif

      ! Heat flux model
      if (jatem > 1 .and. jahisheatflux > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WIND),valobs(IPNT_WIND,:)                               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_TAIR),valobs(IPNT_TAIR,:)                               )
         if (jatem == 5 .and. allocated(Rhum) .and. allocated(Clou) ) then
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_RHUM),valobs(IPNT_RHUM,:)                                    )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CLOU),valobs(IPNT_CLOU,:)                                    )
         endif
         if (jatem == 5 ) then
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QSUN  ),valobs(IPNT_QSUN,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QEVA  ),valobs(IPNT_QEVA,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QCON  ),valobs(IPNT_QCON,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QLONG ),valobs(IPNT_QLON,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QFREVA),valobs(IPNT_QFRE,:)                                     )
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QFRCON),valobs(IPNT_QFRC,:)                                     )
            endif
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_QTOT  ),valobs(IPNT_RHUM,:)                                     )
      endif

      ! Sediment model
      if (jased > 0 .and. .not. stm_included) then
         if (kmx >0) then
               call c_f_pointer (c_loc(valobs(IPNT_SED:IPNT_SED+kmx,1:ntot)), temp_pointer, [kmx*ntot])
               call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SED),temp_pointer)
         else
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SED),valobs(IPNT_SED,:)                                  )
         endif
      endif
      if (IVAL_WS1 > 0) then
         if (kmx > 0) then
            call c_f_pointer (c_loc(valobs(IPNT_WS1:IPNT_WS1+(IVAL_WSN-IVAL_WS1*kmx),1:ntot)), temp_pointer, [(IVAL_WSN-IPNT_WS1)*kmx*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WS),temp_pointer                                                        )
         else
            call c_f_pointer (c_loc(valobs(IPNT_WS1:IVAL_WSN,1:ntot)), temp_pointer, [(IVAL_WSN-IPNT_WS1)*ntot])
            call add_stat_output_item(output_set, output_config%statout(IDX_HIS_WS),temp_pointer                                                        )
         endif
      endif
      if (IVAL_SEDDIF1 > 0) then
         call c_f_pointer (c_loc(valobs(IPNT_WS1:IPNT_WS1+(IVAL_WSN-IVAL_WS1*kmx),1:ntot)), temp_pointer, [(IVAL_WSN-IPNT_WS1)*kmx*ntot])
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_SEDDIF),temp_pointer                                          )
      endif

      !
      ! Variables on observation cross sections
      !
      if (ncrs > 0 .and. NUMCONST_MDU > 0) then
         function_pointer => aggregate_constit_crs_obs_data
         temp_pointer => null()
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_CONSTITUENTS),temp_pointer,function_pointer                                 )
      endif
      

      !
      ! Variables on lateral discharges
      !
      if (jahislateral > 0 .and. numlatsg > 0) then
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS),qplat               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE      ),qplatAve               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS  ),qLatReal               )
         call add_stat_output_item(output_set, output_config%statout(IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE        ),qLatRealAve               )
      endif

         
         
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_S0                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_S1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_POTEVAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_ACTEVAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_PRESCREVAP                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VOL1                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATERDEPTH                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HU                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NEGDPT                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NEGDPT_CUM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NOITER                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NOITER_CUM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_LIMTSTEP                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_LIMTSTEP_CUM                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_COURANT                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_AU                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_U1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_U0                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCXQ_EULERIAN                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCYQ_EULERIAN                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCXQ                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCYQ                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAG_EULER                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAGA_GLM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UCMAGA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WW1                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RHO                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VIU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DIU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Q1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Q1_MAIN                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FIXED_WEIR_ENERGY_LOSS                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SPIRCRV                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SPIRINT                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUMLIMDT                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUS                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAUSMAX                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Z0UCUR                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_Z0UROU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SA1                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CZS                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CZU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFU                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFUTYP                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TEM1                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CONST                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MORS                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TURKIN1                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VICWWU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1_3                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TUREPS1_4                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_0                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_1                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT_2                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFRT                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CFCL                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RAINFALL_RATE                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_INTERCEPTION_WATERDEPTH                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_PATM                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDX                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDY                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDXU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDYU                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDX_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDY_SFERIC                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDXU_SFERIC                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDYU_SFERIC                                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSX                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSY                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSX_SFERIC                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WINDSTRESSY_SFERIC                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TAIR                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_RHUM                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CLOU                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QSUN                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QEVA                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QCON                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QLONG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QFREVA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QFRCON                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QTOT                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TIDALPOTENTIAL                                            )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SALPOTENTIAL                                              )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_INTERNAL_TIDES_DISSIPATION                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TNUDGE                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_TEM                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_SAL                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_DTEM                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NUDGE_DSAL                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_HWAV_SIG                                                  )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TP                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DIR                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXBWAV                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYBWAV                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MX                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_MY                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DISSURF                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DISWCAP                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UORB                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_E                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_R                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DR                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_D                                                         )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DF                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXX                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SYY                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SXY                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CGWAV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SIGMWAV                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_KWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_NWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CTHETA                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_L1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SWE                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_SWT                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_UST_CC                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VST_CC                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_USTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VSTOKES                                                   )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_THETAMEAN                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TWAV                                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FX                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FY                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WAVFU                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WAVFV                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_DTCELL                                                    )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_TIME_WATER_ON_GROUND                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_FREEBOARD                                                 )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATERDEPTH_ON_GROUND                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_VOLUME_ON_GROUND                                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_1D2D                             )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_1D2D                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_LATERAL                          )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_LATERAL                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_WATER_LEVEL_GRADIENT                                      )
      !call add_stat_output_item(output_set, output_config%statout(IDX_MAP_QIN                                                       )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_S1                                                        )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_WATERDEPTH                                                )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCMAG                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCMAG_EULER                                               )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCDIR                                                     )
      !call add_stat_output_item(output_set, output_config%statout(IDX_CLS_UCDIR_EULER                                               )
      !
      
      call initialize_statistical_output(output_set)
         
   end subroutine flow_init_statistical_output_his
   
end module fm_statistical_output