!> Module for the statistical output with FM specific data and functions
module fm_statistical_output
   use m_output_config
   use m_statistical_output
   use messagehandling
   use m_statistical_output_types, only: t_output_variable_item, t_output_variable_set
   use precision, only: dp
   use fm_location_types

   implicit none

   private

   type(t_output_quantity_config_set), public :: config_set_his
   type(t_output_quantity_config_set), public :: config_set_map
   type(t_output_quantity_config_set), public :: config_set_clm

   type(t_output_variable_set), public :: out_variable_set_his
   type(t_output_variable_set), public :: out_variable_set_map
   type(t_output_variable_set), public :: out_variable_set_clm

   public default_fm_statistical_output, flow_init_statistical_output_his, model_is_3D, &
      model_has_fixed_obs_stations, model_has_moving_obs_stations, model_has_obs_stations, close_fm_statistical_output

   type(t_station_nc_dimensions), parameter :: station_nc_dims_2D = t_station_nc_dimensions(statdim=.true., timedim=.true.)
   type(t_station_nc_dimensions), parameter :: station_nc_dims_3D_center = t_station_nc_dimensions(laydim=.true., statdim=.true., timedim=.true.)
   type(t_station_nc_dimensions), parameter :: station_nc_dims_3D_interface_center = t_station_nc_dimensions(laydim_interface_center=.true., statdim=.true., timedim=.true.)
   type(t_station_nc_dimensions), parameter :: station_nc_dims_3D_interface_edge = t_station_nc_dimensions(laydim_interface_edge=.true., statdim=.true., timedim=.true.)

   real(dp), dimension(:, :), allocatable, target :: obscrs_data !< observation cross section constituent data on observation cross sections to be written
   real(dp), dimension(:), allocatable, target :: time_dredged, time_ploughed
   real(dp), dimension(:), allocatable, target :: SBCX, SBCY, SBWX, SBWY, SSWX, SSWY, SSCX, SSCY
   real(dp), dimension(:), allocatable, target :: qplat_data

   logical, public :: apply_statistics_on_output

contains

   subroutine close_fm_statistical_output()
      if (allocated(obscrs_data)) then
         deallocate (obscrs_data)
      end if
      if (allocated(time_dredged)) then
         deallocate (time_dredged)
      end if
      if (allocated(time_ploughed)) then
         deallocate (time_ploughed)
      end if
      if (allocated(SBCX)) then
         deallocate (SBCX)
      end if
      if (allocated(SBCY)) then
         deallocate (SBCY)
      end if
      if (allocated(SBWX)) then
         deallocate (SBWX)
      end if
      if (allocated(SBWY)) then
         deallocate (SBWY)
      end if
      if (allocated(SSWX)) then
         deallocate (SSWX)
      end if
      if (allocated(SSWY)) then
         deallocate (SSWY)
      end if
      if (allocated(SSCX)) then
         deallocate (SSCX)
      end if
      if (allocated(SSCY)) then
         deallocate (SSCY)
      end if
      if (allocated(qplat_data)) then
         deallocate (qplat_data)
      end if
   end subroutine close_fm_statistical_output

   !> allocate array and associate pointer with it if not already done.
   subroutine allocate_and_associate(source_input, size, array1, array2)
      real(dp), pointer, dimension(:), intent(inout) :: source_input !< Pointer to associate with array1
      real(dp), dimension(:), allocatable, target, intent(inout) :: array1 !< Data array 1 to allocate
      real(dp), dimension(:), allocatable, target, intent(inout), optional :: array2 !< Data array 2 to allocate (but not point to)
      integer, intent(in) :: size

      if (.not. allocated(array1)) then
         allocate (array1(size))
         if (present(array2)) then !array 1 and array2 are either both allocated or both not.
            allocate (array2(size))
         end if
      end if
      if (.not. associated(source_input)) then
         source_input => array1
      end if
   end subroutine allocate_and_associate

   subroutine transform_qplat(source_input)
      use m_laterals, only: qplat
      real(dp), pointer, dimension(:), intent(inout) :: source_input !< Unused
      associate (source_input => source_input) ! Unused, since qplat is a module variable
      end associate
      qplat_data = sum(qplat, dim=1)
   end subroutine transform_qplat

   !> Subroutine that divides sediment transport x,y variables by rho
   subroutine assign_sediment_transport(X, Y, IPNT_X, IPNT_Y)
      use m_sediment
      use m_observations_data

      real(dp), dimension(:), intent(out) :: X, Y !< arrays to assign valobs values to
      integer, intent(in) :: IPNT_X, IPNT_Y !< location specifier inside valobs array

      integer :: l, k, ntot
      real(dp) :: rhol

      ntot = numobs + nummovobs
      do l = 1, stmpar%lsedtot
         select case (stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         k = ntot * (l - 1)
         X(k + 1:k + ntot) = valobs(:, IPNT_X + l - 1) / rhol
         Y(k + 1:k + ntot) = valobs(:, IPNT_Y + l - 1) / rhol
      end do
   end subroutine assign_sediment_transport

   !> Wrapper function that will allocate and fill the dredge time arrays
   subroutine calculate_dredge_time_fraction(source_input)
      use m_dad
      use m_flowtimes, only: time1
      real(dp), pointer, dimension(:), intent(inout) :: source_input !< Pointer to source input array for the "SSWX" item, to be assigned once on first call.
      real(dp) :: cof0

      call allocate_and_associate(source_input, dadpar%dredge_dimension_length, time_dredged, time_ploughed)

      cof0 = 1d0; if (time1 > 0d0) cof0 = time1
      time_dredged = dadpar%tim_dredged / cof0
      time_ploughed = dadpar%tim_ploughed / cof0

   end subroutine calculate_dredge_time_fraction

   integer function get_sediment_array_size()
      use m_observations_data, only: numobs, nummovobs
      use m_sediment, only: stmpar

      get_sediment_array_size = (numobs + nummovobs) * stmpar%lsedtot
   end function

   !> Wrapper function that will allocate and fill the sediment transport arrays
   subroutine calculate_sediment_SSW(source_input)
      use m_observations_data
      real(dp), pointer, dimension(:), intent(inout) :: source_input !< Pointer to source input array for the "SSWX" item, to be assigned once on first call.
      call allocate_and_associate(source_input, get_sediment_array_size(), SSWX, SSWY)
      call assign_sediment_transport(SSWX, SSWY, IPNT_SSWX1, IPNT_SSWY1)
   end subroutine calculate_sediment_SSW

   !> Wrapper function that will allocate and fill the sediment transport arrays
   subroutine calculate_sediment_SSC(source_input)
      use m_observations_data
      real(dp), pointer, dimension(:), intent(inout) :: source_input !< Pointer to source input array for the "SSCX" item, to be assigned once on first call.
      call allocate_and_associate(source_input, get_sediment_array_size(), SSCX, SSCY)
      call assign_sediment_transport(SSCX, SSCY, IPNT_SSCX1, IPNT_SSCY1)
   end subroutine calculate_sediment_SSC

   !> Wrapper function that will allocate and fill the sediment transport arrays
   subroutine calculate_sediment_SBW(source_input)
      use m_observations_data
      real(dp), pointer, dimension(:), intent(inout) :: source_input !< Pointer to source input array for the "SBWX" item, to be assigned once on first call.
      call allocate_and_associate(source_input, get_sediment_array_size(), SBWX, SBWY)
      call assign_sediment_transport(SBWX, SBWY, IPNT_SBWX1, IPNT_SBWY1)
   end subroutine calculate_sediment_SBW

   !> Wrapper function that will allocate and fill the sediment transport arrays
   subroutine calculate_sediment_SBC(source_input)
      use m_observations_data
      real(dp), pointer, dimension(:), intent(inout) :: source_input !< Pointer to source input array for the "SBCX" item, to be assigned once on first call.
      call allocate_and_associate(source_input, get_sediment_array_size(), SBCX, SBCY)
      call assign_sediment_transport(SBCX, SBCY, IPNT_SBCX1, IPNT_SBCY1)
   end subroutine calculate_sediment_SBC

   subroutine add_station_water_quality_configs(output_config_set, idx_his_hwq)
      use processes_input, only: num_wq_user_outputs => noout_user
      use results, only: OutputPointers
      use m_fm_wq_processes, only: wq_user_outputs => outputs
      use m_ug_nc_attribute, only: ug_nc_attribute
      use string_module, only: replace_multiple_spaces_by_single_spaces
      use netcdf_utils, only: ncu_set_att
      use m_observations_data, only: numobs, nummovobs
      use m_flow, only: kmx
      type(t_output_quantity_config_set), intent(inout) :: output_config_set !< Output configuration for the his-file.
      integer, allocatable, dimension(:), intent(out) :: idx_his_hwq

      integer :: i, ntot, num_layers
      character(len=255) :: name, description
      type(ug_nc_attribute) :: atts(2)

      num_layers = max(1, kmx)

      ntot = numobs + nummovobs
      allocate (idx_his_hwq(num_wq_user_outputs))

      call ncu_set_att(atts(1), 'geometry', 'station_geom')

      ! Just-in-time add *config* item for water quality output variables
      do i = 1, num_wq_user_outputs
         write (name, "('water_quality_output_',I0)") i
         description = trim(wq_user_outputs%names(i))//' - '//trim(wq_user_outputs%description(i))//' in flow element'
         call replace_multiple_spaces_by_single_spaces(description)
         call ncu_set_att(atts(2), 'description', description)

         call add_output_config(output_config_set, idx_his_hwq(i), 'Wrihis_water_quality_output', trim(name), &
                                trim(wq_user_outputs%names(i)), '', trim(wq_user_outputs%units(i)), UNC_LOC_STATION, &
                                nc_attributes=atts, nc_dim_ids=output_config_set%configs(IDX_HIS_HWQ_ABSTRACT)%nc_dim_ids)

         output_config_set%configs(idx_his_hwq(i))%input_value = output_config_set%configs(IDX_HIS_HWQ_ABSTRACT)%input_value
      end do

   end subroutine add_station_water_quality_configs

   !> Initialize (allocate) observation crosssection data array obscrs_data.
   !! This subroutine should be called only once, such that afterward individual calls
   !! to add_stat_output_items can point to obscrs_data(:,i) slices in this array.
   !! Structure of data array:
   !! * obscrs_data(1:ncrs, 1:5): basic flow quantities
   !! * obscrs_data(1:ncrs, 5+(1:2*NUMCONST_MDU)): constituent transport quantities
   !! * obscrs_data(1:ncrs, 5+2*NUMCONST_MDU+(1:2 + stmpar%lsedtot)): sediment transport quantities
   !! Also, the output config items for sediment and other constituents are also registered
   !! (only now, with the model fully loaded, because earlier the constituents were not yet known).
   subroutine init_obscrs_data_and_config(num_const_items, output_config_set, idx_const)
      use m_monitoring_crosssections, only: ncrs
      use m_ug_nc_attribute
      use m_transport, only: NUMCONST_MDU, const_names, isedn, ised1, const_units
      use m_sediment, only: stmpar, jased, stm_included
      use messagehandling, only: Idlen
      use netcdf_utils, only: ncu_set_att, ncu_sanitize_name
      use MessageHandling, only: err

      integer, intent(out) :: num_const_items !< Number of constituent items (including sediment).
      type(t_output_quantity_config_set), intent(inout) :: output_config_set !< Output configuration for the his-file.
      integer, allocatable, dimension(:), intent(out) :: idx_const !< Indexes for the constituent output variables. To be used for
      !< the registration of the variables in the output set.
      integer :: num, lsed
      character(len=idlen) :: conststr
      character(len=idlen) :: constituent_unit, constituent_cumul_unit
      type(ug_nc_attribute) :: atts(1)

      if (ncrs == 0) then
         return
      end if

      call ncu_set_att(atts(1), 'geometry', 'cross_section_geom')

      num_const_items = 2 * NUMCONST_MDU
      if (jased == 4) then
         num_const_items = num_const_items + 2
         if (stmpar%lsedtot > 0) then
            num_const_items = num_const_items + stmpar%lsedtot
         end if
         if (stmpar%lsedsus > 0) then
            num_const_items = num_const_items + stmpar%lsedsus
         end if
      end if

      if (.not. allocated(obscrs_data)) then
         allocate (obscrs_data(ncrs, 5 + num_const_items)) ! First 5 are for IPNT_Q1C:IPNT_HUA
         allocate (idx_const(num_const_items), source=0)
      else
         call err('Internal error, please report: obscrs_data was already allocated')
      end if

      do num = 1, NUMCONST_MDU
         conststr = const_names(num)
         call ncu_sanitize_name(conststr)

         constituent_unit = ''
         constituent_cumul_unit = ''
         if (num >= ISED1 .and. num <= ISEDN .and. stm_included) then ! The constituent is sediment
            select case (stmpar%morpar%moroutput%transptype)
            case (0)
               constituent_cumul_unit = 'kg'
            case (1, 2)
               constituent_cumul_unit = 'm3'
            end select
         else if (const_units(num) /= ' ') then
            constituent_cumul_unit = trim(const_units(num))//' m3'
         end if
         if (len_trim(constituent_cumul_unit) > 0) then
            constituent_unit = trim(constituent_cumul_unit)//' s-1'
         end if

         ! Just-in-time add *config* item for this constituent current transport
         call add_output_config(output_config_set, idx_const(2 * num - 1), &
                                'Wrihis_crs_constituents', 'cross_section_'//trim(conststr), &
                                'Flux (based on upwind flow cell) for '//trim(conststr), &
                                '', trim(constituent_unit), UNC_LOC_OBSCRS, nc_attributes=atts(1:1))

         output_config_set%configs(idx_const(2 * num - 1))%input_value = &
            output_config_set%configs(IDX_HIS_OBSCRS_CONST_ABSTRACT)%input_value

         ! Just-in-time add *config* item for this constituent cumulative transport
         call add_output_config(output_config_set, idx_const(2 * num), &
                                'Wrihis_crs_constituents', 'cross_section_cumulative_'//trim(conststr), &
                                'Cumulative flux (based on upwind flow cell) for '//trim(conststr), &
                                '', trim(constituent_cumul_unit), UNC_LOC_OBSCRS, nc_attributes=atts(1:1))

         output_config_set%configs(idx_const(2 * num))%input_value = &
            output_config_set%configs(IDX_HIS_OBSCRS_CONST_ABSTRACT)%input_value
      end do
      !
      ! Sediment transport
      !
      if (jased == 4) then
         if (stmpar%lsedtot > 0) then
            ! Just to get the complete list of indexes in idx_const
            idx_const(NUMCONST_MDU * 2 + 1) = IDX_HIS_OBSCRS_SED_BTRANSPORT

            do lsed = 1, stmpar%lsedtot ! Making bedload on crosssections per fraction
               ! Just-in-time add *config* item for this fraction's bed load sediment transport
               call add_output_config(output_config_set, idx_const(NUMCONST_MDU * 2 + 1 + lsed), &
                                      'Wrihis_constituents', 'cross_section_bedload_sediment_transport_'//trim(stmpar%sedpar%namsed(lsed)), &
                                      'Cumulative bed load sediment transport for fraction '//trim(stmpar%sedpar%namsed(lsed)), &
                                      '', 'kg', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))
               output_config_set%configs(idx_const(NUMCONST_MDU * 2 + 1 + lsed))%input_value = &
                  output_config_set%configs(IDX_HIS_OBSCRS_SED_BTRANSPORT_PERFRAC_ABSTRACT)%input_value
            end do
         end if

         if (stmpar%lsedsus > 0) then
            ! Just to get the complete list of indexes in idx_const
            idx_const(NUMCONST_MDU * 2 + 1 + stmpar%lsedtot + 1) = IDX_HIS_OBSCRS_SED_STRANSPORT

            do lsed = 1, stmpar%lsedsus ! Making suspended load on crosssections per fraction
               ! Just-in-time add *config* item for this fraction's suspended load sediment transport
               call add_output_config(output_config_set, idx_const(NUMCONST_MDU * 2 + 1 + stmpar%lsedtot + 1 + lsed), &
                                      'Wrihis_constituents', 'cross_section_suspended_sediment_transport_'//trim(stmpar%sedpar%namsed(lsed)), &
                                      'Cumulative suspended load sediment transport for fraction '//trim(stmpar%sedpar%namsed(lsed)), &
                                      '', 'kg', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))
               output_config_set%configs(idx_const(NUMCONST_MDU * 2 + 1 + stmpar%lsedtot + 1 + lsed))%input_value = &
                  output_config_set%configs(IDX_HIS_OBSCRS_SED_STRANSPORT_PERFRAC_ABSTRACT)%input_value
            end do
         end if
      end if

   end subroutine init_obscrs_data_and_config

   !> Aggregate observation crossection data from crs()% value arrays into source_input data array.
   !! Will fill *all* obscrs_data, and leave the input data_pointer untouched: it assumes that all
   !! individual calls to add_stat_output_items() have already associated their obscrs data_pointer variables
   !! with their corresponding obscrs_data(:,i) slice.
   !! Structure of data array:
   !! * obscrs_data(1:ncrs, 1:5): basic flow quantities
   !! * obscrs_data(1:ncrs, 5+(1:2*NUMCONST_MDU)): constituent transport quantities
   !! * obscrs_data(1:ncrs, 5+2*NUMCONST_MDU+(1:2 + stmpar%lsedtot)): sediment transport quantities
   subroutine aggregate_obscrs_data(data_pointer)
      use m_monitoring_crosssections
      use m_transport, only: ISED1, NUMCONST_MDU, ISEDN
      use m_sediment, only: sedtot2sedsus, stmpar, jased, stm_included
      real(dp), pointer, dimension(:), intent(inout) :: data_pointer !< pointer to constit_crs_obs_data, unused

      integer :: i, IP, num, l, lsed
      real(dp) :: rhol

      associate (data_pointer => data_pointer) ! Unused, since obscrs_data is a module variable
      end associate

      if (ncrs == 0) then
         return
      end if

      ! Fill basic flow quantities
      do i = 1, ncrs
         ! Discharge
         obscrs_data(i, 1) = crs(i)%sumvalcur(IPNT_Q1C)
         ! Cumulative discharge
         obscrs_data(i, 2) = crs(i)%sumvalcum(IPNT_Q1C)
         ! Cross sectional areas A*u
         obscrs_data(i, 3) = crs(i)%sumvalcur(IPNT_AUC)
         ! Average velocity Q/Au
         obscrs_data(i, 4) = crs(i)%sumvalcur(IPNT_U1A)
         ! Entry 5 intentionally left blank (IPNT_S1A)
      end do

      ! Fill all constituent transport through observation crosssection
      do num = 1, NUMCONST_MDU
         IP = IPNT_HUA + num
         if (num >= ISED1 .and. num <= ISEDN .and. stm_included) then
            l = sedtot2sedsus(num - ISED1 + 1)
            select case (stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
         else
            rhol = 1d0 ! dummy
         end if
         do i = 1, ncrs
            obscrs_data(i, 5 + (num - 1) * 2 + 1) = crs(i)%sumvalcur(IP) / rhol
            obscrs_data(i, 5 + (num - 1) * 2 + 2) = crs(i)%sumvalcum(IP) / rhol
         end do
      end do

      if (jased == 4) then

         if (stmpar%lsedtot > 0) then
            ! Bed load (cumulative)
            IP = IPNT_HUA + NUMCONST_MDU + 1
            do i = 1, ncrs
               obscrs_data(i, 5 + 2 * NUMCONST_MDU + 1) = crs(i)%sumvalcum(IP)
            end do

            ! Bed load (per fraction)
            do lsed = 1, stmpar%lsedtot
               IP = IP + 1
               do i = 1, ncrs
                  obscrs_data(i, 5 + 2 * NUMCONST_MDU + 1 + lsed) = crs(i)%sumvalcum(IP)
               end do
            end do
         end if

         if (stmpar%lsedsus > 0) then
            ! Suspended load (cumulative)
            IP = IP + 1
            do i = 1, ncrs
               obscrs_data(i, 5 + 2 * NUMCONST_MDU + 1 + stmpar%lsedtot + 1) = crs(i)%sumvalcum(IP)
            end do

            ! Suspended load (per fraction)
            do lsed = 1, stmpar%lsedsus
               IP = IP + 1
               do i = 1, ncrs
                  obscrs_data(i, 5 + 2 * NUMCONST_MDU + 1 + stmpar%lsedtot + 1 + lsed) = crs(i)%sumvalcum(IP)
               end do
            end do
         end if

      end if

   end subroutine aggregate_obscrs_data

   !> Adds output configs for every tracer on observation stations just in time,
   !! because the tracers are only known during model initialization.
   !! Returns config indices for these variables such that they can be added to the output items for the same tracers
   subroutine add_station_tracer_configs(output_config_set, idx_tracers_stations)
      use netcdf_utils, only: ncu_sanitize_name
      use m_ug_nc_attribute, only: ug_nc_attribute
      use m_transportdata, only: const_names, const_units, ITRA1, ITRAN
      type(t_output_quantity_config_set), intent(inout) :: output_config_set !< Output configuration for the his-file.
      integer, allocatable, intent(out) :: idx_tracers_stations(:) !< Indices of just-in-time added tracers in output_config_set array

      integer :: num_tracers
      character(len=idlen) :: constituent_string
      character(len=idlen) :: unit_string
      integer :: tracer_index, constituent_index

      num_tracers = ITRAN - ITRA1 + 1

      allocate (idx_tracers_stations(num_tracers), source=0)

      do tracer_index = 1, num_tracers
         constituent_index = tracer_index + ITRA1 - 1

         constituent_string = const_names(constituent_index)
         call ncu_sanitize_name(constituent_string)

         if (const_units(constituent_index) /= ' ') then
            unit_string = const_units(constituent_index)
         else
            unit_string = '-'
         end if

         ! add output config item
         call add_output_config(output_config_set, idx_tracers_stations(tracer_index), 'Wrihis_constituents', constituent_string, &
                                const_names(constituent_index), '', unit_string, UNC_LOC_STATION, nc_dim_ids=station_nc_dims_3D_center)

         output_config_set%configs(idx_tracers_stations(tracer_index))%input_value = &
            output_config_set%configs(IDX_HIS_TRACERS_ABSTRACT)%input_value
      end do
   end subroutine add_station_tracer_configs

   !> Add output items for all tracers on stations to output set.
   subroutine add_station_tracer_output_items(output_set, output_config_set, idx_tracers_stations)
      use m_transportdata, only: ITRA1, ITRAN
      use m_flow, only: kmx
      use m_observations_data, only: numobs, nummovobs, valobs, IPNT_TRA1
      type(t_output_variable_set), intent(inout) :: output_set !< Output set that item will be added to
      type(t_output_quantity_config_set), intent(in) :: output_config_set !< Read config items out of config set
      integer, intent(in) :: idx_tracers_stations(:) !< Indices of just-in-time added tracers in output_config_set array

      integer :: num_tracers, num_layers, ntot, variable_index, i_start
      real(dp), pointer :: flattened_valobs_slice(:)

      num_layers = max(1, kmx)
      ntot = numobs + nummovobs
      num_tracers = ITRAN - ITRA1 + 1

      do variable_index = 1, num_tracers
         i_start = IPNT_TRA1 + (variable_index - 1) * num_layers
         flattened_valobs_slice(1:ntot * num_layers) => valobs(:, i_start:i_start + num_layers - 1)
         call add_stat_output_items(output_set, output_config_set%configs(idx_tracers_stations(variable_index)), flattened_valobs_slice)
      end do

   end subroutine add_station_tracer_output_items

   !> Add output configs for the waq bottom substances on observation stations just in time,
   !! because these substances are only known during model initialization.
   !! Return config indices for these variables such that they can be added to the output items for the same substances
   !! NOTE (TB): apparently, these are the 'inactive' substances defined in the waq substance file
   subroutine add_station_wqbot_configs(output_config_set, idx_wqbot_stations)

      use m_fm_wq_processes, only: numwqbots, wqbotnames, wqbotunits
      use netcdf_utils, only: ncu_sanitize_name
      use m_ug_nc_attribute, only: ug_nc_attribute
      use netcdf_utils, only: ncu_set_att

      implicit none

      type(t_output_quantity_config_set), intent(inout) :: output_config_set !< Output configuration set for the his-file.
      integer, allocatable, intent(out) :: idx_wqbot_stations(:) !< Indices of just-in-time added waq bottom substances in output_config_set array

      character(len=idlen) :: waqb_sub_name
      character(len=idlen) :: unit_string
      type(ug_nc_attribute) :: atts(1)
      integer :: i

      allocate (idx_wqbot_stations(numwqbots), source=0)

      call ncu_set_att(atts(1), 'geometry', 'station_geom')

      do i = 1, numwqbots
         waqb_sub_name = wqbotnames(i)
         call ncu_sanitize_name(waqb_sub_name)

         unit_string = wqbotunits(i)
         if (unit_string == ' ') then
            unit_string = '-'
         end if

         ! add output config item
         call add_output_config(output_config_set, idx_wqbot_stations(i), 'Wrihis_wqbot', waqb_sub_name, &
                                trim(wqbotnames(i)), '', unit_string, UNC_LOC_STATION, nc_dim_ids=station_nc_dims_2D, nc_attributes=atts)

         output_config_set%configs(idx_wqbot_stations(i))%input_value = &
            output_config_set%configs(IDX_HIS_WQBOT_ABSTRACT)%input_value
      end do

   end subroutine add_station_wqbot_configs

   !> Add output configs for the 3D waq bottom substances on observation stations just in time,
   !! because these substances are only known during model initialization.
   !! Return config indices for these variables such that they can be added to the output items for the same substances
   subroutine add_station_wqbot3D_configs(output_config_set, idx_wqbot3D_stations)

      use m_fm_wq_processes, only: numwqbots, wqbotnames, wqbotunits
      use netcdf_utils, only: ncu_sanitize_name
      use m_ug_nc_attribute, only: ug_nc_attribute
      use netcdf_utils, only: ncu_set_att

      implicit none

      type(t_output_quantity_config_set), intent(inout) :: output_config_set !< Output configuration for the his-file.
      integer, allocatable, intent(out) :: idx_wqbot3D_stations(:) !< Indices of just-in-time added waq bottom substances in output_config_set array

      character(len=idlen) :: waqb_sub_name
      character(len=idlen) :: unit_string
      type(ug_nc_attribute) :: atts(1)
      integer :: i

      allocate (idx_wqbot3D_stations(numwqbots), source=0)

      call ncu_set_att(atts(1), 'geometry', 'station_geom')

      do i = 1, numwqbots
         waqb_sub_name = wqbotnames(i)
         call ncu_sanitize_name(waqb_sub_name)

         unit_string = wqbotunits(i)
         if (unit_string == ' ') then
            unit_string = '-'
         end if

         ! add output config item
         call add_output_config(output_config_set, idx_wqbot3D_stations(i), 'wrihis_wqbot3d', trim(waqb_sub_name)//'_3D', &
                                trim(wqbotnames(i))//' (3D)', '', unit_string, UNC_LOC_STATION, nc_dim_ids=station_nc_dims_3D_center, nc_attributes=atts)

         output_config_set%configs(idx_wqbot3D_stations(i))%input_value = &
            output_config_set%configs(IDX_HIS_WQBOT3D_ABSTRACT)%input_value
      end do
   end subroutine add_station_wqbot3D_configs

   !> Add output items for all waq bottom substances on stations to output set.
   subroutine add_station_wqbot_output_items(output_set, output_config_set, idx_wqbot_stations)

      use m_fm_wq_processes, only: numwqbots
      use m_observations_data, only: valobs, IPNT_WQB1

      type(t_output_variable_set), intent(inout) :: output_set !< Output set that item will be added to
      type(t_output_quantity_config_set), intent(in) :: output_config_set !< Read config items out of config set
      integer, intent(in) :: idx_wqbot_stations(:) !< Indices of just-in-time added waq bottom substances in output_config_set array

      integer :: i

      do i = 1, numwqbots
         call add_stat_output_items(output_set, output_config_set%configs(idx_wqbot_stations(i)), valobs(:, IPNT_WQB1 + i - 1))
      end do

   end subroutine add_station_wqbot_output_items

   !> Add output items for all 3D waq bottom substances on stations to output set.
   subroutine add_station_wqbot3D_output_items(output_set, output_config_set, idx_wqbot3D_stations)

      use m_fm_wq_processes, only: numwqbots
      use m_observations_data, only: valobs, IPNT_WQB3D1

      type(t_output_variable_set), intent(inout) :: output_set !< Output set that item will be added to
      type(t_output_quantity_config_set), intent(in) :: output_config_set !< Read config items out of config set
      integer, intent(in) :: idx_wqbot3D_stations(:) !< Indices of just-in-time added waq bottom substances in output_config_set array

      integer :: i

      do i = 1, numwqbots
         call add_stat_output_items(output_set, output_config_set%configs(idx_wqbot3D_stations(i)), valobs(:, IPNT_WQB3D1 + i - 1))
      end do

   end subroutine add_station_wqbot3D_output_items

   !> add output config for sediment transports on observation stations
   !! the transpunit is known during model initialisation
   subroutine add_station_sedtrans_configs(output_config_set)
   
      use m_ug_nc_attribute, only: ug_nc_attribute
      use netcdf_utils, only: ncu_set_att
      use m_sediment, only: stmpar
      
      implicit none
      
      type(t_output_quantity_config_set), intent(inout) :: output_config_set
      
      character(len=idlen) :: transpunit
      type(ug_nc_attribute) :: atts(4)
      
      call ncu_set_att(atts(1), 'geometry', 'station_geom')
      
      transpunit = ''
      
      select case (stmpar%morpar%moroutput%transptype)
        case (0)
            transpunit = 'kg s-1 m-1'
        case (1)
            transpunit = 'm3 s-1 m-1'
        case (2)
            transpunit = 'm3 s-1 m-1'
      end select
        
      call add_output_config(output_config_set, IDX_HIS_SBCX, &
                             'wrihis_sediment', 'sbcx', &
                             'Current related bedload transport, x-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(output_config_set, IDX_HIS_SBCY, &
                             'wrihis_sediment', 'sbcy', &
                             'Current related bedload transport, y-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(output_config_set, IDX_HIS_SBWX, &
                             'wrihis_sediment', 'sbwx', &
                             'Wave related bedload transport, x-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(output_config_set, IDX_HIS_SBWY, &
                             'wrihis_sediment', 'sbwy', &
                             'Wave related bedload transport, y-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(output_config_set, IDX_HIS_SSWX, &
                             'wrihis_sediment', 'sswx', &
                             'Wave related suspended transport, x-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(output_config_set, IDX_HIS_SSWY, &
                             'wrihis_sediment', 'sswy', &
                             'Wave related suspended transport, y-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(output_config_set, IDX_HIS_SSCX, &
                             'wrihis_sediment', 'sscx', &
                             'Current related suspended transport, x-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(output_config_set, IDX_HIS_SSCY, &
                             'wrihis_sediment', 'sscy', &
                             'Current related suspended transport, y-component', &
                             '', transpunit, UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      
      output_config_set%configs(IDX_HIS_SBCX)%input_value = '1'
      output_config_set%configs(IDX_HIS_SBCY)%input_value = '1'
      output_config_set%configs(IDX_HIS_SBWX)%input_value = '1'
      output_config_set%configs(IDX_HIS_SBWY)%input_value = '1'      
      output_config_set%configs(IDX_HIS_SSCX)%input_value = '1'
      output_config_set%configs(IDX_HIS_SSCY)%input_value = '1'
      output_config_set%configs(IDX_HIS_SSWX)%input_value = '1'
      output_config_set%configs(IDX_HIS_SSWY)%input_value = '1'
      
   end subroutine add_station_sedtrans_configs
   
   !> Set all possible statistical quantity items in the quantity configuration sets.
   subroutine default_fm_statistical_output()
      use netcdf, only: nf90_int
      use m_flow
      use m_ug_nc_attribute, only: ug_nc_attribute
      use netcdf_utils
      use m_missing
      use m_output_config, only: id_nc_byte, id_nc_char, id_nc_short, id_nc_int, id_nc_float, id_nc_double

      type(ug_nc_attribute) :: atts(4)

      config_set_his%count = 0
      config_set_map%count = 0
      config_set_clm%count = 0

      !
      ! HIS: Mass balances
      !
      call add_output_config(config_set_his, IDX_HIS_VOLTOT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_VOLTOT)), '', '', 'm3', UNC_LOC_GLOBAL, description='Write mass balance totals to his-file')
      call add_output_config(config_set_his, IDX_HIS_STOR, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_STOR)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_VOLERR, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_VOLERR)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_BNDIN, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_BNDIN)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_BNDOUT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_BNDOUT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_BNDTOT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_BNDTOT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXCHIN, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXCHIN)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXCHOUT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXCHOUT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXCHTOT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXCHTOT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_PRECIP_TOTAL, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_PRECIP_TOTAL)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EVAP, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EVAP)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_SOUR, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_SOUR)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_InternalTidesDissipation, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_INTERNALTIDESDISSIPATION)), '', '', 'TJ', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_GravInput, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_GravInput)), '', '', 'TJ', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_SalInput, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_SalInput)), '', '', 'TJ', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_SalInput2, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_SalInput2)), '', '', 'TJ', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_GRWIN, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_GRWIN)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_GRWOUT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_GRWOUT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_GRWTOT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_GRWTOT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATIN, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATIN)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATOUT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATOUT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATTOT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATTOT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATIN1D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATIN1D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATOUT1D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATOUT1D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATTOT1D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATTOT1D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATIN2D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATIN2D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATOUT2D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATOUT2D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_LATTOT2D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_LATTOT2D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTIN, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTIN)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTOUT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTOUT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTTOT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTTOT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTIN1D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTIN1D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTOUT1D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTOUT1D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTTOT1D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTTOT1D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTIN2D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTIN2D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTOUT2D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTOUT2D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EXTTOT2D, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EXTTOT2D)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_ICEPT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_ICEPT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_EVAP_ICEPT, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_EVAP_ICEPT)), '', '', 'm3', UNC_LOC_GLOBAL)
      call add_output_config(config_set_his, IDX_HIS_PRECIP_GROUND, &
                             'Wrihis_balance', 'water_balance_'//trim(voltotname(IDX_PRECIP_GROUND)), '', '', 'm3', UNC_LOC_GLOBAL)

      !
      ! HIS: source sinks
      !
      call ncu_set_att(atts(1), 'geometry', 'source_sink_geom')

      call add_output_config(config_set_his, IDX_HIS_SOURCE_SINK_PRESCRIBED_DISCHARGE, &
                             'Wrihis_sourcesink', 'source_sink_prescribed_discharge', '', '', &
                             'm3 s-1', UNC_LOC_SOSI, nc_attributes=atts(1:1), description='Write source-sink parameters to his file')
      call add_output_config(config_set_his, IDX_HIS_SOURCE_SINK_PRESCRIBED_SALINITY_INCREMENT, &
                             'Wrihis_sourcesink', 'source_sink_prescribed_salinity_increment', '', '', &
                             '1e-3', UNC_LOC_SOSI, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_SOURCE_SINK_PRESCRIBED_TEMPERATURE_INCREMENT, &
                             'Wrihis_sourcesink', 'source_sink_prescribed_temperature_increment', '', '', &
                             'degC', UNC_LOC_SOSI, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_SOURCE_SINK_CURRENT_DISCHARGE, &
                             'Wrihis_sourcesink', 'source_sink_current_discharge', '', '', &
                             'm3 s-1', UNC_LOC_SOSI, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_SOURCE_SINK_CUMULATIVE_VOLUME, &
                             'Wrihis_sourcesink', 'source_sink_cumulative_volume', 'Cumulative volume from the start time until current time at each source/sink', '', &
                             'm3', UNC_LOC_SOSI, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_SOURCE_SINK_DISCHARGE_AVERAGE, &
                             'Wrihis_sourcesink', 'source_sink_discharge_average', 'Average discharge in the past his-file output-interval at each source/sink', '', &
                             'm3 s-1', UNC_LOC_SOSI, nc_attributes=atts(1:1))

      !
      ! HIS: run-up gauges
      !
      call add_output_config(config_set_his, IDX_HIS_RUG_RUHEIGHT, &
                             'Wrihis_runupgauge', 'runup_height', 'runup height', '', &
                             'm', UNC_LOC_RUG, description='Write run-up gauge statistics to his-file')
      call add_output_config(config_set_his, IDX_HIS_RUG_RUX, &
                             'Wrihis_runupgauge', 'rug_x_coordinate', 'time-varying x-coordinate of shoreline position', '', &
                             'm', UNC_LOC_RUG, description='Write run-up gauge statistics to his-file')
      call add_output_config(config_set_his, IDX_HIS_RUG_RUY, &
                             'Wrihis_runupgauge', 'rug_y_coordinate', 'time-varying y-coordinate of shoreline position', '', &
                             'm', UNC_LOC_RUG, description='Write run-up gauge statistics to his-file')

      !
      ! HIS: hydraulic structures
      !

      !! General structure
      call ncu_set_att(atts(1), 'geometry', 'general_structure_geom')

      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE, &
                             'Wrihis_structure_gen', 'general_structure_discharge', 'Total discharge through general structure', '', &
                             'm3 s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1), description='Write general structure parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_CREST_LEVEL, &
                             'Wrihis_structure_gen', 'general_structure_crest_level', 'Crest level of general structure', '', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_GATE_LOWER_EDGE_LEVEL, &
                             'Wrihis_structure_gen', 'general_structure_gate_lower_edge_level', 'Gate lower edge level of general structure', '', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_WIDTH, &
                             'Wrihis_structure_gen', 'general_structure_gate_opening_width', 'Gate opening width of general structure', '', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_S1UP, &
                             'Wrihis_structure_gen', 'general_structure_s1up', 'Water level upstream of general structure', 'sea_surface_height', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_S1DN, &
                             'Wrihis_structure_gen', 'general_structure_s1dn', 'Water level downstream of general structure', 'sea_surface_height', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_HEAD, &
                             'Wrihis_structure_gen', 'general_structure_head', 'Head difference across general structure', '', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA, &
                             'Wrihis_structure_gen', 'general_structure_flow_area', 'Flow area at general structure', '', &
                             'm2', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY, &
                             'Wrihis_structure_gen', 'general_structure_velocity', 'Velocity through general structure', '', &
                             'm s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_CREST_WIDTH, &
                             'Wrihis_structure_gen', 'general_structure_crest_width', 'Crest width of general structure', '', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_THROUGH_GATE_OPENING, &
                             'Wrihis_structure_gen', 'general_structure_discharge_through_gate_opening', 'Discharge through gate opening of general structure', '', &
                             'm3 s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_OVER_GATE, &
                             'Wrihis_structure_gen', 'general_structure_discharge_over_gate', 'Discharge over gate of general structure', '', &
                             'm3 s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_UNDER_GATE, &
                             'Wrihis_structure_gen', 'general_structure_discharge_under_gate', 'Discharge under gate of general structure', '', &
                             'm3 s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_HEIGHT, &
                             'Wrihis_structure_gen', 'general_structure_gate_opening_height', 'Gate opening height of general structure', '', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_GATE_UPPER_EDGE_LEVEL, &
                             'Wrihis_structure_gen', 'general_structure_gate_upper_edge_level', 'Gate upper edge level of general structure', '', &
                             'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY_THROUGH_GATE_OPENING, &
                             'Wrihis_structure_gen', 'general_structure_velocity_through_gate_opening', 'Velocity through gate opening of general structure', '', &
                             'm s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY_OVER_GATE, &
                             'Wrihis_structure_gen', 'general_structure_velocity_over_gate', 'Velocity over gate of general structure', '', &
                             'm s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_VELOCITY_UNDER_GATE, &
                             'Wrihis_structure_gen', 'general_structure_velocity_under_gate', 'Flow area in gate opening of general structure', '', &
                             'm s-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_IN_GATE_OPENING, &
                             'Wrihis_structure_gen', 'general_structure_flow_area_in_gate_opening', 'Flow area in gate opening of general structure', '', &
                             'm2', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_OVER_GATE, &
                             'Wrihis_structure_gen', 'general_structure_flow_area_over_gate', 'Flow area over gate of general structure', '', &
                             'm2', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_UNDER_GATE, &
                             'Wrihis_structure_gen', 'general_structure_flow_area_under_gate', 'Flow area under gate of general structure', '', &
                             'm2', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))

      call ncu_set_att(atts(2), 'flag_values', (/0, 1, 2, 3, 4/))
      call ncu_set_att(atts(3), 'flag_meanings', 'no_flow weir_free weir_submerged gate_free gate_submerged')
      call ncu_set_att(atts(4), 'valid_range', (/0, 4/))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_STATE, &
                             'Wrihis_structure_gen', 'general_structure_state', 'Flow state at general structure', '', &
                             '', UNC_LOC_GENSTRU, id_nc_type=id_nc_int, nc_attributes=atts)
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_S1_ON_CREST, &
                             'Wrihis_structure_gen', 'general_structure_s1_on_crest', 'Water level on crest of general structure', &
                             '', 'm', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GENERAL_STRUCTURE_FORCE_DIFFERENCE, &
                             'Wrihis_structure_gen', 'general_structure_force_difference', 'Force difference per unit at general structure', &
                             '', 'N m-1', UNC_LOC_GENSTRU, nc_attributes=atts(1:1))

      !! Controllable dam (old .ext file)
      call add_output_config(config_set_his, IDX_HIS_CDAM_DISCHARGE, &
                             'Wrihis_structure_dam', 'cdam_discharge', 'controllable dam discharge', &
                             '', 'm3 s-1', UNC_LOC_DAM, description='Write dam parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_CDAM_CREST_LEVEL, &
                             'Wrihis_structure_dam', 'cdam_crest_level', 'controllable dam crest level', &
                             '', 'm', UNC_LOC_DAM)
      call add_output_config(config_set_his, IDX_HIS_CDAM_S1UP, &
                             'Wrihis_structure_dam', 'cdam_s1up', 'controllable dam water level up', &
                             'sea_surface_height', 'm', UNC_LOC_DAM)
      call add_output_config(config_set_his, IDX_HIS_CDAM_S1DN, &
                             'Wrihis_structure_dam', 'cdam_s1dn', 'controllable dam water level down', &
                             'sea_surface_height', 'm', UNC_LOC_DAM)

      !! Pump
      call ncu_set_att(atts(1), 'geometry', 'pump_geom')

      call add_output_config(config_set_his, IDX_HIS_PUMP_STRUCTURE_DISCHARGE, &
                             'Wrihis_structure_pump', 'pump_structure_discharge', 'Discharge through pump', &
                             '', 'm3 s-1', UNC_LOC_PUMP, nc_attributes=atts(1:1), description='Write pump parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_PUMP_CAPACITY, &
                             'Wrihis_structure_pump', 'pump_capacity', 'Capacity of pump', &
                             '', 'm3 s-1', UNC_LOC_PUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PUMP_DISCHARGE_DIR, &
                             'Wrihis_structure_pump', 'pump_discharge_dir', 'Discharge of pump w.r.t. pump orientation', &
                             '', 'm3 s-1', UNC_LOC_PUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PUMP_S1UP, &
                             'Wrihis_structure_pump', 'pump_s1up', 'Water level upstream of pump', &
                             'sea_surface_height', 'm', UNC_LOC_PUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PUMP_S1DN, &
                             'Wrihis_structure_pump', 'pump_s1dn', 'Water level downstream of pump', &
                             'sea_surface_height', 'm', UNC_LOC_PUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PUMP_STRUCTURE_HEAD, &
                             'Wrihis_structure_pump', 'pump_structure_head', 'Head difference across pump structure', &
                             '', 'm', UNC_LOC_PUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PUMP_ACTUAL_STAGE, &
                             'Wrihis_structure_pump', 'pump_actual_stage', 'Actual stage of pump', &
                             '', '', UNC_LOC_PUMP, nc_attributes=atts(1:1), id_nc_type=id_nc_int)
      call add_output_config(config_set_his, IDX_HIS_PUMP_HEAD, &
                             'Wrihis_structure_pump', 'pump_head', 'Head difference in pumping direction', &
                             '', 'm', UNC_LOC_PUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PUMP_REDUCTION_FACTOR, &
                             'Wrihis_structure_pump', 'pump_reduction_factor', 'Reduction factor of pump', &
                             '', '1', UNC_LOC_PUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PUMP_S1_DELIVERY_SIDE, &
                             'Wrihis_structure_pump', 'pump_s1_delivery_side', 'Water level at delivery side of pump', &
                             'sea_surface_height', 'm', UNC_LOC_PUMP)
      call add_output_config(config_set_his, IDX_HIS_PUMP_S1_SUCTION_SIDE, &
                             'Wrihis_structure_pump', 'pump_s1_suction_side', 'Water level at suction side of pump', &
                             'sea_surface_height', 'm', UNC_LOC_PUMP, nc_attributes=atts(1:1))

      !! Gate (old .ext file)
      call add_output_config(config_set_his, IDX_HIS_GATE_DISCHARGE, &
                             'Wrihis_structure_gate', 'gate_discharge', 'gate discharge', &
                             '', 'm3 s-1', UNC_LOC_GATE, description='Write gate parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_GATE_LOWER_EDGE_LEVEL, &
                             'Wrihis_structure_gate', 'gate_lower_edge_level', 'gate lower edge level', &
                             '', 'm', UNC_LOC_GATE)
      call add_output_config(config_set_his, IDX_HIS_GATE_S1UP, &
                             'Wrihis_structure_gate', 'gate_s1up', 'gate water level up', &
                             'sea_surface_height', 'm', UNC_LOC_GATE)
      call add_output_config(config_set_his, IDX_HIS_GATE_S1DN, &
                             'Wrihis_structure_gate', 'gate_s1dn', 'gate water level down', &
                             'sea_surface_height', 'm', UNC_LOC_GATE)

      !! Gate, via general_structure
      call ncu_set_att(atts(1), 'geometry', 'gategen_geom')

      call add_output_config(config_set_his, IDX_HIS_GATEGEN_DISCHARGE, &
                             'Wrihis_structure_gate', 'gategen_discharge', 'gate discharge (via general structure)', &
                             '', 'm3 s-1', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GATEGEN_CREST_LEVEL, &
                             'Wrihis_structure_gate', 'gategen_crest_level', 'gate crest level (via general structure)', &
                             '', 'm', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GATEGEN_CREST_WIDTH, &
                             'Wrihis_structure_gate', 'gategen_crest_width', 'gate crest width (via general structure)', &
                             '', 'm', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GATEGEN_GATE_LOWER_EDGE_LEVEL, &
                             'Wrihis_structure_gate', 'gategen_gate_lower_edge_level', 'gate lower edge level (via general structure)', &
                             '', 'm', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GATEGEN_FLOW_THROUGH_HEIGHT, &
                             'Wrihis_structure_gate', 'gategen_flow_through_height', 'gate flow through height (via general structure)', &
                             '', 'm', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GATEGEN_GATE_OPENING_WIDTH, &
                             'Wrihis_structure_gate', 'gategen_gate_opening_width', 'gate opening width (via general structure)', &
                             '', 'm', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GATEGEN_S1UP, &
                             'Wrihis_structure_gate', 'gategen_s1up', 'gate water level up (via general structure)', &
                             'sea_surface_height', 'm', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_GATEGEN_S1DN, &
                             'Wrihis_structure_gate', 'gategen_s1dn', 'gate water level down (via general structure)', &
                             'sea_surface_height', 'm', UNC_LOC_GATEGEN, nc_attributes=atts(1:1))

      !! Weir (via general_structure)
      call ncu_set_att(atts(1), 'geometry', 'weirgen_geom')

      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_DISCHARGE, &
                             'Wrihis_structure_weir', 'weirgen_discharge', 'Discharge through weir', &
                             '', 'm3 s-1', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1), description='Write weir parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_CREST_LEVEL, &
                             'Wrihis_structure_weir', 'weirgen_crest_level', 'Crest level of weir', &
                             '', 'm', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_CREST_WIDTH, &
                             'Wrihis_structure_weir', 'weirgen_crest_width', 'Crest width of weir', &
                             '', 'm', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_S1UP, &
                             'Wrihis_structure_weir', 'weirgen_s1up', 'Water level upstream of weir', &
                             'sea_surface_height', 'm', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_S1DN, &
                             'Wrihis_structure_weir', 'weirgen_s1dn', 'Water level downstream of weir', &
                             'sea_surface_height', 'm', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_STRUCTURE_HEAD, &
                             'Wrihis_structure_weir', 'weirgen_structure_head', 'Head difference across weir', &
                             '', 'm', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_VELOCITY, &
                             'Wrihis_structure_weir', 'weirgen_velocity', 'Velocity through weir', &
                             '', 'm s-1', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_FLOW_AREA, &
                             'Wrihis_structure_weir', 'weirgen_flow_area', 'Flow area at weir', &
                             '', 'm2', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))

      call ncu_set_att(atts(2), 'flag_values', (/0, 1, 2/))
      call ncu_set_att(atts(3), 'flag_meanings', 'no_flow weir_free weir_submerged')
      call ncu_set_att(atts(4), 'valid_range', (/0, 2/))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_STATE, &
                             'Wrihis_structure_weir', 'weirgen_state', 'Flow state at weir', &
                             '', '', UNC_LOC_WEIRGEN, nc_attributes=atts, id_nc_type=id_nc_int)

      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_FORCE_DIFFERENCE, &
                             'Wrihis_structure_weir', 'weirgen_force_difference', 'Force difference per unit width at weir', '', &
                             'N m-1', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_WEIRGEN_S1_ON_CREST, &
                             'Wrihis_structure_weir', 'weirgen_s1_on_crest', 'Water level on crest of weir', '', &
                             'm', UNC_LOC_WEIRGEN, nc_attributes=atts(1:1))

      !! Orifice
      call ncu_set_att(atts(1), 'geometry', 'orifice_geom')

      call add_output_config(config_set_his, IDX_HIS_ORIFICE_DISCHARGE, &
                             'Wrihis_structure_orifice', 'orifice_discharge', 'Discharge through orifice', '', &
                             'm3 s-1', UNC_LOC_ORIFICE, nc_attributes=atts(1:1), description='Write orifice parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_CREST_LEVEL, &
                             'Wrihis_structure_orifice', 'orifice_crest_level', 'Crest level of orifice', '', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_CREST_WIDTH, &
                             'Wrihis_structure_orifice', 'orifice_crest_width', 'Crest width of orifice', '', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_GATE_LOWER_EDGE_LEVEL, &
                             'Wrihis_structure_orifice', 'orifice_gate_lower_edge_level', 'Gate lower edge level of orifice', '', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_S1UP, &
                             'Wrihis_structure_orifice', 'orifice_s1up', 'Water level upstream of orifice', 'sea_surface_height', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_S1DN, &
                             'Wrihis_structure_orifice', 'orifice_s1dn', 'Water level downstream of orifice', 'sea_surface_height', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_GATE_OPENING_HEIGHT, &
                             'Wrihis_structure_orifice', 'orifice_gate_opening_height', 'Gate opening height of orifice', '', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_HEAD, &
                             'Wrihis_structure_orifice', 'orifice_head', 'Head difference across orifice', '', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_FLOW_AREA, &
                             'Wrihis_structure_orifice', 'orifice_flow_area', 'Flow area at orifice', '', &
                             'm2', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))

      call ncu_set_att(atts(2), 'flag_values', (/0, 1, 2, 3, 4/))
      call ncu_set_att(atts(3), 'flag_meanings', 'no_flow weir_free weir_submerged gate_free gate_submerged')
      call ncu_set_att(atts(4), 'valid_range', (/0, 4/))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_STATE, &
                             'Wrihis_structure_orifice', 'orifice_state', 'Flow state at orifice', '', &
                             '', UNC_LOC_ORIFICE, nc_attributes=atts, id_nc_type=id_nc_int)

      call add_output_config(config_set_his, IDX_HIS_ORIFICE_S1_ON_CREST, &
                             'Wrihis_structure_orifice', 'orifice_s1_on_crest', 'Water level on crest of orifice', '', &
                             'm', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_VELOCITY, &
                             'Wrihis_structure_orifice', 'orifice_velocity', 'Velocity through orifice', '', &
                             'm s-1', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_ORIFICE_FORCE_DIFFERENCE, &
                             'Wrihis_structure_orifice', 'orifice_force_difference', 'Force difference per unit width at orifice', '', &
                             'N m-1', UNC_LOC_ORIFICE, nc_attributes=atts(1:1))

      !! Bridge
      call ncu_set_att(atts(1), 'geometry', 'bridge_geom')

      call add_output_config(config_set_his, IDX_HIS_BRIDGE_DISCHARGE, &
                             'Wrihis_structure_bridge', 'bridge_discharge', 'Discharge through bridge', '', &
                             'm3 s-1', UNC_LOC_BRIDGE, description='Write bridge parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_S1UP, &
                             'Wrihis_structure_bridge', 'bridge_s1up', 'Water level upstream of bridge', 'sea_surface_height', &
                             'm', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_S1DN, &
                             'Wrihis_structure_bridge', 'bridge_s1dn', 'Water level downstream of bridge', 'sea_surface_height', &
                             'm', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_HEAD, &
                             'Wrihis_structure_bridge', 'bridge_head', 'Head difference across bridge', '', &
                             'm', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_FLOW_AREA, &
                             'Wrihis_structure_bridge', 'bridge_flow_area', 'Flow area at bridge', '', &
                             'm2', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_VELOCITY, &
                             'Wrihis_structure_bridge', 'bridge_velocity', 'Velocity through bridge', '', &
                             'm s-1', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_BLUP, &
                             'Wrihis_structure_bridge', 'bridge_blup', 'Bed level at upstream of bridge', 'altitude', &
                             'm', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_BLDN, &
                             'Wrihis_structure_bridge', 'bridge_bldn', 'Bed level at downstream of bridge', 'altitude', &
                             'm', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_BRIDGE_BL_ACTUAL, &
                             'Wrihis_structure_bridge', 'bridge_bl_actual', 'Actual bed level of bridge (crest)', 'altitude', &
                             'm', UNC_LOC_BRIDGE, nc_attributes=atts(1:1))

      !! Culvert
      call ncu_set_att(atts(1), 'geometry', 'culvert_geom')

      call add_output_config(config_set_his, IDX_HIS_CULVERT_DISCHARGE, &
                             'Wrihis_structure_culvert', 'culvert_discharge', 'Discharge through culvert', '', &
                             'm3 s-1', UNC_LOC_CULVERT, nc_attributes=atts(1:1), description='Write culvert parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_CULVERT_CREST_LEVEL, &
                             'Wrihis_structure_culvert', 'culvert_crest_level', 'Crest level of culvert', '', &
                             'm', UNC_LOC_CULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_GATE_LOWER_EDGE_LEVEL, &
                             'Wrihis_structure_culvert', 'culvert_gate_lower_edge_level', 'Gate lower edge level of culvert', '', &
                             'm', UNC_LOC_CULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_S1UP, &
                             'Wrihis_structure_culvert', 'culvert_s1up', 'Water level upstream of culvert', 'sea_surface_height', &
                             'm', UNC_LOC_CULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_S1DN, &
                             'Wrihis_structure_culvert', 'culvert_s1dn', 'Water level downstream of culvert', 'sea_surface_height', &
                             'm', UNC_LOC_CULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_GATE_OPENING_HEIGHT, &
                             'Wrihis_structure_culvert', 'culvert_gate_opening_height', 'Gate opening height of culvert', '', &
                             'm', UNC_LOC_CULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_HEAD, &
                             'Wrihis_structure_culvert', 'culvert_head', 'Head difference across culvert', '', &
                             'm', UNC_LOC_CULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_FLOW_AREA, &
                             'Wrihis_structure_culvert', 'culvert_flow_area', 'Flow area at culvert', '', &
                             'm2', UNC_LOC_CULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_VELOCITY, &
                             'Wrihis_structure_culvert', 'culvert_velocity', 'Velocity through culvert', '', &
                             'm s-1', UNC_LOC_CULVERT, nc_attributes=atts(1:1))

      call ncu_set_att(atts(2), 'flag_values', (/0, 1, 2/))
      call ncu_set_att(atts(3), 'flag_meanings', 'no_flow culvert_free culvert_submerged')
      call ncu_set_att(atts(4), 'valid_range', (/0, 2/))
      call add_output_config(config_set_his, IDX_HIS_CULVERT_STATE, &
                             'Wrihis_structure_culvert', 'culvert_state', 'Flow state at culvert', '', &
                             '', UNC_LOC_CULVERT, nc_attributes=atts, id_nc_type=id_nc_int)

      !! Dambreak
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_S1UP, &
                             'Wrihis_structure_damBreak', 'dambreak_s1up', 'Water level upstream of dambreak', 'sea_surface_height', &
                             'm', UNC_LOC_DAMBREAK, description='Write dam break parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_S1DN, &
                             'Wrihis_structure_damBreak', 'dambreak_s1dn', 'Water level downstream of dambreak', 'sea_surface_height', &
                             'm', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_DISCHARGE, &
                             'Wrihis_structure_damBreak', 'dambreak_discharge', 'Discharge through dambreak', '', &
                             'm3 s-1', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_CUMULATIVE_DISCHARGE, &
                             'Wrihis_structure_damBreak', 'dambreak_cumulative_discharge', 'Cumulative Discharge through dambreak', '', &
                             'm3 s-1', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_VELOCITY, &
                             'Wrihis_structure_damBreak', 'dambreak_velocity', 'Velocity through dambreak', '', &
                             'm s-1', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_HEAD, &
                             'Wrihis_structure_damBreak', 'dambreak_head', 'Head difference across dambreak', '', &
                             'm', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_FLOW_AREA, &
                             'Wrihis_structure_damBreak', 'dambreak_flow_area', 'Flow area at dambreak', '', &
                             'm2', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_CREST_LEVEL, &
                             'Wrihis_structure_damBreak', 'dambreak_crest_level', 'Crest level of dambreak', '', &
                             'm', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_CREST_WIDTH, &
                             'Wrihis_structure_damBreak', 'dambreak_crest_width', 'Crest width of dambreak', '', &
                             'm', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_BREACH_WIDTH_TIME_DERIVATIVE, &
                             'Wrihis_structure_damBreak', 'dambreak_breach_width_time_derivative', 'Breach width time derivative of dambreak', '', &
                             'm s-1', UNC_LOC_DAMBREAK)
      call add_output_config(config_set_his, IDX_HIS_DAMBREAK_WATER_LEVEL_JUMP, &
                             'Wrihis_structure_damBreak', 'dambreak_water_level_jump', 'Breach water level jump of dambreak', '', &
                             'm', UNC_LOC_DAMBREAK)

      !! Universal weir
      call ncu_set_att(atts(1), 'geometry', 'uniweir_geom')

      call add_output_config(config_set_his, IDX_HIS_UNIWEIR_DISCHARGE, &
                             'Wrihis_structure_uniWeir', 'uniweir_discharge', 'Discharge through uniweir', '', &
                             'm3 s-1', UNC_LOC_UNIWEIR, nc_attributes=atts(1:1), description='Write universal weir parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_UNIWEIR_CREST_LEVEL, &
                             'Wrihis_structure_uniWeir', 'uniweir_crest_level', 'Crest level of uniweir', '', &
                             'm', UNC_LOC_UNIWEIR, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_UNIWEIR_S1UP, &
                             'Wrihis_structure_uniWeir', 'uniweir_s1up', 'Water level upstream of uniweir', 'sea_surface_height', &
                             'm', UNC_LOC_UNIWEIR, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_UNIWEIR_S1DN, &
                             'Wrihis_structure_uniWeir', 'uniweir_s1dn', 'Water level downstream of uniweir', 'sea_surface_height', &
                             'm', UNC_LOC_UNIWEIR, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_UNIWEIR_HEAD, &
                             'Wrihis_structure_uniWeir', 'uniweir_head', 'Head difference across uniweir', '', &
                             'm', UNC_LOC_UNIWEIR, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_UNIWEIR_FLOW_AREA, &
                             'Wrihis_structure_uniWeir', 'uniweir_flow_area', 'Flow area at uniweir', '', &
                             'm2', UNC_LOC_UNIWEIR, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_UNIWEIR_VELOCITY, &
                             'Wrihis_structure_uniWeir', 'uniweir_velocity', 'Velocity through uniweir', '', &
                             'm s-1', UNC_LOC_UNIWEIR, nc_attributes=atts(1:1))

      !! Compound structure
      call add_output_config(config_set_his, IDX_HIS_CMPSTRU_DISCHARGE, &
                             'Wrihis_structure_compound', 'cmpstru_discharge', 'Discharge through cmpstru', '', &
                             'm3 s-1', UNC_LOC_CMPSTRU, description='Write compound structure parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_CMPSTRU_S1UP, &
                             'Wrihis_structure_compound', 'cmpstru_s1up', 'Water level upstream of cmpstru', 'sea_surface_height', &
                             'm', UNC_LOC_CMPSTRU)
      call add_output_config(config_set_his, IDX_HIS_CMPSTRU_S1DN, &
                             'Wrihis_structure_compound', 'cmpstru_s1dn', 'Water level downstream of cmpstru', 'sea_surface_height', &
                             'm', UNC_LOC_CMPSTRU)
      call add_output_config(config_set_his, IDX_HIS_CMPSTRU_HEAD, &
                             'Wrihis_structure_compound', 'cmpstru_head', 'Head difference across cmpstru', '', &
                             'm', UNC_LOC_CMPSTRU)
      call add_output_config(config_set_his, IDX_HIS_CMPSTRU_FLOW_AREA, &
                             'Wrihis_structure_compound', 'cmpstru_flow_area', 'Flow area at cmpstru', '', &
                             'm2', UNC_LOC_CMPSTRU)
      call add_output_config(config_set_his, IDX_HIS_CMPSTRU_VELOCITY, &
                             'Wrihis_structure_compound', 'cmpstru_velocity', 'Velocity through cmpstru', '', &
                             'm s-1', UNC_LOC_CMPSTRU)

      !! Long culvert
      call ncu_set_att(atts(1), 'geometry', 'longculvert_geom')

      call add_output_config(config_set_his, IDX_HIS_LONGCULVERT_DISCHARGE, &
                             'Wrihis_structure_longculvert', 'longculvert_discharge', 'Discharge through longculvert', '', &
                             'm3 s-1', UNC_LOC_LONGCULVERT, nc_attributes=atts(1:1), description='Write long culvert parameters to his-file')
      call add_output_config(config_set_his, IDX_HIS_LONGCULVERT_S1UP, &
                             'Wrihis_structure_longculvert', 'longculvert_s1up', 'Water level upstream of longculvert', 'sea_surface_height', &
                             'm', UNC_LOC_LONGCULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_LONGCULVERT_S1DN, &
                             'Wrihis_structure_longculvert', 'longculvert_s1dn', 'Water level downstream of longculvert', 'sea_surface_height', &
                             'm', UNC_LOC_LONGCULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_LONGCULVERT_HEAD, &
                             'Wrihis_structure_longculvert', 'longculvert_head', 'Head difference across longculvert', '', &
                             'm', UNC_LOC_LONGCULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_LONGCULVERT_FLOW_AREA, &
                             'Wrihis_structure_longculvert', 'longculvert_flow_area', 'Flow area at longculvert', '', &
                             'm2', UNC_LOC_LONGCULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_LONGCULVERT_VELOCITY, &
                             'Wrihis_structure_longculvert', 'longculvert_velocity', 'Velocity through longculvert', '', &
                             'm s-1', UNC_LOC_LONGCULVERT, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_LONGCULVERT_VALVE_RELATIVE_OPENING, &
                             'Wrihis_structure_longculvert', 'longculvert_valve_relative_opening', 'Valve relative opening in long culvert', '', &
                             '1', UNC_LOC_LONGCULVERT, nc_attributes=atts(1:1))

      !
      ! HIS: Output on observation stations
      !
      call ncu_set_att(atts(1), 'geometry', 'station_geom')

      ! Basic flow quantities.
      ! When specifying nc_dim_ids, always overspecify here (turn on everything that is possible),
      ! and turn off options in flow_init_statistical_output_his
      call add_output_config(config_set_his, IDX_HIS_WATERLEVEL, &
                             'Wrihis_waterlevel_s1', 'waterlevel', 'water level', 'sea_surface_height', &
                             'm', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write water level to his-file', &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_BEDLEVEL, &
                             'Wrihis_bedlevel', 'bedlevel', 'bottom level', '', &
                             'm', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write bed level to his-file', &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_WATERDEPTH, &
                             'Wrihis_waterdepth', 'waterdepth', 'water depth', '', &
                             'm', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write water depth to his-file', &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_X_VELOCITY, &
                             'Wrihis_velocity_vector', 'x_velocity', 'flow element center velocity vector, x-component', &
                             'sea_water_x_velocity', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write velocity vectors to his-file', &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_Y_VELOCITY, &
                             'Wrihis_velocity_vector', 'y_velocity', 'flow element center velocity vector, y-component', &
                             'sea_water_y_velocity', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_Z_VELOCITY, &
                             'Wrihis_velocity_vector', 'z_velocity', 'vertical/upward component of flow element center velocity vector', &
                             'upward_sea_water_velocity', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_DEPTH_AVERAGED_X_VELOCITY, &
                             'Wrihis_velocity_vector', 'depth-averaged_x_velocity', 'flow element center depth-averaged velocity vector, x-component', &
                             'sea_water_depth-averaged_x_velocity', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_DEPTH_AVERAGED_Y_VELOCITY, &
                             'Wrihis_velocity_vector', 'depth-averaged_y_velocity', 'flow element center depth-averaged velocity vector, y-component', &
                             'sea_water_depth-averaged_y_velocity', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_VELOCITY_MAGNITUDE, &
                             'Wrihis_velocity', 'velocity_magnitude', &
                             'velocity magnitude', &
                             'sea_water_speed', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write velocity magnitude to his-file', &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN, &
                             'Wrihis_velocity', 'velocity_magnitude', &
                             'Eulerian velocity magnitude', &
                             'sea_water_eulerian_speed', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_DISCHARGE_MAGNITUDE, &
                             'Wrihis_discharge', 'discharge_magnitude', &
                             'average discharge magnitude', &
                             'water_volume_transport_in_river_channel', 'm3 s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             description='Write discharge magnitude to his-file', nc_dim_ids=station_nc_dims_3D_center)

      ! Turbulence model
      call add_output_config(config_set_his, IDX_HIS_VIU, &
                             'Wrihis_turbulence', 'viu', 'horizontal eddy viscosity (flowlink-averaged) at pressure point ', '', &
                             'm2 s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_VICWWS, &
                             'Wrihis_turbulence', 'vicwws', 'turbulent vertical eddy viscosity at pressure point', '', &
                             'm2 s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_interface_center)
      call add_output_config(config_set_his, IDX_HIS_VICWWU, &
                             'Wrihis_turbulence', 'vicwwu', 'turbulent vertical eddy viscosity at nearest velocity point', '', &
                             'm2 s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_interface_edge)
      call add_output_config(config_set_his, IDX_HIS_TKIN, &
                             'Wrihis_turbulence', 'tke', 'turbulent kinetic energy at nearest velocity point', '', &
                             'm2 s-2', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write k, eps and viscosity to his-file', &
                             nc_dim_ids=station_nc_dims_3D_interface_edge)
      call add_output_config(config_set_his, IDX_HIS_EPS, &
                             'Wrihis_turbulence', 'eps', 'turbulent energy dissipation at nearest velocity point', '', &
                             'm2 s-3', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_interface_edge)
      call add_output_config(config_set_his, IDX_HIS_TAU, &
                             'Wrihis_turbulence', 'tau', 'turbulent time scale at nearest velocity point', '', &
                             's-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_interface_edge)
      call add_output_config(config_set_his, IDX_HIS_RICH, &
                             'Richardsononoutput', 'rich', 'Richardson number at nearest velocity point', &
                             '', '-', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_interface_edge)

      ! Gravity + buoyancy
      call add_output_config(config_set_his, IDX_HIS_SALINITY, &
                             'Wrihis_salinity', 'salinity', '', 'sea_water_salinity', &
                             '1e-3', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write salinity to his-file', &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_TEMPERATURE, &
                             'Wrihis_temperature', 'temperature', '', 'sea_water_temperature', &
                             'degC', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write temperature to his-file', &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_POTENTIAL_DENSITY, &
                             'Wrihis_density', 'potential_density', 'potential_density', '', &
                             'kg m-3', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write density to his-file', &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_DENSITY, &
                             'Wrihis_density', 'density', 'density', '', &
                             'kg m-3', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_BRUNT_VAISALA_N2, &
                             'Wrihis_density', 'Brunt_Vaisala_N2', 'Brunt_Vaisala_N2', '', &
                             '1/s2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_interface_center)

      ! Wave model
      call add_output_config(config_set_his, IDX_HIS_HWAV, &
                             'Wrihis_waves', 'hwav', 'Significant wave height', &
                             'sea_surface_wave_significant_wave_height', 'm', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             description='Write wave data to his-file', nc_dim_ids=station_nc_dims_2D)
      ! TODO: hwav sig vs. rms
      call add_output_config(config_set_his, IDX_HIS_TWAV, &
                             'Wrihis_waves', 'twav', 'Wave period', &
                             'sea_surface_wave_period', 's', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_PHIWAV, &
                             'Wrihis_waves', 'phiwav', 'Wave from direction', &
                             'sea_surface_wave_from_direction', 'deg from N', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_RLABDA, &
                             'Wrihis_waves', 'rlabda', 'Wave length', &
                             'sea_surface_wave_length', 'm', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_R, &
                             'Wrihis_waves', 'R', 'Roller energy per square meter', &
                             'sea_surface_bulk_roller_energy', 'J m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_UORB, &
                             'Wrihis_waves', 'uorb', 'Orbital velocity', &
                             'sea_surface_wave_orbital_velocity', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_USTOKES, &
                             'Wrihis_waves', 'ustokes', 'Stokes drift, x-component', &
                             'sea_surface_wave_stokes_drift_x', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_VSTOKES, &
                             'Wrihis_waves', 'vstokes', 'Stokes drift, y-component', &
                             'sea_surface_wave_stokes_drift_y', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_3D_center)
      call add_output_config(config_set_his, IDX_HIS_TAUSX, &
                             'Wrihis_taucurrent', 'tausx', &
                             'Mean bottom shear stress vector, x-component', &
                             'mean_bottom_shear_stress vector, x-component', 'Pa', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             description='Write mean bed shear stress to his-file', nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_TAUSY, &
                             'Wrihis_taucurrent', 'tausy', &
                             'Mean bottom shear stress vector, y-component', &
                             'mean_bottom_shear_stress vector, y-component', 'Pa', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)

      ! Meteo

      call add_output_config(config_set_his, IDX_HIS_PATM, 'Wrihis_wind', 'patm', 'atmospheric pressure', '', &
                             'N m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write wind velocities to his-file', &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_WINDX, &
                             'Wrihis_wind', 'windx', 'velocity of air on flow element center, x-component', 'x_wind', &
                             'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_WINDX_SFERIC, &
                             'Wrihis_wind', 'windx', 'velocity of air on flow element center, x-component', 'eastward_wind', &
                             'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_WINDY, &
                             'Wrihis_wind', 'windy', 'velocity of air on flow element center, y-component', 'y_wind', &
                             'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_WINDY_SFERIC, &
                             'Wrihis_wind', 'windy', 'velocity of air on flow element center, y-component', 'northward_wind', &
                             'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_RAIN, &
                             'Wrihis_rain', 'rain', 'precipitation depth per time unit', 'lwe_precipitation_rate', &
                             'mm day-1', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write precipitation to his-file', &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_INFILTRATION_CAP, &
                             'Wrihis_infiltration', 'infiltration_cap', 'Infiltration capacity', '', &
                             'mm hr-1', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write infiltration to his-file', &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL, &
                             'Wrihis_infiltration', 'infiltration_actual', 'Actual infiltration rate', '', &
                             'mm hr-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)

      ! Variable (computed) air density
      call add_output_config(config_set_his, IDX_HIS_AIR_DENSITY, &
                             'Wrihis_airdensity', 'rhoair', 'air density', '', &
                             'kg m-3', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)

      ! Heat flux model
      call add_output_config(config_set_his, IDX_HIS_WIND, &
                             'Wrihis_heat_fluxes', 'wind', 'windspeed', '', &
                             'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write heat fluxes to his-file', &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_TAIR, &
                             'Wrihis_heat_fluxes', 'Tair', 'air temperature', '', &
                             'degC', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_RHUM, &
                             'Wrihis_heat_fluxes', 'rhum', 'relative humidity', '', &
                             '', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_CLOU, &
                             'Wrihis_heat_fluxes', 'clou', 'cloudiness', '', &
                             ' ', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_QSUN, &
                             'Wrihis_heat_fluxes', 'Qsun', 'solar influx', '', &
                             'W m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_QEVA, &
                             'Wrihis_heat_fluxes', 'Qeva', 'evaporative heat flux', '', &
                             'W m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_QCON, &
                             'Wrihis_heat_fluxes', 'Qcon', 'sensible heat flux', '', &
                             'W m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_QLONG, &
                             'Wrihis_heat_fluxes', 'Qlong', 'long wave back radiation', '', &
                             'W m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_QFREVA, &
                             'Wrihis_heat_fluxes', 'Qfreva', 'free convection evaporative heat flux', '', &
                             'W m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_QFRCON, &
                             'Wrihis_heat_fluxes', 'Qfrcon', 'free convection sensible heat flux', '', &
                             'W m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_QTOT, &
                             'Wrihis_heat_fluxes', 'Qtot', 'total heat flux', '', &
                             'W m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=station_nc_dims_2D)

      ! Sediment model
      call add_output_config(config_set_his, IDX_HIS_SED, &
                             'Wrihis_sediment', 'sed', 'Sediment concentration', &
                             '', 'kg m-3', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write sediment transport to his-file', &
                             nc_dim_ids=t_station_nc_dimensions(laydim=.true., statdim=.true., sedsusdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_WS, &
                             'Wrihis_sediment', 'ws', 'Sediment settling velocity', &
                             '', 'm s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=t_station_nc_dimensions(laydim_interface_center=.true., statdim=.true., sedsusdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_SEDDIF, &
                             'Wrihis_sediment', 'seddif', 'Sediment vertical diffusion', &
                             '', 'm2 s-1', UNC_LOC_STATION, nc_attributes=atts(1:1), &
                             nc_dim_ids=t_station_nc_dimensions(laydim_interface_center=.true., statdim=.true., sedsusdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_BODSED, &
                             'Wrihis_sediment', 'bodsed', 'Available sediment mass in the bed', &
                             '', 'kg m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_DPSED, &
                             'Wrihis_sediment', 'dpsed', 'Sediment thickness in the bed', &
                             '', 'm', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_TAUB, &
                             'wrihis_sediment', 'taub', &
                             'Bed shear stress for morphology', &
                             '', 'Pa', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=station_nc_dims_2D)

      ! Bed composition variables
      call add_output_config(config_set_his, IDX_HIS_MSED, &
                             'wrihis_sediment', 'msed', &
                             'Available sediment mass in a layer of the bed', &
                             '', 'kg m-2', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(nlyrdim=.true., statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_THLYR, &
                             'wrihis_sediment', 'thlyr', &
                             'Thickness of a layer of the bed', &
                             '', 'm', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(nlyrdim=.true., statdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_POROS, &
                             'wrihis_sediment', 'poros', &
                             'Porosity of a layer of the bed', &
                             '', '', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(nlyrdim=.true., statdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_LYRFRAC, &
                             'wrihis_sediment', 'lyrfrac', &
                             'Volume fraction in a layer of the bed', &
                             '', 'm', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(nlyrdim=.true., statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_FRAC, &
                             'wrihis_sediment', 'frac', &
                             'Availability fraction in top layer', &
                             '', '', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_MUDFRAC, &
                             'wrihis_sediment', 'mudfrac', &
                             'Mud fraction in top layer', &
                             '', '', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_SANDFRAC, &
                             'wrihis_sediment', 'sandfrac', &
                             'Sand fraction in top layer', &
                             '', '', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=station_nc_dims_2D)
      call add_output_config(config_set_his, IDX_HIS_FIXFRAC, &
                             'wrihis_sediment', 'fixfac', &
                             'Reduction factor due to limited sediment thickness', &
                             '', '', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_HIDEXP, &
                             'wrihis_sediment', 'hidexp', &
                             'Hiding and exposure factor', &
                             '', '', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedtotdim=.true., timedim=.true.))
      call add_output_config(config_set_his, IDX_HIS_MFLUFF, &
                             'wrihis_sediment', 'mfluff', &
                             'Sediment mass in fluff layer', &
                             '', 'kg', UNC_LOC_STATION, nc_attributes=atts(1:1), nc_dim_ids=t_station_nc_dimensions(statdim=.true., sedsusdim=.true., timedim=.true.))

      ! The following output value is an abstract entry representing all water quality outputs.
      ! The actual output variables will only be added later, during init_fm_statistical_output_his().
      ! This is necessary for reading the key from the MDU file and providing the input value.
      call add_output_config(config_set_his, IDX_HIS_HWQ_ABSTRACT, 'Wrihis_water_quality_output', 'water_quality_output_abstract', &
                             '', '', '-', UNC_LOC_STATION, nc_attributes=atts(1:1), description='Write all water quality outputs to his-file', &
                             nc_dim_ids=station_nc_dims_3D_center)

      call add_output_config(config_set_his, IDX_HIS_TRACERS_ABSTRACT, &
                             'wrihis_constituents', 'station_tracer_abstract', '', &
                             '', '-', UNC_LOC_STATION, description='Write tracers to his-file')

      call add_output_config(config_set_his, IDX_HIS_WQBOT_ABSTRACT, &
                             'wriHis_wqBot', 'station_wqb_abstract', '', &
                             '', '-', UNC_LOC_STATION, description='Write waq bottom substances to his-file')

      call add_output_config(config_set_his, IDX_HIS_WQBOT3D_ABSTRACT, &
                             'wriHis_wqBot3d', 'station_wqb3d_abstract', '', &
                             '', '-', UNC_LOC_STATION, description='Write waq bottom 3D substances to his-file')

      ! HIS: Variables on observation cross sections
      !
      call ncu_set_att(atts(1), 'geometry', 'cross_section_geom')

      call add_output_config(config_set_his, IDX_HIS_OBSCRS_DISCHARGE, &
                             'Wrihis_crs_flow', 'cross_section_discharge', 'Discharge through observation cross section', &
                             '', 'm3 s-1', UNC_LOC_OBSCRS, nc_attributes=atts(1:1), description='Write data on observation cross sections to his-file')
      call add_output_config(config_set_his, IDX_HIS_OBSCRS_DISCHARGE_CUMUL, &
                             'Wrihis_crs_flow', 'cross_section_cumulative_discharge', 'Cumulative volume transport through observation cross section', &
                             '', 'm3', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_OBSCRS_AREA, &
                             'Wrihis_crs_flow', 'cross_section_area', 'Wet area of observation cross section', &
                             '', 'm2', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_OBSCRS_VELOCITY, &
                             'Wrihis_crs_flow', 'cross_section_velocity', 'Space-averaged velocity through observation cross section', &
                             '', 'm s-1', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))
      ! Disable writing cross_section_velocity_avg (see UNST-1148), because in a parallel run, it is impossible to compute
      ! summation of area (denominator) at each computational time step in a cheap way, i.e. without communication between
      ! partitions. @see subroutine: update_values_on_cross_sections
      ! TODO: UNST-7786 this is no longer the case, allow writing this? Where is it even disabled?

      ! The following output value is an abstract entry representing all constituents.
      ! The constituents that are actually available depend on the model initialization
      ! and will only be added later, during init_fm_statistical_output_his().
      call add_output_config(config_set_his, IDX_HIS_OBSCRS_CONST_ABSTRACT, &
                             'Wrihis_crs_constituents', 'cross_section_const_abstract', '', &
                             '', '-', UNC_LOC_OBSCRS, nc_attributes=atts(1:1), description='Write all transported constituents to his-file')

      ! Sediment model nr. 4
      call add_output_config(config_set_his, IDX_HIS_OBSCRS_SED_BTRANSPORT, &
                             'Wrihis_crs_sediment', 'cross_section_bedload_sediment_transport', &
                             'Cumulative bed load sediment transport', &
                             '', 'kg', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))

      call add_output_config(config_set_his, IDX_HIS_OBSCRS_SED_STRANSPORT, &
                             'Wrihis_crs_sediment', 'cross_section_suspended_sediment_transport', &
                             'Cumulative suspended load sediment transport', &
                             '', 'kg', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))

      ! The following two output values are abstract entries representing all bedload and suspended
      ! sediment fractions. The fractions that are actually available depend on the model initialization
      ! and will only be added later, during init_fm_statistical_output_his().
      call add_output_config(config_set_his, IDX_HIS_OBSCRS_SED_BTRANSPORT_PERFRAC_ABSTRACT, &
                             'Wrihis_crs_sediment', 'cross_section_bedload_sediment_transport_abstract', '', &
                             '', '-', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_OBSCRS_SED_STRANSPORT_PERFRAC_ABSTRACT, &
                             'Wrihis_crs_sediment', 'cross_section_suspended_sediment_transport_abstract', '', &
                             '', '-', UNC_LOC_OBSCRS, nc_attributes=atts(1:1))

      !
      ! HIS: Lateral discharges
      !
      call ncu_set_att(atts(1), 'geometry', 'lateral_geom')

      call add_output_config(config_set_his, IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS, &
                             'Wrihis_lateral', 'lateral_prescribed_discharge_instantaneous', &
                             'Prescribed discharge through lateral at current time step (instantaneous)', &
                             '', 'm3 s-1', UNC_LOC_LATERAL, nc_attributes=atts(1:1), description='Write lateral data to his-file')
      call add_output_config(config_set_his, IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE, &
                             'Wrihis_lateral', 'lateral_prescribed_discharge_average', &
                             'Prescribed discharge through lateral, average over the last history time interval', &
                             '', 'm3 s-1', UNC_LOC_LATERAL, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS, &
                             'Wrihis_lateral', 'lateral_realized_discharge_instantaneous', &
                             'Realized discharge through lateral at current time step (instantaneous)', &
                             '', 'm3 s-1', UNC_LOC_LATERAL, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE, &
                             'Wrihis_lateral', 'lateral_realized_discharge_average', &
                             'Realized discharge through lateral, average over the last history time interval', &
                             '', 'm3 s-1', UNC_LOC_LATERAL, nc_attributes=atts(1:1))

      ! HIS: Dredge & Dump variables
      !call add_output_config(config_set_his, IDX_HIS_DRED_AREA_NAME,                 &
      !               'Wrihis_dred', 'dredge_area_name',              &
      !               'dredge area identifier',             &
      !               '', '', UNC_LOC_DREDGE, nc_attributes = atts(1:1))
      !call add_output_config(config_set_his, IDX_HIS_DUMP_AREA_NAME,                 &
      !               'Wrihis_dred', 'dump_area_name',              &
      !               'dump area identifier',             &
      !               '', '', UNC_LOC_DUMP, nc_attributes = atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_DRED_LINK_DISCHARGE, &
                             'Wrihis_dred', 'dred_link_discharge', &
                             'Cumulative dredged material transported via links per fraction', &
                             '', 'm3', UNC_LOC_DRED_LINK, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_DRED_DISCHARGE, &
                             'Wrihis_dred', 'dred_discharge', &
                             'Cumulative dredged material for dredge areas', &
                             '', 'm3', UNC_LOC_DREDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_DUMP_DISCHARGE, &
                             'Wrihis_dred', 'dump_discharge', &
                             'Cumulative dredged material for dump areas', &
                             '', 'm3', UNC_LOC_DUMP, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_DRED_TIME_FRAC, &
                             'Wrihis_dred', 'dred_time_frac', &
                             'Time fraction spent dredging', &
                             '', '', UNC_LOC_DREDGE, nc_attributes=atts(1:1))
      call add_output_config(config_set_his, IDX_HIS_PLOUGH_TIME_FRAC, &
                             'Wrihis_dred', 'plough_time_frac', &
                             'Time fraction spent ploughing', &
                             '', '', UNC_LOC_DREDGE, nc_attributes=atts(1:1))
      !
      ! MAP:
      !
      call add_output_config(config_set_map, IDX_MAP_S0, &
                             'Wrimap_waterlevel_s0', 's0', 'Water level on previous timestep', &
                             'sea_surface_height', 'm', UNC_LOC_S, description='Write water levels for previous time step to map file')
      call add_output_config(config_set_map, IDX_MAP_S1, &
                             'Wrimap_waterlevel_s1', 's1', 'Water level', &
                             'sea_surface_height', 'm', UNC_LOC_S, description='Write water levels to map file')
      call add_output_config(config_set_map, IDX_MAP_POTEVAP, &
                             'Wrimap_evaporation', 'potevap', 'Potential evaporation rate at pressure points', &
                             'water_potential_evaporation_flux', 'm s-1', UNC_LOC_S, description='Write evaporation to map file')
      call add_output_config(config_set_map, IDX_MAP_ACTEVAP, &
                             'Wrimap_evaporation', 'actevap', 'Actual evaporation rate at pressure points', &
                             'lwe_water_evaporation_rate', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_PRESCREVAP, &
                             'Wrimap_evaporation', 'prescrevap', 'Prescribed evaporation rate at pressure points', &
                             '', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_VOL1, &
                             'Wrimap_volume1', 'vol1', 'volume of water in grid cell', &
                             '', 'm3', UNC_LOC_S, description='Write volumes to map file')
      call add_output_config(config_set_map, IDX_MAP_WATERDEPTH, &
                             'Wrimap_waterdepth', 'waterdepth', 'Water depth at pressure points', &
                             'sea_floor_depth_below_sea_surface', 'm', UNC_LOC_S, description='Write water depths to map file')
      call add_output_config(config_set_map, IDX_MAP_HU, &
                             'Wrimap_waterdepth_hu', 'hu', 'water depth at velocity points', &
                             'sea_floor_depth_below_sea_surface', 'm', UNC_LOC_U, description='Write water depths on u-points to map file')
      call add_output_config(config_set_map, IDX_MAP_NEGDPT, &
                             'Wrimap_flow_analysis', 'negdpt', 'Number of times negative depth was calculated', &
                             '', '1', UNC_LOC_S, description='Write flow analysis data to map file')
      call add_output_config(config_set_map, IDX_MAP_NEGDPT_CUM, &
                             'Wrimap_flow_analysis', 'negdpt_cum', 'Cumulative number of times negative depth was calculated', &
                             '', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_NOITER, &
                             'Wrimap_flow_analysis', 'noiter', 'Number of times no nonlinear convergence was caused', &
                             '', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_NOITER_CUM, &
                             'Wrimap_flow_analysis', 'noiter_cum', 'Cumulative number of times no nonlinear convergence was caused', &
                             '', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_LIMTSTEP, &
                             'Wrimap_flow_analysis', 'limtstep', 'Number of times a node was limiting for the computational time step', &
                             '', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_LIMTSTEP_CUM, &
                             'Wrimap_flow_analysis', 'limtstep_cum', 'Cumulative number of times a node was limiting for the computational time step', &
                             '', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_COURANT, &
                             'Wrimap_flow_analysis', 'courant', 'Courant number', &
                             '', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_AU, &
                             'Wrimap_flowarea_au', 'au', 'normal flow area between two neighbouring grid cells', &
                             '', 'm2', UNC_LOC_U, description='Write flow areas au to map file')

      call ncu_set_att(atts(1), 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      call add_output_config(config_set_map, IDX_MAP_U1, &
                             'Wrimap_velocity_component_u1', 'u1', 'Velocity at velocity point, n-component', &
                             '', 'm s-1', UNC_LOC_U, nc_attributes=atts(1:1), description='Write velocity component to map file')

      call ncu_set_att(atts(1), 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      call add_output_config(config_set_map, IDX_MAP_U0, 'Wrimap_velocity_component_u0', &
                             'u0', 'Velocity at velocity point at previous time step, n-component ', '', 'm s-1', UNC_LOC_U, nc_attributes=atts(1:1), &
                             description='Write velocity component for previous time step to map file')

      call add_output_config(config_set_map, IDX_MAP_UCXQ_EULERIAN, &
                             'Wrimap_velocity_vector', 'ucxq', 'Flow element center eulerian velocity vector based on discharge, x-component', &
                             'ucxq_eulerian_velocity', 'm s-1', UNC_LOC_S, description='Write cell-center velocity vectors to map file')
      call add_output_config(config_set_map, IDX_MAP_UCYQ_EULERIAN, &
                             'Wrimap_velocity_vector', 'ucyq', 'Flow element center eulerian velocity vector based on discharge, y-component', &
                             'ucyq_eulerian_velocity', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_UCXQ, &
                             'Wrimap_velocity_vector', 'ucxq', 'Flow element center velocity vector based on discharge, x-component', &
                             'ucxq_velocity', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_UCYQ, &
                             'Wrimap_velocity_vector', 'ucyq', 'Flow element center velocity vector based on discharge, y-component', &
                             'ucyq_velocity', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_UCMAG, &
                             'Wrimap_velocity_magnitude', 'ucmag', 'Flow element center velocity magnitude', &
                             'sea_water_speed', 'm s-1', UNC_LOC_S, description='Write cell-center velocity vector magnitude to map file')
      call add_output_config(config_set_map, IDX_MAP_UCMAG_EULER, &
                             'Wrimap_velocity_magnitude', 'ucmag', 'Flow element center eulerian velocity magnitude', &
                             'sea_water_eulerian_speed', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_UCMAGA_GLM, &
                             'Wrimap_velocity_magnitude', 'ucmaga', 'Flow element center depth-averaged GLM velocity magnitude', &
                             'sea_water_speed', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_UCMAGA, &
                             'Wrimap_velocity_magnitude', 'ucmaga', 'Flow element center depth-averaged velocity magnitude', &
                             'sea_water_speed', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WW1, &
                             'Wrimap_upward_velocity_component', 'ww1', 'Upward velocity on vertical interface, n-component', &
                             'upward_sea_water_velocity', 'm s-1', UNC_LOC_W, description='Write upward velocity component on cell interfaces')
      call add_output_config(config_set_map, IDX_MAP_RHO, &
                             'Wrimap_density_rho', 'rho', 'Flow element center mass density', &
                             'sea_water_density', 'kg m-3', UNC_LOC_S3D, description='Write flow density to map file')
      call add_output_config(config_set_map, IDX_MAP_VIU, &
                             'Wrimap_horizontal_viscosity_viu', 'viu', 'Horizontal eddy viscosity', &
                             '', 'm2 s-1', UNC_Loc_U, description='Write horizontal viscosity to map file')
      call add_output_config(config_set_map, IDX_MAP_DIU, &
                             'Wrimap_horizontal_diffusivity_diu', 'diu', 'Horizontal eddy diffusivity', &
                             '', 'm2 s-1', UNC_Loc_U, description='Write horizontal diffusivity to map file')

      call ncu_set_att(atts(1), 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      call add_output_config(config_set_map, IDX_MAP_Q1, &
                             'Wrimap_flow_flux_q1', 'q1', 'Discharge through flow link at current time', &
                             'discharge', 'm3 s-1', UNC_LOC_U, nc_attributes=atts(1:1), description='Write flow flux to map file')

      call ncu_set_att(atts(1), 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      call add_output_config(config_set_map, IDX_MAP_Q1_MAIN, &
                             'Wrimap_flow_flux_q1_main', 'q1_main', 'Main channel discharge through flow link at current time', &
                             '', 'm3 s-1', UNC_LOC_U, nc_attributes=atts(1:1), description='Write flow flux in main channel to map file')

      call add_output_config(config_set_map, IDX_MAP_FIXED_WEIR_ENERGY_LOSS, &
                             'Wrimap_fixed_weir_energy_loss', 'fixed weir energy loss', 'Fixed weir energy loss', &
                             '', 'm', UNC_LOC_U, description='Write fixed weir energy loss to map file')
      call add_output_config(config_set_map, IDX_MAP_SPIRCRV, &
                             'Wrimap_spiral_flow', 'spircrv', 'Flow streamline curvature', &
                             'streamline_curvature', '1/m', UNC_LOC_S, description='Write spiral flow to map file')
      call add_output_config(config_set_map, IDX_MAP_SPIRINT, &
                             'Wrimap_spiral_flow', 'spirint', 'Spiral flow intensity', &
                             'spiral_intensity', 'm/s', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_NUMLIMDT, &
                             'Wrimap_numlimdt', 'Numlimdt', 'Number of times flow element was Courant limiting', &
                             '', '1', UNC_LOC_S, description='Write the total number of times a cell was Courant limiting to map file. (Consider using Wrimap_flow_analysis instead.)')
      call add_output_config(config_set_map, IDX_MAP_TAUSX, &
                             'Wrimap_taucurrent', 'tausx', 'Total bed shear stress vector, x-component', &
                             '', 'N m-2', UNC_LOC_S, description='Write the shear stress to map file')
      call add_output_config(config_set_map, IDX_MAP_TAUSY, &
                             'Wrimap_taucurrent', 'tausy', 'Total bed shear stress vector, y-component', &
                             '', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_TAUS, &
                             'Wrimap_taucurrent', 'taus', 'Total bed shear stress magnitude', &
                             '', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_TAUSMAX, &
                             'Wrimap_taucurrent', 'tausmax', 'Bed shear stress magnitude for morphology', &
                             '', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_Z0UCUR, &
                             'Wrimap_z0', 'z0ucur', 'Current related roughness height', &
                             '', 'm', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_Z0UROU, &
                             'Wrimap_z0_wave', 'z0urou', 'Current-wave related roughness height', &
                             '', 'm', UNC_LOC_U, description='Write current-wave related roughness height to map file')
      call add_output_config(config_set_map, IDX_MAP_SA1, &
                             'Wrimap_salinity', 'sa1', 'Salinity in flow element', &
                             'sea_water_salinity', '1e-3', UNC_LOC_S, description='Write salinity to map file')
      call add_output_config(config_set_map, IDX_MAP_CZS, &
                             'Wrimap_chezy', 'czs', 'Chezy roughness in flow element center', &
                             '', 'm0.5s-1', UNC_LOC_S, description='Write the chezy values on flow links to map file')
      call add_output_config(config_set_map, IDX_MAP_CZU, &
                             'Wrimap_chezy_on_flow_links', 'czu', 'Chezy roughness on flow links', &
                             '', 'm0.5s-1', UNC_LOC_U, description='Write the input roughness on flow links to map file')
      call add_output_config(config_set_map, IDX_MAP_CFU, &
                             'Wrimap_input_roughness', 'cfu', 'Input roughness on flow links', &
                             '', '-', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_CFUTYP, &
                             'Wrimap_input_roughness', 'cfutyp', 'Input roughness type on flow links', &
                             '', '', UNC_LOC_U, id_nc_type=id_nc_int)
      call add_output_config(config_set_map, IDX_MAP_TEM1, &
                             'Wrimap_constituents', 'tem1', 'Temperature in flow element', &
                             'sea_water_temperature', 'degC', UNC_LOC_S, description='Write temperature to map file')
      call add_output_config(config_set_map, IDX_MAP_SED, &
                             'Wrimap_sediment', 'sed', 'Sediment concentration', &
                             '', '-', UNC_LOC_S, description='Write sediment concentrations to map file')
      call add_output_config(config_set_map, IDX_MAP_CONST, &
                             'Wrimap_sediment', 'const', 'Constituents', &
                             '', '-', UNC_LOC_S, description='Write constiuents to map file')
      call add_output_config(config_set_map, IDX_MAP_TURKIN1, &
                             'Wrimap_turbulence', 'turkin1', 'turbulent kinetic energy', &
                             'specific_turbulent_kinetic_energy_of_sea_water', 'm2 s-2', UNC_LOC_WU)
      call add_output_config(config_set_map, IDX_MAP_VICWWU, &
                             'Wrimap_turbulence', 'vicwwu', 'turbulent vertical eddy viscosity', &
                             'eddy_viscosity', 'm2 s-1', UNC_LOC_WU)
      call add_output_config(config_set_map, IDX_MAP_TUREPS1, &
                             'Wrimap_turbulence', 'tureps1', 'turbulent energy dissipation', &
                             'specific_turbulent_kinetic_energy_dissipation_in_sea_water', 'm2 s-3', UNC_LOC_WU)
      call add_output_config(config_set_map, IDX_MAP_TUREPS1_3, &
                             'Wrimap_turbulence', 'tureps1', 'turbulent energy dissipation', &
                             'specific_turbulent_kinetic_energy_dissipation_in_sea_water', 'm2 s-3', UNC_LOC_WU)
      call add_output_config(config_set_map, IDX_MAP_TUREPS1_4, &
                             'Wrimap_turbulence', 'tureps1', 'turbulent time scale', &
                             '', 's-1', UNC_LOC_WU)

      call ncu_set_att(atts(1), 'non_si_units', 'm0.5 s-1')
      call add_output_config(config_set_map, IDX_MAP_CFRT_0, &
                             'Wrimap_trachytopes', 'cfrt', 'Chezy roughness from trachytopes', &
                             '', '', UNC_LOC_L, nc_attributes=atts(1:1))

      call ncu_set_att(atts(1), 'non_si_units', 's m-0.333')
      call add_output_config(config_set_map, IDX_MAP_CFRT_1, &
                             'Wrimap_trachytopes', 'cfrt', 'Manning roughness from trachytopes', &
                             '', '', UNC_LOC_L, nc_attributes=atts(1:1), description='Write trachytope roughnesses to map file')

      call add_output_config(config_set_map, IDX_MAP_CFRT_2, &
                             'Wrimap_trachytopes', 'cfrt', 'White-Colebrook roughness from trachytopes', &
                             '', 'm', UNC_LOC_L)

      call ncu_set_att(atts(1), 'non_si_units', '')
      call add_output_config(config_set_map, IDX_MAP_CFRT, &
                             'Wrimap_trachytopes', 'cfrt', 'Roughness from trachytopes', &
                             '', '', UNC_LOC_L, nc_attributes=atts(1:1))

      call ncu_set_att(atts(1), 'non_si_units', 'm0.5 s-1')
      call add_output_config(config_set_map, IDX_MAP_CFCL, &
                             'Wrimap_calibration', 'cfcl', 'Calibration factor for roughness', &
                             '', '1', UNC_LOC_L, nc_attributes=atts(1:1), description='Write roughness calibration factors to map file')

      call add_output_config(config_set_map, IDX_MAP_RAINFALL_RATE, &
                             'Wrimap_rain', 'rainfall_rate', 'Rainfall rate', &
                             'rainfall_rate', 'm s-1', UNC_LOC_S, description='Write rainfall rates to map file')
      call add_output_config(config_set_map, IDX_MAP_INTERCEPTION_WATERDEPTH, &
                             'Wrimap_interception', 'interception_waterdepth', 'Waterdepth in interception layer', &
                             '', 'm', UNC_LOC_S, description='Write interception to map file')
      call add_output_config(config_set_map, IDX_MAP_PATM, &
                             'Wrimap_wind', 'Patm', 'Atmospheric pressure near surface', &
                             'surface_air_pressure', 'N m-2', UNC_LOC_S, description='Write wind velocities to map file')
      call add_output_config(config_set_map, IDX_MAP_WINDX, &
                             'Wrimap_wind', 'windx', 'velocity of air on flow element center, x-component', &
                             'x_wind', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WINDY, &
                             'Wrimap_wind', 'windy', 'velocity of air on flow element center, y-component', &
                             'y_wind', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WINDXU, &
                             'Wrimap_wind', 'windxu', 'velocity of air on flow links, x-component', &
                             'x_wind', 'm s-1', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_WINDYU, &
                             'Wrimap_wind', 'windyu', 'velocity of air on flow links, y-component', &
                             'y_wind', 'm s-1', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_WINDX_SFERIC, &
                             'Wrimap_wind', 'windx', 'velocity of air on flow element center, x-component', &
                             'eastward_wind', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WINDY_SFERIC, &
                             'Wrimap_wind', 'windy', 'velocity of air on flow element center, y-component', &
                             'northward_wind', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WINDXU_SFERIC, &
                             'Wrimap_wind', 'windxu', 'velocity of air on flow links, x-component', &
                             'eastward_wind', 'm s-1', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_WINDYU_SFERIC, &
                             'Wrimap_wind', 'windyu', 'velocity of air on flow links, y-component', &
                             'northward_wind', 'm s-1', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_WINDSTRESSX, &
                             'Wrimap_windstress', 'windstressx', 'wind stress on flow element center, x-component', &
                             'surface_downward_x_stress', 'N m-2', UNC_LOC_S, description='Write wind stress to map file')
      call add_output_config(config_set_map, IDX_MAP_WINDSTRESSY, &
                             'Wrimap_windstress', 'windstressy', 'wind stress on flow element center, y-component', &
                             'surface_downward_y_stress', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WINDSTRESSX_SFERIC, &
                             'Wrimap_windstress', 'windstressx', 'wind stress on flow element center, x-component', &
                             'surface_downward_eastward_stress', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WINDSTRESSY_SFERIC, &
                             'Wrimap_windstress', 'windstressy', 'wind stress on flow element center, y-component', &
                             'surface_downward_northward_stress', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_TAIR, &
                             'Wrimap_heat_fluxes', 'Tair', 'surface_temperature', &
                             'Air temperature near surface', 'degC', UNC_LOC_S, description='Write heat fluxes to map file')
      call add_output_config(config_set_map, IDX_MAP_RHUM, &
                             'Wrimap_heat_fluxes', 'Rhum', 'surface_specific_humidity', &
                             'Relative humidity near surface', '', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_CLOU, &
                             'Wrimap_heat_fluxes', 'Clou', 'cloud_area_fraction', &
                             'Cloudiness', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_QSUN, &
                             'Wrimap_heat_fluxes', 'Qsun', 'surface_net_downward_shortwave_flux', &
                             'Solar influx', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_QEVA, &
                             'Wrimap_heat_fluxes', 'Qeva', 'surface_downward_latent_heat_flux', &
                             'Evaporative heat flux', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_QCON, &
                             'Wrimap_heat_fluxes', 'Qcon', 'surface_downward_sensible_heat_flux', &
                             'Sensible heat flux', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_QLONG, &
                             'Wrimap_heat_fluxes', 'Qlong', 'surface_net_downward_longwave_flux', &
                             'Long wave back radiation', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_QFREVA, &
                             'Wrimap_heat_fluxes', 'Qfreva', 'downward_latent_heat_flux_in_sea_water_due_to_convection', &
                             'Free convection evaporative heat flux', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_QFRCON, &
                             'Wrimap_heat_fluxes', 'Qfrcon', 'surface_downward_sensible_heat_flux_due_to_convection', &
                             'Free convection sensible heat flux', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_QTOT, &
                             'Wrimap_heat_fluxes', 'Qtot', 'surface_downward_heat_flux_in_sea_water', &
                             'Total heat flux', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_TIDALPOTENTIAL, &
                             'Wrimap_tidal_potential', 'TidalPotential', 'TidalPotential', &
                             'Tidal Potential generated by celestial forces in flow element center', 'm2 s-2', UNC_LOC_S, &
                             description='Write tidal potential to map file')
      call add_output_config(config_set_map, IDX_MAP_SALPOTENTIAL, &
                             'Wrimap_sal_potential', 'SALPotential', 'SALPotential', &
                             'Self-attraction and loading Potential in flow element center', 'm2 s-2', UNC_LOC_S, &
                             description='Write self attraction and loading potential to map file')
      call add_output_config(config_set_map, IDX_MAP_INTERNAL_TIDES_DISSIPATION, &
                             'Wrimap_internal_tides_dissipation', 'internal_tides_dissipation', 'internal_tides_dissipation', &
                             'internal tides dissipation in flow element center', 'J s-1 m-2', UNC_LOC_S, &
                             description='Write internal tides dissipation to map file')
      call add_output_config(config_set_map, IDX_MAP_TNUDGE, &
                             'Wrimap_nudging', 'Tnudge', 'nudging_time', &
                             'Nudging relaxing time', 's', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_NUDGE_TEM, &
                             'Wrimap_nudging', 'nudge_tem', 'nudging_tem', &
                             'Nudging temperature', 'degC', UNC_LOC_S3D)
      call add_output_config(config_set_map, IDX_MAP_NUDGE_SAL, &
                             'Wrimap_nudging', 'nudge_sal', 'nudging_sal', &
                             'Nudging salinity', '1e-3', UNC_LOC_S3D)
      call add_output_config(config_set_map, IDX_MAP_NUDGE_DTEM, &
                             'Wrimap_nudging', 'nudge_Dtem', 'nudging_Dtem', &
                             'Difference of nudging temperature with temperature', 'degC', UNC_LOC_S3D)
      call add_output_config(config_set_map, IDX_MAP_NUDGE_DSAL, &
                             'Wrimap_nudging', 'nudge_Dsal', 'nudging_Dsal', &
                             'Difference of nudging salinity with salinity', '1e-3', UNC_LOC_S3D)
      call add_output_config(config_set_map, IDX_MAP_HWAV, &
                             'Wrimap_waves', 'hwav', 'RMS wave height', &
                             'sea_surface_wave_rms_height', 'm', UNC_LOC_S, description='Write wave information to map file')
      call add_output_config(config_set_map, IDX_MAP_HWAV_SIG, &
                             'Wrimap_waves', 'hwav', 'Significant wave height', &
                             'sea_surface_wave_significant_wave_height', 'm', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_TP, &
                             'Wrimap_waves', 'tp', 'Peak wave period', &
                             '', 's', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_DIR, &
                             'Wrimap_waves', 'dir', 'Mean direction of wave propagation relative to ksi-dir. ccw', &
                             '', 'deg', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SXWAV, &
                             'Wrimap_waves', 'sxwav', 'Surface layer wave forcing term, x-component', &
                             'sea_surface_x_wave_force_surface', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SYWAV, &
                             'Wrimap_waves', 'sywav', 'Surface layer wave forcing term, y-component', &
                             'sea_surface_y_wave_force_surface', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SYBWAV, &
                             'Wrimap_waves', 'sybwav', 'Bottom layer wave forcing term, y-component', &
                             'sea_surface_y_wave_force_bottom', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_MX, &
                             'Wrimap_waves', 'mx', 'Wave-induced volume flux in x-direction', &
                             '', 'm3 s-1 m-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_MY, &
                             'Wrimap_waves', 'my', 'Wave-induced volume flux in y-direction', &
                             '', 'm3 s-1 m-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_DISSURF, &
                             'Wrimap_waves', 'dissurf', 'Wave energy dissipation rate at the free surface', &
                             '', 'w m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_DISWCAP, &
                             'Wrimap_waves', 'diswcap', 'Wave energy dissipation rate due to white capping', &
                             '', 'w m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_UORB, &
                             'Wrimap_waves', 'uorb', 'Wave orbital velocity', &
                             'sea_surface_wave_orbital_velocity', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_E, &
                             'Wrimap_waves', 'E', 'Wave energy per square meter', &
                             'sea_surface_bulk_wave_energy', 'J m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_R, &
                             'Wrimap_waves', 'R', 'Roller energy per square meter', &
                             'sea_surface_bulk_roller_energy', 'J m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_DR, &
                             'Wrimap_waves', 'DR', 'Roller energy dissipation per square meter', &
                             'sea_surface_bulk_roller_dissipation', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_D, &
                             'Wrimap_waves', 'D', 'Wave breaking energy dissipation per square meter', &
                             'sea_surface_wave_breaking_dissipation', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_DF, &
                             'Wrimap_waves', 'Df', 'Wave bottom energy dissipation per square meter', &
                             'sea_surface_wave_bottom_dissipation', 'W m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SXX, &
                             'Wrimap_waves', 'Sxx', 'Radiation stress, x-component', &
                             '', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SYY, &
                             'Wrimap_waves', 'Syy', 'Radiation stress, y-component', &
                             '', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SXY, &
                             'Wrimap_waves', 'Sxy', 'Radiation stress, xy-component', &
                             'sea_surface_wave_radiation_stress_NE', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_CWAV, &
                             'Wrimap_waves', 'cwav', 'Sea_surface_wave_phase_celerity', &
                             'sea_surface_wave_phase_celerity', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_CGWAV, &
                             'Wrimap_waves', 'cgwav', 'Sea_surface_wave_group_celerity', &
                             'sea_surface_wave_group_celerity', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SIGMWAV, &
                             'Wrimap_waves', 'sigmwav', 'Sea_surface_wave_mean_frequency', &
                             'sea_surface_wave_mean_frequency', 'rad s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_KWAV, &
                             'Wrimap_waves', 'kwav', 'Sea_surface_wave_wavenumber', &
                             'sea_surface_wave_wavenumber', 'rad m-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_NWAV, &
                             'Wrimap_waves', 'nwav', 'Sea_surface_wave_ratio_group_phase_speed', &
                             'sea_surface_wave_cg_over_c', '1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_CTHETA, &
                             'Wrimap_waves', 'ctheta', 'Sea_surface_wave_refraction_celerity', &
                             'sea_surface_wave_refraction_celerity', 'rad s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_L1, &
                             'Wrimap_waves', 'L1', 'Sea_surface_wave_wavelength', &
                             'sea_surface_wave_wavelength', 'm', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SWE, &
                             'Wrimap_waves', 'SwE', 'wind source term on wave energy', &
                             'source_term_wind_on_E', 'J m-2 s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SWT, &
                             'Wrimap_waves', 'SwT', 'wind source term on wave period', &
                             'source_term_wind_on_T', 's s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_SXBWAV, &
                             'Wrimap_waves', 'sxbwav', 'Water body wave forcing term, x-component', &
                             'sea_surface_x_wave_force_bottom', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_UST_CC, &
                             'Wrimap_waves', 'ust_cc', 'Stokes drift, x-component', &
                             'sea_surface_x_stokes_drift', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_VST_CC, &
                             'Wrimap_waves', 'vst_cc', 'Stokes drift, y-component', &
                             'sea_surface_y_stokes_drift', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_USTOKES, &
                             'Wrimap_waves', 'ustokes', 'Stokes drift, n-component', &
                             '', 'm s-1', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_VSTOKES, &
                             'Wrimap_waves', 'vstokes', 'Stokes drift, t-component', &
                             '', 'm s-1', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_THETAMEAN, &
                             'Wrimap_waves', 'thetamean', 'Wave from direction', &
                             'sea_surface_wave_from_direction', 'deg from N', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_TWAV, &
                             'Wrimap_waves', 'twav', 'Wave period', &
                             'sea_surface_wave_period', 's', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_FX, &
                             'Wrimap_waves', 'Fx', 'Wave force, x-component', &
                             'sea_surface_x_wave_force', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_FY, &
                             'Wrimap_waves', 'Fy', 'Wave force, y-component', &
                             'sea_surface_y_wave_force', 'N m-2', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WAVFU, &
                             'Wrimap_waves', 'wavfu', 'Wave force at velocity point, n-component', &
                             '', 'N m-2', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_WAVFV, &
                             'Wrimap_waves', 'wavfv', 'Wave force at velocity point, t-component', &
                             '', 'N m-2', UNC_LOC_U)
      call add_output_config(config_set_map, IDX_MAP_DTCELL, &
                             'Wrimap_DTcell', 'dtcell', 'Time step per cell based on CFL', &
                             '', 's', UNC_LOC_S, description='Write time step per cell based on CFL')
      call add_output_config(config_set_map, IDX_MAP_TIME_WATER_ON_GROUND, &
                             'Wrimap_time_water_on_ground', 'time_water_on_ground', 'Cumulative time water above ground level', &
                             '', 's', UNC_LOC_S, description='Write cumulative time when water is above ground level to map file, only for 1D nodes')
      call add_output_config(config_set_map, IDX_MAP_FREEBOARD, &
                             'Wrimap_freeboard', 'freeboard', 'Freeboard', &
                             '', 'm', UNC_LOC_S, description='Write freeboard to map file, only for 1D nodes')
      call add_output_config(config_set_map, IDX_MAP_WATERDEPTH_ON_GROUND, &
                             'Wrimap_waterdepth_on_ground', 'waterdepth_on_ground', 'Waterdepth above ground level', &
                             '', 'm', UNC_LOC_S, description='Write waterdepth that is above ground level to map file, only for 1D nodes')
      call add_output_config(config_set_map, IDX_MAP_VOLUME_ON_GROUND, &
                             'Wrimap_volume_on_ground', 'volume_on_ground', 'Volume above ground level', &
                             '', 'm3', UNC_LOC_S, description='Write volume that is above ground level to map file, only for 1D nodes')
      call add_output_config(config_set_map, IDX_MAP_CURRENT_TOTAL_NET_INFLOW_1D2D, &
                             'Wrimap_total_net_inflow_1d2d', 'current_total_net_inflow_1d2d', 'Current total net inflow via all connected 1d2d links at each 1D node', &
                             '', 'm3 s-1', UNC_LOC_S, description='Write current total 1d2d net inflow (discharge) and cumulative total 1d2d net inflow (volume) to map file, only for 1D nodes')
      call add_output_config(config_set_map, IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_1D2D, &
                             'Wrimap_total_net_inflow_1d2d', 'cumulative_total_net_inflow_1d2d', 'Cumulative total net inflow via all connected 1d2d links at each 1D node', &
                             '', 'm3', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_CURRENT_TOTAL_NET_INFLOW_LATERAL, &
                             'Wrimap_total_net_inflow_lateral', 'current_total_net_inflow_lateral', 'Current total net inflow via all laterals at each 1D node', &
                             '', 'm3 s-1', UNC_LOC_S, description='Write current total lateral net inflow (discharge) and cumulative total net lateral inflow (volume) to map file, only for 1D nodes')
      call add_output_config(config_set_map, IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_LATERAL, &
                             'Wrimap_total_net_inflow_lateral', 'cumulative_total_net_inflow_lateral', 'Cumulative total net inflow via all laterals at each 1D node', &
                             '', 'm3', UNC_LOC_S)
      call add_output_config(config_set_map, IDX_MAP_WATER_LEVEL_GRADIENT, &
                             'Wrimap_water_level_gradient', 'water_level_gradient', 'Water level gradient at each 1D flow link', &
                             '', '1', UNC_LOC_U, description='Write water level gradient to map file, only on 1D links')
      call add_output_config(config_set_map, IDX_MAP_QIN, &
                             'Wrimap_Qin', 'qin', 'Sum of all water influx', &
                             '', 'm3 s-1', UNC_LOC_S, description='Write sum of all influxes to map file')
      call add_output_config(config_set_clm, IDX_CLS_S1, &
                             'WriClass_Waterlevel', 's1', 'Water level', &
                             'sea_surface_height', 'm', UNC_LOC_S, description='Write waterlevel to class map file')
      call add_output_config(config_set_clm, IDX_CLS_WATERDEPTH, &
                             'WriClass_Waterdepth', 'waterdepth', 'Water depth at pressure points', &
                             'sea_floor_depth_below_sea_surface', 'm', UNC_LOC_S)
      call add_output_config(config_set_clm, IDX_CLS_UCMAG, &
                             'WriClass_Velocity', 'ucmag', 'Flow element center velocity magnitude', &
                             'sea_water_speed', 'm s-1', UNC_LOC_S, description='Write center velocity to class map file')
      call add_output_config(config_set_clm, IDX_CLS_UCMAG_EULER, &
                             'WriClass_Velocity', 'ucmag', 'Flow element center Eulerian velocity magnitude', &
                             'sea_water_eulerian_speed', 'm s-1', UNC_LOC_S)
      call add_output_config(config_set_clm, IDX_CLS_UCDIR, &
                             'WriClass_Velocity', 'ucdir', 'Flow element center velocity direction', &
                             'sea_water_velocity_to_direction', 'degree', UNC_LOC_S)
      call add_output_config(config_set_clm, IDX_CLS_UCDIR_EULER, &
                             'WriClass_Velocity', 'ucdir', 'Flow element center Eulerian velocity direction', &
                             'sea_water_eulerian_velocity_to_direction', 'degree', UNC_LOC_S)

   end subroutine default_fm_statistical_output

   !> Initializes the output variable set for history files, based on
   !! the quantity configuration set and the active model settings.
   !! Must be called as part of flow_modelinit.
   subroutine flow_init_statistical_output_his(output_config_set, output_set)
      use m_ug_nc_attribute
      use m_flow
      use fm_external_forcings_data
      use m_structures
      use m_observations_data
      use m_physcoef, only: apply_thermobaricity
      use m_statistical_output_types, only: process_data_interface_double
      use m_transport, only: NUMCONST, itemp, isalt, ised1
      use m_sediment, only: stm_included, stmpar
      use m_longculverts, only: nlongculverts
      use m_monitoring_crosssections, only: ncrs
      use m_monitoring_runupgauges, only: num_rugs, rug
      use m_fm_wq_processes, only: jawaqproc, numwqbots
      use processes_input, only: num_wq_user_outputs => noout_user
      use m_dad, only: dad_included, dadpar
      use m_laterals, only: numlatsg, qplat, qplatAve, qLatRealAve, qLatReal
      use m_sferic, only: jsferic
      use m_wind, only: japatm, jawind, jarain, ja_airdensity, ja_computed_airdensity, clou, rhum
      use, intrinsic :: iso_c_binding

      type(t_output_quantity_config_set), intent(inout) :: output_config_set !< output config for which an output set is needed.
      type(t_output_variable_set), intent(inout) :: output_set !< output set that items need to be added to

      real(dp), pointer, dimension(:) :: temp_pointer

      procedure(process_data_interface_double), pointer :: function_pointer => null()

      integer :: i, ntot, num_const_items, nlyrs, variable_index, start_index, num_layers
      integer, allocatable, dimension(:) :: idx_his_hwq
      integer, allocatable, dimension(:) :: idx_constituents_crs, idx_tracers_stations
      integer, allocatable, dimension(:) :: idx_wqbot_stations, idx_wqbot3D_stations
      
      ntot = numobs + nummovobs
      !
      ! Mass balance variables
      !
      if (jahisbal > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VOLTOT), voltot(IDX_VOLTOT:IDX_VOLTOT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_STOR), voltot(IDX_HIS_STOR:IDX_HIS_STOR))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VOLERR), voltot(IDX_HIS_VOLERR:IDX_HIS_VOLERR))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BNDIN), voltot(IDX_HIS_BNDIN:IDX_HIS_BNDIN))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BNDOUT), voltot(IDX_HIS_BNDOUT:IDX_HIS_BNDOUT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BNDTOT), voltot(IDX_HIS_BNDTOT:IDX_HIS_BNDTOT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXCHIN), voltot(IDX_HIS_EXCHIN:IDX_HIS_EXCHIN))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXCHOUT), voltot(IDX_HIS_EXCHOUT:IDX_HIS_EXCHOUT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXCHTOT), voltot(IDX_HIS_EXCHTOT:IDX_HIS_EXCHTOT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PRECIP_TOTAL), voltot(IDX_HIS_PRECIP_TOTAL:IDX_HIS_PRECIP_TOTAL))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EVAP), voltot(IDX_HIS_EVAP:IDX_HIS_EVAP))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SOUR), voltot(IDX_HIS_SOUR:IDX_HIS_SOUR))
         if (jaFrcInternalTides2D == 1) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_INTERNALTIDESDISSIPATION), voltot(IDX_HIS_INTERNALTIDESDISSIPATION:IDX_HIS_INTERNALTIDESDISSIPATION))
         end if
         if (jatidep > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GravInput), voltot(IDX_HIS_GravInput:IDX_HIS_GravInput))
         end if
         if (jaselfal > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SalInput), voltot(IDX_HIS_SalInput:IDX_HIS_SalInput))
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SalInput2), voltot(IDX_HIS_SalInput2:IDX_HIS_SalInput2))
         end if
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GRWIN), voltot(IDX_HIS_GRWIN:IDX_HIS_GRWIN))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GRWOUT), voltot(IDX_HIS_GRWOUT:IDX_HIS_GRWOUT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GRWTOT), voltot(IDX_HIS_GRWTOT:IDX_HIS_GRWTOT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATIN), voltot(IDX_HIS_LATIN:IDX_HIS_LATIN))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATOUT), voltot(IDX_HIS_LATOUT:IDX_HIS_LATOUT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATTOT), voltot(IDX_HIS_LATTOT:IDX_HIS_LATTOT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATIN1D), voltot(IDX_HIS_LATIN1D:IDX_HIS_LATIN1D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATOUT1D), voltot(IDX_HIS_LATOUT1D:IDX_HIS_LATOUT1D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATTOT1D), voltot(IDX_HIS_LATTOT1D:IDX_HIS_LATTOT1D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATIN2D), voltot(IDX_HIS_LATIN2D:IDX_HIS_LATIN2D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATOUT2D), voltot(IDX_HIS_LATOUT2D:IDX_HIS_LATOUT2D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATTOT2D), voltot(IDX_HIS_LATTOT2D:IDX_HIS_LATTOT2D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTIN), voltot(IDX_HIS_EXTIN:IDX_HIS_EXTIN))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTOUT), voltot(IDX_HIS_EXTOUT:IDX_HIS_EXTOUT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTTOT), voltot(IDX_HIS_EXTTOT:IDX_HIS_EXTTOT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTIN1D), voltot(IDX_HIS_EXTIN1D:IDX_HIS_EXTIN1D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTOUT1D), voltot(IDX_HIS_EXTOUT1D:IDX_HIS_EXTOUT1D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTTOT1D), voltot(IDX_HIS_EXTTOT1D:IDX_HIS_EXTTOT1D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTIN2D), voltot(IDX_HIS_EXTIN2D:IDX_HIS_EXTIN2D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTOUT2D), voltot(IDX_HIS_EXTOUT2D:IDX_HIS_EXTOUT2D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EXTTOT2D), voltot(IDX_HIS_EXTTOT2D:IDX_HIS_EXTTOT2D))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ICEPT), voltot(IDX_HIS_ICEPT:IDX_HIS_ICEPT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EVAP_ICEPT), voltot(IDX_HIS_EVAP_ICEPT:IDX_HIS_EVAP_ICEPT))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PRECIP_GROUND), voltot(IDX_HIS_PRECIP_GROUND:IDX_HIS_PRECIP_GROUND))
      end if

      !
      ! Source-sink variables
      !
      if (jahissourcesink > 0 .and. numsrc > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SOURCE_SINK_PRESCRIBED_DISCHARGE), qstss(1:(numconst + 1) * numsrc:(numconst + 1)))
         i = 1
         if (isalt > 0) then
            i = i + 1
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SOURCE_SINK_PRESCRIBED_SALINITY_INCREMENT), qstss(i:(numconst + 1) * numsrc:(numconst + i)))
         end if
         if (itemp > 0) then
            i = i + 1
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SOURCE_SINK_PRESCRIBED_TEMPERATURE_INCREMENT), qstss(i:(numconst + 1) * numsrc:(numconst + i)))
         end if
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SOURCE_SINK_CURRENT_DISCHARGE), qsrc)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SOURCE_SINK_CUMULATIVE_VOLUME), vsrccum)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SOURCE_SINK_DISCHARGE_AVERAGE), qsrcavg)
      end if

      !
      ! Run-up gauge variables
      !
      if (num_rugs > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_RUG_RUHEIGHT), rug(:)%max_rug_height)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_RUG_RUX), rug(:)%max_x)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_RUG_RUY), rug(:)%max_y)
      end if

      !
      ! Hydraulic structures variables
      !
      if (jahiscgen > 0 .and. ngenstru > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE), valgenstru(IVAL_DIS, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_CREST_LEVEL), valgenstru(IVAL_CRESTL, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_GATE_LOWER_EDGE_LEVEL), valgenstru(IVAL_EDGEL, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_WIDTH), valgenstru(IVAL_OPENW, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_S1UP), valgenstru(IVAL_S1UP, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_S1DN), valgenstru(IVAL_S1DN, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_HEAD), valgenstru(IVAL_HEAD, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA), valgenstru(IVAL_AREA, 1:ngenstru))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_VELOCITY), valgenstru(IVAL_VEL, 1:ngenstru))
      end if
      if (network%sts%numGeneralStructures > 0) then ! write extra fields for new general structure
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_CREST_WIDTH), valgenstru(IVAL_CRESTW, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_THROUGH_GATE_OPENING), valgenstru(IVAL_DIS_OPEN, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_OVER_GATE), valgenstru(IVAL_DIS_OVER, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_UNDER_GATE), valgenstru(IVAL_DIS_UNDER, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_HEIGHT), valgenstru(IVAL_OPENH, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_GATE_UPPER_EDGE_LEVEL), valgenstru(IVAL_UPPL, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_THROUGH_GATE_OPENING), valgenstru(IVAL_VEL_OPEN, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_OVER_GATE), valgenstru(IVAL_VEL_OVER, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_VELOCITY_UNDER_GATE), valgenstru(IVAL_VEL_UNDER, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_IN_GATE_OPENING), valgenstru(IVAL_AREA_OPEN, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_OVER_GATE), valgenstru(IVAL_AREA_OVER, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_UNDER_GATE), valgenstru(IVAL_AREA_UNDER, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_STATE), valgenstru(IVAL_STATE, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_S1_ON_CREST), valgenstru(IVAL_S1ONCREST, 1:network%sts%numGeneralStructures))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GENERAL_STRUCTURE_FORCE_DIFFERENCE), valgenstru(IVAL_FORCEDIF, 1:network%sts%numGeneralStructures))
      end if
      if (jahiscdam > 0 .and. ncdamsg > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CDAM_DISCHARGE), valcdam(2, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CDAM_CREST_LEVEL), zcdam)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CDAM_S1UP), valcdam(3, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CDAM_S1DN), valcdam(4, :))
      end if
      if (jahispump > 0 .and. npumpsg > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_STRUCTURE_DISCHARGE), valpump(IVAL_DIS, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_CAPACITY), valpump(IVAL_PP_CAP, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_DISCHARGE_DIR), valpump(IVAL_PP_DISDIR, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_S1UP), valpump(IVAL_S1UP, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_S1DN), valpump(IVAL_S1DN, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_STRUCTURE_HEAD), valpump(IVAL_HEAD, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_ACTUAL_STAGE), valpump(IVAL_PP_STAG, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_HEAD), valpump(IVAL_PP_HEAD, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_REDUCTION_FACTOR), valpump(IVAL_PP_RED, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_S1_DELIVERY_SIDE), valpump(IVAL_PP_S1DEL, 1:npumpsg))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PUMP_S1_SUCTION_SIDE), valpump(IVAL_PP_S1SUC, 1:npumpsg))
      end if
      if (jahisgate > 0 .and. ngatesg > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATE_DISCHARGE), valgate(2, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATE_LOWER_EDGE_LEVEL), zgate)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATE_S1UP), valgate(3, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATE_S1DN), valgate(4, :))
      end if
      if (jahisgate > 0 .and. ngategen > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_DISCHARGE), valgategen(IVAL_DIS, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_CREST_LEVEL), valgategen(IVAL_GATE_SILLH, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_CREST_WIDTH), valgategen(IVAL_WIDTH, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_GATE_LOWER_EDGE_LEVEL), valgategen(IVAL_GATE_EDGEL, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_FLOW_THROUGH_HEIGHT), valgategen(IVAL_GATE_FLOWH, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_GATE_OPENING_WIDTH), valgategen(IVAL_GATE_OPENW, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_S1UP), valgategen(IVAL_S1UP, :))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_GATEGEN_S1DN), valgategen(IVAL_S1DN, :))
      end if
      if (jahisweir > 0 .and. nweirgen > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_DISCHARGE), valweirgen(IVAL_DIS, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_CREST_LEVEL), valweirgen(IVAL_CRESTL, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_CREST_WIDTH), valweirgen(IVAL_CRESTW, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_S1UP), valweirgen(IVAL_S1UP, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_S1DN), valweirgen(IVAL_S1DN, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_STRUCTURE_HEAD), valweirgen(IVAL_HEAD, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_VELOCITY), valweirgen(IVAL_VEL, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_FLOW_AREA), valweirgen(IVAL_AREA, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_STATE), valweirgen(IVAL_STATE, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_FORCE_DIFFERENCE), valweirgen(IVAL_FORCEDIF, 1:nweirgen))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WEIRGEN_S1_ON_CREST), valweirgen(IVAL_S1ONCREST, 1:nweirgen))
      end if
      if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_DISCHARGE), valorifgen(IVAL_DIS, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_CREST_LEVEL), valorifgen(IVAL_CRESTL, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_CREST_WIDTH), valorifgen(IVAL_CRESTW, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_GATE_LOWER_EDGE_LEVEL), valorifgen(IVAL_EDGEL, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_S1UP), valorifgen(IVAL_S1UP, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_S1DN), valorifgen(IVAL_S1DN, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_GATE_OPENING_HEIGHT), valorifgen(IVAL_OPENH, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_HEAD), valorifgen(IVAL_HEAD, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_FLOW_AREA), valorifgen(IVAL_AREA, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_STATE), valorifgen(IVAL_STATE, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_S1_ON_CREST), valorifgen(IVAL_S1ONCREST, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_VELOCITY), valorifgen(IVAL_VEL, 1:network%sts%numOrifices))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_ORIFICE_FORCE_DIFFERENCE), valorifgen(IVAL_FORCEDIF, 1:network%sts%numOrifices))
      end if
      if (jahisbridge > 0 .and. network%sts%numBridges > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_DISCHARGE), valbridge(IVAL_DIS, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_S1UP), valbridge(IVAL_S1UP, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_S1DN), valbridge(IVAL_S1DN, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_HEAD), valbridge(IVAL_HEAD, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_FLOW_AREA), valbridge(IVAL_AREA, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_VELOCITY), valbridge(IVAL_VEL, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_BLUP), valbridge(IVAL_BLUP, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_BLDN), valbridge(IVAL_BLDN, 1:network%sts%numBridges))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRIDGE_BL_ACTUAL), valbridge(IVAL_BLACTUAL, 1:network%sts%numBridges))
      end if
      if (jahisculv > 0 .and. network%sts%numCulverts > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_DISCHARGE), valculvert(IVAL_DIS, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_CREST_LEVEL), valculvert(IVAL_CL_CRESTL, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_GATE_LOWER_EDGE_LEVEL), valculvert(IVAL_CL_EDGEL, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_S1UP), valculvert(IVAL_S1UP, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_S1DN), valculvert(IVAL_S1DN, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_GATE_OPENING_HEIGHT), valculvert(IVAL_CL_OPENH, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_HEAD), valculvert(IVAL_HEAD, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_FLOW_AREA), valculvert(IVAL_AREA, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_VELOCITY), valculvert(IVAL_VEL, 1:network%sts%numCulverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CULVERT_STATE), valculvert(IVAL_CL_STATE, 1:network%sts%numCulverts))
      end if
      if (jahisdambreak > 0 .and. ndambreaksignals > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_S1UP), valdambreak(IVAL_S1UP, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_S1DN), valdambreak(IVAL_S1DN, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_DISCHARGE), valdambreak(IVAL_DIS, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_CUMULATIVE_DISCHARGE), valdambreak(IVAL_DB_DISCUM, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_VELOCITY), valdambreak(IVAL_VEL, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_HEAD), valdambreak(IVAL_HEAD, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_FLOW_AREA), valdambreak(IVAL_AREA, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_CREST_LEVEL), valdambreak(IVAL_DB_CRESTH, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_CREST_WIDTH), valdambreak(IVAL_DB_CRESTW, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_BREACH_WIDTH_TIME_DERIVATIVE), valdambreak(IVAL_DB_TIMEDIV, 1:ndambreaksignals))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DAMBREAK_WATER_LEVEL_JUMP), valdambreak(IVAL_DB_JUMP, 1:ndambreaksignals))
      end if
      if (jahisuniweir > 0 .and. network%sts%numuniweirs > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UNIWEIR_DISCHARGE), valuniweir(IVAL_DIS, 1:network%sts%numuniweirs))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UNIWEIR_CREST_LEVEL), valuniweir(IVAL_UW_CRESTL, 1:network%sts%numuniweirs))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UNIWEIR_S1UP), valuniweir(IVAL_S1UP, 1:network%sts%numuniweirs))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UNIWEIR_S1DN), valuniweir(IVAL_S1DN, 1:network%sts%numuniweirs))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UNIWEIR_HEAD), valuniweir(IVAL_HEAD, 1:network%sts%numuniweirs))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UNIWEIR_FLOW_AREA), valuniweir(IVAL_AREA, 1:network%sts%numuniweirs))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UNIWEIR_VELOCITY), valuniweir(IVAL_VEL, 1:network%sts%numuniweirs))
      end if
      if (jahiscmpstru > 0 .and. network%cmps%count > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CMPSTRU_DISCHARGE), valcmpstru(IVAL_DIS, 1:network%cmps%count))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CMPSTRU_S1UP), valcmpstru(IVAL_S1UP, 1:network%cmps%count))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CMPSTRU_S1DN), valcmpstru(IVAL_S1DN, 1:network%cmps%count))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CMPSTRU_HEAD), valcmpstru(IVAL_HEAD, 1:network%cmps%count))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CMPSTRU_FLOW_AREA), valcmpstru(IVAL_AREA, 1:network%cmps%count))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CMPSTRU_VELOCITY), valcmpstru(IVAL_VEL, 1:network%cmps%count))
      end if
      if (jahislongculv > 0 .and. nlongculverts > 0) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LONGCULVERT_DISCHARGE), vallongculvert(IVAL_DIS, 1:nlongculverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LONGCULVERT_S1UP), vallongculvert(IVAL_S1UP, 1:nlongculverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LONGCULVERT_S1DN), vallongculvert(IVAL_S1DN, 1:nlongculverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LONGCULVERT_HEAD), vallongculvert(IVAL_HEAD, 1:nlongculverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LONGCULVERT_FLOW_AREA), vallongculvert(IVAL_AREA, 1:nlongculverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LONGCULVERT_VELOCITY), vallongculvert(IVAL_VEL, 1:nlongculverts))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LONGCULVERT_VALVE_RELATIVE_OPENING), vallongculvert(IVAL_LC_VALVE, 1:nlongculverts))
      end if

      !
      ! Output on observation stations
      !

      ! Basic flow quantities
      if (model_has_obs_stations()) then
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WATERLEVEL), valobs(:, IPNT_S1))
         if (stm_included) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BEDLEVEL), valobs(:, IPNT_BL))
         end if
         if (jahiswatdep > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WATERDEPTH), valobs(:, IPNT_HS))
         end if
         if (jahisvelvec > 0) then
            if (model_is_3D()) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_UCX:IPNT_UCX + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_X_VELOCITY), temp_pointer)

               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_UCY:IPNT_UCY + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_Y_VELOCITY), temp_pointer)

               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_UCZ:IPNT_UCZ + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_Z_VELOCITY), temp_pointer)

               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DEPTH_AVERAGED_X_VELOCITY), valobs(:, IPNT_UCXQ))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DEPTH_AVERAGED_Y_VELOCITY), valobs(:, IPNT_UCYQ))
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_X_VELOCITY), valobs(:, IPNT_UCX))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_Y_VELOCITY), valobs(:, IPNT_UCY))
            end if
         end if
         if (jahisvelocity > 0) then
            if (jaeulervel == 0) then
               if (model_is_3D()) then
                  temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_UMAG:IPNT_UMAG + kmx - 1)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VELOCITY_MAGNITUDE), temp_pointer)
               else
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VELOCITY_MAGNITUDE), valobs(:, IPNT_UMAG))
               end if
            else
               if (model_is_3D()) then
                  temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_UMAG:IPNT_UMAG + kmx - 1)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN), temp_pointer)
               else
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN), valobs(:, IPNT_UMAG))
               end if
            end if
         end if
         if (jahisdischarge > 0) then
            if (model_is_3D()) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_QMAG:IPNT_QMAG + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DISCHARGE_MAGNITUDE), temp_pointer)
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DISCHARGE_MAGNITUDE), valobs(:, IPNT_QMAG))
            end if
         end if

         ! Turbulence model
         if (jahistur > 0) then
            if (model_is_3D()) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_VIU:IPNT_VIU + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VIU), temp_pointer)
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VIU), valobs(:, IPNT_VIU))
            end if
         end if
         if (model_is_3D()) then
            if (jahistur > 0) then
               if (iturbulencemodel >= 3) then
                  temp_pointer(1:(kmx + 1) * ntot) => valobs(1:ntot, IPNT_TKIN:IPNT_TKIN + kmx)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TKIN), temp_pointer)
               end if
               if (iturbulencemodel == 3) then
                  temp_pointer(1:(kmx + 1) * ntot) => valobs(1:ntot, IPNT_TEPS:IPNT_TEPS + kmx)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_EPS), temp_pointer)
               end if
               if (iturbulencemodel >= 2) then
                  temp_pointer(1:(kmx + 1) * ntot) => valobs(1:ntot, IPNT_VICWWS:IPNT_VICWWS + kmx)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VICWWS), temp_pointer)
                  temp_pointer(1:(kmx + 1) * ntot) => valobs(1:ntot, IPNT_VICWWU:IPNT_VICWWU + kmx)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VICWWU), temp_pointer)
               end if
               if (iturbulencemodel == 4) then
                  temp_pointer(1:(kmx + 1) * ntot) => valobs(1:ntot, IPNT_TEPS:IPNT_TEPS + kmx)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TAU), temp_pointer)
               end if
            end if
            if (idensform > 0 .and. jaRichardsononoutput > 0) then
               temp_pointer(1:(kmx + 1) * ntot) => valobs(1:ntot, IPNT_RICH:IPNT_RICH + kmx)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_RICH), temp_pointer)
            end if
         end if

         ! Gravity + buoyancy
         if (jasal > 0 .and. jahissal > 0) then
            if (model_is_3D()) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_SA1:IPNT_SA1 + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SALINITY), temp_pointer)
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SALINITY), valobs(:, IPNT_SA1))
            end if
         end if

         if (jatem > 0 .and. jahistem > 0) then
            if (model_is_3D()) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_TEM1:IPNT_TEM1 + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TEMPERATURE), temp_pointer)
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TEMPERATURE), valobs(:, IPNT_TEM1))
            end if
         end if

         if ((jasal > 0 .or. jatem > 0 .or. jased > 0) .and. jahisrho > 0) then
            if (model_is_3D()) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_RHOP:IPNT_RHOP + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_POTENTIAL_DENSITY), temp_pointer)
               if (apply_thermobaricity) then
                  temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_RHO:IPNT_RHO + kmx - 1)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DENSITY), temp_pointer)
               end if

               temp_pointer(1:(kmx + 1) * ntot) => valobs(1:ntot, IPNT_BRUV:IPNT_BRUV + kmx)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BRUNT_VAISALA_N2), temp_pointer)
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_POTENTIAL_DENSITY), valobs(:, IPNT_RHOP))
            end if
         end if

         ! Wave model
         if (jawave > 0 .and. jahiswav > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_HWAV), valobs(:, IPNT_WAVEH))
            !call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_HWAV_SIG),valobs(:,IPNT_HS)                                    )
            ! TODO: hwav sig vs. rms
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TWAV), valobs(:, IPNT_WAVET))
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PHIWAV), valobs(:, IPNT_WAVED))
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_RLABDA), valobs(:, IPNT_WAVEL))
            if (jawave == 4) then
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_R), valobs(:, IPNT_WAVER))
            end if
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_UORB), valobs(:, IPNT_WAVEU))
            if (model_is_3D() .and. .not. flowwithoutwaves) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_UCXST:IPNT_UCXST + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_USTOKES), temp_pointer)

               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_UCYST:IPNT_UCYST + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VSTOKES), temp_pointer)
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_USTOKES), valobs(:, IPNT_UCXST))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_VSTOKES), valobs(:, IPNT_UCYST))
            end if
         end if
         if (jahistaucurrent > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TAUSX), valobs(:, IPNT_TAUX))
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TAUSY), valobs(:, IPNT_TAUY))
         end if

         ! Meteo
         if (japatm > 0 .and. jahiswind > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PATM), valobs(:, IPNT_PATM))
         end if

         if (jawind > 0 .and. jahiswind > 0) then
            if (jsferic == 0) then
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WINDX), valobs(:, IPNT_wx))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WINDY), valobs(:, IPNT_wy))
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WINDX_SFERIC), valobs(:, IPNT_wx))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WINDY_SFERIC), valobs(:, IPNT_wy))
            end if
         end if

         if (jarain > 0 .and. jahisrain > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_RAIN), valobs(:, IPNT_rain))
         end if

         if ((infiltrationmodel == DFM_HYD_INFILT_CONST .or. infiltrationmodel == DFM_HYD_INFILT_HORTON) .and. jahisinfilt > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_INFILTRATION_CAP), valobs(:, IPNT_infiltcap))
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL), valobs(:, IPNT_infiltact))
         end if

         if (ja_airdensity + ja_computed_airdensity > 0 .and. jahis_airdensity > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_AIR_DENSITY), valobs(:, IPNT_AIRDENSITY))
         end if

         ! Heat flux model
         if (jatem > 1 .and. jahisheatflux > 0) then
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WIND), valobs(:, IPNT_WIND))
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TAIR), valobs(:, IPNT_TAIR))
            if (jatem == 5 .and. allocated(Rhum) .and. allocated(Clou)) then
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_RHUM), valobs(:, IPNT_RHUM))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_CLOU), valobs(:, IPNT_CLOU))
            end if
            if (jatem == 5) then
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_QSUN), valobs(:, IPNT_QSUN))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_QEVA), valobs(:, IPNT_QEVA))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_QCON), valobs(:, IPNT_QCON))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_QLONG), valobs(:, IPNT_QLON))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_QFREVA), valobs(:, IPNT_QFRE))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_QFRCON), valobs(:, IPNT_QFRC))
            end if
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_QTOT), valobs(:, IPNT_QTOT))
         end if

         ! Sediment model
         if (jased > 0 .and. .not. stm_included) then
            if (model_is_3D()) then
               temp_pointer(1:kmx * ntot) => valobs(1:ntot, IPNT_SED:IPNT_SED + kmx - 1)
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SED), temp_pointer)
            else
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SED), valobs(:, IPNT_SED))
            end if
         else if (stm_included .and. ISED1 > 0 .and. jahissed > 0 .and. IVAL_SF1 > 0) then
            if (model_is_3D()) then
               temp_pointer(1:(IVAL_SFN - IVAL_SF1 + 1) * kmx * ntot) => valobs(1:ntot, IPNT_SF1:IPNT_SF1 - 1 + (IVAL_SFN - IVAL_SF1 + 1) * kmx)
            else
               temp_pointer(1:(IVAL_SFN - IVAL_SF1 + 1) * ntot) => valobs(:, IPNT_SF1:IPNT_SFN)
            end if
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SED), temp_pointer)
         end if
         if (IVAL_WS1 > 0) then
            if (model_is_3D()) then
               temp_pointer(1:(IVAL_WSN - IVAL_WS1 + 1) * (kmx + 1) * ntot) => valobs(1:ntot, IPNT_WS1:IPNT_WS1 - 1 + (IVAL_WSN - IVAL_WS1 + 1) * (kmx + 1))
            else
               temp_pointer(1:(IVAL_WSN - IVAL_WS1 + 1) * ntot) => valobs(1:ntot, IPNT_WS1:IPNT_WSN)
            end if
            call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_WS), temp_pointer)
         end if
         if (IVAL_SEDDIF1 > 0) then
            if (model_is_3D()) then
               temp_pointer(1:(IVAL_SEDDIFN - IVAL_SEDDIF1 + 1) * (kmx + 1) * ntot) => valobs(1:ntot, IPNT_SEDDIF1:IPNT_SEDDIF1 - 1 + (IVAL_SEDDIFN - IVAL_SEDDIF1 + 1) * (kmx + 1))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SEDDIF), temp_pointer)
            end if
         end if

         if (jahissed > 0 .and. jased > 0 .and. stm_included) then
            if (stmpar%morpar%moroutput%taub) then
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_TAUB), valobs(1:ntot, IPNT_TAUB))
            end if
            if (stmpar%lsedtot > 0) then
               call add_station_sedtrans_configs(output_config_set)
               if (stmpar%morpar%moroutput%sbcuv) then
                  function_pointer => calculate_sediment_SBC
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SBCX), null(), function_pointer)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SBCY), SBCY)
               end if
               if (stmpar%morpar%moroutput%sscuv) then
                  function_pointer => calculate_sediment_SSC
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SSCX), null(), function_pointer)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SSCY), SSCY)
               end if
               if (stmpar%morpar%moroutput%sbwuv .and. jawave > 0 .and. .not. flowWithoutWaves) then
                  function_pointer => calculate_sediment_SBW
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SBWX), null(), function_pointer)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SBWY), SBWY)
               end if
               if (stmpar%morpar%moroutput%sswuv .and. jawave > 0 .and. .not. flowWithoutWaves) then
                  function_pointer => calculate_sediment_SSW
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SSWX), null(), function_pointer)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SSWY), SSWY)
               end if
            end if
         end if
         ! Bed composition variables
         if (jahissed > 0 .and. jased > 0 .and. stm_included) then
            select case (stmpar%morlyr%settings%iunderlyr)
            case (1)
               if (ISED1 > 0) then
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DPSED), valobs(:, IPNT_DPSED))
                  temp_pointer(1:(IVAL_BODSEDN - IVAL_BODSED1 + 1) * ntot) => valobs(:, IVAL_BODSED1:IVAL_BODSEDN)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_BODSED), temp_pointer)
               end if
            case (2)
               nlyrs = stmpar%morlyr%settings%nlyr
               if (ISED1 > 0) then
                  temp_pointer(1:(IVAL_MSEDN - IVAL_MSED1 + 1) * ntot * nlyrs) => valobs(:, IPNT_MSED1:IPNT_MSED1 - 1 + (IVAL_MSEDN - IVAL_MSED1 + 1) * (nlyrs))
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_MSED), temp_pointer)

                  temp_pointer(1:(IVAL_LYRFRACN - IVAL_LYRFRAC1 + 1) * ntot * nlyrs) => valobs(:, IPNT_LYRFRAC1:IPNT_LYRFRAC1 - 1 + (IVAL_LYRFRACN - IVAL_LYRFRAC1 + 1) * (nlyrs))
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LYRFRAC), temp_pointer)
               end if
               temp_pointer(1:ntot * nlyrs) => valobs(:, IPNT_THLYR:IPNT_THLYR + (nlyrs - 1))
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_THLYR), temp_pointer)

               if (stmpar%morlyr%settings%iporosity > 0) then
                  temp_pointer(1:ntot * nlyrs) => valobs(:, IPNT_POROS:IPNT_POROS + (nlyrs - 1))
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_POROS), temp_pointer)
               end if
            end select
            if (ISED1 > 0) then
               if (stmpar%morpar%moroutput%frac) then
                  temp_pointer(1:ntot * (IPNT_FRACN - IPNT_FRAC1 + 1)) => valobs(:, IPNT_FRAC1:IPNT_FRACN)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_FRAC), temp_pointer)
               end if
               if (stmpar%morpar%moroutput%fixfac) then
                  temp_pointer(1:ntot * (IVAL_FIXFACN - IVAL_FIXFAC1 + 1)) => valobs(:, IVAL_FIXFAC1:IVAL_FIXFACN)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_FIXFRAC), temp_pointer)
               end if
               if (stmpar%morpar%moroutput%hidexp) then
                  temp_pointer(1:ntot * (IVAL_HIDEXPN - IVAL_HIDEXP1 + 1)) => valobs(:, IVAL_HIDEXP1:IVAL_HIDEXPN)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_HIDEXP), temp_pointer)
               end if
               if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
                  temp_pointer(1:ntot * (IVAL_MFLUFFN - IVAL_MFLUFF1 + 1)) => valobs(:, IVAL_MFLUFF1:IVAL_MFLUFFN)
                  call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_MFLUFF), temp_pointer)
               end if
            end if
            if (stmpar%morpar%moroutput%mudfrac) then
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_MUDFRAC), valobs(:, IPNT_MUDFRAC))
            end if
            if (stmpar%morpar%moroutput%sandfrac) then
               call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_SANDFRAC), valobs(:, IPNT_SANDFRAC))
            end if
         end if
         ! Water quality variables
         if (jawaqproc > 0 .and. num_wq_user_outputs > 0) then
            call add_station_water_quality_configs(config_set_his, idx_his_hwq)

            num_layers = max(1, kmx)
            do variable_index = 1, num_wq_user_outputs
               start_index = IPNT_HWQ1 + (variable_index - 1) * num_layers
               temp_pointer(1:num_layers * ntot) => valobs(:, start_index:start_index + num_layers - 1)
               call add_stat_output_items(output_set, output_config_set%configs(idx_his_hwq(variable_index)), temp_pointer)
            end do
         end if

         ! Water quality bottom substances
         if (numwqbots > 0) then
            call add_station_wqbot_configs(output_config_set, idx_wqbot_stations)
            call add_station_wqbot_output_items(output_set, output_config_set, idx_wqbot_stations)
            if (model_is_3D()) then
               call add_station_wqbot3D_configs(output_config_set, idx_wqbot3D_stations)
               call add_station_wqbot3D_output_items(output_set, output_config_set, idx_wqbot3D_stations)
            end if
         end if

         ! Transported constituents
         if (model_has_tracers()) then
            call add_station_tracer_configs(output_config_set, idx_tracers_stations)
            call add_station_tracer_output_items(output_set, output_config_set, idx_tracers_stations)
         end if !testcase: UNST-7713
      end if
      !
      ! Variables on observation cross sections
      !
      if (ncrs > 0) then
         !
         ! Prepare data array
         ! Add configuration items for constituents and sediment output (During reading of MDU file this data was not available)
         !
         call init_obscrs_data_and_config(num_const_items, output_config_set, idx_constituents_crs)

         !
         ! Basic flow quantities
         !
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_OBSCRS_DISCHARGE), obscrs_data(:, 1), aggregate_obscrs_data)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_OBSCRS_DISCHARGE_CUMUL), obscrs_data(:, 2))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_OBSCRS_AREA), obscrs_data(:, 3))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_OBSCRS_VELOCITY), obscrs_data(:, 4))

         !
         ! Transported constituents
         !
         do i = 1, num_const_items
            if (idx_constituents_crs(i) > 0) then
               call add_stat_output_items(output_set, output_config_set%configs(idx_constituents_crs(i)), obscrs_data(:, 5 + i))
            end if
         end do
      end if
      !
      ! Variables on lateral discharges
      !
      if (jahislateral > 0 .and. numlatsg > 0) then
         allocate (qplat_data(size(qplat, dim=2)))
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS), qplat_data, transform_qplat)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE), qplatAve)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS), qLatReal)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE), qLatRealAve)
      end if
      if (dad_included) then ! Output for dredging and dumping
         temp_pointer(1:size(stmpar%sedpar%rhosol, 1) * dadpar%nalink) => dadpar%link_sum
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DRED_LINK_DISCHARGE), temp_pointer)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DRED_DISCHARGE), dadpar%totvoldred)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DUMP_DISCHARGE), dadpar%totvoldump)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_DRED_TIME_FRAC), null(), calculate_dredge_time_fraction)
         call add_stat_output_items(output_set, output_config_set%configs(IDX_HIS_PLOUGH_TIME_FRAC), time_ploughed)
      end if

      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_S0                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_S1                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_POTEVAP                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_ACTEVAP                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_PRESCREVAP                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_VOL1                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WATERDEPTH                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_HU                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NEGDPT                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NEGDPT_CUM                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NOITER                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NOITER_CUM                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_LIMTSTEP                                                  )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_LIMTSTEP_CUM                                              )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_COURANT                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_AU                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_U1                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_U0                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCXQ_EULERIAN                                             )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCYQ_EULERIAN                                             )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCXQ                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCYQ                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCMAG                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCMAG_EULER                                               )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCMAGA_GLM                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UCMAGA                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WW1                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_RHO                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_VIU                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_DIU                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_Q1                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_Q1_MAIN                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_FIXED_WEIR_ENERGY_LOSS                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SPIRCRV                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SPIRINT                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NUMLIMDT                                                  )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TAUSX                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TAUSY                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TAUS                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TAUSMAX                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_Z0UCUR                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_Z0UROU                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SA1                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CZS                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CZU                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CFU                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CFUTYP                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TEM1                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SED                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CONST                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TURKIN1                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_VICWWU                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TUREPS1                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TUREPS1_3                                                 )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TUREPS1_4                                                 )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CFRT_0                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CFRT_1                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CFRT_2                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CFRT                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CFCL                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_RAINFALL_RATE                                             )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_INTERCEPTION_WATERDEPTH                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_PATM                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDX                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDY                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDXU                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDYU                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDX_SFERIC                                              )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDY_SFERIC                                              )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDXU_SFERIC                                             )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDYU_SFERIC                                             )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDSTRESSX                                               )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDSTRESSY                                               )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDSTRESSX_SFERIC                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WINDSTRESSY_SFERIC                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TAIR                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_RHUM                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CLOU                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QSUN                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QEVA                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QCON                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QLONG                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QFREVA                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QFRCON                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QTOT                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TIDALPOTENTIAL                                            )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SALPOTENTIAL                                              )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_INTERNAL_TIDES_DISSIPATION                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TNUDGE                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NUDGE_TEM                                                 )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NUDGE_SAL                                                 )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NUDGE_DTEM                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NUDGE_DSAL                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_HWAV                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_HWAV_SIG                                                  )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TP                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_DIR                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SXWAV                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SYWAV                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SXBWAV                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SYBWAV                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_MX                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_MY                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_DISSURF                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_DISWCAP                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UORB                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_E                                                         )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_R                                                         )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_DR                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_D                                                         )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_DF                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SXX                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SYY                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SXY                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CWAV                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CGWAV                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SIGMWAV                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_KWAV                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_NWAV                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CTHETA                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_L1                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SWE                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_SWT                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_UST_CC                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_VST_CC                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_USTOKES                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_VSTOKES                                                   )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_THETAMEAN                                                 )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TWAV                                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_FX                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_FY                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WAVFU                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WAVFV                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_DTCELL                                                    )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_TIME_WATER_ON_GROUND                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_FREEBOARD                                                 )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WATERDEPTH_ON_GROUND                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_VOLUME_ON_GROUND                                          )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_1D2D                             )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_1D2D                          )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CURRENT_TOTAL_NET_INFLOW_LATERAL                          )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_LATERAL                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_WATER_LEVEL_GRADIENT                                      )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_MAP_QIN                                                       )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_CLS_S1                                                        )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_CLS_WATERDEPTH                                                )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_CLS_UCMAG                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_CLS_UCMAG_EULER                                               )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_CLS_UCDIR                                                     )
      !call add_stat_output_items(output_set, output_config_set%configs(IDX_CLS_UCDIR_EULER                                               )
      !
      call process_output_quantity_configs(output_set)
      call realloc(output_set, .true.) ! set size to count
      call initialize_statistical_output(output_set%statout)
      apply_statistics_on_output = any(station_statistics_requested(output_set%statout))

   end subroutine flow_init_statistical_output_his

   !> Update output quantity configs with information about the simulation model (e.g., do not write layers when simulation is 2D)
   subroutine process_output_quantity_configs(output_set)
      type(t_output_variable_set), intent(inout) :: output_set
      integer :: i

      do i = 1, output_set%count
         associate (config => output_set%statout(i)%output_config)
            if (allocated(config%nc_dim_ids)) then
               call process_nc_dim_ids(config%nc_dim_ids)
            end if
         end associate
      end do

   end subroutine process_output_quantity_configs

   !> return whether the output variable item is a station where the requested operation type is a statistical one
   elemental function station_statistics_requested(item) result(res)
      use m_statistical_output_types, only: SO_CURRENT, SO_NONE, SO_UNKNOWN

      type(t_output_variable_item), intent(in) :: item !< output variable item to check
      logical :: res !< Return value

      if (allocated(item%output_config)) then
         res = (.not. any(item%operation_type == [SO_CURRENT, SO_NONE, SO_UNKNOWN])) .and. item%output_config%location_specifier == UNC_LOC_STATION
      else
         res = .false.
      end if

   end function station_statistics_requested

   !> Deactivate invalid dimension IDs depending on model parameters
   subroutine process_nc_dim_ids(nc_dim_ids)
      use m_sediment, only: stm_included

      type(t_station_nc_dimensions), intent(inout) :: nc_dim_ids !< The NetCDF dimension IDs for a possible output variable config

      if (.not. model_is_3D()) then ! Turn off layer dimensions in 2D
         nc_dim_ids%laydim = .false.
         nc_dim_ids%laydim_interface_center = .false.
         nc_dim_ids%laydim_interface_edge = .false.
      end if
      if (.not. stm_included) then ! No sediment dimensions if stm not included
         nc_dim_ids%sedsusdim = .false.
         nc_dim_ids%sedtotdim = .false.
      end if
   end subroutine process_nc_dim_ids

   !> Check if model is 3D.
   pure function model_is_3D() result(res)
      use m_flow, only: kmx
      logical :: res !< Return value

      res = (kmx > 0)
   end function model_is_3D

   !> Check if model has fixed observation stations
   pure function model_has_fixed_obs_stations() result(res)
      use m_observations_data, only: numobs
      logical :: res !< Return value

      res = (numobs > 0)
   end function model_has_fixed_obs_stations

   !> Check if model has moving observation stations
   pure function model_has_moving_obs_stations() result(res)
      use m_observations_data, only: nummovobs
      logical :: res !< Return value

      res = (nummovobs > 0)
   end function model_has_moving_obs_stations

   !> Check if model has any observation stations, fixed or moving
   pure function model_has_obs_stations() result(res)
      use m_observations_data, only: numobs, nummovobs
      logical :: res !< Return value

      res = (numobs + nummovobs > 0)
   end function model_has_obs_stations

   !> Check if model contains tracers.
   pure function model_has_tracers() result(res)
      use m_transport, only: ITRA1
      logical :: res !< Return value

      res = (ITRA1 > 0)
   end function model_has_tracers

end module fm_statistical_output
