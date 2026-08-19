module test_init_spatial_fields_integration
   use assertions_gtest
   use iso_c_utils, only: cstr
   use fm_external_forcings, only: init_spatial_fields
   use m_meteo, only: initialize_ec_module, jarain
   use m_wind, only: air_pressure, rain
   use m_cell_geometry, only: xz, yz, ndx
   use m_flowgeom, only: kcs, ndxi
   use m_file_helpers, only: create_file
   use precision_basics, only: dp
   use unstruc_messages, only: threshold_abort
   use messagehandling, only: LEVEL_FATAL
   use tree_data_types, only: tree_data
   use tree_structures, only: tree_create, tree_destroy
   use properties, only: prop_file
   use m_alloc, only: realloc

   implicit none(type, external)

   character(len=*), parameter :: EXT_FILENAME = "test_spatial.ext"
   character(len=*), parameter :: BC_FILENAME = "test_rain.bc"
   character(len=*), parameter :: BASE_DIR = "."
   integer, parameter :: NUM_SCALAR_METEO_CASES = 15
   character(len=32), dimension(NUM_SCALAR_METEO_CASES), parameter :: SCALAR_METEO_QUANTITIES = [character(len=32) :: &
      'airdensity', 'airpressure', 'airtemperature', 'cloudiness', 'dewpoint', 'humidity', &
      'latentheatflux', 'longwaveradiation', 'netsolarradiation', 'solarradiation', &
      'sensibleheatflux', 'stressx', 'stressy', 'windx', 'windy']
   character(len=48), dimension(NUM_SCALAR_METEO_CASES), parameter :: SCALAR_METEO_VARIABLES = [character(len=48) :: &
      'p140209', 'msl', 't2m', 'tcc', 'd2m', 'rhum', 'slhf', 'strd', 'ssr', 'ssrd', &
      'sshf', 'tauu', 'tauv', 'u10', 'v10']
   character(len=64), dimension(NUM_SCALAR_METEO_CASES), parameter :: SCALAR_METEO_STANDARD_NAMES = [character(len=64) :: &
      'air_density', 'air_pressure', 'air_temperature', 'cloud_area_fraction', 'dew_point_temperature', 'relative_humidity', &
      'surface_upward_latent_heat_flux', 'surface_net_downward_longwave_flux', 'surface_net_downward_shortwave_flux', &
      'surface_downwelling_shortwave_flux_in_air', 'surface_upward_sensible_heat_flux', &
      'surface_downward_eastward_stress', 'surface_downward_northward_stress', 'eastward_wind', 'northward_wind']
   real(dp), dimension(NUM_SCALAR_METEO_CASES), parameter :: SCALAR_METEO_VALUES = [ &
      1.2_dp, 101325.0_dp, 20.0_dp, 0.4_dp, 10.0_dp, 60.0_dp, 120.0_dp, 80.0_dp, 200.0_dp, 250.0_dp, &
      50.0_dp, 0.1_dp, 0.2_dp, 3.0_dp, 4.0_dp]

contains

   !> Set up a minimal 1-cell s-point grid so that get_location_target_properties
   !! and construct_mask do not dereference unallocated arrays.
   subroutine setup_minimal_grid()
      ndx = 1
      ndxi = 1
      if (.not. allocated(xz)) allocate (xz(ndx))
      if (.not. allocated(yz)) allocate (yz(ndx))
      if (.not. allocated(kcs)) allocate (kcs(ndx))
      xz = [0.0_dp]
      yz = [0.0_dp]
      kcs = [1]
   end subroutine setup_minimal_grid

   subroutine teardown_minimal_grid()
      ndx = 0
      if (allocated(xz)) deallocate (xz)
      if (allocated(yz)) deallocate (yz)
      if (allocated(kcs)) deallocate (kcs)
      if (allocated(rain)) deallocate (rain)
      if (allocated(air_pressure)) deallocate (air_pressure)
   end subroutine teardown_minimal_grid

   !> Parse a mini ext-file containing a single [Spatial] block and return
   !! a pointer to that block's tree node. The caller must call tree_destroy(bnd_ptr).
   subroutine parse_spatial_block(file_name, bnd_ptr, block_ptr)
      character(len=*), intent(in) :: file_name
      type(tree_data), pointer, intent(out) :: bnd_ptr
      type(tree_data), pointer, intent(out) :: block_ptr
      integer :: istat

      call tree_create(file_name, bnd_ptr)
      call prop_file('ini', file_name, bnd_ptr, istat)
      block_ptr => bnd_ptr%child_nodes(1)%node_ptr
   end subroutine parse_spatial_block

   subroutine setup_minimal_grid_with_points(npoints)
      integer, intent(in) :: npoints

      ndx = npoints
      ndxi = npoints
      if (allocated(xz)) deallocate (xz)
      if (allocated(yz)) deallocate (yz)
      if (allocated(kcs)) deallocate (kcs)
      allocate (xz(npoints), yz(npoints), kcs(npoints))
      xz = 0.0_dp
      yz = 0.0_dp
      kcs = 1
   end subroutine setup_minimal_grid_with_points

   subroutine create_scalar_netcdf(file_name)
      use netcdf

      character(len=*), intent(in) :: file_name
      integer :: ncid, time_dimid, time_varid, ssrd_varid

      call check_netcdf(nf90_create(file_name, NF90_CLOBBER, ncid))
      call check_netcdf(nf90_def_dim(ncid, 'time', 2, time_dimid))
      call check_netcdf(nf90_def_var(ncid, 'time', NF90_DOUBLE, [time_dimid], time_varid))
      call check_netcdf(nf90_put_att(ncid, time_varid, 'standard_name', 'time'))
      call check_netcdf(nf90_put_att(ncid, time_varid, 'units', 'seconds since 2000-01-01 00:00:00'))
      call check_netcdf(nf90_def_var(ncid, 'ssrd', NF90_DOUBLE, [time_dimid], ssrd_varid))
      call check_netcdf(nf90_put_att(ncid, ssrd_varid, 'standard_name', 'surface_downwelling_shortwave_flux_in_air'))
      call check_netcdf(nf90_put_att(ncid, ssrd_varid, 'units', 'W m-2'))
      call check_netcdf(nf90_enddef(ncid))
      call check_netcdf(nf90_put_var(ncid, time_varid, [0.0_dp, 100.0_dp]))
      call check_netcdf(nf90_put_var(ncid, ssrd_varid, [100.0_dp, 300.0_dp]))
      call check_netcdf(nf90_close(ncid))
   end subroutine create_scalar_netcdf

   subroutine create_windxy_netcdf(file_name)
      use netcdf

      character(len=*), intent(in) :: file_name
      integer :: ncid, time_dimid, time_varid, u10_varid, v10_varid

      call check_netcdf(nf90_create(file_name, NF90_CLOBBER, ncid))
      call check_netcdf(nf90_def_dim(ncid, 'time', 2, time_dimid))
      call check_netcdf(nf90_def_var(ncid, 'time', NF90_DOUBLE, [time_dimid], time_varid))
      call check_netcdf(nf90_put_att(ncid, time_varid, 'standard_name', 'time'))
      call check_netcdf(nf90_put_att(ncid, time_varid, 'units', 'seconds since 2000-01-01 00:00:00'))
      call check_netcdf(nf90_def_var(ncid, 'u10', NF90_DOUBLE, [time_dimid], u10_varid))
      call check_netcdf(nf90_put_att(ncid, u10_varid, 'standard_name', 'eastward_wind'))
      call check_netcdf(nf90_put_att(ncid, u10_varid, 'units', 'm s-1'))
      call check_netcdf(nf90_def_var(ncid, 'v10', NF90_DOUBLE, [time_dimid], v10_varid))
      call check_netcdf(nf90_put_att(ncid, v10_varid, 'standard_name', 'northward_wind'))
      call check_netcdf(nf90_put_att(ncid, v10_varid, 'units', 'm s-1'))
      call check_netcdf(nf90_enddef(ncid))
      call check_netcdf(nf90_put_var(ncid, time_varid, [0.0_dp, 100.0_dp]))
      call check_netcdf(nf90_put_var(ncid, u10_varid, [2.0_dp, 6.0_dp]))
      call check_netcdf(nf90_put_var(ncid, v10_varid, [-4.0_dp, 2.0_dp]))
      call check_netcdf(nf90_close(ncid))
   end subroutine create_windxy_netcdf

   subroutine check_netcdf(status)
      use netcdf, only: NF90_NOERR, nf90_strerror

      integer, intent(in) :: status

      if (status /= NF90_NOERR) error stop nf90_strerror(status)
   end subroutine check_netcdf

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_polygon_preserves_uncovered_values, test_polygon_preserves_uncovered_values,
   subroutine test_polygon_preserves_uncovered_values() bind(C)
      use fm_external_forcings_data, only: NTRANSFORMCOEF
      use fm_location_types, only: UNC_LOC_S
      use m_polygon, only: m_polygon_destructor
      use timespace, only: timespaceinitialfield
      use timespace_parameters, only: INSIDE_POLYGON, OPERAND_OVERRIDE

      character(len=*), parameter :: POL_FILE = "test_partial_polygon.pol"
      real(dp), dimension(NTRANSFORMCOEF) :: transformcoef
      real(dp), dimension(2) :: x, y, values
      logical :: success
      integer :: ierr

      call create_file(POL_FILE, [ &
                       "enclosing_first_point", &
                       "5  2", &
                       "-1.0  -1.0", &
                       " 1.0  -1.0", &
                       " 1.0   1.0", &
                       "-1.0   1.0", &
                       "-1.0  -1.0"])
      x = [0.0_dp, 2.0_dp]
      y = [0.0_dp, 2.0_dp]
      values = [7.0_dp, 7.0_dp]
      transformcoef = -999.0_dp
      transformcoef(1) = 4.0_dp
      ierr = m_polygon_destructor()

      success = timespaceinitialfield(x, y, values, size(values), POL_FILE, INSIDE_POLYGON, 4, &
                                      OPERAND_OVERRIDE, transformcoef, UNC_LOC_S)

      call f90_expect_true(success, "polygon initialization should succeed")
      call f90_expect_eq(values(1), 4.0_dp, "the enclosed point should receive the polygon value")
      call f90_expect_eq(values(2), 7.0_dp, "an uncovered point should retain its existing value")
      ierr = m_polygon_destructor()
   end subroutine test_polygon_preserves_uncovered_values
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_waqparameter_polygon_preserves_uncovered_values, test_waqparameter_polygon_preserves_uncovered_values,
   subroutine test_waqparameter_polygon_preserves_uncovered_values() bind(C)
      use m_flow, only: kmx, ndkx
      use m_polygon, only: m_polygon_destructor
      use processes_input, only: num_spatial_parameters, painp, paname
      use unstruc_inifields, only: register_waq_target

      character(len=*), parameter :: POL_FILE = "test_nonoverlapping_waq_parameter.pol"
      character(len=*), parameter :: EXT_FILE = "test_nonoverlapping_waq_parameter.ext"
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr

      call create_file(POL_FILE, [ &
                       "polygon_away_from_grid", &
                       "5  2", &
                       "9.0   9.0", &
                       "11.0  9.0", &
                       "11.0 11.0", &
                       "9.0  11.0", &
                       "9.0   9.0"])
      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity            = waqparameterSOD", &
                       "    forcingFile         = "//POL_FILE, &
                       "    forcingFileType     = Polygon", &
                       "    interpolationMethod = constant", &
                       "    operand              = override", &
                       "    value                = 4.0"])

      call setup_minimal_grid()
      kmx = 0
      ndkx = ndx
      num_spatial_parameters = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      allocate (paname(0))
      call register_waq_target('waqparameterSOD')
      painp(1, 1) = 2.5_dp
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      call f90_expect_true(success, "WAQ parameter polygon initialization should succeed")
      call f90_expect_eq(real(painp(1, 1), kind=dp), 2.5_dp, &
                         "an uncovered WAQ parameter cell should retain its existing value")

      num_spatial_parameters = 0
      kmx = 0
      ndkx = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      ierr = m_polygon_destructor()
      call teardown_minimal_grid()
   end subroutine test_waqparameter_polygon_preserves_uncovered_values
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_waqparameter_polygon_populates_3d_layers, test_waqparameter_polygon_populates_3d_layers,
   subroutine test_waqparameter_polygon_populates_3d_layers() bind(C)
      use m_flow, only: kmx, ndkx, kbot, ktop, kmxn, zws
      use m_polygon, only: m_polygon_destructor
      use processes_input, only: num_spatial_parameters, painp, paname
      use unstruc_inifields, only: register_waq_target

      character(len=*), parameter :: POL_FILE = "test_3d_waq_parameter.pol"
      character(len=*), parameter :: EXT_FILE = "test_3d_waq_parameter.ext"
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr

      call create_file(POL_FILE, [ &
                       "polygon_around_grid", &
                       "5  2", &
                       "-1.0  -1.0", &
                       " 1.0  -1.0", &
                       " 1.0   1.0", &
                       "-1.0   1.0", &
                       "-1.0  -1.0"])
      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity            = waqparameterSOD", &
                       "    forcingFile         = "//POL_FILE, &
                       "    forcingFileType     = Polygon", &
                       "    interpolationMethod = constant", &
                       "    operand              = override", &
                       "    value                = 4.0"])

      call setup_minimal_grid()
      kmx = 2
      ndkx = 3
      call realloc(kbot, ndx, fill=2, keepExisting=.false.)
      call realloc(ktop, ndx, fill=2, keepExisting=.false.)
      call realloc(kmxn, ndx, fill=2, keepExisting=.false.)
      call realloc(zws, ndkx, fill=0.0_dp, keepExisting=.false.)
      zws = [-2.0_dp, -1.0_dp, 0.0_dp]
      num_spatial_parameters = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      allocate (paname(0))
      call register_waq_target('waqparameterSOD')
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      call f90_expect_true(success, "WAQ parameter polygon initialization should succeed")
      call f90_expect_eq(real(painp(1, 1), kind=dp), 4.0_dp, "the 2D representative should receive the polygon value")
      call f90_expect_eq(real(painp(1, 2), kind=dp), 4.0_dp, "the bottom layer should receive the polygon value")
      call f90_expect_eq(real(painp(1, 3), kind=dp), 4.0_dp, &
                "the inactive layer above the water surface should receive the polygon value")

      num_spatial_parameters = 0
      kmx = 0
      ndkx = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      if (allocated(kbot)) deallocate (kbot)
      if (allocated(ktop)) deallocate (ktop)
      if (allocated(kmxn)) deallocate (kmxn)
      if (allocated(zws)) deallocate (zws)
      ierr = m_polygon_destructor()
      call teardown_minimal_grid()
   end subroutine test_waqparameter_polygon_populates_3d_layers
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_waqsegmentnumber_finalization, test_waqsegmentnumber_finalization,
   !> Verifies that encoded serial WAQ segment numbers are converted to the local
   !! process-space layer index and copied over each target water column.
   subroutine test_waqsegmentnumber_finalization() bind(C)
      use m_fm_wq_processes, only: kbx, reset_waq_segment_number_indices
      use m_fm_wq_processes_sub, only: finalize_waq_spatial_fields
      use m_flow, only: kbot, ktop, kmx, kmxn, ndkx
      use m_flowgeom, only: ndxi
      use m_alloc, only: realloc
      use m_partitioninfo, only: jampi
      use processes_input, only: num_spatial_parameters, painp, paname
      use unstruc_inifields, only: register_waq_target

      logical :: segment_number_registered

      ndxi = 2
      kmx = 3
      ndkx = 7
      kbx = 3
      jampi = 0
      call realloc(kbot, ndxi, keepExisting=.false.)
      call realloc(ktop, ndxi, keepExisting=.false.)
      call realloc(kmxn, ndxi, keepExisting=.false.)
      kbot = [3, 6]
      ktop = [5, 7]
      kmxn = [3, 2]

      num_spatial_parameters = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      allocate (paname(0))
      call reset_waq_segment_number_indices()
      call register_waq_target('waqsegmentnumberSegment')
      segment_number_registered = allocated(painp) .and. size(painp, 1) == 1
      call f90_expect_true(segment_number_registered, 'WAQ segment-number target should be registered')

      ! Global segment 5 is column 1, layer 3; global segment 4 is column 2, layer 2.
      painp(1, :) = [5.0_dp, 4.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 4.0_dp, 4.0_dp]
      call finalize_waq_spatial_fields()

      call f90_expect_eq(real(painp(1, 1), kind=dp), 1.0_dp, 'column 1 representative should map to process segment 1')
      call f90_expect_eq(real(painp(1, 3), kind=dp), 1.0_dp, 'column 1 bottom layer should map to process segment 1')
      call f90_expect_eq(real(painp(1, 5), kind=dp), 1.0_dp, 'column 1 top layer should map to process segment 1')
      call f90_expect_eq(real(painp(1, 2), kind=dp), 4.0_dp, 'column 2 representative should map to process segment 4')
      call f90_expect_eq(real(painp(1, 6), kind=dp), 4.0_dp, 'column 2 bottom layer should map to process segment 4')
      call f90_expect_eq(real(painp(1, 7), kind=dp), 4.0_dp, 'column 2 top layer should map to process segment 4')

      ! Layer 4 is outside the configured maximum of three layers.
      painp(1, 1) = 7.0_dp
      painp(1, 3:5) = 7.0_dp
      call finalize_waq_spatial_fields()
      call f90_expect_eq(real(painp(1, 1), kind=dp), -999.0_dp, 'an invalid encoded layer should be marked invalid')
      call f90_expect_eq(real(painp(1, 3), kind=dp), -999.0_dp, 'an invalid encoded layer should fill the complete column')

      num_spatial_parameters = 0
      ndxi = 0
      ndkx = 0
      kmx = 0
      kbx = 0
      jampi = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      if (allocated(kbot)) deallocate (kbot)
      if (allocated(ktop)) deallocate (ktop)
      if (allocated(kmxn)) deallocate (kmxn)
      call reset_waq_segment_number_indices()
   end subroutine test_waqsegmentnumber_finalization
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_averaging_params_defaults, test_averaging_params_defaults,
   !> When no averaging keywords are present, read_averaging_input must return
   !! the documented defaults: type=1 (mean), relSize=-1, numMin=1, percentile=0.
   subroutine test_averaging_params_defaults() bind(C)
      use m_spatial_field, only: t_averaging_input, read_averaging_input
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_create, tree_destroy
      use properties, only: prop_file
      use m_ec_interpolationsettings, only: RCEL_DEFAULT

      type(tree_data), pointer :: tree
      type(t_averaging_input) :: avg
      integer :: istat

      ! ARRANGE: an empty ini block with no averaging keywords.
      call tree_create('empty', tree)

      ! ACT
      call read_averaging_input(tree, avg)
      call tree_destroy(tree)

      ! ASSERT
      call f90_expect_eq(avg%averaging_type, 1, "default averaging_type should be 1 (mean)")
      call f90_expect_eq(avg%rel_size, RCEL_DEFAULT, "default rel_size should be RCEL_DEFAULT (use EC default)")
      call f90_expect_eq(avg%num_min, 1, "default num_min should be 1")
      call f90_expect_eq(avg%percentile, 0.0_dp, "default percentile should be 0")
   end subroutine test_averaging_params_defaults
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_averaging_params_to_transformcoef, test_averaging_params_to_transformcoef,
   !> averaging_params_to_transformcoef must write the four averaging values
   !! into the correct transformcoef slots (4, 5, 7, 8) without touching others.
   subroutine test_averaging_params_to_transformcoef() bind(C)
      use m_spatial_field, only: t_averaging_input, averaging_params_to_transformcoef
      use fm_external_forcings_data, only: NTRANSFORMCOEF

      type(t_averaging_input) :: avg
      real(dp), dimension(NTRANSFORMCOEF) :: tc

      avg%averaging_type = 4 ! e.g. nearestNb
      avg%rel_size = 2.5_dp
      avg%num_min = 3
      avg%percentile = 50.0_dp

      tc = -999.0_dp
      call averaging_params_to_transformcoef(avg, tc)

      call f90_expect_eq(tc(4), 4.0_dp, "transformcoef(4) should hold averagingType")
      call f90_expect_eq(tc(5), 2.5_dp, "transformcoef(5) should hold relSize")
      call f90_expect_eq(tc(7), 50.0_dp, "transformcoef(7) should hold percentile")
      call f90_expect_eq(tc(8), 3.0_dp, "transformcoef(8) should hold numMin")
      ! Slots not written by the helper must be untouched.
      call f90_expect_eq(tc(1), -999.0_dp, "transformcoef(1) should be untouched")
      call f90_expect_eq(tc(2), -999.0_dp, "transformcoef(2) should be untouched")
   end subroutine test_averaging_params_to_transformcoef
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_rainfall_bcascii_registers_ec_connection, test_rainfall_bcascii_registers_ec_connection,
   !> Verifies that a [Spatial] block with forcingFileType=bcascii sets up the
   !! EC connection via the 'global' location path and activates jarain.
   !! This exercises the bcascii branch inside init_spatial_fields, which passes
   !! 'global' (not the filename) as the location argument to ec_addtimespacerelation.
   !! That branch is never reached from integration tests because they always use
   !! NetCDF meteo files.
   subroutine test_rainfall_bcascii_registers_ec_connection() bind(C)
      use m_meteo, only: initialize_ec_module, jarain, ecInstancePtr
      use m_wind, only: rain, jaqin
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      ! ARRANGE: Create a bcascii forcing file for rainfall and an ext file that references it.
      call create_file(BC_FILENAME, [ &
                       "[General]", &
                       "    fileVersion           = 1.01", &
                       "    fileType              = boundConds", &
                       "", &
                       "[forcing]", &
                       "    name                  = global", &
                       "    function              = timeseries", &
                       "    timeInterpolation     = linear", &
                       "    quantity              = time", &
                       "    unit                  = seconds since 2000-01-01 00:00:00", &
                       "    quantity              = rainfall", &
                       "    unit                  = mm/day", &
                       "    0    1.0", &
                       "    100  2.0"])

      call create_file(EXT_FILENAME, [ &
                       "[Spatial]", &
                       "    quantity        = rainfall", &
                       "    forcingFile     = "//BC_FILENAME, &
                       "    forcingFileType = bcascii"])

      jarain = 0
      jaqin = 0
      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()
      ! ACT: Parse the block and initialize the spatial fields, which should set up the EC connection and activate jarain.
      call parse_spatial_block(EXT_FILENAME, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILENAME, 'Spatial')
      call tree_destroy(bnd_ptr)
      ! ASSERT: init_spatial_fields should succeed, jarain and jaqin should both be 1, and the EC instance should have at least one registered item.
      call f90_expect_true(success, "init_spatial_fields should succeed for a valid bcascii rainfall block")
      call f90_expect_eq(jarain, 1, "jarain should be 1 after a successful bcascii rainfall EC connection")
      call f90_expect_eq(jaqin, 1, "jaqin should be 1 after a successful bcascii rainfall EC connection")
      call f90_expect_true(ecInstancePtr%nItems > 0, "EC instance should have at least one registered item after init_spatial_fields")
      call teardown_minimal_grid()
   end subroutine test_rainfall_bcascii_registers_ec_connection
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_airpressure_bcascii_uses_generic_source_fallback, test_airpressure_bcascii_uses_generic_source_fallback,
   !> Verifies that a simple recognized scalar can use the generic source-item
   !! connection when its provider-specific source mapping does not support BC.
   subroutine test_airpressure_bcascii_uses_generic_source_fallback() bind(C)
      use m_flowtimes, only: irefdate, tzone, tunit, tstart_user
      use m_meteo, only: ec_gettimespacevalue_by_itemID, ecInstancePtr, item_atmosphericpressure

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      real(dp) :: value_at_t0, value_at_t50
      character(len=*), parameter :: AIRPRESSURE_BC = "test_airpressure.bc"
      character(len=*), parameter :: AIRPRESSURE_EXT = "test_airpressure.ext"

      call create_file(AIRPRESSURE_BC, [ &
                       "[General]", &
                       "    fileVersion           = 1.01", &
                       "    fileType              = boundConds", &
                       "", &
                       "[forcing]", &
                       "    name                  = global", &
                       "    function              = timeseries", &
                       "    timeInterpolation     = linear", &
                       "    quantity              = time", &
                       "    unit                  = seconds since 2000-01-01 00:00:00", &
                       "    quantity              = airpressure", &
                       "    unit                  = Pa", &
                       "    0    101325.0", &
                       "    100  101300.0"])

      call create_file(AIRPRESSURE_EXT, [ &
                       "[Spatial]", &
                       "    quantity        = airpressure", &
                       "    forcingFile     = "//AIRPRESSURE_BC, &
                       "    forcingFileType = bcascii"])

      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()

      call parse_spatial_block(AIRPRESSURE_EXT, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, AIRPRESSURE_EXT, 'Spatial')
      call tree_destroy(bnd_ptr)

      call f90_expect_true(success, "init_spatial_fields should use the generic fallback for bcascii airpressure")
      call f90_expect_true(item_atmosphericpressure /= -999, "airpressure should have an EC target item")

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_atmosphericpressure, irefdate, tzone, tunit, 0.0_dp)
      value_at_t0 = air_pressure(1)
      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_atmosphericpressure, irefdate, tzone, tunit, 50.0_dp)
      value_at_t50 = air_pressure(1)

      call f90_expect_near(value_at_t0, 101325.0_dp, 1.0e-6_dp, "airpressure at t=0 should be read from the BC file")
      call f90_expect_near(value_at_t50, 101312.5_dp, 1.0e-6_dp, "airpressure at t=50 should be linearly interpolated")

      call teardown_minimal_grid()
   end subroutine test_airpressure_bcascii_uses_generic_source_fallback
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_unknown_quantity_returns_error, test_unknown_quantity_returns_error,
   !> Verifies that a [Spatial] block with an unrecognized quantity causes
   !! init_spatial_fields to return .false.. The 'default' branch in
   !! init_spatial_fields emits an error and returns .false..
   subroutine test_unknown_quantity_returns_error() bind(C)
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      ! ARRANGE: Create an ext file with a spatial block that references a quantity that init_spatial_fields does not recognize.
      call create_file(EXT_FILENAME, [ &
                       "[Spatial]", &
                       "    quantity    = this_quantity_does_not_exist", &
                       "    forcingFile = dummy.nc"])

      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()

      ! ACT: parse the block and initialize the spatial fields.
      call parse_spatial_block(EXT_FILENAME, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILENAME, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT: init_spatial_fields should return .false. because the quantity is not recognized.
      call f90_expect_false(success, "init_spatial_fields should fail for an unrecognized spatial quantity")

      call teardown_minimal_grid()
   end subroutine test_unknown_quantity_returns_error
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_solarradiation_conflicts_with_netsolarradiation, test_solarradiation_conflicts_with_netsolarradiation,
   !> Verifies that enable_quantity returns .false. when netsolarradiation has
   !! already been registered. This guard in enable_quantity is only reachable
   !! after a successful EC connection, so integration tests never exercise it.
   subroutine test_solarradiation_conflicts_with_netsolarradiation() bind(C)
      use m_wind, only: net_solar_radiation_available, solar_radiation_available
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      character(len=*), parameter :: SOLAR_BC = "test_solar.bc"
      character(len=*), parameter :: SOLAR_EXT = "test_solar_conflict.ext"

      ! ARRANGE: Set up a bcascii forcing file for solar radiation and an ext file that references it.
      call create_file(SOLAR_BC, [ &
                       "[General]", &
                       "    fileVersion           = 1.01", &
                       "    fileType              = boundConds", &
                       "", &
                       "[forcing]", &
                       "    name                  = global", &
                       "    function              = timeseries", &
                       "    timeInterpolation     = linear", &
                       "    quantity              = time", &
                       "    unit                  = seconds since 2000-01-01 00:00:00", &
                       "    quantity              = sw_radiation_flux", &
                       "    unit                  = W m-2", &
                       "    0    100.0", &
                       "    100  200.0"])

      call create_file(SOLAR_EXT, [ &
                       "[Spatial]", &
                       "    quantity        = solarradiation", &
                       "    forcingFile     = "//SOLAR_BC, &
                       "    forcingFileType = bcascii"])

      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()
      net_solar_radiation_available = .true.

      ! ACT: Parse block and initialize the spatial fields
      call parse_spatial_block(SOLAR_EXT, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, SOLAR_EXT, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT: init_spatial_fields should fail because solar_radiation_available is .true. and enable_quantity should not allow both to be active.
      call f90_expect_false(success, "init_spatial_fields should fail when netsolarradiation is already registered")

      call teardown_minimal_grid()
   end subroutine test_solarradiation_conflicts_with_netsolarradiation
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_qext_static_field_populated_at_init, test_qext_static_field_populated_at_init,
   !> Verifies that a qext [Spatial] block with forcingFileType=sample populates
   !! the qext array immediately at initialisation (static_field=.true. path).
   !! This is the regression test for the unified EC path introduced in Step 2:
   !! ec_addtimespacerelation + ec_gettimespacevalue_by_itemID replaces the old
   !! init_qext_forcings/timespaceinitialfield call.
   subroutine test_qext_static_field_populated_at_init() bind(C)
      use m_wind, only: qext, jaQext
      use m_flowtimes, only: irefdate, tzone, tunit, tstart_user
      use m_polygon, only: m_polygon_destructor
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      character(len=*), parameter :: SAMPLE_FILE = "test_qext.xyz"
      character(len=*), parameter :: QEXT_EXT = "test_qext.ext"
      integer ierr

      ! ARRANGE: one sample point exactly at the single grid cell (0,0) with value 1.5.
            call create_file(SAMPLE_FILE, ["-1.0 -1.0  1.5", &
                                      " 1.0 -1.0  1.5", &
                                      " 0.0  1.0  1.5"])

      call create_file(QEXT_EXT, [ &
                       "[Spatial]", &
                       "    quantity        = qext", &
                       "    forcingFile     = "//SAMPLE_FILE, &
                       "    forcingFileType = sample", &
                       "    averagingType   = 5"]) ! 4 = nearestNb; works for a single sample point

      jaQext = 1
      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(QEXT_EXT, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, QEXT_EXT, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for a valid qext sample block")
      call f90_assert_true(allocated(qext), "qext array should be allocated after init")
      call f90_expect_near(qext(1), 1.5_dp, 1.0e-6_dp, "qext(1) should match the sample point value")

      ! CLEANUP
      jaQext = 0
      if (allocated(qext)) deallocate (qext)
      call teardown_minimal_grid()
   end subroutine test_qext_static_field_populated_at_init
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_qext_bcascii_registers_ec_connection, test_qext_bcascii_registers_ec_connection,
   !> Verifies that qext with forcingFileType=bcascii sets up a time-varying EC relation
   !! via the tgt_data1 bypass path (quantity not registered in fm_ext_force_name_to_ec_item).
   !! This proves that user freedom is preserved: qext is not locked to sample files.
   subroutine test_qext_bcascii_registers_ec_connection() bind(C)
      use m_wind, only: qext, jaQext
      use m_flowtimes, only: irefdate, tzone, tunit, tstart_user
      use m_meteo, only: ecInstancePtr, ec_gettimespacevalue_by_itemID
      use m_ec_typedefs, only: tEcItemPtr

      type(tEcItemPtr), dimension(:), pointer :: ecItemsPtr => null()
      integer :: ec_item
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      real(dp) :: value_at_t0, value_at_t50
      character(len=*), parameter :: QEXT_BC = "test_qext_tv.bc"
      character(len=*), parameter :: QEXT_EXT = "test_qext_tv.ext"

      call create_file(QEXT_BC, [ &
                       "[General]", &
                       "    fileVersion = 1.01", &
                       "    fileType    = boundConds", &
                       "", &
                       "[forcing]", &
                       "    name              = global", &
                       "    function          = timeseries", &
                       "    timeInterpolation = linear", &
                       "    quantity          = time", &
                       "    unit              = seconds since 2000-01-01", &
                       "    quantity          = qext", &
                       "    unit              = m3/s", &
                       "    0    1.0", &
                       "    100  3.0"])

      call create_file(QEXT_EXT, [ &
                       "[Spatial]", &
                       "    quantity        = qext", &
                       "    forcingFile     = "//QEXT_BC, &
                       "    forcingFileType = bcascii"])

      jaQext = 1
      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()

      ! ACT
      call parse_spatial_block(QEXT_EXT, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, QEXT_EXT, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT: EC relation established
      call f90_expect_true(success, "init_spatial_fields should succeed for qext bcascii block")
      call f90_assert_true(allocated(qext), "qext should be allocated")

      ! Get the qext EC item ID directly from the instance after init
      ec_item = ecInstancePtr%ecItemsPtr(ecInstancePtr%nItems)%ptr%id
      ! ASSERT: values update correctly over time (proves EC relation is live, not one-shot)
      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, ec_item, &
                                               irefdate, tzone, tunit, 0.0_dp)
      value_at_t0 = qext(1)
      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, ec_item, &
                                               irefdate, tzone, tunit, 50.0_dp)
      value_at_t50 = qext(1)

      call f90_expect_near(value_at_t0, 1.0_dp, 1.0e-6_dp, "qext at t=0 should be 1.0")
      call f90_expect_near(value_at_t50, 2.0_dp, 1.0e-6_dp, "qext at t=50 should be 2.0 (linearly interpolated)")

      jaQext = 0
      if (allocated(qext)) deallocate (qext)
      call teardown_minimal_grid()
   end subroutine test_qext_bcascii_registers_ec_connection
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_solarradiation_scalar_netcdf_broadcast, test_solarradiation_scalar_netcdf_broadcast,
   subroutine test_solarradiation_scalar_netcdf_broadcast() bind(C)
      use m_meteo, only: ecInstancePtr, ec_gettimespacevalue_by_itemID, initialize_ec_module, item_solar_radiation
      use m_sferic, only: jsferic
      use m_wind, only: solar_radiation, solar_radiation_available
      use m_flowtimes, only: irefdate, tunit, tzone, tstart_user

      character(len=*), parameter :: NC_FILE = 'test_solarradiation_uniform.nc'
      character(len=*), parameter :: EXT_FILE = 'test_solarradiation_uniform.ext'
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success

      call create_scalar_netcdf(NC_FILE)
      call create_file(EXT_FILE, [ &
                       '[Spatial]', &
                       '    quantity        = solarradiation', &
                       '    forcingFile     = '//NC_FILE, &
                       '    forcingFileType = netcdf', &
                       '    operand         = override'])

      call setup_minimal_grid_with_points(2)
      solar_radiation_available = .false.
      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      jsferic = 0
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()

      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      call f90_expect_true(success, 'scalar NetCDF initialization should succeed')
      call f90_expect_true(item_solar_radiation > 0, 'solar radiation target item should be registered')

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_solar_radiation, &
                                               irefdate, tzone, tunit, 0.0_dp)
      call f90_expect_true(success, 'scalar NetCDF update at t=0 should succeed')
      call f90_expect_near(solar_radiation(1), 100.0_dp, 1.0e-6_dp, 'first target should receive t=0 value')
      call f90_expect_near(solar_radiation(2), 100.0_dp, 1.0e-6_dp, 'second target should receive t=0 value')

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_solar_radiation, &
                                               irefdate, tzone, tunit, 50.0_dp)
      call f90_expect_true(success, 'scalar NetCDF update at t=50 should succeed')
      call f90_expect_near(solar_radiation(1), 200.0_dp, 1.0e-6_dp, 'first target should receive interpolated value')
      call f90_expect_near(solar_radiation(2), 200.0_dp, 1.0e-6_dp, 'second target should receive interpolated value')

      solar_radiation_available = .false.
      if (allocated(solar_radiation)) deallocate (solar_radiation)
      call teardown_minimal_grid()
   end subroutine test_solarradiation_scalar_netcdf_broadcast
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_windxy_scalar_netcdf_override_and_multiply, test_windxy_scalar_netcdf_override_and_multiply,
   subroutine test_windxy_scalar_netcdf_override_and_multiply() bind(C)
      use m_meteo, only: ecInstancePtr, ec_gettimespacevalue_by_itemID, initialize_ec_module, item_windxy_x
      use m_sferic, only: jsferic
      use m_wind, only: jawind, wx, wy
      use m_flow, only: wdsu, wdsu_x, wdsu_y
      use m_flowgeom, only: lnx, xu, yu
      use m_flowtimes, only: irefdate, tunit, tzone, tstart_user

      character(len=*), parameter :: NC_FILE = 'test_windxy_uniform.nc'
      character(len=*), parameter :: EXT_FILE = 'test_windxy_uniform.ext'
      character(len=*), parameter :: FACTOR_EXT_FILE = 'test_windxy_factor.ext'
      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success

      call create_windxy_netcdf(NC_FILE)
      call create_file(EXT_FILE, [ &
                       '[Spatial]', &
                       '    quantity        = windxy', &
                       '    forcingFile     = '//NC_FILE, &
                       '    forcingFileType = netcdf', &
                       '    operand         = override'])
      call create_file(FACTOR_EXT_FILE, [ &
                       '[Spatial]', &
                       '    quantity        = windxy', &
                       '    forcingFileType = datavalue', &
                       '    dataValue       = 0.5', &
                       '    operand         = multiply'])

      if (allocated(wx)) deallocate (wx)
      if (allocated(wy)) deallocate (wy)
      if (allocated(wdsu)) deallocate (wdsu)
      if (allocated(wdsu_x)) deallocate (wdsu_x)
      if (allocated(wdsu_y)) deallocate (wdsu_y)
      call setup_minimal_grid_with_points(2)
      lnx = 2
      if (allocated(xu)) deallocate (xu)
      if (allocated(yu)) deallocate (yu)
      allocate (xu(lnx), yu(lnx))
      xu = [0.0_dp, 10.0_dp]
      yu = 0.0_dp
      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      jsferic = 0
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()

      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)
      call f90_expect_true(success, 'scalar NetCDF windxy initialization should succeed')

      call parse_spatial_block(FACTOR_EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, FACTOR_EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)
      call f90_expect_true(success, 'windxy dataValue multiply initialization should succeed')

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_windxy_x, &
                                               irefdate, tzone, tunit, 0.0_dp)
      call f90_expect_true(success, 'windxy update at t=0 should succeed')
      call f90_expect_near(wx(1), 1.0_dp, 1.0e-6_dp, 'first x target should be overridden and multiplied at t=0')
      call f90_expect_near(wx(2), 1.0_dp, 1.0e-6_dp, 'second x target should be overridden and multiplied at t=0')
      call f90_expect_near(wy(1), -2.0_dp, 1.0e-6_dp, 'first y target should be overridden and multiplied at t=0')
      call f90_expect_near(wy(2), -2.0_dp, 1.0e-6_dp, 'second y target should be overridden and multiplied at t=0')

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_windxy_x, &
                                               irefdate, tzone, tunit, 50.0_dp)
      call f90_expect_true(success, 'windxy update at t=50 should succeed')
      call f90_expect_near(wx(1), 2.0_dp, 1.0e-6_dp, 'first x target should be interpolated and multiplied at t=50')
      call f90_expect_near(wx(2), 2.0_dp, 1.0e-6_dp, 'second x target should be interpolated and multiplied at t=50')
      call f90_expect_near(wy(1), -0.5_dp, 1.0e-6_dp, 'first y target should be interpolated and multiplied at t=50')
      call f90_expect_near(wy(2), -0.5_dp, 1.0e-6_dp, 'second y target should be interpolated and multiplied at t=50')

      jawind = 0
      lnx = 0
      if (allocated(xu)) deallocate (xu)
      if (allocated(yu)) deallocate (yu)
      if (allocated(wx)) deallocate (wx)
      if (allocated(wy)) deallocate (wy)
      if (allocated(wdsu)) deallocate (wdsu)
      if (allocated(wdsu_x)) deallocate (wdsu_x)
      if (allocated(wdsu_y)) deallocate (wdsu_y)
      call teardown_minimal_grid()
   end subroutine test_windxy_scalar_netcdf_override_and_multiply
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_waqfunction_uses_global_ec_target, test_waqfunction_uses_global_ec_target,
   !> Verifies that a time-varying WAQ function uses a one-element global target,
   !! matching the dummy target used by the old external-forcings initialization.
   subroutine test_waqfunction_uses_global_ec_target() bind(C)
      use m_flowtimes, only: irefdate, refdate_mjd, tzone, tunit, tstart_user
      use m_meteo, only: ecInstancePtr, ec_gettimespacevalue_by_itemID, item_waqfun
      use processes_input, only: funinp, funame, num_time_functions
      use time_module, only: ymd2modified_jul
      use unstruc_inifields, only: register_waq_target

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      character(len=*), parameter :: WAQ_TIM = "test_waqfunction.tim"
      character(len=*), parameter :: WAQ_EXT = "test_waqfunction.ext"

      call create_file(WAQ_TIM, [ &
                       "0.0    10.0", &
                       "100.0  20.0"])
      call create_file(WAQ_EXT, [ &
                       "[Spatial]", &
                       "    quantity            = waqfunctionTest", &
                       "    forcingFile         = "//WAQ_TIM, &
                       "    forcingFileType     = uniform", &
                       "    interpolationMethod = linearSpaceTime", &
                       "    operand              = override"])

      irefdate = 20000101
      success = ymd2modified_jul(irefdate, refdate_mjd)
      call f90_assert_true(success, "the test reference date should convert to MJD")
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      num_time_functions = 0
      if (allocated(funame)) deallocate (funame)
      if (associated(funinp)) deallocate (funinp)
      allocate (funame(0))
      call register_waq_target('waqfunctionTest')
      call setup_minimal_grid()
      call initialize_ec_module()

      call parse_spatial_block(WAQ_EXT, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, WAQ_EXT, 'Spatial')
      call tree_destroy(bnd_ptr)

      call f90_assert_true(success, "init_spatial_fields should succeed for a WAQ function")
      call f90_assert_true(allocated(item_waqfun), "item_waqfun should be allocated")
      call f90_assert_true(item_waqfun(1) > 0, "the WAQ function EC item should be registered")

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_waqfun(1), &
                                               irefdate, tzone, tunit, 0.0_dp)
      call f90_assert_true(success, "the WAQ function should update at t=0")
      call f90_expect_near(funinp(1, 1), 10.0_dp, 1.0e-6_dp, &
                           "the WAQ function value at t=0 should be 10")

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_waqfun(1), &
                                               irefdate, tzone, tunit, 3000.0_dp)
      call f90_assert_true(success, "the WAQ function should update at t=3000 seconds")
      call f90_expect_near(funinp(1, 1), 15.0_dp, 1.0e-6_dp, &
                           "the WAQ function value at 50 minutes should be linearly interpolated")

      num_time_functions = 0
      if (allocated(funame)) deallocate (funame)
      if (associated(funinp)) deallocate (funinp)
      if (allocated(item_waqfun)) deallocate (item_waqfun)
      call teardown_minimal_grid()
   end subroutine test_waqfunction_uses_global_ec_target
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_register_waq_targets, test_register_waq_targets,
   !> Verifies that the new external-forcings pre-scan registers every WAQ input
   !! type handled by the legacy pre-scan before dependent arrays are sized.
   subroutine test_register_waq_targets() bind(C)
      use m_flow, only: ndkx
      use processes_input, only: painp, paname, num_spatial_parameters, &
                                 funinp, funame, num_time_functions, &
                                 sfuninp, sfunname, num_spatial_time_fuctions
      use unstruc_inifields, only: register_waq_target

      ndkx = 2
      num_spatial_parameters = 0
      num_time_functions = 0
      num_spatial_time_fuctions = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      if (allocated(funame)) deallocate (funame)
      if (associated(funinp)) deallocate (funinp)
      if (allocated(sfunname)) deallocate (sfunname)
      if (associated(sfuninp)) deallocate (sfuninp)
      allocate (paname(0))
      allocate (funame(0))
      allocate (sfunname(0))

      call register_waq_target('waqparameterParameter')
      call register_waq_target('waqsegmentnumberSegment')
      call register_waq_target('waqfunctionFunction')
      call register_waq_target('waqsegmentfunctionSegmentFunction')

      call f90_expect_eq(num_spatial_parameters, 2, "two WAQ spatial parameters should be registered")
      call f90_assert_streq(cstr(paname(1)), cstr("Parameter"), cstr("the WAQ parameter suffix should be retained"))
      call f90_assert_streq(cstr(paname(2)), cstr("Segment"), cstr("the WAQ segment-number suffix should be retained"))
      call f90_expect_eq(size(painp, 2), ndkx, "spatial parameter storage should cover all WAQ segments")
      call f90_expect_eq(num_time_functions, 1, "one WAQ function should be registered")
      call f90_assert_streq(cstr(funame(1)), cstr("Function"), cstr("the WAQ function suffix should be retained"))
      call f90_expect_eq(size(funinp, 2), 1, "WAQ function storage should contain one global value")
      call f90_expect_eq(num_spatial_time_fuctions, 1, "one WAQ segment function should be registered")
      call f90_assert_streq(cstr(sfunname(1)), cstr("SegmentFunction"), cstr("the WAQ segment function suffix should be retained"))
      call f90_assert_true(associated(sfuninp), "WAQ segment function target storage should be allocated")
      call f90_expect_eq(size(sfuninp, 1), 1, "target storage should contain one segment function")
      call f90_expect_eq(size(sfuninp, 2), ndkx, "target storage should cover all WAQ segments")

      num_spatial_parameters = 0
      num_time_functions = 0
      num_spatial_time_fuctions = 0
      if (allocated(paname)) deallocate (paname)
      if (allocated(painp)) deallocate (painp)
      if (allocated(funame)) deallocate (funame)
      if (associated(funinp)) deallocate (funinp)
      if (allocated(sfunname)) deallocate (sfunname)
      if (associated(sfuninp)) deallocate (sfuninp)
      ndkx = 0
   end subroutine test_register_waq_targets
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_initialwaterlevel_static_field_populated_at_init, test_initialwaterlevel_static_field_populated_at_init,
   !> Verifies that an initialwaterlevel [Spatial] block populates s1 immediately at
   !! initialisation via the new init_spatial_fields static field path.
   !! This is the regression test proving the quantity was successfully migrated from
   !! initialize_initial_fields to init_spatial_fields.
   subroutine test_initialwaterlevel_static_field_populated_at_init() bind(C)
      use m_flow, only: s1, hs
      use m_flowgeom, only: ndx2D, ndxi, bl
      use m_alloc, only: realloc
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor
      use fm_external_forcings, only: init_spatial_fields

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr
      character(len=*), parameter :: SAMPLE_FILE = "test_wl.xyz"
      character(len=*), parameter :: EXT_FILE    = "test_wl.ext"

      call create_file(SAMPLE_FILE, ["-1.0 -1.0  1.5", &
                                     " 1.0 -1.0  1.5", &
                                     " 0.0  1.0  1.5"])
      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity              = initialwaterlevel", &
                       "    forcingFile           = "//SAMPLE_FILE, &
                       "    forcingFileType       = sample", &
                       "    interpolationMethod   = triangulation"])

      ! ARRANGE

      ndx2D = 0
      call setup_minimal_grid()
      call realloc(bl, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(s1, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(hs, ndx, fill=0.0_dp, keepExisting=.false.)
      irefdate    = 20000101
      tzone       = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for initialwaterlevel sample block")
      call f90_assert_true(allocated(s1), "s1 should be allocated after init")
      call f90_expect_near(s1(1), 1.5_dp, 1.0e-6_dp, "s1(1) should match the sample value")

      ! CLEANUP
      ndxi  = 0
      ndx2D = 0
      if (allocated(bl)) deallocate(bl)
      if (allocated(s1)) deallocate(s1)
      if (allocated(hs)) deallocate(hs)
      call teardown_minimal_grid()
   end subroutine test_initialwaterlevel_static_field_populated_at_init
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_frictioncoefficient_static_field_populated_at_init, test_frictioncoefficient_static_field_populated_at_init,
   !> Verifies that a frictioncoefficient [Spatial] block populates frcu immediately at
   !! initialisation via the new init_spatial_fields static field path.
   !! Also proves UNC_LOC_U routing is correct: xu/yu are used as target coordinates.
   subroutine test_frictioncoefficient_static_field_populated_at_init() bind(C)
      use m_flow, only: frcu
      use m_flowgeom, only: ndx2D, ndxi, bl, lnx, xu, yu
      use m_alloc, only: realloc
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor
      use fm_external_forcings, only: init_spatial_fields

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr
      character(len=*), parameter :: SAMPLE_FILE = "test_fr.xyz"
      character(len=*), parameter :: EXT_FILE    = "test_fr.ext"

      call create_file(SAMPLE_FILE, ["-1.0 -1.0  0.02", &
                                     " 1.0 -1.0  0.02", &
                                     " 0.0  1.0  0.02"])
      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity              = frictioncoefficient", &
                       "    forcingFile           = "//SAMPLE_FILE, &
                       "    forcingFileType       = sample", &
                       "    interpolationMethod   = triangulation"])

      ! ARRANGE: s-point grid for kcs/xz/yz plus a single u-point at (0,0)
      call setup_minimal_grid()
      ndxi  = ndx
      ndx2D = 0
      lnx   = 1
      call realloc(bl, ndx, fill=0.0_dp, keepExisting=.false.)
      if (allocated(xu)) deallocate(xu)
      if (allocated(yu)) deallocate(yu)
      allocate(xu(lnx), yu(lnx))
      call realloc(frcu,ndx, fill=0.0_dp, keepExisting=.false.)
      xu = [0.0_dp]
      yu = [0.0_dp]
      irefdate    = 20000101
      tzone       = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for frictioncoefficient sample block")
      call f90_assert_true(allocated(frcu), "frcu should be allocated after init")
      call f90_expect_near(frcu(1), 0.02_dp, 1.0e-6_dp, "frcu(1) should match the sample value")

      ! CLEANUP
      ndxi  = 0
      ndx2D = 0
      lnx   = 0
      if (allocated(bl))   deallocate(bl)
      if (allocated(xu))   deallocate(xu)
      if (allocated(yu))   deallocate(yu)
      if (allocated(frcu)) deallocate(frcu)
      call teardown_minimal_grid()
   end subroutine test_frictioncoefficient_static_field_populated_at_init
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_initialwaterdepth_derives_s1, test_initialwaterdepth_derives_s1,
   !> Verifies that an initialwaterdepth [Spatial] block fills hs AND derives s1 = bl + hs.
   !! The s1 derivation is post-processing performed by enable_quantity, not by
   !! timespaceinitialfield itself. This proves enable_quantity fires correctly on the
   !! new init_spatial_fields path.
   subroutine test_initialwaterdepth_derives_s1() bind(C)
      use m_flow, only: s1, hs
      use m_flowgeom, only: ndx2D, ndxi, bl
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr
      character(len=*), parameter :: SAMPLE_FILE = "test_wd.xyz"
      character(len=*), parameter :: EXT_FILE    = "test_wd.ext"

      call create_file(SAMPLE_FILE, ["-1.0 -1.0  2.0", &
                                     " 1.0 -1.0  2.0", &
                                     " 0.0  1.0  2.0"])
      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity            = initialwaterdepth", &
                       "    forcingFile         = "//SAMPLE_FILE, &
                       "    forcingFileType     = sample", &
                       "    interpolationMethod = triangulation"])

      ! ARRANGE: bl=0 everywhere, so expected hs=2.0 and s1 = bl + hs = 2.0
      ndx  = 1
      ndxi = ndx
      ndx2D = 0
      call realloc(bl, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(s1, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(hs, ndx, fill=0.0_dp, keepExisting=.false.)
      irefdate    = 20000101
      tzone       = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for initialwaterdepth")
      call f90_expect_near(hs(1), 2.0_dp, 1.0e-6_dp, &
                           "hs(1) should be filled with the sample value")
      call f90_expect_near(s1(1), 2.0_dp, 1.0e-6_dp, &
                           "s1(1) must equal bl+hs=2.0 via enable_quantity post-processing")

      ndxi  = 0
      ndx2D = 0
      if (allocated(bl)) deallocate (bl)
      if (allocated(s1)) deallocate (s1)
      if (allocated(hs)) deallocate (hs)
      call teardown_minimal_grid()
   end subroutine test_initialwaterdepth_derives_s1
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_secchidepth_static_field_and_post_processing, test_secchidepth_static_field_and_post_processing,
   !> Verifies that a secchidepth [Spatial] block fills spatial_secchi_depth and sets
   !! secchi_depth_is_spatially_varying=.true. via enable_quantity post-processing.
   !! Both must fire together: a filled array with the flag still false would silently
   !! cause the model to use the uniform fallback value instead.
   subroutine test_secchidepth_static_field_and_post_processing() bind(C)
      use m_heatfluxes, only: spatial_secchi_depth, secchi_depth_is_spatially_varying
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr
      character(len=*), parameter :: SAMPLE_FILE = "test_sd.xyz"
      character(len=*), parameter :: EXT_FILE    = "test_sd.ext"

      call create_file(SAMPLE_FILE, ["-1.0 -1.0  3.5", &
                                     " 1.0 -1.0  3.5", &
                                     " 0.0  1.0  3.5"])
      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity            = secchidepth", &
                       "    forcingFile         = "//SAMPLE_FILE, &
                       "    forcingFileType     = sample", &
                       "    interpolationMethod = triangulation"])

      irefdate    = 20000101
      tzone       = 0.0_dp
      tstart_user = 0.0_dp
      secchi_depth_is_spatially_varying = .false.
      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for secchidepth")
      call f90_expect_true(secchi_depth_is_spatially_varying, &
                           "secchi_depth_is_spatially_varying must be .true. after init")
      call f90_assert_true(allocated(spatial_secchi_depth), &
                           "spatial_secchi_depth must be allocated")
      call f90_expect_near(spatial_secchi_depth(1), 3.5_dp, 1.0e-6_dp, &
                           "spatial_secchi_depth(1) should match the sample value")

      secchi_depth_is_spatially_varying = .false.
      if (allocated(spatial_secchi_depth)) deallocate (spatial_secchi_depth)
      call teardown_minimal_grid()
   end subroutine test_secchidepth_static_field_and_post_processing
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_frictioncoefficient_with_explicit_frictiontype, test_frictioncoefficient_with_explicit_frictiontype,
   !> Verifies that a frictioncoefficient [Spatial] block with an explicit frictionType=
   !! keyword causes set_friction_type_values_explicit to populate ifrcutp.
   !! This is the only quantity that triggers a third call after enable_quantity and
   !! is the regression test for set_friction_type_values_explicit being wired correctly.
   subroutine test_frictioncoefficient_with_explicit_frictiontype() bind(C)
      use m_flow, only: frcu, ifrcutp
      use m_flowgeom, only: lnx, xu, yu
      use m_physcoef, only: ifrctypuni
      use m_Roughness, only: frictionTypeStringToInteger
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor
      use m_alloc, only: aerr

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr, expected_friction_type
      character(len=*), parameter :: SAMPLE_FILE = "test_frtype.xyz"
      character(len=*), parameter :: EXT_FILE    = "test_frtype.ext"

      call create_file(SAMPLE_FILE, ["-1.0 -1.0  0.02", &
                                     " 1.0 -1.0  0.02", &
                                     " 0.0  1.0  0.02"])
      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity            = frictioncoefficient", &
                       "    forcingFile         = "//SAMPLE_FILE, &
                       "    forcingFileType     = sample", &
                       "    interpolationMethod = triangulation", &
                       "    frictionType        = Manning"])

      ! ARRANGE: get expected integer for Manning and force ifrctypuni /= it
      call frictionTypeStringToInteger('Manning', expected_friction_type)
      ifrctypuni = 0

      call setup_minimal_grid()
      lnx = 1
      call realloc(frcu, 1, fill=0.0_dp, keepExisting=.false.)
      if (allocated(xu)) deallocate (xu)
      if (allocated(yu)) deallocate (yu)
      allocate (xu(lnx), yu(lnx), stat=ierr)
      call aerr('xu/yu(lnx)', ierr, lnx)
      xu = [0.0_dp]
      yu = [0.0_dp]
      if (allocated(ifrcutp)) deallocate (ifrcutp)
      allocate (ifrcutp(lnx), stat=ierr)
      call aerr('ifrcutp(lnx)', ierr, lnx)
      ifrcutp = 0
      irefdate    = 20000101
      tzone       = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for frictioncoefficient with frictionType")
      call f90_assert_true(allocated(ifrcutp), "ifrcutp should be allocated")
      call f90_expect_eq(ifrcutp(1), expected_friction_type, &
                         "ifrcutp(1) must equal the Manning integer from frictionTypeStringToInteger")

      lnx = 0
      if (allocated(xu))     deallocate (xu)
      if (allocated(yu))     deallocate (yu)
      if (allocated(frcu))   deallocate (frcu)
      if (allocated(ifrcutp)) deallocate (ifrcutp)
      call teardown_minimal_grid()
   end subroutine test_frictioncoefficient_with_explicit_frictiontype
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_advectiontype_integer_field_populated, test_advectiontype_integer_field_populated,
   !> Verifies that an advectiontype [Spatial] block populates the iadv integer array
   !! via the resolve_integer_target + timespaceinitialfield_int path.
   !! Uses forcingFileType=insidePolygon since triangulation is not supported by
   !! timespaceinitialfield_int. The value comes from the value= keyword (transformcoef(1)),
   !! not from the polygon file itself.
   !! iadv is pointered to (not allocated) by resolve_integer_target and must therefore
   !! be pre-allocated here before init_spatial_fields is called.
   subroutine test_advectiontype_integer_field_populated() bind(C)
      use m_flowgeom, only: ndx2D, ndxi, lnx, xu, yu, iadv
      use m_alloc, only: realloc, aerr
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr
      character(len=*), parameter :: POL_FILE = "test_iadv.pol"
      character(len=*), parameter :: EXT_FILE = "test_iadv.ext"

      ! ARRANGE: a polygon that fully encloses the single cell at (0,0).
      call create_file(POL_FILE, [ &
                       "enclosing_polygon", &
                       "5  2", &
                       "-2.0  -2.0", &
                       " 2.0  -2.0", &
                       " 2.0   2.0", &
                       "-2.0   2.0", &
                       "-2.0  -2.0"])

      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity        = advectiontype", &
                       "    forcingFile     = "//POL_FILE, &
                       "    forcingFileType = Polygon", &
                       "    value           = 3"])

      call setup_minimal_grid()
      ndxi  = ndx
      ndx2D = 0
      lnx   = 1
      if (allocated(xu)) deallocate (xu)
      if (allocated(yu)) deallocate (yu)
      allocate (xu(lnx), yu(lnx), stat=ierr)
      call aerr('xu/yu(lnx)', ierr, lnx)
      xu = [0.0_dp]
      yu = [0.0_dp]
      ! iadv is only pointered to by resolve_integer_target, not allocated there.
      ! Pre-allocate here so the pointer assignment does not dereference garbage.
      call realloc(iadv, lnx, fill=0, keepExisting=.false.)
      irefdate    = 20000101
      tzone       = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for advectiontype insidePolygon block")
      call f90_assert_true(allocated(iadv), "iadv should be allocated after init")
      call f90_expect_eq(iadv(1), 3, "iadv(1) should be 3 (value= keyword via transformcoef(1))")

      ! CLEANUP
      lnx   = 0
      ndxi  = 0
      ndx2D = 0
      if (allocated(xu))   deallocate (xu)
      if (allocated(yu))   deallocate (yu)
      if (allocated(iadv)) deallocate (iadv)
      call teardown_minimal_grid()
   end subroutine test_advectiontype_integer_field_populated
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_waqbot_vertical_layer_selection, test_waqbot_vertical_layer_selection,
   subroutine test_waqbot_vertical_layer_selection() bind(C)
      use m_flow, only: kmx, kbot, ktop, kmxn
      use timespace_parameters, only: OPERAND_OVERRIDE
      use unstruc_inifields, only: apply_waqbot_target_layer

      real(dp), dimension(1) :: input_2d
      real(dp), dimension(9) :: output_3d
      logical :: success

      kmx = 8
      call realloc(kbot, 1, fill=2, keepExisting=.false.)
      call realloc(ktop, 1, fill=7, keepExisting=.false.)
      call realloc(kmxn, 1, fill=8, keepExisting=.false.)
      input_2d = 1.0_dp

      output_3d = 0.0_dp
      success = apply_waqbot_target_layer(input_2d, output_3d, 'bottom', 'initialwaqbottestbot', OPERAND_OVERRIDE)
      call f90_expect_true(success, "targetLayer should be accepted")
      call f90_expect_eq(output_3d(2), 1.0_dp, "targetLayer should select the active bottom layer")
      call f90_expect_eq(sum(output_3d), 1.0_dp, "targetLayer should update one layer")

      output_3d = 0.0_dp
      success = apply_waqbot_target_layer(input_2d, output_3d, '4', 'initialwaqbottestl4', OPERAND_OVERRIDE)
      call f90_expect_true(success, "layer 4 should be accepted")
      call f90_expect_eq(output_3d(5), 1.0_dp, "layer 4 should be counted from the deepest model plane")
      call f90_expect_eq(sum(output_3d), 1.0_dp, "a fixed layer should update one layer")

      output_3d = 0.0_dp
      success = apply_waqbot_target_layer(input_2d, output_3d, '8', 'initialwaqbottestl8', OPERAND_OVERRIDE)
      call f90_expect_true(success, "layer 8 should be accepted")
      call f90_expect_eq(output_3d(9), 1.0_dp, "an inactive maximum layer should be initialized for restart")
      call f90_expect_eq(sum(output_3d), 1.0_dp, "a maximum fixed layer should update one layer")

      kmx = 0
      if (allocated(kbot)) deallocate (kbot)
      if (allocated(ktop)) deallocate (ktop)
      if (allocated(kmxn)) deallocate (kmxn)
   end subroutine test_waqbot_vertical_layer_selection
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_waqmassbalancearea_polygon_populated, test_waqmassbalancearea_polygon_populated,
   !> Verifies that the legacy-compatible waqmassbalancearea prefix registers a
   !! named mass-balance area and assigns its integer ID to enclosed cells.
   subroutine test_waqmassbalancearea_polygon_populated() bind(C)
      use m_flow, only: ndkx, kmxn
      use m_flowgeom, only: ndx2D, ndxi
      use m_flowtimes, only: irefdate, ti_mba, tzone, tstart_user
      use m_mass_balance_area, only: read_and_initialize_mass_balance_area
      use m_mass_balance_area_data, only: mbadef, mbaname, nomba
      use m_partitioninfo, only: jampi
      use m_polygon, only: m_polygon_destructor
      use m_unstruc_model_data, only: md_mbafile

      integer :: ierr
      character(len=*), parameter :: POL_FILE = "test_mba.pol"
      character(len=*), parameter :: MBA_FILE = "test_mba.ini"

      call create_file(POL_FILE, [ &
                       "enclosing_polygon", &
                       "5  2", &
                       "-2.0  -2.0", &
                       " 2.0  -2.0", &
                       " 2.0   2.0", &
                       "-2.0   2.0", &
                       "-2.0  -2.0"])
      call create_file(MBA_FILE, [ &
                       "[General]", &
                       "fileVersion = 1.00", &
                       "fileType    = massBalanceAreas", &
                       "", &
                       "[MassBalanceArea]", &
                       "name         = test_mba", &
                       "locationFile = "//POL_FILE])

      call setup_minimal_grid()
      jampi = 0
      md_mbafile = MBA_FILE
      ndxi = ndx
      ndkx = ndx
      ndx2D = 0
      allocate (kmxn(1))
      kmxn = 1
      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      ti_mba = 60.0_dp
      threshold_abort = LEVEL_FATAL
      nomba = 0
      if (allocated(mbaname)) deallocate (mbaname)
      if (allocated(mbadef)) deallocate (mbadef)
      allocate (mbaname(0))
      allocate (mbadef(ndkx), source=-999)
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      call read_and_initialize_mass_balance_area(MBA_FILE)

      call f90_expect_eq(nomba, 1, "one mass-balance area should be registered")
      call f90_assert_streq(cstr(mbaname(1)), cstr("test_mba"), cstr("the mass-balance area suffix should be retained"))
      call f90_expect_eq(mbadef(1), 1, "the enclosed cell should belong to the registered area")

      ti_mba = 0.0_dp
      nomba = 0
      ndxi = 0
      ndkx = 0
      ndx2D = 0
      deallocate (kmxn)
      if (allocated(mbaname)) deallocate (mbaname)
      if (allocated(mbadef)) deallocate (mbadef)
      ierr = m_polygon_destructor()
      call teardown_minimal_grid()
   end subroutine test_waqmassbalancearea_polygon_populated
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_initialsalinity_3d_field_populated, test_initialsalinity_3d_field_populated,
   !> Verifies that an initialsalinity [Initial] block populates constituents(ISALT,:)
   !! via the static 3D path: timespaceinitialfield (2D interp) + initialfield2Dto3D_dbl_indx.
   !! constituents is pointered to by resolve_initial_3D_target and must be pre-allocated.
   !! kbot and ktop must also be pre-allocated for initialfield2Dto3D_dbl_indx to iterate layers.
   subroutine test_initialsalinity_3d_field_populated() bind(C)
      use m_transportdata, only: constituents, ISALT, NUMCONST
      use m_flow, only: ndkx, kmx, kbot, ktop, sa1
      use m_flowparameters, only: jasal
      use m_flowgeom, only: ndx2D, ndxi
      use m_alloc, only: realloc
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr
      character(len=*), parameter :: SAMPLE_FILE = "test_sal.xyz"
      character(len=*), parameter :: EXT_FILE    = "test_sal.ext"

      call create_file(SAMPLE_FILE, ["-1.0 -1.0  1.5", &
                                      " 1.0 -1.0  1.5", &
                                      " 0.0  1.0  1.5"])
      call create_file(EXT_FILE, [ &
                       "[Initial]", &
                       "    quantity            = initialsalinity", &
                       "    forcingFile         = "//SAMPLE_FILE, &
                       "    forcingFileType     = sample", &
                       "    interpolationMethod = triangulation"])

      ! ARRANGE
      call setup_minimal_grid()
      ndxi    = ndx
      ndx2D   = 0
      kmx     = 0
      ndkx    = ndx   ! for kmx=0: ndkx == ndx, one layer per cell
      NUMCONST = 1
      ISALT    = 1
      jasal    = 1

      constituents = 0.0_dp
      call realloc(kbot, ndx, fill=1, keepExisting=.false.)
      call realloc(ktop, ndx, fill=1, keepExisting=.false.)
      call realloc(sa1, ndx, fill=0.0_dp, keepExisting=.false.)
      irefdate    = 20000101
      tzone       = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Initial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for initialsalinity sample block")
      call f90_expect_near(sa1(1), 1.5_dp, 1.0e-6_dp, &
                           "sa1 should match the sample value after 2D interp + 3D expansion")

      ! CLEANUP
      jasal    = 0
      NUMCONST = 0
      ISALT    = 0
      ndkx     = 0
      kmx      = 0
      ndxi     = 0
      ndx2D    = 0
      if (allocated(constituents)) deallocate (constituents)
      if (allocated(kbot))         deallocate (kbot)
      if (allocated(ktop))         deallocate (ktop)
      call teardown_minimal_grid()
   end subroutine test_initialsalinity_3d_field_populated
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_initialverticalsalinityprofile, test_initialverticalsalinityprofile,
   !> Verifies that an initialverticalsalinityprofile [Spatial] block populates sa1
   !! via the UNC_LOC_3DV path in init_spatial_fields, which bypasses EC entirely and
   !! calls setinitialverticalprofile directly with the polygon profile file..
   subroutine test_initialverticalsalinityprofile() bind(C)
      use m_flow, only: sa1, kmx, kmxx, kbot, ktop, zws, layertype, ndkx
      use m_flowgeom, only: ndx2D, ndxi
      use m_flowparameters, only: jasal
      use m_alloc, only: realloc
      use m_flowtimes, only: irefdate, tzone, tstart_user
      use m_polygon, only: m_polygon_destructor

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      integer :: ierr
      real(dp), parameter :: EXPECTED_SALINITY = 35.0_dp
      character(len=*), parameter :: PROFILE_FILE = "test_ivsp.pol"
      character(len=*), parameter :: EXT_FILE = "test_ivsp.ext"

      ! ARRANGE: polygon profile file with a constant 35 PSU from bed (-10 m) to surface (0 m).
      ! reapol reads this format: name / M N / depth1 value1 / depth2 value2 ...
      ! lineinterp uses xpl=depth and ypl=salinity value.
      call create_file(PROFILE_FILE, [ &
                       "salinityprofile", &
                       "2  2", &
                       "-10.0  35.0", &
                       "  0.0  35.0"])

      call create_file(EXT_FILE, [ &
                       "[Spatial]", &
                       "    quantity        = initialverticalsalinityprofile", &
                       "    forcingFile     = "//PROFILE_FILE, &
                       "    forcingFileType = Polygon", &
                       "    value           = 35.0"])

      ! Minimal flow geometry: 1 node, 1 sigma layer.
      call setup_minimal_grid()
      ndxi = ndx        ! ndx == 1, set by setup_minimal_grid
      ndx2D = 0
      ndkx = ndx        ! for kmx=1 and 1 node: ndkx = 1

      ! Resolver guard: jasal>0 and kmx>0 are required by resolve_initial_target.
      jasal = 1
      kmx = 1
      layertype = 0      ! sigma layers, avoids the LAYTP_Z branch

      call realloc(kbot, ndxi, fill=1, keepExisting=.false.)
      call realloc(ktop, ndxi, fill=1, keepExisting=.false.)
      call realloc(sa1, ndkx, fill=0.0_dp, keepExisting=.false.)

      ! zws(0:ndkx): zws(0)=bed interface, zws(1)=surface interface.
      ! setinitialverticalprofile computes z_center(1) = 0.5*(zws(1)+zws(0)) = -5 m.
      if (allocated(zws)) deallocate (zws)
      allocate (zws(0:ndkx))
      zws(0) = -10.0_dp
      zws(1) = 0.0_dp

      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call initialize_ec_module()
      ierr = m_polygon_destructor()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Spatial')
      call tree_destroy(bnd_ptr)

      ! ASSERT
      call f90_expect_true(success, &
                           "init_spatial_fields must succeed for initialverticalsalinityprofile")
      ! lineinterp: z_center=-5 lies in the profile range [-10,0], constant value 35 PSU.
      call f90_expect_near(sa1(1), EXPECTED_SALINITY, 1.0e-10_dp, &
                           "sa1(1) must equal the profile value interpolated at z_center=-5 m")

      ! CLEANUP
      jasal = 0
      kmx = 0
      ndkx = 0
      ndxi = 0
      ndx2D = 0
      if (allocated(sa1))  deallocate (sa1)
      if (allocated(kbot)) deallocate (kbot)
      if (allocated(ktop)) deallocate (ktop)
      if (allocated(zws))  deallocate (zws)
      call teardown_minimal_grid()
   end subroutine test_initialverticalsalinityprofile
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_field1d_global_value_applied_to_frictioncoefficient, test_field1d_global_value_applied_to_frictioncoefficient,
   !> Verifies that a frictioncoefficient block with forcingFileType=1dField applies
   !! the [Global] value to all 1D links when no [Branch] blocks are present.
   !! Does not require a real 1D network: with no [Branch] blocks, spaceInit1dField
   !! is never called and the global fallback in init_field1d_block is the only code path.
   subroutine test_field1d_global_value_applied_to_frictioncoefficient() bind(C)
      use m_flow, only: frcu
      use m_flowgeom, only: lnx, lnx1d
      use unstruc_inifields, only: finalize_1dfield_global_values

      type(tree_data), pointer :: bnd_ptr, block_ptr
      logical :: success
      character(len=*), parameter :: FIELD1D_FILE = "test_fr1d.ini"
      character(len=*), parameter :: EXT_FILE     = "test_fr1d.ext"

      call create_file(FIELD1D_FILE, [ &
                       "[General]", &
                       "    fileVersion = 1.00", &
                       "    fileType    = 1dField", &
                       "", &
                       "[Global]", &
                       "    quantity = frictioncoefficient", &
                       "    unit     = -", &
                       "    value    = 0.025"])

      call create_file(EXT_FILE, [ &
                       "[Parameter]", &
                       "    quantity        = frictioncoefficient", &
                       "    forcingFile     = "//FIELD1D_FILE, &
                       "    forcingFileType = 1dField"])

      ! ARRANGE: one 1D flow link; no 1D network needed because there are no [Branch] blocks.
      lnx   = 1
      lnx1d = 1
      call realloc(frcu, lnx, fill=0.0_dp, keepExisting=.false.)
      threshold_abort = LEVEL_FATAL
      call setup_minimal_grid()
      call initialize_ec_module()

      ! ACT
      call parse_spatial_block(EXT_FILE, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, EXT_FILE, 'Parameter')
      call tree_destroy(bnd_ptr)
      call finalize_1dfield_global_values()

      ! ASSERT
      call f90_expect_true(success, "init_spatial_fields should succeed for a 1dField frictioncoefficient block")
      call f90_expect_near(frcu(1), 0.025_dp, 1.0e-6_dp, &
                           "frcu(1) should equal the global value from the [Global] block")

      ! CLEANUP
      lnx   = 0
      lnx1d = 0
      if (allocated(frcu)) deallocate (frcu)
      call teardown_minimal_grid()
   end subroutine test_field1d_global_value_applied_to_frictioncoefficient
   !$f90tw)


   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_scalar_meteo_bcascii_matrix, test_scalar_meteo_bcascii_matrix,
   subroutine test_scalar_meteo_bcascii_matrix() bind(C)
      character(len=*), parameter :: BC_FILE = 'test_scalar_meteo.bc'
      character(len=*), parameter :: EXT_FILE = 'test_scalar_meteo.ext'
      integer :: i

      do i = 1, NUM_SCALAR_METEO_CASES
         call reset_scalar_meteo_state()
         call create_scalar_meteo_bc(BC_FILE, SCALAR_METEO_QUANTITIES(i), SCALAR_METEO_VALUES(i))
         call run_scalar_meteo_case(SCALAR_METEO_QUANTITIES(i), BC_FILE, 'bcascii', EXT_FILE, SCALAR_METEO_VALUES(i))
      end do

      call cleanup_scalar_meteo_state()
   end subroutine test_scalar_meteo_bcascii_matrix
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_scalar_meteo_netcdf_matrix, test_scalar_meteo_netcdf_matrix,
   subroutine test_scalar_meteo_netcdf_matrix() bind(C)
      character(len=*), parameter :: NC_FILE = 'test_scalar_meteo.nc'
      character(len=*), parameter :: EXT_FILE = 'test_scalar_meteo_netcdf.ext'
      integer :: i

      call create_meteo_netcdf(NC_FILE, scalar_source=.true.)
      do i = 1, NUM_SCALAR_METEO_CASES
         call reset_scalar_meteo_state()
         call run_scalar_meteo_case(SCALAR_METEO_QUANTITIES(i), NC_FILE, 'netcdf', EXT_FILE, SCALAR_METEO_VALUES(i))
      end do

      call cleanup_scalar_meteo_state()
   end subroutine test_scalar_meteo_netcdf_matrix
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_fields_integration, test_gridded_meteo_netcdf_matrix, test_gridded_meteo_netcdf_matrix,
   subroutine test_gridded_meteo_netcdf_matrix() bind(C)
      character(len=*), parameter :: NC_FILE = 'test_gridded_meteo.nc'
      character(len=*), parameter :: EXT_FILE = 'test_gridded_meteo.ext'
      integer :: i

      call create_meteo_netcdf(NC_FILE, scalar_source=.false.)
      do i = 1, NUM_SCALAR_METEO_CASES
         call reset_scalar_meteo_state()
         call run_scalar_meteo_case(SCALAR_METEO_QUANTITIES(i), NC_FILE, 'netcdf', EXT_FILE, SCALAR_METEO_VALUES(i))
      end do
      call cleanup_scalar_meteo_state()
   end subroutine test_gridded_meteo_netcdf_matrix
   !$f90tw)

   subroutine run_scalar_meteo_case(quantity, forcing_file, forcing_file_type, ext_file, expected_value)
      use m_flowtimes, only: irefdate, tzone, tunit, tstart_user
      use m_meteo, only: ec_gettimespacevalue_by_itemID, ecInstancePtr, initialize_ec_module
      use m_ec_parameters, only: ec_undef_int

      character(len=*), intent(in) :: quantity
      character(len=*), intent(in) :: forcing_file
      character(len=*), intent(in) :: forcing_file_type
      character(len=*), intent(in) :: ext_file
      real(dp), intent(in) :: expected_value

      type(tree_data), pointer :: bnd_ptr, block_ptr
      real(dp), dimension(:), pointer :: target_data
      integer :: item_id
      logical :: success

      call create_file(ext_file, [ &
                       '[Spatial]', &
                       '    quantity        = '//trim(quantity), &
                       '    forcingFile     = '//trim(forcing_file), &
                       '    forcingFileType = '//trim(forcing_file_type)])

      irefdate = 20000101
      tzone = 0.0_dp
      tstart_user = 0.0_dp
      threshold_abort = LEVEL_FATAL
      call parse_spatial_block(ext_file, bnd_ptr, block_ptr)
      success = init_spatial_fields(block_ptr, BASE_DIR, ext_file, 'Spatial')
      call tree_destroy(bnd_ptr)

      call f90_expect_true(success, trim(quantity)//' should initialize from '//trim(forcing_file_type))
      if (.not. success) return

      call scalar_meteo_target(quantity, item_id, target_data)
      call f90_expect_true(item_id /= ec_undef_int, trim(quantity)//' should have a target item')
      call f90_expect_true(associated(target_data), trim(quantity)//' should have target storage')
      if (item_id == ec_undef_int .or. .not. associated(target_data)) return

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_id, irefdate, tzone, tunit, 0.0_dp, target_array=target_data)
      call f90_expect_true(success, trim(quantity)//' should update at t=0')
      if (trim(forcing_file_type) == 'netcdf') then
         call f90_expect_near(ecInstancePtr%ecFileReadersPtr(1)%ptr%items(1)%ptr%sourceT0FieldPtr%arr1dPtr(1), expected_value, &
                  1.0e-6_dp, trim(quantity)//' source value at t=0')
      end if
      call f90_expect_near(target_data(1), expected_value, 1.0e-6_dp, trim(quantity)//' value at t=0')

      success = ec_gettimespacevalue_by_itemID(ecInstancePtr, item_id, irefdate, tzone, tunit, 50.0_dp, target_array=target_data)
      call f90_expect_true(success, trim(quantity)//' should update at t=50')
      call f90_expect_near(target_data(1), expected_value + 1.0_dp, 1.0e-6_dp, trim(quantity)//' interpolated value at t=50')
   end subroutine run_scalar_meteo_case

   subroutine scalar_meteo_target(quantity, item_id, target_data)
      use m_meteo, only: item_air_density, item_atmosphericpressure, item_air_temperature, item_cloudiness, &
                         item_dew_point_temperature, item_relative_humidity, item_latent_heat_flux, item_long_wave_radiation, &
                         item_solar_radiation, item_sensible_heat_flux, item_stressx, item_stressy, item_windx, item_windy
      use m_wind, only: air_density, air_pressure, air_temperature, cloudiness, dew_point_temperature, relative_humidity, &
                        latent_heat_flux, long_wave_radiation, solar_radiation, sensible_heat_flux, wx, wy
      use m_flow, only: wdsu_x, wdsu_y
      use string_module, only: str_tolower

      character(len=*), intent(in) :: quantity
      integer, intent(out) :: item_id
      real(dp), dimension(:), pointer, intent(out) :: target_data

      item_id = -999
      target_data => null()
      select case (str_tolower(quantity))
      case ('airdensity')
         item_id = item_air_density
         target_data => air_density
      case ('airpressure')
         item_id = item_atmosphericpressure
         target_data => air_pressure
      case ('airtemperature')
         item_id = item_air_temperature
         target_data => air_temperature
      case ('cloudiness')
         item_id = item_cloudiness
         target_data => cloudiness
      case ('dewpoint')
         item_id = item_dew_point_temperature
         target_data => dew_point_temperature
      case ('humidity')
         item_id = item_relative_humidity
         target_data => relative_humidity
      case ('latentheatflux')
         item_id = item_latent_heat_flux
         target_data => latent_heat_flux
      case ('longwaveradiation')
         item_id = item_long_wave_radiation
         target_data => long_wave_radiation
      case ('netsolarradiation', 'solarradiation')
         item_id = item_solar_radiation
         target_data => solar_radiation
      case ('sensibleheatflux')
         item_id = item_sensible_heat_flux
         target_data => sensible_heat_flux
      case ('stressx')
         item_id = item_stressx
         target_data => wdsu_x
      case ('stressy')
         item_id = item_stressy
         target_data => wdsu_y
      case ('windx')
         item_id = item_windx
         target_data => wx
      case ('windy')
         item_id = item_windy
         target_data => wy
      end select
   end subroutine scalar_meteo_target

   subroutine create_scalar_meteo_bc(filename, quantity, initial_value)
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: quantity
      real(dp), intent(in) :: initial_value
      character(len=64) :: value0, value1

      write (value0, '(es24.16)') initial_value
      write (value1, '(es24.16)') initial_value + 2.0_dp
      call create_file(filename, [ &
                       '[General]', &
                       '    fileVersion           = 1.01', &
                       '    fileType              = boundConds', &
                       '', &
                       '[forcing]', &
                       '    name                  = global', &
                       '    function              = timeseries', &
                       '    timeInterpolation     = linear', &
                       '    quantity              = time', &
                       '    unit                  = seconds since 2000-01-01 00:00:00', &
                       '    quantity              = '//trim(quantity), &
                       '    unit                  = 1', &
                       '    0    '//trim(value0), &
                       '    100  '//trim(value1)])
   end subroutine create_scalar_meteo_bc

   subroutine create_meteo_netcdf(filename, scalar_source)
      use netcdf

      character(len=*), intent(in) :: filename
      logical, intent(in) :: scalar_source
      integer :: ncid, time_dimid, x_dimid, y_dimid, time_varid, x_varid, y_varid
      integer, dimension(NUM_SCALAR_METEO_CASES) :: variable_ids
      integer :: i, ierr
      real(dp), dimension(2) :: times, x_coord, y_coord
      real(dp), dimension(2, 2, 2) :: values

      x_dimid = -1
      y_dimid = -1
      x_varid = -1
      y_varid = -1

      call check_meteo_netcdf(nf90_create(filename, NF90_CLOBBER, ncid), 'create NetCDF file')
      call check_meteo_netcdf(nf90_def_dim(ncid, 'time', 2, time_dimid), 'define time dimension')
      if (.not. scalar_source) then
         call check_meteo_netcdf(nf90_def_dim(ncid, 'x', 2, x_dimid), 'define x dimension')
         call check_meteo_netcdf(nf90_def_dim(ncid, 'y', 2, y_dimid), 'define y dimension')
      end if

      call check_meteo_netcdf(nf90_def_var(ncid, 'time', NF90_DOUBLE, [time_dimid], time_varid), 'define time')
      call check_meteo_netcdf(nf90_put_att(ncid, time_varid, 'standard_name', 'time'), 'set time standard name')
      call check_meteo_netcdf(nf90_put_att(ncid, time_varid, 'units', 'seconds since 2000-01-01 00:00:00'), 'set time units')
      if (.not. scalar_source) then
         call check_meteo_netcdf(nf90_def_var(ncid, 'x', NF90_DOUBLE, [x_dimid], x_varid), 'define x')
         call check_meteo_netcdf(nf90_put_att(ncid, x_varid, 'standard_name', 'projection_x_coordinate'), 'set x standard name')
         call check_meteo_netcdf(nf90_def_var(ncid, 'y', NF90_DOUBLE, [y_dimid], y_varid), 'define y')
         call check_meteo_netcdf(nf90_put_att(ncid, y_varid, 'standard_name', 'projection_y_coordinate'), 'set y standard name')
      end if

      do i = 1, NUM_SCALAR_METEO_CASES
         if (scalar_source) then
            call check_meteo_netcdf(nf90_def_var(ncid, trim(SCALAR_METEO_VARIABLES(i)), NF90_DOUBLE, &
                                                         [time_dimid], variable_ids(i)), &
                                           'define '//trim(SCALAR_METEO_VARIABLES(i)))
         else
            call check_meteo_netcdf(nf90_def_var(ncid, trim(SCALAR_METEO_VARIABLES(i)), NF90_DOUBLE, &
                                                         [x_dimid, y_dimid, time_dimid], variable_ids(i)), &
                                           'define '//trim(SCALAR_METEO_VARIABLES(i)))
         end if
         call check_meteo_netcdf(nf90_put_att(ncid, variable_ids(i), 'standard_name', trim(SCALAR_METEO_STANDARD_NAMES(i))), &
                                        'set '//trim(SCALAR_METEO_VARIABLES(i))//' standard name')
         if (.not. scalar_source) then
            call check_meteo_netcdf(nf90_put_att(ncid, variable_ids(i), 'coordinates', 'x y'), &
                                           'set '//trim(SCALAR_METEO_VARIABLES(i))//' coordinates')
         end if
      end do

      call check_meteo_netcdf(nf90_enddef(ncid), 'finish NetCDF definition')
      times = [0.0_dp, 100.0_dp]
      x_coord = [-1.0_dp, 1.0_dp]
      y_coord = [-1.0_dp, 1.0_dp]
      call check_meteo_netcdf(nf90_put_var(ncid, time_varid, times), 'write time')
      if (.not. scalar_source) then
         call check_meteo_netcdf(nf90_put_var(ncid, x_varid, x_coord), 'write x')
         call check_meteo_netcdf(nf90_put_var(ncid, y_varid, y_coord), 'write y')
      end if
      do i = 1, NUM_SCALAR_METEO_CASES
         if (scalar_source) then
            ierr = nf90_put_var(ncid, variable_ids(i), [SCALAR_METEO_VALUES(i), SCALAR_METEO_VALUES(i) + 2.0_dp])
         else
            values(:, :, 1) = SCALAR_METEO_VALUES(i)
            values(:, :, 2) = SCALAR_METEO_VALUES(i) + 2.0_dp
            ierr = nf90_put_var(ncid, variable_ids(i), values)
         end if
         call check_meteo_netcdf(ierr, 'write '//trim(SCALAR_METEO_VARIABLES(i)))
      end do
      call check_meteo_netcdf(nf90_close(ncid), 'close NetCDF file')
   end subroutine create_meteo_netcdf

   subroutine check_meteo_netcdf(ierr, operation)
      use netcdf, only: nf90_noerr

      integer, intent(in) :: ierr
      character(len=*), intent(in) :: operation

      call f90_expect_eq(ierr, nf90_noerr, operation)
   end subroutine check_meteo_netcdf

   subroutine reset_scalar_meteo_state()
      use m_flow, only: wdsu, wdsu_x, wdsu_y
      use m_flowgeom, only: lnx, xu, yu
      use m_flowparameters, only: itempforcingtyp
      use m_meteo, only: initialize_ec_module
      use m_wind, only: wx, wy, ec_pwxwy_x, ec_pwxwy_y, ec_pwxwy_c, ec_charnock, wcharnock, &
                        air_pressure, pseudo_air_pressure, water_level_correction, rain, qext, air_temperature, &
                        dew_point_temperature, relative_humidity, cloudiness, air_density, solar_radiation, &
                        net_solar_radiation, long_wave_radiation, sensible_heat_flux, latent_heat_flux, &
                        jawind, jaspacevarcharn, jawindstressgiven, jastresstowind, ja_airdensity, jarain, jaevap, jaqin, jaQext, &
                        solar_radiation_available, net_solar_radiation_available, long_wave_radiation_available, &
                        sensible_heat_flux_available, latent_heat_flux_available, air_pressure_available, &
                        pseudo_air_pressure_available, water_level_correction_available

      call initialize_ec_module()
      call release_scalar_meteo_arrays()
      jawind = 0
      jaspacevarcharn = 0
      jawindstressgiven = 0
      jastresstowind = 0
      ja_airdensity = 0
      jarain = 0
      jaevap = 0
      jaqin = 0
      jaQext = 0
      solar_radiation_available = .false.
      net_solar_radiation_available = .false.
      long_wave_radiation_available = .false.
      sensible_heat_flux_available = .false.
      latent_heat_flux_available = .false.
      air_pressure_available = .false.
      pseudo_air_pressure_available = .false.
      water_level_correction_available = .false.
      itempforcingtyp = 0

      call setup_minimal_grid()
      lnx = 1
      allocate (xu(lnx), yu(lnx))
      xu = 0.0_dp
      yu = 0.0_dp
   end subroutine reset_scalar_meteo_state

   subroutine cleanup_scalar_meteo_state()
      use m_flowgeom, only: lnx
      use m_meteo, only: initialize_ec_module

      call initialize_ec_module()
      call release_scalar_meteo_arrays()
      lnx = 0
   end subroutine cleanup_scalar_meteo_state

   subroutine release_scalar_meteo_arrays()
      use m_flow, only: wdsu, wdsu_x, wdsu_y
      use m_flowgeom, only: lnx, xu, yu
      use m_wind, only: wx, wy, ec_pwxwy_x, ec_pwxwy_y, ec_pwxwy_c, ec_charnock, wcharnock, &
                        air_pressure, pseudo_air_pressure, water_level_correction, rain, qext, air_temperature, &
                        dew_point_temperature, relative_humidity, cloudiness, air_density, solar_radiation, &
                        net_solar_radiation, long_wave_radiation, sensible_heat_flux, latent_heat_flux

      if (allocated(wx)) deallocate (wx)
      if (allocated(wy)) deallocate (wy)
      if (allocated(wdsu)) deallocate (wdsu)
      if (allocated(wdsu_x)) deallocate (wdsu_x)
      if (allocated(wdsu_y)) deallocate (wdsu_y)
      if (allocated(ec_pwxwy_x)) deallocate (ec_pwxwy_x)
      if (allocated(ec_pwxwy_y)) deallocate (ec_pwxwy_y)
      if (allocated(ec_pwxwy_c)) deallocate (ec_pwxwy_c)
      if (allocated(ec_charnock)) deallocate (ec_charnock)
      if (allocated(wcharnock%values)) deallocate (wcharnock%values)
      if (allocated(air_pressure)) deallocate (air_pressure)
      if (allocated(pseudo_air_pressure)) deallocate (pseudo_air_pressure)
      if (allocated(water_level_correction)) deallocate (water_level_correction)
      if (allocated(rain)) deallocate (rain)
      if (allocated(qext)) deallocate (qext)
      if (allocated(air_temperature)) deallocate (air_temperature)
      if (allocated(dew_point_temperature)) deallocate (dew_point_temperature)
      if (allocated(relative_humidity)) deallocate (relative_humidity)
      if (allocated(cloudiness)) deallocate (cloudiness)
      if (allocated(air_density)) deallocate (air_density)
      if (allocated(solar_radiation)) deallocate (solar_radiation)
      if (allocated(net_solar_radiation)) deallocate (net_solar_radiation)
      if (allocated(long_wave_radiation)) deallocate (long_wave_radiation)
      if (allocated(sensible_heat_flux)) deallocate (sensible_heat_flux)
      if (allocated(latent_heat_flux)) deallocate (latent_heat_flux)
      if (allocated(xu)) deallocate (xu)
      if (allocated(yu)) deallocate (yu)
      call teardown_minimal_grid()
      lnx = 0
      ndxi = 0
   end subroutine release_scalar_meteo_arrays

end module test_init_spatial_fields_integration