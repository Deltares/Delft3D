module test_init_spatial_field
   use assertions_gtest
   use m_spatial_field, only: t_spatial_field_input, validate_spatial_field_input
   use m_wind, only: jaQext
   use unstruc_messages, only: threshold_abort
   use messagehandling, only: LEVEL_FATAL
   use m_alloc, only: realloc

   implicit none(type, external)

   character(len=*), parameter :: EXT_FILENAME = "test.ext"
   character(len=*), parameter :: GROUP_NAME = "Spatial"
   character(len=*), parameter :: BASE_DIR = "."

contains

   subroutine make_test_input(input)
      type(t_spatial_field_input), intent(out) :: input
      input%quantity = 'windx'
      input%forcing_file_type = 'netcdf'
      input%forcing_file = 'dummy.nc'
   end subroutine make_test_input

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_unrecognized_interpolation_method, test_validate_unrecognized_interpolation_method,
   !> An unrecognized interpolationMethod= string leaves method at -1 and must fail.
   !! This branch is never exercised by integration tests because they always use
   !! valid file types with known method strings.
   subroutine test_validate_unrecognized_interpolation_method() bind(C)
      type(t_spatial_field_input) :: input
      logical :: success
      call make_test_input(input)
      threshold_abort = LEVEL_FATAL
      input%interpolation_method = 'this_method_does_not_exist'
      success = validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR)
      call f90_expect_false(success, "validation should fail when interpolationMethod is unrecognized")
   end subroutine test_validate_unrecognized_interpolation_method
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_file_type_extension_mismatch, test_validate_file_type_extension_mismatch,
   subroutine test_validate_file_type_extension_mismatch() bind(C)
      type(t_spatial_field_input) :: input
      call make_test_input(input)
      input%forcing_file_type = 'bcascii' ! bcascii has no spatial default method
      input%interpolation_method = ' ' ! no explicit method either
      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "validation should fail when forcingFileType does not match input file extension")
   end subroutine test_validate_file_type_extension_mismatch
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_nonexistent_target_mask_file, test_validate_nonexistent_target_mask_file,
   !> Specifying a targetMaskFile= that does not exist on disk must fail.
   !! The inquire() branch inside validate_spatial_field_input is never reached
   !! in integration tests because they either omit the mask or supply a real file.
   subroutine test_validate_nonexistent_target_mask_file() bind(C)
      type(t_spatial_field_input) :: input
      call make_test_input(input)
      input%target_mask_file = 'this_mask_does_not_exist.pol'
      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "validation should fail when targetMaskFile does not exist on disk")
   end subroutine test_validate_nonexistent_target_mask_file
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_extrapolation_changes_method, test_validate_extrapolation_changes_method,
   !> When extrapolationAllowed=true, update_method_in_case_extrapolation must
   !! mutate the derived method value. Verifies that the call is actually made
   !! and has an observable effect, which integration tests do not check directly.
   subroutine test_validate_extrapolation_changes_method() bind(C)
      type(t_spatial_field_input) :: input_without_extrap
      type(t_spatial_field_input) :: input_with_extrap
      logical :: success_without, success_with

      call make_test_input(input_without_extrap)
      input_without_extrap%is_extrapolation_allowed = .false.
      success_without = validate_spatial_field_input(input_without_extrap, EXT_FILENAME, GROUP_NAME, BASE_DIR)
      call f90_assert_true(success_without, "baseline validation without extrapolation should succeed")

      call make_test_input(input_with_extrap)
      input_with_extrap%is_extrapolation_allowed = .true.
      success_with = validate_spatial_field_input(input_with_extrap, EXT_FILENAME, GROUP_NAME, BASE_DIR)
      call f90_assert_true(success_with, "validation with extrapolation should succeed")

      call f90_expect_true(input_with_extrap%method /= input_without_extrap%method, &
                           "enabling extrapolation should produce a different method value")
   end subroutine test_validate_extrapolation_changes_method
   !$f90tw)

end module test_init_spatial_field

module test_init_spatial_fields_integration
   use assertions_gtest
   use fm_external_forcings, only: init_spatial_fields
   use m_meteo, only: initialize_ec_module, jarain
   use m_wind, only: rain
   use m_cell_geometry, only: xz, yz, ndx
   use m_flowgeom, only: kcs
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

contains

   !> Set up a minimal 1-cell s-point grid so that get_location_target_properties
   !! and construct_target_mask do not dereference unallocated arrays.
   subroutine setup_minimal_grid()
      ndx = 1
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

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_averaging_params_defaults, test_averaging_params_defaults,
   !> When no averaging keywords are present, read_averaging_input must return
   !! the documented defaults: type=1 (mean), relSize=-1, numMin=1, percentile=0.
   subroutine test_averaging_params_defaults() bind(C)
      use m_spatial_field, only: t_averaging_input, read_averaging_input
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_create, tree_destroy
      use properties, only: prop_file

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
      call f90_expect_lt(avg%rel_size, 0.0_dp, "default rel_size should be negative (use EC default)")
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
      real(dp) :: tc(NTRANSFORMCOEF)

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

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_parse_location_type, test_parse_location_type,
   !> parse_location_type must map all recognized strings and default to ALL for unknown/empty.
   subroutine test_parse_location_type() bind(C)
      use m_spatial_field, only: parse_location_type
      use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL

      call f90_expect_eq(parse_location_type('1d'), ILATTP_1D, "'1d' should map to ILATTP_1D")
      call f90_expect_eq(parse_location_type('2d'), ILATTP_2D, "'2d' should map to ILATTP_2D")
      call f90_expect_eq(parse_location_type('1d2d'), ILATTP_ALL, "'1d2d' should map to ILATTP_ALL")
      call f90_expect_eq(parse_location_type('all'), ILATTP_ALL, "'all' should map to ILATTP_ALL")
      call f90_expect_eq(parse_location_type(' '), ILATTP_ALL, "empty string should default to ILATTP_ALL")
      call f90_expect_eq(parse_location_type('bogus'), ILATTP_ALL, "unknown string should default to ILATTP_ALL")
   end subroutine test_parse_location_type
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

     !$f90tw TESTCODE(TEST, test_init_spatial_field, test_resolve_parameter_target_unknown_quantity_returns_null, test_resolve_parameter_target_unknown_quantity_returns_null,
   !> An unrecognized quantity must return .false., leave target_array null and
   !! target_location_type at the sentinel value 0.
   !! This is the regression guard for the intent(out) bug: before the fix,
   !! calling a resolver for an unhandled quantity would leave target_location_type undefined.
   subroutine test_resolve_parameter_target_unknown_quantity_returns_null() bind(C)
      use unstruc_inifields, only: resolve_parameter_target
      use fm_location_types, only: UNC_LOC_S

      real(dp), dimension(:), pointer :: target_array
      integer :: target_location_type
      logical :: success

      target_array => null()
      target_location_type = UNC_LOC_S   ! must be overwritten to sentinel 0

      success = resolve_parameter_target('this_quantity_does_not_exist', 'test.ext', target_location_type, target_array)

      call f90_expect_false(success, "resolve_parameter_target should return .false. for an unrecognized quantity")
      call f90_expect_false(associated(target_array), "target_array should be null for an unrecognized parameter quantity")
      call f90_expect_eq(target_location_type, 0, "target_location_type should be sentinel 0 for an unrecognized quantity")
   end subroutine test_resolve_parameter_target_unknown_quantity_returns_null
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_resolve_parameter_target_friction_uses_loc_u, test_resolve_parameter_target_friction_uses_loc_u,
   !> frictioncoefficient must resolve to UNC_LOC_U (flow links), not the default UNC_LOC_S.
   !! This is the key parameter quantity where getting the location type wrong would silently
   !! apply friction values to the wrong element set.
   subroutine test_resolve_parameter_target_friction_uses_loc_u() bind(C)
      use unstruc_inifields, only: resolve_parameter_target
      use fm_location_types, only: UNC_LOC_U
      use m_flowgeom, only: lnx
      use m_flow, only: frcu
   
      real(dp), dimension(:), pointer :: target_array
      integer :: target_location_type
      logical :: success

      lnx = 1
      target_array => null()
      target_location_type = 0
      call realloc(frcu, 1, fill=0.0_dp, keepExisting=.false.)
      success = resolve_parameter_target('frictioncoefficient', 'test.ext', target_location_type, target_array)

      call f90_expect_true(success, "resolve_parameter_target should return .true. for frictioncoefficient")
      call f90_expect_true(associated(target_array), "target_array should be associated for frictioncoefficient")
      call f90_expect_eq(target_location_type, UNC_LOC_U, "frictioncoefficient must map to UNC_LOC_U, not UNC_LOC_S")

      lnx = 0
      if (associated(target_array)) nullify(target_array)
   end subroutine test_resolve_parameter_target_friction_uses_loc_u
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_resolve_initial_target_waterlevel_points_to_s1, test_resolve_initial_target_waterlevel_points_to_s1,
   !> initialwaterlevel must return .true. and resolve to a pointer associated with s1 itself.
   !! Pointer identity proves the resolver wired the correct target.
   subroutine test_resolve_initial_target_waterlevel_points_to_s1() bind(C)
      use unstruc_inifields, only: resolve_initial_target
      use fm_location_types, only: UNC_LOC_S
      use m_flow, only: s1
      use m_flowgeom, only: ndx
      use m_alloc, only: realloc

      real(dp), dimension(:), pointer :: target_array
      integer :: target_location_type
      logical :: success

      ndx = 1
      call realloc(s1, ndx, fill=0.0_dp, keepExisting=.false.)
      target_array => null()
      target_location_type = 0

      success = resolve_initial_target('initialwaterlevel', 'test.ext', target_location_type, target_array)

      call f90_expect_true(success, "resolve_initial_target should return .true. for initialwaterlevel")
      call f90_expect_true(associated(target_array), "target_array should be associated for initialwaterlevel")
      call f90_expect_eq(target_location_type, UNC_LOC_S, "initialwaterlevel must map to UNC_LOC_S")
      call f90_expect_true(associated(target_array, s1), "target_array must point directly to s1, not a copy")

      ndx = 0
      if (allocated(s1)) deallocate(s1)
   end subroutine test_resolve_initial_target_waterlevel_points_to_s1
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
      ndxi  = ndx
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

end module test_init_spatial_fields_integration
