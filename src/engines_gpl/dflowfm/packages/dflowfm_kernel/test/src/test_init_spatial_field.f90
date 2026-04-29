module test_init_spatial_field
   use assertions_gtest
   use m_spatial_field, only: t_spatial_field_input, validate_spatial_field_input
   use m_wind, only: jaQext
   use timespace_parameters, only: OPERAND_ADD
   use unstruc_messages, only: threshold_abort
   use messagehandling, only: LEVEL_FATAL, LEVEL_WARN, GetMessageCount, GetMessage_MH, SetMessageHandling

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

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_qext_wrong_file_type, test_validate_qext_wrong_file_type,
   !> quantity='qext' with a forcingFileType other than 'sample' must fail even
   !! when jaQext=1. This quantity-specific constraint is a separate code path
   !! from the generic validation and is not covered by integration tests.
   subroutine test_validate_qext_wrong_file_type() bind(C)
      type(t_spatial_field_input) :: input
      call make_test_input(input)
      input%quantity = 'qext'
      input%forcing_file_type = 'netcdf' ! must be 'sample' for qext
      jaQext = 1
      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "validation should fail when qext is used with a non-sample forcingFileType")
      jaQext = 0 ! restore global state
   end subroutine test_validate_qext_wrong_file_type
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_legacy_operand_warns_but_succeeds, test_validate_legacy_operand_warns_but_succeeds,
   !> Legacy single-character operand values remain supported for backward
   !! compatibility, but they must produce a deprecation warning.
   subroutine test_validate_legacy_operand_warns_but_succeeds() bind(C)
      type(t_spatial_field_input) :: input
      logical :: success
      integer :: log_level
      character(len=512) :: message

      call make_test_input(input)
      input%operand_string = '+'

      threshold_abort = LEVEL_FATAL
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)

      success = validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR)

      call f90_expect_true(success, "validation should succeed for legacy single-character operand values")
      call f90_expect_eq(input%oper, OPERAND_ADD)
      call f90_expect_eq(GetMessageCount(), 1)

      log_level = GetMessage_MH(1, message)
      call f90_expect_eq(log_level, LEVEL_WARN)
      call f90_expect_true(index(message, 'deprecated') > 0)
   end subroutine test_validate_legacy_operand_warns_but_succeeds
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_nonlegacy_operand_does_not_warn, test_validate_nonlegacy_operand_does_not_warn,
   subroutine test_validate_nonlegacy_operand_does_not_warn() bind(C)
      type(t_spatial_field_input) :: input
      logical :: success

      call make_test_input(input)
      input%operand_string = 'add'

      threshold_abort = LEVEL_FATAL
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)

      success = validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR)

      call f90_expect_true(success, "validation should succeed for non-legacy operand values")
      call f90_expect_eq(input%oper, OPERAND_ADD)
      call f90_expect_eq(GetMessageCount(), 0)
   end subroutine test_validate_nonlegacy_operand_does_not_warn
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

end module test_init_spatial_fields_integration
