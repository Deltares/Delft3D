module test_init_spatial_field
   use assertions_gtest
   use m_spatial_field, only: t_spatial_field_input, validate_spatial_field_input
   use m_wind, only: jaQext
   use timespace_parameters, only: OPERAND_ADD
   use unstruc_messages, only: threshold_abort
   use messagehandling, only: LEVEL_FATAL, LEVEL_WARN, GetMessageCount, GetMessage_MH, SetMessageHandling
   use m_alloc, only: realloc, reallocP
   use precision_basics, only: dp

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
      integer :: kx
      kx = 1
      target_array => null()
      target_location_type = UNC_LOC_S ! must be overwritten to sentinel 0

      success = resolve_parameter_target('this_quantity_does_not_exist', 'test.ext', target_location_type, target_array, kx)

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
      integer :: kx
      kx = 1
      lnx = 1
      target_array => null()
      target_location_type = 0
      call realloc(frcu, 1, fill=0.0_dp, keepExisting=.false.)
      success = resolve_parameter_target('frictioncoefficient', 'test.ext', target_location_type, target_array, kx)

      call f90_expect_true(success, "resolve_parameter_target should return .true. for frictioncoefficient")
      call f90_expect_true(associated(target_array), "target_array should be associated for frictioncoefficient")
      call f90_expect_eq(target_location_type, UNC_LOC_U, "frictioncoefficient must map to UNC_LOC_U, not UNC_LOC_S")

      lnx = 0
      if (associated(target_array)) nullify (target_array)
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
      if (allocated(s1)) deallocate (s1)
   end subroutine test_resolve_initial_target_waterlevel_points_to_s1
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_resolve_parameter_null_target, test_resolve_parameter_null_target,
   !> EC-driven quantities (sea_ice_area_fraction) must return success=.true.,
   !! null target_array and UNC_LOC_S. The null target is the key correctness
   !! invariant for the whole EC-only pattern: EC writes directly, no pointer needed.
   subroutine test_resolve_parameter_null_target() bind(C)
      use unstruc_inifields, only: resolve_parameter_target
      use fm_location_types, only: UNC_LOC_S
      use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read

      real(dp), dimension(:), pointer :: target_array
      integer :: target_location_type
      logical :: success
      integer :: kx

      ! ARRANGE: mark ice as already activated to skip fm_ice_activate_by_ext_forces side effect
      if (.not. associated(ja_ice_area_fraction_read)) then
         allocate(ja_ice_area_fraction_read)
      end if
      if (.not. associated(ja_ice_thickness_read)) then
         allocate(ja_ice_thickness_read)
      end if
      ja_ice_area_fraction_read = 1
      ja_ice_thickness_read = 1
      target_array => null()
      target_location_type = 0
      kx = 1

      ! ACT
      success = resolve_parameter_target('sea_ice_area_fraction', 'test.ext', target_location_type, target_array, kx)

      ! ASSERT
      call f90_expect_true(success, "sea_ice_area_fraction should be recognized by resolve_parameter_target")
      call f90_expect_false(associated(target_array), &
                            "target_array must be null for EC-driven quantities - EC writes directly via quantity name")
      call f90_expect_eq(target_location_type, UNC_LOC_S, "sea_ice_area_fraction must map to UNC_LOC_S")

      ja_ice_area_fraction_read = 0
      ja_ice_thickness_read = 0
   end subroutine test_resolve_parameter_null_target
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_resolve_parameter_target_wave_requires_wave_nc_offline, test_resolve_parameter_target_wave_requires_wave_nc_offline,
   !> wavesignificantheight must fail with success=.false. when jawave /= WAVE_NC_OFFLINE.
   !! This validation guard prevents silently ignoring wave quantities when the wave
   !! model is not configured, which would be a hard-to-diagnose runtime error.
   subroutine test_resolve_parameter_target_wave_requires_wave_nc_offline() bind(C)
      use unstruc_inifields, only: resolve_parameter_target
      use m_flowparameters, only: jawave
      use m_waveconst, only: WAVE_NC_OFFLINE

      real(dp), dimension(:), pointer :: target_array
      integer :: target_location_type
      logical :: success
      integer :: kx

      ! ARRANGE: wave model not configured
      jawave = 0
      target_array => null()
      target_location_type = 0
      kx = 1

      ! ACT
      success = resolve_parameter_target('wavesignificantheight', 'test.ext', target_location_type, target_array, kx)

      ! ASSERT
      call f90_expect_false(success, &
                            "wavesignificantheight must fail when WaveModelNr /= WAVE_NC_OFFLINE")
      call f90_expect_false(associated(target_array), &
                            "target_array must remain null on validation failure")
   end subroutine test_resolve_parameter_target_wave_requires_wave_nc_offline
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_resolve_parameter_target_nudge_saltemp_sets_kx_2, test_resolve_parameter_target_nudge_saltemp_sets_kx_2,
   !> nudgesalinitytemperature must set kx=2 because it carries two values per location
   !! (salinity + temperature). Any other value would cause EC to allocate the wrong
   !! number of target slots and silently corrupt one of the two fields.
   subroutine test_resolve_parameter_target_nudge_saltemp_sets_kx_2() bind(C)
      use unstruc_inifields, only: resolve_parameter_target
      use fm_location_types, only: UNC_LOC_S3D
      use m_flow, only: ndkx
      use m_cell_geometry, only: ndx

      real(dp), dimension(:), pointer :: target_array
      integer :: target_location_type
      logical :: success
      integer :: kx

      ! ARRANGE: minimal ndx/ndkx so alloc_nudging does not dereference null
      ndx = 1
      ndkx = 1
      target_array => null()
      target_location_type = 0
      kx = 1

      ! ACT
      success = resolve_parameter_target('nudgesalinitytemperature', 'test.ext', target_location_type, target_array, kx)

      ! ASSERT
      call f90_expect_true(success, "nudgesalinitytemperature should be recognized")
      call f90_expect_eq(kx, 2, &
                         "nudgesalinitytemperature must set kx=2 (salinity + temperature per location)")
      call f90_expect_eq(target_location_type, UNC_LOC_S3D, &
                         "nudgesalinitytemperature must map to UNC_LOC_S3D")
      call f90_expect_false(associated(target_array), &
                            "target_array must be null - EC drives nudging directly")

      ndx = 0
      ndkx = 0
   end subroutine test_resolve_parameter_target_nudge_saltemp_sets_kx_2
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

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_radiation_stress, test_offline_wave_requirements_radiation_stress,
   subroutine test_offline_wave_requirements_radiation_stress() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_RADIATION_STRESS, WAVE_FORCES_ON, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, WAVE_BOUNDARYLAYER_OFF, &
                                                          .false., .false.)

      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_PERIOD), &
                           "radiation-stress force limiting requires waveperiod")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_FORCE_X), &
                           "radiation-stress forcing requires xwaveforce")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_FORCE_Y), &
                           "radiation-stress forcing requires ywaveforce")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT), &
                            "direct radiation-stress forcing does not require wave height")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_DIRECTION), &
                            "direct radiation-stress forcing does not require direction")
   end subroutine test_offline_wave_requirements_radiation_stress
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_stokes_adds_kinematics, test_offline_wave_requirements_stokes_adds_kinematics,
   subroutine test_offline_wave_requirements_stokes_adds_kinematics() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_RADIATION_STRESS, WAVE_FORCES_ON, &
                                                          STOKES_DRIFT_DEPTHUNIFORM, WAVE_STREAMING_OFF, &
                                                          WAVE_BOUNDARYLAYER_OFF, .false., .false.)

      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT), &
                           "Stokes drift requires wave height")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_PERIOD), &
                           "Stokes drift requires wave period")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_DIRECTION), &
                           "Stokes drift requires wave direction")
   end subroutine test_offline_wave_requirements_stokes_adds_kinematics
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_flow_without_waves, test_offline_wave_requirements_flow_without_waves,
   subroutine test_offline_wave_requirements_flow_without_waves() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_NO_WAVEFORCES, WAVE_FORCES_OFF, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, &
                                                          WAVE_BOUNDARYLAYER_OFF, .false., .true.)

      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT), &
                           "FlowWithoutWaves requires wave height for D-WAQ orbital velocity")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_PERIOD), &
                           "FlowWithoutWaves requires wave period for D-WAQ orbital velocity")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_DIRECTION), &
                            "FlowWithoutWaves does not require wave direction")
   end subroutine test_offline_wave_requirements_flow_without_waves
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_disabled_forces, test_offline_wave_requirements_disabled_forces,
   subroutine test_offline_wave_requirements_disabled_forces() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_DISSIPATION_TOTAL, WAVE_FORCES_OFF, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, WAVE_BOUNDARYLAYER_OFF, &
                                                          .false., .false.)

      call f90_expect_eq(requirements, 0, "no active wave consumer should require no offline wave input")
   end subroutine test_offline_wave_requirements_disabled_forces
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_3d_dissipation, test_offline_wave_requirements_3d_dissipation,
   subroutine test_offline_wave_requirements_3d_dissipation() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_DISSIPATION_3D, WAVE_FORCES_ON, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, WAVE_BOUNDARYLAYER_OFF, &
                                                          .false., .false.)

      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT), "3D dissipation requires wave height")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_PERIOD), "3D dissipation requires wave period")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_DIRECTION), "3D dissipation requires wave direction")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_FORCE_X), "3D dissipation requires xwaveforce")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_FORCE_Y), "3D dissipation requires ywaveforce")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_DISSIPATION_SURFACE), &
                           "3D dissipation requires surface-breaking dissipation")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_DISSIPATION_WHITE_CAPPING), &
                           "3D dissipation requires white-capping dissipation")
   end subroutine test_offline_wave_requirements_3d_dissipation
   !$f90tw)

end module test_init_spatial_field
