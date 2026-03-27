module test_init_spatial_field
   use assertions_gtest
   use m_spatial_field, only: t_spatial_field_input, validate_spatial_field_input
   use m_file_helpers, only: create_file
   use m_wind, only: jaQext

   implicit none(type, external)

   character(len=*), parameter :: EXT_FILENAME = "test.ext"
   character(len=*), parameter :: GROUP_NAME   = "Spatial"
   character(len=*), parameter :: BASE_DIR      = "."

contains

   subroutine make_test_input(input)
      type(t_spatial_field_input), intent(out) :: input
      input%quantity          = 'windx'
      input%forcing_file_type = 'netcdf'
      input%forcing_file      = 'dummy.nc'
   end subroutine make_test_input

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_unrecognized_interpolation_method, test_validate_unrecognized_interpolation_method,
   !> An unrecognized interpolationMethod= string leaves method at -1 and must fail.
   !! This branch is never exercised by integration tests because they always use
   !! valid file types with known method strings.
   subroutine test_validate_unrecognized_interpolation_method() bind(C)
      type(t_spatial_field_input) :: input
      logical :: success
      call make_test_input(input)
      input%interpolation_method = 'this_method_does_not_exist'
      success = validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR)
      call f90_expect_false(success, "validation should fail when interpolationMethod is unrecognized")
   end subroutine test_validate_unrecognized_interpolation_method
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_file_type_extension_mismatch, test_validate_file_type_extension_mismatch,
   subroutine test_validate_file_type_extension_mismatch() bind(C)
      type(t_spatial_field_input) :: input
      call make_test_input(input)
      input%forcing_file_type    = 'bcascii'     ! bcascii has no spatial default method
      input%interpolation_method = ' '           ! no explicit method either
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
      input%quantity          = 'qext'
      input%forcing_file_type = 'netcdf'   ! must be 'sample' for qext
      jaQext = 1
      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "validation should fail when qext is used with a non-sample forcingFileType")
      jaQext = 0  ! restore global state
   end subroutine test_validate_qext_wrong_file_type
   !$f90tw)

end module test_init_spatial_field