module test_init_spatial_field
   use assertions_gtest
   use m_spatial_field, only: t_spatial_field_input, validate_spatial_field_input, is_static_spatial_input
   use m_wind, only: jaQext
   use timespace_parameters, only: DATAVALUE, OPERAND_ADD, METHOD_TRIANGULATION, METHOD_AVERAGING, METHOD_CONSTANT, &
                                  WEIGHTFACTORS, WEIGHTFACTORS_EXTRAPOLATION, SPACEANDTIME, NCFLOW, JUSTUPDATE
   use unstruc_messages, only: threshold_abort
   use messagehandling, only: LEVEL_FATAL, LEVEL_WARN, GetMessageCount, GetMessage_MH, SetMessageHandling
   use m_alloc, only: realloc, reallocP
   use precision_basics, only: dp
   use iso_c_utils, only: cstr

   implicit none(type, external)

   character(len=*), parameter :: EXT_FILENAME = "test.ext"
   character(len=*), parameter :: GROUP_NAME = "Spatial"
   character(len=*), parameter :: BASE_DIR = "."

contains

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_netcdf_generic_quantity_uses_explicit_variable, test_netcdf_generic_quantity_uses_explicit_variable,
   subroutine test_netcdf_generic_quantity_uses_explicit_variable() bind(C)
      use m_ec_support, only: ecSupportNetcdfGetQuantityCandidateNames

      character(len=256), dimension(:), allocatable :: variable_names, standard_names, fallback_names

      call ecSupportNetcdfGetQuantityCandidateNames('test.nc', 'arbitrary_target', standard_names, &
                                                   variable_names, fallback_names, varname='custom_field')
      call f90_assert_true(allocated(variable_names))
      call f90_assert_true(allocated(standard_names))
      call f90_expect_eq(size(variable_names), 1)
      call f90_expect_eq(size(standard_names), 1)
      call f90_expect_streq(cstr(variable_names(1)), cstr('custom_field'))
      call f90_expect_streq(cstr(standard_names(1)), cstr('custom_field'))
      call f90_expect_false(allocated(fallback_names))
   end subroutine test_netcdf_generic_quantity_uses_explicit_variable
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_netcdf_generic_quantity_requires_variable, test_netcdf_generic_quantity_requires_variable,
   subroutine test_netcdf_generic_quantity_requires_variable() bind(C)
      use m_ec_support, only: ecSupportNetcdfGetQuantityCandidateNames
      use m_ec_message, only: clear_ec_message

      character(len=256), dimension(:), allocatable :: variable_names, standard_names, fallback_names

      call clear_ec_message()
      call ecSupportNetcdfGetQuantityCandidateNames('test.nc', 'arbitrary_target', standard_names, &
                                                   variable_names, fallback_names)
      call f90_expect_false(allocated(variable_names))
      call f90_expect_false(allocated(standard_names))
      call clear_ec_message()
      call ecSupportNetcdfGetQuantityCandidateNames('test.nc', 'arbitrary_target', standard_names, &
                                                   variable_names, fallback_names, varname='')
      call f90_expect_false(allocated(variable_names))
      call f90_expect_false(allocated(standard_names))
      call clear_ec_message()
   end subroutine test_netcdf_generic_quantity_requires_variable
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_initial_netcdf_normalizes_quantity, test_initial_netcdf_normalizes_quantity,
   subroutine test_initial_netcdf_normalizes_quantity() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, quantity='initialSecchiDepth')
      call f90_expect_true(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR))
      call f90_expect_true(input%is_static_field)
      call f90_expect_streq(cstr(input%quantity), cstr('secchidepth'))
      call f90_expect_eq(input%method, WEIGHTFACTORS)
   end subroutine test_initial_netcdf_normalizes_quantity
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_initial_modifier_preserves_suffix, test_initial_modifier_preserves_suffix,
   subroutine test_initial_modifier_preserves_suffix() bind(C)
      character(len=64) :: quantity

      quantity = 'initialtracerNO3'
      call f90_expect_true(is_static_spatial_input('netcdf', WEIGHTFACTORS, quantity))
      call f90_expect_streq(cstr(quantity), cstr('tracerNO3'))
      quantity = 'secchidepth'
      call f90_expect_false(is_static_spatial_input('netcdf', WEIGHTFACTORS, quantity))
      call f90_expect_streq(cstr(quantity), cstr('secchidepth'))
      quantity = 'initialverticalsalinityprofile'
      call f90_expect_true(is_static_spatial_input('netcdf', WEIGHTFACTORS, quantity))
      call f90_expect_streq(cstr(quantity), cstr('initialverticalsalinityprofile'))
   end subroutine test_initial_modifier_preserves_suffix
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_resolvers_select_3d_targets, test_resolvers_select_3d_targets,
   subroutine test_resolvers_select_3d_targets() bind(C)
      use unstruc_inifields, only: resolve_initial_target, resolve_constituent_target
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_S3D
      use m_flow, only: sa1, tem1, kmx, ndkx
      use m_flowparameters, only: jasal, temperature_model, TEMPERATURE_MODEL_TRANSPORT, initem2D

      real(dp), dimension(:), pointer :: target, target_row
      real(dp), dimension(:, :), pointer :: constituent_target
      integer :: location, first_index, saved_kmx, saved_ndkx
      integer :: saved_jasal, saved_temperature_model, saved_initem2D
      logical :: success

      saved_kmx = kmx
      saved_ndkx = ndkx
      saved_jasal = jasal
      saved_temperature_model = temperature_model
      saved_initem2D = initem2D
      kmx = 2
      ndkx = 3
      jasal = 1
      temperature_model = TEMPERATURE_MODEL_TRANSPORT
      threshold_abort = LEVEL_FATAL
      call realloc(sa1, ndkx, fill=42.0_dp, keepExisting=.false.)
      call realloc(tem1, ndkx, fill=42.0_dp, keepExisting=.false.)

      success = resolve_constituent_target('salinity', location, constituent_target, first_index)
      call f90_expect_true(success)
      call f90_expect_eq(location, UNC_LOC_S, 'default salinity input must remain horizontal')
      success = resolve_constituent_target('salinity', location, constituent_target, first_index, target_layer='3D')
      call f90_expect_true(success)
      call f90_expect_eq(location, UNC_LOC_S3D)
      call f90_expect_eq(first_index, 1)
      target_row => constituent_target(first_index, :)
      call f90_expect_true(associated(target_row, sa1), 'resolver must retain full salinity storage')

      success = resolve_initial_target('temperature', location, target, target_layer='all')
      call f90_expect_true(success)
      call f90_expect_eq(location, UNC_LOC_S)
      call f90_expect_eq(initem2D, 1)
      success = resolve_initial_target('temperature', location, target, target_layer='3d')
      call f90_expect_true(success)
      call f90_expect_eq(location, UNC_LOC_S3D)
      call f90_expect_true(associated(target, tem1), 'resolver must retain full temperature storage')
      call f90_expect_eq(initem2D, 0, 'resolver owns temperature expansion suppression')

      kmx = 0
      call f90_expect_false(resolve_initial_target('temperature', location, target, target_layer='3d'))
      call f90_expect_false(resolve_constituent_target('salinity', location, constituent_target, first_index, target_layer='3d'))

      kmx = saved_kmx
      ndkx = saved_ndkx
      jasal = saved_jasal
      temperature_model = saved_temperature_model
      initem2D = saved_initem2D
      deallocate (sa1, tem1)
   end subroutine test_resolvers_select_3d_targets
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_3d_target_requires_initial_netcdf, test_3d_target_requires_initial_netcdf,
   subroutine test_3d_target_requires_initial_netcdf() bind(C)
      type(t_spatial_field_input) :: input

      threshold_abort = LEVEL_FATAL
      call make_test_input(input, quantity='initialSalinity')
      input%target_layer = '3D'
      call f90_expect_true(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                  'initialSalinity with targetLayer=3D and NetCDF must pass input validation')
      call make_test_input(input, quantity='initialSalinity', forcing_file='dummy.xyz', forcing_file_type='sample')
      input%target_layer = '3d'
      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                   'initialSalinity with targetLayer=3d and sample input must fail input validation')
      !call make_test_input(input, quantity='salinity')
      !input%target_layer = '3d'
      !call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR))
   end subroutine test_3d_target_requires_initial_netcdf
   !$f90tw)

   subroutine make_test_input( &
         input, quantity, forcing_file, forcing_file_type, target_mask_file, interpolation_method, &
         operand_string, data_value, is_extrapolation_allowed)
      use m_missing, only: dmiss

      type(t_spatial_field_input), intent(out) :: input
      character(len=*), intent(in), optional :: quantity
      character(len=*), intent(in), optional :: forcing_file
      character(len=*), intent(in), optional :: forcing_file_type
      character(len=*), intent(in), optional :: target_mask_file
      character(len=*), intent(in), optional :: interpolation_method
      character(len=*), intent(in), optional :: operand_string
      real(dp), intent(in), optional :: data_value
      logical, intent(in), optional :: is_extrapolation_allowed

      input%quantity = 'windx'
      if (present(quantity)) then
         input%quantity = quantity
      end if

      input%forcing_file = 'dummy.nc'
      if (present(forcing_file)) then
         input%forcing_file = forcing_file
      end if

      input%forcing_file_type = 'netcdf'
      if (present(forcing_file_type)) then
         input%forcing_file_type = forcing_file_type
      end if

      if (present(target_mask_file)) then
         input%target_mask_file = target_mask_file
      end if
      if (present(interpolation_method)) then
         input%interpolation_method = interpolation_method
      end if
      if (present(operand_string)) then
         input%operand_string = operand_string
      end if

      input%data_value = dmiss
      if (present(data_value)) then
         input%data_value = data_value
      end if

      if (present(is_extrapolation_allowed)) then
         input%is_extrapolation_allowed = is_extrapolation_allowed
      end if
   end subroutine make_test_input

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_rejects_unknown_interpolation_method_string, test_validate_rejects_unknown_interpolation_method_string,
   !> An unrecognized interpolationMethod= string leaves method at -1 and must fail.
   !! This branch is never exercised by integration tests because they always use
   !! valid file types with known method strings.
   subroutine test_validate_rejects_unknown_interpolation_method_string() bind(C)
      type(t_spatial_field_input) :: input
      logical :: success
      call make_test_input(input, interpolation_method='this_method_does_not_exist')
      threshold_abort = LEVEL_FATAL
      success = validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR)
      call f90_expect_false(success, "validation should fail when interpolationMethod is unrecognized")
   end subroutine test_validate_rejects_unknown_interpolation_method_string
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_rejects_constant_method_for_sample_file, test_validate_rejects_constant_method_for_sample_file,
   subroutine test_validate_rejects_constant_method_for_sample_file() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, forcing_file='dummy.xyz', forcing_file_type='sample', interpolation_method='constant')
      threshold_abort = LEVEL_FATAL

      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            'sample files should reject constant interpolation')
   end subroutine test_validate_rejects_constant_method_for_sample_file
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_rejects_averaging_method_for_bcascii, test_validate_rejects_averaging_method_for_bcascii,
   subroutine test_validate_rejects_averaging_method_for_bcascii() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, forcing_file='dummy.bc', forcing_file_type='bcascii', interpolation_method='averaging')
      threshold_abort = LEVEL_FATAL

      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            'bcascii files should reject averaging interpolation')
   end subroutine test_validate_rejects_averaging_method_for_bcascii
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_rejects_triangulation_method_for_bcascii, test_validate_rejects_triangulation_method_for_bcascii,
   subroutine test_validate_rejects_triangulation_method_for_bcascii() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, forcing_file='dummy.bc', forcing_file_type='bcascii', interpolation_method='triangulation')
      threshold_abort = LEVEL_FATAL

      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            'bcascii files should reject triangulation interpolation')
   end subroutine test_validate_rejects_triangulation_method_for_bcascii
   !$f90tw)


   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_spiderweb_defaults_to_weightfactors, test_validate_spiderweb_defaults_to_weightfactors,
   subroutine test_validate_spiderweb_defaults_to_weightfactors() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, forcing_file='dummy.spw', forcing_file_type='spiderweb')

      call f90_expect_true(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                           'spiderweb should use its default when interpolationMethod is omitted')
      call f90_expect_eq(input%method, WEIGHTFACTORS, 'spiderweb should default to weightfactors')
   end subroutine test_validate_spiderweb_defaults_to_weightfactors
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_spiderweb_accepts_linearspacetime_extrapolation, test_validate_spiderweb_accepts_linearspacetime_extrapolation,
   subroutine test_validate_spiderweb_accepts_linearspacetime_extrapolation() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, forcing_file='dummy.spw', forcing_file_type='spiderweb', &
                           interpolation_method='linearSpaceTime', is_extrapolation_allowed=.true.)

      call f90_expect_true(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                           'spiderweb should accept linearSpaceTime with extrapolation enabled')
      call f90_expect_eq(input%method, WEIGHTFACTORS_EXTRAPOLATION, &
                         'linearSpaceTime with extrapolation should select weightfactors_extrapolation')
   end subroutine test_validate_spiderweb_accepts_linearspacetime_extrapolation
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_spiderweb_rejects_constant_method, test_validate_spiderweb_rejects_constant_method,
   subroutine test_validate_spiderweb_rejects_constant_method() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, forcing_file='dummy.spw', forcing_file_type='spiderweb', interpolation_method='constant')
      threshold_abort = LEVEL_FATAL

      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            'spiderweb should reject constant interpolation because its default is weightfactors')
   end subroutine test_validate_spiderweb_rejects_constant_method
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_dflowfm_map_file_type, test_validate_dflowfm_map_file_type,
   subroutine test_validate_dflowfm_map_file_type() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, quantity='initialwaterlevel', forcing_file='flow_map.nc', forcing_file_type='map')

      call f90_expect_true(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                  "map should accept an FM map NetCDF file")
      call f90_expect_eq(input%filetype, NCFLOW)
      call f90_expect_eq(input%method, METHOD_TRIANGULATION)
      call f90_expect_true(input%is_static_field, "map should be initialized as a static spatial field")
   end subroutine test_validate_dflowfm_map_file_type
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_inline_polygon_selection_restores_polygon_state, test_inline_polygon_selection_restores_polygon_state,
   subroutine test_inline_polygon_selection_restores_polygon_state() bind(C)
      use m_polygon, only: increasepol, npl, xpl, ypl, zpl
      use timespace, only: selectelset_internal_nodes
      use timespace_parameters, only: LOCTP_POLYGON_XY

      real(dp), parameter :: target_x(2) = [5.0_dp, 15.0_dp]
      real(dp), parameter :: target_y(2) = [5.0_dp, 5.0_dp]
      integer, parameter :: mask(2) = [1, 1]
      integer :: selected_nodes(2)
      integer :: num_selected
      real(dp), parameter :: selection_x(4) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      real(dp), parameter :: selection_y(4) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      real(dp), parameter :: sentinel_x(2) = [101.0_dp, 102.0_dp]
      real(dp), parameter :: sentinel_y(2) = [201.0_dp, 202.0_dp]
      real(dp), parameter :: sentinel_z(2) = [301.0_dp, 302.0_dp]

      call increasepol(size(sentinel_x), 0)
      xpl(1:size(sentinel_x)) = sentinel_x
      ypl(1:size(sentinel_y)) = sentinel_y
      zpl(1:size(sentinel_z)) = sentinel_z
      npl = size(sentinel_x)

      call selectelset_internal_nodes(target_x, target_y, mask, size(target_x), selected_nodes, num_selected, &
                                      LOCTP_POLYGON_XY, numcoord=size(selection_x), xpin=selection_x, ypin=selection_y)

      call F90_ASSERT_EQ(num_selected, 1)
      if (num_selected == 1) then
         call F90_ASSERT_EQ(selected_nodes(1), 1)
      end if
      call F90_ASSERT_EQ(npl, size(sentinel_x))
      call F90_ASSERT_TRUE(all(xpl(1:npl) == sentinel_x))
      call F90_ASSERT_TRUE(all(ypl(1:npl) == sentinel_y))
      call F90_ASSERT_TRUE(all(zpl(1:npl) == sentinel_z))
   end subroutine test_inline_polygon_selection_restores_polygon_state
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_file_type_extension_mismatch, test_validate_file_type_extension_mismatch,
   subroutine test_validate_file_type_extension_mismatch() bind(C)
      type(t_spatial_field_input) :: input
      integer :: log_level
      character(len=512) :: message

      call make_test_input(input, forcing_file_type='bcascii')
      input%interpolation_method = ' ' ! no explicit method either
      threshold_abort = LEVEL_FATAL
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)

      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "validation should fail when forcingFileType does not match input file extension")
      call f90_expect_eq(GetMessageCount(), 1)
      log_level = GetMessage_MH(1, message)
      call f90_expect_true(index(message, 'Accepted extensions: .bc.') > 0)
   end subroutine test_validate_file_type_extension_mismatch
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_supported_extensions_select_effective_methods, test_validate_supported_extensions_select_effective_methods,
   subroutine test_validate_supported_extensions_select_effective_methods() bind(C)
      character(len=16), parameter :: file_types(11) = [character(len=16) :: &
         '1dfield', 'arcinfo', 'bcascii', 'curvigrid', 'geotiff', 'netcdf', 'polygon', 'sample', 'spiderweb', 'uniform', 'unimagdir']
      character(len=16), parameter :: extensions(11) = [character(len=16) :: &
         '.ini', '.aice', '.bc', '.apwxwy', '.tiff', '.nc', '.pliz', '.xyb', '.spw', '.tem', '.wnd']
      character(len=16), parameter :: methods(11) = [character(len=16) :: &
         '', 'linearSpaceTime', 'linearSpaceTime', '', '', 'linearSpaceTime', &
         'constant', 'triangulation', 'linearSpaceTime', 'linearSpaceTime', '']
      integer, parameter :: expected_methods(11) = [JUSTUPDATE, SPACEANDTIME, SPACEANDTIME, WEIGHTFACTORS, &
                                                     METHOD_TRIANGULATION, WEIGHTFACTORS, METHOD_CONSTANT, METHOD_TRIANGULATION, &
                                                     WEIGHTFACTORS, SPACEANDTIME, SPACEANDTIME]
      type(t_spatial_field_input) :: input
      integer :: i

      do i = 1, size(file_types)
         call make_test_input(input, forcing_file='dummy'//trim(extensions(i)), forcing_file_type=trim(file_types(i)), &
                              interpolation_method=trim(methods(i)))
         call f90_expect_true(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                              trim(file_types(i))//' should accept '//trim(extensions(i)))
         call f90_expect_eq(input%method, expected_methods(i), trim(file_types(i))//' should select the expected effective method')
      end do
   end subroutine test_validate_supported_extensions_select_effective_methods
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_unknown_file_extension, test_validate_unknown_file_extension,
   subroutine test_validate_unknown_file_extension() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, forcing_file='dummy.unsupported')
      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "validation should fail for an extension unsupported by forcingFileType")
   end subroutine test_validate_unknown_file_extension
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_validate_nonexistent_target_mask_file, test_validate_nonexistent_target_mask_file,
   !> Specifying a targetMaskFile= that does not exist on disk must fail.
   !! The inquire() branch inside validate_spatial_field_input is never reached
   !! in integration tests because they either omit the mask or supply a real file.
   subroutine test_validate_nonexistent_target_mask_file() bind(C)
      type(t_spatial_field_input) :: input
      call make_test_input(input, target_mask_file='this_mask_does_not_exist.pol')
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

      call make_test_input(input_without_extrap, is_extrapolation_allowed=.false.)
      success_without = validate_spatial_field_input(input_without_extrap, EXT_FILENAME, GROUP_NAME, BASE_DIR)
      call f90_assert_true(success_without, "baseline validation without extrapolation should succeed")

      call make_test_input(input_with_extrap, is_extrapolation_allowed=.true.)
      success_with = validate_spatial_field_input(input_with_extrap, EXT_FILENAME, GROUP_NAME, BASE_DIR)
      call f90_assert_true(success_with, "validation with extrapolation should succeed")

      call f90_expect_true(input_with_extrap%method /= input_without_extrap%method, &
                           "enabling extrapolation should produce a different method value")
   end subroutine test_validate_extrapolation_changes_method
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_spatial_field_input__data_value, test_validate_spatial_field_input__data_value,
   subroutine test_validate_spatial_field_input__data_value() bind(C)
      type(t_spatial_field_input) :: input
      logical :: success

      call make_test_input(input, data_value=0.875_dp, forcing_file_type="", forcing_file="")

      success = validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR)

      call f90_assert_true(success, cstr("forcing_file_type and forcing_file may be empty if data_value is supplied"))
   end subroutine test_validate_spatial_field_input__data_value
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_datavalue_type_accepts_constant_method, test_validate_datavalue_type_accepts_constant_method,
   subroutine test_validate_datavalue_type_accepts_constant_method() bind(C)
      type(t_spatial_field_input) :: input
      logical :: success

      call make_test_input(input, data_value=0.875_dp, forcing_file_type='datavalue', forcing_file='', &
                           interpolation_method='constant')

      success = validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR)

      call f90_expect_true(success, "dataValue may explicitly use dataFileType=datavalue")
      call f90_expect_eq(input%filetype, DATAVALUE)
      call f90_expect_eq(input%method, METHOD_CONSTANT, 'dataValue should accept constant interpolation')
   end subroutine test_validate_datavalue_type_accepts_constant_method
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_data_value_with_file_type_fails, test_validate_data_value_with_file_type_fails,
   subroutine test_validate_data_value_with_file_type_fails() bind(C)
      type(t_spatial_field_input) :: input

      call make_test_input(input, data_value=0.875_dp, forcing_file_type="not_a_file_type", forcing_file="")

      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "dataValue cannot be combined with dataFileType")
   end subroutine test_validate_data_value_with_file_type_fails
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field,
   !$f90tw test_validate_unknown_file_type_message, test_validate_unknown_file_type_message,
   subroutine test_validate_unknown_file_type_message() bind(C)
      type(t_spatial_field_input) :: input
      integer :: log_level
      character(len=512) :: message

      call make_test_input(input, forcing_file_type="not_a_file_type")
      threshold_abort = LEVEL_FATAL
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)

      call f90_expect_false(validate_spatial_field_input(input, EXT_FILENAME, GROUP_NAME, BASE_DIR), &
                            "validation should reject an unknown dataFileType")
      call f90_expect_eq(GetMessageCount(), 1)
      log_level = GetMessage_MH(1, message)
      call f90_expect_true(index(message, "Field 'dataFileType' has unknown value 'not_a_file_type'") > 0)
   end subroutine test_validate_unknown_file_type_message
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

      success = resolve_initial_target('waterlevel', target_location_type, target_array)

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

      call make_test_input(input, operand_string='+')

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

      call make_test_input(input, operand_string='add')

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
                                                          .false., .false., WAVE_BREAKER_TURB_OFF)

      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_PERIOD), &
                   "direct radiation-stress forcing does not require wave period")
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
                                                          WAVE_BOUNDARYLAYER_OFF, .false., .false., WAVE_BREAKER_TURB_OFF)

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
                                                          WAVE_BOUNDARYLAYER_OFF, .false., .true., WAVE_BREAKER_TURB_OFF)

      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT), &
                           "FlowWithoutWaves requires wave height for D-WAQ orbital velocity")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_PERIOD), &
                           "FlowWithoutWaves requires wave period for D-WAQ orbital velocity")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_DIRECTION), &
                            "FlowWithoutWaves does not require wave direction")
   end subroutine test_offline_wave_requirements_flow_without_waves
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_breaker_turbulence, test_offline_wave_requirements_breaker_turbulence,
   subroutine test_offline_wave_requirements_breaker_turbulence() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_NO_WAVEFORCES, WAVE_FORCES_OFF, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, &
                                                          WAVE_BOUNDARYLAYER_OFF, .false., .false., WAVE_BREAKER_TURB_ON)

      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT), &
                           "breaker turbulence requires wave height")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_DISSIPATION_SURFACE), &
                           "breaker turbulence requires surface-breaking dissipation")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_DISSIPATION_WHITE_CAPPING), &
                           "breaker turbulence requires white-capping dissipation")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_DIRECTION), &
                            "breaker turbulence alone does not require wave direction")

      requirements = get_offline_wave_input_requirements(WAVEFORCING_NO_WAVEFORCES, WAVE_FORCES_OFF, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, &
                                                          WAVE_BOUNDARYLAYER_OFF, .false., .true., WAVE_BREAKER_TURB_ON)

      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT), &
                           "FlowWithoutWaves still requires wave height")
      call f90_expect_true(wave_input_is_required(requirements, WAVE_INPUT_PERIOD), &
                           "FlowWithoutWaves still requires wave period")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_DISSIPATION_SURFACE), &
                   "FlowWithoutWaves does not use surface-breaking dissipation")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_DISSIPATION_WHITE_CAPPING), &
                   "FlowWithoutWaves does not use white-capping dissipation")
      call f90_expect_false(wave_input_is_required(requirements, WAVE_INPUT_DIRECTION), &
                            "FlowWithoutWaves with breaker turbulence does not require wave direction")
   end subroutine test_offline_wave_requirements_breaker_turbulence
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_disabled_forces, test_offline_wave_requirements_disabled_forces,
   subroutine test_offline_wave_requirements_disabled_forces() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_DISSIPATION_TOTAL, WAVE_FORCES_OFF, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, WAVE_BOUNDARYLAYER_OFF, &
                                                          .false., .false., WAVE_BREAKER_TURB_OFF)

      call f90_expect_eq(requirements, 0, "no active wave consumer should require no offline wave input")
   end subroutine test_offline_wave_requirements_disabled_forces
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_offline_wave_requirements_3d_dissipation, test_offline_wave_requirements_3d_dissipation,
   subroutine test_offline_wave_requirements_3d_dissipation() bind(C)
      use m_waveconst

      integer :: requirements

      requirements = get_offline_wave_input_requirements(WAVEFORCING_DISSIPATION_3D, WAVE_FORCES_ON, &
                                                          NO_STOKES_DRIFT, WAVE_STREAMING_OFF, WAVE_BOUNDARYLAYER_OFF, &
                                                          .false., .false., WAVE_BREAKER_TURB_OFF)

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
