# test dependencies
set(dependencies deltares_common)

create_test(
    tests_string_utils
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/utils_lgpl/deltares_common/packages/deltares_common
    test_files tests_string_utils.f90
    test_list 
        test_dlwq13_recognise_nanstest_string_equals_default_settings 
        test_string_equals_with_custom_settings 
        test_index_in_array_default_settings 
        test_index_in_array_custom_settings
    labels 
        "test_dlwq13_recognise_nanstest_string_equals_default_settings:fast" 
        "test_string_equals_with_custom_settings:fast"
        "test_index_in_array_default_settings:fast" 
        "test_index_in_array_custom_settings:fast"
)
