# test dependencies
set(dependencies waq_utils_f)

create_test(
    tests_file_path_utils_get_file_name
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_utils
    test_list test_get_file_name
    test_files tests_file_path_utils.f90
    )

create_test(
    tests_file_path_utils_get_filename_without_extension
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_utils
    test_list test_get_filename_without_extension
    test_files tests_file_path_utils.f90
    )