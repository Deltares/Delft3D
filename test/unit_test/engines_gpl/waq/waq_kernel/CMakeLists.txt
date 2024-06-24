# test dependencies
set(dependencies delwaq_lib
        waq_kernel
)

create_test(
        test_write_restart_map_file
        dependencies ${dependencies}
        visual_studio_folder tests/unit_test/engines_gpl/waq/waq_kernel
        test_files test_write_restart_map_file.f90
        test_list
            test_write_restart_map_file_recognise_nans
            test_write_restart_map_file_no_nans
            test_write_restart_map_file_with_nans
        include_dir ${CMAKE_CURRENT_SOURCE_DIR}/data/
        labels "test_write_restart_map_file_recognise_nans:fast" "test_write_restart_map_file_no_nans:fast" "test_write_restart_map_file_with_nans:fast"
)
