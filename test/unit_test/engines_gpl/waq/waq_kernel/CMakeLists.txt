# test dependencies
set(dependencies delwaq_lib
                 waq_kernel
                )

create_test(
    tests_dlwq13
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_kernel
    test_files tests_dlwq13.f90
    include_dir ${CMAKE_CURRENT_SOURCE_DIR}/data/
    )
