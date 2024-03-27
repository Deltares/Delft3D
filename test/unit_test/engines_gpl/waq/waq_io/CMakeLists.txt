# test dependencies
set(dependencies delwaq_lib
                 waq_data
                 waq_io
                )

create_test(
    tests_dlwq5a
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_io
    test_files tests_dlwq5a.f90
    include_dir ${CMAKE_CURRENT_SOURCE_DIR}/data/
    )
