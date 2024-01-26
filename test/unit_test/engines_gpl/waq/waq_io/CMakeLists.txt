# test dependencies
set(dependencies delwaq_lib
                 waq_io
                )

create_test(
    tests_dlwq5b
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_io
    test_files tests_dlwq5b.f90
    )
