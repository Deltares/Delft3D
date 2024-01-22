set(dependencies delwaq_lib)

create_test(
    test_delwaq_dimr
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/dimr
    test_files test_delwaq_dimr.f90
    )
