set(dependencies delwaq_lib)

create_test(
    test_delwaq_dimr
    dependencies ${dependencies}
    visual_studio_folder tests/integration_test/engines_gpl/waq/waq_bmi
    labels "test_delwaq_dimr:integration_test"
    test_files test_waq_bmi.f90
    )
