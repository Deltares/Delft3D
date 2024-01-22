# Delwaq_lib tests

# test 1: test_waq_model_run
set(dependencies delwaq_lib)
create_test( 
    test_waq_model_run
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/delwaq_lib
    test_files test_waq_model_run.f90
    )


# test 2: test_waq_omi_lib
create_test( 
    test_waq_omi_lib
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/delwaq_lib
    test_files test_waq_omi_lib.f90
    )


# test 2: test_waqlib_multi
create_test( 
    test_waqlib_multi
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/delwaq_lib
    test_files test_waqlib_multi.f90
    )
