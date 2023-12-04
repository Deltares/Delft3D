# Delwaq 2 tests

# test 1: test_delwaq2_stepwise
set(dependencies delwaq_lib)
create_test( 
    test_delwaq2_stepwise
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/delwaq2
    test_files test_delwaq2_stepwise.f90
    )


# test 2: test_delwaq2_two_instances

set(dependencies deltares_common
                 waq_utils_f
                 delwaq_lib
                 waq_hyd_data
                 waq_kernel
                 )
create_test( 
    test_delwaq2_two_instances
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/delwaq2
    test_files test_delwaq2_two_instances.f90
    )

    