# test dependencies
set(dependencies delwaq_lib
                 waq_process
                )

create_test(
    tests_protist_mortality_salinity
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_process
    test_list test_protist_mortality_salinity_influence_salinity test_protist_mortality_salinity_influence_sigmoid_gradient test_protist_mortality_salinity_influence_sigmoid_shift test_protist_mortality_salinity_influence_rate_low_salinity test_protist_mortality_salinity_influence_rate_high_salinity
    labels "test_protist_mortality_salinity_influence_salinity:fast" "test_protist_mortality_salinity_influence_sigmoid_gradient:fast" "test_protist_mortality_salinity_influence_sigmoid_shift:fast" "test_protist_mortality_salinity_influence_rate_low_salinity:fast" "test_protist_mortality_salinity_influence_rate_high_salinity:fast" 
    test_files tests_protist_mortality_salinity.f90
    )