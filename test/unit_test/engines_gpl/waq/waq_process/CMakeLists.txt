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

create_test(
    tests_meteo
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_process
    test_list test_meteo_nearest test_meteo_linear test_meteo_quadratic
    labels "test_meteo_nearest:fast" "test_meteo_linear:fast" "test_meteo_quadratic:fast"
    test_files tests_meteo.f90
    )

create_test(
    tests_rear
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_process
    test_list test_rear_simple test_rear_temperature_seawater test_rear_temperature_fresh_water
    labels "test_rear_simple:fast" "test_rear_temperature_seawater:fast" "test_rear_temperature_fresh_water:fast"
    test_files tests_rear.f90
    )

create_test(
    tests_tmode
    dependencies ${dependencies}
    visual_studio_folder tests/unit_test/engines_gpl/waq/waq_process
    test_list test_tmode
    labels "test_tmode:fast"
    test_files tests_tmode.f90
    )


add_subdirectory(debgrz test_debgrz)
