if(TARGET GDAL::GDAL)
    set(geotiff_test_source src/test_bilinear_geotiff.f90)
endif()
add_fortran_gtest(test_dflowfm_kernel_gtest
    TEST_TEARDOWN reset_dflowfm_after_test
    SOURCES
        src/dflowfm_test_cleanup.f90
        src/test_dflowfm_kernel_example.f90
        src/test_interpolate_on_polyline.f90
        src/test_fdster.f90
        src/test_tridag.f90
        src/test_getverticallyaveraged.f90
        src/test_3d_layer_administration.f90
        src/test_pol_to_cellmask.f90
        src/test_longculverts.f90
        src/test_compute_horton_infiltration.f90
        src/test_bubblescreen.f90
        src/test_sources_sinks.f90
        src/test_timespace_parameters.f90
        src/test_ec_module_block.f90
        src/test_ec_module_data_value.f90
        src/test_unstruc_inifields.f90
        src/test_unstruc_model.f90
        src/test_restart_coordinates.f90
        src/test_mdu_file_read_write.f90
        src/test_getprof_1d.f90
        src/test_dflowfm_dambreak.f90
        src/test_init_spatial_field.f90
        src/test_bilinear_arcinfo.f90
        ${geotiff_test_source}
        src/test_init_spatial_field_integration.f90
        src/test_offline_wave_external_forcing.f90
        src/test_atmospheric_stability.f90
        src/test_precice_adapter.f90
        src/test_fm_external_forcings_utils.f90
        src/test_cross_section_geometry.f90
        src/test_1d_grid.f90
        src/test_airdensity.f90
        src/test_cross_section.f90
        src/test_iniField_1dField.f90
        src/test_laterals.f90
        src/test_observations.f90
        src/test_observ_Cross_Sections.f90
        src/test_read_property.f90
        src/test_read_statistical_output.f90
        src/test_roughness.f90
        src/test_statistical_output.f90
        src/test_storage_nodes.f90
        src/test_temporal_statistics.f90
    LIBRARIES dflowfm_kernel test_helpers netCDF::netcdff
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)
add_subdirectory(helpers)

add_custom_target(test_dflowfm_kernel_gtest_resources
    COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different
        "${CMAKE_CURRENT_SOURCE_DIR}/resources"
        "$<TARGET_FILE_DIR:test_dflowfm_kernel_gtest>"
    VERBATIM
)
set_target_properties(test_dflowfm_kernel_gtest_resources PROPERTIES FOLDER engines_gpl/dflowfm/test)
add_dependencies(test_dflowfm_kernel_gtest test_dflowfm_kernel_gtest_resources)

if(WIN32)
    # Copy MSVC runtime DLLs to test output directory
    foreach(runtime_lib IN LISTS CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
        add_custom_command(TARGET test_dflowfm_kernel_gtest PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${runtime_lib}"
                "$<TARGET_FILE_DIR:test_dflowfm_kernel_gtest>"
        )
    endforeach()

    # Copy pthreads runtime dependency
    add_custom_command(TARGET test_dflowfm_kernel_gtest PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${checkout_src_root}/third_party_open/pthreads/bin/x64/msvcr100.dll"
            "$<TARGET_FILE_DIR:test_dflowfm_kernel_gtest>"
    )
endif()

set_target_properties(test_dflowfm_kernel_gtest PROPERTIES
    VS_DEBUGGER_WORKING_DIRECTORY "$<TARGET_FILE_DIR:test_dflowfm_kernel_gtest>"
)
