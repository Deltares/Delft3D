function(copy_test_resources NAME SOURCE_FOLDER TARGET_FOLDER)
    file(GLOB_RECURSE test_resources RELATIVE "${SOURCE_FOLDER}" "${SOURCE_FOLDER}/*")

    foreach(resource_file IN LISTS test_resources)
        add_custom_command(
            TARGET ${NAME} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${SOURCE_FOLDER}/${resource_file}"
                "${TARGET_FOLDER}/${resource_file}"
        )
    endforeach()
endfunction()

set(test_target test_dflowfm_kernel_gtest)
add_fortran_gtest(${test_target}
    TEST_TEARDOWN reset_dflowfm_after_test
    SOURCES
        src/dflowfm_test_cleanup.f90
        src/test_dflowfm_kernel_example.f90
        src/test_tridag.f90
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
        src/test_mdu_file_read_write.f90
        src/test_getprof_1d.f90
        src/test_dflowfm_dambreak.f90
        src/test_init_spatial_field.f90
        src/test_init_spatial_field_integration.f90
        src/test_offline_wave_external_forcing.f90
        src/test_atmospheric_stability.f90
        src/test_precice_adapter.f90
        src/test_fm_external_forcings_utils.f90
    LIBRARIES dflowfm_kernel test_helpers netCDF::netcdff
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)
add_subdirectory(helpers)

copy_test_resources("${test_target}" "${CMAKE_CURRENT_SOURCE_DIR}/resources/MDUversion" "$<TARGET_FILE_DIR:${test_target}>/MDUversion")

# Copy MSVC runtime DLLs to test output directory
foreach(runtime_lib IN LISTS CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
    add_custom_command(TARGET ${test_target} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${runtime_lib}"
            "$<TARGET_FILE_DIR:${test_target}>"
    )
endforeach()

# Copy pthreads runtime dependency
add_custom_command(TARGET ${test_target} PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${checkout_src_root}/third_party_open/pthreads/bin/x64/msvcr100.dll"
        "$<TARGET_FILE_DIR:${test_target}>"
)

set_target_properties(${test_target} PROPERTIES
    VS_DEBUGGER_WORKING_DIRECTORY "$<TARGET_FILE_DIR:${test_target}>"
)