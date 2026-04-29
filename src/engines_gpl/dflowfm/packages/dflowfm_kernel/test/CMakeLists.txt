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
f90twtest(${test_target}
    CFILES test_dflowfm_kernel.cpp
    F90FILES
        src/test_dflowfm_kernel_example.f90 # This must be the first Fortran file, as it is referenced in test_dflowfm_kernel.cpp!!
        src/test_3d_layer_administration.f90
        src/test_pol_to_cellmask.f90
        src/test_longculverts.f90
        src/test_compute_horton_infiltration.f90
        src/test_bubblescreen.f90
        src/test_sources_sinks.f90
        src/test_timespace_parameters.f90
        src/test_ec_module_block.f90
        src/test_unstruc_inifields.f90
        src/test_unstruc_model.f90
        src/test_mdu_file_read_write.f90
        src/test_dflowfm_dambreak.f90
        src/test_init_spatial_field.f90
    F2HFILES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_dflowfm_kernel_example.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_3d_layer_administration.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_pol_to_cellmask.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_longculverts.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_compute_horton_infiltration.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_bubblescreen.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_sources_sinks.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_timespace_parameters.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_ec_module_block.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_unstruc_inifields.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_unstruc_model.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_mdu_file_read_write.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_dflowfm_dambreak.f90
        ${CMAKE_CURRENT_SOURCE_DIR}/src/test_init_spatial_field.f90
    OUTDIR "${CMAKE_CURRENT_BINARY_DIR}"
    LIBRARIES dflowfm_kernel test_helpers
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)
add_subdirectory(helpers)

copy_test_resources("${test_target}" "${CMAKE_CURRENT_SOURCE_DIR}/resources/MDUversion" "$<TARGET_FILE_DIR:${test_target}>/MDUversion")
set_target_properties(${test_target} PROPERTIES
    VS_DEBUGGER_WORKING_DIRECTORY "$<TARGET_FILE_DIR:${test_target}>"
)