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
if(TARGET GDAL::GDAL)
    set(geotiff_test_source src/test_bilinear_geotiff.f90)
endif()

function(conan_test_directory output variable_prefix)
    set(directory "${${variable_prefix}_RELEASE}")
    foreach(configuration IN ITEMS Debug RelWithDebInfo MinSizeRel)
        string(TOUPPER "${configuration}" configuration_upper)
        if(${variable_prefix}_${configuration_upper})
            if(NOT directory)
                set(directory "${${variable_prefix}_${configuration_upper}}")
            endif()
            set(directory "$<IF:$<CONFIG:${configuration}>,${${variable_prefix}_${configuration_upper}},${directory}>")
        endif()
    endforeach()
    if(NOT directory)
        message(FATAL_ERROR "Missing Conan directory for ${variable_prefix}; rerun the Conan install.")
    endif()
    set(${output} "${directory}" PARENT_SCOPE)
endfunction()

conan_test_directory(netcdf_bin netcdf_BIN_DIRS)
set(ncgen_test_launcher "${CMAKE_COMMAND}" -E env --modify "PATH=path_list_prepend:${netcdf_bin}")
if(WIN32)
    set(runtime_variable PATH)
    set(runtime_directory BIN)
else()
    set(runtime_variable LD_LIBRARY_PATH)
    set(runtime_directory LIB)
    conan_test_directory(netcdf_lib netcdf_LIB_DIRS)
    list(APPEND ncgen_test_launcher --modify "${runtime_variable}=path_list_prepend:${netcdf_lib}")
endif()
foreach(package IN ITEMS hdf5 zlib)
    conan_test_directory(dependency_dir "${package}_${runtime_directory}_DIRS")
    list(APPEND ncgen_test_launcher --modify "${runtime_variable}=path_list_prepend:${dependency_dir}")
endforeach()
list(APPEND ncgen_test_launcher --)

add_fortran_gtest(${test_target}
    TEST_TEARDOWN reset_dflowfm_after_test
    TEST_LAUNCHER ${ncgen_test_launcher}
    SOURCES
        src/dflowfm_test_cleanup.f90
        src/test_dflowfm_kernel_example.f90
        src/test_interpolate_on_polyline.f90
        src/test_fdster.f90
        src/test_tridag.f90
        src/test_getverticallyaveraged.f90
        src/test_3d_layer_administration.f90
        src/test_absgen_3d.f90
        src/test_pol_to_cellmask.f90
        src/test_longculverts.f90
        src/test_compute_horton_infiltration.f90
        src/test_bubblescreen.f90
        src/test_ncdump_to_netcdf.f90
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
