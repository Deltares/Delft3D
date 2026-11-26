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

set_source_files_properties(src/test_dflowfm_version.F90 PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")
add_fortran_gtest(test_dflowfm_kernel_gtest
    TEST_TEARDOWN reset_dflowfm_after_test
    TEST_LAUNCHER ${ncgen_test_launcher}
    SOURCES
        src/dflowfm_test_cleanup.f90
        src/test_dflowfm_kernel_example.f90
        src/test_dflowfm_version.F90
        src/test_interpolate_on_polyline.f90
        src/test_fdster.f90
        src/test_tridag.f90
        src/test_getverticallyaveraged.f90
        src/test_3d_layer_administration.f90
        src/test_sed_transport.f90
        src/test_sed_exchange.f90
        src/test_mass_balance.f90
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

# Run the complete timestep sequence in its own process, with model-owned global state.
add_fortran_gtest(test_bedload_balance
    SOURCES src/test_bedload_balance.f90
    LIBRARIES dflowfm_kernel test_helpers netCDF::netcdff
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)

# Use a separate working directory for the generated model inputs and diagnostics.
add_fortran_gtest(test_bedload_areas
    SOURCES src/test_bedload_areas.f90
    LIBRARIES dflowfm_kernel test_helpers netCDF::netcdff
    VISUAL_STUDIO_PATH engines_gpl/dflowfm/test
)
set_target_properties(test_bedload_areas PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test_bedload_areas"
)

add_executable(test_subsupl_balance src/test_subsupl_balance.f90)
target_link_libraries(test_subsupl_balance PRIVATE dflowfm_kernel test_helpers netCDF::netcdff)
set_target_properties(test_subsupl_balance PROPERTIES
    FOLDER engines_gpl/dflowfm/test
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test_subsupl_balance"
)
if(WIN32)
    target_link_options(test_subsupl_balance PRIVATE /STACK:${DFLOWFM_STACK_SIZE_BYTES})
    add_custom_command(TARGET test_subsupl_balance POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:test_subsupl_balance> $<TARGET_RUNTIME_DLLS:test_subsupl_balance>
        COMMAND_EXPAND_LISTS
    )
endif()
foreach(dimensions IN ITEMS 2 3)
    foreach(morphology IN ITEMS off bedload suspended composition)
        foreach(update_s1 IN ITEMS 0 1)
            foreach(direction IN ITEMS -1 1)
                set(case_name "mass_balance_subsupl.${dimensions}d_${morphology}_s1${update_s1}_${direction}")
                set(case_directory "${CMAKE_CURRENT_BINARY_DIR}/subsupl_cases/${case_name}")
                file(MAKE_DIRECTORY "${case_directory}")
                add_test(NAME ${case_name} COMMAND test_subsupl_balance ${dimensions} ${morphology} ${update_s1} ${direction})
                set_tests_properties(${case_name} PROPERTIES
                    WORKING_DIRECTORY "${case_directory}"
                    ENVIRONMENT "OMP_NUM_THREADS=1"
                    TIMEOUT 30
                )
            endforeach()
        endforeach()
    endforeach()
endforeach()

add_test(NAME mass_balance_subsupl.volume_remap COMMAND test_subsupl_balance volume_remap)
add_test(NAME mass_balance_subsupl.sigma_boundary COMMAND test_subsupl_balance sigma_boundary)

foreach(dimensions IN ITEMS 2 3)
    set(case_name "mass_balance_dredging.${dimensions}d")
    set(case_directory "${CMAKE_CURRENT_BINARY_DIR}/dredging_cases/${case_name}")
    file(MAKE_DIRECTORY "${case_directory}")
    add_test(NAME ${case_name} COMMAND test_subsupl_balance dredging ${dimensions})
    set_tests_properties(${case_name} PROPERTIES
        WORKING_DIRECTORY "${case_directory}"
        ENVIRONMENT "OMP_NUM_THREADS=1"
        TIMEOUT 30
    )
endforeach()

foreach(dimensions IN ITEMS 2 3)
    foreach(composition IN ITEMS deposit repay hold)
        set(case_name "mass_balance_dumping.${dimensions}d.${composition}")
        set(case_directory "${CMAKE_CURRENT_BINARY_DIR}/dredging_cases/${case_name}")
        file(MAKE_DIRECTORY "${case_directory}")
        add_test(NAME ${case_name} COMMAND test_subsupl_balance dumping ${dimensions} ${composition})
        set_tests_properties(${case_name} PROPERTIES
            WORKING_DIRECTORY "${case_directory}"
            ENVIRONMENT "OMP_NUM_THREADS=1"
            TIMEOUT 30
        )
    endforeach()
endforeach()

add_executable(test_1d_bed_geometry src/test_1d_bed_geometry.f90)
target_link_libraries(test_1d_bed_geometry PRIVATE dflowfm_kernel test_helpers netCDF::netcdff)
set_target_properties(test_1d_bed_geometry PROPERTIES
    FOLDER engines_gpl/dflowfm/test
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test_1d_bed_geometry"
)
if(WIN32)
    target_link_options(test_1d_bed_geometry PRIVATE /STACK:${DFLOWFM_STACK_SIZE_BYTES})
    add_custom_command(TARGET test_1d_bed_geometry POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:test_1d_bed_geometry> $<TARGET_RUNTIME_DLLS:test_1d_bed_geometry>
        COMMAND_EXPAND_LISTS
    )
endif()

function(add_1d_bed_test name case_dir)
    add_test(NAME ${name} COMMAND ${ARGN})
    set_tests_properties(${name} PROPERTIES
        WORKING_DIRECTORY "${case_dir}"
        ENVIRONMENT "OMP_NUM_THREADS=1"
        TIMEOUT 30
    )
endfunction()

set(reach_resources "${CMAKE_CURRENT_SOURCE_DIR}/resources/1d_bed_geometry")
foreach(morphology IN ITEMS off fixed moving)
    foreach(update_s1 IN ITEMS 0 1)
        foreach(direction IN ITEMS -1 0 1)
            set(case_name "bed_geometry_1d.${morphology}_s1${update_s1}_${direction}")
            set(case_dir "${CMAKE_CURRENT_BINARY_DIR}/bed_geometry_1d_cases/${case_name}")
            file(COPY "${reach_resources}/" DESTINATION "${case_dir}")
            add_1d_bed_test(${case_name} "${case_dir}" test_1d_bed_geometry ground ${morphology} ${update_s1} ${direction})
        endforeach()
    endforeach()
endforeach()

foreach(restart IN ITEMS complete missing_fixed missing_changed displaced)
    set(seed moving)
    if(restart STREQUAL "missing_fixed")
        set(seed fixed)
    elseif(restart STREQUAL "missing_changed")
        set(seed small_change)
    elseif(restart STREQUAL "displaced")
        set(seed displaced)
    endif()
    set(case_name "bed_geometry_1d.restart_${restart}")
    set(case_dir "${CMAKE_CURRENT_BINARY_DIR}/bed_geometry_1d_cases/${case_name}")
    file(COPY "${reach_resources}/" DESTINATION "${case_dir}")
    add_1d_bed_test(${case_name}_seed "${case_dir}" test_1d_bed_geometry seed ${seed})
    set_tests_properties(${case_name}_seed PROPERTIES FIXTURES_SETUP ${case_name})
    if(restart STREQUAL "missing_changed")
        add_1d_bed_test(${case_name} "${case_dir}" ${CMAKE_COMMAND}
            -Dtest_executable=$<TARGET_FILE:test_1d_bed_geometry>
            -P "${CMAKE_CURRENT_SOURCE_DIR}/check_1d_restart_rejection.cmake"
        )
    else()
        add_1d_bed_test(${case_name} "${case_dir}" test_1d_bed_geometry restart ${restart})
    endif()
    set_tests_properties(${case_name} PROPERTIES FIXTURES_REQUIRED ${case_name})
endforeach()

add_custom_target(test_dflowfm_kernel_gtest_resources
    COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different
        "${CMAKE_CURRENT_SOURCE_DIR}/resources"
        "$<TARGET_FILE_DIR:test_dflowfm_kernel_gtest>"
    VERBATIM
)
set_target_properties(test_dflowfm_kernel_gtest_resources PROPERTIES FOLDER engines_gpl/dflowfm/test)
add_dependencies(test_dflowfm_kernel_gtest test_dflowfm_kernel_gtest_resources)

foreach(runtime_target IN ITEMS test_dflowfm_kernel_gtest test_bedload_balance test_bedload_areas test_subsupl_balance test_1d_bed_geometry)
    if(WIN32)
        # Copy MSVC runtime DLLs to test output directory
        foreach(runtime_lib IN LISTS CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS)
            add_custom_command(TARGET ${runtime_target} PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${runtime_lib}"
                    "$<TARGET_FILE_DIR:${runtime_target}>"
            )
        endforeach()

        # Copy pthreads runtime dependency
        add_custom_command(TARGET ${runtime_target} PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${checkout_src_root}/third_party_open/pthreads/bin/x64/msvcr100.dll"
                "$<TARGET_FILE_DIR:${runtime_target}>"
        )
    endif()

    set_target_properties(${runtime_target} PROPERTIES
        VS_DEBUGGER_WORKING_DIRECTORY "$<TARGET_FILE_DIR:${runtime_target}>"
    )
endforeach()
