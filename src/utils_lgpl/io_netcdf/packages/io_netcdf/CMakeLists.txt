set(library_name io_netcdf)


# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/io_netcdf_version.F90)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rc_dir ${CMAKE_CURRENT_SOURCE_DIR}/version)
set(rc_version_file ${rc_dir}/io_netcdf_version_number.rc)

###########
# IO_NETCDF
###########

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src resource_files ${fortran_version_file})

# Create the folder structure in vfproj
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)

target_include_directories(${library_name}  PRIVATE ${version_include_dir} ${rc_dir})

target_link_libraries(${library_name} PRIVATE
                        deltares_common
                        io_netcdf_data
                        fortrangis
)
target_link_libraries(${library_name} PUBLIC
                        netCDF::netcdff
)

if(UNIX)
    target_compile_definitions(${library_name} PRIVATE LINUX64)
endif(UNIX)

if (WIN32)
    target_compile_definitions(${library_name} PRIVATE WIN64)
    target_compile_options(${library_name} PRIVATE "${heap_arrays_one_flag}")
endif(WIN32)

set(src_path src)
# Set additional compilation properties
set_source_files_properties(${fortran_version_file}
                            ${src_path}/coordinate_reference_system.F90
                            ${src_path}/io_ugrid.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


###############
# IO_NETCDF_DLL
###############
string(TOUPPER ${CONFIGURATION_TYPE} configuration_type)
if ( ${configuration_type} STREQUAL "DFLOWFM" OR
     ${configuration_type} STREQUAL "DELFT3D4" OR
     ${configuration_type} STREQUAL "FM-SUITE" OR
     ${configuration_type} STREQUAL "D3D4-SUITE" OR
     ${configuration_type} STREQUAL "ALL" OR
     ${configuration_type} STREQUAL "ALL-TESTBENCH" OR
     ${configuration_type} STREQUAL "FM-TESTBENCH" OR
     ${configuration_type} STREQUAL "TOOLS" )
    set(library_name io_netcdf_dll)
    set(library_renamed libio_netcdf.so)
    # Add dynamic io_netcdf library
    add_rc_object_library(${library_name} "${rc_version_file}" "${version_include_dir};${rc_dir}")
    add_library(${library_name} SHARED ${src_path}/../../io_netcdf_api/io_netcdf_api.F90)
    target_link_libraries(${library_name} PRIVATE ${library_name}_rc)
    include_directories(${version_include_dir})

    set(oss_dependencies io_netcdf)
    target_link_libraries(${library_name} PRIVATE ${oss_dependencies})

    # Create the folder structure in vfproj
    source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${src_path}/io_netcdf_api.F90)
    set_target_properties(${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)
    set_target_properties(${library_name} PROPERTIES OUTPUT_NAME io_netcdf)
    if(WIN32)
        # Use a distinct name for the DLL import library so it does not
        # overwrite the static io_netcdf.lib that other targets link against.
        set_target_properties(${library_name} PROPERTIES ARCHIVE_OUTPUT_NAME io_netcdf_dll)
    endif()
    if(UNIX)
        set_target_properties(${library_name} PROPERTIES PREFIX "")
    endif()

    install(TARGETS ${library_name} RUNTIME DESTINATION bin
                                    LIBRARY DESTINATION lib
    )
endif()
