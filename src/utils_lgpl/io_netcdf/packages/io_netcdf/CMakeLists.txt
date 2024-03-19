set(library_name io_netcdf)


# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/io_netcdf_version.F90)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rc_dir ${CMAKE_CURRENT_SOURCE_DIR}/version)
set(rc_version_file ${rc_dir}/io_netcdf_version_number.rc)

if(UNIX)
     # the `pkg_check_modules` function is created with this call
     find_package(PkgConfig REQUIRED)

     # these calls create special `PkgConfig::<MODULE>` variables
     pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
endif()


###########
# IO_NETCDF
###########

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src resource_files ${fortran_version_file})

# Create the folder structure in vfproj
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)

target_include_directories(${library_name}  PRIVATE ${version_include_dir} ${rc_dir})

if(UNIX)
    target_link_directories(${library_name}
            INTERFACE
            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")


    # Add dependency
    set(oss_dependencies deltares_common
                         io_netcdf_data
                         fortrangis
                         netcdff)
    target_compile_definitions(${library_name} PRIVATE LINUX64)
    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies}
         PkgConfig::NETCDF)
    include_directories(${CMAKE_BINARY_DIR}/netcdff)
endif(UNIX)

if (WIN32)
    target_link_directories(${library_name}
            INTERFACE
            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    # Add dependency
    set(oss_dependencies deltares_common
                        fortrangis
                        io_netcdf_data
                        netcdf4
                        netcdff)
    target_compile_definitions(${library_name} PRIVATE WIN64)
    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name} ${oss_dependencies}
                                          netcdf)

    target_compile_options(${library_name} PRIVATE "${heap_arrays_one_flag}")
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")


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
     ${configuration_type} STREQUAL "ALL" OR
     ${configuration_type} STREQUAL "TOOLS" )
    set(library_name io_netcdf_dll)
    set(library_renamed libio_netcdf.so)
    # Add dynamic io_netcdf library
    add_library(${library_name} SHARED ${src_path}/io_netcdf_api.F90 ${rc_version_file})
    include_directories(${version_include_dir})

    set(oss_dependencies io_netcdf)
    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies})

    # Set additional compilation properties, specific for Debug mode
    target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
    target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

    # Create the folder structure in vfproj
    source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${src_path}/io_netcdf_api.F90)
    set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)

    # Set post-build step
    set(install_dir ${CMAKE_BINARY_DIR})
    set(build_dir ${CMAKE_BINARY_DIR})

    install(PROGRAMS $<TARGET_FILE:${library_name}> RENAME io_netcdf.${library_suffix} DESTINATION lib)
endif()
