# Set version file variables
set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_waqmerge.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set directory which contains all source files
set(src_path .)

# Gather source files
get_fortran_source_files(${src_path} source)

# Define executable
set(executable_name waqmerge)
add_executable(${executable_name} ${source}
                                  ${rc_version_file})

set(oss_dependencies deltares_common
                     io_netcdf
                     io_netcdf_data
                     io_hyd
                     waq_hyd_data
                     waq_utils_f)

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${version_include_dir})

if (UNIX)
    target_link_libraries(${executable_name}
         ${oss_dependencies}
         netcdf
         netcdff)

    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})

    message(STATUS "Setting target_compile_definitions in Unix")
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)

endif(UNIX)

if (WIN32)
    target_link_libraries(${executable_name} ${oss_dependencies})
    target_compile_definitions(${executable_name} PRIVATE WIN64)
endif(WIN32)

target_link_libraries(${executable_name} netcdf)

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    target_link_libraries(${executable_name}
                          "netcdf.lib")
endif(WIN32)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waqmerge)

target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_waqmerge.sh  DESTINATION bin)
endif(UNIX)
