# Set version file variables
set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_agrhyd.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set source directory
set(src_path .)

# Gather source files
get_fortran_source_files(${src_path} source)

# Define executable
set(executable_name agrhyd)
add_executable(${executable_name} ${source}
                                  ${rc_version_file})


set_source_files_properties(${fortran_version_file_delwaq}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})
set(oss_dependencies deltares_common
                     deltares_common_c
                     io_netcdf
                     io_netcdf_data
                     io_hyd
                     waq_hyd_data
                     waq_utils_f)

oss_include_libraries(${executable_name} oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)

if (WIN32)
   target_compile_definitions(${executable_name} PRIVATE WIN64)
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/agrhyd)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${executable_name})

if (UNIX)
    install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_agrhyd.sh  DESTINATION bin)
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/default/agrhyd.ini  DESTINATION share/delft3d)
endif(UNIX)
