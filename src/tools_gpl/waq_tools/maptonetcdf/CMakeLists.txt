set(executable_name maptonetcdf)

# Set version file variables
set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_maptonetcdf.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(${executable_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir . resource_files ${rc_version_file} target_type "executable")

set(oss_dependencies waq_definition
                     waq_kernel
                     netcdf-fortran::netcdf-fortran)

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})

if (UNIX)
    target_link_libraries(${executable_name}
         ${oss_dependencies}
         uuid)
endif(UNIX)

if (WIN32)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE ${waq_default_flags})

if (WIN32)
   target_compile_definitions(${executable_name} PRIVATE WIN64)
   configure_visual_studio_user_file(${executable_name})
endif(WIN32)

# Define how the files should be structured within Visual Studio
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/maptonetcdf)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_maptonetcdf.${platform_extension}  DESTINATION bin)
