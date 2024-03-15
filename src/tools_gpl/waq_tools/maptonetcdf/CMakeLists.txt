set(executable_name maptonetcdf)

# Set version file variables
set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_maptonetcdf.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(${executable_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir . resource_files ${rc_version_file} target_type "executable")

set(oss_dependencies_general waq_definition waq_kernel)

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    oss_include_libraries(${executable_name} oss_dependencies)

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN
         uuid)
endif(UNIX)

if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)

    oss_include_libraries(${executable_name} oss_dependencies)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

if(UNIX)
    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
endif(UNIX)

if (WIN32)
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
                          
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)

# Define how the files should be structured within Visual Studio
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/maptonetcdf)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_maptonetcdf.sh  DESTINATION bin)
endif(UNIX)
if (WIN32)
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_maptonetcdf.bat  DESTINATION bin)
endif(WIN32)
