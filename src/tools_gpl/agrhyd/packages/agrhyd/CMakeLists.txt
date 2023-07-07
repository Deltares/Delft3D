# Define include directory
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(agrhyd_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the AGRHYD path
set(waq_root_path ${agrhyd_root_path}/../../engines_gpl/waq) # Trace back to the WAQ path

# Set version file variables
set(waq_version_path ${waq_root_path}/version)

set(fortran_version_file_delwaq ${waq_root_path}/packages/waq_utils_f/src/waq_utils/delwaq_version.F90)
set(rc_version_file ${waq_version_path}/version_number_agrhyd.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)


# Set source directory
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name agrhyd)
add_executable(${executable_name} ${source_files}
                                  ${rc_version_file}
                                  ${fortran_version_file_delwaq})


set_source_files_properties(${fortran_version_file_delwaq}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")



# Add dependencies
target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies deltares_common
                         deltares_common_c
                         io_netcdf
                         io_hyd
                         waq_utils_f)

    oss_include_libraries(${executable_name} oss_dependencies)

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)

if (WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         io_netcdf
                         io_hyd
                         netcdf4
                         netcdff
                         waq_utils_f)

    oss_include_libraries(${executable_name} oss_dependencies)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

if(UNIX)
    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})

    message(STATUS "Setting target_compile_definitions in Unix")
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
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
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/agrhyd)

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
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../tools_gpl/agrhyd/scripts/run_agrhyd.sh  DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../tools_gpl/agrhyd/default/agrhyd.ini  DESTINATION share/delft3d)
endif(UNIX)

