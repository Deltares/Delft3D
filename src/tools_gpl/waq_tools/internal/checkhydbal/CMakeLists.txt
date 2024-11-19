set(executable_name checkhydbal)

set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_checkhydbal.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(
        ${executable_name} ${CMAKE_CURRENT_SOURCE_DIR}
        src_dir .
        resource_files ${rc_version_file}
        target_type "executable"
)

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${version_include_dir})

if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
endif(UNIX)


set(oss_dependencies    deltares_common
                        io_netcdf
                        io_netcdf_data
                        io_hyd
                        waq_hyd_data
                        waq_utils_f)

target_link_libraries(${executable_name} ${oss_dependencies})


if (WIN32)
    # Set linker properties
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    target_link_libraries(${executable_name}
                          "netcdf.lib")
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)


# Define how the files should be structured within Visual Studio
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/checkhydbal)
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

# Set post-build step
# internal use only - not part of the DIMR set

if(WIN32)
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)
