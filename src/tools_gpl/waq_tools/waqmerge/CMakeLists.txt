set(executable_name waqmerge)

set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path
set(rc_version_file ${waq_root_path}/resources/version/version_number_waqmerge.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(
        ${executable_name} ${CMAKE_CURRENT_SOURCE_DIR}
        src_dir .
        resource_files ${rc_version_file}
        target_type "executable"
)

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${version_include_dir})
if (UNIX)

    set(oss_dependencies deltares_common
                         io_netcdf
                         io_netcdf_data
                         io_hyd
                         waq_hyd_data
                         waq_utils_f)


    target_link_libraries(${executable_name}
         ${oss_dependencies}
         netcdf
         netcdff)
endif(UNIX)

if (WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         io_netcdf
                         io_netcdf_data
                         io_hyd
                         waq_hyd_data
                         netcdf4
                         netcdff
                         waq_utils_f)

    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

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
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    target_link_libraries(${executable_name}
                          "netcdf.lib")
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)


# Define how the files should be structured within Visual Studio
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waqmerge)

target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_waqmerge.${platform_extension}  DESTINATION bin)