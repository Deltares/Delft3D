set(executable_name ddcouple)

set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq)
set(rc_version_file ${waq_root_path}/resources/version/version_number_ddcouple.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(
        ${executable_name}
        ${CMAKE_CURRENT_SOURCE_DIR}
        src_dir .
        resource_files ${rc_version_file}
        target_type "executable"
)

# Add dependencies
target_include_directories(${executable_name} PRIVATE ${version_include_dir})
set(oss_dependencies deltares_common
                     io_netcdf
                     io_hyd
                     waq_hyd_data
                     waq_utils_f)

target_link_libraries(${executable_name} ${oss_dependencies})

# Define how the files should be structured within Visual Studio
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/ddcouple)

target_compile_options(${executable_name} PRIVATE ${waq_default_flags})
if (WIN32)
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)


install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/resources/scripts/run_ddcouple.${platform_extension}  DESTINATION bin)