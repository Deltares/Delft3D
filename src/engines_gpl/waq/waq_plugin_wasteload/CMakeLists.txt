set(library_name waq_plugin_wasteload)

set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/..)
set(rc_version_file ${waq_root_path}/resources/version/version_number_waq_plugin_wasteload.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)


create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir . src resource_files ${rc_version_file} shared)

# Set dependencies
set(oss_dependencies waq_definition
        deltares_common
        deltares_common_c
        waq_data
        waq_utils_f)

target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
target_include_directories(${library_name} PRIVATE ${version_include_dir})

# Define how the files should be structured within Visual Studio
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)

install(TARGETS ${library_name} RUNTIME DESTINATION lib)