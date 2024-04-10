set(library_name wq_processes)

# Set version file variables
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../)
set(rc_version_file ${waq_root_path}/resources/version/version_number.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir . resource_files ${rc_version_file})

# Set dependencies
set(oss_dependencies waq_definition
        deltares_common
        deltares_common_c
        waq_process
        waq_hyd_data
        waq_utils_c
        waq_utils_f)

# Link libraries
target_link_libraries(${library_name} ${oss_dependencies})

oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE ${waq_default_flags})
target_include_directories(${library_name} PRIVATE ${version_include_dir})

# Define how the files should be structured in Visual studio
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)