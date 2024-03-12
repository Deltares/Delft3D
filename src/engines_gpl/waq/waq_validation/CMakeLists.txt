set(library_name waq_validation)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir .)


# Set dependencies
set(oss_dependencies waq_data
        waq_utils_f
        waq_hyd_data)

target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)
