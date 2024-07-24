set(library_name part_utils_f)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir .)

# Add dependencies
set(oss_dependencies deltares_common
        part_data_f
        waq_utils_f)

target_link_libraries(${library_name} ${oss_dependencies})

set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/part)

target_compile_options(${library_name} PRIVATE ${openmp_flag})
target_compile_options(${library_name} PRIVATE ${qauto_threaded_flags})
target_compile_options(${library_name} PRIVATE ${waq_default_flags})
