set(library_name part_data_f)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir .)

target_link_libraries(${library_name} PRIVATE
    waq_definition
    waq_hyd_data
)

set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/part)

target_compile_options(${library_name} PRIVATE ${openmp_flag})
target_compile_options(${library_name} PRIVATE ${qauto_threaded_flags})
target_compile_options(${library_name} PRIVATE ${waq_default_flags})
