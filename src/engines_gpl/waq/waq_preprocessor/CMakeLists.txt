set(library_name waq_preprocessor)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir . recursive TRUE)

# Set dependencies
set(oss_dependencies
        waq_io
        waq_proc_preprocess
        waq_memory)

target_link_libraries(${library_name} ${oss_dependencies} ${CMAKE_DL_LIBS})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE ${waq_default_flags})

set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)