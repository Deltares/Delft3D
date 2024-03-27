set(library_name waq_utils_f)

set(waq_root_path ${CMAKE_SOURCE_DIR}/../engines_gpl/waq) # Trace back to the WAQ path

set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(waq_version_path ${waq_root_path}/resources/version)
set(fortran_static_version_file ${waq_root_path}/waq_utils/static_version_info.f90)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir .)

# Set dependencies
set(oss_dependencies deltares_common
        waq_definition
)


target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE ${qauto_threaded_flags})
target_compile_options(${library_name} PRIVATE ${waq_default_flags})

set_source_files_properties(${fortran_static_version_file} PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_include_directories(${library_name} PRIVATE ${version_include_dir}
        ${waq_version_path})

set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)
