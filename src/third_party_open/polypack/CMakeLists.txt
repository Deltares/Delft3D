set(library_name polypack)

# Set the directory of where the source code is located.
create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${real_size_64_flag}")
target_link_libraries(${library_name} PRIVATE no_compiler_warnings)

set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)
