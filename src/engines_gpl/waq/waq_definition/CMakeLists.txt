# Define the library
set(library_name waq_definition)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir .)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE ${waq_default_flags})
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)