# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define the library
set(library_name waq_data)
add_library(${library_name} ${source_files})


# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
