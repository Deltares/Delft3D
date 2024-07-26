set(src_path src) 

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define the library
set(library_name control_lib)
add_library(${library_name} ${source_files})

set_source_files_properties(
                   src/CONTROL.F90
                   PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_compile_definitions(${library_name} PRIVATE "X64")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

 # Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/${library_name})
