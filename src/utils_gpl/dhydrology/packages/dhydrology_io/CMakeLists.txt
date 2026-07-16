set(src_path src) 

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define the library
set(library_name dhydrology_io)
add_library(${library_name} ${source_files})

# Specify dependencies
set(oss_dependencies deltares_common ec_module)

target_link_libraries(${library_name} ${oss_dependencies})

 # Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/${library_name})
