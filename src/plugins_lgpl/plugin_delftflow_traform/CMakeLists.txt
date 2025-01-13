# Shared definitions
# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} library_files)

# Define library
set(library_name plugin_delftflow_traform)
add_library(${library_name} SHARED  ${library_files})

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_files})
set_target_properties (${library_name} PROPERTIES FOLDER plugins_lgpl/plugin_delftflow_traform)

install(TARGETS ${library_name} RUNTIME DESTINATION lib)
