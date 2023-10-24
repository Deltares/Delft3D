# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files_recursive(${src_path} sources)

# Define library
set(library_name io_netcdf_data)
add_library(${library_name} ${sources})

# Set dependencies
set(oss_dependencies deltares_common)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)

# Set dependencies
set(oss_dependencies deltares_common)

target_link_libraries(${library_name} ${oss_dependencies})
oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")
