set(library_name flow2d3d_kernel_dd_f)
# Set directory which contains all source files
set(src_path src)

get_module_include_path(${checkout_src_root}/${deltares_common_module} 
                        ${library_name}
                        deltares_common_include_path)
message(STATUS "Configuring '${library_name}' with the following deltares_common include path: '${deltares_common_include_path}'")

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies    deltares_common
                        flow2d3d_data
                        flow2d3d_io) 

target_link_libraries(${library_name} ${oss_dependencies})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)

# Change project flags at the end of this file
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
