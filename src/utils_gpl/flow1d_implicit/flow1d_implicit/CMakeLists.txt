set(src_path src) 

# Gather source files
get_fortran_source_files(${src_path} source_files)

file(GLOB include_files "${CMAKE_CURRENT_SOURCE_DIR}/include/*.i")

# Define the library
set(library_name flow1d_implicit)
add_library(${library_name} ${source_files} 
                            ${include_files})

# Specify dependencies
set(oss_dependencies    deltares_common) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Create the structure in Visual Studio
source_group("Include" FILES ${include_files}) 

 # Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/flow1d_implicit)
