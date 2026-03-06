# Define the library
set(library_name flow1d_core)
set(src_path src) 

# Gather source files
get_fortran_source_files(${src_path} source_files)

file(GLOB include_files "${CMAKE_CURRENT_SOURCE_DIR}/../../include/*.inc")

add_library(${library_name} ${source_files} 
                            ${include_files})

set_source_files_properties(
                   src/Laterals.F90
                   src/wetcrs_modules.F90
                   PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Specify dependencies
set(oss_dependencies    deltares_common         gridgeom) 

target_link_libraries(${library_name} ${oss_dependencies})

# Create the structure in Visual Studio
source_group("Include" FILES ${include_files}) 

 # Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/flow1d_library)
