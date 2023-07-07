# Define include directory
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include) # Note that the CMAKE_CURRENT_SOURCE_DIR represents THIS directory

# Set source directory
set(src_path src)

# Gather source files 
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name waqpb_lib)
add_library(${library_name} ${source_files})

set(oss_dependencies deltares_common
                     waq_utils_f)

oss_include_libraries(${library_name} oss_dependencies)


# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/waqpb)