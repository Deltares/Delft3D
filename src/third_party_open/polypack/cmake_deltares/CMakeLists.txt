# Set the directory of where the source code is located.
set(src_directory "${CMAKE_CURRENT_SOURCE_DIR}/../src") 

# Gather source files
get_fortran_source_files(${src_directory} source_files)

# Define library
set(library_name polypack)
add_library(${library_name} ${source_files})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${real_size_64_flag}")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Define how the files should be structured in Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)