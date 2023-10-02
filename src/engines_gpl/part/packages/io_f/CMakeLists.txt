# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name part_io_f)
add_library(${library_name} ${source_files})

# Add dependencies
set(oss_dependencies deltares_common
                     nefis
                     part_data_f
                     part_utils_f
                     waq_utils_f
                     io_netcdf
                     io_hyd)


target_link_libraries(${library_name} ${oss_dependencies})
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/part)

target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
