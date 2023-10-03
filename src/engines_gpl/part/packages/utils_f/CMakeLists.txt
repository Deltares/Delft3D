# Set directory which contains all source files
set(src_path src)

# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(fortran_version_file ${part_root_path}/../waq/packages/waq_utils_f/src/delwaq_version.F90)


# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name part_utils_f)
add_library(${library_name} ${source_files})

# Add dependencies
set(oss_dependencies deltares_common
                     part_data_f
                     io_netcdf
                     io_hyd
                     waq_utils_f)

target_link_libraries(${library_name} ${oss_dependencies})
oss_include_libraries(${library_name} oss_dependencies)

# Define additional compilation flags
set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/part)

target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
