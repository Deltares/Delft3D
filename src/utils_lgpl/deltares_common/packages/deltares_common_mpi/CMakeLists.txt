include_directories(${mpi_module_path})

set(library_name deltares_common_mpi)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)
# Set dependencies
set(oss_dependencies deltares_common)

# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE HAVE_MPI)

# Set additional compilation properties
set(source_path src)
set_source_files_properties(${source_path}/dfbroadc.f90
                            ${source_path}/dfreduce.f90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_link_libraries(${library_name} ${oss_dependencies})

# Create the folder structure in vfproj
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/deltares_common)
