include_directories(${mpi_module_path})

set(library_name deltares_common_mpi)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE HAVE_MPI)

# Set additional compilation properties
set_source_files_properties(src/dfbroadc.f90
                            src/dfreduce.f90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_link_libraries(${library_name} PRIVATE deltares_common)

# Create the folder structure in vfproj
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/deltares_common)
