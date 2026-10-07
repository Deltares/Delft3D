# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name wave_kernel)
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies    wave_data
                        deltares_common
                        wave_io
                        SWAN_MPI::SWAN_MPI)
target_link_libraries(${library_name} ${oss_dependencies})

if(UNIX)
    target_link_libraries(${library_name} MPI::MPI_Fortran)
endif()

include_directories(${mpi_module_path})


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)
