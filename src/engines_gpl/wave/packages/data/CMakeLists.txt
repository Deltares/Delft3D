# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(wave_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the D-Waves root

# Set version file variables
set(wave_version_path ${wave_root_path}/version)

set(fortran_version_files ${wave_root_path}/packages/data/src/wave_version.F90
                          ${wave_root_path}/packages/data/src/dwaves_static_version_info.F90)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)
set(cxx_source_files ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/inja_templates.cpp)

# Define libraries
add_library(wave_data ${source_files}
                            ${fortran_version_files})
add_library(wave_data_cpp OBJECT ${cxx_source_files})

target_include_directories(wave_data_cpp PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(wave_data_cpp pantor::inja)

target_include_directories(wave_data PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(wave_data wave_data_cpp pantor::inja)

set_source_files_properties(${fortran_version_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
if (WIN32)
    set(oss_dependencies    delftio
                            deltares_common
                            ec_module
                            gridgeom
                            io_netcdf
                            io_netcdf_data
                            nefis
                            netCDF::netcdff)
    target_link_libraries(wave_data ${oss_dependencies})

    include_directories(${mpi_module_path}
                        ${version_include_dir}
                        ${wave_version_path})

    # Define preprocessor definitions
    target_compile_definitions(wave_data PRIVATE "WIN32;HAVE_MPI")

endif(WIN32)

if (UNIX)
    set(oss_dependencies    delftio
                            deltares_common
                            ec_module
                            gridgeom
                            io_netcdf
                            io_netcdf_data
                            nefis
                            netCDF::netcdff
                            stdc++
    )

    target_link_libraries(wave_data ${oss_dependencies})

    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include"
                        ${mpi_module_path}
                        ${version_include_dir}
                        ${wave_version_path})

    # Define preprocessor definitions
    target_compile_definitions(wave_data PRIVATE "HAVE_MPI")

endif(UNIX)

# Define additional compilation flags
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/wave_mpi.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files} ${cxx_source_files})
set_target_properties (wave_data PROPERTIES FOLDER engines_gpl/wave)
set_target_properties (wave_data_cpp PROPERTIES FOLDER engines_gpl/wave)
