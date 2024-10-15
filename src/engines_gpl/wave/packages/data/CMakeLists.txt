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

# Define library
set(library_name wave_data)
add_library(${library_name} ${source_files}
                            ${fortran_version_files})

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
                            netcdf4
                            netcdff)
    target_link_libraries(${library_name} ${oss_dependencies})

    include_directories(${mpi_module_path}
                        ${version_include_dir}
                        ${wave_version_path})

    # Define preprocessor definitions
    target_compile_definitions(${library_name} PRIVATE "WIN32;HAVE_MPI")

endif(WIN32)

if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies    delftio
                            deltares_common
                            ec_module
                            gridgeom
                            io_netcdf
                            io_netcdf_data
                            nefis
                            netcdf4
                            netcdff)

    target_link_libraries(${library_name} ${oss_dependencies})

    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include"
                        ${mpi_module_path}
                        ${version_include_dir}
                        ${wave_version_path})

    # Define preprocessor definitions
    target_compile_definitions(${library_name} PRIVATE "HAVE_MPI")

endif(UNIX)

# Define additional compilation flags
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/wave_mpi.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)
