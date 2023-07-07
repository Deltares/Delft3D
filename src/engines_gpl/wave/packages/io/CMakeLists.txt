# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name wave_io)
add_library(${library_name} ${source_files})

# Set dependencies
if (WIN32)
    set(oss_dependencies    wave_data
                            deltares_common
                            netcdf4
                            netcdff) 
    oss_include_libraries(${library_name} oss_dependencies)
endif(WIN32)

if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies    wave_data
                            deltares_common)

    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)

    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include")
endif(UNIX)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)
