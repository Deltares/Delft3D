# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
endif(UNIX)

# Define library
set(library_name io_hyd)
add_library(${library_name} ${source_files})

# Set dependencies
if(WIN32)
    set(oss_dependencies    deltares_common
                            io_netcdf
                            netcdf4
                            netcdff)
endif(WIN32)

if(UNIX)
    # Add dependency
    set(oss_dependencies deltares_common
                         io_netcdf)
    target_compile_definitions(${library_name} PRIVATE LINUX64)

    target_link_libraries(${library_name} ${oss_dependencies} PkgConfig::NETCDF PkgConfig::NETCDF_FTN)
endif(UNIX)

oss_include_libraries(${library_name} oss_dependencies)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_hyd)
