# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name part_data_f)
add_library(${library_name} ${source_files})


set(oss_dependencies_general deltares_common
                             waq_utils_f
                             io_netcdf
                             io_hyd)
                             
                             
# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
    
endif(UNIX)

if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)
oss_include_libraries(${library_name} oss_dependencies)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/part)

target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
