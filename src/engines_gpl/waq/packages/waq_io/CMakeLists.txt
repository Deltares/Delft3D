# Set path for include files 
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_io_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_io_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/ods ods_files)
get_fortran_source_files(${src_path}/waq_io waq_io_files)

# Define the library
set(library_name waq_io)
add_library(${library_name} ${ods_files}
                            ${waq_io_files})

# Set dependencies
set(oss_dependencies_general deltares_common
                             part_data_f
                             part_io_f
                             io_hyd
                             io_netcdf
                             waq_data
                             waq_kernel
                             waq_plugin_wasteload
                             waq_utils_c
                             waq_utils_f) 


if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)
if(WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)

oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${ods_files}
                                                    ${waq_io_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
