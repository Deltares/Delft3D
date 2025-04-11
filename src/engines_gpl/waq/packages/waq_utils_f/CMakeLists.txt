# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../..) # Trace back to the WAQ path
set(waq_version_path ${waq_root_path}/version)

set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(fortran_static_version_file ${CMAKE_CURRENT_SOURCE_DIR}/waq_utils/static_version_info.f90)

# Set directory which contains all source files
set(src_path src)

# Gather the source files
set(dlwq_netcdf_path ${src_path}/dlwq_netcdf)
get_fortran_source_files(${dlwq_netcdf_path} dlwq_netcdf_source)

set(waq_utils_path ${src_path}/waq_utils)
get_fortran_source_files(${waq_utils_path} waq_utils_source)

set(library_name waq_utils_f)
add_library(${library_name} ${dlwq_netcdf_source}
                            ${waq_utils_source})

# Set dependencies
set(oss_dependencies_general deltares_common
                             waq_data
                             io_netcdf)

if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    # WARNING: waq_process and waq_util_f are (weakly) circular dependent
    # Just add waq_process to "target_lib_libraries", nothing more
    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${library_name}
         ${oss_dependencies}
         waq_process
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)
if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)
endif(WIN32)

oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

set_source_files_properties(${fortran_static_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_include_directories(${library_name}  PRIVATE ${mpi_module_path} ${mpi_include_path}/
                                                    "${CMAKE_CURRENT_SOURCE_DIR}/../../include"
                                                    ${version_include_dir}
                                                    ${waq_version_path})

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES  ${dlwq_netcdf_source}
                                                     ${waq_utils_source})

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)

