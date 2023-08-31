# Set path for include files
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_kernel_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_kernel_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/kernel kernel_files)
get_fortran_source_files(${src_path}/kernel/integration integration_files)
get_fortran_source_files(${src_path}/kernel/calculation calculation_files)
get_fortran_source_files(${src_path}/kernel/reactions reactions_files)
get_fortran_source_files(${src_path}/results result_files)
get_fortran_source_files(${src_path}/read_wrk read_wrk_files)

# Define library
set(library_name waq_kernel)
add_library(${library_name} ${kernel_files}
                            ${integration_files}
                            ${calculation_files}
                            ${reactions_files}
                            ${result_files}
                            ${read_wrk_files}
                            )


# Set dependencies
set(oss_dependencies_general deltares_common
                             part_data_f
                             part_io_f
                             part_kernel_f
                             part_utils_f
                             waq_data
                             waq_memory
                             waq_plugin_wasteload
                             waq_process
                             waq_utils_c
                             waq_utils_f
                             io_netcdf
                             io_hyd)

if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)

if(WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)

    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)


# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${kernel_files}
                                                    ${integration_files}
                                                    ${calculation_files}
                                                    ${reactions_files}
                                                    ${read_wrk_files}
                                                    ${result_files}
                                                    ${preproccessor_files}
                                                    ${openmq_files}
                                                    ${proces_file})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
