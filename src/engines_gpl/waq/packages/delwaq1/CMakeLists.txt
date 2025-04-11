# Define include directories
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the WAQ path

# Set version file variables
set(waq_version_path ${waq_root_path}/version)
set(waq_data_path ${waq_root_path}/include)

set(fortran_version_file_delwaq ${waq_root_path}/packages/waq_utils_f/src/waq_utils/delwaq_version.F90)
set(rc_version_file ${waq_version_path}/version_number_delwaq1.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set icon file
set(icon_file ${waq_version_path}/waq_icon.rc)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name delwaq1)
add_executable(${executable_name} ${source_files}
                                  ${icon_file}
                                  ${rc_version_file}
                                  ${fortran_version_file_delwaq})

set_source_files_properties(${fortran_version_file} ${fortran_version_file_delwaq}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add dependencies
set(oss_dependencies_general deltares_common
                             deltares_common_c
                             waq_delftio
                             io_netcdf
                             nefis
                             delwaq_lib
                             waq_io
                             waq_data
                             waq_kernel
                             waq_plugin_wasteload
                             waq_process
                             waq_utils_c
                             waq_utils_f)


if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)

if(WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)

    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

oss_include_libraries(${executable_name} oss_dependencies)


# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

if(UNIX)
    target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})

    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})

    target_link_options(${executable_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
    target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir} ${mpi_module_path} ${mpi_include_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")


    target_link_libraries(${executable_name}
                            "comctl32.lib"
                            "wsock32.lib"
                            "netapi32.lib"
                            "kernel32.lib"
                            "user32.lib"
                            "Shlwapi.lib"
                            "Gdi32.lib"
                            "Winspool.lib"
                            "netcdf.lib"
                            "${mpi_fortran_library}"
                            "pthreadVC2.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES    ${icon_file}
                                ${rc_version_file})
target_compile_options(${executable_name} PRIVATE ${openmp_flag})
set_target_properties(${executable_name} PROPERTIES FOLDER engines_gpl/waq)
if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/scripts/run_delwaq.sh  DESTINATION bin)
endif(UNIX)

