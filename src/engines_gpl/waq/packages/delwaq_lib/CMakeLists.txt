# Define include directories
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the WAQ path
include_directories(${waq_root_path}/include)

# Set version file variables
set(waq_version_path ${waq_root_path}/version)

set(rc_version_file ${waq_version_path}/version_number.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)
file(GLOB preprocessor_files ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/waq_bmi.F90)

# Define library
set(library_name delwaq_lib)
add_library(${library_name} SHARED ${source_files}
                                   ${rc_version_file})

set(oss_dependencies_general deltares_common
                             deltares_common_c
                             waq_delftio
                             io_netcdf
                             nefis
                             waq_data
                             waq_io
                             waq_kernel
                             waq_plugin_wasteload
                             waq_process
                             waq_utils_c
                             waq_utils_f
                             part_utils_f)


if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                         netcdf4
                         netcdff)

    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_include_directories(${library_name} PRIVATE ${mpi_module_path} ${mpi_include_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")


    target_link_libraries(${library_name}
                            "libiomp5md.lib"
                            "${mpi_fortran_library}"
                            "WbeMuuid.lib"
                            "ole32.lib"
                            "oleaut32.lib"
                            "wsock32.lib"
                            "netapi32.lib"
                            "kernel32.lib"
                            "user32.lib"
                            "Shlwapi.lib"
                            "Gdi32.lib"
                            "Winspool.lib"
                            "netcdf.lib"
                            "pthreadVC2.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Set dependencies
if (UNIX)
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
         PkgConfig::NETCDF_FTN
         uuid)
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES    ${rc_version_file})

target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

target_include_directories(${library_name} PRIVATE ${version_include_dir})
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)

if(UNIX)
    target_link_options(${library_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${library_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)


# Change the name of the target library to delwaq.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME delwaq)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${library_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${library_name})

install(TARGETS ${library_name} DESTINATION lib)
if (UNIX)
    install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/default/bloom.spe          DESTINATION share/delft3d)
    install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/default/proc_def.dat       DESTINATION share/delft3d)
    install(FILES ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/default/proc_def.def       DESTINATION share/delft3d)
    install(DIRECTORY  ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/default/csvFiles      DESTINATION share/delft3d)
    install(DIRECTORY  ${CMAKE_SOURCE_DIR}/../engines_gpl/waq/default/subFiles      DESTINATION share/delft3d)
endif(UNIX)


