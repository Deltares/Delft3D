# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Set version file variables
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/rrmain_version.F90)

set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rr_version_header_path ${CMAKE_CURRENT_SOURCE_DIR}/../../version)
set(rc_version_file ${rr_version_header_path}/rr_version.rc)

include_directories(${version_include_dir} ${rr_version_header_path})

# Define executable
set(executable_name rr)
add_executable(${executable_name} ${source_files}
                                ${rc_version_file} 
                                ${fortran_version_file})

set_source_files_properties(
                    src/rrmain.F90
                    ${fortran_version_file}
                    PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")
                   
target_compile_definitions(${executable_name} PRIVATE "X64")

set(oss_dependencies_general 
                            control_lib
                            delftio
                            deltares_common
                            deltares_common_c
                            ec_module
                            gridgeom
                            io_netcdf
                            kdtree_wrapper
                            kdtree2
                            rr_dll
                            rr_kernel_c
                            rr_kernel_f
                            rr_rtc_tools
                            triangle_c
                            wl_openmi_support
                            netcdff
                            esmfsm
                            )
                             
                             
# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
         
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                        netcdf4
                        netcdff)
endif(WIN32)

target_link_libraries(${executable_name} ${oss_dependencies})


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/rr)

install(TARGETS ${executable_name} RUNTIME DESTINATION bin)

