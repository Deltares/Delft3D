# Define executable
set(executable_name rtc)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Set default files to be included in distribution
set(default_files ${CMAKE_CURRENT_SOURCE_DIR}/../../default/RTC.DAT
                  ${CMAKE_CURRENT_SOURCE_DIR}/../../default/RTC.FNM
                  ${CMAKE_CURRENT_SOURCE_DIR}/../../default/RTC.LNG
                  ${CMAKE_CURRENT_SOURCE_DIR}/../../default/RTC.RTN)

set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rtc_version_header_path ${CMAKE_CURRENT_SOURCE_DIR}/../../version)
set(rc_version_file ${rtc_version_header_path}/rtc_version.rc)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/rtc_version.F90)

include_directories(${version_include_dir} ${rtc_version_header_path})

add_executable(${executable_name} ${source_files}
                                ${rc_version_file} 
                                ${fortran_version_file})

set_source_files_properties(
                    src/rtc_main.F90
                    src/rtcmodule.F90
                    ${fortran_version_file}
                    PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")
                   
target_compile_definitions(${executable_name} PRIVATE "X64")

set(oss_dependencies_general control_lib
                            delftio
                            delftio_shm
                            deltares_common
                            deltares_common_c
                            io_netcdf
                            rtc_plugin_c
                            rtc_kernel
                            rr_rtc_tools
                            wl_openmi_support
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
    add_definitions(-DHAVE_CONFIG_H)
endif(UNIX)

if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                        netcdf4
                        netcdff)

    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/rtc)

install(TARGETS ${executable_name} RUNTIME DESTINATION bin)
install(FILES ${default_files} DESTINATION share/rtc)