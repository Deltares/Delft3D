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
add_rc_object_library(${executable_name} "${rc_version_file}" "${version_include_dir};${rr_version_header_path}")
add_executable(${executable_name} ${source_files}
                                ${fortran_version_file})
target_link_libraries(${executable_name} PRIVATE ${executable_name}_rc)

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
                            triangle::triangle
                            wl_openmi_support
                            netCDF::netcdff
                            esmfsm
                            )


# Add dependencies
if (UNIX)
    set(oss_dependencies ${oss_dependencies_general})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                        netCDF::netcdff)
endif(WIN32)

target_link_libraries(${executable_name} PRIVATE ${oss_dependencies})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/rr)

install(TARGETS ${executable_name} RUNTIME DESTINATION bin)
