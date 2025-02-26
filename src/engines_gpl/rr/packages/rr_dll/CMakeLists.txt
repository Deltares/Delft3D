# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Set version file variables
#Version includes
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rr_version_header_path ${CMAKE_CURRENT_SOURCE_DIR}/../../version)
set(rc_version_file ${rr_version_header_path}/rr_dll_version.rc)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/rr_dll_version.F90)

include_directories(${version_include_dir} ${rr_version_header_path})

# Define library
set(library_name rr_dll)
add_library(${library_name} SHARED  ${source_files}
                                    ${rc_version_file} 
                                    ${fortran_version_file})

set_source_files_properties(
                    src/OESOpenMI.F90
                    src/rr_dll.F90
                    ${fortran_version_file}
                    PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")
                   
target_compile_definitions(${library_name} PRIVATE "X64")

set(oss_dependencies_general control_lib
                            delftio
                            deltares_common
                            deltares_common_c
                            ec_module
                            gridgeom
                            io_netcdf
                            flow1d_core
                            kdtree2
                            kdtree_wrapper
                            rr_kernel_f
                            rr_rtc_tools
                            wl_openmi_support
                            esmfsm)
                             
# Add dependencies
if (UNIX)
    set(oss_dependencies ${oss_dependencies_general} netcdff)

    target_link_libraries(${library_name}
         ${oss_dependencies}
    )
    add_definitions(-DHAVE_CONFIG_H)   
endif(UNIX)
if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                        netcdff)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/rr)

install(TARGETS ${library_name} RUNTIME DESTINATION lib)
