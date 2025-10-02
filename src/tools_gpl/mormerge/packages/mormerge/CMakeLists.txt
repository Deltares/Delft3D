# Set directory which contains all source files
set(src_path src)

# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(mormerge_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the Mormerge path
set(mormerge_version_path ${mormerge_root_path}/version)
set(fortran_version_files ${mormerge_root_path}/packages/mormerge/src/mormerge_version.F90
                          ${mormerge_root_path}/packages/mormerge/src/mormerge_static_version_info.F90)

set(rc_version_file ${mormerge_version_path}/mormerge_version.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)


# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name mormerge)
add_executable(${executable_name}   ${source_files}
                                    ${rc_version_file})


# Define precompilation flags
set_source_files_properties(${fortran_version_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c) 
target_link_libraries(${executable_name} ${oss_dependencies})

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}                                                   
                            "pthreadVC2.lib" 
                            "wsock32.lib"
                            "netapi32.lib" 
                            "comctl32.lib"
                            "user32.lib" 
                            "advapi32.lib" 
                            "comdlg32.lib" 
                            "gdi32.lib" 
                            "winspool.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

if(UNIX)
    target_link_libraries(${executable_name} ${oss_dependencies})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif()
include_directories(${version_include_dir}
                    ${mormerge_version_path})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES ${rc_version_file})

set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/mormerge)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})


install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

install(PROGRAMS ${CMAKE_SOURCE_DIR}/../tools_gpl/mormerge/scripts/mormerge.tcl     DESTINATION bin)
install(PROGRAMS ${CMAKE_SOURCE_DIR}/../tools_gpl/mormerge/scripts/run_mormerge.${platform_extension}  DESTINATION bin)
if (WIN32)
install(PROGRAMS ${CMAKE_SOURCE_DIR}/../third_party_open/tcl/bin/win64/tclkitsh852.exe DESTINATION bin)
endif(WIN32)
