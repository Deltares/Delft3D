# Define include directories
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(part_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the PART path
set(waq_root_path ${part_root_path}/../waq) # Trace back to the WAQ path

# Set version file variables
set(waq_version_path ${waq_root_path}/version)

set(fortran_version_file_delwaq ${waq_root_path}/packages/waq_utils_f/src/delwaq_version.F90)
set(rc_version_file ${waq_version_path}/version_number_delpar.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set icon file
set(icon_file ${waq_version_path}/part_icon.rc)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name delpar)
add_executable(${executable_name} ${source_files}
    ${icon_file}
    ${rc_version_file}
    ${fortran_version_file_delwaq})

set_source_files_properties(${fortran_version_file} ${fortran_version_file_delwaq}
    PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     part_data_f
                     part_utils_f
                     part_kernel_f
                     part_io_f
                     waq_utils_f
                     io_netcdf
                     io_hyd)

target_link_libraries(${executable_name} ${oss_dependencies})
oss_include_libraries(${executable_name} oss_dependencies)

target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${waq_default_flags}")

if(UNIX)
    target_link_options(${executable_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if(WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
        PRIVATE
        "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}
        "pthreadVC2.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES ${icon_file}
    ${rc_version_file})
target_compile_options(${executable_name} PRIVATE ${openmp_flag})
set_target_properties(${executable_name} PROPERTIES FOLDER engines_gpl/part)

if(WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target(${executable_name}
    ${install_dir}
    ${build_dir}
    ${checkout_src_root}
    ${executable_name})

install(TARGETS ${executable_name} RUNTIME DESTINATION bin)

if(UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/part/scripts/run_dpart.sh DESTINATION bin)
endif(UNIX)
