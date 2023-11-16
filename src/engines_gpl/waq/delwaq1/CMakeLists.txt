# Set directory which contains all source files
set(src_path .)

# Set version file variables
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../) # Trace back to the WAQ path

# Set version file variables
set(waq_version_path ${waq_root_path}/resources/version)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rc_version_file ${waq_version_path}/version_number_delwaq1.rc)

# Gather source files
get_fortran_source_files(${src_path} source)

# Set icon file
set(icon_file ${waq_version_path}/waq_icon.rc)

# Define executable
set(executable_name delwaq1)
add_executable(${executable_name} ${source}
                                  ${icon_file}
                                  ${rc_version_file})

# Add dependencies
set(oss_dependencies delwaq_lib)

target_link_libraries(${executable_name} ${oss_dependencies})

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
    target_include_directories(${executable_name} PRIVATE ${waq_data_path} ${version_include_dir} ${mpi_include_path})

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
    install(PROGRAMS ${CMAKE_CURRENT_LIST_DIR}/../resources/scripts/run_delwaq.sh  DESTINATION bin)
endif(UNIX)
