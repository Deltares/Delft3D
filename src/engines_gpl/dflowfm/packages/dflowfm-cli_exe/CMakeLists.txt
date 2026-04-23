# Set the DFlow FM component path
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../../")
set(dflowfm_cli_path "${CMAKE_CURRENT_SOURCE_DIR}/src/")

set(resource_path ${dflowfm_path}/res)

set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

set(source ${dflowfm_cli_path}/net_main.F90)

set(icon_file ${resource_path}/dflowfm_icon.rc)
set(rc_version_file ${dflowfm_path}/version/dflowfm_cli_version.rc)

# Define the executable
set(executable_name dflowfm-cli)
add_executable(${executable_name} ${source})

target_link_libraries(${executable_name} PRIVATE
                      dflowfm_kernel
                      gridgeom
                      deltares_common
                      netcdff
)

if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
    set_target_properties(${executable_name} PROPERTIES OUTPUT_NAME dflowfm)
endif(UNIX)


if(WIN32)
    set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
endif(WIN32)


# Define additional compilation flags
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/net_main.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

if (WIN32)
    target_link_libraries(${executable_name} PRIVATE netcdf)
    include_directories(${mpi_module_path}
                        ${version_include_dir}
    )

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
    )

    target_link_libraries(${executable_name} PRIVATE
                          "pthreadVC2.lib"
                          "${mpi_fortran_library}"
    )

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})

    target_sources(${executable_name} PRIVATE
                   ${rc_version_file}
                   ${icon_file}
    )
endif(WIN32)

target_link_directories(${executable_name} PRIVATE
                        "${mpi_library_path}")

target_link_libraries(${executable_name} PRIVATE "${mpi_fortran_library}")

# Create the folder structure in Visual Studio
source_group("Resources" FILES
             ${rc_version_file}
             ${icon_file}
)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/dflowfm)
if (WIN32)
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)

install(TARGETS ${executable_name} RUNTIME DESTINATION bin)

if(WIN32)
   if(WITH_INTERACTER)
      add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../../interacter install_interacter)
   endif()
endif()
if (UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/parallel/generate_parallel_mdu.sh  DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/lnx64/submit_dflowfm_h7.sh    DESTINATION bin)
endif(UNIX)

install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/runscripts/run_dflowfm.${platform_extension} DESTINATION bin)
