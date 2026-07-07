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
add_rc_object_library(${executable_name} "${rc_version_file};${icon_file}" "${version_include_dir};${dflowfm_path}/version;${resource_path}")
add_executable(${executable_name} ${source})
target_link_libraries(${executable_name} PRIVATE ${executable_name}_rc)

target_link_libraries(${executable_name} PRIVATE
                      dflowfm_kernel
                      gridgeom
                      deltares_common
                      netCDF::netcdff
)

if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
    set_target_properties(${executable_name} PROPERTIES OUTPUT_NAME dflowfm)
endif(UNIX)


set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS "${openmp_flag}")
target_link_options(${executable_name} PRIVATE "${openmp_flag}")


# Define additional compilation flags
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/net_main.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Include MPI module path for both Windows and Unix
include_directories(${mpi_module_path}
                    ${version_include_dir}
)

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
    
    # Link MPI libraries for Windows
    target_link_directories(${executable_name} PRIVATE "${mpi_library_path}")
    target_link_libraries(${executable_name} PRIVATE "${mpi_fortran_library}")
endif(WIN32)

if(UNIX)
    # Link MPI libraries for Unix using CMake's MPI targets
    target_link_libraries(${executable_name} PRIVATE MPI::MPI_Fortran)
endif(UNIX)

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
install(FILES $<TARGET_RUNTIME_DLLS:${executable_name}> DESTINATION bin)

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
