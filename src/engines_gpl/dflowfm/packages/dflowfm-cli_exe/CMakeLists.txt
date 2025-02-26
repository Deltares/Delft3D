# Set directories
# Set the directory of where the source code is located.
set(src_path src)

# Set the DFlow FM component path
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../../")
set(dflowfm_cli_path "${CMAKE_CURRENT_SOURCE_DIR}/src/")

set(resource_path ${dflowfm_path}/res)

set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Interacter
if(WITH_INTERACTER)
    message(CHECK_START "Looking for Interacter library (intmwi64.lib)")
    find_library (lib_interacter intmwi64.lib
              HINTS ENV INTERACTER_DIR
                  PATHS "${checkout_src_root}/third_party/interacter/bin/win32/x64")

    if(NOT lib_interacter)
       message(CHECK_FAIL "not found")
       message(SEND_ERROR "Required Interacter library is missing.")
    else()
       message(CHECK_PASS "found")
    endif()
endif()

set(source ${dflowfm_cli_path}/net_main.F90)

set(icon_file ${resource_path}/dflowfm_icon.rc)
set(rc_version_file ${dflowfm_path}/version/dflowfm_cli_version.rc)

# Define the executable
set(executable_name dflowfm-cli)
add_executable(${executable_name} ${source})

# Add dependencies
if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(PETSC REQUIRED IMPORTED_TARGET PETSc)

    target_link_libraries(${executable_name} PRIVATE
         PkgConfig::PETSC
         interacter_stub
    )
endif(UNIX)

if(WIN32)
    if(WITH_INTERACTER)
        # Add lib_interacter before adding the other libraries. Sometimes it seems to make a difference
        target_link_libraries(${executable_name} PRIVATE ${lib_interacter})
    else()
        target_link_libraries(${executable_name} PRIVATE interacter_stub)
    endif()
    target_link_libraries(${executable_name} PRIVATE
                          petsc
    )
endif(WIN32)

target_link_libraries(${executable_name} PRIVATE
                      dflowfm_kernel
                      gridgeom
                      deltares_common
                      netcdff
)

if(UNIX)
    message(STATUS "Setting target_compile_definitions in Unix")
    target_compile_definitions(${executable_name} PRIVATE "HAVE_DISPLAY=0;HAVE_MPI;HAVE_PETSC")
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)


if(WIN32)
    set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
endif(WIN32)


# Define additional compilation flags
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/net_main.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

if (WIN32)
    target_link_libraries(${executable_name} PRIVATE netcdf)
    include_directories(${mpi_module_path}
                        ${petsc_libdir}
                        ${tecplot_path}
                        ${version_include_dir}
    )

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${petsc_libdir}"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64"
    )

    target_link_libraries(${executable_name} PRIVATE
                          "pthreadVC2.lib"
                          "tecio.lib"
                          "libpetsc.lib"
                          "${mpi_fortran_library}"
    )

    message(STATUS "Setting target_compile_definitions in windows")
    if(WITH_INTERACTER)
        # Add HAVE_DISPLAY=1
        target_compile_definitions(${executable_name} PRIVATE "WIN32;HAVE_MPI;HAVE_PETSC;HAVE_DISPLAY=1")
    else()
        target_compile_definitions(${executable_name} PRIVATE "WIN32;HAVE_MPI;HAVE_PETSC;HAVE_DISPLAY=0")
    endif()


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
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000" )
    configure_visual_studio_user_file(${executable_name})
endif(WIN32)

install(TARGETS ${executable_name} RUNTIME DESTINATION bin)

if (UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/parallel/generate_parallel_mdu.sh  DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/team-city/submit_dflowfm_h7.sh     DESTINATION bin)
endif(UNIX)

install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/team-city/run_dflowfm.${platform_extension}           DESTINATION bin)
install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/team-city/run_dflowfm_processes.${platform_extension} DESTINATION bin)
install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/team-city/run_dfmoutput.${platform_extension}         DESTINATION bin)
install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dflowfm/scripts/team-city/run_dfm_volume_tool.${platform_extension}   DESTINATION bin)
