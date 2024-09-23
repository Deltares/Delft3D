# Add the resource files as a separate target, since we don't want to share the compiler flags for the Fortran and RC compilers.
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../..")
set(res_path ${dflowfm_path}/res)
set(icon_file ${res_path}/dflowfm_icon.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(dflowfm_cli_version_file ${dflowfm_path}/version/dflowfm_cli_version.rc)

add_library(dflowfm_cli_resources ${icon_file} ${dflowfm_cli_version_file})
target_include_directories(dflowfm_cli_resources PRIVATE ${version_include_dir})
set_target_properties (dflowfm_cli_resources PROPERTIES FOLDER engines_gpl/dflowfm/resources)

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

# Gather the source files
file(GLOB source src/net_main.F90)

# Define the executable
set(executable_name dflowfm-cli)
add_executable(${executable_name} ${source})

# Add dependencies
if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(PETSC      REQUIRED IMPORTED_TARGET PETSc)

    set(oss_dependencies    deltares_common
                            deltares_common_c
                            deltares_common_mpi
                            dhydrology_kernel
                            ec_module
                            flow1d
                            flow1d_core
                            flow1d_io
                            gridgeom
                            io_netcdf
                            io_netcdf_data
                            interacter_stub
                            kdtree_wrapper
                            kdtree2
                            fortrangis
                            libsigwatch
                            md5digest
                            md5
                            metis
                            metisoptions
                            morphology_data
                            morphology_io
                            morphology_kernel
                            morphology_plugins_c
                            nefis
                            polypack
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            waq_definition
                            wq_processes
                            waq_process
                            waq_hyd_data
                            waq_utils_c
                            waq_utils_f
                            dflowfm_kernel)

    target_link_libraries(${executable_name} PRIVATE
         ${oss_dependencies}
         PkgConfig::PETSC
)
endif(UNIX)

if(WIN32)
    set(oss_dependencies    deltares_common
                            deltares_common_c
                            deltares_common_mpi
                            dhydrology_kernel
                            ec_module
                            flow1d
                            flow1d_core
                            flow1d_io
                            gridgeom
                            io_netcdf
                            io_netcdf_data
                            kdtree_wrapper
                            kdtree2
                            fortrangis
                            libsigwatch
                            md5digest
                            md5
                            metis
                            metisoptions
                            morphology_data
                            morphology_io
                            morphology_kernel
                            morphology_plugins_c
                            nefis
                            petsc
                            polypack
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            waq_definition
                            wq_processes
                            waq_process
                            waq_hyd_data
                            waq_utils_c
                            waq_utils_f
                            dflowfm_kernel)


    if(WITH_INTERACTER)
        # Add lib_interacter before adding the other libraries. Sometimes it seems to make a difference
        target_link_libraries(${executable_name} PRIVATE ${lib_interacter})
    else()
        set(oss_dependencies ${oss_dependencies} interacter_stub)
    endif()

    target_link_libraries(${executable_name} PRIVATE ${oss_dependencies})
endif(WIN32)

oss_include_libraries(${executable_name} oss_dependencies)

if(UNIX)
    message(STATUS "Setting target_compile_definitions in Unix")
    target_compile_definitions(${executable_name} PRIVATE "HAVE_DISPLAY=0;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC")
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)


if(WIN32)
    set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
endif(WIN32)


# Define additional compilation flags
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/net_main.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

if (WIN32)
    include_directories(${mpi_module_path}
                        ${petsc_libdir}
                        ${tecplot_path}
                        ${dflowfm_version_path})


    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${petsc_libdir}"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64")

    target_link_libraries(${executable_name} PRIVATE
                          "comctl32.lib"
                          "wsock32.lib"
                          "netapi32.lib"
                          "kernel32.lib"
                          "user32.lib"
                          "Shlwapi.lib"
                          "Gdi32.lib"
                          "Winspool.lib"
                          "pthreadVC2.lib"
                          "${mpi_fortran_library}"
                          "libpetsc.lib"
                          "tecio.lib")

    message(STATUS "Setting target_compile_definitions in windows")
    if(WITH_INTERACTER)
        # Add HAVE_DISPLAY=1
        target_compile_definitions(${executable_name} PRIVATE "WIN32;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC;HAVE_OPENGL;HAVE_DISPLAY=1")
    else()
        target_compile_definitions(${executable_name} PRIVATE "WIN32;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC;HAVE_OPENGL;HAVE_DISPLAY=0")
    endif()


    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

target_link_directories(${executable_name} PRIVATE
                        "${mpi_library_path}")
                        
target_link_libraries(${executable_name} PRIVATE "${mpi_fortran_library}"
                                                 netcdf-fortran::netcdf-fortran
                                                 dflowfm_cli_resources
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
