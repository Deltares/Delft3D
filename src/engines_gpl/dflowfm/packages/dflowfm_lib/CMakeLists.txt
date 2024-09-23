# Add the resource files as a separate target, since we don't want to share the compiler flags for the Fortran and RC compilers.
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../..")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(dflowfm_dll_version_file ${dflowfm_path}/version/dflowfm_dll_version.rc)
add_library(dflowfm_dll_resources ${dflowfm_dll_version_file})
target_include_directories(dflowfm_dll_resources PRIVATE ${version_include_dir})
set_target_properties (dflowfm_dll_resources PROPERTIES FOLDER engines_gpl/dflowfm/resources)

# Gather the source files
set(source src/unstruc_bmi.F90
           src/unstruc_dll_api.F90
)
file(GLOB includes include/*.inc)

# Define the executable.
set(library_name dflowfm_dll)
add_library(${library_name} SHARED ${source}
                                   ${includes}
)

target_include_directories(${library_name} PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/include")

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(PETSC REQUIRED IMPORTED_TARGET PETSc)

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

    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name} PRIVATE
         ${oss_dependencies}
         PkgConfig::PETSC)
endif(UNIX)

if (WIN32)
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

    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name} PRIVATE ${oss_dependencies})
endif(WIN32)

if(UNIX)
    message(STATUS "Setting target_compile_definitions in Unix")
    target_compile_definitions(${library_name} PRIVATE "HAVE_DISPLAY=0;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC")
endif(UNIX)

if(WIN32)
    message(STATUS "Setting target_compile_definitions in windows")
    target_compile_definitions(${library_name} PRIVATE "WIN32;HAVE_DISPLAY=0;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC;HAVE_OPENGL")
endif(WIN32)


# Define additional compilation flags
set_source_files_properties(${source}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


if (WIN32)
    target_include_directories(${library_name} PRIVATE  ${mpi_module_path}
                                                        ${petsc_libdir}
                                                        ${tecplot_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${petsc_libdir}"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64")


    target_link_libraries(${library_name} PRIVATE
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


    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

target_link_libraries(${library_name} PRIVATE dflowfm_dll_resources
                                              netcdf-fortran::netcdf-fortran
)

# Change the name of the target library to dflowfm.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME dflowfm)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} ${includes})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/dflowfm)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})


install(TARGETS ${library_name} 
         RUNTIME DESTINATION lib)

