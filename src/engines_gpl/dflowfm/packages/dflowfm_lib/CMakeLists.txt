# Set the DFlow FM component path
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../../")

#Version includes
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rc_version_file ${dflowfm_path}/version/dflowfm_dll_version.rc)

include_directories(${version_include_dir})
if(UNIX)
   include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include")
endif(UNIX)

# Gather the source files
set(source src/unstruc_bmi.F90
           src/unstruc_dll_api.F90
)
file(GLOB includes include/*.inc)

# Define the executable
set(library_name dflowfm_dll)
add_library(${library_name} SHARED ${source}
                                   ${includes}
)

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(PETSC REQUIRED IMPORTED_TARGET PETSc)

    target_link_libraries(${library_name} PRIVATE PkgConfig::PETSC)
endif(UNIX)

target_link_libraries(${library_name} PRIVATE
                      dflowfm_kernel
                      gridgeom
                      deltares_common
)

if(UNIX)
    message(STATUS "Setting target_compile_definitions in Unix")
    target_compile_definitions(${library_name} PRIVATE "HAVE_MPI;HAVE_PETSC")
endif(UNIX)

if(WIN32)
    message(STATUS "Setting target_compile_definitions in windows")
    target_compile_definitions(${library_name} PRIVATE "WIN32;HAVE_MPI;HAVE_PETSC")
endif(WIN32)

# Define additional compilation flags
set_source_files_properties(${source} PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

if (WIN32)
    target_include_directories(${library_name} PRIVATE  include
                                                        ${mpi_module_path}
                                                        ${petsc_libdir}
                                                        ${tecplot_path}
    )

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${petsc_libdir}"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64")

    target_link_libraries(${library_name} PRIVATE
                          "pthreadVC2.lib"
                          "tecio.lib"
                          "libpetsc.lib"
                          "${mpi_fortran_library}"
    )


    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})

    target_sources(${library_name} PRIVATE ${rc_version_file})
endif(WIN32)

# Change the name of the target library to dflowfm.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME  dflowfm)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} ${includes})
source_group(Resources FILES ${rc_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/dflowfm)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

install(TARGETS ${library_name}
         RUNTIME DESTINATION lib
)
