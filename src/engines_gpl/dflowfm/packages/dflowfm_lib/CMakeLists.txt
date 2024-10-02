# Set directories
# Set the directory of where the source code is located.
set(src_path src)
set(include_path include)

# Set the DFlow FM component path
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../../")

# Set the directory referring to the ref folder
set(res_path ${dflowfm_path}/res)

#Version includes
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(dflowfm_version_header_path ${dflowfm_path}/version)
set(rc_version_file ${dflowfm_version_header_path}/dflowfm_dll_version.rc)

include_directories(${version_include_dir})
if(UNIX)
   include_directories("${CMAKE_CURRENT_SOURCE_DIR}/${include_path}")
endif(UNIX)

# Gather the source files
set(source ${src_path}/unstruc_bmi.F90
           ${src_path}/unstruc_dll_api.F90
           )
file(GLOB includes ${include_path}/*.inc)

# Define the executable
set(library_name dflowfm_dll)
add_library(${library_name} SHARED  ${source}
                                    ${includes}
                                    ${rc_version_file})

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
                            netcdf4
                            netcdff
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

    target_link_libraries(${library_name}
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
                            netcdf4
                            netcdff
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

    target_link_libraries(${library_name} ${oss_dependencies})
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

    target_include_directories(${library_name} PRIVATE  ${include_path}
                                                        ${mpi_module_path}
                                                        ${petsc_libdir}
                                                        ${tecplot_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${petsc_libdir}"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64")


    target_link_libraries(${library_name}
                            "comctl32.lib"
                            "wsock32.lib"
                            "netapi32.lib"
                            "kernel32.lib"
                            "user32.lib"
                            "Shlwapi.lib"
                            "Gdi32.lib"
                            "Winspool.lib"
                            "netcdf.lib"
                            "pthreadVC2.lib"
                            "${mpi_fortran_library}"
                            "libpetsc.lib"
                            "tecio.lib")


    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Change the name of the target library to dflowfm.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME  dflowfm)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} ${includes})
source_group(Resources FILES    ${rc_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/dflowfm)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})


install(TARGETS ${library_name} 
         RUNTIME DESTINATION lib)

