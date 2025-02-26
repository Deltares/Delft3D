set(executable_name dflowfm_kernel_test)

file(GLOB source    src/*.f90)

add_executable(${executable_name} ${source})

if(UNIX)
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)

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
                            ftnunit
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
                            morphology_data
                            morphology_io
                            morphology_kernel
                            morphology_plugins_c
                            polypack
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            wq_processes
                            waq_process
                            waq_utils_c
                            waq_utils_f
                            dflowfm_kernel)


    target_link_libraries(${executable_name}
         ${oss_dependencies}
         Threads::Threads
         PkgConfig::PETSC)

    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)



if(WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         deltares_common_mpi
                         dhydrology_kernel
                         ec_module
                         flow1d
                         flow1d_core
                         flow1d_io
                         ftnunit
                         gridgeom
                         io_netcdf
                         io_netcdf_data
                         kdtree_wrapper
                         kdtree2
                         fortrangis
                         libsigwatch
                         md5digest
                         md5
                         morphology_data
                         morphology_io
                         morphology_kernel
                         morphology_plugins_c
                         netcdff
                         petsc
                         polypack
                         trachytopes_io
                         trachytopes_kernel
                         triangle_c
                         wq_processes
                         waq_process
                         waq_utils_c
                         waq_utils_f
                         interacter_stub
                         dflowfm_kernel)


target_link_directories(${executable_name} PRIVATE
                        "${mpi_library_path}"
                        "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                        "${petsc_libdir}"
                        "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                        "${checkout_src_root}/third_party_open/Tecplot/lib/x64")

target_link_libraries(${executable_name} ${oss_dependencies}
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


endif(WIN32)

include_directories(${mpi_module_path}
                    ${petsc_path}
                    ${tecplot_path})

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tests/engines_gpl/dflowfm)

configure_visual_studio_user_file(${executable_name})

install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../test_dflowfm/test_data DESTINATION test)
if (WIN32)
	install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scripts/ DESTINATION test)
endif()
install(TARGETS ${executable_name} RUNTIME  DESTINATION test)
