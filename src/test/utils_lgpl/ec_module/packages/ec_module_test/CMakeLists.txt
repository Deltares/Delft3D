set(executable_name ec_module_test)

file(GLOB source    src/*.f90
                    src/*.F90)

add_executable(${executable_name} ${source})




if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies    deltares_common
                            deltares_common_c
                            deltares_common_mpi
                            ec_module
                            ftnunit
                            gridgeom
                            io_netcdf
                            kdtree_wrapper
                            kdtree2)

    oss_include_libraries(${executable_name} oss_dependencies)

    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)

    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)



if(WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         ec_module
                         ftnunit
                         gridgeom
                         io_netcdf
                         kdtree_wrapper
                         kdtree2
                         netcdf4
                         netcdff)

    target_link_directories(${executable_name} PRIVATE
                                "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    oss_include_libraries(${executable_name}   oss_dependencies)
    target_link_libraries(${executable_name} ${oss_dependencies}
                                             "netcdf.lib")
endif(WIN32)

set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/ec_module_test.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/src/m_ec_module_test.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})
set(build_project ${executable_name})

post_build_target( ${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${build_project} )

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)


