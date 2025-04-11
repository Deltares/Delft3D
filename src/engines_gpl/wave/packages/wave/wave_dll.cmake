
# Gather source files
set(library_files ${src_path}/wave_bmi.f90) # Because the .dll and the .exe are defined in the same directory, retrieve the relevant files for the library alone

# Define library
set(library_name wave)
add_library(${library_name} SHARED  ${library_files}
                                    ${rc_version_file})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")

# Set dependencies on windows
if (WIN32)
    set(library_dependencies    wave_data
                                delftio
                                delftio_shm
                                deltares_common
                                deltares_common_c
                                deltares_common_mpi
                                ec_module
                                gridgeom
                                wave_io
                                io_netcdf
                                wave_kernel
                                wave_manager
                                nefis
                                netcdf4
                                netcdff
                                triangle_c
                                swan
                                ) 

    oss_include_libraries(${library_name} library_dependencies)
    target_link_libraries(${library_name} ${library_dependencies})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${mpi_library_path}")

    target_link_libraries(${library_name}                                                   
                            "pthreadVC2.lib"
                            "netcdf.lib"
                            "${mpi_fortran_library}")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Set dependencies on linux
if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(library_dependencies    wave_data
                                delftio
                                delftio_shm
                                deltares_common
                                deltares_common_c
                                ec_module
                                gridgeom
                                wave_io
                                io_netcdf
                                wave_kernel
                                wave_manager
                                nefis
                                triangle_c
                                swan
                                esmfsm
                                )
                                
    oss_include_libraries(${library_name} library_dependencies)

    target_link_libraries(${library_name}
         ${library_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)

    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${library_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
    
    #target_link_options(${library_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${library_name} PROPERTY LINKER_LANGUAGE Fortran)

endif(UNIX)
include_directories(${mpi_include_path} ${mpi_module_path} ${version_include_dir})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_files})
source_group(Resources FILES    ${rc_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)

# Change the name of the target library to wave.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME wave)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${library_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${library_name})

install(TARGETS ${library_name} DESTINATION lib)
