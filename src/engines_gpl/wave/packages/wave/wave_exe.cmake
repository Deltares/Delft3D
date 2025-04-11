# Gather source files
set(executable_files ${src_path}/wave_exe.f90) # Because the .dll and the .exe are defined in the same directory, retrieve the relevant files for the executable alone

# Define icon
set(icon_file resource/wl.ico)

# Define executable
set(executable_name wave_exe)
add_executable(${executable_name}   ${executable_files}
                                    ${rc_version_file}
                                    ${icon_file})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${extend_source132_flag}")

# Set dependencies
if (WIN32)
    set(exe_dependencies    wave_data
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
                            netcdf4
                            netcdff
                            triangle_c
                            swan
                            ) 

    oss_include_libraries(${executable_name} exe_dependencies)
    target_link_libraries(${executable_name} ${exe_dependencies})

endif(WIN32)

# Add dependencies
if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(exe_dependencies    wave_data
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
    
    oss_include_libraries(${executable_name} exe_dependencies)

    target_link_libraries(${executable_name}
         ${exe_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)

include_directories(${mpi_include_path} ${mpi_module_path} ${version_include_dir})

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties on windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${mpi_library_path}")

    target_link_libraries(${executable_name}                                                   
                            "pthreadVC2.lib"
                            "netcdf.lib"
                            "${mpi_fortran_library}")

    # Set linker options
    message(STATUS "Setting target_link_options on windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

if (UNIX)
    # Set linker properties
    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
    
    #target_link_options(${executable_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${executable_files})
source_group(Resources FILES    ${rc_version_file}
                                ${icon_file})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/wave)


# Change the name of the target library to wave.exe
set_target_properties (${executable_name} PROPERTIES OUTPUT_NAME wave_exe)
if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/wave/scripts/run_dwaves.sh  DESTINATION bin)
endif(UNIX)