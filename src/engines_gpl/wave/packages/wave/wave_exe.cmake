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
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)

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
                            netcdff
                            )
    
    oss_include_libraries(${executable_name} exe_dependencies)

    target_link_libraries(${executable_name}
         ${exe_dependencies}
         PkgConfig::NETCDF
         )
endif(UNIX)

include_directories(${mpi_module_path} ${version_include_dir})

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
	set (userfilename "${CMAKE_BINARY_DIR}/template.vfproj.user")
	configure_file(
    ${userfilename}
    "${CMAKE_CURRENT_BINARY_DIR}/${executable_name}.vfproj.$ENV{USERNAME}.user"
    @ONLY
	)
endif(WIN32)

if (UNIX)
    install(PROGRAMS $<TARGET_FILE:${executable_name}> RENAME wave DESTINATION bin)
elseif (WIN32)
    install(PROGRAMS $<TARGET_FILE:${executable_name}> RENAME wave.exe DESTINATION bin)
endif()

install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/wave/scripts/run_dwaves.${platform_extension}  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../third_party_open/esmf/lnx64/scripts/ESMF_RegridWeightGen_in_Delft3D-WAVE.sh DESTINATION bin)
endif(UNIX)
if(WIN32)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../third_party_open/esmf/win64/scripts/ESMF_RegridWeightGen_in_Delft3D-WAVE.bat DESTINATION bin)
    install (DIRECTORY ${CMAKE_SOURCE_DIR}/../third_party_open/esmf/win64/bin/ DESTINATION lib
FILES_MATCHING
PATTERN "*.dll"
PATTERN "*.dll.a"
)
    install (PROGRAMS ${CMAKE_SOURCE_DIR}/../third_party_open/esmf/win64/bin/ESMF_RegridWeightGen.exe DESTINATION bin)
endif(WIN32)