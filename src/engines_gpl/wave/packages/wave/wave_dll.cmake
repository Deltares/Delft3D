
# Gather source files
set(library_files ${src_path}/wave_bmi.f90) # Because the .dll and the .exe are defined in the same directory, retrieve the relevant files for the library alone

# Define library
set(library_name wave)
add_rc_object_library(${library_name} "${rc_version_file}" "${version_include_dir};${wave_version_path}")
add_library(${library_name} SHARED ${library_files})
target_link_libraries(${library_name} PRIVATE ${library_name}_rc)

target_link_libraries(${library_name} PRIVATE
    wave_data
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
    netCDF::netcdff
    triangle::triangle
    swan
)

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${mpi_library_path}")

    target_link_libraries(${library_name} PRIVATE
                            "pthreadVC2.lib"
                            "${mpi_fortran_library}")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

if(UNIX)
    target_link_libraries(${library_name} PRIVATE esmfsm)

    set_property(TARGET ${library_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

include_directories(${mpi_module_path} ${version_include_dir})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_files})
source_group(Resources FILES    ${rc_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)

# Change the name of the target library to wave.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME wave)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

install(TARGETS ${library_name} RUNTIME DESTINATION bin
                                LIBRARY DESTINATION lib
)
install(FILES $<TARGET_RUNTIME_DLLS:${library_name}> DESTINATION bin)
