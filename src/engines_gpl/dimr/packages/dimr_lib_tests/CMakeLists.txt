# Set the directory of where the source_files code is located.
set(src_path tests)

# Gather source_files files
file(GLOB source_files ${src_path}/*.cpp)

# Define the executable
set(executable_name dimr_lib_tests)
set(dimr_lib_sources "${dimr_src_path}/clock.cpp"
                     "${dimr_src_path}/exception.cpp"
                     "${dimr_src_path}/log.cpp")

add_executable(${executable_name} ${source_files} ${dimr_lib_sources})

set(oss_dependencies deltares_common_c dimr_lib) 

if(UNIX)
   target_compile_definitions(${executable_name} PRIVATE LINUX64)
   target_include_directories(${executable_name} 
                               PRIVATE
                              "${dimr_lib_path}/include"
                              "${expat_include_path}")


    target_link_libraries(${executable_name} ${oss_dependencies}) 
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
    
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries(${executable_name} Threads::Threads)

endif(UNIX)


if (WIN32)
    # Define preprocessor definitions
    target_compile_definitions(${executable_name} PRIVATE WIN32;_CONSOLE)
    target_compile_definitions(${executable_name} PRIVATE $<$<CONFIG:DEBUG>:_DEBUG>)
    
    target_include_directories(${executable_name} 
                               PRIVATE
                               "${expat_include_path}"
                               "${checkout_src_root}/third_party_open/mpich2/include"
                               "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/include"
                               "${checkout_src_root}/third_party_open/pthreads/include/x64")

    # Set the linker properties
    target_link_directories(${executable_name}
                            PRIVATE
                            "${expat_library_path_win}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${checkout_src_root}/third_party_open/mpich2/x64/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name} ${oss_dependencies} 
                                             "fmpich2.lib"
                                             "libexpat.lib"
                                             "mpi.lib"
                                             "pthreadVC2.lib")
endif(WIN32)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/dimr)
