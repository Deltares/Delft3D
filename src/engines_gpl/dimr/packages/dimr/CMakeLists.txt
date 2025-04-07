# Set the directory of where the source code is located.
set(src_path src)
set(header_path include)

# Set variable to trace to the dimr_lib package as some files are included from dimr_lib for this project
set(dimr_lib_path "${CMAKE_CURRENT_SOURCE_DIR}/../dimr_lib/")

# Set version file variables
set(absolute_header_path "${CMAKE_CURRENT_SOURCE_DIR}/${header_path}") 
set(rc_version_file "${absolute_header_path}/dimr_exe_version.rc")
set(header_version_file "${absolute_header_path}/dimr_exe_version.h")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

include_directories(${version_include_dir})

# Gather source files
file(GLOB source ${src_path}/*.cpp)

set(dimr_src_path "${dimr_lib_path}/src")
set(dimr_lib_sources "${dimr_src_path}/clock.cpp"
                     "${dimr_src_path}/exception.cpp"
                     "${dimr_src_path}/log.cpp")

# Define the executable
set(executable_name dimr)
add_executable(${executable_name}   ${source}
                                    ${dimr_lib_sources}
                                    ${rc_version_file} 
                                    ${header_version_file})

# Common UNIX and WIN32 link library dependency
target_link_libraries(${executable_name} netcdf)

if(UNIX)
   add_definitions(-DD3D_PLUGIN_EXT=".so")

   set(oss_dependencies    deltares_common_c
                           dimr_lib
                           expat)

   target_compile_definitions(${executable_name} PRIVATE LINUX64)
   target_include_directories(${executable_name} 
                               PRIVATE  
                               ${header_path}
                              "${dimr_lib_path}/include"
                              "${expat_include_path}")

    target_link_libraries(${executable_name} ${oss_dependencies} ${CMAKE_DL_LIBS})

    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries(${executable_name} Threads::Threads)


endif(UNIX)

if (WIN32)

   set(oss_dependencies    deltares_common_c
                           dimr_lib) 
    # Define preprocessor definitions
    target_compile_definitions(${executable_name} PRIVATE WIN32;_CONSOLE)
    target_compile_definitions(${executable_name} PRIVATE $<$<CONFIG:DEBUG>:_DEBUG>)
    
    target_include_directories(${executable_name} 
                                PRIVATE  
                                ${header_path}
                                "${dimr_lib_path}/include"
                                "${expat_include_path}"
                                "${mpi_include_path}"
                                "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/include"
                                "${checkout_src_root}/third_party_open/pthreads/include/x64")

    # Set the linker properties
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${expat_library_path_win}"
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}    ${oss_dependencies}
                                                "${mpi_c_library}"
                                                "libexpat.lib"
                                                "pthreadVC2.lib")
endif(WIN32)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} ${dimr_lib_sources} ${header_version_file}  ${rc_version_file})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/dimr)
if (WIN32)
	if (CMAKE_GENERATOR MATCHES "Visual Studio" AND NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${executable_name}.vcxproj.$ENV{USERNAME}.user")
		configure_file(
		"${CMAKE_BINARY_DIR}/template.vcxproj.user"
		"${CMAKE_CURRENT_BINARY_DIR}/${executable_name}.vcxproj.user"
		@ONLY
		)
	endif()
endif(WIN32)

 message(STATUS "build_config ${build_config}")

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/lnx64/run_dimr.sh             DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/lnx64/submit_dimr.sh          DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/lnx64/submit_dimr_h7.sh       DESTINATION bin)
endif(UNIX)

if (WIN32)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/win64/run_dimr.bat            DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/win64/run_dimr_parallel.bat   DESTINATION bin)
endif(WIN32)
