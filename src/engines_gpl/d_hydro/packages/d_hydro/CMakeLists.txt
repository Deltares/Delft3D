# Define the executable
set(executable_name d_hydro)
# Set the directory of where the source code is located.
set(src_path src)
set(header_path include)

# Define include directories
get_module_include_path(${checkout_src_root}/${d_hydro_lib_module} 
                        ${executable_name}
                        d_hydro_lib_include_path)
get_module_include_path(${checkout_src_root}/${deltares_common_c_module} 
                        ${executable_name}
                        deltares_common_c_include_path)

message(STATUS "Configuring '${executable_name}' with the following d_hydro_lib include path: '${d_hydro_lib_include_path}'")
message(STATUS "Configuring '${executable_name}' with the following deltares_common_c include path: '${deltares_common_c_include_path}'")

# Set version file variables
set(absolute_header_path "${CMAKE_CURRENT_SOURCE_DIR}/${header_path}") 
set(rc_version_file "${absolute_header_path}/d_hydro_version.rc")
set(header_version_file "${absolute_header_path}/d_hydro_version.h")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

include_directories(${include_path}
                    ${deltares_common_c_include_path}
                    ${d_hydro_lib_include_path}
                    ${version_include_dir})

if (UNIX)
    add_definitions(-DD3D_PLUGIN_EXT=".so")
endif(UNIX)

# Gather source files
file(GLOB source ${src_path}/*.cpp)

set(d_hydro_src_path "${d_hydro_lib_path}/src")

add_executable(${executable_name}   ${source}
                                    ${rc_version_file} 
                                    ${header_version_file})

# Common UNIX and WIN32 link library dependency
if(UNIX)
   set(oss_dependencies    deltares_common_c
                           d_hydro_lib
                           expat)

   target_compile_definitions(${executable_name} PRIVATE LINUX64)
   target_include_directories(${executable_name} 
                               PRIVATE  
                               ${header_path}
                              "${d_hydro_lib_path}/include"
                              "${expat_include_path}")

    target_link_libraries(${executable_name} ${oss_dependencies})

    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries(${executable_name} Threads::Threads)
    add_definitions(-DHAVE_CONFIG_H)
endif(UNIX)

if (WIN32)
   set(oss_dependencies    deltares_common_c
                           d_hydro_lib) 
    # Define preprocessor definitions
    target_compile_definitions(${executable_name} PRIVATE WIN32;_CONSOLE)
    target_compile_definitions(${executable_name} PRIVATE $<$<CONFIG:DEBUG>:_DEBUG>)
    
    target_include_directories(${executable_name} 
                                PRIVATE  
                                ${header_path}
                                "${d_hydro_lib_path}/include"
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
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} ${d_hydro_lib_sources} ${header_version_file}  ${rc_version_file})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/d_hydro)
if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
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
