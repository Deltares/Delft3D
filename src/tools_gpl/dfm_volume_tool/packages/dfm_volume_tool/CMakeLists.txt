# Set directories
# Set the directory of where the source code is located.
set(src_path src)
set(res_path res)
# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
# Gather the source files
get_fortran_source_files(${src_path} sources)
set(rc_version_file ${res_path}/dfm_volume_tool_version.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(fortran_version_files ${src_path}/dfm_volume_tool_version.F90
                          ${src_path}/dfm_volume_tool_static_version_info.F90)  
set_source_files_properties(${fortran_version_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")                          
# Define the executable
set(executable_name dfm_volume_tool)
add_executable(${executable_name} ${sources} ${rc_version_file})

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies deltares_common 
                         deltares_common_c
                         dflowfm_kernel
                         gridgeom
                         flow1d_core
                         io_netcdf 
                         FLAP
                         dfm_api_access)

    oss_include_libraries(${executable_name} oss_dependencies)

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)

if (WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         dflowfm_kernel
                         gridgeom
                         flow1d_core
                         io_netcdf
                         FLAP
                         netcdf4
                         netcdff
                         dfm_api_access)

    oss_include_libraries(${executable_name} oss_dependencies)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

if(UNIX)
    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})

    message(STATUS "Setting target_compile_definitions in Unix")    
   set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
   
   include_directories(${version_include_dir}
					   ${res_path})
endif(UNIX)

if (WIN32)
   target_compile_definitions(${executable_name} PRIVATE WIN64)
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dfm_volume_tool_main.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_link_libraries(${executable_name} netcdf)

if (WIN32)
    include_directories(${mpi_module_path} ${mpi_include_path}
                        ${tecplot_path}
						${version_include_dir}
						${res_path})
                        

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64")

    target_link_libraries(${executable_name}
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
                          "tecio.lib")

    message(STATUS "Setting target_compile_definitions in windows")
    target_compile_definitions(${executable_name} PRIVATE "WIN32;HAVE_DISPLAY=0;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PETSC")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} ${fortran_version_file})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/dfm_volume_tool)
install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)


# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name})
