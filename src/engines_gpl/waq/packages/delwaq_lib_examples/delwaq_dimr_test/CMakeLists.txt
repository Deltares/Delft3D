# Include all directories
set(src_path src) 

# Gather the source files
get_fortran_source_files(${src_path} source_files)

# Executable target
set(executable_name delwaq_dimr_test)
add_executable(${executable_name} ${source_files}) 

# Extra dependencies
set(oss_dependencies delwaq_lib)
oss_include_libraries(${executable_name} oss_dependencies)

# Link libraries
target_link_libraries(${executable_name} ${oss_dependencies})

# Other link libraries
if (WIN32)
target_link_directories(${executable_name} PRIVATE
                        "${mpi_library_path}"
                        "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib"
                        "${checkout_src_root}/third_party_open/pthreads/bin/x64")

target_link_libraries(${executable_name}
                      "libiomp5md.lib"
                      "${mpi_fortran_library}"
                      "WbeMuuid.lib"
                      "ole32.lib"
                      "oleaut32.lib"
                      "wsock32.lib"
                      "netapi32.lib"
                      "kernel32.lib"
                      "user32.lib" 
                      "Shlwapi.lib"
                      "Gdi32.lib"
                      "Winspool.lib"
                      "netcdf.lib"
                      "pthreadVC2.lib")
endif(WIN32)
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    target_link_libraries(${executable_name} PkgConfig::NETCDF PkgConfig::NETCDF_FTN)
    target_link_options(${executable_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
set_target_properties(${executable_name} PROPERTIES FOLDER engines_gpl/waq)

# Define how the files should be visualized in visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})



