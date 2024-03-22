# Define library
set(library_name io_hyd)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
endif(UNIX)


# Set dependencies
set(oss_dependencies    deltares_common
                        waq_hyd_data
                        io_netcdf_data
                        io_netcdf
                        netcdff)

target_link_libraries(${library_name} ${oss_dependencies})

if(UNIX)
    target_compile_definitions(${library_name} PRIVATE LINUX64)
endif(UNIX)

set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_hyd)
