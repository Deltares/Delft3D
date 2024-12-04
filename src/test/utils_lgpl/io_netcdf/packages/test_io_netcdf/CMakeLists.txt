set(executable_name test_io_netcdf)

file(GLOB source src/*.f90)

add_executable(${executable_name} ${source})

set(oss_dependencies    fortrangis
                        ftnunit
                        io_netcdf
                        netcdf4
                        netcdff)

if (UNIX)
    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
    target_link_libraries(${executable_name} ${oss_dependencies})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
    target_link_directories(${executable_name} PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/${netcdf_version}/lib")

    target_link_libraries(${executable_name} ${oss_dependencies} "netcdf.lib")
endif(WIN32)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tests/utils_lgpl/io_netcdf)

install(TARGETS ${executable_name} RUNTIME DESTINATION test)