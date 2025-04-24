set(executable_name test_io_netcdf)

file(GLOB source src/*.f90)

add_executable(${executable_name} ${source})

set(oss_dependencies    fortrangis
                        ftnunit
                        io_netcdf
                        netCDF::netcdf
)

target_link_libraries(${executable_name} ${oss_dependencies})

if (UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tests/utils_lgpl/io_netcdf)

install(TARGETS ${executable_name} RUNTIME DESTINATION test)