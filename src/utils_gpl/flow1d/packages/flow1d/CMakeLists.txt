set(library_name flow1d)
create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Specify dependencies
set(oss_dependencies    deltares_common
                        flow1d_core
                        flow1d_io
                        gridgeom
                        io_netcdf
                        io_netcdf_data)

target_link_libraries(${library_name} ${oss_dependencies})

set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/flow1d_library)
