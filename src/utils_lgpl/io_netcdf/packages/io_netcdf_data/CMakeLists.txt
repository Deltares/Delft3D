set(library_name io_netcdf_data)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src recursive)

# Set dependencies
set(oss_dependencies deltares_common)

# Create the folder structure in vfproj
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)

# Set dependencies
set(oss_dependencies deltares_common)

target_link_libraries(${library_name} ${oss_dependencies})
