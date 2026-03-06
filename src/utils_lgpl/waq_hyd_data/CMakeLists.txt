set(library_name waq_hyd_data)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir .)

# Set dependencies
set(oss_dependencies deltares_common
        io_netcdf_data)

target_link_libraries(${library_name} ${oss_dependencies})
set_target_properties(${library_name} PROPERTIES FOLDER utils_lgpl/waq_hyd_data)
