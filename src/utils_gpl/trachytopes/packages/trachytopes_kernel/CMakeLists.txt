set(library_name trachytopes_kernel)
create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Add dependencies
target_link_libraries(${library_name} deltares_common)
set(oss_dependencies deltares_common)

target_link_libraries(${library_name} ${oss_dependencies})

set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/trachytopes)
