set(library_name trachytopes_io)
create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Add dependencies
set(oss_dependencies deltares_common deltares_common_mpi trachytopes_kernel)

target_link_libraries(${library_name} ${oss_dependencies})

set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/trachytopes)
