set(library_name trachytopes_io)
create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Add dependencies
set(oss_dependencies deltares_common deltares_common_mpi trachytopes_kernel)
oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

if (UNIX)
   target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/trachytopes)
