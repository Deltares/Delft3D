set(library_name kdtree_wrapper)
create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

set (oss_dependencies deltares_common kdtree2)

target_link_libraries(${library_name} ${oss_dependencies})

set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/kdtree_wrapper)
