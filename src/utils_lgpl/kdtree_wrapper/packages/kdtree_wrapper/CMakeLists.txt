set(library_name kdtree_wrapper)
get_fortran_source_files(src source_files)
add_library(${library_name} ${source_files})

set (oss_dependencies deltares_common kdtree2)
oss_include_libraries(${library_name} oss_dependencies)

if (UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/kdtree_wrapper)
