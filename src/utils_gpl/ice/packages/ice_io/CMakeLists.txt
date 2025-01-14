set(library_name ice_io)
get_fortran_source_files(src source_files)
add_library(${library_name} ${source_files})


# Set dependencies
set(oss_dependencies    deltares_common
                        ice_data)

target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "SHELL:$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/ice)
