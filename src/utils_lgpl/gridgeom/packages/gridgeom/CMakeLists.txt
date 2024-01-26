set(library_name gridgeom)


create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

# Set the dependencies of this library
set(oss_dependencies    deltares_common
                        io_netcdf_data
                        kdtree_wrapper
                        kdtree2
                        triangle_c)

oss_include_libraries(${library_name} oss_dependencies)

target_include_directories(${library_name}  PRIVATE ${version_include_dir} ${rc_dir})

if (UNIX)
    message(STATUS "Setting oss_target_link_libraries in Unix")
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

if (WIN32)
    target_link_libraries(${library_name} kdtree2
                                          triangle_c)

    # Set additional compilation properties
    target_compile_options(${library_name} PRIVATE "${heap_arrays_one_flag}")
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the folder structure in vfproj
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/gridgeom)
