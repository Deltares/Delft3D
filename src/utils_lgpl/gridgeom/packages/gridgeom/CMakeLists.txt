create_target(gridgeom ${CMAKE_CURRENT_SOURCE_DIR} src_dir src)

target_link_libraries(gridgeom PRIVATE
    deltares_common
    io_netcdf_data
    kdtree_wrapper
    kdtree2
    triangle::triangle
)

target_include_directories(gridgeom PRIVATE ${version_include_dir} ${rc_dir})

if (WIN32)
    # Set additional compilation properties
    target_compile_options(gridgeom PRIVATE "${heap_arrays_one_flag}")
endif(WIN32)

# Create the folder structure in vfproj
set_target_properties (gridgeom PROPERTIES FOLDER utils_lgpl/gridgeom)
