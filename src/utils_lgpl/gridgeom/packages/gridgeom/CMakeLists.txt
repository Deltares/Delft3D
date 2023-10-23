# Set the path where the source files are located
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

set(library_name gridgeom)
add_library(${library_name} ${source_files})

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
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files} ${fortran_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/gridgeom)