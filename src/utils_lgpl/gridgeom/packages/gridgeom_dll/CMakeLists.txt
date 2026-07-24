# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to the location of where this CMakeLists file is defined in.
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/src/gridgeom_version.F90")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(version_dir ${CMAKE_CURRENT_SOURCE_DIR}/version)
set(rc_version_file ${version_dir}/gridgeom_version_number.rc)
set(version_file ${version_dir}/gridgeom_version.h)

# Gather source files
get_fortran_source_files(src source_files)

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add dynamic gridgeom library
add_rc_object_library(gridgeom_dll "${rc_version_file}" "${version_include_dir};${version_dir}")
add_library(gridgeom_dll SHARED ${source_files} ${version_file})
target_link_libraries(gridgeom_dll PRIVATE gridgeom_dll_rc)
include_directories(${version_include_dir} ${version_dir})

target_link_libraries(gridgeom_dll PRIVATE
    deltares_common
    ec_module
    io_netcdf
    io_netcdf_data
    kdtree_wrapper
    kdtree2
    triangle::triangle
    gridgeom
)

if (WIN32)
    # Set additional compilation properties
    target_compile_options(gridgeom_dll PRIVATE "${heap_arrays_one_flag}")
endif(WIN32)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties(gridgeom_dll PROPERTIES FOLDER utils_lgpl/gridgeom)
set_target_properties(gridgeom_dll PROPERTIES OUTPUT_NAME gridgeom)
if(UNIX)
    set_target_properties(gridgeom_dll PROPERTIES PREFIX "")
endif()

install(TARGETS gridgeom_dll
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
)
