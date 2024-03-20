# Set the path where the source files are located
set(src_path src)

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to the location of where this CMakeLists file is defined in.
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/gridgeom_version.F90")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(version_dir ${CMAKE_CURRENT_SOURCE_DIR}/version)
set(rc_version_file ${version_dir}/gridgeom_version_number.rc)
set(version_file ${version_dir}/gridgeom_version.h)

# Gather source files
get_fortran_source_files(${src_path} source_files)

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

set(library_name gridgeom_dll)

# Add dynamic gridgeom library
add_library(${library_name} SHARED ${source_files} ${rc_version_file} ${version_file})
include_directories(${version_include_dir} ${version_dir})

# Set the dependencies of this library
set(oss_dependencies    deltares_common
                        ec_module
                        io_netcdf
                        io_netcdf_data
                        kdtree_wrapper
                        kdtree2
                        triangle_c
                        gridgeom)

oss_include_libraries(${library_name} oss_dependencies)

if (UNIX)
    message(STATUS "Setting oss_target_link_libraries in Unix")
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

if (WIN32)
    target_link_libraries(${library_name} ${oss_dependencies}
                                          triangle_c)

    # Set additional compilation properties
    target_compile_options(${library_name} PRIVATE "${heap_arrays_one_flag}")
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/gridgeom)

if ((CMAKE_GENERATOR MATCHES "Visual Studio") OR UNIX)
    # Install the shared library in the lib folder with rename
    install(PROGRAMS $<TARGET_FILE:${library_name}> RENAME gridgeom.${library_suffix} DESTINATION lib)
endif()