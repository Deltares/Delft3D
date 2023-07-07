# Set the path where the source files are located
set(src_path src) 

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to the location of where this CMakeLists file is defined in.
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/gridgeom_version.F90")
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)
set(rc_dir ${CMAKE_CURRENT_SOURCE_DIR}/version)
set(rc_version_file ${rc_dir}/gridgeom_version_number.rc)

# Gather source files
get_fortran_source_files(${src_path} source_files)

list(REMOVE_ITEM source_files ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/gridgeom_api.F90)

set(library_name gridgeom)
add_library(${library_name} ${source_files}
                            ${fortran_version_file})

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


# Set the dependencies of this library
set(oss_dependencies    deltares_common
                        io_netcdf
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

###############
# GRIDGEOM_DLL
###############
set(library_name gridgeom_dll)
set(library_output_name gridgeom)

# Add dynamic gridgeom library
add_library(${library_name} SHARED ${src_path}/gridgeom_api.F90 ${rc_version_file})
include_directories(${version_include_dir})

# Set the dependencies of this library
set(oss_dependencies    deltares_common
                        ec_module
                        io_netcdf
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
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${src_path}/gridgeom_api.F90)
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/gridgeom)

# Rename the gridgeom_dll shared object to one without the "_dll"
# A simple change of the output name of the gridgeom_dll project to gridgeom does not work
# because CMake then gives an error when trying to replace the static library artifacts from the gridgeom project
# Another solution would be to split gridgeom and gridgeom_dll in two separate components with their own CMakeLists.txt
if(WIN32)
    if (CMAKE_GENERATOR MATCHES "Visual Studio") # for visual studio
      SET(library_renamed ${CMAKE_BINARY_DIR}/gridgeom/$<CONFIG>/gridgeom.dll)
      ADD_CUSTOM_COMMAND(TARGET gridgeom_dll POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E rename
                       $<TARGET_FILE:${library_name}>
                       ${library_renamed}
                   COMMENT "Renaming '${library_name}' library to '${library_renamed}'")
    else() # for Ninja generator on Windows
      SET(library_renamed ${CMAKE_BINARY_DIR}/gridgeom/gridgeom.dll)
    endif()

endif(WIN32)

if(UNIX)
    SET(library_renamed libgridgeom.so)
    # Actual rename is done in the the install step below
endif(UNIX)

if(WIN32)
    # post-build
    set(install_dir ${CMAKE_BINARY_DIR})
    set(build_dir ${CMAKE_BINARY_DIR})
    set(build_project ${library_name})

    post_build_target( ${library_name}
                       ${install_dir}
                       ${build_dir} 
                       ${checkout_src_root} 
                       ${build_project} )

endif(WIN32)

if ((CMAKE_GENERATOR MATCHES "Visual Studio") OR UNIX)
    # Install the shared library in the lib folder with rename
    install(PROGRAMS $<TARGET_FILE:${library_name}> RENAME ${library_renamed} DESTINATION lib)  
endif()