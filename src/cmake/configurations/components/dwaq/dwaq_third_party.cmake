# Third-party targets needed for D-Waq
include(${CMAKE_CURRENT_LIST_DIR}/dwaq_functions.cmake)

# List of targets and their module names

add_target_with_subdirectory(kdtree2 ${kdtree_module})
add_target_with_subdirectory(kdtree_wrapper ${kdtree_wrapper_module})
add_target_with_subdirectory(triangle_c ${triangle_c_module})
add_target_with_subdirectory(fortrangis ${fortrangis_module})
add_target_with_subdirectory(shp ${shp_module})

# proj
if(WIN32)
    set(MESSAGE_QUIET ON)
    include_target_with_path(proj ${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/proj_configuration.cmake)
    unset(MESSAGE_QUIET)
endif(WIN32)