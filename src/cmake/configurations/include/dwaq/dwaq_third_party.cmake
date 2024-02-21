# Third-party targets needed for D-Waq

if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
endif()

if(NOT TARGET fortrangis)
    add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
endif()

if(NOT TARGET shp)
    add_subdirectory(${checkout_src_root}/${shp_module} shp)
endif()

# proj
if(WIN32)
    if(NOT TARGET proj)
        include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/proj_configuration.cmake)
    endif()
endif(WIN32)