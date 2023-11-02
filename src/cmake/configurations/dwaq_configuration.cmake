#
# D-WAQ kernel
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_base.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_kernel.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_dflowfm_online_coupling.cmake)

#
# D-Waq tools
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_tools.cmake)

#
# D-Part kernel
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dpart.cmake)

#
# Third party libraries for D-Waq
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_third_party.cmake)


#
# Utils for D-Waq
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_utils.cmake)

<<<<<<< HEAD
=======
if(NOT TARGET part_utils_f)
    add_subdirectory(${checkout_src_root}/${part_utils_f_module} part_utils_f)
endif()

if(NOT TARGET part_io_f)
    add_subdirectory(${checkout_src_root}/${part_io_f_module} part_io_f)
endif()

if(NOT TARGET part_kernel_f)
    add_subdirectory(${checkout_src_root}/${part_kernel_f_module} part_kernel_f)
endif()

if(NOT TARGET delpar)
    add_subdirectory(${checkout_src_root}/${delpar_module} delpar)
endif()

#
# Third party libraries
#=============
# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
endif()

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
endif()

if(NOT TARGET shp)
    add_subdirectory(${checkout_src_root}/${shp_module} shp)
endif()

#
# Utils
#=============
# Deltares_common
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()
if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

# netcdf
if(NOT TARGET netcdff)
    add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
endif()

# io_netcdf
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()

# gridgeom
if(NOT TARGET gridgeom)
    add_subdirectory(${checkout_src_root}/${gridgeom_module} gridgeom)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# Solvesaphe
if(NOT TARGET solvesaphe)
    add_subdirectory(${checkout_src_root}/${solvesaphe_module} solvesaphe)
endif()

# io_hyd
if(NOT TARGET io_hyd)
    add_subdirectory(${checkout_src_root}/${io_hyd_module} io_hyd)
endif()
>>>>>>> 1522a33c04 (UNST-7379: Remove proj subdirectory target)

#
# Linux installation
#=============
if(UNIX)
    add_subdirectory(${checkout_src_root}/${install_waq_module} install_waq)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dwaq)
