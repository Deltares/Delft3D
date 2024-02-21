# ============
add_subdirectory(${checkout_src_root}/${flow1d_cf_module} delftflow)
add_subdirectory(${checkout_src_root}/${flow1d_kernel_cf_module} kernel_cf)

# Specify the modules to be included

# Deltares common
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# UTILS_GPL
#
# flow1d_library
if(NOT TARGET flow1d)
    add_subdirectory(${checkout_src_root}/${flow1d_module} flow1d)
endif()
if(NOT TARGET flow1d_core)
    add_subdirectory(${checkout_src_root}/${flow1d_core_module} flow1d_core)
endif()
if(NOT TARGET flow1d_io)
    add_subdirectory(${checkout_src_root}/${flow1d_io_module} flow1d_io)
endif()

# morphology
if(NOT TARGET morphology_data)
    add_subdirectory(${checkout_src_root}/${morphology_data_module} morphology_data)
endif()
if(NOT TARGET morphology_io)
    add_subdirectory(${checkout_src_root}/${morphology_io_module} morphology_io)
endif()
if(NOT TARGET morphology_kernel)
    add_subdirectory(${checkout_src_root}/${morphology_kernel_module} morphology_kernel)
endif()
if(NOT TARGET morphology_plugins_c)
    add_subdirectory(${checkout_src_root}/${morphology_plugins_c_module} morphology_plugins_c)
endif()

# UTILS_LGPL
#
# delftio
if(NOT TARGET delftio)
    add_subdirectory(${checkout_src_root}/${delftio_module} delftio)
endif()
if(NOT TARGET delftio_shm)
    add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
endif()
if(NOT TARGET ec_module)
    add_subdirectory(${checkout_src_root}/${ec_module} ec_module)
endif()
if(NOT TARGET gridgeom)
    add_subdirectory(${checkout_src_root}/${gridgeom_module} gridgeom)
endif()
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()

if(NOT TARGET io_netcdf_data)
    add_subdirectory(${checkout_src_root}/${io_netcdf_data_module} io_netcdf_data)
endif()

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
endif()
if(NOT TARGET shp)
    add_subdirectory(${checkout_src_root}/${shp_module} shp)
endif()
if(WIN32)
    if(NOT TARGET proj)
        include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/proj_configuration.cmake)
    endif()
endif(WIN32)

# Third party libraries
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

if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()

# io_hyd
if(NOT TARGET io_hyd)
    add_subdirectory(${checkout_src_root}/${io_hyd_module} io_hyd)
endif()

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_base.cmake)


# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# emfsm
if(NOT TARGET esmfsm_version_number)
    add_subdirectory(${checkout_src_root}/${esmfsm_version_number_module} esmfsm_version_number)
endif()

if(NOT TARGET esmfsm_c)
    add_subdirectory(${checkout_src_root}/${esmfsm_c_module} esmfsm_c)
endif()

if(NOT TARGET esmfsm)
    add_subdirectory(${checkout_src_root}/${esmfsm_module} esmfsm)
endif()

# D-Hydro lib
if(NOT TARGET d_hydro_lib)
    add_subdirectory(${checkout_src_root}/${d_hydro_lib_module} d_hydro_lib)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(flow1d)
