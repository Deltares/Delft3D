# RR Rainfall Runoff
# ============
add_subdirectory(${checkout_src_root}/${rr_dll_module} rr_dll)
add_subdirectory(${checkout_src_root}/${rr_kernel_c_module} rr_kernel_c)
add_subdirectory(${checkout_src_root}/${rr_kernel_f_module} rr_kernel_f)
add_subdirectory(${checkout_src_root}/${rr_walrus_c_module} rr_walrus_c)
add_subdirectory(${checkout_src_root}/${rr_module} rr)


# Utils
# =====
if(NOT TARGET control_lib)
    add_subdirectory(${checkout_src_root}/${control_lib_module} control_lib)
endif()
if(NOT TARGET rr_rtc_tools)
    add_subdirectory(${checkout_src_root}/${rr_rtc_tools_module} rr_rtc_tools)
endif()
if(NOT TARGET wl_openmi_support)
    add_subdirectory(${checkout_src_root}/${wl_openmi_support_module} wl_openmi_support)
endif()


# Utils LGPL
# =====

# Flow1d
if(NOT TARGET flow1d_core)
    add_subdirectory(${checkout_src_root}/${flow1d_core_module} flow1d_core)
endif()

if(NOT TARGET delftio_shm)
    add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
endif()

if(NOT TARGET delftio)
    add_subdirectory(${checkout_src_root}/${delftio_module} delftio)
endif()

if(NOT TARGET deltares_common) 
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
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

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

if(NOT TARGET shp)
    add_subdirectory(${checkout_src_root}/${shp_module} shp)
endif()

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
endif()

# proj
if(WIN32)
    if(NOT TARGET proj)
        include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/proj_configuration.cmake)
    endif()
endif(WIN32)

# esmfsm
if(NOT TARGET esmfsm_version_number)
    add_subdirectory(${checkout_src_root}/${esmfsm_version_number_module} esmfsm_version_number)
endif()
if(NOT TARGET esmfsm_c)
    add_subdirectory(${checkout_src_root}/${esmfsm_c_module} esmfsm_c)
endif()
if(NOT TARGET esmfsm)
    add_subdirectory(${checkout_src_root}/${esmfsm_module} esmfsm)
endif()

# Third party
# ===========
if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
endif()

if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()

if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()


# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(rr)
