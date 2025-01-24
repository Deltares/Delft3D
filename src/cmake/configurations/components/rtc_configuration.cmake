# RTC Real Time Control
# =====================
add_subdirectory(${checkout_src_root}/${rtc_module} rtc)
add_subdirectory(${checkout_src_root}/${rtc_plugin_c_module} plugin_rtc_c)
add_subdirectory(${checkout_src_root}/${rtc_kernel_module} rtc_kernel)


# Utils
# =====
if(NOT TARGET rr_rtc_tools)
    add_subdirectory(${checkout_src_root}/${rr_rtc_tools_module} rr_rtc_tools)
endif()
if(NOT TARGET wl_openmi_support)
    add_subdirectory(${checkout_src_root}/${wl_openmi_support_module} wl_openmi_support)
endif()
if(NOT TARGET control_lib)
    add_subdirectory(${checkout_src_root}/${control_lib_module} control_lib)
endif()

# Utils LGPL
# =====
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
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()
if(NOT TARGET io_netcdf_data)
    add_subdirectory(${checkout_src_root}/${io_netcdf_data_module} io_netcdf_data)
endif()

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

# Third party
# ===========
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()


# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(rtc)
