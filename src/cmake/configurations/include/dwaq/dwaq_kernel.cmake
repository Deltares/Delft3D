# Tools for D-Waq

if(NOT TARGET waq_netcdf)
    add_subdirectory(${checkout_src_root}/${waq_netcdf_module} waq_netcdf)
endif()

if(NOT TARGET waq_validation)
    add_subdirectory(${checkout_src_root}/${waq_validation_module} waq_validation)
endif()

if(NOT TARGET waq_kernel)
    add_subdirectory(${checkout_src_root}/${waq_kernel_module} waq_kernel)
endif()

if(NOT TARGET waq_memory)
    add_subdirectory(${checkout_src_root}/${waq_memory_module} waq_memory)
endif()

if(NOT TARGET waq_io)
    add_subdirectory(${checkout_src_root}/${waq_io_module} waq_io)
endif()

if(NOT TARGET waq_plugin_wasteload)
    add_subdirectory(${checkout_src_root}/${waq_plugin_wasteload_module} waq_plugin_wasteload)
endif()

if(NOT TARGET delwaq_lib)
    add_subdirectory(${checkout_src_root}/${delwaq_lib_module} delwaq_lib)
endif()

if(NOT TARGET delwaq1)
    add_subdirectory(${checkout_src_root}/${delwaq1_module} delwaq1)
endif()

if(NOT TARGET delwaq2)
    add_subdirectory(${checkout_src_root}/${delwaq2_module} delwaq2)
endif()