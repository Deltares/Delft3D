# Tools for D-Waq

include(${CMAKE_CURRENT_LIST_DIR}/dwaq_functions.cmake)

add_target_with_subdirectory(waq_netcdf ${waq_netcdf_module})
add_target_with_subdirectory(waq_validation ${waq_validation_module})
add_target_with_subdirectory(waq_kernel ${waq_kernel_module})
add_target_with_subdirectory(waq_memory ${waq_memory_module})
add_target_with_subdirectory(waq_io ${waq_io_module})
add_target_with_subdirectory(waq_plugin_wasteload ${waq_plugin_wasteload_module})
add_target_with_subdirectory(delwaq_lib ${delwaq_lib_module})
add_target_with_subdirectory(delwaq1 ${delwaq1_module})
add_target_with_subdirectory(delwaq2 ${delwaq2_module})