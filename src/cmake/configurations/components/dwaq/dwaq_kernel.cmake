# Tools for D-Waq

include(${CMAKE_CURRENT_LIST_DIR}/dwaq_functions.cmake)

add_target_with_subdirectory(waq_netcdf ${waq_netcdf_module})
add_target_with_subdirectory(waq_validation ${waq_validation_module})
add_target_with_subdirectory(waq_external_access_layer ${waq_external_access_layer_module})
add_target_with_subdirectory(waq_preprocessor ${waq_preprocessor_module})
add_target_with_subdirectory(waq_proc_preprocess ${waq_proc_preprocess_module})
add_target_with_subdirectory(waq_computation ${waq_computation_module})
add_target_with_subdirectory(waq_kernel ${waq_kernel_module})
add_target_with_subdirectory(waq_memory ${waq_memory_module})
add_target_with_subdirectory(waq_io ${waq_io_module})
add_target_with_subdirectory(waq_plugin_wasteload ${waq_plugin_wasteload_module})
add_target_with_subdirectory(delwaq_lib ${delwaq_lib_module})
add_target_with_subdirectory(waq_logging ${waq_logging_module})
add_target_with_subdirectory(delwaq ${delwaq_exe_module})