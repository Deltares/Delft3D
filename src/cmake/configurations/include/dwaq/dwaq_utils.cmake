# Utils targets needed for D-Waq

include(${CMAKE_CURRENT_LIST_DIR}/dwaq_functions.cmake)

add_target_with_subdirectory(deltares_common ${deltares_common_module})
add_target_with_subdirectory(deltares_common_c ${deltares_common_c_module})
add_target_with_subdirectory(netcdff ${netcdf_module})
add_target_with_subdirectory(io_netcdf ${io_netcdf_module})
add_target_with_subdirectory(io_netcdf_data ${io_netcdf_data_module})
add_target_with_subdirectory(gridgeom ${gridgeom_module})
add_target_with_subdirectory(nefis ${nefis_module})
add_target_with_subdirectory(io_hyd ${io_hyd_module})
add_target_with_subdirectory(waq_hyd_data ${waq_hyd_data_module})
add_target_with_subdirectory(morphology_data ${morphology_data_module})
