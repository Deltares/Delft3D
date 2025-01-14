# Tools for D-Waq

include(${CMAKE_CURRENT_LIST_DIR}/dwaq_functions.cmake)

add_target_with_subdirectory(waqpb_lib ${waqpb_lib_module})
add_target_with_subdirectory(waqpb_import ${waqpb_import_module})
add_target_with_subdirectory(waqpb_export ${waqpb_export_module})
add_target_with_subdirectory(waqmerge ${waqmerge_module})
add_target_with_subdirectory(ddcouple ${ddcouple_module})
add_target_with_subdirectory(agrhyd ${agrhyd_module})
add_target_with_subdirectory(maptonetcdf ${maptonetcdf_module})
add_target_with_subdirectory(calcage ${calcage_module})
add_target_with_subdirectory(checkhydbal ${checkhydbal_module})