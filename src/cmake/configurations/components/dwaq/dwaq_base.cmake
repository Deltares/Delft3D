# D-Waq base targets

include(${CMAKE_CURRENT_LIST_DIR}/dwaq_functions.cmake)

add_target_with_subdirectory(waq_definition ${waq_definition_module})
add_target_with_subdirectory(waq_utils_c ${waq_utils_c_module})
add_target_with_subdirectory(waq_utils_f ${waq_utils_f_module})
add_target_with_subdirectory(waq_process ${waq_process_module})
add_target_with_subdirectory(waq_data ${waq_data_module})
add_target_with_subdirectory(waq_hyd_data ${waq_hyd_data_module})
add_target_with_subdirectory(solvesaphe ${solvesaphe_module})