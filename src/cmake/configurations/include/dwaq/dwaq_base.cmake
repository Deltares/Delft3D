# D-Waq base targets

if(NOT TARGET waq_definition)
    add_subdirectory(${checkout_src_root}/${waq_definition_module} waq_definition)
endif()

if(NOT TARGET waq_utils_c)
    add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)
endif()

if(NOT TARGET waq_utils_f)
    add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
endif()

if(NOT TARGET waq_process)
    add_subdirectory(${checkout_src_root}/${waq_process_module} waq_process)
endif()

if(NOT TARGET waq_data)
    add_subdirectory(${checkout_src_root}/${waq_data_module} waq_data)
endif()

if(NOT TARGET waq_hyd_data)
    add_subdirectory(${checkout_src_root}/${waq_hyd_data_module} waq_hyd_data)
endif()

if(NOT TARGET solvesaphe)
    add_subdirectory(${checkout_src_root}/${solvesaphe_module} solvesaphe)
endif()