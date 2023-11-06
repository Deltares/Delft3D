# D-Part targets

if(NOT TARGET part_data_f)
    add_subdirectory(${checkout_src_root}/${part_data_f_module} part_data_f)
endif()

if(NOT TARGET part_utils_f)
    add_subdirectory(${checkout_src_root}/${part_utils_f_module} part_utils_f)
endif()

if(NOT TARGET part_io_f)
    add_subdirectory(${checkout_src_root}/${part_io_f_module} part_io_f)
endif()

if(NOT TARGET part_kernel_f)
    add_subdirectory(${checkout_src_root}/${part_kernel_f_module} part_kernel_f)
endif()

if(NOT TARGET delpar)
    add_subdirectory(${checkout_src_root}/${delpar_module} delpar)
endif()