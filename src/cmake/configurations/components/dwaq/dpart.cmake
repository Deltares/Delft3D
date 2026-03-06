# D-Part targets
include(${CMAKE_CURRENT_LIST_DIR}/dwaq_functions.cmake)

add_target_with_subdirectory(part_data_f ${part_data_f_module})
add_target_with_subdirectory(part_utils_f ${part_utils_f_module})
add_target_with_subdirectory(part_io_f ${part_io_f_module})
add_target_with_subdirectory(part_kernel_f ${part_kernel_f_module})
add_target_with_subdirectory(delpar ${delpar_module})