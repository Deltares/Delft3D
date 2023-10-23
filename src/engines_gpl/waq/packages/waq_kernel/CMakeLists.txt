# Set path for include files
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_kernel_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_kernel_include_path})

# Define library
set(library_name waq_kernel)
create_library_recursive(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src)

# Set dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     part_data_f
                     part_io_f
                     part_kernel_f
                     part_utils_f
                     waq_data
                     waq_memory
                     waq_plugin_wasteload
                     waq_process
                     waq_utils_c
                     waq_utils_f
                     waq_netcdf
                     netcdff
                     io_netcdf
                     io_netcdf_data
                     io_hyd
                     waq_hyd_data)

oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
