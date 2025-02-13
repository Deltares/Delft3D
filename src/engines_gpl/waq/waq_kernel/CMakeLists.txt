set(library_name waq_kernel)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir . recursive)

# Set dependencies
set(oss_dependencies deltares_common
        part_data_f
        part_io_f
        part_kernel_f
        part_utils_f
        waq_memory
        waq_utils_f
        waq_plugin_wasteload
        waq_process
        waq_data
        waq_netcdf
        waq_hyd_data)

target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE ${qauto_threaded_flags})
target_compile_options(${library_name} PRIVATE ${waq_default_flags})

set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)
