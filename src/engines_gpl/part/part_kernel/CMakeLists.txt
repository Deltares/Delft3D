set(library_name part_kernel_f)

create_target(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src_dir .)
set(oss_dependencies deltares_common
        waq_definition
        part_data_f
        part_utils_f
        part_io_f
        waq_utils_f
        gridgeom
        kdtree_wrapper
        kdtree2
        io_hyd
        io_netcdf
        waq_hyd_data
        netcdff
)

target_link_libraries(${library_name} ${oss_dependencies})

set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/part)

target_compile_options(${library_name} PRIVATE ${openmp_flag})
target_compile_options(${library_name} PRIVATE ${qauto_threaded_flags})
target_compile_options(${library_name} PRIVATE ${waq_default_flags})
