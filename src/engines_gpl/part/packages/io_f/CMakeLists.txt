# Define library
set(library_name part_io_f)

create_library(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src)
# Add dependencies
set(oss_dependencies deltares_common
                     nefis
                     part_data_f
                     part_utils_f
                     waq_utils_f
                     netcdff
                     io_netcdf_data
                     waq_hyd_data)


target_link_libraries(${library_name} ${oss_dependencies})
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/part)

target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
