# Set path for include files
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_io_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_io_include_path})

# Define the library
set(library_name waq_io)

create_library_recursive(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src)

# Set dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     part_data_f
                     part_io_f
                     part_utils_f
                     io_hyd
                     netcdff
                     waq_hyd_data
                     io_netcdf
                     io_netcdf_data
                     waq_data
                     waq_netcdf
                     waq_validation
                     waq_utils_c
                     waq_utils_f)

target_link_libraries(${library_name} ${oss_dependencies})
oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
set_target_properties(${library_name} PROPERTIES FOLDER engines_gpl/waq)
