# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
# Define library
set(library_name waq_netcdf)
create_library(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src)

# Set dependencies
set(oss_dependencies deltares_common
                     waq_utils_f
                     netcdff
                     io_netcdf
                     io_netcdf_data)

target_link_libraries(${library_name} ${oss_dependencies})
oss_include_libraries(${library_name} oss_dependencies)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
