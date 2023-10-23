# Define library
set(library_name waq_process)

create_library_recursive(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} src)
# Set dependencies
set(oss_dependencies deltares_common
                     deltares_common_c
                     nefis
                     solvesaphe
                     waq_data
                     waq_hyd_data
                     waq_utils_c
                     waq_utils_f)

oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})


target_include_directories(${library_name}  PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
