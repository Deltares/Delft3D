# Define library
set(library_name waq_memory)
create_library(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} .)

# Set dependencies
set(oss_dependencies deltares_common
                     waq_data
                     waq_utils_f)

target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
