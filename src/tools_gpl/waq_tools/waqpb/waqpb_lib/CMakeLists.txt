# Define include directory
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include) # Note that the CMAKE_CURRENT_SOURCE_DIR represents THIS directory

# Define library
set(library_name waqpb_lib)
create_library(${library_name} ${CMAKE_CURRENT_SOURCE_DIR} .)

set(oss_dependencies deltares_common
                     nefis
                     waq_utils_f)

oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/waqpb)