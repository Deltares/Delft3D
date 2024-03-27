# Shared definitions
# Set directory which contains all source files
set(src_path src)

# Gather source files
set(library_files ${src_path}/plugin_culvert.f90)

# Define library
set(library_name plugin_culvert)
add_library(${library_name} SHARED  ${library_files})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")
# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointers_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${floating_point_exception_flag}>")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_files})
set_target_properties (${library_name} PROPERTIES FOLDER plugins_lgpl/plugin_culvert)

install(TARGETS ${library_name} RUNTIME DESTINATION lib)
