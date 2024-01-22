set(library_name metisoptions)

# Check if the include path is defined. If it is not present, it cannot be guaranteed that this module can be configured correctly
include_directories($ENV{METIS_DIR}/include)

get_module_include_path(${checkout_src_root}/${metis_module} 
                        ${library_name}
                        metis_include_path)
include_directories(${metis_include_path})
message(STATUS "Configuring '${library_name}' with the following metis_include_path: ${metis_include_path}")

file(GLOB source src/*.c)
add_library(${library_name} ${source})

target_link_libraries(${library_name} metis)

# Create the folder structure in project
source_group("${visual_studio_source_group_name}" FILES ${source})

# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE HAVE_METIS)
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/metistools)

