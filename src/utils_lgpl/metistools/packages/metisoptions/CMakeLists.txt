set(library_name metisoptions)

file(GLOB source src/*.c)
add_library(${library_name} ${source})

target_link_libraries(${library_name} PRIVATE metis::metis)

# Create the folder structure in project
source_group("${visual_studio_source_group_name}" FILES ${source})

# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE HAVE_METIS)
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/metistools)
