# Gather the source files 
set(source_path "${CMAKE_CURRENT_SOURCE_DIR}/../src-f90")
file(GLOB source    ${source_path}/kdtree2.f90)

set(library_name kdtree2)
add_library(${library_name} ${source})

target_link_libraries(${library_name} PRIVATE no_compiler_warnings)

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${source})
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)
