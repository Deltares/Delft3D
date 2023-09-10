set(library_name clipper)

# Set the directory of where the source code is located.
set(src_path "${CMAKE_CURRENT_SOURCE_DIR}/../")

# Gather source files
file(GLOB source ${src_path}/*.cpp)

# Define library
add_library(${library_name} ${source})

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR}/../ FILES ${source})
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)
