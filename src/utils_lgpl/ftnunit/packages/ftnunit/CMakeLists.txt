# Define library
set(library_name ftnunit)

# Gather the source files
file(GLOB source    src/*.f90)

list(REMOVE_ITEM source ${CMAKE_CURRENT_SOURCE_DIR}/src/ftnunit_hooks.f90)

add_library(${library_name} ${source})
if (CMAKE_GENERATOR MATCHES "Visual Studio")
	# Create the folder structure in vfproj
	source_group(${visual_studio_source_group_name} FILES ${source})
endif()

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/ftnunit)

