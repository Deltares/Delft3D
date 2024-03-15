set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/../")

set(headers "${library_path}/triangle.h"
            "${library_path}/f2c.h")
set(source  "${library_path}/triangle.c"
            "${library_path}/tricall2.c")

set(library_name triangle_c)
add_library(${library_name} ${headers}
                            ${source})
if (CMAKE_GENERATOR MATCHES "Visual Studio")
	# Create the folder structure in project
	source_group(${visual_studio_source_group_name} FILES ${source})
	source_group("${visual_studio_source_group_name}\\Header Files" FILES ${headers}) 
endif()
# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE _LIB)

# Create the folder structure in project
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES   ${headers} ${source} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)
