set(library_path "${CMAKE_CURRENT_SOURCE_DIR}/../")

set(headers "${library_path}/config.h")
set(source "${library_path}/sigwatch.c")

set(library_name libsigwatch)
add_library(${library_name} ${headers}
                            ${source})

# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE _MBCS)

# Create the folder structure in vfproj
source_group(TREE ${library_path} FILES ${source} ${headers})
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open)