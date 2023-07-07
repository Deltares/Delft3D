# Set version file variables
set(include_path include)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

include_directories(${include_path}
                    ${version_include_dir})

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(h_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/version_number.h)

# Set directory which contains all source files
set(src_path src)

# Gather source files
file(GLOB source_files ${src_path}/*.c)

# Define library
set(library_name esmfsm_version_number)
add_library(${library_name} ${source_files}
                            ${h_version_file})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${h_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/esmfsm)
