set(source_path "${CMAKE_CURRENT_SOURCE_DIR}/..")

set(public_include_path "${CMAKE_CURRENT_SOURCE_DIR}/..")

project(getopt)
set(getopt_INSTALL FALSE)
set(getopt_LIBRARY_TYPE STATIC)

include_directories(${source_path})

add_library(getopt ${getopt_LIBRARY_TYPE} ../getopt.c)

target_link_libraries(getopt PRIVATE no_compiler_warnings)

set_target_properties (getopt PROPERTIES FOLDER third_party_open/getopt)
