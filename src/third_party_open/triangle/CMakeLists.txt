cmake_minimum_required(VERSION 3.15)
project(triangle LANGUAGES C)

set(headers "${CMAKE_CURRENT_SOURCE_DIR}/triangle.h"
            "${CMAKE_CURRENT_SOURCE_DIR}/f2c.h")
set(source  "${CMAKE_CURRENT_SOURCE_DIR}/triangle.c"
            "${CMAKE_CURRENT_SOURCE_DIR}/tricall2.c")

set(library_name triangle)
add_library(${library_name} ${headers}
                            ${source})
