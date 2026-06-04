cmake_minimum_required(VERSION 3.21)
project(dflowfm_io LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_library(dflowfm_io SHARED
    src/MduModel.cpp
    src/dflowfm_io.cpp
)

include(GenerateExportHeader)
generate_export_header(dflowfm_io
    EXPORT_FILE_NAME ${CMAKE_CURRENT_BINARY_DIR}/dflowfm_io/dflowfm_io_export.h
)

target_include_directories(dflowfm_io
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${CMAKE_CURRENT_BINARY_DIR}
)

add_subdirectory(wrappers/fortran)

add_subdirectory(test)

