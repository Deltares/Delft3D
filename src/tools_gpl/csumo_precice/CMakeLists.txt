cmake_minimum_required(VERSION 3.30)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)

project(
    csumo_precice
    VERSION 0.0.1
    DESCRIPTION "CSUMO wrapper for coupling through preCICE"
    LANGUAGES CXX
)

include(cmake/PreventInSourceBuilds.cmake)
include(cmake/CompilerWarnings.cmake)

add_subdirectory(src)
