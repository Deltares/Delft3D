cmake_minimum_required(VERSION 3.21)
project(dflowfm_io_solution LANGUAGES CXX Fortran)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_subdirectory(dflowfm_ini_parser)
add_subdirectory(dflowfm_io)
add_subdirectory(dflowfm_io_tool)
