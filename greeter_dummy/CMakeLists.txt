cmake_minimum_required(VERSION 3.30)
project(precice_dummy LANGUAGES CXX DESCRIPTION "dummy for testing PreCICE")

find_package(PkgConfig REQUIRED)
pkg_check_modules(precice REQUIRED IMPORTED_TARGET libprecice)

add_executable(greeter_dummy greeter_dummy.cpp)
set_target_properties(greeter_dummy PROPERTIES CXX_STANDARD 23)
target_link_libraries(greeter_dummy PRIVATE PkgConfig::precice)
