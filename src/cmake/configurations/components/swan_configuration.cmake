# Install the SWAN executables supplied by Conan.
include(${CMAKE_CURRENT_LIST_DIR}/../../install_swan.cmake)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(swan)
