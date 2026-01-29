set(NO_FM_TOOLS True)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/tools_configuration.cmake)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(tools)
