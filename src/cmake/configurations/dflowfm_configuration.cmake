#Undefine parameter WITH_INTERACTER
unset(WITH_INTERACTER)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dflowfm_configuration_basic.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/windows_postbuild_configuration.cmake)
# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dflowfm)
