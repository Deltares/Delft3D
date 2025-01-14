# Specify the modules to be included

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dimr_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dflowfm_configuration_basic.cmake)
 
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaq_configuration.cmake)
 
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaves_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/drr_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/tools_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/windows_postbuild_configuration.cmake)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(fm-suite)
