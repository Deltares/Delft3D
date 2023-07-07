# Specify the modules to be included

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dflowfm_configuration.cmake)
 
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dwaq_configuration.cmake)
 
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dwaves_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/rr_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/rtc_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/flow1d_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/flow1d2d_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/flow2d3d_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/d_hydro_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dimr_configuration.cmake)

# Not officially supported yet: include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/tests_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/tools_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/tools_gpl_configuration.cmake)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(all)
