# FM testbench is currently identical to 'all'.
# This can/will become shorter when we implement code-change based logic.

# Specify the modules to be included

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dimr_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dflowfm_configuration_basic.cmake)
 
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaq_configuration.cmake)
 
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaves_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/flow2d3d_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/d_hydro_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/rtc_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/drr_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/tools_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/tools_gpl_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/windows_postbuild_configuration.cmake)

# Mormerge
if(NOT TARGET mormerge)
    add_subdirectory(${checkout_src_root}/${mormerge_module} mormerge)
endif()

# Plugins
if(NOT TARGET plugin_culvert)
    add_subdirectory(${checkout_src_root}/plugins_lgpl/plugin_culvert plugin_culvert)
endif()
if(NOT TARGET plugin_delftflow_traform)
    add_subdirectory(${checkout_src_root}/plugins_lgpl/plugin_delftflow_traform plugin_delftflow_traform)
endif()

if(UNIX)
  # install
  add_subdirectory(${checkout_src_root}/${install_flow2d3d_module} install_flow2d3d)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(fm)