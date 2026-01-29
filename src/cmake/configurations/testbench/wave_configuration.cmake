include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dimr_configuration.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dflowfm_configuration_basic.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaves_configuration.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/flow2d3d_configuration.cmake) # For d3d4(+waves) support.
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/d_hydro_configuration.cmake)  # For d3d4(+waves) support.
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/drr_configuration.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/fbc_configuration.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/tools_configuration.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/windows_postbuild_configuration.cmake)

project(wave)