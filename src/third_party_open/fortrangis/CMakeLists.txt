# CMake file for the FortranGIS project
#

#
# Require the minimum version for CMake - as suggested by CMake itself
#
cmake_minimum_required(VERSION 3.6)
PROJECT(fortrangis LANGUAGES C Fortran)
# We need at least Fortran and C is required by CMake
#
# TODO: check ISO_C_BINDING
#

#
# Find the optional components - at least one must be present
#
set(GOBUILD 0)

option(BUILD_SHAPELIB "Build Shapelib library" ON)
option(BUILD_PROJ "Build Proj library" ON)
option(BUILD_GDAL "Build Gdal library" ON)
option(BUILD_READOSM "Build ReadOSM library" OFF)

if(BUILD_SHAPELIB)
  find_path(Shapelib_INCLUDE_DIR NAMES shapefil.h REQUIRED)
  find_library(SHAPELIB_LIBRARY
    NAMES shp shapelib
    )
  if(SHAPELIB_LIBRARY)
    set(GOBUILD 1)
#    set(HAVE_SHAPELIB ON CACHE BOOL "No Shape library" FORCE)
    message(STATUS "Shapelib: found")
  else(SHAPELIB_LIBRARY)
    message(STATUS "Shapelib: not found")
  endif(SHAPELIB_LIBRARY)
endif(BUILD_SHAPELIB)

if(BUILD_GDAL)
  find_library(GDAL_LIBRARY
    NAMES gdal gdal19 gdallib
    )
  if(GDAL_LIBRARY)
    set(GOBUILD 1)
    message(STATUS "GDAL library: found")
  else(GDAL_LIBRARY)
    message(STATUS "GDAL library: not found")
  endif(GDAL_LIBRARY)
endif(BUILD_GDAL)

if(BUILD_PROJ)
  find_library(PROJ_LIBRARY
    NAMES proj projlib
    )
  if(PROJ_LIBRARY)
    set(GOBUILD 1)
    message(STATUS "Proj library: found")
  else(PROJ_LIBRARY)
    message(STATUS "Proj library: not found")
  endif(PROJ_LIBRARY)
endif(BUILD_PROJ)

if(BUILD_READOSM)
  find_library(READOSM_LIBRARY
    NAMES readosm readosmlib
    )
  if(READOSM_LIBRARY)
    set(GOBUILD 1)
    message(STATUS "ReadOSM library: found")
  else(READOSM_LIBRARY)
    message(STATUS "ReadOSM library: not found")
  endif(READOSM_LIBRARY)
endif(BUILD_READOSM)

#
# Do we have a reason to build the libraries?
#
if(NOT GOBUILD)
    message(FATAL_ERROR "At least one component of Shapelib, GDAL, Proj or ReadOSM must be present - none were found")
endif(NOT GOBUILD)

#
# Run over the subdirectories ...
#
enable_testing()
add_subdirectory(libfortranc)
add_subdirectory(libfortrangis)

#
# Configure the config.h file
# renamed to cmake_config.h.in to avoid conflict with automake since
# this one needs to be under version control
#
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake_config.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/config.h
    )

#summary()

# Installation:
install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
  include/${PROJECT_NAME})
install(TARGETS fortranc fortranGIS EXPORT ${PROJECT_NAME}Targets
  INCLUDES DESTINATION include/${PROJECT_NAME})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION lib/cmake/${PROJECT_NAME}
  NAMESPACE ${PROJECT_NAME}:: FILE ${PROJECT_NAME}Config.cmake)

export(EXPORT ${PROJECT_NAME}Targets NAMESPACE ${PROJECT_NAME}:: FILE
  ${PROJECT_NAME}Config.cmake)
