# This file is an adjusted CMakeLists.txt that is defined by the Metis directory.
# The following adjustments were made:
# - CMake version prerequisite.
# - Excluded the subdirectory "programs" as this is not necessary for the OSS usage of the metis library.
# - Setting the source path and the headers include path
set(source_path "${CMAKE_CURRENT_SOURCE_DIR}/../metis-5.1.0")

set(public_include_path "${CMAKE_CURRENT_SOURCE_DIR}/../metis-5.1.0/include")

project(METIS)

set(GKLIB_PATH "${source_path}/GKlib" CACHE PATH "path to GKlib")
set(SHARED FALSE CACHE BOOL "build a shared library")
add_definitions(-DHAVE_METIS)

set(METIS_INSTALL FALSE)


# Configure libmetis library.
if(SHARED)
  set(METIS_LIBRARY_TYPE SHARED)
else()
  set(METIS_LIBRARY_TYPE STATIC)
endif(SHARED)

include(${GKLIB_PATH}/GKlibSystem.cmake)
# Add include directories.
include_directories(${GKLIB_PATH})
include_directories(${source_path}/include)
# Recursively look for CMakeLists.txt in subdirs.
add_subdirectory("${source_path}/include" include)
add_subdirectory("${source_path}/libmetis" libmetis)
