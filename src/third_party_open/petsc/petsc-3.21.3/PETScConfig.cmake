# PETSc CMake Configuration File
# This file allows CMake's find_package() to locate PETSc

# Compute the installation prefix relative to this file
get_filename_component(PETSC_CMAKE_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
set(PETSC_DIR "${PETSC_CMAKE_DIR}")

# Set version information
set(PETSc_VERSION_MAJOR 3)
set(PETSc_VERSION_MINOR 21)
set(PETSc_VERSION_SUBMINOR 3)
set(PETSc_VERSION "${PETSc_VERSION_MAJOR}.${PETSc_VERSION_MINOR}.${PETSc_VERSION_SUBMINOR}")

# Set include directories
set(PETSC_INCLUDE_DIRS "${PETSC_DIR}/include")

# Set library directory
set(PETSC_LIBRARY_DIR "${PETSC_DIR}/lib/x64/Release-oneAPI")

# Find the PETSc library
find_library(PETSC_LIBRARY
  NAMES petsc libpetsc
  PATHS "${PETSC_LIBRARY_DIR}"
  NO_DEFAULT_PATH
)

# Set libraries
set(PETSC_LIBRARIES "${PETSC_LIBRARY}")

# Handle REQUIRED and QUIET arguments
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PETSc
  REQUIRED_VARS PETSC_LIBRARY PETSC_INCLUDE_DIRS
  VERSION_VAR PETSc_VERSION
)

# Create imported target
if(PETSc_FOUND AND NOT TARGET PETSc::PETSc)
  add_library(PETSc::PETSc SHARED IMPORTED)
  set_target_properties(PETSc::PETSc PROPERTIES
    IMPORTED_LOCATION "${PETSC_LIBRARY_DIR}/libpetsc.dll"
    IMPORTED_IMPLIB "${PETSC_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${PETSC_INCLUDE_DIRS}"
  )
endif()

# Set standard output variables
if(PETSc_FOUND)
  set(PETSC_FOUND TRUE)
  mark_as_advanced(PETSC_LIBRARY)
endif()
