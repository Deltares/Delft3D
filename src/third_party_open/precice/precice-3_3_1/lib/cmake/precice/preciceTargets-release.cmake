#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "precice::precice" for configuration "Release"
set_property(TARGET precice::precice APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(precice::precice PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/precice.lib"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/precice.dll"
  )

list(APPEND _cmake_import_check_targets precice::precice )
list(APPEND _cmake_import_check_files_for_precice::precice "${_IMPORT_PREFIX}/lib/precice.lib" "${_IMPORT_PREFIX}/bin/precice.dll" )

# Import target "precice::precice-tools" for configuration "Release"
set_property(TARGET precice::precice-tools APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(precice::precice-tools PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/precice-tools.exe"
  )

list(APPEND _cmake_import_check_targets precice::precice-tools )
list(APPEND _cmake_import_check_files_for_precice::precice-tools "${_IMPORT_PREFIX}/bin/precice-tools.exe" )

# Import target "precice::precice-version" for configuration "Release"
set_property(TARGET precice::precice-version APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(precice::precice-version PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/precice-version.exe"
  )

list(APPEND _cmake_import_check_targets precice::precice-version )
list(APPEND _cmake_import_check_files_for_precice::precice-version "${_IMPORT_PREFIX}/bin/precice-version.exe" )

# Import target "precice::precice-config-validate" for configuration "Release"
set_property(TARGET precice::precice-config-validate APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(precice::precice-config-validate PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/precice-config-validate.exe"
  )

list(APPEND _cmake_import_check_targets precice::precice-config-validate )
list(APPEND _cmake_import_check_files_for_precice::precice-config-validate "${_IMPORT_PREFIX}/bin/precice-config-validate.exe" )

# Import target "precice::precice-config-doc" for configuration "Release"
set_property(TARGET precice::precice-config-doc APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(precice::precice-config-doc PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/precice-config-doc.exe"
  )

list(APPEND _cmake_import_check_targets precice::precice-config-doc )
list(APPEND _cmake_import_check_files_for_precice::precice-config-doc "${_IMPORT_PREFIX}/bin/precice-config-doc.exe" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
