#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "hdf5-static" for configuration "Release"
set_property(TARGET hdf5-static APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(hdf5-static PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libhdf5.lib"
  )

list(APPEND _cmake_import_check_targets hdf5-static )
list(APPEND _cmake_import_check_files_for_hdf5-static "${_IMPORT_PREFIX}/lib/libhdf5.lib" )

# Import target "hdf5-shared" for configuration "Release"
set_property(TARGET hdf5-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(hdf5-shared PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/hdf5.lib"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/hdf5.dll"
  )

list(APPEND _cmake_import_check_targets hdf5-shared )
list(APPEND _cmake_import_check_files_for_hdf5-shared "${_IMPORT_PREFIX}/lib/hdf5.lib" "${_IMPORT_PREFIX}/bin/hdf5.dll" )

# Import target "mirror_server" for configuration "Release"
set_property(TARGET mirror_server APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(mirror_server PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/mirror_server.exe"
  )

list(APPEND _cmake_import_check_targets mirror_server )
list(APPEND _cmake_import_check_files_for_mirror_server "${_IMPORT_PREFIX}/bin/mirror_server.exe" )

# Import target "mirror_server_stop" for configuration "Release"
set_property(TARGET mirror_server_stop APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(mirror_server_stop PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/mirror_server_stop.exe"
  )

list(APPEND _cmake_import_check_targets mirror_server_stop )
list(APPEND _cmake_import_check_files_for_mirror_server_stop "${_IMPORT_PREFIX}/bin/mirror_server_stop.exe" )

# Import target "hdf5_tools-static" for configuration "Release"
set_property(TARGET hdf5_tools-static APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(hdf5_tools-static PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libhdf5_tools.lib"
  )

list(APPEND _cmake_import_check_targets hdf5_tools-static )
list(APPEND _cmake_import_check_files_for_hdf5_tools-static "${_IMPORT_PREFIX}/lib/libhdf5_tools.lib" )

# Import target "hdf5_tools-shared" for configuration "Release"
set_property(TARGET hdf5_tools-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(hdf5_tools-shared PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/hdf5_tools.lib"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/hdf5_tools.dll"
  )

list(APPEND _cmake_import_check_targets hdf5_tools-shared )
list(APPEND _cmake_import_check_files_for_hdf5_tools-shared "${_IMPORT_PREFIX}/lib/hdf5_tools.lib" "${_IMPORT_PREFIX}/bin/hdf5_tools.dll" )

# Import target "h5diff" for configuration "Release"
set_property(TARGET h5diff APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5diff PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5diff.exe"
  )

list(APPEND _cmake_import_check_targets h5diff )
list(APPEND _cmake_import_check_files_for_h5diff "${_IMPORT_PREFIX}/bin/h5diff.exe" )

# Import target "h5diff-shared" for configuration "Release"
set_property(TARGET h5diff-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5diff-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5diff-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5diff-shared )
list(APPEND _cmake_import_check_files_for_h5diff-shared "${_IMPORT_PREFIX}/bin/h5diff-shared.exe" )

# Import target "h5ls" for configuration "Release"
set_property(TARGET h5ls APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5ls PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5ls.exe"
  )

list(APPEND _cmake_import_check_targets h5ls )
list(APPEND _cmake_import_check_files_for_h5ls "${_IMPORT_PREFIX}/bin/h5ls.exe" )

# Import target "h5ls-shared" for configuration "Release"
set_property(TARGET h5ls-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5ls-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5ls-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5ls-shared )
list(APPEND _cmake_import_check_files_for_h5ls-shared "${_IMPORT_PREFIX}/bin/h5ls-shared.exe" )

# Import target "h5debug" for configuration "Release"
set_property(TARGET h5debug APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5debug PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5debug.exe"
  )

list(APPEND _cmake_import_check_targets h5debug )
list(APPEND _cmake_import_check_files_for_h5debug "${_IMPORT_PREFIX}/bin/h5debug.exe" )

# Import target "h5repart" for configuration "Release"
set_property(TARGET h5repart APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5repart PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5repart.exe"
  )

list(APPEND _cmake_import_check_targets h5repart )
list(APPEND _cmake_import_check_files_for_h5repart "${_IMPORT_PREFIX}/bin/h5repart.exe" )

# Import target "h5mkgrp" for configuration "Release"
set_property(TARGET h5mkgrp APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5mkgrp PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5mkgrp.exe"
  )

list(APPEND _cmake_import_check_targets h5mkgrp )
list(APPEND _cmake_import_check_files_for_h5mkgrp "${_IMPORT_PREFIX}/bin/h5mkgrp.exe" )

# Import target "h5clear" for configuration "Release"
set_property(TARGET h5clear APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5clear PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5clear.exe"
  )

list(APPEND _cmake_import_check_targets h5clear )
list(APPEND _cmake_import_check_files_for_h5clear "${_IMPORT_PREFIX}/bin/h5clear.exe" )

# Import target "h5delete" for configuration "Release"
set_property(TARGET h5delete APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5delete PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5delete.exe"
  )

list(APPEND _cmake_import_check_targets h5delete )
list(APPEND _cmake_import_check_files_for_h5delete "${_IMPORT_PREFIX}/bin/h5delete.exe" )

# Import target "h5debug-shared" for configuration "Release"
set_property(TARGET h5debug-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5debug-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5debug-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5debug-shared )
list(APPEND _cmake_import_check_files_for_h5debug-shared "${_IMPORT_PREFIX}/bin/h5debug-shared.exe" )

# Import target "h5repart-shared" for configuration "Release"
set_property(TARGET h5repart-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5repart-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5repart-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5repart-shared )
list(APPEND _cmake_import_check_files_for_h5repart-shared "${_IMPORT_PREFIX}/bin/h5repart-shared.exe" )

# Import target "h5mkgrp-shared" for configuration "Release"
set_property(TARGET h5mkgrp-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5mkgrp-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5mkgrp-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5mkgrp-shared )
list(APPEND _cmake_import_check_files_for_h5mkgrp-shared "${_IMPORT_PREFIX}/bin/h5mkgrp-shared.exe" )

# Import target "h5clear-shared" for configuration "Release"
set_property(TARGET h5clear-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5clear-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5clear-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5clear-shared )
list(APPEND _cmake_import_check_files_for_h5clear-shared "${_IMPORT_PREFIX}/bin/h5clear-shared.exe" )

# Import target "h5delete-shared" for configuration "Release"
set_property(TARGET h5delete-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5delete-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5delete-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5delete-shared )
list(APPEND _cmake_import_check_files_for_h5delete-shared "${_IMPORT_PREFIX}/bin/h5delete-shared.exe" )

# Import target "h5import" for configuration "Release"
set_property(TARGET h5import APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5import PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5import.exe"
  )

list(APPEND _cmake_import_check_targets h5import )
list(APPEND _cmake_import_check_files_for_h5import "${_IMPORT_PREFIX}/bin/h5import.exe" )

# Import target "h5import-shared" for configuration "Release"
set_property(TARGET h5import-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5import-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5import-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5import-shared )
list(APPEND _cmake_import_check_files_for_h5import-shared "${_IMPORT_PREFIX}/bin/h5import-shared.exe" )

# Import target "h5repack" for configuration "Release"
set_property(TARGET h5repack APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5repack PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5repack.exe"
  )

list(APPEND _cmake_import_check_targets h5repack )
list(APPEND _cmake_import_check_files_for_h5repack "${_IMPORT_PREFIX}/bin/h5repack.exe" )

# Import target "h5repack-shared" for configuration "Release"
set_property(TARGET h5repack-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5repack-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5repack-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5repack-shared )
list(APPEND _cmake_import_check_files_for_h5repack-shared "${_IMPORT_PREFIX}/bin/h5repack-shared.exe" )

# Import target "h5jam" for configuration "Release"
set_property(TARGET h5jam APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5jam PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5jam.exe"
  )

list(APPEND _cmake_import_check_targets h5jam )
list(APPEND _cmake_import_check_files_for_h5jam "${_IMPORT_PREFIX}/bin/h5jam.exe" )

# Import target "h5unjam" for configuration "Release"
set_property(TARGET h5unjam APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5unjam PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5unjam.exe"
  )

list(APPEND _cmake_import_check_targets h5unjam )
list(APPEND _cmake_import_check_files_for_h5unjam "${_IMPORT_PREFIX}/bin/h5unjam.exe" )

# Import target "h5jam-shared" for configuration "Release"
set_property(TARGET h5jam-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5jam-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5jam-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5jam-shared )
list(APPEND _cmake_import_check_files_for_h5jam-shared "${_IMPORT_PREFIX}/bin/h5jam-shared.exe" )

# Import target "h5unjam-shared" for configuration "Release"
set_property(TARGET h5unjam-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5unjam-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5unjam-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5unjam-shared )
list(APPEND _cmake_import_check_files_for_h5unjam-shared "${_IMPORT_PREFIX}/bin/h5unjam-shared.exe" )

# Import target "h5copy" for configuration "Release"
set_property(TARGET h5copy APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5copy PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5copy.exe"
  )

list(APPEND _cmake_import_check_targets h5copy )
list(APPEND _cmake_import_check_files_for_h5copy "${_IMPORT_PREFIX}/bin/h5copy.exe" )

# Import target "h5copy-shared" for configuration "Release"
set_property(TARGET h5copy-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5copy-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5copy-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5copy-shared )
list(APPEND _cmake_import_check_files_for_h5copy-shared "${_IMPORT_PREFIX}/bin/h5copy-shared.exe" )

# Import target "h5stat" for configuration "Release"
set_property(TARGET h5stat APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5stat PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5stat.exe"
  )

list(APPEND _cmake_import_check_targets h5stat )
list(APPEND _cmake_import_check_files_for_h5stat "${_IMPORT_PREFIX}/bin/h5stat.exe" )

# Import target "h5stat-shared" for configuration "Release"
set_property(TARGET h5stat-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5stat-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5stat-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5stat-shared )
list(APPEND _cmake_import_check_files_for_h5stat-shared "${_IMPORT_PREFIX}/bin/h5stat-shared.exe" )

# Import target "h5dump" for configuration "Release"
set_property(TARGET h5dump APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5dump PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5dump.exe"
  )

list(APPEND _cmake_import_check_targets h5dump )
list(APPEND _cmake_import_check_files_for_h5dump "${_IMPORT_PREFIX}/bin/h5dump.exe" )

# Import target "h5dump-shared" for configuration "Release"
set_property(TARGET h5dump-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5dump-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5dump-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5dump-shared )
list(APPEND _cmake_import_check_files_for_h5dump-shared "${_IMPORT_PREFIX}/bin/h5dump-shared.exe" )

# Import target "h5format_convert" for configuration "Release"
set_property(TARGET h5format_convert APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5format_convert PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5format_convert.exe"
  )

list(APPEND _cmake_import_check_targets h5format_convert )
list(APPEND _cmake_import_check_files_for_h5format_convert "${_IMPORT_PREFIX}/bin/h5format_convert.exe" )

# Import target "h5format_convert-shared" for configuration "Release"
set_property(TARGET h5format_convert-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5format_convert-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5format_convert-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5format_convert-shared )
list(APPEND _cmake_import_check_files_for_h5format_convert-shared "${_IMPORT_PREFIX}/bin/h5format_convert-shared.exe" )

# Import target "h5perf_serial" for configuration "Release"
set_property(TARGET h5perf_serial APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5perf_serial PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5perf_serial.exe"
  )

list(APPEND _cmake_import_check_targets h5perf_serial )
list(APPEND _cmake_import_check_files_for_h5perf_serial "${_IMPORT_PREFIX}/bin/h5perf_serial.exe" )

# Import target "hdf5_hl-static" for configuration "Release"
set_property(TARGET hdf5_hl-static APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(hdf5_hl-static PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libhdf5_hl.lib"
  )

list(APPEND _cmake_import_check_targets hdf5_hl-static )
list(APPEND _cmake_import_check_files_for_hdf5_hl-static "${_IMPORT_PREFIX}/lib/libhdf5_hl.lib" )

# Import target "hdf5_hl-shared" for configuration "Release"
set_property(TARGET hdf5_hl-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(hdf5_hl-shared PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/hdf5_hl.lib"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/hdf5_hl.dll"
  )

list(APPEND _cmake_import_check_targets hdf5_hl-shared )
list(APPEND _cmake_import_check_files_for_hdf5_hl-shared "${_IMPORT_PREFIX}/lib/hdf5_hl.lib" "${_IMPORT_PREFIX}/bin/hdf5_hl.dll" )

# Import target "h5watch" for configuration "Release"
set_property(TARGET h5watch APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5watch PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5watch.exe"
  )

list(APPEND _cmake_import_check_targets h5watch )
list(APPEND _cmake_import_check_files_for_h5watch "${_IMPORT_PREFIX}/bin/h5watch.exe" )

# Import target "h5watch-shared" for configuration "Release"
set_property(TARGET h5watch-shared APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(h5watch-shared PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/h5watch-shared.exe"
  )

list(APPEND _cmake_import_check_targets h5watch-shared )
list(APPEND _cmake_import_check_files_for_h5watch-shared "${_IMPORT_PREFIX}/bin/h5watch-shared.exe" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
