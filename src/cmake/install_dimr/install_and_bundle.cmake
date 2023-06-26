IF(UNIX)
set(BUILD_LIBRARIES
   ${CMAKE_INSTALL_PREFIX}/lib/libdimr_lib.so
)
ENDIF(UNIX)

set(THIRDPARTY_x64_LIB_FOLDERS
  ${CMAKE_INSTALL_PREFIX}
  ${CMAKE_INSTALL_PREFIX}/bin
  ${CMAKE_INSTALL_PREFIX}/lib
  ${CMAKE_INSTALL_PREFIX}/share
)
function(gp_resolved_file_type_override resolved_file type_var)
  set(${type_var} local PARENT_SCOPE)
endfunction()

function(gp_item_default_embedded_path_override item default_embedded_path_var)
  if(item MATCHES ".so")
    set(path "@executable_path/../lib" PARENT_SCOPE)
    set( overridden 1 PARENT_SCOPE )
  endif()
endfunction(gp_item_default_embedded_path_override)

include(BundleUtilities)
include(${CMAKE_CURRENT_SOURCE_DIR}/../src/cmake/functions.cmake)

set(BU_CHMOD_BUNDLE_ITEMS 1)
IF(UNIX)
	fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/dimr" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")

	#ugly way to rename file "libdimr_lib.so" to "libdimr.so", repairing the reference in dimr
	message("renaming libdimr_lib.so to libdimr.so, repairing reference in dimr")

   file(rename ${cmake_install_prefix}/lib/libdimr_lib.so ${cmake_install_prefix}/lib/libdimr.so) 
   execute_process(command bash -c "patchelf --replace-needed libdimr_lib.so libdimr.so ${cmake_install_prefix}/bin/dimr")

	set_rpath("${CMAKE_INSTALL_PREFIX}/bin" "$ORIGIN:$ORIGIN/../lib")
	set_rpath("${CMAKE_INSTALL_PREFIX}/lib" "$ORIGIN")
	set_rpath("${CMAKE_INSTALL_PREFIX}/share" "$ORIGIN/../lib:$ORIGIN")
	set_rpath("${CMAKE_INSTALL_PREFIX}/scripts" "$ORIGIN/../scripts:$ORIGIN")
	
	execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type l -exec echo "remove destination of symlink:" {} \; -exec bash -c "cp --remove-destination $(readlink {}) {};"  {} \; WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/lib" )
ENDIF(UNIX)