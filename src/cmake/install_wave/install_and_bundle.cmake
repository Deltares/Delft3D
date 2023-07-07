set(BUILD_LIBRARIES
   ${CMAKE_INSTALL_PREFIX}/lib/libwave.so
)
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

fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/wave_exe" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")

set_rpath("${CMAKE_INSTALL_PREFIX}/bin" "$ORIGIN:$ORIGIN/../lib")
set_rpath("${CMAKE_INSTALL_PREFIX}/lib" "$ORIGIN")
set_rpath("${CMAKE_INSTALL_PREFIX}/share" "$ORIGIN/../lib:$ORIGIN")

execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type l -exec echo "remove destination of symlink:" {} \; -exec bash -c "cp --remove-destination $(readlink {}) {};"  {} \; WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/lib" )

# ESMF. rpath: Don't refer to $ORIGIN/../lib
#file(COPY ${CMAKE_SOURCE_DIR}/../src/third_party_open/esmf/lnx64/bin/ESMF_RegridWeightGen                        DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
#file(COPY ${CMAKE_SOURCE_DIR}/../src/third_party_open/esmf/lnx64/scripts/ESMF_RegridWeightGen_in_Delft3D-WAVE.sh DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
#file(COPY ${CMAKE_SOURCE_DIR}/../src/third_party_open/esmf/lnx64/bin      DESTINATION ${CMAKE_INSTALL_PREFIX}/share/delft3d/esmf/lnx64/ FILES_MATCHING PATTERN "lib*")
#file(COPY ${CMAKE_SOURCE_DIR}/../src/third_party_open/esmf/lnx64/bin_COS7 DESTINATION ${CMAKE_INSTALL_PREFIX}/share/delft3d/esmf/lnx64/ FILES_MATCHING PATTERN "lib*")
#execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/share/delft3d/esmf/lnx64" -type f -exec echo "patched rpath of: "  {} \; -exec bash -c "patchelf --set-rpath '$ORIGIN' $1" _ {} \;)

file(RENAME ${CMAKE_INSTALL_PREFIX}/bin/wave_exe ${CMAKE_INSTALL_PREFIX}/bin/wave)

