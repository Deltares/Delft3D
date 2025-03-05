set(BUILD_LIBRARIES
   ${CMAKE_INSTALL_PREFIX}/lib/libFBCTools_BMI.so
   ${CMAKE_INSTALL_PREFIX}/lib/libFBCTools_OpenMI.so
)

set(FBC_THIRDPARTY_x64_LIB_FOLDERS
  ${CMAKE_INSTALL_PREFIX}
  ${CMAKE_INSTALL_PREFIX}/lib
  ${CMAKE_INSTALL_PREFIX}/bin
  ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-linux-gcc-3.4/lib
  ${PROJECT_SOURCE_DIR}/thirdParty/ipopt_3_11_7_linux64bit/lib
  ${Boost_LIBRARY_DIRS}
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

set(BU_CHMOD_BUNDLE_ITEMS ON)

fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/FBCTools" "${BUILD_LIBRARIES}" "${FBC_THIRDPARTY_x64_LIB_FOLDERS}")
find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/bin" -type f -not -name "*.sh" -not -name "*.xsd" -not -name "*.txt" -exec echo "patched rpath of: " {} \; -exec bash -c "patchelf --set-rpath '$ORIGIN:$ORIGIN/../lib' $1" _ {} \;)

  execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type f -exec echo "patched rpath of: " {} \; -exec bash -c "patchelf --set-rpath '$ORIGIN' $1" _ {} \;)
endif()
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type l -exec echo "remove destination of symlink: " {} \; -exec bash -c "cp --remove-destination $(readlink {}) {};"  {} \; WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/lib" )
