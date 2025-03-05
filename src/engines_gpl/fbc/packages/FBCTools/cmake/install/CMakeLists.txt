configure_file(${PROJECT_SOURCE_DIR}/src/FBCTools.sh.in ${PROJECT_SOURCE_DIR}/src/FBCTools.sh)

set(FBC_SUPPORT_FILES
  ${PROJECT_SOURCE_DIR}/xsd/pi_diag.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_modelparameters.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_run.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_sharedtypes.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_state.xsd
  ${PROJECT_SOURCE_DIR}/xsd/pi_timeseries.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcDataConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcObjectiveConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcRuntimeConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcSharedTypes.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcStateConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/rtcToolsConfig.xsd
  ${PROJECT_SOURCE_DIR}/xsd/treeVector.xsd
  ${PROJECT_SOURCE_DIR}/GPLv2.txt
  ${PROJECT_SOURCE_DIR}/LICENSE.txt
  ${PROJECT_SOURCE_DIR}/release_notes.txt
)

set(FBC_EXECUTABLE_HELPER_FILES
 ${PROJECT_SOURCE_DIR}/src/FBCTools.sh
)

set(FBC_THIRDPARTY_x64_FILES
  ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-linux-gcc-3.4/lib/libxerces-c.so
  ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86_64-linux-gcc-3.4/lib/libxerces-c-3.1.so
)
set(FBC_THIRDPARTY_x32_FILES
                ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-linux-gcc-3.4/lib/libxerces-c.so
                ${PROJECT_SOURCE_DIR}/thirdParty/xerces/xerces-c-3.1.1-x86-linux-gcc-3.4/lib/libxerces-c-3.1.so
  )  
# install other linux
  install(FILES ${FBC_SUPPORT_FILES} DESTINATION share)
  install(PROGRAMS ${FBC_EXECUTABLE_HELPER_FILES} DESTINATION bin)  
  if(${CMAKE_SIZEOF_VOID_P} MATCHES "8")
    install(PROGRAMS ${FBC_THIRDPARTY_x64_FILES} DESTINATION lib)
  else()
    install(PROGRAMS ${FBC_THIRDPARTY_x32_FILES} DESTINATION lib)
  endif()

install(SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/install_and_bundle.cmake")
set(CPACK_PACKAGE_DIRECTORY "${CMAKE_INSTALL_PREFIX}/../package")
set(CPACK_VERBATIM_VARIABLES YES)
set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY OFF)
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "D-Feedback Control")
set(CPACK_PACKAGE_VENDOR "Deltares 2020")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/ReadMe.txt")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/Copyright.txt")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "0")
set(CPACK_GENERATOR "STGZ;TGZ")
include(CPack)
