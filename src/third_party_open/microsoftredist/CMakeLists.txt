message(STATUS "Installing Microsoft Redistributable Libraries")
add_custom_target(microsoftredist)
set_target_properties(microsoftredist PROPERTIES FOLDER third_party_open)

if(WIN32)
   install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ FILES_MATCHING DESTINATION lib
   PATTERN "msvcp140*.dll"
   PATTERN "vcruntime140*.dll"
   )
endif(WIN32)
