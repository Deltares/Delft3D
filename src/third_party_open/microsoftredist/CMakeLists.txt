message(STATUS "Installing Microsoft Redistributable Libraries")
add_custom_target(microsoftredist)
set_target_properties(microsoftredist PROPERTIES FOLDER third_party_open)

if(WIN32)
   install (FILES
   ${CMAKE_CURRENT_SOURCE_DIR}/msvcp140.dll
   DESTINATION lib
   )
endif(WIN32)
