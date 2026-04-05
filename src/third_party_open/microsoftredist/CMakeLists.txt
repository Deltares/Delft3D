message(STATUS "Installing Microsoft Redistributable Libraries")
add_custom_target(microsoftredist)
set_target_properties(microsoftredist PROPERTIES FOLDER third_party_open)
if(WIN32)
install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
          DESTINATION lib
)
endif(WIN32)
